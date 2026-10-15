/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eyes4s.templatevalidationconsumer

import eyes4s.design.*
import scala.compiletime.testing.typeCheckErrors

class TemplateValidationWorkSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def split(size: Int, trainResponse: Double = 2.0, failing: Boolean = false) =
    val basis = get(TemplateBasis.of("work-fixture", Vector("x", "z"), "score"))
    val train = Vector(
      get(TemplateObservation.of("train-x", "train", Vector(1.0, 0.0), trainResponse)),
      get(TemplateObservation.of("train-z", "train", Vector(0.0, 1.0), 3.0))
    )
    val held = (0 until size).toVector.map { i =>
      get(
        TemplateObservation.of(
          s"held-$i",
          "held",
          if failing && i == size - 1 then Vector(1e308, 1e308) else Vector(i.toDouble, 1.0),
          i.toDouble
        )
      )
    }
    get(TemplateSplit.of(TemplateDesign.fixed(basis), train ++ held, Set("held")))

  test(
    "the fit is one stage followed by quantum-bounded held-out rows with exact oracle predictions"
  ) {
    for size <- Vector(1, 3, 37); amount <- Vector(1, 2, 32, Int.MaxValue) do
      val input   = split(size); val q = get(PairQuantum.of(amount))
      val initial = Template.crossValidationWork(input)
      assertEquals(initial.stage, TemplateValidationStage.FittingWholeTraining)
      assertEquals(initial.processedRows, 0)
      var work = initial; var visited = 0; var fitSteps = 0
      var result: Option[TemplateValidation[String, Vector[Double]]] = None
      while result.isEmpty && visited <= size do
        get(work.advance(q)) match
          case TemplateValidationStep.More(stage, units, next) =>
            assertEquals(stage, work.stage)
            assertEquals(next.stage, TemplateValidationStage.EvaluatingWholeRows)
            if stage == TemplateValidationStage.FittingWholeTraining then
              assertEquals(units, 1); fitSteps += 1
              assertEquals(next.processedRows, 0)
            else
              assert(units > 0 && units <= amount)
              visited += units
              assertEquals(next.processedRows, visited)
            assertEquals(next.totalRows, size)
            work = next
          case TemplateValidationStep.Done(units, completed) =>
            assert(units > 0 && units <= amount)
            assertEquals(work.stage, TemplateValidationStage.EvaluatingWholeRows)
            visited += units; result = Some(completed)
      val done = result.getOrElse(fail("did not terminate"))
      assertEquals(fitSteps, 1); assertEquals(visited, size)
      assertEquals(initial.stage, TemplateValidationStage.FittingWholeTraining)
      assertEquals(initial.processedRows, 0)
      assertEquals(done.fitted.coefficients, Vector(2.0, 3.0))
      assertEquals(done.evaluation.rows.map(_.key), input.heldOut.rows.map(_.key))
      assertEquals(
        done.evaluation.rows.map(_.result),
        (0 until size).toVector.map(i => Right((2.0 * i + 3.0, i.toDouble - (2.0 * i + 3.0))))
      )
      assertEquals(get(initial.complete(q)).evaluation.rows, done.evaluation.rows)
      assertEquals(get(Template.crossValidate(input)).evaluation.rows, done.evaluation.rows)
  }

  test("failed predictions remain rows and refuse MSE") {
    val input  = split(4, failing = true)
    val result = get(Template.crossValidationWork(input).complete(get(PairQuantum.of(1))))
    assertEquals(result.evaluation.rows.size, 4)
    assert(result.evaluation.rows.last.result.isLeft)
    assert(result.evaluation.meanSquaredError.isLeft)
  }

  test("a rank-deficient native fit is the same operand-bearing refusal before evaluation") {
    val basis = get(TemplateBasis.of("rank", Vector("x", "z"), "score"))
    val rows  = Vector(
      get(TemplateObservation.of("a", "train", Vector(1.0, 1.0), 1.0)),
      get(TemplateObservation.of("b", "train", Vector(2.0, 2.0), 2.0)),
      get(TemplateObservation.of("c", "held", Vector(1.0, 2.0), 3.0))
    )
    val input    = get(TemplateSplit.of(TemplateDesign.fixed(basis), rows, Set("held")))
    val expected = Template.fit(input.training).left.toOption
    assert(expected.nonEmpty)
    assertEquals(
      Template.crossValidationWork(input).advance(PairQuantum.default).left.toOption,
      expected
    )
    assertEquals(Template.crossValidate(input).left.toOption, expected)
  }

  test("the historical imported-lm route remains explicitly refused by native work") {
    val native = split(2)
    val basis  = get(TemplateBasis.of("work-fixture", Vector("x", "z"), "score"))
    val input  =
      get(TemplateSplit.of(TemplateDesign.importedLm(basis), native.rows, Set("held")))
    val expected = Left(TemplateError.Route(input.design.method, "native fit"))
    assertEquals(
      Template.crossValidationWork(input).advance(PairQuantum.default).left.toOption,
      expected.left.toOption
    )
  }

  test("external callers can name work but cannot forge its fitted state") {
    assertEquals(
      typeCheckErrors(
        "type Work = eyes4s.design.TemplateValidationWork[String, Vector[Double]]; val work: Option[Work] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "new eyes4s.design.TemplateValidationWork[String, Vector[Double]](null,None, null)"
    )
    assert(errors.exists(_.message.contains("TemplateValidationWork")))
  }

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

package eyes4s.templateworkconsumer

import eyes4s.design.*
import scala.compiletime.testing.typeCheckErrors

class TemplateEvaluationWorkSuite extends munit.FunSuite:
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

  test("pages respect row bounds, input order, cardinality and identities for every quantum") {
    for size <- Vector(1, 2, 3, 9, 37); amount <- Vector(1, 2, 3, 32, Int.MaxValue) do
      val input   = split(size); val fitted = get(Template.fit(input.training))
      val quantum = get(PairQuantum.of(amount));
      var work    = get(fitted.evaluationWork(input.heldOut))
      var result: Option[TemplateEvaluation[String]] = None; var visited = 0; var steps = 0
      while result.isEmpty && steps <= size do
        assertEquals(work.total, size)
        val before = work.position
        work.advance(quantum) match
          case TemplateEvaluationStep.More(units, next) =>
            assert(units > 0 && units <= amount)
            assertEquals(next.position, before + units)
            assertEquals(next.total, size)
            assertEquals(work.position, before)
            visited += units; work = next
          case TemplateEvaluationStep.Done(units, value) =>
            assert(units > 0 && units <= amount)
            assertEquals(before + units, size)
            visited += units; result = Some(value)
        steps += 1
      val completed = result.getOrElse(fail("work did not terminate"))
      assertEquals(visited, size)
      assertEquals(completed.rows.map(_.key), input.heldOut.rows.map(_.key))
      assertEquals(completed.rows.map(_.observed), input.heldOut.rows.map(_.response))
      assertEquals(completed.trainingHash, input.training.hash)
      assertEquals(completed.heldOutHash, input.heldOut.hash)
  }

  test("an Int.MaxValue quantum after partial progress cannot overflow the end index") {
    val input   = split(3); val fitted = get(Template.fit(input.training))
    val initial = get(fitted.evaluationWork(input.heldOut))
    initial.advance(get(PairQuantum.of(1))) match
      case TemplateEvaluationStep.More(1, next) =>
        next.advance(get(PairQuantum.of(Int.MaxValue))) match
          case TemplateEvaluationStep.Done(2, result) =>
            assertEquals(result.rows.map(_.key), input.heldOut.rows.map(_.key))
          case other => fail(other.toString)
      case other => fail(other.toString)
    assertEquals(initial.position, 0)
    assertEquals(initial.complete().rows.map(_.key), input.heldOut.rows.map(_.key))
  }

  test("a failed prediction remains one row and prevents silently reducing the denominator") {
    val input  = split(4, failing = true); val fitted = get(Template.fit(input.training))
    val result = get(fitted.evaluationWork(input.heldOut)).complete(get(PairQuantum.of(1)))
    assertEquals(result.rows.size, 4)
    assertEquals(result.rows.map(_.key), input.heldOut.rows.map(_.key))
    assert(
      result.rows.last.result.left.toOption.exists(_.isInstanceOf[TemplateError.Numerical])
    )
    assert(result.meanSquaredError.isLeft)
  }

  test("work and eager evaluation refuse a different training identity before any rows run") {
    val a        = split(3); val b = split(3, trainResponse = 99.0);
    val fit      = get(Template.fit(a.training))
    val expected = Left(TemplateError.Identity(a.training.hash, b.training.hash))
    assertEquals(fit.evaluationWork(b.heldOut).left.toOption, expected.left.toOption)
    assertEquals(fit.evaluate(b.heldOut).left.toOption, expected.left.toOption)
  }

  test("an external caller cannot manufacture cursor positions") {
    assertEquals(
      typeCheckErrors(
        "type Work = eyes4s.design.TemplateEvaluationWork[String,Vector[Double]]; val value: Option[Work] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "new eyes4s.design.TemplateEvaluationWork[String,Vector[Double]](null,null,0,Vector.empty)"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("TemplateEvaluationWork")))
  }

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

package eyes4s.importedtemplateconsumer

import eyes4s.design.*
import scala.compiletime.testing.typeCheckErrors

class TemplateImportedFitWorkSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val basis = get(TemplateBasis.of("imported-work", Vector("x", "z"), "score"))
  private def split(response: Double = 2.0) = get(
    TemplateSplit.of(
      TemplateDesign.importedLm(basis),
      Vector(
        get(TemplateObservation.of("x", "train", Vector(1.0, 0.0), response)),
        get(TemplateObservation.of("z", "train", Vector(0.0, 1.0), 3.0)),
        get(TemplateObservation.of("held", "held", Vector(2.0, 1.0), 8.0))
      ),
      Set("held")
    )
  )
  private val input   = split()
  private val receipt = get(
    TemplateImportedFit.of(
      input.training,
      input.training.hash.render,
      basis.columns,
      Vector(2.0, 3.0),
      2,
      2,
      "external fixture"
    )
  )

  test(
    "receipt checking is explicit, followed by paged held-out evaluation without native fitting"
  ) {
    val initial = Template.importedCrossValidationWork(input, receipt)
    assertEquals(initial.stage, TemplateValidationStage.CheckingWholeImportedReceipt)
    get(initial.advance(get(PairQuantum.of(1)))) match
      case TemplateValidationStep.More(
            TemplateValidationStage.CheckingWholeImportedReceipt,
            1,
            next
          ) =>
        assertEquals(next.stage, TemplateValidationStage.EvaluatingWholeRows)
        assertEquals(next.processedRows, 0)
        get(next.advance(get(PairQuantum.of(1)))) match
          case TemplateValidationStep.Done(1, result) =>
            assertEquals(result.fitted.backend, "external fixture")
            assertEquals(result.fitted.method, input.design.method)
            assertEquals(result.evaluation.rows.map(_.result), Vector(Right((7.0, 1.0))))
          case other => fail(s"$other")
      case other => fail(s"$other")
    assertEquals(initial.stage, TemplateValidationStage.CheckingWholeImportedReceipt)
    assert(Template.crossValidate(input).isLeft)
  }

  test(
    "source binding and ordered columns are checked by the existing imported-fit admission"
  ) {
    val wrong    = split(99.0)
    val expected = receipt.bind(wrong.training).left.toOption
    assert(expected.nonEmpty)
    assertEquals(
      Template
        .importedCrossValidationWork(wrong, receipt)
        .advance(PairQuantum.default)
        .left
        .toOption,
      expected
    )
    assert(
      TemplateImportedFit
        .of(
          input.training,
          input.training.hash.render,
          basis.columns.reverse,
          Vector(2.0, 3.0),
          2,
          2,
          "external fixture"
        )
        .isLeft
    )
    assert(
      TemplateImportedFit
        .of(
          input.training,
          input.training.hash.render,
          basis.columns,
          Vector(2.0, 3.0),
          1,
          2,
          "external fixture"
        )
        .isLeft
    )
  }

  test(
    "receipt identity includes backend and coefficients; callers cannot manufacture unchecked receipts"
  ) {
    val changed = get(
      TemplateImportedFit.of(
        input.training,
        input.training.hash.render,
        basis.columns,
        Vector(4.0, 3.0),
        2,
        2,
        "other external fixture"
      )
    )
    assertNotEquals(receipt.identity, changed.identity)
    assertEquals(
      typeCheckErrors(
        "type Receipt = eyes4s.design.TemplateImportedFit; val receipt: Option[Receipt] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      """new eyes4s.design.TemplateImportedFit("hash",Vector("x"),Vector(1.0),1,1,"backend")"""
    )
    assert(errors.exists(_.message.contains("TemplateImportedFit")))
  }

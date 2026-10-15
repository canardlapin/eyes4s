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

package eyes4s.templateplanconsumer

import eyes4s.compare.ComparisonQuantum
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class TemplateFitPlanSuite extends munit.FunSuite:
  private def get[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)
  private val basis = get(TemplateBasis.of("plan-basis", Vector("x", "z"), "score"))
  private def split(excludedResponse: Double = 99.0) =
    val rows = Vector(
      get(TemplateObservation.of("x", "train", Vector(1.0, 0.0), 2.0, Some("x"))),
      get(TemplateObservation.of("z", "train", Vector(0.0, 1.0), 3.0, Some("z"))),
      get(
        TemplateObservation
          .of("excluded", "train", Vector(4.0, 5.0), excludedResponse, Some("held-item"))
      ),
      get(TemplateObservation.of("held", "held", Vector(2.0, 1.0), 8.0, Some("held-item")))
    )
    get(TemplateSplit.of(TemplateDesign.fixed(basis), rows, Set("held")))
  private def plan = get(TemplateFitPlan.of(split()))

  test("native plan preserves exclusions, exact predictions and paged completion") {
    val p = plan
    assertEquals(p.split.excluded.map(_.row.key), Vector("excluded"))
    for amount <- Vector(1, 2, Int.MaxValue) do
      val q      = WorkQuanta(get(PairQuantum.of(amount)), ComparisonQuantum.default)
      val result = get(Stepwise.complete(p.work, q)).validation
      assertEquals(result.fitted.coefficients, Vector(2.0, 3.0))
      assertEquals(result.evaluation.rows.map(_.result), Vector(Right((7.0, 1.0))))
      assertEquals(get(p.run).validation.evaluation.rows, result.evaluation.rows)
  }

  test(
    "input identity includes excluded rows even when training and evaluation identities agree"
  ) {
    val a = get(TemplateFitPlan.of(split(99.0)))
    val b = get(TemplateFitPlan.of(split(100.0)))
    assertEquals(a.split.training.hash, b.split.training.hash)
    assertEquals(a.split.heldOut.hash, b.split.heldOut.hash)
    assertNotEquals(a.inputHash, b.inputHash)
    assertNotEquals(a.planHash, b.planHash)
    assertNotEquals(a.inputRef, b.inputRef)
  }

  test("form covers held-out selection and rebuilds the complete description") {
    val p = plan; val form = new TemplateFitForm(p)
    assertEquals(get(p.inspect).description, p.description)
    assertEquals(form.views, form.fields.map(_.view))
    form.fields.foreach { field =>
      assertEquals(
        field.restore(form.values.get(field.view.id)),
        Right(form.values.get(field.view.id))
      )
      assertEquals(form.validate(field.view.id, form.values.get(field.view.id)), Right(()))
    }
    assert(form.validate(form.heldOutGroups.view.id, RawValue.Number("1")).isLeft)
    val unknown = get(FieldId.of("unknown-template-field"))
    assertEquals(
      form.validate(unknown, RawValue.Absent),
      Left(FieldError.UnknownField(unknown))
    )
    val restored = get(get(form.parse(form.values)).plan(p))
    assertEquals(restored.description, p.description)
    assertEquals(p.diff(restored), Vector.empty)
    assertEquals(
      TemplateFitRecipe(Vector("held", "held")).plan(p),
      Left(TemplateError.SplitGroups(Vector("held", "held")))
    )
    assertEquals(
      TemplateFitRecipe(Vector.empty).plan(p),
      Left(TemplateError.SplitGroups(Vector.empty))
    )
  }

  test("preflight binds source rows and explicitly leaves fitting and evaluation unchecked") {
    val p     = plan
    val ready = p.preflight(Some(p.inputRef))
    assertEquals(ready.family, RecipeFamily.TemplateFit)
    assertEquals(ready.availability, Availability.Ready)
    assertEquals(p.preflight(None).availability, Availability.Unavailable)
    val other = get(TemplateFitPlan.of(split(100.0)))
    assertEquals(p.preflight(Some(other.inputRef)).availability, Availability.Unavailable)
    assertEquals(
      ready.notChecked,
      Vector(UncheckedAspect.TemplateFitting, UncheckedAspect.TemplateEvaluation)
    )
  }

  test("historical imported-lm design cannot be admitted as a native fit plan") {
    val native   = split()
    val imported =
      get(TemplateSplit.of(TemplateDesign.importedLm(basis), native.rows, Set("held")))
    assertEquals(
      TemplateFitPlan.of(imported).left.toOption,
      Some(TemplateError.Route(imported.design.method, "native fit plan"))
    )
  }

  test("the same family admits and executes a checked training-mean map design") {
    val frame                 = get(Frame.screen("template-plan-map", 2, 1));
    val grid                  = get(Grid.over(frame, 2, 1))
    def mass(values: Double*) =
      val cells = IArray.from(values)
      get(
        Surface
          .intensity(grid, cells, Provenance.raw(ContentHash.of(cells)))
          .flatMap(_.normalised)
      )
    val rows = Vector(
      get(TemplateObservation.of("a", "train", mass(1, 0), 2.0, Some("a"))),
      get(TemplateObservation.of("b", "train", mass(0, 1), 3.0, Some("b"))),
      get(TemplateObservation.of("c", "held", mass(1, 1), 4.0, Some("c")))
    )
    val input = get(
      TemplateSplit.of(
        get(TemplateDesign.meanMap[Px]("participant", "score")),
        rows,
        Set("held")
      )
    )
    val p = get(TemplateFitPlan.of(input))
    assertEquals(
      get(p.run).validation.evaluation.rows,
      get(Template.crossValidate(input)).evaluation.rows
    )
    assert(get(p.run).validation.fitted.template.nonEmpty)
    assertEquals(get(p.inspect).description, p.description)
  }

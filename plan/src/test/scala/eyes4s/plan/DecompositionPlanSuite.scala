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

package eyes4s.decompositionconsumer

import eyes4s.compare.ComparisonQuantum
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class DecompositionPlanSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                             = get(Frame.screen("decomposition-plan", 3, 1))
  private val grid                              = get(Grid.over(frame, 3, 1))
  private def mass(values: Double*): Mass[Px]   =
    val cells = IArray.from(values)
    get(
      Surface
        .intensity(grid, cells, Provenance.raw(ContentHash.of(cells)))
        .flatMap(_.normalised)
    )
  private val response   = mass(1, 3, 0)
  private val predictors = get(
    PredictorSet.of(
      Vector(
        get(PredictorId.of("a")) -> mass(1, 0, 0),
        get(PredictorId.of("b")) -> mass(0, 1, 0)
      )
    )
  )
  private val rank  = RelativeRankTolerance.default
  private val dual  = RelativeDualTolerance.default
  private val specs = Vector(
    DecompositionSpec.Ols(Intercept.Exclude, rank),
    DecompositionSpec.NonNegative(rank, dual),
    DecompositionSpec.Mixture(Intercept.Exclude, rank, dual)
  )

  test("every method has one honest whole-surface step and preserves complete input identity") {
    specs.foreach { spec =>
      val p = get(DecompositionPlan.of(response, predictors, spec))
      assertEquals(p.work.stage, DecompositionStage.SolvingWholeSurface)
      for amount <- Vector(1, Int.MaxValue) do
        val q = WorkQuanta(get(PairQuantum.of(amount)), ComparisonQuantum.default)
        get(p.work.advance(q)) match
          case WorkStep.Done(1, result) =>
            assertEquals(result.inputHash, p.inputHash)
            assertEquals(result.planHash, p.planHash)
            result.fit match
              case DecompositionFit.Ols(value) =>
                assertEquals(value.coefficients.map(_._2), Vector(0.25, 0.75))
              case DecompositionFit.NonNegative(value) =>
                assert(value.coefficients.forall(_._2 >= 0))
              case DecompositionFit.Mixture(value) => assert(value.weights.forall(_._2 >= 0))
          case other => fail(s"$other")
      assert(get(Stepwise.complete(p.work, WorkQuanta.default)).inputHash == p.inputHash)
    }
  }

  test("plan review names the exact changed solver setting") {
    import Provenance.Param.*
    val before = get(DecompositionPlan.of(response, predictors, specs.head))
    val after  = get(
      DecompositionPlan.of(response, predictors, DecompositionSpec.Ols(Intercept.Include, rank))
    )
    assertEquals(before.diff(before), Vector.empty)
    assertEquals(
      before.diff(after),
      Vector(
        PlanChange(
          "spec",
          Vector(Text("ols"), Text("Exclude"), Num(rank.value)),
          Vector(Text("ols"), Text("Include"), Num(rank.value))
        )
      )
    )
    assertEquals(
      after.diff(before),
      before.diff(after).map(change => PlanChange(change.field, change.after, change.before))
    )
  }

  test("all typed variant fields restore and rebuild the scientific description") {
    val form = new DecompositionForm
    assertEquals(form.views, form.fields.map(_.view))
    specs.foreach { spec =>
      val p = get(DecompositionPlan.of(response, predictors, spec))
      assertEquals(get(p.inspect).description, p.description)
      form.fields.foreach { field =>
        assertEquals(
          field.restore(form.values(p).get(field.view.id)),
          Right(form.values(p).get(field.view.id))
        )
        assertEquals(form.validate(field.view.id, form.values(p).get(field.view.id)), Right(()))
      }
      assertEquals(get(get(form.parse(form.values(p))).plan(p)).description, p.description)
    }
    assert(!NumericBounds.openUnit.contains(0.0))
    assert(!NumericBounds.openUnit.contains(1.0))
    assert(NumericBounds.openUnit.contains(0.5))
  }

  test("raw boundary tolerances refuse instead of silently clamping") {
    val form     = new DecompositionForm
    val original = get(DecompositionPlan.of(response, predictors, specs(1)))
    val raw      = form.values(original).get(form.spec.view.id)
    def withRank(value: String): RawValue = raw match
      case RawValue.Variant(kind, parts) =>
        RawValue.Variant(
          kind,
          parts.map { (id, part) =>
            id -> (if id.value == "rankTolerance" then RawValue.Number(value) else part)
          }
        )
      case _ => fail("expected variant")
    assert(form.spec.parse(withRank("0")).isLeft)
    assert(form.spec.parse(withRank("1")).isLeft)
    assert(form.validate(form.spec.view.id, withRank("0")).isLeft)
    val unknown = get(FieldId.of("unknown-decomposition-field"))
    assertEquals(
      form.validate(unknown, RawValue.Absent),
      Left(FieldError.UnknownField(unknown))
    )
  }

  test(
    "input identity includes actual cells and predictor order even with unchanged provenance"
  ) {
    val p       = get(DecompositionPlan.of(response, predictors, specs.head))
    val changed = get(Surface.mass(grid, IArray(0.5, 0.5, 0.0), response.provenance))
    assertNotEquals(
      get(DecompositionPlan.of(changed, predictors, specs.head)).inputHash,
      p.inputHash
    )
    val reversed = get(PredictorSet.of(predictors.entries.reverse))
    assertNotEquals(
      get(DecompositionPlan.of(response, reversed, specs.head)).inputHash,
      p.inputHash
    )
    assertNotEquals(
      get(DecompositionPlan.of(response, predictors, specs(1))).planHash,
      p.planHash
    )
  }

  test(
    "geometry refuses by operand; preflight checks input without promising a successful solve"
  ) {
    val otherGrid = get(Grid.over(get(Frame.screen("other-decomposition-frame", 3, 1)), 3, 1))
    val other     = get(Surface.mass(otherGrid, IArray(0.25, 0.75, 0.0), response.provenance))
    val refused   = DecompositionPlan.of(other, predictors, specs.head).left.toOption.get
    assert(refused.message.contains("response"))
    val p = get(DecompositionPlan.of(response, predictors, specs.head))
    assertEquals(p.preflight(Some(p.inputRef)).availability, Availability.Ready)
    assertEquals(p.preflight(None).availability, Availability.Unavailable)
    assertEquals(
      p.preflight(Some(p.inputRef)).notChecked,
      Vector(UncheckedAspect.SurfaceSolving)
    )
  }

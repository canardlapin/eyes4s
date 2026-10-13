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

package eyes4s.studio.core.real

import eyes4s.codec.FormCodecs
import eyes4s.design.Trials
import eyes4s.kernel.{Frame, Unit2D}
import eyes4s.plan.{FieldId, FormValues, RawValue, StudyInput, TrialKey as CoreKey}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import io.circe.syntax.*

class RecipeFormServiceSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val document                          = get(StoryMoments.t2)
  private val context                           = document.draftContext.get
  private val request = RecipeEditRequest(context.id, context.dataset, context.recipe)
  private val screen  = get(Frame.screen("screen", 1920, 1080))
  private val input   = StudyInput[CoreKey, Unit2D.Px](Trials(Vector.empty))
  private def serve(r: RecipeEditRequest) = get(RealRecipeForm.serve(r, screen, input, None))
  private def id(s: String)               = get(FieldId.of(s))
  private def edit(values: FormValues)    = serve(
    request.copy(values = Some(get(FormCodecs.values.encode(values))))
  )

  test("served descriptors, sentence roles and scale advice come from the library") {
    val result    = serve(request)
    val (view, _) = get(result.decoded)
    assertEquals(result.accepted, Some(request.recipe))
    assert(view.fields.exists(_.id.value == "pairing"))
    assert(view.fields.exists(_.id.value == "expectedUnmatched"))
    assert(result.sentence.exists(_.role.contains("Query")))
    assert(result.sentence.exists(_.role.contains("Reference")))
    assertEquals(result.cellDegrees, Some(RecipeCell(16.0 / 35.0, 16.0 / 35.0)))
    assertEquals(
      result.warnings.map(w => w.scale -> w.kind),
      Vector(0 -> "belowCells", 4 -> "nearUniform")
    )
    assertEquals(get(io.circe.parser.decode[RecipeFormResult](result.asJson.noSpaces)), result)
  }

  test("the displayed draft diff is the library's structural plan diff") {
    val before =
      request.recipe.copy(scales = get(ScaleSet.of(request.recipe.scales.values.init)))
    val result = serve(request.copy(compareTo = Some(before)))
    assertEquals(result.differences, Vector("scales +8°"))
    assertEquals(result.differenceProblem, None)
  }

  test("incomplete numeric text survives validation and cannot become a recipe") {
    val (_, values) = get(serve(request).decoded)
    val incomplete  = values.updated(
      id("grid"),
      RawValue.Group(
        Vector(
          id("columns") -> RawValue.Number("-"),
          id("rows")    -> RawValue.Number("48")
        )
      )
    )
    val result = edit(incomplete)
    assertEquals(result.accepted, None)
    assert(result.problems.exists(_.field.contains("columns")))
    assertEquals(get(result.decoded)._2, incomplete)
    assertEquals(request.recipe, context.recipe)
  }

  test("a valid edit changes only its parsed recipe fields and keeps immutable identities") {
    val (_, values) = get(serve(request).decoded)
    val updated     = edit(
      values.updated(
        id("grid"),
        RawValue.Group(
          Vector(
            id("columns") -> RawValue.Number("32"),
            id("rows")    -> RawValue.Number("24")
          )
        )
      )
    )
    assertEquals(updated.accepted, Some(request.recipe.copy(grid = get(GridSize.of(32, 24)))))
    assertEquals(updated.cellDegrees, Some(RecipeCell(32.0 / 35.0, 32.0 / 35.0)))
  }

  test("a rule missing from a new dataset remains editable and can be cleared") {
    val rule    = get(eyes4s.plan.ExpectedUnmatchedRule.of("old_new", Vector("new")))
    val req     = request.copy(recipe = request.recipe.copy(expectedUnmatched = Some(rule)))
    val blocked = serve(req)
    assertEquals(blocked.accepted, None)
    val (form, values) = get(blocked.decoded)
    assert(form.fields.exists(_.id.value == "expectedUnmatched"))
    val cleared  = values.updated(id("expectedUnmatched"), RawValue.Absent)
    val restored = serve(req.copy(values = Some(get(FormCodecs.values.encode(cleared)))))
    assertEquals(restored.accepted, Some(request.recipe))
  }

  test("the editor cannot inject trial assignments or silently discard unknown fields") {
    val (_, values) = get(serve(request).decoded)
    val unknown     = edit(values.updated(id("newScientificField"), RawValue.Text("x")))
    assertEquals(unknown.accepted, None)
    assertEquals(unknown.problems.map(_.field), Vector("newScientificField"))
    val selected = values.updated(
      id("expectedUnmatched"),
      RawValue.Group(
        Vector(
          id("column")      -> RawValue.Text("old_new"),
          id("values")      -> RawValue.Items(Vector(RawValue.Text("new"))),
          id("assignments") -> RawValue.Items(Vector.empty)
        )
      )
    )
    val result = edit(selected)
    assertEquals(result.accepted, None)
    assert(result.problems.exists(_.message.contains("inventory")))
  }

  test(
    "a declared fixation cross round-trips through the served form without an inferred position"
  ) {
    val (_, values) = get(serve(request).decoded)
    val raw         = values.updated(
      id("initialFixations"),
      RawValue.Variant(
        "dropLeadingInClosedDisc",
        Vector(
          id("crossX") -> RawValue.Number("960"),
          id("crossY") -> RawValue.Number("540"),
          id("radius") -> RawValue.Number("12")
        )
      )
    )
    val result = edit(raw)
    val recipe = result.accepted.getOrElse(fail(result.problems.toString))
    assertEquals(
      recipe.initialFixations,
      InitialFixationChoice.DropLeadingAtScreenCross(
        get(ScreenCross.of(960, 540)),
        get(CrossRadius.of(12))
      )
    )
    assertEquals(serve(request.copy(recipe = recipe)).accepted, Some(recipe))
  }

  test(
    "a legacy policy missing its cross position stays editable without inventing coordinates"
  ) {
    val req = request.copy(recipe =
      request.recipe.copy(initialFixations =
        InitialFixationChoice.DropLeadingNearCross(get(CrossRadius.of(1.5)))
      )
    )
    val result = serve(req)
    assertEquals(result.accepted, None)
    val (_, values) = get(result.decoded)
    assert(result.problems.exists(_.field.contains("crossX")))
    val cleared =
      values.updated(id("initialFixations"), RawValue.Variant("keepAll", Vector.empty))
    assertEquals(
      serve(req.copy(values = Some(get(FormCodecs.values.encode(cleared))))).accepted,
      Some(request.recipe.copy(initialFixations = InitialFixationChoice.KeepAll))
    )
  }

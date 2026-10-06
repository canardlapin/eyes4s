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

package eyes4s.studio.core.preset

import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments

class InitialRecipeSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val original                          =
    get(StoryMoments.t2).dataset(StoryMoments.r3).getOrElse(fail("no dataset"))

  test("first recipes use declared geometry and every offered preset's actual conventions") {
    val geometry = get(
      Geometry.of(
        get(ScreenSize.of(800, 600)),
        get(ImagePlacement.of(200, 100, 400, 300)),
        get(DeclaredPixelsPerDegree.of(80.0))
      )
    )
    val dataset = original.copy(geometry = geometry)
    RecipePresets.all.foreach { preset =>
      val (recipe, fields) = get(InitialRecipe.of(dataset, preset.preset))
      assertEquals(recipe.window, Some(get(AnalysisWindow.of(200.0, 100.0, 600.0, 400.0))))
      assertEquals(recipe.angularScale, Some(geometry.pixelsPerDegree))
      assertEquals(recipe.grid, get(GridSize.of(64, 48)))
      assertEquals(recipe.scales.values.map(_.degrees), Vector(0.5, 1.0, 2.0, 4.0))
      assertEquals(recipe.input, None)
      assert(preset.holds(recipe))
      assertEquals(fields.preset, preset.preset)
      assertEquals(
        fields.name.value,
        s"${preset.phases.reference.label} → ${preset.phases.focal.label}"
      )
    }
  }

  test("custom is an edited recipe rather than an invented initial configuration") {
    assertEquals(
      InitialRecipe.of(original, Preset.Custom),
      Left(DocumentError.PresetWithoutDefaults(Preset.Custom, original.id))
    )
  }

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

package eyes4s.studio.app.analysis

import eyes4s.studio.app.StoryModels
import eyes4s.studio.core.document.*

class RecipeEditorSuite extends munit.FunSuite:
  private val document = StoryModels.t2
  private val base = PresetPicker.edited(document).get._2
  private val fields = RecipeEditor.input(base)

  test("an unchanged form preserves every recipe field") {
    assertEquals(RecipeEditor.parse(base, fields), Right(base))
    assertEquals(RecipeEditor.command(document, fields), Right(None))
  }
  test("incomplete, nonfinite, duplicate and inverted values name their operands") {
    Vector(fields.copy(columns = "1.5"), fields.copy(scales = "NaN"),
      fields.copy(scales = "1, 1"), fields.copy(xMin = "900", xMax = "2"),
      fields.copy(reference = fields.focal), fields.copy(pixelsPerDegree = "0"),
      fields.copy(xMin = "")).foreach { invalid =>
      assert(RecipeEditor.parse(base, invalid).left.exists(_.message.nonEmpty))
    }
  }
  test("phase grid crop and kernel edits are one typed change with other policies intact") {
    val edited = fields.copy(focal = "Cue", reference = "Study", columns = "60", rows = "40",
      scales = "0.75, 1.25", xMin = "10", yMin = "20", xMax = "310", yMax = "220")
    val result = RecipeEditor.parse(base, edited).toOption.get
    assertEquals(result.phases.focal.label, "Cue")
    assertEquals(result.grid.columns, 60)
    assertEquals(result.scales.values.map(_.degrees), Vector(.75, 1.25))
    assertEquals(result.window.map(_.xMin), Some(10.0))
    assertEquals(result.method, base.method)
    assertEquals(result.controls, base.controls)
    assert(RecipeEditor.command(document, edited).toOption.flatten.nonEmpty)
    val full = RecipeEditor.parse(base, fields.copy(xMin="",yMin="",xMax="",yMax="")).toOption.get
    assertEquals(full.window, None)
    assertEquals(full.offWindow, None)
  }

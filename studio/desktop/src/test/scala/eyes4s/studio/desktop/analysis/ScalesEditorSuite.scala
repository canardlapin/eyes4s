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

package eyes4s.studio.desktop.analysis

import eyes4s.kernel.{Frame, Unit2D}
import eyes4s.plan.*
import eyes4s.studio.desktop.shell.KeyboardFocus
import javafx.scene.input.KeyCode
import eyes4s.design.Trials
import eyes4s.studio.core.backend.{RecipeEditRequest, RecipeFormResult}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.real.RealRecipeForm
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.StudioFxSuite
import javafx.scene.layout.VBox

class ScalesEditorSuite extends StudioFxSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val document                          = get(StoryMoments.t2)
  private val context                           = document.draftContext.get
  private def served: RecipeFormResult          = get(
    RealRecipeForm.serve(
      RecipeEditRequest(context.id, context.dataset, context.recipe),
      get(Frame.screen("screen", 1920, 1080)),
      StudyInput[TrialKey, Unit2D.Px](Trials(Vector.empty)),
      None
    )
  )
  private def id(value: String) = get(FieldId.of(value))

  fxStage.test(
    "scale chips share one Tab stop and keyboard removal keeps the remaining scales"
  ) { fx =>
    val result      = served
    val (_, values) = get(result.decoded)
    var edited      = Option.empty[RawValue]
    val chips       = runOnFx(ScaleChips(value => edited = Some(value)))
    val root        = runOnFx {
      assert(chips.show(values.get(id("scales")), Set(.5, 1, 2, 4), Some(result)))
      val root = VBox(chips.node)
      root.getStylesheets.setAll(get(StudioStyles.stylesheets(Theme.Light))*)
      root
    }
    runOnFx {
      val labels = chips.node.lookupAll(".label").toArray.toVector.collect {
        case l: javafx.scene.control.Label => l.getText
      }
      assert(labels.exists(_.contains("cell 0.46° × 0.46°")))
      assert(labels.exists(t => t.contains("σ 0.5°") && t.contains("σ 8°")))
    }
    fx.show(root)
    fx.awaitLayout()
    runOnFx {
      assertEquals(chips.bar.segmentNodes.count(_.isFocusTraversable), 1)
      assertEquals(chips.bar.segmentNodes.last.getText, "8° · new")
      KeyboardFocus.byKeyboard(chips.bar.segmentNodes.last)
    }
    fx.robot.press(KeyCode.DELETE)
    runOnFx {
      val expected = values.get(id("scales")) match
        case RawValue.Items(items) => RawValue.Items(items.init)
        case other                 => other
      assertEquals(edited, Some(expected))
    }
  }

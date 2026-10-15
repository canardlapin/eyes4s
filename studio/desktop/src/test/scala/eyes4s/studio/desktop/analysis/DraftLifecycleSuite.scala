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

import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.core.command.Command
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.harness.{SnapshotScale, StudioTheme}
import javafx.scene.control.{Button, Label, TextArea}
import eyes4s.studio.app.layout.StudioLayouts
import scala.jdk.CollectionConverters.*

class DraftLifecycleSuite extends ShellFxSuite:
  fxStage.test(
    "served edits stay in the draft until Save and run, with earlier run bindings preserved"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Analysis)
    eventually(fx, "recipe and preflight") { w.recipe.editor.ready && w.preflight.runButton._2 }
    val original = runOnFx(w.runtime.model.document)
    runOnFx {
      w.recipe.editor.descriptor.textFields("grid.columns").setText("-")
      assert(!w.preflight.runButton._2)
    }
    eventually(fx, "invalid field result") { !w.recipe.editor.state.pending }
    runOnFx {
      assert(w.recipe.editor.state.result.exists(_.accepted.isEmpty))
      assert(w.recipe.editor.apply.isDisabled)
      assertEquals(w.runtime.model.document, original)
      w.recipe.editor.descriptor.textFields("grid.columns").setText("32")
    }
    eventually(fx, "valid field result") { !w.recipe.editor.apply.isDisabled }
    runOnFx {
      assert(!w.preflight.runButton._2)
      w.recipe.editor.apply.fire()
    }
    eventually(fx, "applied settings and refreshed preflight") {
      w.recipe.editor.ready && w.preflight.runButton._2
    }
    val edited = runOnFx(w.runtime.model.document)
    assertEquals(edited.analyses, original.analyses)
    assertEquals(edited.runs, original.runs)
    assertEquals(edited.draftRecipe.map(_.grid.columns), Some(32))
    dispatch(fx, w, Intent.FocusPane(StudioLayouts.recipeDiff))
    runOnFx {
      val text = w.recipe.diffNode
        .lookupAll(".label")
        .asScala
        .collect { case l: Label => l.getText }
        .mkString(" ")
      assert(text.contains("grid"), text)
    }
    dispatch(fx, w, Intent.FocusPane(StudioLayouts.recipeDescription))
    runOnFx {
      w.preflight.descriptionNode
        .lookup(".text-area")
        .asInstanceOf[TextArea]
        .setText("Declared grid comparison")
      val button = w.preflight.node.lookup(".preflight-run").asInstanceOf[Button]
      assert(!button.isDisabled)
      button.fire()
    }
    eventually(fx, "immutable revision") { w.runtime.model.document.draft.isEmpty }
    val saved = runOnFx(w.runtime.model.document)
    assertEquals(saved.analyses.size, original.analyses.size + 1)
    assertEquals(saved.analyses.last.recipe, edited.draftRecipe.get)
    assertEquals(saved.analyses.last.studio.description, "Declared grid comparison")
    assertEquals(saved.runs.take(original.runs.size), original.runs)
    assertEquals(saved.runs.last.analysis, saved.analyses.last.id)
    assertEquals(saved.runs.size, original.runs.size + 1)
  }

  fxStage.test("discarding a draft leaves saved recipes and runs unchanged") { fx =>
    val w = boot(fx, StoryModels.t2Analysis)
    eventually(fx, "recipe and preflight") { w.recipe.editor.ready && w.preflight.runButton._2 }
    val before = runOnFx(w.runtime.model.document)
    fx.snapshot(StudioTheme.Light, List(SnapshotScale.X1))
    dispatch(fx, w, Intent.Dispatch(Command.SetTheme(eyes4s.studio.core.document.Theme.Dark)))
    fx.snapshot(StudioTheme.Dark, List(SnapshotScale.X1))
    runOnFx {
      import eyes4s.studio.app.tokens.{Tokens, Theme, ThemedToken}
      import javafx.scene.paint.Color
      import javafx.scene.text.Text
      val ink      = Color.web(Tokens.themed(Theme.Dark, ThemedToken.Ink).hexRgb)
      val hint     = Color.web(Tokens.themed(Theme.Dark, ThemedToken.Ink3).hexRgb)
      val sentence =
        w.recipe.editor.node.lookup(".recipe-sentence").asInstanceOf[javafx.scene.text.TextFlow]
      val plain = sentence.getChildren.asScala.collect {
        case t: Text if !t.getStyleClass.asScala.exists(_.startsWith("recipe-token-")) => t
      }
      assert(plain.nonEmpty)
      plain.foreach(t => assertEquals(t.getFill, ink))
      val prompt = w.recipe.editor.scales.entry
        .lookupAll("Text")
        .asScala
        .collectFirst {
          case t: Text if t.getText == w.recipe.editor.scales.entry.getPromptText => t
        }
        .getOrElse(fail("no scale prompt"))
      assertEquals(prompt.getFill, hint)
    }
    dispatch(fx, w, Intent.Dispatch(Command.DiscardDraft))
    val after = runOnFx(w.runtime.model.document)
    assertEquals(after.draft, None)
    assertEquals(after.analyses, before.analyses)
    assertEquals(after.runs, before.runs)
  }

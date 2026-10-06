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

package eyes4s.studio.desktop.figures

import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.app.appearance.Appearance
import eyes4s.studio.app.tokens.Wcag
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.shell.{A11yChecks, ShellFxSuite}
import javafx.scene.control.{Button, Label}
import javafx.scene.paint.Color
import javafx.scene.text.Text

import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** The methods.md and "Diff vs generated" panes in the studio window (ticket
  * S9.4; Figures.dc.html) at story moment t2: Figure 1's methods from run 7.
  */
class FigureMethodsFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  private val Replay = "D measures spatial correspondence, not sequential replay."
  private val Edited = "D measures where gaze went, not the order it went there."

  private def generated(fx: FxStage, w: StudioWindow): String =
    eventually(fx, "the methods text is generated") {
      w.figures.vm.methods.exists(_.text.isRight)
    }
    runOnFx(w.figures.methodsEditor.getText)

  private def diffTexts(w: StudioWindow): Vector[String] =
    runOnFx(
      w.figures.diffLines.getChildren.asScala.toVector.collect { case l: Label => l.getText }
    )

  private def press(w: StudioWindow, name: String): Unit =
    runOnFx(
      w.figures.methodsNode
        .lookupAll(".button")
        .asScala
        .toVector
        .collectFirst { case b: Button if b.getAccessibleText == name => b }
        .getOrElse(fail(s"no button '$name'"))
        .fire()
    )

  Vector(Appearance.Light, Appearance.Dark).foreach { appearance =>
    fxStage.test(
      s"methods pending palette and first enabled frame remain readable in $appearance"
    ) { fx =>
      val w = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
      generated(fx, w)
      dispatch(fx, w, Intent.SetAppearance(appearance))
      runOnFx {
        val editor = w.figures.methodsEditor
        // Hold the real control's pending palette, then enable it without
        // a CSS pulse: native method generation can make this transition
        // between layout and the next pulse. Both palettes must be legible.
        editor.setDisable(true)
        w.root.applyCss()
        val texts = A11yChecks.all(editor).collect {
          case text: Text if A11yChecks.shown(text) && text.getText.nonEmpty => text
        }
        assert(texts.nonEmpty, "the methods editor must actually draw its text")
        texts.foreach { text =>
          val pixels = A11yChecks
            .pixels(text, text.getFill.asInstanceOf[Color], fx.scene)
            .fold(fail(_), identity)
          val contrast =
            pixels.map((glyph, background) => A11yChecks.ratio(glyph, background)).min
          assert(
            contrast >= Wcag.TextMinimum,
            s"$appearance pending methods contrast $contrast"
          )
        }
        assertEquals(editor.getOpacity, 1.0)
        editor.setDisable(false)
        assertEquals(A11yChecks.lowContrast(editor), Vector.empty[String])
      }
    }
  }

  fxStage.test("the methods text is generated from run 7 and set in the prose face") { fx =>
    val w    = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
    val text = generated(fx, w)
    assert(text.startsWith("Fixations (fixations.csv, 11,520 records; dataset r3)"), text)
    assert(text.endsWith("Analysis rev 4, run 7; eyes4s 0.1."), text)
    assertEquals(
      runOnFx(w.figures.methodsEditor.getFont.getFamily),
      "Source Serif 4"
    )
    assertEquals(
      w.figures.vm.methods.map(_.heading),
      Some("Generated from run 7 and reporting spec “By retrieval response”")
    )
  }

  fxStage.test("an edit is kept through Regenerate and shown in the diff pane") { fx =>
    val w    = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
    val text = generated(fx, w)
    runOnFx(w.figures.methodsEditor.setText(text.replace(Replay, Edited)))
    assert(
      w.figures.vm.methods.exists(_.heading.endsWith("· 1 sentence edited by you")),
      w.figures.vm.methods.map(_.heading)
    )
    val diff = diffTexts(w)
    assert(diff.contains(s"− $Replay"), diff)
    assert(diff.contains(s"+ $Edited"), diff)
    press(w, "Regenerate")
    assertEquals(runOnFx(w.figures.methodsEditor.getText), text.replace(Replay, Edited))
    assertEquals(
      w.figures.vm.methods.flatMap(_.status),
      Some("The generated text has not changed; your edits are kept.")
    )
  }

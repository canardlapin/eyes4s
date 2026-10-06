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
import eyes4s.studio.app.figures.MethodsCopy
import eyes4s.studio.core.backend.{DatasetRevision, RunId}
import eyes4s.studio.core.document.Theme
import eyes4s.studio.core.figures.MethodsFacts
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.{StudioWindow, ThemeHost}
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.shell.{A11yChecks, ShellFxSuite}
import javafx.scene.control.{Button, Label}
import javafx.scene.paint.Color
import javafx.scene.text.Text
import javafx.scene.layout.VBox
import java.util.concurrent.{CompletableFuture, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

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

  /** All data comes from the existing session; only delivery of its actual
    * methods facts is gated, independently of the summary/report callbacks.
    */
  private final class GatedMethods(delegate: FigureInputs) extends FigureInputs:
    export delegate.{
      stimuli,
      fixations,
      summary,
      report,
      references,
      displays,
      mapGrid,
      status,
      bundle,
      save
    }
    val facts            = CompletableFuture[Either[String, MethodsFacts]]()
    private val callback = AtomicReference[Option[Either[String, MethodsFacts] => Unit]](None)
    def methods(
        run: RunId,
        dataset: DatasetRevision,
        done: Either[String, MethodsFacts] => Unit
    ): Unit =
      callback.set(Some(done))
      delegate.methods(run, dataset, answer => facts.complete(answer): Unit)
    def deliver(answer: Either[String, MethodsFacts]): Unit =
      callback.get().getOrElse(fail("no actual methods request was registered"))(answer)

  fxStage.test("no figure is explained once without a duplicate methods status") { fx =>
    val w = boot(fx, StoryModels.firstRun, StoryMoment.T2)
    runOnFx {
      assertEquals(w.figures.vm.methods, None)
      assert(w.figures.methodsEditor.isDisabled)
      val explanations = A11yChecks.all(w.figures.methodsNode).collect {
        case label: Label
            if label.isVisible && label.isManaged && label.getText == MethodsCopy.NoFigure =>
          label
      }
      assertEquals(explanations.size, 1)
    }
  }

  fxStage.test(
    "actual delayed methods facts block editing visibly, then late facts preserve authored text"
  ) { fx =>
    val w     = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
    val gated = GatedMethods(FigureInputs.of(w.session, NavigatorDisplays.golden, () => None))
    val (host, root) = runOnFx {
      val host = FiguresHost(() => w.runtime.model, w.runtime.dispatch, gated)
      val root = VBox(host.methodsNode)
      root.getStyleClass.add("studio-shell")
      root.getStylesheets.setAll(
        ThemeHost.sheets(Theme.Light).fold(e => fail(e.message), identity)*
      )
      host.sync(w.runtime.model)
      (host, root)
    }
    try
      fx.show(root)
      val actual = gated.facts.get(10, TimeUnit.SECONDS)
      assert(actual.isRight, actual)
      val waiting = MethodsCopy.reading(eyes4s.studio.core.fixture.StoryMoments.run7)
      runOnFx {
        assertEquals(host.vm.methods.map(_.text), Some(Left(waiting)))
        assert(host.methodsEditor.isDisabled)
        assertEquals(host.methodsEditor.getText, "")
        assertEquals(host.methodsEditor.getPromptText, waiting)
        val label = A11yChecks
          .all(host.methodsNode)
          .collectFirst {
            case label: Label if label.getText == waiting => label
          }
          .getOrElse(fail("no visible methods loading status"))
        assert(label.isVisible && label.isManaged && A11yChecks.shown(label))
        A11yChecks
          .all(host.methodsNode)
          .collectFirst {
            case button: Button if button.getAccessibleText == "Regenerate" => button
          }
          .getOrElse(fail("no regenerate control"))
          .fire()
        assertEquals(host.vm.methods.flatMap(_.status), Some(waiting))
      }
      val refused = "Methods facts temporarily unavailable"
      gated.deliver(Left(refused))
      eventually(fx, "a refused methods callback is visibly explained") {
        host.vm.methods.exists(_.text == Left(refused))
      }
      runOnFx {
        assert(host.methodsEditor.isDisabled)
        assert(A11yChecks.all(host.methodsNode).exists {
          case label: Label => label.getText == refused && label.isVisible && label.isManaged
          case _            => false
        })
      }
      gated.deliver(actual)
      eventually(fx, "genuine methods facts make the editor ready") {
        host.vm.methods.exists(_.text.isRight) && !host.methodsEditor.isDisabled
      }
      val original = runOnFx(host.methodsEditor.getText)
      assert(
        original.startsWith("Fixations (fixations.csv, 11,520 records; dataset r3)"),
        original
      )
      runOnFx {
        assertEquals(host.vm.methods.flatMap(_.status), None)
        assert(!A11yChecks.all(host.methodsNode).exists {
          case label: Label => label.getText == waiting && label.isVisible
          case _            => false
        })
        host.methodsEditor.requestFocus()
        host.methodsEditor.positionCaret(0)
      }
      fx.awaitLayout()
      fx.robot.typeText("Authored: ")
      val authored = "Authored: " + original
      assertEquals(runOnFx(host.methodsEditor.getText), authored)
      gated.deliver(actual)
      fx.awaitLayout()
      assertEquals(runOnFx(host.methodsEditor.getText), authored)
      assertEquals(runOnFx(host.vm.methods.flatMap(_.text.toOption)), Some(authored))
    finally runOnFx(host.dispose())
  }

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

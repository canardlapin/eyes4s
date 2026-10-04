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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.appearance.Appearance
import eyes4s.studio.app.tokens.{Theme, ThemedToken, Tokens, Wcag}
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import javafx.scene.control.{Menu, RadioMenuItem}
import javafx.scene.paint.Color
import scala.jdk.CollectionConverters.*

/** The rendered window against WCAG (ticket S10.5; DESIGN_SPEC §10), with
  * [[A11yChecks]]: every shown, enabled text in every perspective, light and
  * dark, reaches its contrast minimum over what is drawn behind it, and
  * every Tab stop changes how it is drawn when focused. TokenContrastSuite
  * checks the token pairs the spec names; this checks the pairs the window
  * actually draws. A popped-out dock window is held to the same checks and
  * to A11yTreeSuite's names.
  */
class A11yRenderSuite extends ShellFxSuite:

  private val perspectives: Vector[(String, () => AppModel, StoryMoment)] = Vector(
    ("data", () => StoryModels.t1Data, StoryMoment.T1),
    ("data-empty", () => StoryModels.firstRun, StoryMoment.T2),
    ("explore", () => StoryModels.t2Explore, StoryMoment.T2),
    ("analysis", () => StoryModels.t2Analysis, StoryMoment.T2),
    ("compare", () => StoryModels.t2Compare, StoryMoment.T2),
    ("compare-t3", () => StoryModels.t3Summary, StoryMoment.T3),
    ("figures", () => StoryModels.t2Figures, StoryMoment.T2)
  )

  private def dark(fx: FxStage, w: StudioWindow): Unit =
    runOnFx {
      val view = w.shell.menus.find(_.getText == "View").getOrElse(fail("no View menu"))
      view.getItems.asScala
        .collectFirst { case m: Menu if m.getText == "Appearance" => m }
        .flatMap(
          _.getItems.asScala.collectFirst {
            case r: RadioMenuItem
                if r.getId == s"view.appearance-${Appearance.Dark.toString.toLowerCase}" =>
              r
          }
        )
        .getOrElse(fail("no View › Appearance › Dark"))
        .fire()
    }
    fx.awaitLayout()

  private def fx(c: eyes4s.studio.app.tokens.Colour): Color =
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  test("the local luminance agrees with Wcag on a token pair") {
    val (ink3, surface) = (
      Tokens.themed(Theme.Light, ThemedToken.Ink3),
      Tokens.themed(Theme.Light, ThemedToken.Surface)
    )
    val ours = A11yChecks.ratio(A11yChecks.rgb(fx(ink3)), A11yChecks.rgb(fx(surface)))
    // JavaFX keeps a colour's channels as floats.
    val FloatChannels = 1e-6
    assertEqualsDouble(
      ours,
      Wcag.contrast(ink3, surface).fold(e => fail(e.message), identity),
      FloatChannels
    )
  }

  for
    (name, model, moment) <- perspectives
    theme                 <- Vector("light", "dark")
  do
    fxStage.test(s"$name, $theme: every shown text reaches its contrast minimum") { fx =>
      assumeFullStage(fx)
      val w = boot(fx, model(), moment)
      if theme == "dark" then dark(fx, w)
      // The walk reads the window's texts, not an empty scene.
      val texts = runOnFx(A11yChecks.texts(w.root).size)
      assert(texts >= 20, s"only $texts texts shown")
      assertEquals(runOnFx(A11yChecks.lowContrast(w.root)), Vector.empty[String])
    }

  for
    (name, model, moment) <- perspectives
    theme                 <- Vector("light", "dark")
  do
    fxStage.test(s"$name, $theme: every Tab stop is drawn differently when focused") { fx =>
      assumeFullStage(fx)
      val w = boot(fx, model(), moment)
      if theme == "dark" then dark(fx, w)
      val stops = runOnFx(A11yChecks.focusable(w.root))
      assert(stops.size >= 3, s"only ${stops.size} stops")
      assertEquals(runOnFx(A11yChecks.unmarked(w.root)), Vector.empty[String])
    }

  Vector("light", "dark").foreach { theme =>
    fxStage.test(s"a popped-out dock window, $theme: named, legible, its focus drawn") { fx =>
      val w     = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
      val stage = runOnFx(fx.scene.getWindow)
      val pane  = runOnFx(w.host.dock.state.groups.find(_.tabs.nonEmpty).get)
      runOnFx(w.host.dock.popOut(pane.id))
      fx.awaitLayout()
      if theme == "dark" then dark(fx, w)
      val popout = runOnFx(
        javafx.stage.Window.getWindows.asScala.find(win => !(win eq stage) && win.isShowing)
      ).getOrElse(fail("no popped-out window"))
      val root = runOnFx(popout.getScene.getRoot)
      assert(runOnFx(A11yChecks.texts(root).nonEmpty), "the popout shows no text")
      assert(runOnFx(A11yChecks.focusable(root).nonEmpty), "the popout has no Tab stop")
      assertEquals(runOnFx(A11yChecks.unlabelled(root)), Vector.empty[String])
      assertEquals(runOnFx(A11yChecks.lowContrast(root)), Vector.empty[String])
      assertEquals(runOnFx(A11yChecks.unmarked(root)), Vector.empty[String])
    }
  }

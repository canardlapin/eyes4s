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

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.vm.Shell
import eyes4s.studio.core.backend.{DiagnosticLevel, DiagnosticOrigin, StudioDiagnostic}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.StudioTheme
import javafx.scene.control.{Button, Label}
import javafx.scene.input.KeyCode
import javafx.scene.layout.{BorderStrokeStyle, Region}

import scala.jdk.CollectionConverters.*

/** The context strip (ticket S1.6) in the window: back and forward, live
  * crumbs that cross perspectives (E2E-17), and the freshness badge with the
  * draft chip or the newer run's chip. Snapshots go to
  * `target/studio-snapshots/ContextStripFxSuite/`.
  */
class ContextStripFxSuite extends ShellFxSuite:

  private def strip(w: StudioWindow): Vector[javafx.scene.Node] =
    runOnFx(w.shell.contextStrip.node.getChildren.asScala.toVector)

  private def backButton(w: StudioWindow): Button    = strip(w)(0).asInstanceOf[Button]
  private def forwardButton(w: StudioWindow): Button = strip(w)(1).asInstanceOf[Button]

  private def crumbs(w: StudioWindow): Vector[Button] = runOnFx(w.shell.contextStrip.crumbs)

  private def crumb(w: StudioWindow, label: String): Button =
    crumbs(w)
      .find(b => runOnFx(drawn(b)) == label)
      .getOrElse(fail(s"no crumb '$label' in ${crumbs(w).map(b => runOnFx(drawn(b)))}"))

  private def location(w: StudioWindow) = runOnFx(w.runtime.model.location)

  private def chips(w: StudioWindow): Vector[Label] = strip(w).drop(4).collect {
    case l: Label if runOnFx(l.getStyleClass.contains("freshness")) => l
  }

  private def dotOf(l: Label): Vector[String] =
    runOnFx(l.getGraphic.getStyleClass.asScala.toVector.filter(_.startsWith("tone-")))

  private def draftChip(w: StudioWindow): Option[Button] = strip(w).drop(4).collectFirst {
    case b: Button if runOnFx(b.getStyleClass.contains("draft-chip")) => b
  }

  private def assertMatchesModel(w: StudioWindow): Unit =
    assertNoDiff(texts(w).mkString("\n"), expected(w).mkString("\n"))

  fxStage.test("every crumb is a live button: a click lands on its prefix, Back returns") {
    fx =>
      assumeFullStage(fx)
      val w     = boot(fx, StoryModels.t2Explore)
      val start = location(w)
      val count = crumbs(w).size
      assertEquals(count, 7)
      (0 until count).foreach { i =>
        val model  = runOnFx(w.runtime.model)
        val target = Shell.crumbTarget(model, i).getOrElse(fail(s"crumb $i"))
        val vm     = Shell.context(model).trail(i)
        val button = crumbs(w)(i)
        assertEquals(runOnFx(button.getAccessibleText), vm.accessible, s"crumb $i")
        fx.robot.click(button)
        assertEquals(location(w), target, s"crumb $i")
        assertEquals(runOnFx(w.host.active).isDefined, true)
        assertMatchesModel(w)
        if !vm.current then
          fx.robot.click(backButton(w))
          assertEquals(location(w), start, s"back after crumb $i")
      }
  }

  fxStage.test(
    "E2E-17: fixation and record crumbs cross into Explore and back, by click and key"
  ) { fx =>
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t2Compare)
    assertEquals(runOnFx(w.host.active), Some("compare.query"))
    // From the pair, explain the record: the trail crosses into Explore.
    dispatch(fx, w, Intent.Explain(Place.At(StoryModels.record)))
    assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Explore)
    assertEquals(runOnFx(w.host.active), Some("explore"))
    val labels = crumbs(w).map(b => runOnFx(drawn(b)))
    assertEquals(
      labels.takeRight(2),
      Vector("enc_03 · fixation 6", "fixations.csv record 7,214")
    )
    assert(runOnFx(crumbs(w).last.getPseudoClassStates.contains(Fx.Current)))
    // The fixation crumb stays in Explore, at the fixation.
    fx.robot.click(crumb(w, "enc_03 · fixation 6"))
    assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Explore)
    assertEquals(runOnFx(drawn(crumbs(w).last)), "enc_03 · fixation 6")
    // The pair crumb crosses back into Compare's query layout, where it was.
    fx.robot.click(crumb(w, "pair ret_07 × enc_03"))
    assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Compare)
    assertEquals(runOnFx(w.host.active), Some("compare.query"))
    assertEquals(location(w), StoryModels.t2Compare.location)
    // ⌘[ ⌘[ retrace to the record; the Back button is the same command.
    fx.robot.press(KeyCode.OPEN_BRACKET, shortcut)
    assertEquals(runOnFx(drawn(crumbs(w).last)), "enc_03 · fixation 6")
    fx.robot.click(backButton(w))
    assertEquals(runOnFx(drawn(crumbs(w).last)), "fixations.csv record 7,214")
    assertEquals(runOnFx(w.host.active), Some("explore"))
    // ⌘] twice returns to Compare; Forward is then disabled.
    assert(runOnFx(!forwardButton(w).isDisabled))
    fx.robot.press(KeyCode.CLOSE_BRACKET, shortcut)
    fx.robot.press(KeyCode.CLOSE_BRACKET, shortcut)
    assertEquals(location(w), StoryModels.t2Compare.location)
    assert(runOnFx(forwardButton(w).isDisabled))
    assertMatchesModel(w)
  }

  fxStage.test("back and forward name their shortcuts and follow the history") { fx =>
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t2Compare)
    assertEquals(runOnFx(backButton(w).getAccessibleText), "Back (⌘[)")
    assertEquals(runOnFx(forwardButton(w).getAccessibleText), "Forward (⌘])")
    assert(runOnFx(!backButton(w).isDisabled && forwardButton(w).isDisabled))
    val before = location(w)
    fx.robot.click(backButton(w))
    assertNotEquals(location(w), before)
    assert(runOnFx(!forwardButton(w).isDisabled))
    fx.robot.click(forwardButton(w))
    assertEquals(location(w), before)
  }

  fxStage.test("t2: the badge's teal dot, and the dashed draft chip opens Analysis") { fx =>
    assumeFullStage(fx)
    val w     = boot(fx, StoryModels.t2Compare)
    val badge = chips(w)
    assertEquals(
      badge.map(l => runOnFx(drawn(l))),
      Vector("Analysis rev 4 · run 7 · data r3 · current")
    )
    assertEquals(dotOf(badge.head), Vector("tone-current"))
    val chip = draftChip(w).getOrElse(fail("no draft chip"))
    assertEquals(runOnFx(drawn(chip)), "Draft rev 5 · 1 change · ready")
    assertEquals(
      runOnFx(chip.getAccessibleText),
      "Draft rev 5 · 1 change · ready; review in Analysis"
    )
    val dashed = runOnFx(chip.getBorder.getStrokes.get(0).getTopStyle)
    assertNotEquals(dashed, BorderStrokeStyle.SOLID)
    assert(!dashed.getDashArray.isEmpty, "the draft chip's border is dashed")
    // The strip is 32 px and the chips fit in it.
    assertEqualsDouble(runOnFx(w.shell.contextStrip.node.getHeight), 32, 1)
    fx.snapshot(StudioTheme.Light)
    fx.robot.click(chip)
    assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Analysis)
    assertEquals(runOnFx(drawn(crumbs(w).last)), "Draft rev 5")
    // Explore is view-only: no draft chip there.
    fx.robot.press(KeyCode.DIGIT2, shortcut)
    assertEquals(draftChip(w), None)
  }

  fxStage.test("t3: the two-chip form: 'Showing …' with a hollow dot, then the running run") {
    fx =>
      assumeFullStage(fx)
      val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      val c = chips(w)
      assertEquals(
        c.map(l => runOnFx(drawn(l))),
        Vector("Showing analysis rev 4 · run 7 · data r3", "Rev 5 · run 8 running · 48%")
      )
      assertEquals(c.map(dotOf), Vector(Vector("tone-showing"), Vector("tone-running")))
      // The hollow dot: a ring of ink 2 around the surface.
      val fills = runOnFx(c.head.getGraphic.asInstanceOf[Region].getBackground.getFills.size)
      assertEquals(fills, 2)
      assertEquals(draftChip(w), None)
      assertMatchesModel(w)
      fx.snapshot(StudioTheme.Light)
      // Progress moves the chip, and only the chip: the crumbs are the same nodes.
      val before = crumbs(w)
      w.session.await(w.session.backend.advanceToPairs(StoryMoments.run8Job, 33_634L))
      eventually(fx, "the chip at 75%")(
        chips(w).lastOption.exists(l => drawn(l) == "Rev 5 · run 8 running · 75%")
      )
      assert(crumbs(w).zip(before).forall(_ eq _), "the trail was rebuilt by a progress tick")
  }

  private val lost = StudioDiagnostic(
    "studio-execution.lost-job",
    DiagnosticLevel.Error,
    DiagnosticOrigin.Host,
    Vector.empty,
    "lost"
  )

  fxStage.test("failed: run 8 fails; the badge stays current and a failed chip says so") { fx =>
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    w.session.await(w.session.backend.fail(StoryMoments.run8Job, Vector(lost, lost)))
    eventually(fx, "the failed chip")(
      chips(w).size == 2 && drawn(chips(w).last).contains("failed")
    )
    // Job events keep arriving after the first failed render; wait for quiet.
    eventually(fx, "the shell to match the model")(texts(w) == expected(w))
    val c = chips(w)
    assertEquals(runOnFx(drawn(c.head)), "Analysis rev 4 · run 7 · data r3 · current")
    assertEquals(dotOf(c.head), Vector("tone-current"))
    assertEquals(dotOf(c.last), Vector("tone-failed"))
    assertMatchesModel(w)
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("stale: showing run 5 on r2 marks the badge stale") { fx =>
    assumeFullStage(fx)
    val stale = AppModel
      .update(
        StoryModels.t2Compare,
        Intent.Dispatch(eyes4s.studio.core.command.Command.ShowRun(Some(StoryMoments.run5)))
      )
      ._1
    val w = boot(fx, stale)
    val c = chips(w)
    assertEquals(dotOf(c.head), Vector("tone-stale"))
    assert(runOnFx(drawn(c.head)).endsWith("· stale"), runOnFx(drawn(c.head)))
    assertMatchesModel(w)
  }

  fxStage.test("a focused crumb keeps its focus while unrelated updates render") { fx =>
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t2Explore)
    val c = crumbs(w)(2)
    runOnFx(c.requestFocus())
    fx.awaitLayout()
    assert(runOnFx(fx.scene.getFocusOwner eq c), "the crumb takes focus")
    val view = eyes4s.studio.core.selection.ViewId.of("explore.trial-view").toOption.get
    dispatch(fx, w, Intent.HoverOver(view, None))
    dispatch(fx, w, Intent.FocusPane(runOnFx(w.runtime.model.focusedPane)))
    assert(
      runOnFx((fx.scene.getFocusOwner eq c) && (c.getScene eq fx.scene)),
      "the crumb lost focus"
    )
  }

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

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.vm.Shell
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.StudioTheme
import javafx.scene.control.{Button, Label}
import javafx.scene.input.KeyCode
import javafx.scene.layout.BorderStrokeStyle

import scala.jdk.CollectionConverters.*

/** The draft banner (ticket S1.7): 30 px, dashed, only in Compare and
  * Figures; its words come from the plan diff and the job state; Review in
  * Analysis and Discard draft (with its confirmation) work. Snapshots go to
  * `target/studio-snapshots/DraftBannerFxSuite/`.
  */
class DraftBannerFxSuite extends ShellFxSuite:

  private def shown(w: StudioWindow): Boolean = runOnFx(w.shell.banner.node.isVisible)

  private def words(w: StudioWindow): Vector[String] = runOnFx(
    w.shell.banner.node.getChildren.asScala.toVector.collect { case l: Label => drawn(l) }
  )

  private def buttons(w: StudioWindow): Vector[Button] = runOnFx(
    w.shell.banner.node.getChildren.asScala.toVector.collect { case b: Button => b }
  )

  private def button(w: StudioWindow, label: String): Button =
    buttons(w).find(b => runOnFx(drawn(b)) == label).getOrElse(fail(s"no '$label' button"))

  /** The banner's words and buttons as the view-model gives them. */
  private def fromModel(w: StudioWindow): Vector[String] = runOnFx {
    Shell
      .banner(w.runtime.model)
      .toVector
      .flatMap(b => Vector(b.lead) ++ Option.when(b.detail.nonEmpty)(b.detail))
  }

  fxStage.test("t2: Compare and Figures only; 30 px, dashed; the words of the plan diff") {
    fx =>
      val w = boot(fx, StoryModels.t2Compare)
      assert(shown(w))
      assertEquals(
        words(w),
        Vector("Showing run 7 (analysis rev 4).", "Draft rev 5 adds σ 8° and has not been run.")
      )
      assertEquals(words(w), fromModel(w))
      assertEquals(
        buttons(w).map(b => runOnFx(drawn(b))),
        Vector("Review in Analysis", "Discard draft")
      )
      assertEqualsDouble(runOnFx(w.shell.banner.node.getHeight), 30, 1)
      val stroke = runOnFx(w.shell.banner.node.getBorder.getStrokes.get(0))
      assertNotEquals(stroke.getBottomStyle, BorderStrokeStyle.SOLID)
      assert(!stroke.getBottomStyle.getDashArray.isEmpty, "the banner's rule is dashed")
      fx.snapshot(StudioTheme.Light)
      val expected = Map(
        KeyCode.DIGIT1 -> false,
        KeyCode.DIGIT2 -> false,
        KeyCode.DIGIT3 -> false,
        KeyCode.DIGIT4 -> true,
        KeyCode.DIGIT5 -> true
      )
      expected.toVector.sortBy(_._1.getCode).foreach { (key, visible) =>
        fx.robot.press(key, shortcut)
        assertEquals(shown(w), visible, key)
        assertEquals(runOnFx(w.shell.banner.node.isManaged), visible, key)
      }
      // In Figures the words name the figure and say figures never follow.
      assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Figures)
      assertEquals(words(w), fromModel(w))
      assert(words(w).last.endsWith("Figures never follow a draft."), words(w).toString)
  }

  fxStage.test("Review in Analysis opens the draft; the banner hides there") { fx =>
    val w = boot(fx, StoryModels.t2Compare)
    fx.robot.click(button(w, "Review in Analysis"))
    assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Analysis)
    assertEquals(runOnFx(w.host.active), Some("analysis"))
    assert(!shown(w))
  }

  fxStage.test("Discard draft asks first: Keep draft keeps it, Discard draft discards it") {
    fx =>
      val w       = boot(fx, StoryModels.t2Compare)
      val confirm = w.shell.confirmation
      assert(runOnFx(!confirm.node.isVisible))
      fx.robot.click(button(w, "Discard draft"))
      assert(runOnFx(confirm.node.isVisible))
      assertEquals(
        runOnFx(drawn(confirm.text)),
        "Discard draft rev 5 and its 1 change? Runs are not affected."
      )
      assertNoDiff(texts(w).mkString("\n"), expected(w).mkString("\n"))
      fx.snapshot(StudioTheme.Light)
      def confirmButton(label: String) = runOnFx(
        confirm.node.getChildren.asScala
          .collectFirst {
            case b: Button if drawn(b) == label => b
          }
          .getOrElse(fail(s"no '$label'"))
      )
      fx.robot.click(confirmButton("Keep draft"))
      assert(runOnFx(!confirm.node.isVisible))
      assert(runOnFx(w.runtime.model.document.draft.isDefined))
      assert(shown(w))
      fx.robot.click(button(w, "Discard draft"))
      fx.robot.click(confirmButton("Discard draft"))
      assert(runOnFx(!confirm.node.isVisible))
      assertEquals(runOnFx(w.runtime.model.document.draft), None)
      // No draft differs from what is shown, and nothing runs: no banner, no chip.
      assert(!shown(w))
      assert(
        runOnFx(
          !w.shell.contextStrip.node.getChildren.asScala
            .exists(_.getStyleClass.contains("draft-chip"))
        )
      )
      // ⌘Z brings the draft back, and with it the banner.
      fx.robot.press(KeyCode.Z, shortcut)
      assert(runOnFx(w.runtime.model.document.draft.isDefined))
      assert(shown(w))
      assertEquals(words(w), fromModel(w))
  }

  fxStage.test("t3: 'Showing run 7 (rev 4).' then the run-8 sentence; Show waits for the run") {
    fx =>
      val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      assertEquals(
        words(w),
        Vector(
          "Showing run 7 (rev 4).",
          "Run 8 (rev 5, adds σ 8°) is running — results will not replace this view until you choose Show."
        )
      )
      val show = button(w, "Show run 8 · when finished")
      assert(runOnFx(show.isDisabled))
      fx.snapshot(StudioTheme.Light)
      // The job's state, not a string, drives the words: finishing run 8
      // makes Show live and the banner says the run is ready.
      w.session.await(w.session.backend.complete(StoryMoments.run8Job))
      eventually(fx, "Show run 8 enabled")(
        buttons(w).exists(b => drawn(b) == "Show run 8" && !b.isDisabled)
      )
      assertEquals(words(w), fromModel(w))
      assertEquals(words(w).head, "Showing run 7 (analysis rev 4).")
      fx.robot.click(button(w, "Show run 8"))
      assertEquals(
        runOnFx(w.runtime.model.document.presentation.shownRun),
        Some(StoryMoments.run8)
      )
      // Run 8 is rev 5, the latest: nothing newer, no draft; the banner goes.
      assert(!shown(w), words(w).toString)
  }

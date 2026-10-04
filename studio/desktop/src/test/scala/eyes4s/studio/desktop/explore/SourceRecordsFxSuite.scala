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

package eyes4s.studio.desktop.explore

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.explore.{SourceRecords, SourceRowVM}
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.animation.AnimationTimer
import javafx.scene.input.KeyCode

import scala.collection.mutable
import scala.concurrent.duration.Duration

/** Explore's source records table in the studio window (ticket S6.4;
  * Explore.dc.html, bottom) at t2, over fixtures/studio-golden behind the
  * port: the story's selected record 7,214 is under the cursor with its
  * backend values; scrolling all 11,520 records makes at most 60 row cells
  * and no frame longer than 32 ms; 'Show raw record' shows the verbatim CSV
  * line; the keys move the cursor and Enter selects the row's fixation.
  */
class SourceRecordsFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(240, "s")

  private def ready(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "the records are read") {
      w.sourceRecords.current.total.nonEmpty &&
      w.sourceRecords.current.cursor.exists(c =>
        SourceRecords.rowVM(w.sourceRecords.current, c).isInstanceOf[SourceRowVM.Shown]
      )
    }

  fxStage.test("the selected record 7,214 is under the cursor with the backend's values") {
    fx =>
      val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2, records = GoldenRecords.source)
      ready(fx, w)
      val s = runOnFx(w.sourceRecords.current)
      assertEquals(s.total, Some(11520))
      assertEquals(s.cursor, Some(7213))
      SourceRecords.rowVM(s, 7213) match
        case SourceRowVM.Shown(cells, cursor, selected, name) =>
          assertEquals(
            cells,
            Vector(
              "7,214",
              "enc_03",
              "6",
              "2,160",
              "412",
              "1148.0,456.0",
              "700,300",
              "+5.4°,+2.4°",
              "206",
              "inside"
            )
          )
          assert(cursor && selected, (cursor, selected))
          assertEquals(name, "Record 7,214, enc_03, fixation 6")
        case other => fail(s"row 7,213 is $other")
      // The cursor row is in view.
      eventually(fx, "the cursor row is in view")(w.sourceRecords.visible(7213))
      fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("scrolling all 11,520 records makes at most 60 row cells; no frame over 32 ms") {
    fx =>
      val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2, records = GoldenRecords.source)
      ready(fx, w)
      val list  = runOnFx(w.sourceRecords.rows)
      val gaps  = mutable.ArrayBuffer.empty[Long]
      var last  = 0L
      val timer = new AnimationTimer:
        def handle(now: Long): Unit =
          if last != 0L then gaps += now - last
          last = now
      runOnFx(list.scrollTo(0))
      fx.awaitLayout()
      runOnFx(timer.start())
      (0 until 11520 by 40).foreach { i =>
        runOnFx(list.scrollTo(i))
        Thread.sleep(8)
      }
      runOnFx(list.scrollTo(11519))
      Thread.sleep(100)
      runOnFx(timer.stop())
      val made = runOnFx(w.sourceRecords.cellsMade)
      assert(made <= 60, s"$made row cells were made")
      // Every visible row of the end is read.
      eventually(fx, "the last rows are read") {
        SourceRecords.rowVM(w.sourceRecords.current, 11519).isInstanceOf[SourceRowVM.Shown]
      }
      val worst = gaps.drop(2).maxOption.getOrElse(0L) / 1_000_000.0
      assert(gaps.size > 50, gaps.size)
      assert(worst <= 32.0, f"the slowest frame took $worst%.1f ms over ${gaps.size} frames")
      assert(runOnFx(w.sourceRecords.current.pages.size) <= SourceRecords.KeptPages)
  }

  fxStage.test(
    "'Show raw record' shows the verbatim CSV line; Down and Enter select the next fixation"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2, records = GoldenRecords.source)
    ready(fx, w)
    assertEquals(runOnFx(w.sourceRecords.rawText), None)
    runOnFx(w.sourceRecords.toggleRaw())
    fx.awaitLayout()
    assertEquals(
      runOnFx(w.sourceRecords.rawText),
      Some("P17,Encoding,enc_03,1,6,1148.0,456.0,2160,412,206")
    )
    assertEquals(runOnFx(w.sourceRecords.rawText), Some(GoldenRecords.lines(7213)))
    // The pane's stop takes the cursor's keys.
    val stop = runOnFx(w.sourceRecords.node.getParent)
    runOnFx(stop.requestFocus())
    fx.awaitLayout()
    fx.robot.press(KeyCode.DOWN)
    fx.robot.press(KeyCode.ENTER)
    fx.awaitLayout()
    assertEquals(runOnFx(w.sourceRecords.current.cursor), Some(7214))
    val seventh =
      StudioRef.Fixation(MockStudy.key("P17", "enc_03"), FixationIndex.of(7).toOption.get)
    eventually(fx, "fixation 7 is selected")(
      w.runtime.model.selection.selected == Vector(seventh)
    )
    assertEquals(
      runOnFx(w.sourceRecords.rawText),
      Some(GoldenRecords.lines(7214))
    )
  }

  fxStage.test("with no source served, the table says so") { fx =>
    val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
    eventually(fx, "the status is shown") {
      SourceRecords.status(w.sourceRecords.current).nonEmpty &&
      !SourceRecords.status(w.sourceRecords.current).contains("Reading the source records…")
    }
    assertEquals(
      runOnFx(SourceRecords.status(w.sourceRecords.current)),
      Some("Source records are not served in this window")
    )
  }

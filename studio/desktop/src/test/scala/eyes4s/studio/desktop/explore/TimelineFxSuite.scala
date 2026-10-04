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
import eyes4s.studio.app.explore.{PlaybackSpeed, TimelineIntent}
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.plot.{PlotHostStatus, PlotTwinStatus, TableTwinView}
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.viz.plot.{DataPoint, PlotTargets}
import javafx.event.Event
import javafx.geometry.Point2D
import javafx.scene.input.{MouseButton, MouseEvent, PickResult}

import scala.concurrent.duration.Duration

/** Explore's timeline hosted in its pane (ticket S6.3; Explore.dc.html,
  * timeline), in a studio window over the fake backend at story moment t2:
  * P17 enc_03's 13 fixation intervals as bars and rows, play, pause, step
  * and speed, and a brush that selects the overlapping fixations in the plot
  * and the table, leaving the analysis document exactly as it was.
  */
class TimelineFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(120, "s")

  private val enc03 = StoryModels.p17enc03

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def ref(i: Int): StudioRef = StudioRef.Fixation(enc03, right(FixationIndex.of(i)))

  /** A window at t2 in Explore with the timeline drawn. */
  private def ready(fx: FxStage): (StudioWindow, PlotTargets) =
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
    eventually(fx, "the timeline's bars") {
      w.timeline.vm.enabled && (
        (w.timeline.twin.status.get, w.timeline.twin.plotHost.status.get) match
          case (PlotTwinStatus.Shown(_), PlotHostStatus.Drawn(_)) =>
            w.timeline.twin.input.targets.isDefined
          case _ => false
      )
    }
    (w, runOnFx(w.timeline.twin.input.targets).get)

  private def fireAt(
      w: StudioWindow,
      t: PlotTargets,
      ms: Double,
      still: Boolean,
      kind: javafx.event.EventType[MouseEvent]
  ): Unit =
    val host = w.timeline.twin.plotHost
    val at   = right(t.transform.dataToDevice(DataPoint(ms, 20.0)))
    val c    = t.transform.deviceToCanvas(at)
    runOnFx {
      val p = host.localToScene(Point2D(c.x, c.y))
      Event.fireEvent(
        host,
        MouseEvent(
          kind,
          p.getX,
          p.getY,
          p.getX,
          p.getY,
          MouseButton.PRIMARY,
          1,
          false,
          false,
          false,
          false,
          kind == MouseEvent.MOUSE_PRESSED,
          false,
          false,
          true,
          false,
          still,
          PickResult(host, p.getX, p.getY)
        )
      )
    }

  fxStage.test("t2: P17 enc_03's 13 intervals, the playhead and the board's toolbar") { fx =>
    val (w, t) = ready(fx)
    val pane = runOnFx(w.host.node(StudioLayouts.timeline)).getOrElse(fail("no timeline pane"))
    assert(runOnFx(pane.getScene eq fx.scene))
    val h = w.timeline
    assertEquals(runOnFx(h.status.getText), "playhead 0.00 s")
    assertEquals(
      runOnFx(h.disclaimer.getText),
      "Brushing highlights; it does not crop the analysis"
    )
    assertEquals(
      runOnFx((h.play.getText, h.stepBack.getText, h.stepForward.getText)),
      ("Play", "Step back", "Step forward")
    )
    assertEquals(
      runOnFx(
        PlaybackSpeed.values.toVector.map(s => (h.speeds(s).getText, h.speeds(s).isSelected))
      ),
      Vector(("0.5×", false), ("1×", true), ("2×", false))
    )
    assertEquals(t.plot.source.rows.map(_.ref), (1 to 13).toVector.map(ref))
    assertEquals(runOnFx(h.twin.table.rowNodes.size), 13)
  }

  fxStage.test(
    "a brush selects the overlapping fixations in the plot and the table; the document is unchanged"
  ) { fx =>
    val (w, t)  = ready(fx)
    val before  = runOnFx(w.runtime.model.document)
    val encoded = right(StudioDocument.encode(before))
    fireAt(w, t, 1200.0, true, MouseEvent.MOUSE_PRESSED)
    fireAt(w, t, 2000.0, false, MouseEvent.MOUSE_DRAGGED)
    fireAt(w, t, 2800.0, false, MouseEvent.MOUSE_RELEASED)
    fx.awaitLayout()
    val brushed = (3 to 7).toVector.map(ref)
    eventually(fx, "the brush's selection")(w.runtime.model.selection.selected == brushed)
    // The table shows the same selection: every view sees it.
    assertEquals(
      runOnFx(
        w.timeline.twin.table.rowNodes
          .map(_.getPseudoClassStates.contains(TableTwinView.Selected))
      ),
      (1 to 13).toVector.map(i => i >= 3 && i <= 7)
    )
    assert(runOnFx(w.timeline.status.getText).startsWith("playhead 0.00 s · brush 1.20–2.80 s"))
    // The brush is view state: the document and its encoding are unchanged.
    val after = runOnFx(w.runtime.model.document)
    assertEquals(after, before)
    assertEquals(right(StudioDocument.encode(after)), encoded)
  }

  fxStage.test("play, step, speed and pause act on the playhead") { fx =>
    val (w, _) = ready(fx)
    val h      = w.timeline
    runOnFx(h.stepForward.fire())
    assertEquals(runOnFx(h.status.getText), "playhead 0.06 s")
    runOnFx(h.speeds(PlaybackSpeed.Two).fire())
    assertEquals(runOnFx(h.timeline.speed), PlaybackSpeed.Two)
    runOnFx(h.play.fire())
    assertEquals(runOnFx(h.play.getText), "Pause")
    // The clock advances a playing timeline.
    eventually(fx, "the playhead to move")(h.timeline.playheadMs > 60.0)
    runOnFx(h.play.fire())
    assertEquals(runOnFx((h.play.getText, h.timeline.playing)), ("Play", false))
    val held = runOnFx(h.timeline.playheadMs)
    runOnFx(h.dispatch(TimelineIntent.Tick(500.0)))
    assertEquals(runOnFx(h.timeline.playheadMs), held)
    runOnFx(h.stepBack.fire())
    assert(runOnFx(h.timeline.playheadMs) < held)
  }

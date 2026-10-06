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
import eyes4s.studio.app.plot.HalfOpenSpan
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.plot.{PlotHostStatus, PlotTwinStatus}
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

  // JavaFX scene/local pointer conversion uses float coordinates. At this
  // fixture's 5-second axis and pane width, its roundoff is below 0.001 ms.
  private val PointerRoundoffMs                                                         = 0.001
  private def assertSpan(span: Option[HalfOpenSpan], from: Double, until: Double): Unit =
    val value = span.getOrElse(fail("no brush span"))
    assertEqualsDouble(value.from, from, PointerRoundoffMs)
    assertEqualsDouble(value.until, until, PointerRoundoffMs)

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
          kind == MouseEvent.MOUSE_PRESSED || kind == MouseEvent.MOUSE_DRAGGED,
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
    assertEquals(runOnFx(h.twin.table.modelRowTexts.size), 13)
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
    // The table's rows hold the same selection: every view sees it. Its
    // Table tab is not shown here, so the rows are read from its state.
    assertEquals(
      runOnFx(
        w.timeline.twin.table.modelRowSelected
      ),
      (1 to 13).toVector.map(i => i >= 3 && i <= 7)
    )
    assert(runOnFx(w.timeline.status.getText).startsWith("playhead 0.00 s · brush 1.20–2.80 s"))
    // The brush is view state: the document and its encoding are unchanged.
    val after = runOnFx(w.runtime.model.document)
    assertEquals(after, before)
    assertEquals(right(StudioDocument.encode(after)), encoded)
  }

  fxStage.test("drag motion shades the live span before release without committing selection") {
    fx =>
      val (w, t)    = ready(fx)
      val selection = runOnFx(w.runtime.model.selection)
      val before    = runOnFx(w.timeline.twin.plotHost.snapshot(null, null))
      val profile   = runOnFx(w.timeline.twin.plotHost.profile)
      fireAt(w, t, 1200.0, true, MouseEvent.MOUSE_PRESSED)
      fireAt(w, t, 2000.0, false, MouseEvent.MOUSE_DRAGGED)
      fx.awaitLayout()
      assertSpan(runOnFx(w.timeline.brush.span), 1200.0, 2000.0)
      assertEquals(runOnFx(w.timeline.timeline.brush), None)
      assertEquals(runOnFx(w.runtime.model.selection), selection)
      assert(runOnFx(w.timeline.twin.plotHost.profile.baseDraws) > profile.baseDraws)
      val after = runOnFx(w.timeline.twin.plotHost.snapshot(null, null))
      assert(
        (0 until before.getWidth.toInt).exists { x =>
          (0 until before.getHeight.toInt).exists(y =>
            before.getPixelReader.getArgb(x, y) != after.getPixelReader.getArgb(x, y)
          )
        },
        "drag shading must change the drawn pixels before release"
      )
      // Reverse the drag, then release: only the final span selects rows.
      fireAt(w, t, 800.0, false, MouseEvent.MOUSE_DRAGGED)
      fx.awaitLayout()
      assertSpan(runOnFx(w.timeline.brush.span), 800.0, 1200.0)
      assertEquals(runOnFx(w.runtime.model.selection), selection)
      fireAt(w, t, 2800.0, false, MouseEvent.MOUSE_RELEASED)
      eventually(fx, "final brush")(w.timeline.timeline.brush.isDefined)
      assertSpan(runOnFx(w.timeline.timeline.brush), 1200.0, 2800.0)
      assertEquals(runOnFx(w.runtime.model.selection.selected), (3 to 7).toVector.map(ref))
  }

  fxStage.test("playhead updates retain a live drag until release") { fx =>
    val (w, t) = ready(fx)
    runOnFx(w.timeline.dispatch(TimelineIntent.Play))
    fireAt(w, t, 1200.0, true, MouseEvent.MOUSE_PRESSED)
    fireAt(w, t, 2000.0, false, MouseEvent.MOUSE_DRAGGED)
    runOnFx(w.timeline.dispatch(TimelineIntent.Tick(100.0)))
    assertSpan(runOnFx(w.timeline.brush.span), 1200.0, 2000.0)
    assertEquals(runOnFx(w.timeline.timeline.brush), None)
    runOnFx(w.timeline.dispatch(TimelineIntent.Pause))
    fireAt(w, t, 2800.0, false, MouseEvent.MOUSE_RELEASED)
    eventually(fx, "brush while playing")(w.timeline.timeline.brush.isDefined)
    assertSpan(runOnFx(w.timeline.timeline.brush), 1200.0, 2800.0)
  }

  fxStage.test("Escape or restoring the view cancels a live drag before its release") { fx =>
    val (w, t) = ready(fx)
    fireAt(w, t, 1200.0, true, MouseEvent.MOUSE_PRESSED)
    fireAt(w, t, 2000.0, false, MouseEvent.MOUSE_DRAGGED)
    assert(runOnFx(w.timeline.brush.span.isDefined))
    runOnFx {
      Event.fireEvent(
        w.timeline.twin.plotHost,
        javafx.scene.input.KeyEvent(
          javafx.scene.input.KeyEvent.KEY_PRESSED,
          "",
          "",
          javafx.scene.input.KeyCode.ESCAPE,
          false,
          false,
          false,
          false
        )
      )
    }
    assertEquals(runOnFx(w.timeline.brush.span), None)
    fireAt(w, t, 2800.0, false, MouseEvent.MOUSE_RELEASED)
    assertEquals(runOnFx(w.timeline.timeline.brush), None)
    assertEquals(runOnFx(w.runtime.model.selection.selected), Vector.empty)
    fireAt(w, t, 1200.0, true, MouseEvent.MOUSE_PRESSED)
    fireAt(w, t, 2000.0, false, MouseEvent.MOUSE_DRAGGED)
    runOnFx(w.timeline.brush.restore(None))
    fireAt(w, t, 2800.0, false, MouseEvent.MOUSE_RELEASED)
    assertEquals(runOnFx(w.timeline.brush.span), None)
    assertEquals(runOnFx(w.timeline.timeline.brush), None)
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

  private def drag(w: StudioWindow, t: PlotTargets, from: Double, to: Double): Unit =
    fireAt(w, t, from, true, MouseEvent.MOUSE_PRESSED)
    fireAt(w, t, (from + to) / 2.0, false, MouseEvent.MOUSE_DRAGGED)
    fireAt(w, t, to, false, MouseEvent.MOUSE_RELEASED)

  fxStage.test(
    "the brush ends when the selection moves on: another view's selection, and Escape"
  ) { fx =>
    val (w, t) = ready(fx)
    val h      = w.timeline
    drag(w, t, 1200.0, 2800.0)
    eventually(fx, "the brush")(h.timeline.brush.isDefined && h.brush.span.isDefined)
    // Another view selects fixation 1: the span and its status go.
    dispatch(fx, w, StoryModels.select(w.runtime.model, "explore.trial-view", ref(1)))
    eventually(fx, "the brush cleared")(h.timeline.brush.isEmpty && h.brush.span.isEmpty)
    assertEquals(runOnFx(h.status.getText), "playhead 0.00 s")
    // Brush again; Escape on the plot clears the selection, and with it the brush.
    val t2 = runOnFx(h.twin.input.targets).get
    drag(w, t2, 1200.0, 2800.0)
    eventually(fx, "the second brush")(h.timeline.brush.isDefined)
    runOnFx(h.twin.plotHost.requestFocus())
    fx.awaitLayout()
    fx.robot.press(javafx.scene.input.KeyCode.ESCAPE)
    eventually(fx, "Escape's clear")(
      w.runtime.model.selection.selected.isEmpty && h.timeline.brush.isEmpty
    )
    assertEquals(runOnFx(h.brush.span), None)
  }

  fxStage.test("playback stops at the trial's end; a trial change or dispose stops the clock") {
    fx =>
      val (w, _) = ready(fx)
      val h      = w.timeline
      // To the last onset (4,688 ms), then play at 2×: 350 ms of trial remain.
      runOnFx {
        (1 to 13).foreach(_ => h.stepForward.fire())
        h.speeds(PlaybackSpeed.Two).fire()
        h.play.fire()
      }
      assert(runOnFx(h.clockRunning))
      eventually(fx, "the end")(!h.timeline.playing)
      assertEquals(runOnFx((h.timeline.playheadMs, h.play.getText)), (5038.0, "Play"))
      eventually(fx, "the clock to stop")(!h.clockRunning)
      // Playing, then another trial: paused at its start, the clock stopped.
      runOnFx(h.play.fire())
      assertEquals(runOnFx(h.timeline.playheadMs), 0.0)
      assert(runOnFx(h.timeline.playing))
      val enc04 = enc03.copy(trial = "enc_04")
      dispatch(
        fx,
        w,
        eyes4s.studio.app.Intent.Navigate(
          eyes4s.studio.app.nav.Location(
            eyes4s.studio.core.document.Perspective.Explore,
            Vector(eyes4s.studio.app.nav.Place.At(StudioRef.Trial(enc04)))
          )
        )
      )
      eventually(fx, "the new trial")(h.timeline.trial.contains(enc04))
      assertEquals(runOnFx((h.timeline.playing, h.timeline.playheadMs)), (false, 0.0))
      eventually(fx, "the clock to stop")(!h.clockRunning)
      // Disposed while playing: the clock stops, and a button no longer starts it.
      eventually(fx, "enc_04's bars")(h.vm.enabled)
      runOnFx(h.play.fire())
      assert(runOnFx(h.clockRunning))
      runOnFx(h.dispose())
      assert(!runOnFx(h.clockRunning))
      runOnFx(h.play.fire())
      assert(!runOnFx(h.clockRunning))
  }

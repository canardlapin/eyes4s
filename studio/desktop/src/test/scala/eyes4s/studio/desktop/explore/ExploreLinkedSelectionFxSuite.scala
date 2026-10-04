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
import eyes4s.studio.app.explore.SourceRecordsIntent
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, StudioRef}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.plot.{CanvasPlotHost, TableTwinView}
import eyes4s.studio.desktop.shell.ShellFxSuite
import intaglio.DevicePoint
import javafx.event.Event
import javafx.geometry.Point2D
import javafx.scene.input.{MouseButton, MouseEvent, PickResult}

import scala.concurrent.duration.Duration

/** E2E-03, linked selection in Explore (ticket S6.6; Explore.dc.html,
  * interactive), in a studio window at t2 on P17 enc_03: a fixation mark, a
  * timeline bar, a source record and Prev/Next each select the same fixation
  * in every view (the marks, the timeline and its table, the records' row
  * cursor and the inspector), and the trail's fixation and record crumbs
  * follow. Fixation k is fixations.csv record 7,208 + k.
  */
class ExploreLinkedSelectionFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(240, "s")

  private val enc03 = StoryModels.p17enc03

  private def fixation(i: Int): StudioRef.Fixation =
    StudioRef.Fixation(enc03, FixationIndex.of(i).toOption.get)

  private def record(i: Int): StudioRef =
    StudioRef.SourceRecord(
      enc03,
      FixationIndex.of(i).toOption,
      SourceRole.Fixations,
      RecordNumber.of(7208 + i).toOption.get
    )

  /** A window at t2 with every Explore view drawn and read. */
  private def ready(fx: FxStage): StudioWindow =
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
    eventually(fx, "the marks, the timeline, the records and the inspector") {
      w.explore.input.targets.isDefined &&
      w.timeline.twin.input.targets.isDefined &&
      w.sourceRecords.current.total.nonEmpty &&
      w.inspector.titleText.nonEmpty
    }
    w

  // A pointer click at device point `at` of `host`'s drawn frame.
  private def click(host: CanvasPlotHost, at: DevicePoint): Unit =
    runOnFx {
      val frame = host.frame.getOrElse(fail("nothing drawn"))
      val c     = frame.transform.deviceToCanvas(at)
      val p     = host.localToScene(Point2D(c.x, c.y))
      Vector(MouseEvent.MOUSE_PRESSED, MouseEvent.MOUSE_RELEASED, MouseEvent.MOUSE_CLICKED)
        .foreach { kind =>
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
              true,
              PickResult(host, p.getX, p.getY)
            )
          )
        }
    }

  /** Fixation `i` is selected in every view, and the trail is at it. */
  private def everywhere(fx: FxStage, w: StudioWindow, i: Int): Unit =
    eventually(fx, s"fixation $i everywhere") {
      w.runtime.model.selection.selected == Vector(fixation(i)) &&
      w.runtime.model.location.trail.takeRight(2) ==
        Vector(Place.At(fixation(i)), Place.At(record(i))) &&
        w.inspector.titleText.startsWith(s"Fixation $i of 13") &&
        w.sourceRecords.current.cursor.contains(7207 + i)
    }
    // The marks' input and the timeline's table show it.
    assertEquals(runOnFx(w.explore.input.state.selected), Vector[StudioRef](fixation(i)))
    assertEquals(
      runOnFx(
        w.timeline.twin.table.rowNodes
          .map(_.getPseudoClassStates.contains(TableTwinView.Selected))
      ),
      (1 to 13).toVector.map(_ == i)
    )

  fxStage.test(
    "E2E-03: a mark, a timeline bar, a record and Prev/Next select one fixation everywhere"
  ) { fx =>
    val w = ready(fx)
    // t2: record 7,214, fixation 6, and its crumbs.
    eventually(fx, "t2's fixation 6")(w.inspector.titleText.startsWith("Fixation 6 of 13"))
    assertEquals(
      runOnFx(w.runtime.model.location.trail.takeRight(2)),
      Vector(Place.At(fixation(6)), Place.At(record(6)))
    )
    // A mark in the trial view: fixation 8.
    val mark = runOnFx(w.explore.input.targets.flatMap(_.target(fixation(8))))
      .getOrElse(fail("no mark 8"))
    click(w.explore.trialView.plotHost, mark.anchor)
    everywhere(fx, w, 8)
    // A timeline bar: fixation 3.
    val bar = runOnFx(w.timeline.twin.input.targets.flatMap(_.target(fixation(3))))
      .getOrElse(fail("no bar 3"))
    click(w.timeline.twin.plotHost, bar.anchor)
    everywhere(fx, w, 3)
    // A source record: 7,210, fixation 2 (the table's rows are indexed from 0).
    runOnFx(w.sourceRecords.dispatch(SourceRecordsIntent.Click(7209)))
    everywhere(fx, w, 2)
    // Next and Prev step through the scanpath.
    runOnFx(w.explore.pane.next.fire())
    everywhere(fx, w, 3)
    runOnFx(w.explore.pane.prev.fire())
    runOnFx(w.explore.pane.prev.fire())
    everywhere(fx, w, 1)
    // At the first fixation there is no Previous.
    eventually(fx, "Prev disabled")(w.explore.pane.prev.isDisabled)
    assert(!runOnFx(w.explore.pane.next.isDisabled))
    // At the last there is no Next.
    val last = runOnFx(w.timeline.twin.input.targets.flatMap(_.target(fixation(13))))
      .getOrElse(fail("no bar 13"))
    click(w.timeline.twin.plotHost, last.anchor)
    everywhere(fx, w, 13)
    eventually(fx, "Next disabled")(w.explore.pane.next.isDisabled)
    assert(!runOnFx(w.explore.pane.prev.isDisabled))
  }

  fxStage.test("Prev and Next name what pressed them: a pointer, a key, or neither") { _ =>
    import eyes4s.studio.core.selection.InputCause
    import javafx.scene.control.Button
    import javafx.scene.input.{KeyCode, KeyEvent}
    var seen = Vector.empty[InputCause]
    val b    = runOnFx {
      val b = Button("›")
      ExploreTrialViewPane.onPress(b)(c => seen = seen :+ c)
      b
    }
    runOnFx {
      Event.fireEvent(
        b,
        KeyEvent(KeyEvent.KEY_PRESSED, "", "", KeyCode.SPACE, false, false, false, false)
      )
      b.fire()
      Event.fireEvent(
        b,
        MouseEvent(
          MouseEvent.MOUSE_PRESSED,
          1,
          1,
          1,
          1,
          MouseButton.PRIMARY,
          1,
          false,
          false,
          false,
          false,
          true,
          false,
          false,
          true,
          false,
          true,
          null
        )
      )
      b.fire()
      b.fire()
    }
    assertEquals(seen, Vector(InputCause.Keyboard, InputCause.Pointer, InputCause.Programmatic))
    // A press dragged off the button, or a Tab that only moves focus, is
    // forgotten on its release: a later fire() names no cause.
    def mouse(kind: javafx.event.EventType[MouseEvent]) = MouseEvent(
      kind,
      1,
      1,
      1,
      1,
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
      true,
      null
    )
    runOnFx {
      Event.fireEvent(b, mouse(MouseEvent.MOUSE_PRESSED))
      Event.fireEvent(b, mouse(MouseEvent.MOUSE_RELEASED))
    }
    runOnFx(b.fire())
    runOnFx {
      Event.fireEvent(
        b,
        KeyEvent(KeyEvent.KEY_PRESSED, "", "", KeyCode.TAB, false, false, false, false)
      )
      Event.fireEvent(
        b,
        KeyEvent(KeyEvent.KEY_RELEASED, "", "", KeyCode.TAB, false, false, false, false)
      )
    }
    runOnFx(b.fire())
    assertEquals(seen.drop(3), Vector(InputCause.Programmatic, InputCause.Programmatic))
  }

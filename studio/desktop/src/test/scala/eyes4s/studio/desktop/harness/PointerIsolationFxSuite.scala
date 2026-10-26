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

package eyes4s.studio.desktop.harness

import javafx.event.Event
import javafx.scene.layout.StackPane
import javafx.scene.input.{MouseButton, MouseEvent, PickResult}

private[harness] object PointerProbe:
  def event(node: StackPane): MouseEvent = MouseEvent(
    MouseEvent.MOUSE_MOVED,
    12,
    12,
    12,
    12,
    MouseButton.NONE,
    0,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    true,
    PickResult(node, 12, 12)
  )
  // Deliberately bypass the robot to reproduce foreign dispatch; never use in ordinary suites.
  def foreign(node: StackPane): Unit = Event.fireEvent(node, event(node))

class PointerIsolationFxSuite extends StudioFxSuite:
  fxStage.test("foreign events are consumed and counted; owned and nested dispatch survives") {
    fx =>
      val node     = runOnFx(StackPane())
      var received = 0
      runOnFx(node.addEventHandler(MouseEvent.MOUSE_MOVED, _ => received += 1))
      fx.show(node)
      val before = fx.foreignPointerCount
      runOnFx(PointerProbe.foreign(node))
      assertEquals(runOnFx(received), 0)
      assert(fx.foreignPointerCount > before)
      assert(fx.foreignPointerSamples.nonEmpty)
      runOnFx(StudioRobot.fireEvent(node, PointerProbe.event(node)))
      assertEquals(runOnFx(received), 1)
      runOnFx {
        node.addEventHandler(
          MouseEvent.MOUSE_MOVED,
          _ => if received == 2 then StudioRobot.fireEvent(node, PointerProbe.event(node))
        )
        StudioRobot.fireEvent(node, PointerProbe.event(node))
      }
      assertEquals(runOnFx(received), 3)
      runOnFx(PointerProbe.foreign(node))
      assertEquals(runOnFx(received), 3)
      assert(!runOnFx(StudioRobot.firingMouse))
  }

  fxStage.test("changing the scene root retains pointer isolation") { fx =>
    (1 to 3).foreach { _ =>
      val node     = runOnFx(StackPane())
      var received = 0
      runOnFx(node.addEventHandler(MouseEvent.MOUSE_MOVED, _ => received += 1))
      fx.show(node)
      runOnFx(PointerProbe.foreign(node))
      assertEquals(runOnFx(received), 0)
      runOnFx(StudioRobot.fireEvent(node, PointerProbe.event(node)))
      assertEquals(runOnFx(received), 1)
    }
  }

class PointerOptOutFxSuite extends StudioFxSuite:
  override protected def allowForeignPointerEvents: Boolean = true
  fxStage.test("explicit suite opt-out permits foreign pointer behavior") { fx =>
    val node     = runOnFx(StackPane())
    var received = 0
    runOnFx(node.addEventHandler(MouseEvent.MOUSE_MOVED, _ => received += 1))
    fx.show(node)
    val before = runOnFx(received)
    runOnFx(PointerProbe.foreign(node))
    assert(runOnFx(received) > before)
    assertEquals(fx.foreignPointerCount, 0L)
  }

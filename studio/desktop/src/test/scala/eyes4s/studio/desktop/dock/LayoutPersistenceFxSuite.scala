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

package eyes4s.studio.desktop.dock

import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.core.document.{Perspective, SavedLayout}
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.stage.{Window, WindowEvent}
import javafx.event.Event
import scala.jdk.CollectionConverters.*

class LayoutPersistenceFxSuite extends ShellFxSuite:
  private def windows = Window.getWindows.asScala.filter(_.isShowing).toVector

  fxStage.test("E2E18 pop-outs retain one live pane across perspective switches and close") {
    fx =>
      val w      = boot(fx, StoryModels.t2Figures, nativeMenu = true)
      val id     = eyes4s.studio.app.layout.PaneId.of("figures.page").toOption.get
      val pane   = runOnFx(w.host.node(id).getOrElse(fail("page pane missing")))
      val main   = runOnFx(windows.toSet)
      val popOut = runOnFx {
        val group = Iterator
          .iterate(pane)(_.getParent)
          .takeWhile(_ != null)
          .find(_.getStyleClass.contains("dock-group"))
          .getOrElse(fail("page group missing"))
        val button = group.lookup(".dock-header-button.popout")
        assert(button != null && button.isVisible)
        button
      }
      fx.robot.click(popOut)
      fx.awaitLayout()
      val floated = runOnFx {
        assertEquals(w.host.dock.state.floating.size, 1)
        val window = pane.getScene.getWindow
        assert(!main(window))
        assert(window.isShowing)
        assertEquals(w.themes.registered, 1)
        window
      }
      runOnFx {
        Event.fireEvent(
          pane,
          new javafx.scene.input.KeyEvent(
            javafx.scene.input.KeyEvent.KEY_PRESSED,
            "",
            "",
            javafx.scene.input.KeyCode.DIGIT3,
            false,
            !isMac,
            false,
            isMac
          )
        )
        assertEquals(w.runtime.model.perspective, Perspective.Analysis)
      }
      fx.awaitLayout()
      runOnFx {
        assert(!floated.isShowing)
        assertEquals(w.themes.registered, 0)
        assert(w.host.node(id).exists(_ eq pane))
      }
      dispatch(fx, w, Intent.SwitchPerspective(Perspective.Figures))
      runOnFx {
        assertEquals(w.host.dock.state.floating.size, 1)
        assert(w.host.node(id).exists(_ eq pane))
        assert(pane.getScene.getWindow.isShowing)
        Event.fireEvent(
          pane.getScene.getWindow,
          WindowEvent(pane.getScene.getWindow, WindowEvent.WINDOW_CLOSE_REQUEST)
        )
        assertEquals(w.host.dock.state.floating.size, 0)
        assert(w.host.node(id).exists(_ eq pane))
        w.host.dock.popOut(w.host.dock.state.groupOf(w.host.dockId(id)).get.id)
        w.close()
        assertEquals(windows.toSet, main)
      }
  }

  fxStage.test(
    "floating layouts round-trip, reset removes saved override and keeps live panes"
  ) { fx =>
    val w     = boot(fx, StoryModels.t2Figures)
    val id    = eyes4s.studio.app.layout.PaneId.of("figures.page").toOption.get
    val saved = runOnFx {
      w.host.dock.popOut(w.host.dock.state.groupOf(w.host.dockId(id)).get.id)
      w.host.capture().collect { case (p, Some(blob)) => SavedLayout(p, blob) }
    }
    assert(saved.exists(_.perspective == Perspective.Figures))
    runOnFx {
      w.host.reset(Perspective.Figures)
      assertEquals(w.host.dock.state.floating.size, 0)
      assertEquals(w.host.capture().toMap.apply(Perspective.Figures), None)
      assertEquals(w.host.restore(saved, w.runtime.model), Vector.empty)
      assertEquals(w.host.dock.state.floating.size, 1)
      assertEquals(
        w.host.capture().collect { case (p, Some(blob)) => SavedLayout(p, blob) },
        saved
      )
    }
  }

  fxStage.test("unknown saved pane types load as placeholders and preserve their payload") {
    fx =>
      import eyes4s.studio.core.document.LayoutBlob
      val w                                = boot(fx, StoryModels.t2Figures)
      val encoded                          = scaladock.LayoutCodec.encode(w.host.dock.state)
      def change(value: ujson.Value): Unit = value match
        case obj: ujson.Obj =>
          if obj.obj.get("id").contains(ujson.Str("figures.page")) then
            obj("type") = "future.studio.pane"
            obj("state") = ujson.Obj("keep" -> 42)
          else obj.obj.values.foreach(change)
        case arr: ujson.Arr => arr.arr.foreach(change)
        case _              => ()
      change(encoded)
      val blob = LayoutBlob(ujson.write(ujson.Obj(w.host.active.get -> encoded)))
      runOnFx {
        assertEquals(
          w.host.restore(Vector(SavedLayout(Perspective.Figures, blob)), w.runtime.model),
          Vector.empty
        )
        val saved = w.host.capture().toMap.apply(Perspective.Figures).get
        assert(saved.text.contains("future.studio.pane"))
        assert(saved.text.contains("\"keep\":42"))
        assert(!w.host.dock.view.lookupAll(".dock-unresolved").isEmpty)
      }
  }

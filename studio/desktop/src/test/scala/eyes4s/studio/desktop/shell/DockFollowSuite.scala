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
import eyes4s.studio.app.layout.PaneId
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.dock.{DockGesture, DockLayouts, PerspectiveHost}
import javafx.scene.Node

class DockFollowSuite extends ShellFxSuite:
  private def pane(value: String): PaneId =
    PaneId.of(value).fold(e => fail(e.toString), identity)
  private val navigator = pane("figures.figures")
  private val page      = pane("figures.page")
  private val inspector = pane("figures.panel")
  private def start     = AppModel.update(StoryModels.t2Figures, Intent.FocusPane(navigator))._1

  test("an overtaken dock focus gesture does not move the model backwards") {
    assertEquals(
      StudioWindow
        .follow(start, DockGesture.Focused(page), false, Some(DockLayouts.paneId(inspector))),
      None
    )
    assertEquals(StudioWindow.follow(start, DockGesture.Focused(page), false, None), None)
  }

  test("the current dock focus is followed once") {
    val live = Some(DockLayouts.paneId(inspector))
    assertEquals(
      StudioWindow.follow(start, DockGesture.Focused(inspector), false, live),
      Some(Intent.FocusPane(inspector))
    )
    val followed = AppModel.update(start, Intent.FocusPane(inspector))._1
    assertEquals(
      StudioWindow.follow(followed, DockGesture.Focused(inspector), false, live),
      None
    )
  }

  fxStage.test("queued pane focus moves retain the latest model, dock and keyboard focus") {
    fx =>
      val w = boot(fx, start)
      runOnFx {
        PerspectiveHost.focusInside(w.figures.pageNode)
        PerspectiveHost.focusInside(w.figures.inspectorNode)
        assertEquals(w.host.dock.state.focused, Some(DockLayouts.paneId(inspector)))
      }
      fx.awaitLayout()
      runOnFx {
        assertEquals(w.runtime.model.focusedPane, inspector)
        assertEquals(w.host.dock.state.focused, Some(DockLayouts.paneId(inspector)))
        val owner = Option(fx.scene.getFocusOwner).getOrElse(fail("no keyboard focus owner"))
        assert(
          Iterator
            .iterate[Node](owner)(_.getParent)
            .takeWhile(_ != null)
            .exists(_ eq w.figures.inspectorNode),
          owner.getAccessibleText
        )
      }
  }

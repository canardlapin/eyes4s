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
import eyes4s.studio.app.layout.{PaneId, StudioLayouts}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{LayoutBlob, Perspective, SavedLayout}
import eyes4s.studio.core.fixture.StoryMoment
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

  private def legacyData: scaladock.LayoutState =
    val id                                        = DockLayouts.paneId(StudioLayouts.dataIssues)
    def remove(n: scaladock.Node): scaladock.Node = n match
      case g: scaladock.Node.Group => g.copy(tabs = g.tabs.filterNot(_.id == id))
      case s: scaladock.Node.Split =>
        s.copy(cells = s.cells.map(c => c.copy(node = remove(c.node))))
    val state = DockLayouts.state(StudioLayouts.dataVerify)
    state.copy(root = state.root.map(remove))

  private def savedData(state: scaladock.LayoutState): LayoutBlob =
    LayoutBlob(ujson.write(ujson.Obj("data.verify" -> scaladock.LayoutCodec.encode(state))))

  fxStage.test(
    "older saved Data arrangements restore with reachable issues and repeat idempotently"
  ) { fx =>
    val old   = legacyData
    val blob  = savedData(old)
    val model = AppModel
      .update(
        StoryModels.t1Data,
        Intent.Dispatch(Command.SaveLayout(Perspective.Data, Some(blob)))
      )
      ._1
    val w        = boot(fx, model, StoryMoment.T1)
    val restored = runOnFx(w.host.dock.state)
    assert(restored.findPane(DockLayouts.paneId(StudioLayouts.dataIssues)).isDefined)
    assertEquals(
      restored.panes.filterNot(_.id == DockLayouts.paneId(StudioLayouts.dataIssues)),
      old.panes
    )
    assertEquals(AppModel.savedLayout(runOnFx(w.runtime.model), Perspective.Data), Some(blob))
    val again =
      runOnFx(w.host.restore(Vector(SavedLayout(Perspective.Data, blob)), w.runtime.model))
    assertEquals(again, Vector.empty)
    assertEquals(runOnFx(w.host.dock.state.root), restored.root)
    dispatch(fx, w, Intent.FocusPane(StudioLayouts.dataIssues))
    assertEquals(
      runOnFx(w.host.dock.state.focused),
      Some(DockLayouts.paneId(StudioLayouts.dataIssues))
    )
  }

  fxStage.test(
    "a saved Data layout missing Admission falls back while preserving its original blob"
  ) { fx =>
    val blob  = savedData(DockLayouts.state(StudioLayouts.dataFirstRun))
    val model = AppModel
      .update(
        StoryModels.t1Data,
        Intent.Dispatch(Command.SaveLayout(Perspective.Data, Some(blob)))
      )
      ._1
    val w = boot(fx, model, StoryMoment.T1)
    assertEquals(AppModel.savedLayout(runOnFx(w.runtime.model), Perspective.Data), Some(blob))
    assert(
      runOnFx(
        w.host.dock.state.findPane(DockLayouts.paneId(StudioLayouts.dataIssues))
      ).isDefined
    )
    val refused =
      runOnFx(w.host.restore(Vector(SavedLayout(Perspective.Data, blob)), w.runtime.model))
    assertEquals(refused.map(_.perspective), Vector(Perspective.Data))
    assert(refused.head.reason.contains("no Admission pane"), refused.head.reason)
    assertEquals(AppModel.savedLayout(runOnFx(w.runtime.model), Perspective.Data), Some(blob))
  }

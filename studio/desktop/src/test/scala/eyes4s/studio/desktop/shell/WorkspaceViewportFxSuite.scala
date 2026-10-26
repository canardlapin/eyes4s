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
import eyes4s.studio.app.appearance.Appearance
import eyes4s.studio.app.vm.A11y
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.dock.StudioPanes
import eyes4s.studio.desktop.harness.{FxStage, Modifiers, StageSize, StudioTheme}
import eyes4s.studio.desktop.plot.PlotHostStatus
import javafx.scene.{Node, Parent}
import javafx.scene.control.ScrollPane
import javafx.scene.input.KeyCode
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** Small windows must preserve a usable data frame, not just return Drawn. */
class WorkspaceViewportFxSuite extends StoryCaptureFixture:
  override protected def stageSize: StageSize = StageSize(1024, 656)
  private val MinimumMapHeight                = 120.0
  private val MinimumParticipantHeight        = 200.0

  private def contains(parent: Node, node: Node): Boolean =
    Iterator.iterate(node)(_.getParent).takeWhile(_ != null).exists(_ eq parent)

  /** A large composite may exceed the viewport; separate probes check its
    * cursor through every nested clip. Every ordinary control must fit completely inside it.
    */
  private def visibleFocus(fx: FxStage, w: StudioWindow): Unit =
    fx.awaitLayout()
    runOnFx {
      val node      = fx.scene.getFocusOwner
      val view      = w.shell.workspace.lookup(".viewport")
      val a         = node.localToScene(node.getBoundsInLocal)
      val b         = view.localToScene(view.getBoundsInLocal)
      val tolerance = 1.0 // fractional scroll offsets round to device pixels
      if a.getWidth <= b.getWidth then
        assert(
          a.getMinX >= b.getMinX - tolerance && a.getMinX <= b.getMaxX,
          s"${node.getAccessibleText}: horizontal leading edge $a outside $b"
        )
      if a.getHeight <= b.getHeight then
        assert(
          a.getMinY >= b.getMinY - tolerance && a.getMinY <= b.getMaxY,
          s"${node.getAccessibleText}: vertical leading edge $a outside $b"
        )
      if a.getWidth <= b.getWidth then
        assert(
          a.getMaxX <= b.getMaxX + tolerance,
          s"${node.getAccessibleText}: $a outside $b; scroll=${w.shell.workspace.getVvalue}, area=${w.shell.workspace.getContent.getLayoutBounds}; local=${w.shell.workspace.getContent.sceneToLocal(a)}"
        )
      if a.getHeight <= b.getHeight then
        assert(
          a.getMaxY <= b.getMaxY + tolerance,
          s"${node.getAccessibleText}: $a outside $b; scroll=${w.shell.workspace.getVvalue}, area=${w.shell.workspace.getContent.getLayoutBounds}; local=${w.shell.workspace.getContent.sceneToLocal(a)}"
        )
      assert(a.intersects(b), s"${node.getAccessibleText}: $a does not intersect $b")
    }

  private def resultsCycle(fx: FxStage, w: StudioWindow, backwards: Boolean): Vector[String] =
    runOnFx(w.shell.appBar.project.requestFocus())
    fx.awaitLayout()
    val expected = runOnFx(
      A11y.tabOrder(w.runtime.model, inside = w.paneStops, focus = w.paneFocus)
    )
    val ordered = if backwards then expected.head +: expected.tail.reverse else expected
    val visited = ordered.map { stop =>
      visibleFocus(fx, w)
      val actual = runOnFx {
        val node = fx.scene.getFocusOwner
        (node.getAccessibleRole, node.getAccessibleText)
      }
      assertEquals(actual, (StudioPanes.accessibleRole(stop.role), stop.name))
      fx.robot.press(KeyCode.TAB, Modifiers(shift = backwards))
      stop.render
    }
    assert(runOnFx(fx.scene.getFocusOwner eq w.shell.appBar.project))
    visited

  private def cursorVisible(fx: FxStage, composite: Node, selector: String): Unit =
    for key <- Vector(KeyCode.END, KeyCode.HOME) do
      fx.robot.press(key)
      fx.awaitLayout()
      runOnFx {
        val row = composite.lookup(selector)
        assert(row != null, s"missing cursor $selector")
        val a = row.localToScene(row.getBoundsInLocal)
        Iterator
          .iterate(row.getParent)(_.getParent)
          .takeWhile(_ != null)
          .collect { case pane: ScrollPane => pane }
          .foreach { pane =>
            val viewport = pane.lookup(".viewport")
            val b        = viewport.localToScene(viewport.getBoundsInLocal)
            assert(
              a.getMinY >= b.getMinY - 1.0 && a.getMaxY <= b.getMaxY + 1.0,
              s"$key cursor $a outside scrolling clip $b"
            )
          }
      }

  fxStage.test("clearing focus does not reveal unrelated rows or scroll the workspace") { fx =>
    val w = boot(fx, StoryModels.t1Data, StoryMoment.T1, clock = () => None)
    runOnFx(w.shell.appBar.project.requestFocus())
    fx.awaitLayout()
    def scrolls(node: Node): Vector[ScrollPane] =
      val own = node match
        case pane: ScrollPane => Vector(pane)
        case _                => Vector.empty
      own ++ (node match
        case p: Parent => p.getChildrenUnmodifiable.asScala.toVector.flatMap(scrolls)
        case _         => Vector.empty)
    val panes = runOnFx(scrolls(w.root))
    assert(panes.size > 1)
    runOnFx(panes.foreach { pane =>
      pane.setHvalue(0.0)
      pane.setVvalue(0.0)
    })
    fx.awaitLayout()
    val before = runOnFx(panes.map(p => (p.getHvalue, p.getVvalue)))
    RestingFocus.clear(fx, w)
    assertEquals(runOnFx(panes.map(p => (p.getHvalue, p.getVvalue))), before)
  }

  for theme <- StudioTheme.values do
    fxStage.test("small query workspace keeps map space " + theme.id) { fx =>
      val model = AppModel
        .update(
          StoryModels.t2Compare,
          Intent.SetAppearance(if theme == StudioTheme.Dark then Appearance.Dark
          else Appearance.Light)
        )
        ._1
      val w = boot(
        fx,
        model,
        StoryMoment.T2,
        panels = golden,
        project = Some(fixtureProject),
        clock = () => None
      )
      val hosts =
        Vector(w.summary.panels.queryView.plotHost, w.summary.panels.referenceView.plotHost)
      eventually(fx, "both query maps are drawn") {
        hosts.forall(_.status.get.isInstanceOf[PlotHostStatus.Drawn])
      }
      val heights = runOnFx(
        hosts.map(h =>
          h.frame.get.transform.panelFrame.height / h.frame.get.surface.deviceScale
        )
      )
      assert(
        heights.forall(_ >= MinimumMapHeight),
        s"map heights $heights, need $MinimumMapHeight"
      )
      runOnFx(w.shell.appBar.project.requestFocus())
      val paths = fx.snapshot(theme)
      runOnFx(KeyboardFocus.byKeyboard(w.summary.queries.node.getParent))
      cursorVisible(fx, w.summary.queries.node, ".queries-cursor")
      Files.writeString(
        paths.head.getParent.resolve("bounds.json"),
        ujson.write(
          ujson.Obj(
            "scene"               -> ujson.Arr(fx.sceneSize._1, fx.sceneSize._2),
            "mapDataFrameHeights" -> ujson.Arr.from(heights)
          ),
          indent = 2
        ) + "\n"
      )
    }

    fxStage.test("small Results workspace keeps space for axes and annotations " + theme.id) {
      fx =>
        val model = AppModel
          .update(
            StoryModels.t3Summary,
            Intent.SetAppearance(if theme == StudioTheme.Dark then Appearance.Dark
            else Appearance.Light)
          )
          ._1
        val w = boot(fx, model, StoryMoment.T3, clock = () => None)
        eventually(fx, "the participant plot is drawn") {
          w.summary.participantPlot.plotHost.status.get.isInstanceOf[PlotHostStatus.Drawn] &&
          w.summary.scaleProfile.plot.isDefined && w.summary.participantTable.source.isDefined &&
          w.summary.vm.scales.forall(_.available)
        }
        val height = runOnFx(w.summary.participantPlot.plotHost.getHeight)
        assert(
          height >= MinimumParticipantHeight,
          s"plot height $height, need $MinimumParticipantHeight"
        )
        val forward = resultsCycle(fx, w, backwards = false)
        val reverse = resultsCycle(fx, w, backwards = true)
        assertEquals(reverse, forward.head +: forward.tail.reverse)
        val paths = fx.snapshot(theme)
        runOnFx(KeyboardFocus.byKeyboard(w.summary.participants.node.getParent))
        cursorVisible(fx, w.summary.participants.node, ".participants-cursor")
        // F6 and Ctrl+Tab must land in the content that now owns each pane's
        // single stop, including both plot/table twins and reporting controls.
        runOnFx(
          w.host
            .node(w.runtime.model.focusedPane)
            .foreach(eyes4s.studio.desktop.dock.PerspectiveHost.focusInside)
        )
        val groups = runOnFx(w.runtime.model.layout.groups.size)
        (0 until groups).foreach { _ =>
          val group = runOnFx(w.runtime.model.layout.groupOf(w.runtime.model.focusedPane).get)
          group.panes.toVector.foreach { _ =>
            visibleFocus(fx, w)
            assert(
              runOnFx(
                w.host
                  .node(w.runtime.model.focusedPane)
                  .exists(contains(_, fx.scene.getFocusOwner))
              )
            )
            fx.robot.press(KeyCode.TAB, Modifiers(control = true))
          }
          fx.robot.press(KeyCode.F6)
        }
        Files.writeString(
          paths.head.getParent.resolve("bounds-and-focus.json"),
          ujson.write(
            ujson.Obj(
              "scene"                 -> ujson.Arr(fx.sceneSize._1, fx.sceneSize._2),
              "participantPlotHeight" -> height,
              "forward"               -> ujson.Arr.from(forward),
              "reverse"               -> ujson.Arr.from(reverse)
            ),
            indent = 2
          ) + "\n"
        )
    }

/** The same row-reveal contract at the reference board size. Native displays
  * may clamp it; each receipt records the actual scene dimensions.
  */
class BoardWorkspaceViewportFxSuite extends WorkspaceViewportFxSuite:
  override protected def stageSize: StageSize = StageSize.Board

  fxStage.test("Explore fits the viewport without horizontal displacement at board size") {
    fx =>
      val w = boot(
        fx,
        StoryModels.t2Explore,
        StoryMoment.T2,
        project = Some(fixtureProject),
        clock = () => None
      )
      eventually(fx, "the Explore map is drawn") {
        w.explore.trialView.status.get match
          case eyes4s.studio.desktop.trial.TrialViewStatus.Shown(s) => s.map.nonEmpty
          case _                                                    => false
      }
      RestingFocus.clear(fx, w)
      runOnFx {
        val workspace  = w.shell.workspace
        val content    = workspace.getContent
        val viewport   = workspace.lookup(".viewport")
        val actual     = content.localToScene(content.getLayoutBounds)
        val visible    = viewport.localToScene(viewport.getLayoutBounds)
        val excess     = math.max(0.0, actual.getWidth - visible.getWidth)
        val tolerance  = 0.01
        val diagnostic =
          s"content=$actual viewport=$visible scroll=${workspace.getHvalue} internal=${workspace.getViewportBounds}"
        assert(actual.getMinX <= visible.getMinX + tolerance, diagnostic)
        assert(actual.getMinX >= visible.getMinX - excess - tolerance, diagnostic)
        if visible.getWidth >= WorkspaceViewport.MinimumWidth then
          assertEqualsDouble(actual.getWidth, visible.getWidth, tolerance, diagnostic)
          assertEqualsDouble(actual.getMinX, visible.getMinX, tolerance, diagnostic)
      }
  }

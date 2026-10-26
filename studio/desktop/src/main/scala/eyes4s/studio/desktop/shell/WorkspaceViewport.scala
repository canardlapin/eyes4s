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

import javafx.application.Platform
import javafx.beans.value.ChangeListener
import javafx.event.EventHandler
import javafx.scene.Node
import javafx.scene.control.ScrollPane
import javafx.scene.input.{KeyEvent, MouseEvent}
import javafx.scene.layout.Region

/** Keeps the work area legible on smaller displays, with ordinary scroll bars.
  * Keyboard traversal and F6 reveal the focused control without another Tab
  * stop. Pointer drags remain available to plots, tables and dock dividers.
  */
final class WorkspaceViewport(content: Region, accessibleName: String)
    extends ScrollPane(content):
  import WorkspaceViewport.*

  content.setMinSize(MinimumWidth, MinimumHeight)
  setFitToWidth(true)
  setFitToHeight(true)
  setFocusTraversable(false)
  setMinSize(0.0, 0.0)
  setAccessibleText(accessibleName)
  getStyleClass.add("workspace-viewport")

  // ScrollPaneSkin caches its viewport by default. A live work area changes
  // inside that cache on every table scroll or canvas repaint, rebuilding a
  // whole-window bitmap at native scale. Keep its normal clip and scrolling,
  // but let the renderer update the changed descendants directly.
  skinProperty.addListener((_, _, _) => Option(lookup(".viewport")).foreach(_.setCache(false)))

  private var disposed = false
  private var pending  = false
  private val focused  = sceneProperty.flatMap[Node](_.focusOwnerProperty)
  private val sizeChanged: ChangeListener[javafx.geometry.Bounds] = (_, _, _) => revealLater()
  private val textChanged: ChangeListener[String]                 = (_, _, _) => revealLater()
  private val focusChanged: ChangeListener[Node]                  = (_, before, after) =>
    Option(before).foreach(_.boundsInParentProperty.removeListener(sizeChanged))
    Option(before).foreach(_.accessibleTextProperty.removeListener(textChanged))
    Option(after).foreach(_.boundsInParentProperty.addListener(sizeChanged))
    Option(after).foreach(_.accessibleTextProperty.addListener(textChanged))
    revealLater()
  private val keyed: EventHandler[KeyEvent] = _ => revealLater()
  // The ScrollPane skin otherwise takes focus after a canvas handles a
  // press. Stop bubbling only after every content/dock handler has run;
  // scrollbar presses and drag/scroll gestures retain their normal path.
  private val pressed: EventHandler[MouseEvent] = _.consume()
  content.addEventHandler(MouseEvent.MOUSE_PRESSED, pressed)
  addEventFilter(KeyEvent.KEY_PRESSED, keyed)
  focused.addListener(focusChanged)
  viewportBoundsProperty.addListener(sizeChanged)

  private def revealLater(): Unit =
    if !disposed && !pending then
      pending = true
      Platform.runLater { () =>
        pending = false
        if !disposed then
          // A row may have just been replaced or scrolled by its key handler.
          // Resolve its actual geometry before revealing it through this clip.
          content.applyCss()
          content.layout()
          // Clearing focus to the scene root must not search its descendants
          // for an unrelated cursor and change their scroll positions.
          Option(focused.getValue).filter(contains).foreach { node =>
            val b = node.getBoundsInLocal
            // Row navigation also reveals the row through an inner scroll
            // area when the complete composite fits the outer viewport.
            CompositeFocus.cursorTarget(node).foreach(reveal)
            if b.getHeight <= getViewportBounds.getHeight &&
              b.getWidth <= getViewportBounds.getWidth || CompositeFocus
                .cursorTarget(node)
                .isEmpty
            then reveal(node)
          }
      }

  private def contains(node: Node): Boolean =
    Iterator.iterate(node)(_.getParent).takeWhile(_ != null).exists(_ eq content)

  private def reveal(node: Node): Unit =
    if node.getScene != null && contains(node) then
      // Reveal inner scrolling first, then recalculate scene coordinates for
      // the outer work area. A cursor cannot escape either clip.
      Iterator
        .iterate(node.getParent)(_.getParent)
        .takeWhile(_ != null)
        .collect { case pane: ScrollPane => pane }
        .foreach { pane =>
          val area     = pane.getContent
          val bounds   = area.sceneToLocal(node.localToScene(node.getBoundsInLocal))
          val viewport = pane.getViewportBounds
          def position(
              value: Double,
              minimum: Double,
              maximum: Double,
              total: Double,
              visible: Double,
              start: Double,
              end: Double
          ): Double =
            val excess = math.max(0.0, total - visible)
            if excess == 0.0 || maximum <= minimum then minimum
            else
              val offset = (value - minimum) / (maximum - minimum) * excess
              val next   =
                if end - start > visible || start < offset then start
                else if end > offset + visible then end - visible
                else offset
              minimum + math.max(0.0, math.min(excess, next)) / excess * (maximum - minimum)
          pane.setHvalue(
            position(
              pane.getHvalue,
              pane.getHmin,
              pane.getHmax,
              area.getLayoutBounds.getWidth,
              viewport.getWidth,
              bounds.getMinX,
              bounds.getMaxX
            )
          )
          pane.setVvalue(
            position(
              pane.getVvalue,
              pane.getVmin,
              pane.getVmax,
              area.getLayoutBounds.getHeight,
              viewport.getHeight,
              bounds.getMinY,
              bounds.getMaxY
            )
          )
          pane.layout()
        }

  def dispose(): Unit =
    if !disposed then
      disposed = true
      Option(focused.getValue).foreach(_.boundsInParentProperty.removeListener(sizeChanged))
      Option(focused.getValue).foreach(_.accessibleTextProperty.removeListener(textChanged))
      removeEventFilter(KeyEvent.KEY_PRESSED, keyed)
      content.removeEventHandler(MouseEvent.MOUSE_PRESSED, pressed)
      focused.removeListener(focusChanged)
      viewportBoundsProperty.removeListener(sizeChanged)

object WorkspaceViewport:
  /** Logical pixels; board-sized windows expand the work area normally. */
  val MinimumWidth  = 1280.0
  val MinimumHeight = 864.0

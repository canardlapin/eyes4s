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
import javafx.scene.Node
import javafx.scene.control.ScrollPane
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

  private var disposed = false
  private var pending  = false
  private val focused  = sceneProperty.flatMap[Node](_.focusOwnerProperty)
  private val sizeChanged: ChangeListener[javafx.geometry.Bounds] = (_, _, _) => revealLater()
  private val focusChanged: ChangeListener[Node]                  = (_, before, after) =>
    Option(before).foreach(_.boundsInParentProperty.removeListener(sizeChanged))
    Option(after).foreach(_.boundsInParentProperty.addListener(sizeChanged))
    revealLater()
  focused.addListener(focusChanged)
  viewportBoundsProperty.addListener(sizeChanged)

  private def revealLater(): Unit =
    if !disposed && !pending then
      pending = true
      Platform.runLater { () =>
        pending = false
        if !disposed then Option(focused.getValue).foreach(reveal)
      }

  private def contains(node: Node): Boolean =
    Iterator.iterate(node)(_.getParent).takeWhile(_ != null).exists(_ eq content)

  private def reveal(node: Node): Unit =
    if node.getScene != null && contains(node) then
      val bounds   = content.sceneToLocal(node.localToScene(node.getBoundsInLocal))
      val viewport = getViewportBounds
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
      setHvalue(
        position(
          getHvalue,
          getHmin,
          getHmax,
          content.getWidth,
          viewport.getWidth,
          bounds.getMinX,
          bounds.getMaxX
        )
      )
      setVvalue(
        position(
          getVvalue,
          getVmin,
          getVmax,
          content.getHeight,
          viewport.getHeight,
          bounds.getMinY,
          bounds.getMaxY
        )
      )

  def dispose(): Unit =
    if !disposed then
      disposed = true
      Option(focused.getValue).foreach(_.boundsInParentProperty.removeListener(sizeChanged))
      focused.removeListener(focusChanged)
      viewportBoundsProperty.removeListener(sizeChanged)

object WorkspaceViewport:
  /** Logical pixels; board-sized windows expand the work area normally. */
  val MinimumWidth  = 1280.0
  val MinimumHeight = 864.0

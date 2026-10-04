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

package eyes4s.studio.desktop.plot

import eyes4s.studio.viz.plot.{Brushed, CanvasPoint, PlotBrushing}
import intaglio.DevicePoint
import javafx.application.Platform
import javafx.event.EventHandler
import javafx.scene.input.{MouseButton, MouseEvent}

/** A brush on a [[PlotTwin]]'s plot (ticket S4.5e): a primary-button drag
  * across the canvas selects exactly the rows whose x value lies in the
  * dragged span ([[PlotBrushing.brushed]]), through the plot's own input,
  * so the table and every other view see the same selection. The adapter
  * only binds pointer events; the span and the rows are computed by the
  * pure brushing. A press that does not move is a click, left to the plot.
  *
  * FX thread only. [[dispose]] removes its handler.
  */
final class PlotBrushAdapter private (twin: PlotTwin):

  private var start: Option[DevicePoint] = None
  private var last: Option[Brushed]      = None
  private var disposed: Boolean          = false

  private val handler: EventHandler[MouseEvent] = e =>
    if e.getButton == MouseButton.PRIMARY then
      val kind = e.getEventType
      if kind == MouseEvent.MOUSE_PRESSED then start = at(e)
      else if kind == MouseEvent.MOUSE_RELEASED then
        val from = start
        start = None
        if !e.isStillSincePress then
          for
            a       <- from
            b       <- at(e)
            targets <- twin.input.targets
            brushed <- PlotBrushing.brushed(targets, a, b)
          do
            last = Some(brushed)
            twin.input.brush(brushed.refs)

  twin.plotHost.addEventHandler(MouseEvent.ANY, handler)

  /** The last brush applied, if any. */
  def brushed: Option[Brushed] = last

  /** Removes the handler. Idempotent. */
  def dispose(): Unit =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"PlotBrushAdapter.dispose must run on the JavaFX application thread, not ${Thread.currentThread.getName}"
      )
    if !disposed then
      disposed = true
      twin.plotHost.removeEventHandler(MouseEvent.ANY, handler)

  // Host-local logical coordinates to the drawn frame's device pixels.
  private def at(e: MouseEvent): Option[DevicePoint] =
    twin.plotHost.frame.map(_.transform.canvasToDevice(CanvasPoint(e.getX, e.getY)))

object PlotBrushAdapter:

  /** A brush on `twin`'s plot. On the FX application thread. */
  def attach(twin: PlotTwin): PlotBrushAdapter =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"PlotBrushAdapter.attach must run on the JavaFX application thread, not ${Thread.currentThread.getName}"
      )
    PlotBrushAdapter(twin)

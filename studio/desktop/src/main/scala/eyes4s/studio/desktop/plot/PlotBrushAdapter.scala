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

import eyes4s.studio.app.plot.{BrushRule, HalfOpenSpan}
import eyes4s.studio.viz.plot.{Brushed, CanvasPoint, PlotBrushing, PlotBuilder}
import intaglio.DevicePoint
import javafx.application.Platform
import javafx.event.EventHandler
import javafx.scene.input.{MouseButton, MouseEvent}

/** A brush on a [[PlotTwin]]'s plot (ticket S4.5e): a primary-button drag
  * across the canvas selects exactly the rows `rule` picks from the dragged
  * span ([[PlotBrushing.brushed]]), through the plot's own input, so the
  * table and every other view see the same selection; the plot is then
  * drawn again with the span shaded (`draw` of the span).
  *
  * "Exactly" holds in data space: the span runs between the data values
  * under the press and the release, read through the plot's one transform,
  * and the rule is applied to the source's values. A row boundary that
  * falls within the release's pixel is resolved by that sub-pixel value,
  * not by what the pixel shows.
  *
  * The span is the view's state ([[span]]): `onBrush` reports each new one
  * so a host can keep it (S6.3), and [[restore]] draws a kept span again
  * without selecting. The adapter only binds pointer events; the span and
  * the rows are computed by the pure brushing. A press that does not move
  * is a click, left to the plot: while attached, the plot's input picks
  * only on such clicks.
  *
  * FX thread only. [[dispose]] removes its handler.
  */
final class PlotBrushAdapter private (
    twin: PlotTwin,
    rule: BrushRule,
    draw: Option[HalfOpenSpan] => PlotBuilder,
    onBrush: Option[HalfOpenSpan] => Unit
):

  private var start: Option[DevicePoint]  = None
  private var last: Option[Brushed]       = None
  private var shown: Option[HalfOpenSpan] = None
  private var disposed: Boolean           = false

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
            brushed <- PlotBrushing.brushed(targets, rule, a, b)
          do
            last = Some(brushed)
            twin.input.brush(brushed.refs)
            redraw(Some(brushed.span))
            onBrush(shown)

  twin.plotHost.addEventHandler(MouseEvent.ANY, handler)
  twin.input.brushOwnsDrags(true)

  /** The last brush a drag applied, with the rows it selected. */
  def brushed: Option[Brushed] = last

  /** The span drawn on the plot, if any. */
  def span: Option[HalfOpenSpan] = shown

  /** Draws `span` (or none) on the plot, as a kept brush is restored; the
    * selection is left as it is.
    */
  def restore(span: Option[HalfOpenSpan]): Unit =
    onFxThread("restore")
    if !disposed then redraw(span)

  /** Removes the handler. Idempotent. */
  def dispose(): Unit =
    onFxThread("dispose")
    if !disposed then
      disposed = true
      twin.plotHost.removeEventHandler(MouseEvent.ANY, handler)
      twin.input.brushOwnsDrags(false)

  private def redraw(span: Option[HalfOpenSpan]): Unit =
    shown = span
    twin.rebuild(draw(span))

  // Host-local logical coordinates to the drawn frame's device pixels.
  private def at(e: MouseEvent): Option[DevicePoint] =
    twin.plotHost.frame.map(_.transform.canvasToDevice(CanvasPoint(e.getX, e.getY)))

  private def onFxThread(operation: String): Unit =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"PlotBrushAdapter.$operation must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )

object PlotBrushAdapter:

  /** A brush on `twin`'s plot picking rows by `rule`, drawing the plot with
    * `draw` of the span after each brush and reporting the span to
    * `onBrush`. On the FX application thread.
    */
  def attach(
      twin: PlotTwin,
      rule: BrushRule,
      draw: Option[HalfOpenSpan] => PlotBuilder,
      onBrush: Option[HalfOpenSpan] => Unit = _ => ()
  ): PlotBrushAdapter =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        "PlotBrushAdapter.attach must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )
    PlotBrushAdapter(twin, rule, draw, onBrush)

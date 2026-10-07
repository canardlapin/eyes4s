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

package eyes4s.studio.viz.plot

import eyes4s.studio.core.selection.StudioRef
import intaglio.{
  Clip,
  DashPattern,
  DevicePoint,
  GraphicParams,
  GraphicsError,
  Grob,
  Interval,
  LineType,
  Point,
  Scene,
  Size,
  Viewport,
  YDirection
}
import cats.syntax.all.*

/** One selected contiguous line run, in device pixels. */
final case class OverlayLine(ref: StudioRef, points: Vector[DevicePoint]) derives CanEqual

/** Feedback for marks drawn as lines and marks drawn as points or bars. */
object OverlaySelections:
  val LinePx: Double         = 3.0
  val DashPx: Vector[Double] = Vector(6.0, 3.0)

  def scene(
      rings: Vector[OverlayRing],
      lines: Vector[OverlayLine],
      palette: OverlayPalette,
      width: Double,
      height: Double,
      deviceScale: Double
  ): Either[GraphicsError, Scene] =
    if lines.isEmpty then OverlayRings.scene(rings, palette, width, height, deviceScale)
    else
      for
        ringsScene <- OverlayRings.scene(rings, palette, width, height, deviceScale)
        dash       <- DashPattern(DashPx.map(_ * deviceScale))
        gp         <- GraphicParams.checked(
          stroke = Some(IntaglioColours.toIntaglio(palette.selectedOuter)),
          fill = None,
          lineWidth = LinePx * deviceScale,
          lineType = LineType.Custom(dash)
        )
        grobs <- lines.traverse(line =>
          line.points.traverse(p => Point.native(p.x, p.y)).flatMap(Grob.lines(_, gp = gp))
        )
        xs    <- Interval(0.0, width)
        ys    <- Interval(0.0, height)
        at    <- Point.npc(0.0, 0.0)
        whole <- Size.npc(1.0, 1.0)
        port  <- Viewport.checked(at, whole, xs, ys, Clip.Off, yDirection = YDirection.Down)
      yield Scene(Vector(Grob.group(grobs, viewport = Some(port)))) ++ ringsScene

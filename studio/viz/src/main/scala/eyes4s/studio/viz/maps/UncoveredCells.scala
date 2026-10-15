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

package eyes4s.studio.viz.maps

import eyes4s.studio.app.maps.MapGrid
import intaglio.{GraphicParams, GraphicsError, GraphicsName, Grob, Point}

/** A map's cells without coverage as the one shape that hatches them (review
  * ticket 8): never transparent and never a ramp colour. The trial view's map
  * and a figure's map tile both draw it, each in its own hatch paint.
  *
  * One ring for each run of missing cells along a row, all in one shape:
  * runs in adjacent rows share an edge, and as separate rects each would
  * cover only its part of a device pixel that edge crosses, so the screen
  * showed through the seam (bead bd-01M4TCR2EWPSX1NG4R686MY3M7). One fill
  * covers the seam once.
  */
object UncoveredCells:

  // One run of missing cells: row `row` (0 is the top), columns `from`
  // until `until`.
  private final case class Run(row: Int, from: Int, until: Int)

  // The runs of `grid`'s missing cells, top row first, left to right.
  private def runs(grid: MapGrid): Vector[Run] =
    for
      y   <- (0 until grid.rows).toVector
      run <- (0 until grid.columns).toVector
        .filter(x => grid.atTop(x, y).isEmpty)
        .foldLeft(Vector.empty[(Int, Int)]) {
          case (rs :+ ((from, to)), x) if x == to => rs :+ (from -> (x + 1))
          case (rs, x)                            => rs :+ (x    -> (x + 1))
        }
    yield Run(y, run._1, run._2)

  /** `grid`'s missing cells over the region at (`left`, `top`), `width` by
    * `height` native units with y down, as one compound polygon filled with
    * `paint`; `None` when every cell is covered.
    */
  def shape(
      grid: MapGrid,
      left: Double,
      top: Double,
      width: Double,
      height: Double,
      paint: GraphicParams,
      name: GraphicsName
  ): Either[GraphicsError, Option[Grob]] =
    val cellW = width / grid.columns
    val cellH = height / grid.rows
    val found = runs(grid)
    if found.isEmpty then Right(None)
    else
      for
        rings <- found.foldLeft[Either[GraphicsError, Vector[Vector[Point]]]](
          Right(Vector.empty)
        ) { case (acc, Run(y, from, to)) =>
          val (x0, x1) = (left + from * cellW, left + to * cellW)
          val (y0, y1) = (top + y * cellH, top + (y + 1) * cellH)
          for
            rs <- acc
            a  <- Point.native(x0, y0)
            b  <- Point.native(x1, y0)
            c  <- Point.native(x1, y1)
            d  <- Point.native(x0, y1)
          yield rs :+ Vector(a, b, c, d)
        }
        shape <- Grob.compoundPolygon(rings, gp = paint, name = Some(name))
      yield Some(shape)

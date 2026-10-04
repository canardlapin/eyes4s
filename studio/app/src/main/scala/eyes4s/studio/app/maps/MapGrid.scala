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

package eyes4s.studio.app.maps

import eyes4s.studio.core.backend.TrialKey
import eyes4s.studio.core.selection.ScaleIndex

/** Why a map grid was refused. Every case names the map and what failed. */
enum MapGridError derives CanEqual:

  /** The grid has no cells along an axis. */
  case Empty(map: MapId, columns: Int, rows: Int)

  /** The grid lists `cells` values for `columns × rows` cells. */
  case CellCount(map: MapId, columns: Int, rows: Int, cells: Int)

  /** Cell `index` holds `value`, which is not a finite number. */
  case NotFinite(map: MapId, index: Int, value: Double)

  /** Isoline level `index` is `level`, which is not a finite number. */
  case Level(map: MapId, index: Int, level: Double)

  def message: String = this match
    case Empty(m, c, r)        => s"Map ${m.label}: a grid of $c × $r cells has none."
    case CellCount(m, c, r, n) => s"Map ${m.label}: $n values for $c × $r cells."
    case NotFinite(m, i, v)    => s"Map ${m.label}: cell $i holds $v, which is not finite."
    case Level(m, i, l) => s"Map ${m.label}: isoline level $i is $l, which is not finite."

/** Which map a grid is: one trial's density at one scale of a run. */
final case class MapId(trial: TrialKey, scale: ScaleIndex) derives CanEqual:
  def label: String = s"${trial.label} at scale ${scale.value}"

/** One map's values as the backend serves them (ticket S4.4; UI-E):
  * `columns × rows` cells, x fastest, each a density value or missing.
  * Missing is not zero: a cell without a value is drawn as nothing.
  * `levels` are the isoline levels the backend supplies (UI-E: 50% and 90%
  * of mass), kept exactly as served; studio never derives a level, it only
  * contours the ones it is given (S4.3b).
  */
final case class MapGrid private (
    map: MapId,
    columns: Int,
    rows: Int,
    cells: Vector[Option[Double]],
    levels: Vector[Double]
) derives CanEqual:

  /** The value of the cell at column `x` and row `y`, if it has one. */
  def at(x: Int, y: Int): Option[Double] =
    if x < 0 || y < 0 || x >= columns || y >= rows then None else cells(y * columns + x)

  /** A digest of the grid's identity, shape, values and levels: the same
    * for the same stored values, whatever palette or opacity draws them.
    * FNV-1a over the bits of every number.
    */
  def contentHash: Long =
    val prime                       = 0x100000001b3L
    def mix(h: Long, v: Long): Long =
      (0 until 8).foldLeft(h)((acc, k) => (acc ^ ((v >>> (8 * k)) & 0xffL)) * prime)
    val shaped = mix(mix(0xcbf29ce484222325L, columns.toLong), rows.toLong)
    val valued = cells.foldLeft(shaped) {
      case (h, Some(v)) => mix(mix(h, 1L), java.lang.Double.doubleToLongBits(v))
      case (h, None)    => mix(h, 0L)
    }
    levels.foldLeft(mix(valued, levels.size.toLong))((h, l) =>
      mix(h, java.lang.Double.doubleToLongBits(l))
    )

object MapGrid:

  /** A grid of `columns × rows` cells of `map`, refusing an empty shape, a
    * wrong number of cells, and a non-finite value or level.
    */
  def of(
      map: MapId,
      columns: Int,
      rows: Int,
      cells: Vector[Option[Double]],
      levels: Vector[Double]
  ): Either[MapGridError, MapGrid] =
    if columns <= 0 || rows <= 0 then Left(MapGridError.Empty(map, columns, rows))
    else if cells.size.toLong != columns.toLong * rows then
      Left(MapGridError.CellCount(map, columns, rows, cells.size))
    else
      cells.iterator.zipWithIndex
        .collectFirst { case (Some(v), i) if !v.isFinite => MapGridError.NotFinite(map, i, v) }
        .orElse(
          levels.iterator.zipWithIndex.collectFirst {
            case (l, i) if !l.isFinite => MapGridError.Level(map, i, l)
          }
        )
        .toLeft(MapGrid(map, columns, rows, cells, levels))

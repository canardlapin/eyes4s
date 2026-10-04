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

/** A point of a map grid, in cells: `x` from 0 at the grid's left edge to
  * its columns at the right, `y` from 0 at the map's top edge to its rows
  * at the bottom, whatever the grid's stored [[RowOrder]]. A cell's value stands at its centre, `(i + 0.5, j + 0.5)`.
  */
final case class GridPoint(x: Double, y: Double) derives CanEqual

/** One isoline: the contour of a map at one backend-supplied level, as
  * segments in grid coordinates.
  */
final case class Isoline(level: Double, segments: Vector[(GridPoint, GridPoint)])
    derives CanEqual

/** The contours of a map at the levels its grid carries (ticket S4.3b).
  * Studio does not choose the levels; it only draws where the grid crosses
  * the ones the backend supplied (UI-E: 50% and 90% of mass).
  *
  * Marching squares over the cell centres: each square of four neighbouring
  * centres contributes the segments where its edges cross the level,
  * linearly interpolated along the edge. A corner at or above the level is
  * inside. A square with a missing corner contributes nothing, so no line is
  * drawn through a missing value; a saddle is split by the mean of its
  * corners.
  */
object Isolines:

  /** The isolines of `grid`, one per level, in the grid's level order. */
  def of(grid: MapGrid): Vector[Isoline] = grid.levels.map(l => Isoline(l, contour(grid, l)))

  /** The segments where `grid` crosses `level`. */
  def contour(grid: MapGrid, level: Double): Vector[(GridPoint, GridPoint)] =
    val out = Vector.newBuilder[(GridPoint, GridPoint)]
    for
      j <- 0 until grid.rows - 1
      i <- 0 until grid.columns - 1
    do
      for
        a <- grid.atTop(i, j)         // top left
        b <- grid.atTop(i + 1, j)     // top right
        c <- grid.atTop(i + 1, j + 1) // bottom right
        d <- grid.atTop(i, j + 1)     // bottom left
      do out ++= square(i, j, a, b, c, d, level)
    out.result()

  // The segments of the square whose top-left centre is cell (i, j).
  private def square(
      i: Int,
      j: Int,
      a: Double,
      b: Double,
      c: Double,
      d: Double,
      level: Double
  ): Vector[(GridPoint, GridPoint)] =
    val (x0, y0)                              = (i + 0.5, j + 0.5)
    def cross(v0: Double, v1: Double): Double =
      if v1 == v0 then 0.5 else (level - v0) / (v1 - v0)
    lazy val top           = GridPoint(x0 + cross(a, b), y0)
    lazy val right         = GridPoint(x0 + 1.0, y0 + cross(b, c))
    lazy val bottom        = GridPoint(x0 + cross(d, c), y0 + 1.0)
    lazy val left          = GridPoint(x0, y0 + cross(a, d))
    def in(v: Double): Int = if v >= level then 1 else 0
    (in(a) << 3) | (in(b) << 2) | (in(c) << 1) | in(d) match
      case 0 | 15 => Vector.empty
      case 1 | 14 => Vector((left, bottom))
      case 2 | 13 => Vector((bottom, right))
      case 3 | 12 => Vector((left, right))
      case 4 | 11 => Vector((top, right))
      case 6 | 9  => Vector((top, bottom))
      case 7 | 8  => Vector((left, top))
      case 5      =>
        // a and c out, b and d in: joined through the middle when it is in.
        if (a + b + c + d) / 4.0 >= level then Vector((left, top), (bottom, right))
        else Vector((left, bottom), (top, right))
      case 10 =>
        // a and c in, b and d out.
        if (a + b + c + d) / 4.0 >= level then Vector((left, bottom), (top, right))
        else Vector((left, top), (bottom, right))
      case _ => Vector.empty // 0 and 15: the square is all out or all in

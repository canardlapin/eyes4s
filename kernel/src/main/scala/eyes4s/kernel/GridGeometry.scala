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

package eyes4s.kernel

import cats.syntax.all.*

/** Cell order carried with rendering geometry: x varies fastest. */
enum GridCellOrder derives CanEqual:
  case RowMajorXFastest

/** Checked rendering geometry. Cell extents are in the grid's static unit;
  * angular extents exist only when an explicit linear angular scale is supplied.
  * `origin` is the lower corner in `admissionFrame`, including a window's offset.
  */
final class GridGeometry[U <: Unit2D] private (
    val grid: Grid[U],
    val admissionFrame: Frame[U],
    val origin: Pt[U],
    val cell: Extent[U],
    val cellDegrees: Option[Extent[Unit2D.Deg]]
):
  def frame: Frame[U]                              = grid.frame
  def bounds: Bounds[U]                            = grid.frame.bounds
  def yAxis: YAxis                                 = grid.frame.yAxis
  def nx: Int                                      = grid.nx
  def ny: Int                                      = grid.ny
  def order: GridCellOrder                         = GridCellOrder.RowMajorXFastest
  def rowMajorIndex(ix: Int, iy: Int): Option[Int] = grid.indexAt(ix, iy)

  override def equals(other: Any): Boolean = other match
    case that: GridGeometry[?] =>
      grid == that.grid && admissionFrame == that.admissionFrame && origin == that.origin &&
      cell == that.cell && cellDegrees == that.cellDegrees
    case _ => false
  override def hashCode: Int = (grid, admissionFrame, origin, cell, cellDegrees).hashCode

object GridGeometry:
  /** `window`, if present, belongs to the admission frame and its local
    * frame must agree with the grid. The angular scale names the admission
    * frame, as study plans do. No viewing geometry is inferred.
    */
  def of[U <: Unit2D](
      grid: Grid[U],
      window: Option[Subframe[U]] = None,
      angularScale: Option[LinearAngularScale[U]] = None
  ): Either[GeometryError, GridGeometry[U]] =
    val admission = window.fold(grid.frame)(_.parent)
    val origin    = window.fold(Pt[U](grid.frame.bounds.xMin, grid.frame.bounds.yMin))(_.origin)
    for
      _       <- window.traverse(w => Agreement.frames(grid.frame, w.frame))
      _       <- angularScale.traverse(s => Agreement.frames(admission, s.frame))
      cell    <- Extent.of[U](grid.cellWidth, grid.cellHeight)
      degrees <- angularScale.traverse(s =>
        Extent.of[Unit2D.Deg](cell.width / s.unitsPerDegree, cell.height / s.unitsPerDegree)
      )
    yield new GridGeometry(grid, admission, origin, cell, degrees)

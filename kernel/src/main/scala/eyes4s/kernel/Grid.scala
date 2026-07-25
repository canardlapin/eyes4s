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

/** A standard deviation, in the units of the frame it applies to.
  *
  * ==Always a standard deviation==
  *
  * Never a bandwidth in some backend's private convention, never a full width,
  * never a half width. The reference R implementation produces a **four times
  * different** kernel depending on which density package happens to be
  * installed, because one path treats the parameter as an SD and the fallbacks
  * divide it by four internally; the switch is announced by a `message()` in
  * the middle of a computation. A backend with a different convention converts
  * at its own boundary, and this type is what it converts to.
  *
  * The unit parameter is what makes a bandwidth comparable across studies. A
  * one-degree kernel is a statement about the fovea; a thirty-pixel kernel is a
  * statement about nothing until you say which screen.
  */
opaque type Sigma[U <: Unit2D] = Double

object Sigma:

  def of[U <: Unit2D](value: Double): Either[GeometryError, Sigma[U]] =
    if !value.isFinite then Left(GeometryError.NonFiniteSigma(value))
    else if value <= 0.0 then Left(GeometryError.NonPositiveSigma(value))
    else Right(value)

  def deg(v: Double): Either[GeometryError, Sigma[Unit2D.Deg]]   = of[Unit2D.Deg](v)
  def px(v: Double): Either[GeometryError, Sigma[Unit2D.Px]]     = of[Unit2D.Px](v)
  def norm(v: Double): Either[GeometryError, Sigma[Unit2D.Norm]] = of[Unit2D.Norm](v)

  extension [U <: Unit2D](s: Sigma[U])
    def value: Double = s

    /** The variance this standard deviation implies. */
    def variance: Double = s * s

    /** Scale, e.g. when a bandwidth is expressed as a multiple of another. */
    def *(k: Double): Either[GeometryError, Sigma[U]] = of[U](s * k)

    def render(using u: UnitLabel[U]): String = f"σ=$s%.4g${u.symbol}"

  given [U <: Unit2D]: cats.kernel.Order[Sigma[U]] =
    cats.kernel.Order.from((a, b) => java.lang.Double.compare(a, b))

end Sigma

/** Nominal identity of a discretisation.
  *
  * Two grids of the same dimensions over different frames are not
  * interchangeable, and neither are two grids over the same frame at different
  * resolutions. Identity is by name for the same reason [[FrameId]] is.
  */
final case class GridId(name: String) derives CanEqual:
  override def toString: String = name

/** A regular rectangular discretisation of a frame.
  *
  * ==Index order is stated, once==
  *
  * `index = iy * nx + ix`, so `x` varies fastest and rows are contiguous. This
  * is the image convention. It is written here because the alternative is what
  * the reference implementation does: rely on the host language's
  * column-major flattening as an *unstated* invariant, relied upon in five
  * separate files, none of which mentions it.
  *
  * ==Cells are areas, positions are their centres==
  *
  * A grid cell covers a rectangle; [[cellCentre]] returns the middle of it. A
  * density evaluated "at" a cell is evaluated at its centre, and a mass
  * assigned "to" a cell belongs to the whole rectangle. Conflating the two is
  * how cell-area factors go missing from normalisation.
  */
final case class Grid[U <: Unit2D] private (
    id: GridId,
    frame: Frame[U],
    nx: Int,
    ny: Int
) derives CanEqual:

  def size: Int = nx * ny

  def cellWidth: Double  = frame.bounds.width / nx
  def cellHeight: Double = frame.bounds.height / ny
  def cellArea: Double   = cellWidth * cellHeight

  def indexAt(ix: Int, iy: Int): Int = iy * nx + ix

  def columnOf(index: Int): Int = index % nx
  def rowOf(index: Int): Int    = index / nx

  /** The centre of cell `index`. */
  def cellCentre(index: Int): Pt[U] =
    Pt(
      frame.bounds.xMin + (columnOf(index) + 0.5) * cellWidth,
      frame.bounds.yMin + (rowOf(index) + 0.5) * cellHeight
    )

  /** The cell containing `p`, or `None` when it falls outside the frame. */
  def indexOf(p: Pt[U]): Option[Int] =
    if !frame.bounds.contains(p) then None
    else
      val ix = ((p.x - frame.bounds.xMin) / cellWidth).toInt
      val iy = ((p.y - frame.bounds.yMin) / cellHeight).toInt
      // Guard the top edge against floating-point landing exactly on nx/ny.
      Some(indexAt(math.min(ix, nx - 1), math.min(iy, ny - 1)))

  /** Every cell centre, in index order. */
  def centres: IArray[Pt[U]] =
    IArray.tabulate(size)(cellCentre)

  def render(using u: UnitLabel[U]): String =
    s"$id ${nx}x$ny over ${frame.id}${u.symbol}"

end Grid

object Grid:

  def of[U <: Unit2D](
      id: GridId,
      frame: Frame[U],
      nx: Int,
      ny: Int
  ): Either[GeometryError, Grid[U]] =
    if nx <= 0 || ny <= 0 then Left(GeometryError.DegenerateGrid(nx, ny))
    else Right(Grid(id, frame, nx, ny))

  /** A grid named after its frame and resolution.
    *
    * Convenient, and safe precisely because the name encodes both: two grids
    * built this way collide only when they genuinely agree.
    */
  def over[U <: Unit2D](frame: Frame[U], nx: Int, ny: Int): Either[GeometryError, Grid[U]] =
    of(GridId(s"${frame.id.name}@${nx}x$ny"), frame, nx, ny)

  /** A square grid over a frame. */
  def square[U <: Unit2D](frame: Frame[U], n: Int): Either[GeometryError, Grid[U]] =
    over(frame, n, n)

end Grid

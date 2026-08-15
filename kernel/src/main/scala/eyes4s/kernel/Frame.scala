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

/** A size -- a width and a height -- in a known unit.
  *
  * Distinct from [[Bounds]], which is a size *and* a position. A dispersion
  * threshold, a stimulus size or a tolerance box has extent but no location,
  * and giving it one would invite the question of where it is.
  */
final case class Extent[U <: Unit2D] private (width: Double, height: Double) derives CanEqual:
  def area: Double                          = width * height
  def diagonal: Double                      = math.hypot(width, height)
  def render(using u: UnitLabel[U]): String = f"$width%.3g x $height%.3g${u.symbol}"

object Extent:
  def of[U <: Unit2D](width: Double, height: Double): Either[GeometryError, Extent[U]] =
    if !(width.isFinite && height.isFinite) then
      Left(GeometryError.NonFiniteBounds(0, 0, width, height))
    else if width <= 0.0 || height <= 0.0 then
      Left(GeometryError.DegenerateBounds(0, 0, width, height))
    else Right(Extent(width, height))

  /** A square extent. */
  def square[U <: Unit2D](side: Double): Either[GeometryError, Extent[U]] = of(side, side)

/** An axis-aligned rectangular extent, in a known unit.
  *
  * Bounds are stored as an explicit rectangle rather than a width and height,
  * because coordinate systems in this domain are not all anchored at zero. A
  * screen runs `[0, 1280) x [0, 1024)`; an angular frame centred on the display
  * centre runs `[-17, 17] x [-13.5, 13.5]`. Deriving one from the other by
  * convention is how the reference R implementation ends up with four unrelated
  * notions of "the screen" -- a clip rectangle, a per-trial data range, a
  * `c(0, 1000)` default and a pair of denominators -- none derived from any
  * other.
  */
final case class Bounds[U <: Unit2D] private (
    xMin: Double,
    yMin: Double,
    xMax: Double,
    yMax: Double
) derives CanEqual:

  def width: Double  = xMax - xMin
  def height: Double = yMax - yMin

  /** The size, forgetting the position. */
  def extent: Extent[U] = Extent.of[U](width, height).toOption.get

  /** The corner-to-corner distance.
    *
    * Named once here because scanpath measures normalise by it constantly, and
    * the reference implementation recomputes `sqrt(w^2 + h^2)` inline at five
    * separate sites.
    */
  def diagonal: Double = math.hypot(width, height)

  def area: Double = width * height

  def centre: Pt[U] = Pt((xMin + xMax) / 2.0, (yMin + yMax) / 2.0)

  /** Half-open in both axes, matching [[Interval]]'s convention for time. */
  def contains(p: Pt[U]): Boolean =
    p.x >= xMin && p.x < xMax && p.y >= yMin && p.y < yMax

  def clamp(p: Pt[U]): Pt[U] =
    val xUpper = java.lang.Math.nextDown(xMax)
    val yUpper = java.lang.Math.nextDown(yMax)
    Pt(math.min(math.max(p.x, xMin), xUpper), math.min(math.max(p.y, yMin), yUpper))

  def render(using u: UnitLabel[U]): String =
    f"[$xMin%.1f, $xMax%.1f) x [$yMin%.1f, $yMax%.1f)${u.symbol}"

end Bounds

object Bounds:

  def of[U <: Unit2D](
      xMin: Double,
      yMin: Double,
      xMax: Double,
      yMax: Double
  ): Either[GeometryError, Bounds[U]] =
    if !(xMin.isFinite && yMin.isFinite && xMax.isFinite && yMax.isFinite) then
      Left(GeometryError.NonFiniteBounds(xMin, yMin, xMax, yMax))
    else if xMax <= xMin || yMax <= yMin then
      Left(GeometryError.DegenerateBounds(xMin, yMin, xMax, yMax))
    else Right(Bounds(xMin, yMin, xMax, yMax))

  /** `[0, w) x [0, h)`, the usual device rectangle. */
  def sized[U <: Unit2D](width: Double, height: Double): Either[GeometryError, Bounds[U]] =
    of(0.0, 0.0, width, height)

  /** `[-hw, hw] x [-hh, hh]`, centred on the origin. */
  def centred[U <: Unit2D](
      halfWidth: Double,
      halfHeight: Double
  ): Either[GeometryError, Bounds[U]] =
    of(-halfWidth, -halfHeight, halfWidth, halfHeight)

end Bounds

/** Which way the vertical axis runs.
  *
  * Eye trackers and image formats put the origin at the top left with `y`
  * increasing downward; mathematics and most plotting put it at the bottom left
  * with `y` increasing upward. The reference R implementation performs this flip
  * through the *sign of a clip-bounds argument* -- passing `c(0, 1280, 1024, 0)`
  * rather than `c(0, 1280, 0, 1024)` silently inverts the axis -- and records
  * nowhere that it happened. Here it is a field, and it survives.
  */
enum YAxis derives CanEqual:
  case Down
  case Up

/** Nominal identity of a coordinate frame.
  *
  * Two frames with identical bounds are still different frames if they describe
  * different things: the display, and the image shown on part of it. Structural
  * equality would silently conflate them, so identity is by name, following the
  * same reasoning as `linop4s`'s `SpaceId`.
  */
final case class FrameId(name: String) derives CanEqual:
  override def toString: String = name

/** Structural coordinate metadata carried under a nominal [[FrameId]].
  *
  * It is obtained from a validated [[Frame]], not constructed independently.
  * This keeps identity nominal while allowing [[Agreement]] to detect the
  * corrupt-metadata case in which one identity names two geometries.
  */
final case class FrameSpec private (
    xMin: Double,
    yMin: Double,
    xMax: Double,
    yMax: Double,
    yAxis: YAxis
) derives CanEqual:
  def render: String = s"[$xMin, $xMax) x [$yMin, $yMax), y-$yAxis"

object FrameSpec:
  private[kernel] def from[U <: Unit2D](frame: Frame[U]): FrameSpec =
    FrameSpec(
      frame.bounds.xMin,
      frame.bounds.yMin,
      frame.bounds.xMax,
      frame.bounds.yMax,
      frame.yAxis
    )

/** A planar coordinate frame: an identity, an extent, and an axis direction.
  *
  * A `Frame` is carried by whole collections -- a recording, a scanpath, a grid
  * -- not by individual positions. That is the level at which the data actually
  * shares a coordinate system, and it is why the geometry cannot be forgotten
  * partway through an analysis the way it is when bounds are merely an argument.
  */
final case class Frame[U <: Unit2D] private (
    id: FrameId,
    bounds: Bounds[U],
    yAxis: YAxis
) derives CanEqual:

  def spec: FrameSpec = FrameSpec.from(this)

  def contains(p: Pt[U]): Boolean = bounds.contains(p)
  def centre: Pt[U]               = bounds.centre
  def diagonal: Double            = bounds.diagonal
  def width: Double               = bounds.width
  def height: Double              = bounds.height

  /** Same shape, different identity. Useful for per-stimulus frames that share
    * a geometry but must not be interchangeable.
    */
  def withId(newId: FrameId): Frame[U] = Frame(newId, bounds, yAxis)

  def render(using u: UnitLabel[U]): String =
    s"$id ${bounds.render} y-$yAxis"

end Frame

object Frame:

  def of[U <: Unit2D](id: FrameId, bounds: Bounds[U], yAxis: YAxis): Frame[U] =
    Frame(id, bounds, yAxis)

  /** A display of `width` x `height` pixels, origin top-left, `y` downward --
    * the convention every eye tracker emits.
    */
  def screen(
      name: String,
      width: Int,
      height: Int
  ): Either[GeometryError, Frame[Unit2D.Px]] =
    Bounds
      .sized[Unit2D.Px](width.toDouble, height.toDouble)
      .map(Frame(FrameId(name), _, YAxis.Down))

  /** An angular frame centred on the origin, spanning `width` x `height`
    * degrees, `y` upward.
    */
  def angular(
      name: String,
      width: Double,
      height: Double
  ): Either[GeometryError, Frame[Unit2D.Deg]] =
    Bounds
      .centred[Unit2D.Deg](width / 2.0, height / 2.0)
      .map(Frame(FrameId(name), _, YAxis.Up))

  /** The unit square, origin top-left, matching image coordinates. */
  def unitSquare(name: String): Either[GeometryError, Frame[Unit2D.Norm]] =
    Bounds.sized[Unit2D.Norm](1.0, 1.0).map(Frame(FrameId(name), _, YAxis.Down))

end Frame

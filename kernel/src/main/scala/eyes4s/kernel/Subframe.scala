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

/** Where a parent-frame position falls relative to a [[Subframe]] whose
  * extent is half-open, `[x0, x1) x [y0, y1)`, like every [[Bounds]].
  *
  * `Inside` carries the position in the subframe's own coordinates; `Outside`
  * keeps the parent position unchanged. Nothing is clamped or dropped.
  */
enum HalfOpenPlacement[U <: Unit2D] derives CanEqual:
  case Inside(local: Pt[U])
  case Outside(parent: Pt[U])

  def isInside: Boolean = this match
    case Inside(_)  => true
    case Outside(_) => false

/** A rectangular window of a parent frame, with its own nominal frame.
  *
  * The window occupies the half-open region `[x0, x1) x [y0, y1)` of `parent`.
  * Its own `frame` has the same unit and axis direction, its origin at the
  * region's minimum corner and extent `[0, x1 - x0) x [0, y1 - y0)`: a
  * 1024 x 768 image centred on a 1920 x 1080 display is the region
  * `[448, 1472) x [156, 924)` of the display frame and the frame
  * `[0, 1024) x [0, 768)` of the image.
  *
  * The two frames have different identities, so a position cannot silently
  * pass from one to the other: [[enter]] and [[exit]] are the translations
  * between them, and every containment decision goes through [[locate]].
  * Containment is decided once, by the half-open region in the parent's
  * coordinates. Translating an inside position can round it onto the window's
  * far edge (for a region with a negative corner, say); the local position is
  * then the largest double below that edge, so every inside position lies in
  * the window's frame and a grid over it bins it. A window that does not lie
  * within its parent is unrepresentable.
  */
final class Subframe[U <: Unit2D] private (
    val parent: Frame[U],
    val frame: Frame[U],
    val region: Bounds[U]
):
  /** The parent-frame position of the window's own origin. */
  def origin: Pt[U] = Pt(region.xMin, region.yMin)

  /** Translation from the parent frame into the window's frame. */
  def enter: Warp[U, U] =
    Warp.Affine(parent, frame, Mat3.translation(-region.xMin, -region.yMin))

  /** Translation from the window's frame back into the parent frame. */
  def exit: Warp[U, U] =
    Warp.Affine(frame, parent, Mat3.translation(region.xMin, region.yMin))

  /** Classify a parent-frame position against the half-open window. */
  def locate(p: Pt[U]): HalfOpenPlacement[U] =
    if region.contains(p) then
      HalfOpenPlacement.Inside(frame.bounds.clamp(Pt[U](p.x - region.xMin, p.y - region.yMin)))
    else HalfOpenPlacement.Outside(p)

  override def equals(that: Any): Boolean = that match
    case s: Subframe[?] => parent == s.parent && frame == s.frame && region == s.region
    case _              => false
  override def hashCode: Int = (parent, frame, region).hashCode

  def render(using u: UnitLabel[U]): String =
    s"${frame.id} = ${region.render} of ${parent.id}"

object Subframe:

  /** The window `region` of `parent`, named `id`. The region is half-open and
    * must lie within the parent's bounds; the window's frame starts at the
    * region's minimum corner.
    */
  def of[U <: Unit2D](
      parent: Frame[U],
      id: FrameId,
      region: Bounds[U]
  ): Either[GeometryError, Subframe[U]] =
    val within =
      region.xMin >= parent.bounds.xMin && region.yMin >= parent.bounds.yMin &&
        region.xMax <= parent.bounds.xMax && region.yMax <= parent.bounds.yMax
    if id == parent.id then Left(GeometryError.SubframeIdentity(id))
    else if !within then
      Left(
        GeometryError.SubframeOutsideParent(
          id,
          region.xMin,
          region.yMin,
          region.xMax,
          region.yMax,
          parent.id,
          parent.spec
        )
      )
    else
      Bounds
        .sized[U](region.width, region.height)
        .map(local => new Subframe(parent, Frame.of(id, local, parent.yAxis), region))

  /** A window of the given size centred on the parent frame. */
  def centred[U <: Unit2D](
      parent: Frame[U],
      id: FrameId,
      size: Extent[U]
  ): Either[GeometryError, Subframe[U]] =
    val x0 = parent.bounds.xMin + (parent.width - size.width) / 2.0
    val y0 = parent.bounds.yMin + (parent.height - size.height) / 2.0
    Bounds.of[U](x0, y0, x0 + size.width, y0 + size.height).flatMap(of(parent, id, _))

  /** The window covering the whole parent, under its own identity. */
  def whole[U <: Unit2D](parent: Frame[U], id: FrameId): Either[GeometryError, Subframe[U]] =
    of(parent, id, parent.bounds)

end Subframe

/** A declared, uniform linear relation between a frame's unit and degrees of
  * visual angle: `unitsPerDegree` frame units span one degree everywhere.
  *
  * This is the linear approximation, stated as such: it is a recorded
  * parameter, not a calibration, and it is exact only near the centre of a
  * flat surface (see [[Warp.Tangent]] for the exact projection). Degrees are
  * measured from the frame's centre with `y` upward.
  */
final class LinearAngularScale[U <: Unit2D] private (
    val frame: Frame[U],
    val unitsPerDegree: Double
):
  /** A bandwidth in degrees, expressed in this frame's units. */
  def sigma(degrees: Sigma[Unit2D.Deg]): Either[GeometryError, Sigma[U]] =
    Sigma.of[U](degrees.value * unitsPerDegree)

  /** The angular frame this scale induces, centred on the frame's centre. */
  def angularFrame(id: FrameId): Either[GeometryError, Frame[Unit2D.Deg]] =
    Bounds
      .centred[Unit2D.Deg](
        frame.width / unitsPerDegree / 2.0,
        frame.height / unitsPerDegree / 2.0
      )
      .map(Frame.of(id, _, YAxis.Up))

  /** The linear warp into degrees from the frame's centre, `y` upward. */
  def angular(id: FrameId): Either[GeometryError, Warp[U, Unit2D.Deg]] =
    angularFrame(id).flatMap(Warp.rescale(frame, _))

  /** The same scale on a window of this scale's frame. */
  def on(window: Subframe[U]): Either[GeometryError, LinearAngularScale[U]] =
    Agreement
      .frames(frame, window.parent)
      .map(_ => new LinearAngularScale(window.frame, unitsPerDegree))

  override def equals(that: Any): Boolean = that match
    case s: LinearAngularScale[?] =>
      frame == s.frame && java.lang.Double.compare(unitsPerDegree, s.unitsPerDegree) == 0
    case _ => false
  override def hashCode: Int = (frame, unitsPerDegree).hashCode

object LinearAngularScale:
  def of[U <: Unit2D](
      frame: Frame[U],
      unitsPerDegree: Double
  ): Either[GeometryError, LinearAngularScale[U]] =
    if !unitsPerDegree.isFinite || unitsPerDegree <= 0.0 then
      Left(GeometryError.NonPositiveAngularScale(frame.id, unitsPerDegree))
    else Right(new LinearAngularScale(frame, unitsPerDegree))

/** A recorded correction of a source's coordinates, applied at admission
  * before containment is checked. Each is an affine endomorphism of the frame
  * the coordinates were declared in; the raw values themselves are never
  * rewritten.
  *
  * `FlipX` and `FlipY` reflect about the frame's centre line, so each is its
  * own inverse; `Translate` adds a finite displacement.
  */
sealed trait Correction derives CanEqual:
  def render: String

  /** The correction as a warp from the declared source frame to the frame of
    * admission. The source frame keeps its own identity; the warp is the only
    * route between the two.
    */
  def warp[U <: Unit2D](source: Frame[U], admission: Frame[U]): Warp[U, U] =
    val b = source.bounds
    val m = this match
      case Correction.FlipX           => Mat3.affine(-1, 0, b.xMin + b.xMax, 0, 1, 0)
      case Correction.FlipY           => Mat3.affine(1, 0, 0, 0, -1, b.yMin + b.yMax)
      case Correction.Translate(x, y) => Mat3.translation(x, y)
    Warp.Affine(source, admission, m)

object Correction:
  case object FlipX extends Correction:
    def render: String = "flipX"
  case object FlipY extends Correction:
    def render: String = "flipY"
  final case class Translate private[kernel] (dx: Double, dy: Double) extends Correction:
    def render: String = s"translate($dx, $dy)"

  def translate(dx: Double, dy: Double): Either[GeometryError, Correction] =
    if dx.isFinite && dy.isFinite then Right(new Translate(dx, dy))
    else Left(GeometryError.NonFiniteTranslation(dx, dy))

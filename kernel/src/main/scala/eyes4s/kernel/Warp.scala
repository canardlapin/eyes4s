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

/** A coordinate transform between two frames, retained as an inspectable value.
  *
  * ==Structure is retained==
  *
  * A `Warp` is a sealed ADT, not an opaque `Pt => Pt`. Composition builds a
  * tree that can be printed, inverted structurally, and later rewritten or
  * serialised. This follows `linop4s`, where operator composition is an
  * expression tree for the same reasons: a closure can be applied but not
  * examined, and an analysis pipeline that cannot show its own transform chain
  * cannot explain a result.
  *
  * ==Composition is a partial category==
  *
  * `andThen` returns `Either`, because two warps compose only when the frames
  * meet: `f.to.id` must equal `g.from.id`. Associativity and identity therefore
  * hold on the subcategory where frame identities align, and nowhere else. A
  * total `Category` instance would have to pretend that a warp *into* one
  * stimulus composes with a warp *out of* a different one. That is exactly the
  * error this type exists to prevent, so the laws are stated on the subcategory
  * and `eyes4s-laws` tests them there.
  *
  * ==Construction is closed==
  *
  * Every case has a private constructor. `Then` in particular is reachable only
  * through [[Warp.andThen]], so an unchecked composition cannot be assembled by
  * hand. A public case class would make the frame check advisory.
  */
sealed trait Warp[A <: Unit2D, B <: Unit2D]:

  def from: Frame[A]
  def to: Frame[B]

  /** Map a position. `None` where the transform is undefined at that point,
    * which a homography can be.
    */
  def apply(p: Pt[A]): Option[Pt[B]]

  /** The reverse transform, where one exists. */
  def inverse: Option[Warp[B, A]]

  def render: String

  /** Compose with a warp that starts where this one ends. */
  def andThen[C <: Unit2D](that: Warp[B, C]): Either[GeometryError, Warp[A, C]] =
    if to.id == that.from.id then Right(Warp.Then(this, that))
    else Left(GeometryError.FrameMismatch(to.id, that.from.id))

end Warp

object Warp:

  /** The identity on a frame. */
  final case class Id[A <: Unit2D] private[kernel] (frame: Frame[A]) extends Warp[A, A]:
    def from: Frame[A]                 = frame
    def to: Frame[A]                   = frame
    def apply(p: Pt[A]): Option[Pt[A]] = Some(p)
    def inverse: Option[Warp[A, A]]    = Some(this)
    def render: String                 = s"id[${frame.id}]"

  /** A linear transform in homogeneous coordinates, restricted to affine. */
  final case class Affine[A <: Unit2D, B <: Unit2D] private[kernel] (
      from: Frame[A],
      to: Frame[B],
      m: Mat3
  ) extends Warp[A, B]:
    def apply(p: Pt[A]): Option[Pt[B]] = m.applyTo[A, B](p)
    def inverse: Option[Warp[B, A]]    = m.inverse.map(Affine(to, from, _))
    def render: String                 = s"affine[${from.id}->${to.id}]"

  /** A full projective transform, for surfaces seen at an angle. */
  final case class Homography[A <: Unit2D, B <: Unit2D] private[kernel] (
      from: Frame[A],
      to: Frame[B],
      h: Mat3
  ) extends Warp[A, B]:
    def apply(p: Pt[A]): Option[Pt[B]] = h.applyTo[A, B](p)
    def inverse: Option[Warp[B, A]]    = h.inverse.map(Homography(to, from, _))
    def render: String                 = s"homography[${from.id}->${to.id}]"

  /** Which way a [[Tangent]] runs. */
  enum Sense derives CanEqual:
    case Forward // linear -> angular
    case Inverse // angular -> linear

  /** The projection between a linear unit on a flat surface and the angle that
    * surface subtends from a viewpoint.
    *
    * ==This is nonlinear, and that matters==
    *
    * The relation is `theta = atan(offset / distance)`, not `theta = offset * k`.
    * The linear approximation that eye-tracking code routinely substitutes is
    * accurate near the centre and progressively wrong toward the edges -- on a
    * typical bench setup it overestimates eccentricity by several percent at the
    * screen corners. Implementing the tangent exactly costs nothing and the
    * error never enters.
    *
    * ==Axes are reconciled==
    *
    * A screen frame runs `y` downward and an angular frame runs `y` upward. The
    * flip is applied here, from the two frames' declared [[YAxis]], rather than
    * being left to a caller to remember.
    *
    * ==Per-axis tangent==
    *
    * Horizontal and vertical angles are computed independently, which is the
    * standard convention in this literature. A fully spherical treatment differs
    * slightly off-axis; the difference is below other sources of error on a flat
    * display and the convention is documented here rather than assumed.
    *
    * `Sense` makes the pairing genuinely invertible: the reverse direction is
    * the same value with the sense flipped and the frames swapped, so
    * `inverse` never has to return `None` for want of a case to return.
    */
  final case class Tangent[A <: Unit2D, B <: Unit2D] private[kernel] (
      from: Frame[A],
      to: Frame[B],
      perspective: Perspective,
      sense: Sense
  ) extends Warp[A, B]:

    private def flipY: Boolean = from.yAxis != to.yAxis

    def apply(p: Pt[A]): Option[Pt[B]] =
      val d = perspective.distance.toMm
      sense match
        case Sense.Forward =>
          // linear -> angular
          val mmPerX = perspective.surfaceWidth.toMm / from.width
          val mmPerY = perspective.surfaceHeight.toMm / from.height
          val dxMm   = (p.x - from.centre.x) * mmPerX
          val dyMm0  = (p.y - from.centre.y) * mmPerY
          val dyMm   = if flipY then -dyMm0 else dyMm0
          val tx     = math.atan(dxMm / d) * 180.0 / math.Pi
          val ty     = math.atan(dyMm / d) * 180.0 / math.Pi
          val r      = Pt[B](to.centre.x + tx, to.centre.y + ty)
          if r.isFinite then Some(r) else None

        case Sense.Inverse =>
          // angular -> linear
          val mmPerX = perspective.surfaceWidth.toMm / to.width
          val mmPerY = perspective.surfaceHeight.toMm / to.height
          val tx     = (p.x - from.centre.x) * math.Pi / 180.0
          val ty     = (p.y - from.centre.y) * math.Pi / 180.0
          val dxMm   = d * math.tan(tx)
          val dyMm0  = d * math.tan(ty)
          val dyMm   = if flipY then -dyMm0 else dyMm0
          val r      = Pt[B](to.centre.x + dxMm / mmPerX, to.centre.y + dyMm / mmPerY)
          if r.isFinite then Some(r) else None

    def inverse: Option[Warp[B, A]] =
      val flipped = sense match
        case Sense.Forward => Sense.Inverse
        case Sense.Inverse => Sense.Forward
      Some(Tangent(to, from, perspective, flipped))

    def render: String =
      s"tangent[${from.id}->${to.id}, ${perspective.render}]"

  end Tangent

  /** Composition. Private: reachable only through [[Warp.andThen]], which
    * checks that the frames meet.
    */
  final case class Then[A <: Unit2D, B <: Unit2D, C <: Unit2D] private[kernel] (
      f: Warp[A, B],
      g: Warp[B, C]
  ) extends Warp[A, C]:
    def from: Frame[A] = f.from
    def to: Frame[C]   = g.to

    def apply(p: Pt[A]): Option[Pt[C]] = f(p).flatMap(g.apply)

    /** `(g . f)^-1 = f^-1 . g^-1`, derived structurally rather than asserted. */
    def inverse: Option[Warp[C, A]] =
      for
        gi <- g.inverse
        fi <- f.inverse
      yield Then(gi, fi)

    def render: String = s"(${f.render} then ${g.render})"

  end Then

  // -------------------------------------------------------------------------
  // Smart constructors -- the only public way in
  // -------------------------------------------------------------------------

  def id[A <: Unit2D](frame: Frame[A]): Warp[A, A] = Id(frame)

  def affine[A <: Unit2D, B <: Unit2D](
      from: Frame[A],
      to: Frame[B],
      m: Mat3
  ): Either[GeometryError, Warp[A, B]] =
    if !m.isAffine then Left(GeometryError.NotAffine(m.render))
    else Right(Affine(from, to, m))

  def homography[A <: Unit2D, B <: Unit2D](
      from: Frame[A],
      to: Frame[B],
      h: Mat3
  ): Warp[A, B] = Homography(from, to, h)

  /** The projection from a linear frame to an angular one.
    *
    * Note what is required to call this: a [[Perspective]], meaning a real
    * distance and a real physical size. There is no default and no way to
    * obtain degrees without supplying one -- which is the entire point, since
    * a threshold in degrees applied to data that never met a viewing geometry
    * is a number with no meaning.
    */
  def tangent[A <: Unit2D, B <: Unit2D](
      from: Frame[A],
      to: Frame[B],
      perspective: Perspective
  ): Warp[A, B] = Tangent(from, to, perspective, Sense.Forward)

  /** Map one rectangle onto another, preserving relative position.
    *
    * The workhorse for placing a stimulus on a display, or normalising a
    * display to the unit square.
    */
  def rescale[A <: Unit2D, B <: Unit2D](
      from: Frame[A],
      to: Frame[B]
  ): Either[GeometryError, Warp[A, B]] =
    val sx = to.bounds.width / from.bounds.width
    val sy = to.bounds.height / from.bounds.height
    val fy = if from.yAxis == to.yAxis then 1.0 else -1.0
    // Map from-centre to to-centre, scaling about the centres.
    val tx = to.centre.x - sx * from.centre.x
    val ty = to.centre.y - fy * sy * from.centre.y
    affine(from, to, Mat3.affine(sx, 0, tx, 0, fy * sy, ty))

end Warp

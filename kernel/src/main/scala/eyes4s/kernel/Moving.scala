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

/** How a [[Moving]] transform behaves between its samples. */
enum Interp derives CanEqual:

  /** Piecewise constant: each segment's transform holds for its whole extent.
    *
    * Exact, not an approximation, whenever the transform genuinely is constant
    * across the segment -- which is the case for a stimulus rendered frame by
    * frame, where the surface really does occupy one position for the duration
    * of each frame.
    */
  case Hold

  /** Blend toward the next segment's transform across the current extent.
    *
    * For sampled transforms -- a surface tracked by a camera at 30 Hz while
    * gaze arrives at 1000 Hz -- holding produces visible stair-stepping and
    * interpolation is closer to the truth.
    *
    * The blend is elementwise on the underlying matrices. This is the standard
    * choice and it is *not* geometrically principled: a rotation interpolated
    * elementwise does not trace the shorter arc, and an interpolated matrix can
    * pick up a small shear. For the inter-sample motion this is used on the
    * error is far below tracking noise, but the limitation is stated rather
    * than discovered. A principled version would decompose and interpolate the
    * rotation separately.
    */
  case Lerp

/** A transform between two fixed frames whose value changes over time.
  *
  * ==What this buys==
  *
  * Dynamic stimuli and head-worn recordings need gaze mapped onto a surface
  * that moves. Because [[Warp]] is a value rather than a closure, "a transform
  * that varies" needs no new machinery: it is a piecewise function from an
  * [[Instant]] to a `Warp`. Dynamic areas of interest, video stimuli, scrolling
  * pages and marker-tracked surfaces all reduce to this one type.
  *
  * ==Invariants, proven at construction==
  *
  *   - at least one segment;
  *   - every segment on the same timeline;
  *   - every segment mapping between the same two frames;
  *   - segments ordered and non-overlapping.
  *
  * The last is what makes [[at]] a function rather than a relation. Overlapping
  * segments would make the answer depend on search order, which is the kind of
  * defect that survives for years because it only shows up as a few misassigned
  * samples.
  */
final case class Moving[A <: Unit2D, B <: Unit2D] private (
    from: Frame[A],
    to: Frame[B],
    clock: ClockId,
    segments: Vector[Moving.Segment[A, B]],
    interp: Interp
):

  /** The whole extent this transform is defined over. */
  def extent: Interval =
    Interval
      .of(clock, segments.head.interval.onset, segments.last.interval.offset)
      .toOption
      .get

  /** The transform in force at `t`, or `None` outside the defined extent.
    *
    * `None` rather than a clamped edge value: a sample recorded before the
    * stimulus appeared has no meaningful position on it, and silently mapping
    * it onto the first frame would invent data.
    */
  def at(t: Instant): Option[Warp[A, B]] =
    indexAt(t).flatMap { i =>
      val seg = segments(i)
      interp match
        case Interp.Hold => Some(seg.warp)
        case Interp.Lerp =>
          if i == segments.length - 1 then Some(seg.warp)
          else
            val next = segments(i + 1)
            val span = seg.interval.duration.toMicros.toDouble
            if span <= 0.0 then Some(seg.warp)
            else
              val u = (seg.interval.onset.until(t).toMicros.toDouble / span).max(0.0).min(1.0)
              Moving.blend(from, to, seg.warp, next.warp, u).orElse(Some(seg.warp))
    }

  /** Map a position at a given instant, in one step. */
  def apply(t: Instant, p: Pt[A]): Option[Pt[B]] =
    at(t).flatMap(_.apply(p))

  private def indexAt(t: Instant): Option[Int] =
    // Segments are ordered and disjoint, so a binary search is well defined.
    var lo    = 0
    var hi    = segments.length - 1
    var found = -1
    while lo <= hi do
      val mid = (lo + hi) / 2
      val s   = segments(mid).interval
      if s.contains(t) then
        found = mid
        lo = hi + 1
      else if t.toMicros < s.onset.toMicros then hi = mid - 1
      else lo = mid + 1
    if found >= 0 then Some(found) else None

  def render: String =
    s"moving[${from.id}->${to.id}, ${segments.length} segments, $interp]"

end Moving

object Moving:

  final case class Segment[A <: Unit2D, B <: Unit2D](interval: Interval, warp: Warp[A, B])

  def of[A <: Unit2D, B <: Unit2D](
      segments: Vector[Segment[A, B]],
      interp: Interp
  ): Either[MovingError, Moving[A, B]] =
    if segments.isEmpty then Left(MovingError.NoSegments)
    else
      val ordered = segments.sortBy(_.interval.onset.toMicros)

      val overlap = ordered.sliding(2).collectFirst {
        case Vector(a, b) if b.interval.onset.toMicros < a.interval.offset.toMicros =>
          (a.interval, b.interval)
      }

      for
        _ <- Agreement
          .allClocks(ordered.map(_.interval.clock))
          .left
          .map(MovingError.Clock.apply)
        _ <- Agreement
          .allFrames(ordered.map(_.warp.from))
          .left
          .map(MovingError.Frame.apply)
        _ <- Agreement
          .allFrames(ordered.map(_.warp.to))
          .left
          .map(MovingError.Frame.apply)
        _ <- overlap.fold(Right(())) { case (a, b) =>
          Left(MovingError.OverlappingSegments(a.render, b.render))
        }
      yield Moving(
        ordered.head.warp.from,
        ordered.head.warp.to,
        ordered.head.interval.clock,
        ordered,
        interp
      )

  /** Elementwise blend of two transforms, where both expose a matrix. */
  private[kernel] def blend[A <: Unit2D, B <: Unit2D](
      from: Frame[A],
      to: Frame[B],
      x: Warp[A, B],
      y: Warp[A, B],
      u: Double
  ): Option[Warp[A, B]] =
    for
      mx <- matrixOf(x)
      my <- matrixOf(y)
    yield
      def mix(a: Double, b: Double): Double = a + (b - a) * u
      val m                                 = Mat3(
        mix(mx.m00, my.m00),
        mix(mx.m01, my.m01),
        mix(mx.m02, my.m02),
        mix(mx.m10, my.m10),
        mix(mx.m11, my.m11),
        mix(mx.m12, my.m12),
        mix(mx.m20, my.m20),
        mix(mx.m21, my.m21),
        mix(mx.m22, my.m22)
      )
      Warp.affine(from, to, m).getOrElse(Warp.homography(from, to, m))

  private[kernel] def matrixOf[A <: Unit2D, B <: Unit2D](w: Warp[A, B]): Option[Mat3] =
    w match
      case a: Warp.Affine[A, B]     => Some(a.m)
      case h: Warp.Homography[A, B] => Some(h.h)
      case _                        => None

end Moving

/** Failures constructing a [[Moving]] transform. */
enum MovingError derives CanEqual:
  case NoSegments
  case OverlappingSegments(first: String, second: String)
  case Clock(underlying: TimeError)
  case Frame(underlying: GeometryError)

  def message: String = this match
    case NoSegments =>
      "A Moving transform needs at least one segment."
    case OverlappingSegments(a, b) =>
      s"Segments overlap: $a and $b. Overlapping extents would make the " +
        "transform in force at an instant depend on search order."
    case Clock(e) => s"Segments are not on one timeline. ${e.message}"
    case Frame(e) => s"Segments do not share their frames. ${e.message}"

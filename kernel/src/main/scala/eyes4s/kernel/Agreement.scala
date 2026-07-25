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

/** The single place two coordinate systems are checked for agreement.
  *
  * ==Why this exists as a named thing==
  *
  * The check itself is one comparison, which is exactly why it wants a home.
  * The reference R implementation inlines its window predicate at four call
  * sites with three different validation regimes, and copies its join preamble
  * verbatim into four functions -- not because any one of them is hard, but
  * because a one-line check is easier to retype than to look up. The copies
  * then drift.
  *
  * This library had the same seam forming after a single file: [[Interval]]
  * carried a private `sameClock` while `Overlap.selects` re-inlined the same
  * comparison with the operands in the opposite order. Both were correct; they
  * would not have stayed that way.
  *
  * ==The rule this enforces==
  *
  * Any operation combining two values that carry a coordinate system -- a
  * [[ClockId]] in time, a [[FrameId]] in space -- returns `Either`. The unit
  * parameter proves two positions are both in degrees; only the frame proves
  * they are in the *same* degrees, and only the clock proves two timestamps
  * are on the same timeline.
  */
object Agreement:

  /** Require two timelines to be the same, yielding it on success. */
  def clocks(left: ClockId, right: ClockId): Either[TimeError, ClockId] =
    if left == right then Right(left) else Left(TimeError.ClockMismatch(left, right))

  /** Require two frames to be the same, yielding it on success.
    *
    * Compares identity, not geometry. Two frames with identical bounds are
    * different frames when they describe different things.
    */
  def frames[U <: Unit2D](left: Frame[U], right: Frame[U]): Either[GeometryError, Frame[U]] =
    if left.id == right.id then Right(left)
    else Left(GeometryError.FrameMismatch(left.id, right.id))

  /** Require a whole collection to share one frame.
    *
    * The n-ary form matters more than it looks: averaging a set of maps or
    * pooling a set of paths is where a stray frame slips in, and checking
    * pairwise as you fold reports the mismatch against whichever element
    * happened to come first.
    */
  def allFrames[U <: Unit2D](
      frames: Seq[Frame[U]]
  ): Either[GeometryError, Option[Frame[U]]] =
    frames.headOption match
      case None       => Right(None)
      case Some(head) =>
        frames.tail
          .find(_.id != head.id)
          .fold(Right(Some(head))) { bad =>
            Left(GeometryError.FrameMismatch(head.id, bad.id))
          }

  /** Require a whole collection to share one timeline. */
  def allClocks(clocks: Seq[ClockId]): Either[TimeError, Option[ClockId]] =
    clocks.headOption match
      case None       => Right(None)
      case Some(head) =>
        clocks.tail
          .find(_ != head)
          .fold(Right(Some(head)))(bad => Left(TimeError.ClockMismatch(head, bad)))

end Agreement

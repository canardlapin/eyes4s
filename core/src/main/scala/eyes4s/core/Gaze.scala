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

package eyes4s.core

import eyes4s.kernel.*

/** Which eye a recording describes. */
enum Eye derives CanEqual:
  case Left, Right, Cyclopean

/** What a tracker's pupil column actually means.
  *
  * Devices report pupil size in incompatible ways, and the difference is not a
  * scale factor: area varies as the square of diameter, so averaging one and
  * comparing it to the other is wrong in a way that looks plausible. The unit
  * is recorded once on the [[Recording]] rather than repeated on every sample.
  */
enum PupilUnit derives CanEqual:
  case Area, Diameter

  /** Device units with no documented physical interpretation, which is the
    * common case for video-based trackers.
    */
  case Arbitrary

/** The state of the eye at one instant.
  *
  * ==No missing-value sentinel==
  *
  * This is the whole reason the type exists. A tracker that loses the eye emits
  * something -- zeros, a large negative number, a blank field -- and every
  * pipeline that represents gaze as a bare pair of numbers must remember to
  * filter it. `eyesim` inherits `NA` and propagates it silently through means,
  * so a blink can shift a fixation centroid without anything reporting that it
  * did.
  *
  * Here the cases are distinct and exhaustive, so a caller cannot forget one:
  * the compiler will not let a match go unhandled, and there is no numeric
  * value that secretly means "absent".
  *
  * The distinction between [[Blink]] and [[Lost]] is worth keeping. A blink is
  * a physiological event with a duration that belongs in the record; signal
  * loss is a measurement failure. Data-quality reporting needs to tell them
  * apart, and only the tracker knows which it saw.
  */
enum Gaze[U <: Unit2D] derives CanEqual:

  /** The eye was tracked at this position. */
  case Tracked(at: Pt[U], pupilSize: Option[Double]) extends Gaze[U]

  /** The eye was closed. A physiological event, not a failure. */
  case Blink() extends Gaze[U]

  /** The tracker had no signal. A measurement failure, not an event. */
  case Lost() extends Gaze[U]

  /** Tracked, but outside the frame. Kept rather than dropped: gaze leaving
    * the display is data, and silently discarding it biases dwell measures
    * toward whatever remains.
    */
  case OffScreen(at: Pt[U]) extends Gaze[U]

  /** Where the eye was, when that is known. */
  def position: Option[Pt[U]] = this match
    case Tracked(p, _) => Some(p)
    case OffScreen(p)  => Some(p)
    case Blink()       => None
    case Lost()        => None

  def pupil: Option[Double] = this match
    case Tracked(_, s) => s
    case _             => None

  /** Tracked and inside the frame: usable for spatial analysis without further
    * qualification.
    */
  def isUsable: Boolean = this match
    case Tracked(_, _) => true
    case _             => false

  def isMissing: Boolean = this match
    case Blink() | Lost() => true
    case _                => false

  /** Move to another coordinate frame, preserving the classification. */
  def warp[V <: Unit2D](w: Warp[U, V]): Gaze[V] = this match
    case Tracked(p, s) =>
      w(p) match
        case Some(q) => if w.to.contains(q) then Gaze.Tracked(q, s) else Gaze.OffScreen(q)
        case None    => Gaze.Lost()
    case OffScreen(p) =>
      w(p) match
        case Some(q) => if w.to.contains(q) then Gaze.Tracked(q, None) else Gaze.OffScreen(q)
        case None    => Gaze.Lost()
    case Blink() => Gaze.Blink()
    case Lost()  => Gaze.Lost()

end Gaze

/** One inspectable step in the derivation of a sample value. */
enum SampleOrigin derives CanEqual:
  case Measured
  case Interpolated
  case Smoothed
  case Projected

/** Non-empty, ordered derivation history for one sample value.
  *
  * Interpolation creates a value whose basis is `Interpolated`. Smoothing and
  * projection append steps, so an interpolated sample that is later smoothed
  * and projected remains distinguishable from a measured projected sample.
  */
final class SampleLineage private (private val steps: Vector[SampleOrigin]) derives CanEqual:
  def latest: SampleOrigin                     = steps.last
  def contains(step: SampleOrigin): Boolean    = steps.contains(step)
  def toVector: Vector[SampleOrigin]           = steps
  def render: String                           = steps.mkString(">")
  private[eyes4s] def smoothed: SampleLineage  = append(SampleOrigin.Smoothed)
  private[eyes4s] def projected: SampleLineage = append(SampleOrigin.Projected)

  private def append(step: SampleOrigin): SampleLineage =
    new SampleLineage(steps :+ step)

  override def equals(other: Any): Boolean = other match
    case that: SampleLineage => steps == that.steps
    case _                   => false

  override def hashCode: Int = steps.hashCode

object SampleLineage:
  val measured: SampleLineage     = new SampleLineage(Vector(SampleOrigin.Measured))
  val interpolated: SampleLineage = new SampleLineage(Vector(SampleOrigin.Interpolated))

  /** Compatibility constructor for callers that previously supplied only the
    * latest origin. Derived measured values receive their implied measured
    * basis; interpolation remains a distinct basis.
    */
  def fromLatest(origin: SampleOrigin): SampleLineage = origin match
    case SampleOrigin.Measured     => measured
    case SampleOrigin.Interpolated => interpolated
    case SampleOrigin.Smoothed     => measured.smoothed
    case SampleOrigin.Projected    => measured.projected

/** One instant of a recording. */
final case class Sample[U <: Unit2D](
    t: Instant,
    gaze: Gaze[U],
    lineage: SampleLineage = SampleLineage.measured
) derives CanEqual:
  def position: Option[Pt[U]] = gaze.position
  def isUsable: Boolean       = gaze.isUsable
  def origin: SampleOrigin    = lineage.latest

  private[eyes4s] def withSmoothedGaze(value: Gaze[U]): Sample[U] =
    copy(gaze = value, lineage = lineage.smoothed)

  private[eyes4s] def withProjectedGaze[V <: Unit2D](value: Gaze[V]): Sample[V] =
    val projected =
      if gaze.position.isDefined && value.position.isDefined then lineage.projected
      else lineage
    Sample(t, value, projected)

object Sample:
  def apply[U <: Unit2D](t: Instant, gaze: Gaze[U], origin: SampleOrigin): Sample[U] =
    new Sample(t, gaze, SampleLineage.fromLatest(origin))

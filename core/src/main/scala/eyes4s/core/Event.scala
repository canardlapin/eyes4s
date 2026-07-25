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

/** Something the eye did over an extent of time.
  *
  * ==Events have extent; samples do not==
  *
  * This is why [[Overlap]] exists and why `Recording.within` takes no policy
  * while event selection does. A sample is an instant and either falls in a
  * window or does not. An event straddling the boundary genuinely admits three
  * answers, and which one an analysis takes changes its results.
  *
  * ==The interval is stored, not implied==
  *
  * `eyesim` represents a fixation as an onset plus a duration and has no offset
  * column anywhere in the package, so every window filter it performs tests the
  * onset alone: a fixation beginning one millisecond before a window closes is
  * counted in full, however long it ran on. Here the extent is an [[Interval]]
  * and the straddling policy is named.
  */
sealed trait Event[U <: Unit2D] derives CanEqual:
  def span: Interval
  def duration: Span = span.duration

  /** Where the event happened, for the events that have a single place. */
  def location: Option[Pt[U]] = this match
    case f: Event.Fixation[U] => Some(f.centre)
    case _                    => None

object Event:

  /** The eye held still. */
  final case class Fixation[U <: Unit2D](
      span: Interval,
      centre: Pt[U],
      dispersion: Double,
      sampleCount: Int
  ) extends Event[U]

  /** The eye moved from one place to another.
    *
    * `peakVelocity` is optional because there are two ways to come by a
    * saccade, and they know different things. A detector working from samples
    * measures the velocity profile. A saccade *inferred* from a pair of
    * consecutive fixations knows only its endpoints -- and reporting a peak
    * velocity that was never measured is exactly the sort of invented number
    * this library exists to prevent.
    */
  final case class Saccade[U <: Unit2D](
      span: Interval,
      from: Pt[U],
      to: Pt[U],
      peakVelocity: Option[Velocity[U]]
  ) extends Event[U]

  /** The eye was closed. */
  final case class Blink[U <: Unit2D](span: Interval) extends Event[U]

  /** The eye tracked a moving target. */
  final case class Pursuit[U <: Unit2D](span: Interval, path: IArray[Pt[U]]) extends Event[U]

  extension [U <: Unit2D](s: Event.Saccade[U])

    /** Straight-line displacement between the endpoints. */
    def displacement: Vec2[U] = s.from.vectorTo(s.to)

    /** Magnitude of that displacement. In an angular frame, the amplitude in
      * degrees; in a display frame, a number of pixels and not comparable
      * across setups.
      */
    def amplitude: Double = displacement.norm

    def direction: Angle = displacement.angle

    /** Average speed over the extent, which is not the peak.
      *
      * Named separately from [[Event.Saccade.peakVelocity]] because the two
      * differ by roughly a factor of two for a typical saccade, and a main
      * sequence fitted to the wrong one is simply a different relationship.
      */
    def meanVelocity: Option[Velocity[U]] = Velocity.over(displacement, s.duration)

  extension [U <: Unit2D](f: Event.Fixation[U])
    /** Whether the fixation's centre lies in a region. */
    def isIn(r: Region[U]): Boolean = r.contains(f.centre)

  /** Select events under an explicit straddling policy.
    *
    * The policy is required rather than defaulted. `eyesim`'s implicit choice
    * is onset-inside, applied at four separate call sites and named at none of
    * them; making it an argument means a reader of the call can see which
    * convention an analysis used.
    */
  def select[U <: Unit2D](
      events: Vector[Event[U]],
      window: Interval,
      policy: Overlap
  ): Either[TimeError, Vector[Event[U]]] =
    events.foldLeft[Either[TimeError, Vector[Event[U]]]](Right(Vector.empty)) { (acc, e) =>
      for
        kept <- acc
        hit  <- policy.selects(e.span, window)
      yield if hit then kept :+ e else kept
    }

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

/** An absolute half-open extent `[onset, offset)` on a named timeline.
  *
  * ==Two types, not one== (bead `q-interval-clock`)
  *
  * There are two things an analysis calls a "time window", and conflating them
  * is a bug class rather than a naming quibble:
  *
  *   - an extent that actually happened, anchored on a real timeline -- the
  *     duration of a detected event. That is an `Interval`, and it carries the
  *     [[ClockId]] it was measured on.
  *   - an extent an analyst wrote down, relative to something -- "0 to 3000 ms
  *     after onset". That has no clock, because it has not happened yet and is
  *     not attached to anything. That is a [[Window]].
  *
  * Tagging a single type with a clock would force relative windows to invent
  * one, and callers would supply whatever was to hand. Splitting the types
  * means the clock is present exactly where it is meaningful and absent where
  * it is not. The reference R implementation has only the absolute form, which
  * is why its every window filter is expressed as raw onsets and why a
  * stimulus-clock timestamp can silently enter a tracker-clock comparison.
  *
  * ==Offset is stored==
  *
  * Not onset-plus-duration. Duration is derived. Representing an extent by its
  * start and a length invites the two to drift apart under transformation, and
  * makes "does this straddle the boundary?" depend on which field you trust.
  */
final case class Interval private (clock: ClockId, onset: Instant, offset: Instant)
    derives CanEqual:

  def duration: Span = onset.until(offset)

  /** Half-open: `onset <= t < offset`. */
  def contains(t: Instant): Boolean =
    t.toMicros >= onset.toMicros && t.toMicros < offset.toMicros

  def isEmpty: Boolean = onset.toMicros == offset.toMicros

  def shift(by: Span): Interval =
    Interval(clock, onset + by, offset + by)

  /** True when the two extents share any instant. Requires a common timeline. */
  def overlaps(that: Interval): Either[TimeError, Boolean] =
    Agreement.clocks(clock, that.clock).map { _ =>
      onset.toMicros < that.offset.toMicros && that.onset.toMicros < offset.toMicros
    }

  /** True when `that` lies wholly inside this extent. Requires a common timeline. */
  def encloses(that: Interval): Either[TimeError, Boolean] =
    Agreement.clocks(clock, that.clock).map { _ =>
      that.onset.toMicros >= onset.toMicros && that.offset.toMicros <= offset.toMicros
    }

  def render: String = s"[${onset.render}, ${offset.render}) on $clock"

end Interval

object Interval:

  /** The only public route in. Rejects a reversed extent rather than silently
    * normalising it, because a reversed interval is nearly always a sign that
    * two timestamps were subtracted in the wrong order upstream.
    */
  def of(clock: ClockId, onset: Instant, offset: Instant): Either[TimeError, Interval] =
    if offset.toMicros >= onset.toMicros then Right(Interval(clock, onset, offset))
    else Left(TimeError.ReversedInterval(clock, onset.toMicros, offset.toMicros))

  /** Convenience for the common case of an extent given by its start and length. */
  def lasting(clock: ClockId, onset: Instant, d: Span): Either[TimeError, Interval] =
    of(clock, onset, onset + d)

end Interval

/** A relative extent: two offsets from an anchor yet to be supplied.
  *
  * Carries no [[ClockId]] because it is not on a timeline. `Window(0ms, 3000ms)`
  * becomes an [[Interval]] only when [[Window.at]] is given an anchor instant
  * and the clock that anchor was measured on.
  *
  * The anchor itself is deliberately not modelled here. What counts as an
  * anchor -- the start of a trial, the first event of a recording, a stimulus
  * change -- is domain vocabulary, and this module is the trajectory-and-measure
  * kernel. Callers in `eyes4s-core` supply the instant.
  */
final case class Window private (from: Span, until: Span) derives CanEqual:

  def width: Span = until - from

  /** Resolve to an absolute extent against an anchor on a named timeline. */
  def at(clock: ClockId, anchor: Instant): Either[TimeError, Interval] =
    Interval.of(clock, anchor + from, anchor + until)

  def shift(by: Span): Window = Window(from + by, until + by)

  def render: String = s"[${from.render}, ${until.render}) relative"

end Window

object Window:

  def of(from: Span, until: Span): Either[TimeError, Window] =
    if until.toMicros >= from.toMicros then Right(Window(from, until))
    else Left(TimeError.ReversedWindow(from.toMicros, until.toMicros))

  /** A window starting at the anchor and running for `d`. */
  def lasting(d: Span): Either[TimeError, Window] = of(Span.zero, d)

end Window

/** How a selection treats an extent that straddles the boundary of a window.
  *
  * The reference R implementation inlines `onset >= lo & onset < hi` at four
  * separate call sites with three different validation regimes and no name for
  * the convention. Naming it is the entire point: `OnsetInside` is a choice,
  * and a reader can see which choice was made.
  */
enum Overlap derives CanEqual:

  /** Select when the extent's onset falls in the window. The R convention. */
  case OnsetInside

  /** Select only when the extent lies wholly within the window. */
  case FullyContained

  /** Select when the extent shares any instant with the window. */
  case AnyIntersection

object Overlap:

  extension (policy: Overlap)
    /** Does `event` belong to `window` under this policy? */
    def selects(event: Interval, window: Interval): Either[TimeError, Boolean] =
      policy match
        case Overlap.OnsetInside =>
          Agreement.clocks(window.clock, event.clock).map(_ => window.contains(event.onset))
        case Overlap.FullyContained  => window.encloses(event)
        case Overlap.AnyIntersection => window.overlaps(event)

end Overlap

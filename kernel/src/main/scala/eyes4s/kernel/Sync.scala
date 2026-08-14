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

/** An affine map between two timelines: `to = from * (1 + drift) + offset`.
  *
  * Clocks in a recording session are not the same clock. A tracker's internal
  * counter, the presentation machine's clock and the system clock start at
  * different epochs and tick at slightly different rates; over an hour a drift
  * of a few parts per million is tens of milliseconds, which is several
  * fixations' worth of error.
  *
  * `Sync` is the morphism between timelines, and it is the only way to move an
  * [[Interval]] from one to another. This mirrors [[Warp]] for space: you may
  * not silently compare quantities from different frames, and if you want to,
  * you must produce the conversion and say what it is.
  *
  * @param drift fractional rate difference, dimensionless. Zero for a pure
  *              offset. A value of 1e-6 means the target clock runs one
  *              microsecond fast per second.
  */
final case class Sync private (from: ClockId, to: ClockId, offset: Span, drift: Double)
    derives CanEqual:

  /** Convert an instant. Unchecked: the caller asserts it is on `from`.
    *
    * Prefer [[apply]], which checks. This exists for inner loops that have
    * already established the clock once for a whole recording.
    */
  def unsafeInstant(t: Instant): Instant =
    Instant.micros(math.round(t.toMicros * (1.0 + drift)) + offset.toMicros)

  /** Convert an interval, checking that it is on the source timeline. */
  def apply(i: Interval): Either[TimeError, Interval] =
    Agreement
      .clocks(from, i.clock)
      .left
      .map(_ => TimeError.WrongSourceClock(from, i.clock))
      .flatMap(_ => Interval.of(to, unsafeInstant(i.onset), unsafeInstant(i.offset)))

  /** The inverse map, exact up to rounding at microsecond resolution. */
  def inverse: Sync =
    val k = 1.0 / (1.0 + drift)
    Sync(to, from, Span.micros(-math.round(offset.toMicros * k)), k - 1.0)

  def render: String =
    s"$from -> $to (offset ${offset.render}, drift ${drift * 1e6} ppm)"

end Sync

object Sync:

  /** An affine synchronization with a finite, direction-preserving clock
    * scale.
    */
  def affine(
      from: ClockId,
      to: ClockId,
      offset: Span,
      drift: Double
  ): Either[TimeError, Sync] =
    if !drift.isFinite then Left(TimeError.NonFiniteDrift(from, to, drift))
    else if 1.0 + drift <= 0.0 then Left(TimeError.NonPositiveClockScale(from, to, drift))
    else Right(Sync(from, to, offset, drift))

  /** A pure offset with no rate difference. */
  def offsetOnly(from: ClockId, to: ClockId, offset: Span): Sync =
    Sync(from, to, offset, 0.0)

  /** The identity on a single timeline. */
  def identity(clock: ClockId): Sync =
    Sync(clock, clock, Span.zero, 0.0)

  /** Fit an offset-only sync from a pair of instants known to be simultaneous.
    *
    * The common practical case: a shared trigger or message appears on both
    * timelines, and its two timestamps pin the offset.
    */
  def fromCommonEvent(
      from: ClockId,
      to: ClockId,
      onFrom: Instant,
      onTo: Instant
  ): Sync =
    Sync(from, to, Span.micros(onTo.toMicros - onFrom.toMicros), 0.0)

end Sync

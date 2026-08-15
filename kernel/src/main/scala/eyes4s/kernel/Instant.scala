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

import cats.kernel.Order

/** A point on some timeline, in exact microseconds since that timeline's epoch.
  *
  * An `Instant` carries no clock identity of its own: two instants are only
  * comparable if something else established that they came from the same
  * timeline. That "something else" is [[Interval]], which does carry a
  * [[ClockId]], and [[Recording]] in `eyes4s-core`. Bare instants are a
  * primitive, not a public currency.
  *
  * Note the deliberate absence of an overloaded `-`. Subtracting a [[Span]]
  * yields an `Instant` and subtracting an `Instant` yields a `Span`, but both
  * arguments erase to `Long`, so the two overloads would collide. `until` names
  * the second operation instead, which also reads better at call sites:
  * `onset.until(offset)` rather than `offset - onset`.
  *
  * See [[Span]] for why this type lives in its own compilation unit.
  */
opaque type Instant = Long

object Instant:

  /** The timeline's own zero. Not an absolute date -- device epochs differ. */
  val epoch: Instant = 0L

  def micros(us: Long): Instant   = us
  def millis(ms: Long): Instant   = ms * 1000L
  def seconds(s: Double): Instant = math.round(s * 1e6)

  extension (t: Instant)
    def toMicros: Long    = t
    def toMillis: Double  = t / 1e3
    def toSeconds: Double = t / 1e6

    def +(s: Span): Instant = t + s.toMicros
    def -(s: Span): Instant = t - s.toMicros

    /** The span from `t` forward to `u`. Negative when `u` precedes `t`. */
    def until(u: Instant): Span = Span.micros(u.toMicros - t.toMicros)

    def render: String = Span.renderMilliseconds(t.toMicros)

  given Order[Instant] = Order.from((a, b) => java.lang.Long.compare(a, b))

end Instant

/** Sampling rate, in hertz. */
opaque type Hz = Double

object Hz:
  def apply(v: Double): Either[TimeError, Hz] =
    if v > 0.0 && v.isFinite then Right(v) else Left(TimeError.NonPositiveRate(v))

  extension (h: Hz)
    def value: Double = h

    /** The nominal interval between consecutive samples at this rate. */
    def period: Span = Span.seconds(1.0 / h)

  given Order[Hz] = Order.from((a, b) => java.lang.Double.compare(a, b))

end Hz

/** Nominal identity of a timeline.
  *
  * A session has an eye-tracker clock, a stimulus-presentation clock and a
  * system clock, and they drift. Identity is nominal rather than structural for
  * the same reason [[FrameId]] is: two clocks that happen to agree at one
  * instant are still different clocks.
  */
final case class ClockId(name: String) derives CanEqual:
  override def toString: String = name

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

/** Recoverable failures in the time layer.
  *
  * Every case names the operands, not just the reason, so that an application
  * can point at the object that failed rather than reporting "invalid input".
  * That requirement is PRD APP-14, and it applies from the first error type
  * onward rather than being retrofitted at the application layer.
  */
enum TimeError derives CanEqual:

  /** An interval whose offset precedes its onset. */
  case ReversedInterval(clock: ClockId, onsetMicros: Long, offsetMicros: Long)

  /** A window whose end precedes its start. */
  case ReversedWindow(fromMicros: Long, untilMicros: Long)

  /** Two intervals from different timelines were combined.
    *
    * This is the failure the `Interval` / `Window` split exists to surface. An
    * eye-tracker timestamp and a stimulus-presentation timestamp are both
    * `Long` microseconds and will compare without complaint; only the clock
    * identity distinguishes them.
    */
  case ClockMismatch(left: ClockId, right: ClockId)

  /** A `Sync` was applied to an interval on a timeline it does not describe. */
  case WrongSourceClock(expected: ClockId, actual: ClockId)

  case NonPositiveRate(value: Double)
  case NonFiniteDrift(from: ClockId, to: ClockId, drift: Double)
  case NonPositiveClockScale(from: ClockId, to: ClockId, drift: Double)

  def message: String = this match
    case ReversedInterval(c, on, off) =>
      s"Interval on clock '$c' ends before it starts: onset=${on}us, offset=${off}us. " +
        "An interval is half-open [onset, offset) and requires offset >= onset."
    case ReversedWindow(f, u) =>
      s"Window ends before it starts: from=${f}us, until=${u}us."
    case ClockMismatch(l, r) =>
      s"Cannot combine intervals on different timelines: '$l' and '$r'. " +
        "Convert one with a Sync before comparing them."
    case WrongSourceClock(exp, act) =>
      s"This Sync converts from clock '$exp', but was applied to an interval " +
        s"on clock '$act'."
    case NonPositiveRate(v) =>
      s"Sampling rate must be finite and positive, was $v Hz."
    case NonFiniteDrift(from, to, drift) =>
      s"Clock drift from '$from' to '$to' must be finite, was $drift."
    case NonPositiveClockScale(from, to, drift) =>
      s"Clock drift from '$from' to '$to' implies a non-positive scale " +
        s"(1 + $drift). A synchronization must preserve the direction of time."

end TimeError

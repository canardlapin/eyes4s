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

import cats.kernel.{CommutativeGroup, Order}

/** A signed extent of time, in exact microseconds.
  *
  * ==Why microseconds, and why `Long`==
  *
  * Sample timestamps arrive from devices as integers and are compared,
  * differenced and accumulated millions of times per session. A `Double` of
  * seconds accumulates representation error under exactly that usage, and the
  * error is silent. `Long` microseconds is exact to well beyond any plausible
  * recording length and totally ordered without qualification.
  *
  * ==Why this lives in its own file==
  *
  * `Span` and [[Instant]] are both opaque over `Long`, so inside a scope where
  * both are transparent they would be mutually assignable and the distinction
  * would be advisory rather than checked. Defining each in its own compilation
  * unit means neither is ever transparent where the other is in scope, so the
  * separation holds everywhere -- including inside this library. This is
  * stronger than the limitation PRD T-6 anticipated.
  */
opaque type Span = Long

object Span:

  val zero: Span = 0L

  def micros(us: Long): Span   = us
  def millis(ms: Long): Span   = ms * 1000L
  def seconds(s: Double): Span = math.round(s * 1e6)

  extension (a: Span)
    def toMicros: Long    = a
    def toMillis: Double  = a / 1e3
    def toSeconds: Double = a / 1e6

    def +(b: Span): Span = a + b
    def -(b: Span): Span = a - b
    def unary_- : Span   = -a

    /** Scale by a dimensionless factor. Rounds to the nearest microsecond. */
    def *(k: Double): Span = math.round(a.toMicros * k)

    def isNegative: Boolean = a < 0L
    def isZero: Boolean     = a == 0L

    def render: String = renderMilliseconds(a)

  /** Render exact microseconds as milliseconds without platform-dependent
    * `Double.toString` behavior. Scala.js omits the trailing `.0` that the JVM
    * retains for integral doubles, which must not change persistent reports.
    */
  private[kernel] def renderMilliseconds(micros: Long): String =
    val whole     = micros / 1000L
    val remainder = micros % 1000L
    if remainder == 0L then s"$whole.0ms"
    else
      val sign           = if micros < 0L then "-" else ""
      val magnitudeWhole =
        if whole < 0L then -whole
        else whole
      val magnitudeRemainder =
        if remainder < 0L then -remainder
        else remainder
      val padded =
        if magnitudeRemainder < 10L then s"00$magnitudeRemainder"
        else if magnitudeRemainder < 100L then s"0$magnitudeRemainder"
        else magnitudeRemainder.toString
      val fraction = padded.reverse.dropWhile(_ == '0').reverse
      s"$sign$magnitudeWhole.$fraction" + "ms"

  given Order[Span] = Order.from((a, b) => java.lang.Long.compare(a, b))

  /** Spans form a commutative group under addition: durations add, negate, and
    * have a zero. This is what makes a timeline arithmetic rather than a set of
    * labels, and it is law-tested in `eyes4s-laws`.
    */
  given CommutativeGroup[Span] with
    def empty: Span                     = Span.zero
    def combine(a: Span, b: Span): Span = a + b
    def inverse(a: Span): Span          = -a

end Span

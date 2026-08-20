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

/** A duration or accounting bucket whose non-negativity is construction
  * evidence rather than a caller convention.
  */
opaque type NonNegativeSpan = Span

object NonNegativeSpan:
  val zero: NonNegativeSpan = Span.zero

  def of(value: Span): Either[TimeQuantityError, NonNegativeSpan] =
    if value.isNegative then Left(TimeQuantityError.NegativeNonNegativeSpan(value))
    else Right(value)

  extension (value: NonNegativeSpan)
    def span: Span       = value
    def toMicros: Long   = Span.toMicros(value)
    def toMillis: Double = Span.toMillis(value)
    def render: String   = Span.renderMilliseconds(Span.toMicros(value))

/** A strictly positive duration suitable for bin widths and periods. */
opaque type PositiveSpan = Span

object PositiveSpan:
  def of(value: Span): Either[TimeQuantityError, PositiveSpan] =
    if value.isNegative || value.isZero then Left(TimeQuantityError.NonPositiveSpan(value))
    else Right(value)

  extension (value: PositiveSpan)
    def span: Span       = value
    def toMicros: Long   = Span.toMicros(value)
    def toMillis: Double = Span.toMillis(value)
    def render: String   = Span.renderMilliseconds(Span.toMicros(value))

/** A count whose non-negativity is construction evidence. Arithmetic remains
  * checked so `Long` overflow cannot silently invalidate the type's claim.
  */
opaque type NonNegativeLong = Long

object NonNegativeLong:
  val zero: NonNegativeLong = 0L

  def of(value: Long): Either[TimeQuantityError, NonNegativeLong] =
    if value < 0L then Left(TimeQuantityError.NegativeNonNegativeLong(value))
    else Right(value)

  extension (value: NonNegativeLong)
    def toLong: Long = value

    def plus(that: NonNegativeLong): Either[TimeQuantityError, NonNegativeLong] =
      if Long.MaxValue - value < that then
        Left(TimeQuantityError.NonNegativeLongOverflow(value, that))
      else Right(value + that)

    def increment: Either[TimeQuantityError, NonNegativeLong] =
      if value == Long.MaxValue then Left(TimeQuantityError.NonNegativeLongOverflow(value, 1L))
      else Right(value + 1L)

enum TimeQuantityError derives CanEqual:
  case NegativeNonNegativeSpan(value: Span)
  case NonPositiveSpan(value: Span)
  case NegativeNonNegativeLong(value: Long)
  case NonNegativeLongOverflow(left: Long, right: Long)

  def message: String = this match
    case NegativeNonNegativeSpan(value) =>
      s"NonNegativeSpan value=${value.render} is negative."
    case NonPositiveSpan(value) =>
      s"PositiveSpan value=${value.render} is not strictly positive."
    case NegativeNonNegativeLong(value) =>
      s"NonNegativeLong value=$value is negative."
    case NonNegativeLongOverflow(left, right) =>
      s"NonNegativeLong addition left=$left right=$right exceeds Long.MaxValue."

end TimeQuantityError

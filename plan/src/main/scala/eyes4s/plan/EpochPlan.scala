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

package eyes4s.plan

import cats.Eq
import eyes4s.kernel.*

/** Nth uses a checked zero-based occurrence index in stable timeline order. */
enum Occurrence derives CanEqual:
  case RequireUnique, First, Last
  case Nth(index: NonNegativeLong)

enum FinalBin derives CanEqual:
  case RequireExactDivision, IncludeShortFinal, ExcludeAndReport

/** Selection is data; equal-time marks retain Timeline's stable input order. */
final case class MarkSelector[K](kind: K, occurrence: Occurrence)

final class EpochBins private[plan] (
    val anchor: Instant,
    val interval: Interval,
    val bins: Vector[Interval],
    val excludedTail: Option[Interval]
)

enum EpochError[T, K] derives CanEqual:
  case Clock(trial: T, selector: MarkSelector[K], underlying: TimeError)
  case AnchorMatches(trial: T, selector: MarkSelector[K], count: Int)
  case AnchorOverflow(trial: T, selector: MarkSelector[K], anchor: Instant, window: Window)
  case DurationOverflow(trial: T, window: Window, durationMicros: BigInt)
  case NonDivisible(trial: T, window: Window, width: PositiveSpan, remainderMicros: Long)
  case BinLimit(
      trial: T,
      window: Window,
      width: PositiveSpan,
      requested: BigInt,
      maximum: NonNegativeLong
  )

  def message: String = this match
    case Clock(t, s, e)         => s"Trial '$t' selector=$s: ${e.message}"
    case AnchorMatches(t, s, n) =>
      s"Trial '$t' selector=$s cannot select from $n matching marks."
    case AnchorOverflow(t, s, a, w) =>
      s"Trial '$t' selector=$s anchorMicros=${a.toMicros} window=$w exceeds signed 64-bit time."
    case DurationOverflow(t, w, d) =>
      s"Trial '$t' window=$w has durationMicros=$d, exceeding the signed 64-bit Span representation."
    case NonDivisible(t, w, b, r) =>
      s"Trial '$t' window=$w widthMicros=${b.toMicros} leaves remainderMicros=$r."
    case BinLimit(t, w, b, n, m) =>
      s"Trial '$t' window=$w widthMicros=${b.toMicros} requires $n bins; maximum=${m.toLong}."

/** A serializable epoch recipe. Resolution requires observed timing and an
  * explicit allocation budget; arithmetic is exact even beyond 2^53 micros.
  */
final class EpochPlan[K] private (
    val anchor: MarkSelector[K],
    val window: Window,
    val binWidth: PositiveSpan,
    val finalBin: FinalBin
):
  def resolve[T](
      trial: T,
      timeline: ObservedTimeline[K],
      expectedClock: ClockId,
      maximumBins: NonNegativeLong
  )(using keys: Eq[K]): Either[EpochError[T, K], EpochBins] =
    for
      clock <- Agreement
        .clocks(expectedClock, timeline.clock)
        .left
        .map(e => EpochError.Clock(trial, anchor, e))
      matches  = timeline.marks.filter(m => keys.eqv(m.value, anchor.kind))
      selected = anchor.occurrence match
        case Occurrence.RequireUnique => Option.when(matches.size == 1)(0)
        case Occurrence.First         => Option.when(matches.nonEmpty)(0)
        case Occurrence.Last          => Option.when(matches.nonEmpty)(matches.size - 1)
        case Occurrence.Nth(index)    =>
          Option.when(index.toLong < matches.size.toLong)(index.toLong.toInt)
      mark <- selected
        .map(matches)
        .toRight(EpochError.AnchorMatches(trial, anchor, matches.size))
      from  = BigInt(mark.at.toMicros) + window.from.toMicros
      until = BigInt(mark.at.toMicros) + window.until.toMicros
      _ <- Either.cond(
        from.isValidLong && until.isValidLong,
        (),
        EpochError.AnchorOverflow(trial, anchor, mark.at, window)
      )
      width  = BigInt(binWidth.toMicros)
      length = until - from
      _ <- Either.cond(
        length.isValidLong,
        (),
        EpochError.DurationOverflow(trial, window, length)
      )
      full      = length / width
      remainder = length % width
      _ <- Either.cond(
        finalBin != FinalBin.RequireExactDivision || remainder == 0,
        (),
        EpochError.NonDivisible(trial, window, binWidth, remainder.toLong)
      )
      count = full + (if finalBin == FinalBin.IncludeShortFinal && remainder > 0 then 1 else 0)
      _ <- Either.cond(
        count <= maximumBins.toLong && count.isValidInt,
        (),
        EpochError.BinLimit(trial, window, binWidth, count, maximumBins)
      )
      interval <- Interval
        .of(clock, Instant.micros(from.toLong), Instant.micros(until.toLong))
        .left
        .map(e => EpochError.Clock(trial, anchor, e))
      // Endpoints have been bounded above. Traverse retains constructor failures as values.
      bins <- (0 until count.toInt)
        .foldLeft[Either[EpochError[T, K], Vector[Interval]]](Right(Vector.empty)) { (acc, i) =>
          for
            built <- acc
            bin   <- Interval
              .of(
                clock,
                Instant.micros((from + width * i).toLong),
                Instant.micros((from + width * (BigInt(i) + 1)).min(until).toLong)
              )
              .left
              .map(e => EpochError.Clock(trial, anchor, e))
          yield built :+ bin
        }
      tail <-
        if finalBin == FinalBin.ExcludeAndReport && remainder > 0 then
          Interval
            .of(
              clock,
              Instant.micros((from + width * full).toLong),
              Instant.micros(until.toLong)
            )
            .left
            .map(e => EpochError.Clock(trial, anchor, e))
            .map(Some(_))
        else Right(None)
    yield new EpochBins(mark.at, interval, bins, tail)

object EpochPlan:
  def of[K](
      anchor: MarkSelector[K],
      window: Window,
      binWidth: PositiveSpan,
      finalBin: FinalBin
  ): Either[TimeError, EpochPlan[K]] =
    // Recheck the window so the constructor remains the single admission path.
    Window.of(window.from, window.until).map(w => new EpochPlan(anchor, w, binWidth, finalBin))

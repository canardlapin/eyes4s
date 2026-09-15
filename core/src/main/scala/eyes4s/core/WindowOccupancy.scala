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

import cats.syntax.all.*
import eyes4s.kernel.*

/** How a duration map treats fixations crossing a window or a coverage gap. */
enum FixationBoundary derives CanEqual:
  case ClipDuration
  case FullyContained

/** Disjoint observed intervals on one clock. Gaps carry no observed time. */
final class ObservedCoverage private (val clock: ClockId, val intervals: Vector[Interval]):
  private[core] def overlapMicros(onset: Long, offset: Long): BigInt =
    intervals.foldLeft(BigInt(0)) { (total, interval) =>
      val start = math.max(onset, interval.onset.toMicros)
      val end   = math.min(offset, interval.offset.toMicros)
      total + (if end > start then BigInt(end) - start else BigInt(0))
    }

object ObservedCoverage:
  def of(
      clock: ClockId,
      intervals: Vector[Interval]
  ): Either[WindowOccupancyError, ObservedCoverage] =
    val sorted = intervals.sortBy(_.onset.toMicros)
    for
      _ <- sorted.traverse(i =>
        Agreement.clocks(clock, i.clock).left.map(WindowOccupancyError.Time.apply)
      )
      _ <- Either.cond(
        !sorted.exists(_.isEmpty),
        (),
        WindowOccupancyError.EmptyCoverageInterval(clock, intervals)
      )
      _ <- Either.cond(
        !sorted.zip(sorted.drop(1)).exists { case (a, b) =>
          a.offset.toMicros > b.onset.toMicros
        },
        (),
        WindowOccupancyError.OverlappingCoverage(clock, intervals)
      )
    yield new ObservedCoverage(clock, sorted)

/** One ledger entry for every original fixation, including excluded ones. */
final case class FixationWindowTime(index: Int, originalMicros: BigInt, retainedMicros: Long)
    derives CanEqual

final class WindowOccupancy[U <: Unit2D] private[core] (
    val interval: Interval,
    val boundary: FixationBoundary,
    val measure: PointMeasure[U],
    val observedMicros: Long,
    val missingMicros: Long,
    val fixationTimes: Vector[FixationWindowTime]
):
  def retainedMicros: Long           = fixationTimes.foldLeft(0L)(_ + _.retainedMicros)
  def excludedFixations: Vector[Int] = fixationTimes.filter(_.retainedMicros == 0).map(_.index)

object WindowOccupancy:
  def apply[U <: Unit2D](
      path: Scanpath[U],
      interval: Interval,
      coverage: ObservedCoverage,
      boundary: FixationBoundary
  ): Either[WindowOccupancyError, WindowOccupancy[U]] =
    val width = BigInt(interval.offset.toMicros) - interval.onset.toMicros
    for
      _ <- Agreement
        .clocks(path.clock, interval.clock)
        .left
        .map(WindowOccupancyError.Time.apply)
      _ <- Agreement
        .clocks(path.clock, coverage.clock)
        .left
        .map(WindowOccupancyError.Time.apply)
      _ <- Either.cond(
        width > 0 && width.isValidLong,
        (),
        WindowOccupancyError.InvalidWidth(interval, width)
      )
      ledger = path.fixations.toVector.zipWithIndex.map { case (fixation, index) =>
        val original = BigInt(fixation.span.offset.toMicros) - fixation.span.onset.toMicros
        val start    = math.max(fixation.span.onset.toMicros, interval.onset.toMicros)
        val end      = math.min(fixation.span.offset.toMicros, interval.offset.toMicros)
        val covered  = if end > start then coverage.overlapMicros(start, end) else BigInt(0)
        val retained = boundary match
          case FixationBoundary.ClipDuration   => covered
          case FixationBoundary.FullyContained =>
            if covered == original then covered else BigInt(0)
        FixationWindowTime(index, original, retained.toLong)
      }
      selected = ledger.filter(_.retainedMicros > 0)
      measure <- PointMeasure
        .of(
          path.frame,
          IArray.from(selected.map(row => path.fixations(row.index).centre)),
          IArray.from(selected.map(_.retainedMicros.toDouble / 1000000.0))
        )
        .left
        .map(WindowOccupancyError.Measure.apply)
      observed = coverage
        .overlapMicros(interval.onset.toMicros, interval.offset.toMicros)
        .toLong
    yield new WindowOccupancy(
      interval,
      boundary,
      measure,
      observed,
      width.toLong - observed,
      ledger
    )

enum WindowOccupancyError derives CanEqual:
  case Time(underlying: TimeError)
  case Measure(underlying: SurfaceError)
  case InvalidWidth(interval: Interval, micros: BigInt)
  case EmptyCoverageInterval(clock: ClockId, intervals: Vector[Interval])
  case OverlappingCoverage(clock: ClockId, intervals: Vector[Interval])
  def message: String = this match
    case Time(e)            => e.message
    case Measure(e)         => e.message
    case InvalidWidth(i, n) =>
      s"Window ${i.render} needs a positive representable duration, got $n microseconds."
    case EmptyCoverageInterval(c, xs) =>
      s"Observed coverage on $c contains an empty interval: $xs."
    case OverlappingCoverage(c, xs) =>
      s"Observed coverage on $c contains overlapping intervals: $xs."

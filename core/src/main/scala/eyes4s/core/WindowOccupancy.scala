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

  /** Checked reconstruction of an archived occupancy from its ledger and the
    * positions of its retained fixations, without the scanpath or coverage it
    * was computed from. What follows from the ledger alone is re-derived and
    * checked: the window has a positive representable width, the observed and
    * missing time partition it, every ledger row sits at its own fixation
    * index with `0 <= retained <= original` and no more retained than the
    * window observed (and, under `FullyContained`, `retained` either zero or
    * the whole fixation), one position is supplied
    * per retained fixation, in ledger order, and each retained fixation's
    * weight is its retained time in seconds, exactly as `apply` weights it.
    * The positions themselves are the scanpath's fixation centres, which only
    * the input can confirm.
    */
  def reconstruct[U <: Unit2D](
      interval: Interval,
      boundary: FixationBoundary,
      frame: Frame[U],
      positions: IArray[Pt[U]],
      observedMicros: Long,
      missingMicros: Long,
      fixationTimes: Vector[FixationWindowTime]
  ): Either[WindowOccupancyError, WindowOccupancy[U]] =
    val width    = BigInt(interval.offset.toMicros) - interval.onset.toMicros
    val selected = fixationTimes.filter(_.retainedMicros > 0)
    for
      _ <- Either.cond(
        width > 0 && width.isValidLong,
        (),
        WindowOccupancyError.InvalidWidth(interval, width)
      )
      _ <- Either.cond(
        observedMicros >= 0 && missingMicros >= 0 &&
          BigInt(observedMicros) + missingMicros == width,
        (),
        WindowOccupancyError.ObservedTime(interval, observedMicros, missingMicros)
      )
      _ <- fixationTimes.zipWithIndex
        .collectFirst {
          case (row, position)
              if row.index != position || row.retainedMicros < 0 ||
                BigInt(row.retainedMicros) > row.originalMicros ||
                row.retainedMicros > observedMicros ||
                (boundary == FixationBoundary.FullyContained && row.retainedMicros != 0 &&
                  BigInt(row.retainedMicros) != row.originalMicros) =>
            WindowOccupancyError.Ledger(
              position,
              row.index,
              row.originalMicros,
              row.retainedMicros,
              boundary
            )
        }
        .toLeft(())
      _ <- Either.cond(
        positions.length == selected.size,
        (),
        WindowOccupancyError.MeasureSupport(selected.size, positions.length)
      )
      measure <- PointMeasure
        .of(frame, positions, IArray.from(selected.map(_.retainedMicros.toDouble / 1000000.0)))
        .left
        .map(WindowOccupancyError.Measure.apply)
    yield new WindowOccupancy(
      interval,
      boundary,
      measure,
      observedMicros,
      missingMicros,
      fixationTimes
    )

enum WindowOccupancyError derives CanEqual:
  case Time(underlying: TimeError)
  case Measure(underlying: SurfaceError)
  case InvalidWidth(interval: Interval, micros: BigInt)
  case EmptyCoverageInterval(clock: ClockId, intervals: Vector[Interval])
  case OverlappingCoverage(clock: ClockId, intervals: Vector[Interval])

  /** An archived occupancy's observed and missing time do not partition its window. */
  case ObservedTime(interval: Interval, observedMicros: Long, missingMicros: Long)

  /** An archived ledger row is out of place or retains time its fixation cannot. */
  case Ledger(
      position: Int,
      index: Int,
      originalMicros: BigInt,
      retainedMicros: Long,
      boundary: FixationBoundary
  )

  /** An archived measure does not hold one position per retained fixation. */
  case MeasureSupport(retained: Int, positions: Int)
  def message: String = this match
    case Time(e)            => e.message
    case Measure(e)         => e.message
    case InvalidWidth(i, n) =>
      s"Window ${i.render} needs a positive representable duration, got $n microseconds."
    case EmptyCoverageInterval(c, xs) =>
      s"Observed coverage on $c contains an empty interval: $xs."
    case OverlappingCoverage(c, xs) =>
      s"Observed coverage on $c contains overlapping intervals: $xs."
    case ObservedTime(i, observed, missing) =>
      s"Window ${i.render} records observed=$observed and missing=$missing microseconds, " +
        "which do not partition its width."
    case Ledger(position, index, original, retained, b) =>
      s"Ledger row $position names fixation $index with original=$original and " +
        s"retained=$retained microseconds, which $b cannot retain."
    case MeasureSupport(retained, positions) =>
      s"The ledger retains $retained fixations but the measure holds $positions positions."

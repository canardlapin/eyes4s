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

/** Both policies select the latest onset, ignoring fixation offsets and gaps. */
enum TrajectoryEndpoint derives CanEqual:
  /** Missing before the first and after the last onset; the last onset itself is included. */
  case OnsetRange

  /** Missing before the first onset; the final fixation is held indefinitely. */
  case HoldLastOnset

enum TrajectoryMissing derives CanEqual:
  case Empty, BeforeFirstOnset, AfterLastOnset

final case class FixationLocation[U <: Unit2D](index: Int, point: Pt[U])
final case class TrajectorySample[U <: Unit2D](
    time: Instant,
    location: Either[TrajectoryMissing, FixationLocation[U]]
)
final class TrajectorySamples[U <: Unit2D] private[core] (
    val frame: Frame[U],
    val clock: ClockId,
    val endpoint: TrajectoryEndpoint,
    val rows: Vector[TrajectorySample[U]]
)

/** Shared deterministic trajectory for overlap, density lookup and temporal sampling.
  * Inputs use exact microsecond instants; no implicit conversion from seconds or milliseconds.
  * A nonempty trajectory inherits strict ordering from Scanpath. Empty support is explicit.
  */
final class FixationTrajectory[U <: Unit2D] private (
    val frame: Frame[U],
    val clock: ClockId,
    private val fixations: Vector[Event.Fixation[U]]
):
  def sample(
      queryClock: ClockId,
      times: Vector[Instant],
      endpoint: TrajectoryEndpoint
  ): Either[TimeError, TrajectorySamples[U]] =
    Agreement.clocks(clock, queryClock).map { _ =>
      new TrajectorySamples(
        frame,
        clock,
        endpoint,
        times.map(t => TrajectorySample(t, at(t, endpoint)))
      )
    }

  private def at(
      time: Instant,
      endpoint: TrajectoryEndpoint
  ): Either[TrajectoryMissing, FixationLocation[U]] =
    if fixations.isEmpty then Left(TrajectoryMissing.Empty)
    else if time.toMicros < fixations.head.span.onset.toMicros then
      Left(TrajectoryMissing.BeforeFirstOnset)
    else if endpoint == TrajectoryEndpoint.OnsetRange && time.toMicros > fixations.last.span.onset.toMicros
    then Left(TrajectoryMissing.AfterLastOnset)
    else
      var low  = 0
      var high = fixations.size
      while low < high do
        val middle = low + (high - low) / 2
        if fixations(middle).span.onset.toMicros <= time.toMicros then low = middle + 1
        else high = middle
      val index = low - 1
      Right(FixationLocation(index, fixations(index).centre))

object FixationTrajectory:
  def fromScanpath[U <: Unit2D](path: Scanpath[U]): FixationTrajectory[U] =
    new FixationTrajectory(path.frame, path.clock, path.fixations.toVector)
  def empty[U <: Unit2D](frame: Frame[U], clock: ClockId): FixationTrajectory[U] =
    new FixationTrajectory(frame, clock, Vector.empty)

enum ReplicationError derives CanEqual:
  case Period(microseconds: Long)
  case Duration(index: Int, microseconds: Long)
  case MaximumRows(value: Int)
  case Cardinality(requested: BigInt, maximum: Int)
  def message: String = this match
    case Period(us)      => s"Replication period=$us microseconds must be positive."
    case Duration(i, us) =>
      s"Replication duration at index=$i is $us microseconds; negative durations are invalid."
    case MaximumRows(n)      => s"Replication maximumRows=$n must be nonnegative."
    case Cardinality(n, max) => s"Replication requests $n rows, exceeding maximumRows=$max."

/** Exact positive sampling period; never inferred by rounding a floating frequency. */
final class ReplicationPeriod private (val duration: Span)
object ReplicationPeriod:
  def of(duration: Span): Either[ReplicationError, ReplicationPeriod] =
    Either.cond(
      duration.toMicros > 0,
      new ReplicationPeriod(duration),
      ReplicationError.Period(duration.toMicros)
    )

final case class ReplicatedFixation[U <: Unit2D](sourceIndex: Int, fixation: Event.Fixation[U])
object DurationReplication:
  /** floor(duration/period), with a minimum of one for each nonnegative duration.
    * A caller-supplied row bound is checked using exact integer arithmetic before allocation.
    */
  def counts(
      durations: Vector[Span],
      period: ReplicationPeriod,
      maximumRows: Int
  ): Either[ReplicationError, Vector[Int]] =
    if maximumRows < 0 then Left(ReplicationError.MaximumRows(maximumRows))
    else
      durations.zipWithIndex.find(_._1.isNegative) match
        case Some((duration, index)) =>
          Left(ReplicationError.Duration(index, duration.toMicros))
        case None =>
          val counts = durations.map(d => math.max(1L, d.toMicros / period.duration.toMicros))
          val total  = counts.foldLeft(BigInt(0))((sum, n) => sum + BigInt(n))
          if total > BigInt(maximumRows) then
            Left(ReplicationError.Cardinality(total, maximumRows))
          else Right(counts.map(_.toInt))

  /** Replicas remain rows, not a Scanpath with fabricated duplicate onset ordering. */
  def replicate[U <: Unit2D](
      path: Scanpath[U],
      period: ReplicationPeriod,
      maximumRows: Int
  ): Either[ReplicationError, Vector[ReplicatedFixation[U]]] =
    counts(path.fixations.toVector.map(_.duration), period, maximumRows).map { sizes =>
      sizes.zipWithIndex.flatMap((n, i) =>
        Vector.fill(n)(ReplicatedFixation(i, path.fixations(i)))
      )
    }

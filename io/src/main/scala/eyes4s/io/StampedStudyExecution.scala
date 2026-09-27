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

package eyes4s.io

import eyes4s.codec.{RunStamp, StampedCountedStudyCursor, StampedStudy, StampedStudyResult}
import eyes4s.compare.{ComparisonBudget, ComparisonQuantum}
import eyes4s.design.{PairQuantum, WorkQuanta}
import eyes4s.fs2.{RunProgress, Submission}
import eyes4s.kernel.Unit2D
import eyes4s.plan.*

/** Canonical scientific identity plus the quanta that determine study step boundaries.
  * Sample quanta do not affect this cursor. A scheduling job identity remains separate.
  */
final case class StampedStudyRunId[K, U <: Unit2D, P, S, D](
    stamp: RunStamp[StudyPlan[K, U, P, S, D], StudyInput[K, U]],
    pairQuantum: PairQuantum,
    comparisonQuantum: ComparisonQuantum
):
  def sameAs(other: StampedStudyRunId[K, U, P, S, D]): Boolean =
    stamp.sameAs(other.stamp) && pairQuantum.value == other.pairQuantum.value &&
      comparisonQuantum.value == other.comparisonQuantum.value

  override def equals(other: Any): Boolean = other match
    case that: StampedStudyRunId[?, ?, ?, ?, ?] =>
      stamp.equals(that.stamp) && pairQuantum.value == that.pairQuantum.value &&
      comparisonQuantum.value == that.comparisonQuantum.value
    case _ => false
  override def hashCode: Int = (stamp, pairQuantum.value, comparisonQuantum.value).hashCode

/** The codec/fs2 bridge lives in io, the module permitted to depend on both. */
object StampedStudyExecution:
  def submission[K, U <: Unit2D, P, S, D](
      work: StampedStudy[K, U, P, S, D],
      budget: ComparisonBudget = ComparisonBudget.default,
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[
    StampedStudyRunId[K, U, P, S, D],
    StampedCountedStudyCursor[K, U, P, S, D],
    StudyRunStage,
    StudyRunSegment,
    StudyRunError,
    StampedStudyResult[K, U, P, S, D]
  ] = new Submission(
    StampedStudyRunId(work.stamp, quanta.pairs, quanta.comparison),
    quanta,
    () => work.countedWork(budget),
    StudyRunSegment.of,
    (segment, cursor) => cursor.total(segment),
    Some((cursor, step) => cursor.completedStage(step))
  )

  /** Export the committed snapshot without moving codec dependencies into fs2. */
  def snapshot[K, U <: Unit2D, P, S, D](
      progress: RunProgress[StampedStudyRunId[K, U, P, S, D], StudyRunStage, StudyRunSegment]
  ): Either[
    eyes4s.codec.CodecError,
    eyes4s.codec.StudyProgressSnapshot[StudyPlan[K, U, P, S, D], StudyInput[K, U]]
  ] =
    if progress.segment != StudyRunSegment.of(progress.stage) then
      Left(
        eyes4s.codec.CodecError.Field(
          "segment",
          _root_.io.circe.Json.fromString(progress.segment.toString),
          s"segment must agree with completed stage ${progress.stage}"
        )
      )
    else
      eyes4s.codec.StudyProgressSnapshot.of(
        progress.run.stamp,
        progress.run.pairQuantum,
        progress.run.comparisonQuantum,
        progress.step,
        progress.stage,
        progress.stepUnits,
        progress.segmentUnits,
        progress.segmentTotal,
        progress.totalUnits
      )

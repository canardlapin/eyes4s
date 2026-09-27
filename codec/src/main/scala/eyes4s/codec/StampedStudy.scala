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

package eyes4s.codec

import eyes4s.compare.ComparisonBudget
import eyes4s.design.{PairScheduleBudget, WorkQuanta}
import eyes4s.kernel.Unit2D
import eyes4s.plan.*

/** A result created only by completion of its bound execution cursor.
  * Persisted stamps are separate claims; decoding one cannot construct this evidence.
  */
final class StampedStudyResult[K, U <: Unit2D, P, S, D] private (
    val stamp: RunStamp[StudyPlan[K, U, P, S, D], StudyInput[K, U]],
    val result: StudyResult[K, U, S, D]
):
  def checkAgainst(
      current: StampedStudy[K, U, P, S, D]
  ): Either[
    RunStampError[StudyPlan[K, U, P, S, D], StudyInput[K, U]],
    StudyResult[K, U, S, D]
  ] =
    stamp
      .check(current.stamp, PlanChange.between(result.description, current.plan.description))
      .map(_ => result)

object StampedStudyResult:
  private[codec] def completed[K, U <: Unit2D, P, S, D](
      stamp: RunStamp[StudyPlan[K, U, P, S, D], StudyInput[K, U]],
      result: StudyResult[K, U, S, D]
  ): StampedStudyResult[K, U, P, S, D] = new StampedStudyResult(stamp, result)

/** Preparation captures canonical identities and the exact work together.
  * There is no public operation to attach a stamp to a caller-supplied result.
  */
final class StampedStudy[K, U <: Unit2D, P, S, D] private (
    val plan: StudyPlan[K, U, P, S, D],
    val prepared: PreparedStudy[K, U, P, S, D],
    val stamp: RunStamp[StudyPlan[K, U, P, S, D], StudyInput[K, U]]
):
  def work(
      budget: ComparisonBudget = ComparisonBudget.default
  ): Either[PlanError, StampedStudyCursor[K, U, P, S, D]] =
    prepared.work(budget).map(new StampedStudyCursor(_, stamp))

  /** One cursor counts, assembles diagnostics and executes without recounting. */
  def countedWork(
      budget: ComparisonBudget = ComparisonBudget.default
  ): Either[StudyRunError, StampedCountedStudyCursor[K, U, P, S, D]] =
    CountedStudyCursor.of(prepared, budget).map(new StampedCountedStudyCursor(_, stamp))

  def run: Either[PlanError, StampedStudyResult[K, U, P, S, D]] =
    work().flatMap(Stepwise.complete(_, WorkQuanta.default))

object StampedStudy:
  def prepare[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      plans: StudyCodec[K, U, P, S, D],
      inputs: StudyInputCodec[K, U],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[CodecError, StampedStudy[K, U, P, S, D]] =
    for
      stamp    <- RunStamp.of(plan, input, plans.codec, inputs.input)
      prepared <- plan.prepare(input, budget).left.map(CodecError.Definition(_))
    yield new StampedStudy(plan, prepared, stamp)

/** The stamp travels with the cursor and is attached at its terminal step. */
final class StampedStudyCursor[K, U <: Unit2D, P, S, D] private[codec] (
    private val cursor: StudyCursor[K, U, S, D],
    val stamp: RunStamp[StudyPlan[K, U, P, S, D], StudyInput[K, U]]
):
  def stage: StudyStage = cursor.stage

  def advance(quanta: WorkQuanta): Either[PlanError, WorkStep[
    StudyStage,
    StampedStudyCursor[K, U, P, S, D],
    StampedStudyResult[K, U, P, S, D]
  ]] =
    cursor.advance(quanta).map {
      case StudyStep.More(stage, units, next) =>
        WorkStep.More(stage, units, new StampedStudyCursor(next, stamp))
      case StudyStep.Done(units, result) =>
        WorkStep.Done(units, StampedStudyResult.completed(stamp, result))
    }

object StampedStudyCursor:
  given [K, U <: Unit2D, P, S, D]: Stepwise[
    StampedStudyCursor[K, U, P, S, D],
    StudyStage,
    PlanError,
    StampedStudyResult[K, U, P, S, D]
  ] with
    def stage(cursor: StampedStudyCursor[K, U, P, S, D]): StudyStage = cursor.stage
    def advance(cursor: StampedStudyCursor[K, U, P, S, D], quanta: WorkQuanta): Either[
      PlanError,
      WorkStep[StudyStage, StampedStudyCursor[K, U, P, S, D], StampedStudyResult[K, U, P, S, D]]
    ] = cursor.advance(quanta)

/** A composite cursor whose terminal result receives only its captured canonical stamp. */
final class StampedCountedStudyCursor[K, U <: Unit2D, P, S, D] private[codec] (
    private val cursor: CountedStudyCursor[K, U, P, S, D],
    val stamp: RunStamp[StudyPlan[K, U, P, S, D], StudyInput[K, U]]
):
  def stage: StudyRunStage                          = cursor.stage
  def total(segment: StudyRunSegment): SegmentTotal = cursor.total(segment)

  def advance(quanta: WorkQuanta): Either[StudyRunError, WorkStep[
    StudyRunStage,
    StampedCountedStudyCursor[K, U, P, S, D],
    StampedStudyResult[K, U, P, S, D]
  ]] = cursor.advance(quanta).map {
    case WorkStep.More(stage, units, next) =>
      WorkStep.More(stage, units, new StampedCountedStudyCursor(next, stamp))
    case WorkStep.Done(units, result) =>
      WorkStep.Done(units, StampedStudyResult.completed(stamp, result))
  }

  def completedStage(
      step: WorkStep[
        StudyRunStage,
        StampedCountedStudyCursor[K, U, P, S, D],
        StampedStudyResult[K, U, P, S, D]
      ]
  ): Either[StudyRunError, StudyRunStage] = step match
    case WorkStep.More(stage, _, _)   => Right(stage)
    case WorkStep.Done(units, result) =>
      cursor.completedStage(WorkStep.Done(units, result.result))

object StampedCountedStudyCursor:
  given [K, U <: Unit2D, P, S, D]: Stepwise[
    StampedCountedStudyCursor[K, U, P, S, D],
    StudyRunStage,
    StudyRunError,
    StampedStudyResult[K, U, P, S, D]
  ] with
    def stage(cursor: StampedCountedStudyCursor[K, U, P, S, D]): StudyRunStage = cursor.stage
    def advance(cursor: StampedCountedStudyCursor[K, U, P, S, D], quanta: WorkQuanta): Either[
      StudyRunError,
      WorkStep[
        StudyRunStage,
        StampedCountedStudyCursor[K, U, P, S, D],
        StampedStudyResult[K, U, P, S, D]
      ]
    ] = cursor.advance(quanta)

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

package eyes4s.fs2

import cats.effect.kernel.{Concurrent, Resource}
import eyes4s.compare.ComparisonBudget
import eyes4s.design.WorkQuanta
import eyes4s.kernel.{Provenance, Unit2D}
import eyes4s.plan.*

import _root_.fs2.Stream

/** The identity of one submission: the exact prepared input and plan revision
  * plus the quanta that cut its steps. It is a function of those values, so
  * two submissions of the same prepared study at the same quanta share an id
  * and their event sequences are identical. An id names the work; a consumer
  * that schedules the same work twice attaches its own job identity.
  *
  * The `ComparisonBudget` is deliberately not part of the id: it changes no
  * produced value, only whether the run begins, and a refusal is already
  * visible as `StudyOutcome.Failed` carrying the budget error. The sample
  * quantum is not part of it either: no study step feeds samples.
  */
final case class StudyRunId(
    input: String,
    layout: DefinitionId,
    method: DefinitionId,
    parameters: Vector[(String, Vector[Provenance.Param])],
    pairQuantum: Int,
    comparisonQuantum: Int
) derives CanEqual

object StudyRunId:
  def of[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D],
      quanta: WorkQuanta
  ): StudyRunId =
    StudyRunId(
      work.inputReference.digest,
      work.layoutId,
      work.methodId,
      work.description,
      quanta.pairs.value,
      quanta.comparison.value
    )

/** The counted span of a run is [[eyes4s.plan.StudySegment]], stated in the
  * pure plan module with its totals so the published execution laws check
  * the shipped claims; these aliases keep the name and its cases here.
  */
type StudySegment = eyes4s.plan.StudySegment
val StudySegment: eyes4s.plan.StudySegment.type = eyes4s.plan.StudySegment

/** The study family's instances of the shared runner vocabulary; see
  * [[RunProgress]], [[RunOutcome]], [[RunEvent]] and [[Run]]. `StudyProgress`
  * is an alias: its companion offers `apply` for construction only, with no
  * `unapply`, so match on `RunProgress(...)` or read its fields. The outcome
  * and event companions forward the shared cases, which construct and match.
  */
type StudyProgress = RunProgress[StudyRunId, StudyStage, StudySegment]
object StudyProgress:
  def apply(
      run: StudyRunId,
      step: Long,
      stage: StudyStage,
      stepUnits: Int,
      segment: StudySegment,
      segmentUnits: Long,
      segmentTotal: SegmentTotal,
      totalUnits: Long
  ): StudyProgress =
    RunProgress(run, step, stage, stepUnits, segment, segmentUnits, segmentTotal, totalUnits)

type StudyOutcome[K, U <: Unit2D, S, D] =
  RunOutcome[StudyRunId, StudyStage, StudySegment, PlanError, StudyResult[K, U, S, D]]
object StudyOutcome:
  type Completed[K, U <: Unit2D, S, D] =
    RunOutcome.Completed[StudyRunId, StudyStage, StudySegment, PlanError, StudyResult[
      K,
      U,
      S,
      D
    ]]
  type Cancelled[K, U <: Unit2D, S, D] =
    RunOutcome.Cancelled[StudyRunId, StudyStage, StudySegment, PlanError, StudyResult[
      K,
      U,
      S,
      D
    ]]
  type Failed[K, U <: Unit2D, S, D] =
    RunOutcome.Failed[StudyRunId, StudyStage, StudySegment, PlanError, StudyResult[K, U, S, D]]
  val Completed: RunOutcome.Completed.type = RunOutcome.Completed
  val Cancelled: RunOutcome.Cancelled.type = RunOutcome.Cancelled
  val Failed: RunOutcome.Failed.type       = RunOutcome.Failed

type StudyEvent[K, U <: Unit2D, S, D] =
  RunEvent[StudyRunId, StudyStage, StudySegment, PlanError, StudyResult[K, U, S, D]]
object StudyEvent:
  type Advanced[K, U <: Unit2D, S, D] =
    RunEvent.Advanced[StudyRunId, StudyStage, StudySegment, PlanError, StudyResult[K, U, S, D]]
  type Finished[K, U <: Unit2D, S, D] =
    RunEvent.Finished[StudyRunId, StudyStage, StudySegment, PlanError, StudyResult[K, U, S, D]]
  val Advanced: RunEvent.Advanced.type = RunEvent.Advanced
  val Finished: RunEvent.Finished.type = RunEvent.Finished

type StudyRun[F[_], K, U <: Unit2D, S, D] =
  Run[F, StudyRunId, StudyStage, StudySegment, PlanError, StudyResult[K, U, S, D]]

/** Interpret a prepared study's bounded steps under Cats Effect.
  *
  * Both entry points drive the same [[StudyCursor]] that `PreparedStudy.run`
  * drives, so a completed result is the pure runner's result. Each step is
  * one `cursor.advance(quanta)` followed by its bookkeeping, evaluated as a
  * single uncancelable region; the fiber cedes before every step, so
  * cancellation lands between steps and never inside one. Cancellation of
  * one trial's estimation therefore waits for that trial (X2 is deferred).
  *
  * This is a thin wrapper: it builds the [[Submission]] and hands it to the
  * shared [[Execution]] runner.
  */
object StudyExecution:

  /** Fix the effect type once: `StudyExecution[IO].start(work)`. */
  def apply[F[_]](using F: Concurrent[F]): Runner[F] = new Runner[F]

  final class Runner[F[_]](using F: Concurrent[F]):
    private val execution = Execution[F]

    /** The deterministic step sequence as a stream; see [[Execution.Runner.events]]. */
    def events[K, U <: Unit2D, P, S, D](
        work: PreparedStudy[K, U, P, S, D],
        budget: ComparisonBudget = ComparisonBudget.default,
        quanta: WorkQuanta = WorkQuanta.default
    ): Stream[F, StudyEvent[K, U, S, D]] =
      execution.events(submission(work, budget, quanta))

    /** Run a prepared study on its own fiber; see [[Execution.Runner.start]]. */
    def start[K, U <: Unit2D, P, S, D](
        work: PreparedStudy[K, U, P, S, D],
        budget: ComparisonBudget = ComparisonBudget.default,
        quanta: WorkQuanta = WorkQuanta.default,
        between: F[Unit] = F.cede
    ): Resource[F, StudyRun[F, K, U, S, D]] =
      execution.start(submission(work, budget, quanta), between)

  /** The study family's submission: id, cursor, segments and totals. */
  def submission[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D],
      budget: ComparisonBudget = ComparisonBudget.default,
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[
    StudyRunId,
    StudyCursor[K, U, S, D],
    StudyStage,
    StudySegment,
    PlanError,
    StudyResult[K, U, S, D]
  ] =
    new Submission(
      StudyRunId.of(work, quanta),
      quanta,
      () => work.work(budget),
      StudySegment.of,
      total(work, _, _)
    )

  /** Automatic counting followed by scientific execution under a caller's typed
    * identity. Counting completion is a transition in this one submission, never
    * a completed run. Every running stage carries its committed object meter.
    * The original submission remains the compatibility path for unit-only progress.
    */
  def submissionWithId[Id, K, U <: Unit2D, P, S, D](
      id: Id,
      work: PreparedStudy[K, U, P, S, D],
      budget: ComparisonBudget = ComparisonBudget.default,
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[
    Id,
    CountedStudyCursor[K, U, P, S, D],
    StudyRunStage,
    StudyRunSegment,
    StudyRunError,
    StudyResult[K, U, S, D]
  ] =
    new Submission(
      id,
      quanta,
      () => CountedStudyCursor.of(work, budget),
      StudyRunSegment.of,
      (segment, cursor) => cursor.total(segment),
      Some((cursor, step) => cursor.completedStage(step))
    )

  /** The total stated as a segment begins; see [[eyes4s.plan.StudySegment.total]]. */
  def total[K, U <: Unit2D, S, D](
      work: PreparedStudy[K, U, ?, S, D],
      segment: StudySegment,
      cursor: StudyCursor[K, U, S, D]
  ): SegmentTotal = StudySegment.total(work, segment, cursor)

  /** Totals the prepared study can state before the segment runs; see
    * [[eyes4s.plan.StudySegment.total]]. A reduction is `Unknown` here and
    * exact once its segment begins.
    */
  def total[K, U <: Unit2D, S, D](
      work: PreparedStudy[K, U, ?, S, D],
      segment: StudySegment
  ): SegmentTotal = StudySegment.total(work, segment)

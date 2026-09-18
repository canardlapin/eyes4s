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

/** The identity of one temporal submission: the temporal input digest (the
  * fixation input plus every anchor and coverage ledger), the base layout and
  * method identities, the full plan description with its windows and
  * repetitions, and the quanta that cut the study steps. As for a study, the
  * comparison budget and the sample quantum are not part of it.
  */
final case class TemporalRunId(
    input: String,
    layout: DefinitionId,
    method: DefinitionId,
    parameters: Vector[(String, Vector[Provenance.Param])],
    pairQuantum: Int,
    comparisonQuantum: Int
) derives CanEqual

object TemporalRunId:
  def of[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      quanta: WorkQuanta
  ): TemporalRunId =
    TemporalRunId(
      work.inputReference.digest,
      work.layoutId,
      work.methodId,
      work.description,
      quanta.pairs.value,
      quanta.comparison.value
    )

/** The counted span of a temporal run is [[eyes4s.plan.TemporalSegment]],
  * stated in the pure plan module with its totals; these aliases keep the
  * name and its cases here.
  */
type TemporalSegment = eyes4s.plan.TemporalSegment
val TemporalSegment: eyes4s.plan.TemporalSegment.type = eyes4s.plan.TemporalSegment

type TemporalProgress = RunProgress[TemporalRunId, TemporalStage, TemporalSegment]
type TemporalOutcome[K, U <: Unit2D, P, S, D] = RunOutcome[
  TemporalRunId,
  TemporalStage,
  TemporalSegment,
  TemporalStudyError,
  TemporalStudyResult[K, U, P, S, D]
]
type TemporalEvent[K, U <: Unit2D, P, S, D] = RunEvent[
  TemporalRunId,
  TemporalStage,
  TemporalSegment,
  TemporalStudyError,
  TemporalStudyResult[K, U, P, S, D]
]
type TemporalRun[F[_], K, U <: Unit2D, P, S, D] = Run[
  F,
  TemporalRunId,
  TemporalStage,
  TemporalSegment,
  TemporalStudyError,
  TemporalStudyResult[K, U, P, S, D]
]

/** Interpret a prepared temporal study's bounded steps under Cats Effect
  * through the shared [[Execution]] runner. The cursor is the one
  * `TemporalStudyPlan.run` drives: per cell, one step per trial's occupancy,
  * then the cell's study cursor step for step, so a temporal run's step
  * sequence is its cells' study sequences concatenated and stamped, and a
  * completed result is the pure result. Cancellation lands between steps:
  * between trials of a cell's preparation, between study steps, and between
  * cells; no partial cell is ever returned as a result.
  */
object TemporalExecution:

  def apply[F[_]](using F: Concurrent[F]): Runner[F] = new Runner[F]

  final class Runner[F[_]](using F: Concurrent[F]):
    private val execution = Execution[F]

    /** The deterministic step sequence as a stream; see [[Execution.Runner.events]]. */
    def events[K, U <: Unit2D, P, S, D](
        work: PreparedTemporalStudy[K, U, P, S, D],
        budget: ComparisonBudget = ComparisonBudget.default,
        quanta: WorkQuanta = WorkQuanta.default
    ): Stream[F, TemporalEvent[K, U, P, S, D]] =
      execution.events(submission(work, budget, quanta))

    /** Run a prepared temporal study on its own fiber; see [[Execution.Runner.start]]. */
    def start[K, U <: Unit2D, P, S, D](
        work: PreparedTemporalStudy[K, U, P, S, D],
        budget: ComparisonBudget = ComparisonBudget.default,
        quanta: WorkQuanta = WorkQuanta.default,
        between: F[Unit] = F.cede
    ): Resource[F, TemporalRun[F, K, U, P, S, D]] =
      execution.start(submission(work, budget, quanta), between)

  /** The temporal family's submission: id, cursor, segments and totals. */
  def submission[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      budget: ComparisonBudget = ComparisonBudget.default,
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[
    TemporalRunId,
    TemporalCursor[K, U, P, S, D],
    TemporalStage,
    TemporalSegment,
    TemporalStudyError,
    TemporalStudyResult[K, U, P, S, D]
  ] =
    new Submission(
      TemporalRunId.of(work, quanta),
      quanta,
      () => work.work(budget),
      TemporalSegment.of,
      total(work, _, _)
    )

  /** The total stated as a segment begins; see [[eyes4s.plan.TemporalSegment.total]]. */
  def total[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      segment: TemporalSegment,
      cursor: TemporalCursor[K, U, P, S, D]
  ): SegmentTotal = TemporalSegment.total(work, segment, cursor)

  /** Totals the prepared temporal study can state before a segment runs; see
    * [[eyes4s.plan.TemporalSegment.total]].
    */
  def total[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      segment: TemporalSegment
  ): SegmentTotal = TemporalSegment.total(work, segment)

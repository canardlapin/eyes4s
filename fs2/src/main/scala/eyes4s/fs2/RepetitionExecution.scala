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
import eyes4s.design.WorkQuanta
import eyes4s.kernel.Unit2D
import eyes4s.plan.*

import _root_.fs2.Stream

/** The identity of one repetition submission: the plan's input and plan
  * content hashes and the pair quantum that cuts its steps. Comparisons are
  * whole, so the comparison quantum is not part of it.
  */
final case class RepetitionRunId(input: String, plan: String, pairQuantum: Int) derives CanEqual

object RepetitionRunId:
  def of[K, U <: Unit2D](plan: RepetitionPlan[K, U], quanta: WorkQuanta): RepetitionRunId =
    RepetitionRunId(plan.inputHash.render, plan.planHash.render, quanta.pairs.value)

type RepetitionProgress   = RunProgress[RepetitionRunId, RepetitionStage, RepetitionStage]
type RepetitionOutcome[K] =
  RunOutcome[RepetitionRunId, RepetitionStage, RepetitionStage, Nothing, RepetitionPlanResult[
    K
  ]]
type RepetitionEvent[K] =
  RunEvent[RepetitionRunId, RepetitionStage, RepetitionStage, Nothing, RepetitionPlanResult[K]]
type RepetitionRun[F[_], K] =
  Run[F, RepetitionRunId, RepetitionStage, RepetitionStage, Nothing, RepetitionPlanResult[K]]

/** Interpret a repetition plan's bounded steps under Cats Effect through the
  * shared [[Execution]] runner. The cursor is [[eyes4s.plan.RepetitionPlan!.work RepetitionPlan.work]]: the
  * matched pairs, then the control pairs, a page of comparisons per step, so a
  * completed run is the pure `run`. Each stage is its own segment, with the
  * exact pair total its pairing fixed. Cancellation lands between steps.
  */
object RepetitionExecution:

  def apply[F[_]](using F: Concurrent[F]): Runner[F] = new Runner[F]

  final class Runner[F[_]](using F: Concurrent[F]):
    private val execution = Execution[F]

    /** The deterministic step sequence as a stream; see [[Execution.Runner.events]]. */
    def events[K, U <: Unit2D](
        plan: RepetitionPlan[K, U],
        quanta: WorkQuanta = WorkQuanta.default
    ): Stream[F, RepetitionEvent[K]] =
      execution.events(submission(plan, quanta))

    /** Run a repetition plan on its own fiber; see [[Execution.Runner.start]]. */
    def start[K, U <: Unit2D](
        plan: RepetitionPlan[K, U],
        quanta: WorkQuanta = WorkQuanta.default,
        between: F[Unit] = F.cede
    ): Resource[F, RepetitionRun[F, K]] =
      execution.start(submission(plan, quanta), between)

  /** The repetition family's submission: id, cursor, segments and totals. */
  def submission[K, U <: Unit2D](
      plan: RepetitionPlan[K, U],
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[
    RepetitionRunId,
    RepetitionCursor[K],
    RepetitionStage,
    RepetitionStage,
    Nothing,
    RepetitionPlanResult[K]
  ] =
    new Submission(
      RepetitionRunId.of(plan, quanta),
      quanta,
      () => Right(plan.work),
      identity,
      (stage, cursor) => SegmentTotal.Exact(cursor.totalPairs(stage))
    )

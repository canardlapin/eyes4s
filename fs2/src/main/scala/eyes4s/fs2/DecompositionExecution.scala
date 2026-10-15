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
import eyes4s.design.{DecompositionError, WorkQuanta}
import eyes4s.kernel.Unit2D
import eyes4s.plan.*
import _root_.fs2.Stream

final case class DecompositionRunId(input: String, plan: String) derives CanEqual
object DecompositionRunId:
  def of[U <: Unit2D](plan: DecompositionPlan[U]): DecompositionRunId =
    DecompositionRunId(plan.inputHash.render, plan.planHash.render)
type DecompositionProgress =
  RunProgress[DecompositionRunId, DecompositionStage, DecompositionStage]
type DecompositionOutcome[U <: Unit2D] = RunOutcome[
  DecompositionRunId,
  DecompositionStage,
  DecompositionStage,
  DecompositionError,
  DecompositionResult[U]
]
type DecompositionEvent[U <: Unit2D] = RunEvent[
  DecompositionRunId,
  DecompositionStage,
  DecompositionStage,
  DecompositionError,
  DecompositionResult[U]
]
type DecompositionRun[F[_], U <: Unit2D] = Run[
  F,
  DecompositionRunId,
  DecompositionStage,
  DecompositionStage,
  DecompositionError,
  DecompositionResult[U]
]

/** One whole surface solve: cancellation is observed before that operation or
  * after completion, with no intra-solver latency promise.
  */
object DecompositionExecution:
  def apply[F[_]](using F: Concurrent[F]): Runner[F] = new Runner[F]
  final class Runner[F[_]](using F: Concurrent[F]):
    private val execution = Execution[F]
    def events[U <: Unit2D](
        plan: DecompositionPlan[U],
        quanta: WorkQuanta = WorkQuanta.default
    ): Stream[F, DecompositionEvent[U]] = execution.events(submission(plan, quanta))
    def start[U <: Unit2D](
        plan: DecompositionPlan[U],
        quanta: WorkQuanta = WorkQuanta.default,
        between: F[Unit] = F.cede
    ): Resource[F, DecompositionRun[F, U]] = execution.start(submission(plan, quanta), between)
  def submission[U <: Unit2D](
      plan: DecompositionPlan[U],
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[DecompositionRunId, DecompositionCursor[
    U
  ], DecompositionStage, DecompositionStage, DecompositionError, DecompositionResult[U]] =
    new Submission(
      DecompositionRunId.of(plan),
      quanta,
      () => Right(plan.work),
      identity,
      (_, _) => SegmentTotal.Exact(1L)
    )

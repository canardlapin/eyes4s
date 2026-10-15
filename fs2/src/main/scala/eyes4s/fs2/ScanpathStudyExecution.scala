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

final case class ScanpathStudyRunId(input: String, plan: String, pairQuantum: Int)
    derives CanEqual
object ScanpathStudyRunId:
  def of[K, U <: Unit2D](
      plan: ScanpathStudyPlan[K, U],
      quanta: WorkQuanta
  ): ScanpathStudyRunId =
    ScanpathStudyRunId(plan.inputHash.render, plan.planHash.render, quanta.pairs.value)

type ScanpathStudyProgress =
  RunProgress[ScanpathStudyRunId, ScanpathStudyStage, ScanpathStudyStage]
type ScanpathStudyOutcome[K, U <: Unit2D] =
  RunOutcome[
    ScanpathStudyRunId,
    ScanpathStudyStage,
    ScanpathStudyStage,
    PlanError,
    ScanpathStudyResult[K, U]
  ]
type ScanpathStudyEvent[K, U <: Unit2D] =
  RunEvent[
    ScanpathStudyRunId,
    ScanpathStudyStage,
    ScanpathStudyStage,
    PlanError,
    ScanpathStudyResult[K, U]
  ]
type ScanpathStudyRun[F[_], K, U <: Unit2D] =
  Run[
    F,
    ScanpathStudyRunId,
    ScanpathStudyStage,
    ScanpathStudyStage,
    PlanError,
    ScanpathStudyResult[K, U]
  ]

/** Cancellation boundaries separate whole preparation, whole pair comparisons
  * and whole final reduction. A pair quantum never interrupts an alignment,
  * query batch or transport solver. No partial result is returned on cancel.
  */
object ScanpathStudyExecution:
  def apply[F[_]](using F: Concurrent[F]): Runner[F] = new Runner[F]
  final class Runner[F[_]](using F: Concurrent[F]):
    private val execution = Execution[F]
    def events[K, U <: Unit2D](
        plan: ScanpathStudyPlan[K, U],
        quanta: WorkQuanta = WorkQuanta.default
    ): Stream[F, ScanpathStudyEvent[K, U]] =
      execution.events(submission(plan, quanta))
    def start[K, U <: Unit2D](
        plan: ScanpathStudyPlan[K, U],
        quanta: WorkQuanta = WorkQuanta.default,
        between: F[Unit] = F.cede
    ): Resource[F, ScanpathStudyRun[F, K, U]] =
      execution.start(submission(plan, quanta), between)
  def submission[K, U <: Unit2D](
      plan: ScanpathStudyPlan[K, U],
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[ScanpathStudyRunId, ScanpathStudyCursor[
    K,
    U
  ], ScanpathStudyStage, ScanpathStudyStage, PlanError, ScanpathStudyResult[K, U]] =
    new Submission(
      ScanpathStudyRunId.of(plan, quanta),
      quanta,
      () => Right(plan.work),
      identity,
      (stage, cursor) =>
        SegmentTotal.Exact(stage match
          case ScanpathStudyStage.PreparingWholeStudy | ScanpathStudyStage.ReducingWholeStudy =>
            1L
          case _ => cursor.totalPairs)
    )

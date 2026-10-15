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

/** The identity of one point-sampling submission: the plan's input and plan
  * content hashes and the pair quantum that cuts its steps. Comparisons are
  * whole, so the comparison quantum is not part of it.
  */
final case class PointSamplingRunId(input: String, plan: String, pairQuantum: Int)
    derives CanEqual

object PointSamplingRunId:
  def of[K, U <: Unit2D](
      plan: PointSamplingPlan[K, U],
      quanta: WorkQuanta
  ): PointSamplingRunId =
    PointSamplingRunId(plan.inputHash.render, plan.planHash.render, quanta.pairs.value)

type PointSamplingProgress =
  RunProgress[PointSamplingRunId, PointSamplingStage, PointSamplingStage]
type PointSamplingOutcome[K, U <: Unit2D] =
  RunOutcome[
    PointSamplingRunId,
    PointSamplingStage,
    PointSamplingStage,
    Nothing,
    PointSamplingResult[K, U]
  ]
type PointSamplingEvent[K, U <: Unit2D] =
  RunEvent[
    PointSamplingRunId,
    PointSamplingStage,
    PointSamplingStage,
    Nothing,
    PointSamplingResult[K, U]
  ]
type PointSamplingRun[F[_], K, U <: Unit2D] =
  Run[
    F,
    PointSamplingRunId,
    PointSamplingStage,
    PointSamplingStage,
    Nothing,
    PointSamplingResult[K, U]
  ]

/** Run immutable point-sampling work through the shared runner. Preparation is
  * one whole operation, followed by pages of whole focal trials. Cancellation
  * takes effect between those operations; normalization and a trial's queries,
  * controls and reductions cannot be interrupted internally.
  */
object PointSamplingExecution:

  def apply[F[_]](using F: Concurrent[F]): Runner[F] = new Runner[F]

  final class Runner[F[_]](using F: Concurrent[F]):
    private val execution = Execution[F]

    /** The deterministic step sequence as a stream; see [[Execution.Runner.events]]. */
    def events[K, U <: Unit2D](
        plan: PointSamplingPlan[K, U],
        quanta: WorkQuanta = WorkQuanta.default
    ): Stream[F, PointSamplingEvent[K, U]] =
      execution.events(submission(plan, quanta))

    /** Run a point-sampling plan on its own fiber; see [[Execution.Runner.start]]. */
    def start[K, U <: Unit2D](
        plan: PointSamplingPlan[K, U],
        quanta: WorkQuanta = WorkQuanta.default,
        between: F[Unit] = F.cede
    ): Resource[F, PointSamplingRun[F, K, U]] =
      execution.start(submission(plan, quanta), between)

  /** Submission with one preparation unit and the exact focal-trial total. */
  def submission[K, U <: Unit2D](
      plan: PointSamplingPlan[K, U],
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[
    PointSamplingRunId,
    PointSamplingCursor[K, U],
    PointSamplingStage,
    PointSamplingStage,
    Nothing,
    PointSamplingResult[K, U]
  ] =
    new Submission(
      PointSamplingRunId.of(plan, quanta),
      quanta,
      () => Right(plan.work),
      identity,
      (stage, cursor) =>
        SegmentTotal.Exact(stage match
          case PointSamplingStage.PreparingWholeStudy => 1L
          case PointSamplingStage.SamplingWholeTrials => cursor.totalTrials.toLong)
    )

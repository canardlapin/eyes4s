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
import eyes4s.design.{TemplateError, TemplateValidationStage, WorkQuanta}
import eyes4s.plan.*

import _root_.fs2.Stream

/** The identity of one template-fit submission: the plan's input and plan
  * content hashes and the row quantum that cuts evaluation steps. Fitting and
  * feature extraction remain whole; comparison/sample quanta do not split them.
  */
final case class TemplateFitRunId(input: String, plan: String, pairQuantum: Int)
    derives CanEqual

object TemplateFitRunId:
  def of[K, X](
      plan: TemplateFitPlan[K, X],
      quanta: WorkQuanta
  ): TemplateFitRunId =
    TemplateFitRunId(plan.inputHash.render, plan.planHash.render, quanta.pairs.value)

type TemplateFitProgress =
  RunProgress[TemplateFitRunId, TemplateValidationStage, TemplateValidationStage]
type TemplateFitOutcome[K, X] =
  RunOutcome[
    TemplateFitRunId,
    TemplateValidationStage,
    TemplateValidationStage,
    TemplateError,
    TemplateFitResult[K, X]
  ]
type TemplateFitEvent[K, X] =
  RunEvent[
    TemplateFitRunId,
    TemplateValidationStage,
    TemplateValidationStage,
    TemplateError,
    TemplateFitResult[K, X]
  ]
type TemplateFitRun[F[_], K, X] =
  Run[
    F,
    TemplateFitRunId,
    TemplateValidationStage,
    TemplateValidationStage,
    TemplateError,
    TemplateFitResult[K, X]
  ]

/** Run immutable template-fit work through the shared runner. Fitting is
  * one whole operation, followed by pages of whole held-out rows. Cancellation
  * takes effect between those operations; native fitting and a row's feature
  * extraction cannot be interrupted internally.
  */
object TemplateFitExecution:

  def apply[F[_]](using F: Concurrent[F]): Runner[F] = new Runner[F]

  final class Runner[F[_]](using F: Concurrent[F]):
    private val execution = Execution[F]

    /** The deterministic step sequence as a stream; see [[Execution.Runner.events]]. */
    def events[K, X](
        plan: TemplateFitPlan[K, X],
        quanta: WorkQuanta = WorkQuanta.default
    ): Stream[F, TemplateFitEvent[K, X]] =
      execution.events(submission(plan, quanta))

    /** Run a template-fit plan on its own fiber; see [[Execution.Runner.start]]. */
    def start[K, X](
        plan: TemplateFitPlan[K, X],
        quanta: WorkQuanta = WorkQuanta.default,
        between: F[Unit] = F.cede
    ): Resource[F, TemplateFitRun[F, K, X]] =
      execution.start(submission(plan, quanta), between)

  /** Submission with one fitting unit and the exact held-out row total. */
  def submission[K, X](
      plan: TemplateFitPlan[K, X],
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[
    TemplateFitRunId,
    TemplateFitCursor[K, X],
    TemplateValidationStage,
    TemplateValidationStage,
    TemplateError,
    TemplateFitResult[K, X]
  ] =
    new Submission(
      TemplateFitRunId.of(plan, quanta),
      quanta,
      () => Right(plan.work),
      identity,
      (stage, cursor) =>
        SegmentTotal.Exact(stage match
          case TemplateValidationStage.FittingWholeTraining |
              TemplateValidationStage.CheckingWholeImportedReceipt =>
            1L
          case TemplateValidationStage.EvaluatingWholeRows => cursor.totalRows.toLong)
    )

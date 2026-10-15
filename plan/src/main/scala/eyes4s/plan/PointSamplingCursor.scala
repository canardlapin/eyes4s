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

package eyes4s.plan

import eyes4s.design.WorkQuanta
import eyes4s.kernel.Unit2D

/** Cancellation boundaries name the actual indivisible operations. */
enum PointSamplingStage derives CanEqual:
  case PreparingWholeStudy, SamplingWholeTrials

private[plan] final class PreparedPointSampling[K, U <: Unit2D](
    val totalTrials: Int,
    val trial: Int => PointTrialResult[K, U],
    val result: Vector[PointTrialResult[K, U]] => PointSamplingResult[K, U]
)

/** Immutable point-sampling work. Preparation runs once as a whole operation;
  * each sampling unit evaluates one complete focal trial, including all its
  * queries, selected controls and reductions. Pair quanta cap trial keys.
  * There is no intra-preparation or intra-trial cancellation guarantee.
  */
final class PointSamplingCursor[K, U <: Unit2D] private[plan] (
    plan: PointSamplingPlan[K, U],
    prepared: Option[PreparedPointSampling[K, U]],
    val processedTrials: Int,
    completed: Vector[PointTrialResult[K, U]]
):
  def stage: PointSamplingStage = prepared match
    case None    => PointSamplingStage.PreparingWholeStudy
    case Some(_) => PointSamplingStage.SamplingWholeTrials

  def totalTrials: Int = plan.source.trials.rows.size

  def advance(
      quanta: WorkQuanta
  ): WorkStep[PointSamplingStage, PointSamplingCursor[K, U], PointSamplingResult[K, U]] =
    prepared match
      case None =>
        val ready = plan.prepareWork
        if ready.totalTrials == 0 then WorkStep.Done(1, ready.result(Vector.empty))
        else
          WorkStep.More(
            PointSamplingStage.PreparingWholeStudy,
            1,
            new PointSamplingCursor(plan, Some(ready), 0, Vector.empty)
          )
      case Some(ready) =>
        val count = math.min(quanta.pairs.value, ready.totalTrials - processedTrials)
        val end   = processedTrials + count
        val rows  = completed ++ (processedTrials until end).toVector.map(ready.trial)
        if end == ready.totalTrials then WorkStep.Done(count, ready.result(rows))
        else
          WorkStep.More(
            PointSamplingStage.SamplingWholeTrials,
            count,
            new PointSamplingCursor(plan, prepared, end, rows)
          )

  def complete(quanta: WorkQuanta = WorkQuanta.default): PointSamplingResult[K, U] =
    @annotation.tailrec
    def loop(work: PointSamplingCursor[K, U]): PointSamplingResult[K, U] =
      work.advance(quanta) match
        case WorkStep.More(_, _, next) => loop(next)
        case WorkStep.Done(_, result)  => result
    loop(this)

object PointSamplingCursor:
  given stepwise[K, U <: Unit2D]: Stepwise[PointSamplingCursor[
    K,
    U
  ], PointSamplingStage, Nothing, PointSamplingResult[K, U]] with
    def stage(cursor: PointSamplingCursor[K, U]): PointSamplingStage = cursor.stage
    def advance(cursor: PointSamplingCursor[K, U], quanta: WorkQuanta): Either[
      Nothing,
      WorkStep[PointSamplingStage, PointSamplingCursor[K, U], PointSamplingResult[K, U]]
    ] =
      Right(cursor.advance(quanta))

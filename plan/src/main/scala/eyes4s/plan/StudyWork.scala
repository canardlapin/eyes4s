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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*

/** Prepared work binds the exact input, plan description, layout and method
  * identity to two reusable schedules. Preparation does no density estimation
  * or comparisons. Source indexing/identity checks are O(source rows), bounded
  * by the supplied budget; arbitrary custom projections must be pure/stable.
  *
  * Pair order follows the original input: focal-major, reference-minor. Keys
  * remain typed values; their display strings and digests never decide equality.
  * The ordinary runner still materializes completed scores and performs whole
  * numerical operations. Those operations become incremental in subsequent work.
  */
final class PreparedStudy[K, U <: Unit2D, P, S, D] private[plan] (
    private val plan: StudyPlan[K, U, P, S, D],
    val input: StudyInput[K, U],
    val description: Vector[(String, Vector[Provenance.Param])],
    val focalIndices: Vector[Int],
    val referenceIndices: Vector[Int],
    val excludedPhases: Vector[K],
    val frameChecks: Vector[Either[StudyFailure[K], Unit]],
    val matched: DirectedPairSchedule[K, K],
    val controls: DirectedPairSchedule[K, K],
    val candidateVisitsAcrossScales: Long,
    val budget: PairScheduleBudget
):
  val inputReference: ArtifactRef[StudyInput[K, U]] = input.reference
  val layoutId: DefinitionId                        = plan.layout.id
  val methodId: DefinitionId                        = plan.method.id
  val estimates: Vector[StudyEstimate[U]]           = plan.estimates

  /** Execute this exact prepared plan, retaining its source order and evidence. */
  def run: Either[PlanError, StudyResult[K, U, S, D]] =
    execute(
      (key, path) => path.occupancy(plan.weight).left.map(StudyFailure.Occupancy(key, _)),
      Vector.empty
    )

  private[plan] def execute(
      occupancy: (K, Scanpath[U]) => Either[StudyFailure[K], PointMeasure[U]],
      context: Vector[(String, Provenance.Param)]
  ): Either[PlanError, StudyResult[K, U, S, D]] =
    if plan.description != description then
      Left(PlanError.ChangedPreparedPlan(methodId, layoutId))
    else plan.executeWork(this, occupancy, context)

object PreparedStudy:
  private[plan] def build[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      budget: PairScheduleBudget
  ): Either[PlanError, PreparedStudy[K, U, P, S, D]] =
    for
      _ <- budget
        .checkCounts(input.trials.rows.size.toLong, 0L)
        .left
        .map(PlanError.Schedule.apply)
      phases          = input.trials.rows.map(t => plan.layout.phase(t.key))
      focal           = phases.indices.filter(i => phases(i) == plan.focalPhase).toVector
      reference       = phases.indices.filter(i => phases(i) == plan.referencePhase).toVector
      candidateVisits = BigInt(focal.size) * reference.size * 2 * plan.estimates.size
      _ <- Either.cond(
        candidateVisits <= budget.maxCandidatePairs,
        (),
        PlanError.StudyWorkBudget(
          focal.size,
          reference.size,
          plan.estimates.size,
          budget.maxCandidatePairs
        )
      )
      left          = focal.map(i => input.trials.rows(i).key)
      right         = reference.map(i => input.trials.rows(i).key)
      matchedDesign = Pairing
        .between[K, K]
        .sameOn(plan.layout.participant, plan.layout.participant)
        .sameOn(plan.layout.stimulus, plan.layout.stimulus)
        .all
      controlDesign = Pairing
        .between[K, K]
        .sameOn(plan.layout.participant, plan.layout.participant)
        .differentOn(plan.layout.stimulus, plan.layout.stimulus)
        .all
      matched <- DirectedPairSchedule
        .exhaustive(left, right, matchedDesign.relation, budget)
        .left
        .map(PlanError.Schedule.apply)
      controls <- DirectedPairSchedule
        .exhaustive(left, right, controlDesign.relation, budget)
        .left
        .map(PlanError.Schedule.apply)
    yield new PreparedStudy(
      plan,
      input,
      plan.description,
      focal,
      reference,
      phases.indices
        .filter(i => phases(i) != plan.focalPhase && phases(i) != plan.referencePhase)
        .map(i => input.trials.rows(i).key)
        .toVector,
      input.trials.rows.map(t =>
        Agreement
          .frames(plan.grid.frame, t.value.frame)
          .map(_ => ())
          .left
          .map(StudyFailure.Frame(t.key, _))
      ),
      matched,
      controls,
      candidateVisits.toLong,
      budget
    )

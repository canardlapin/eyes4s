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

import eyes4s.design.*
import eyes4s.kernel.{Provenance, Unit2D}

/** A cancellable count of both exact study schedules, without estimating maps
  * or comparing them. Each step is one pair page, bounded by the pair quantum.
  * Completion includes unmatched-reference diagnostics, even for empty schedules.
  * The quantum bounds candidate visits. Matched-phase completion also aggregates
  * retained pairs and source keys for cardinality; that work is not quantum-bounded.
  */
final class CountCursor[K] private[plan] (
    private val cursor: PairCursor[K, K],
    private val controls: DirectedPairSchedule[K, K],
    private val phase: CountPhase[K],
    private val pairs: Vector[(K, K)],
    private val focal: Set[K],
    private val cardinality: (Vector[(K, K)], PairingReport[K, K]) => MatchedCardinality[K],
    private val maps: Int,
    private val scales: Int,
    val visited: Long,
    private val input: ArtifactRef[?],
    private val description: Vector[(String, Vector[Provenance.Param])]
):
  def stage: StudyDesign = phase match
    case CountPhase.Matched()     => StudyDesign.Matched
    case CountPhase.Control(_, _) => StudyDesign.Control

  def advance(
      quanta: WorkQuanta
  ): Either[PlanError, WorkStep[StudyDesign, CountCursor[K], StudyCounts[K]]] =
    cursor.advance(quanta.pairs).left.map(PlanError.Schedule.apply).map {
      case PairPage.More(page, units, next) =>
        WorkStep.More(
          stage,
          units,
          new CountCursor(
            next,
            controls,
            phase,
            appendMatched(page),
            focal ++ page.map(_.left),
            cardinality,
            maps,
            scales,
            visited + units,
            input,
            description
          )
        )
      case PairPage.Done(page, units, report) =>
        val allFocal = focal ++ page.map(_.left)
        val counts   = new DesignCounts(
          report.eligiblePairCount,
          allFocal.size.toLong,
          report.unmatchedLeft.size.toLong,
          report.unmatchedRight.size.toLong,
          report.ambiguous.size.toLong
        )
        phase match
          case CountPhase.Matched() =>
            val matchedCardinality = cardinality(appendMatched(page), report)
            WorkStep.More(
              stage,
              units,
              new CountCursor(
                controls.start,
                controls,
                CountPhase.Control(counts, matchedCardinality),
                Vector.empty,
                Set.empty,
                cardinality,
                maps,
                scales,
                visited + units,
                input,
                description
              )
            )
          case CountPhase.Control(matched, matchedCardinality) =>
            WorkStep.Done(
              units,
              new StudyCounts(
                matched,
                counts,
                matchedCardinality,
                matched.focalWithPairs + matched.unmatchedFocal,
                maps.toLong,
                scales,
                input,
                description
              )
            )
    }

  private def appendMatched(page: Vector[ScheduledPair[K, K]]): Vector[(K, K)] = phase match
    case CountPhase.Matched()     => pairs ++ page.map(p => p.left -> p.right)
    case CountPhase.Control(_, _) => Vector.empty

private[plan] enum CountPhase[K]:
  case Matched()
  case Control(matched: DesignCounts, cardinality: MatchedCardinality[K])

object CountCursor:
  /** Bind counting to an unchanged prepared plan. Source eligibility comes
    * from its schedules; window failures are deliberately not a filter.
    */
  def of[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      work: PreparedStudy[K, U, P, S, D]
  ): Either[PlanError, CountCursor[K]] =
    if plan.input != work.inputReference then
      Left(PlanError.ArtifactMismatch(plan.input.digest, work.inputReference.digest))
    else if plan.description != work.description then
      Left(PlanError.ChangedPreparedPlan(work.methodId, work.layoutId))
    else
      Right(
        new CountCursor(
          work.matched.start,
          work.controls,
          CountPhase.Matched(),
          Vector.empty,
          Set.empty,
          (pairs, report) =>
            StudyPairingWork.cardinalityFromPairs(
              plan.layout,
              plan.pairing,
              work.input.trials.rows.map(_.key),
              work.referenceIndices.map(i => work.input.trials.rows(i).key),
              pairs,
              report
            ),
          work.input.trials.rows.size,
          work.estimates.size,
          0L,
          work.inputReference,
          work.description
        )
      )

  given [K]: Stepwise[CountCursor[K], StudyDesign, PlanError, StudyCounts[K]] with
    def stage(cursor: CountCursor[K]): StudyDesign = cursor.stage
    def advance(cursor: CountCursor[K], quanta: WorkQuanta): Either[
      PlanError,
      WorkStep[StudyDesign, CountCursor[K], StudyCounts[K]]
    ] = cursor.advance(quanta)

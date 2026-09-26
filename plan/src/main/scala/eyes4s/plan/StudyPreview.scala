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
import eyes4s.kernel.*

/** Scientific choices and pageable eligibility for one prepared study.
  *
  * The two schedules are the exact objects consumed by execution. Their
  * candidatePairCount is an upper bound before relation filtering; exact eligible
  * counts and unmatched keys arrive in PairPage.Done. Neither is a successful or
  * contributing score count. No density estimation or comparison runs here.
  *
  * Order is focal-major/reference-minor in source order. ScheduledPair indices
  * and duplicate indices address focalKeys/referenceKeys, not the full input.
  * Repeated stimulus occurrences with distinct full keys remain separate trials.
  * Only duplicate full keys are ambiguous and excluded in full.
  *
  * inputReference, layoutId, methodId and description stamp this snapshot. A UI
  * retaining it across edits can use checkCurrent before presenting it as current.
  * Registered behavior and projections must be pure and stable for their ids.
  *
  * windowTallies lists, in input order, how each trial falls against the
  * screen and the plan's analysis window (a trial in another frame carries its
  * refusal); a trial whose map
  * would be empty, or that the window policy fails, fails at every scale with
  * StudyFailure.OffWindow.
  *
  * initialFixationTallies lists, in input order, how many leading fixations
  * of each trial the plan's initial-fixation policy drops; the window
  * tallies count only the fixations it keeps.
  *
  * matchedCardinality is the prepared study's own value: focal trials with
  * more than one matched reference, ambiguous control references, unmatched
  * focal trials and item conflicts under the plan's pairing, and whether they
  * refuse the study.
  */
final class StudyPreview[K, U <: Unit2D] private[plan] (
    val inputReference: ArtifactRef[StudyInput[K, U]],
    val layoutId: DefinitionId,
    val methodId: DefinitionId,
    val description: Vector[(String, Vector[Provenance.Param])],
    val focalKeys: Vector[K],
    val referenceKeys: Vector[K],
    val excludedPhases: Vector[K],
    val matched: DirectedPairSchedule[K, K],
    val controls: DirectedPairSchedule[K, K],
    val failurePolicy: FailurePolicy,
    val windowTallies: Vector[(K, Either[GeometryError, WindowTally])],
    val pairing: StudyPairing,
    cardinality: () => Either[PlanError, MatchedCardinality[K]],
    val initialFixationTallies: Vector[(K, Either[GeometryError, InitialFixationTally])],
    val counts: Option[StudyCounts[K]]
):
  /** Legacy synchronous cardinality access. Constructing a preview does not enumerate pairs. */
  lazy val matchedCardinality: Either[PlanError, MatchedCardinality[K]] = cardinality()

  /** Initial fixations the plan's policy drops, across the input. */
  def initialFixationSummary: InitialFixationSummary =
    InitialFixationSummary.of(initialFixationTallies)

  /** Records outside the analysis window and the screen, across the input:
    * for example "543 of 11,520 records in 409 trials".
    */
  def windowSummary: WindowSummary = WindowSummary.of(windowTallies)

  /** Both matched and control scores are averaged by the focal (left) key. */
  val reductionOrientation: ReductionOrientation = ReductionOrientation.ByLeft

  /** Refuse a stale snapshot after either source data or declared choices change. */
  def checkCurrent[P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U]
  ): Either[PlanError, Unit] =
    if input.reference != inputReference then
      Left(PlanError.ArtifactMismatch(inputReference.digest, input.reference.digest))
    else if plan.description != description then
      Left(PlanError.ChangedPreparedPlan(methodId, layoutId))
    else Right(())

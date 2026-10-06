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

package eyes4s.studio.core.real

import cats.syntax.all.*
import eyes4s.codec.{CodecError, StudyCodecs, StudyInputCodecs, StudyResultCodecs}
import eyes4s.plan.{ComparisonMethod, StudyCounts, StudyPreview, TrialKey as CoreKey}
import eyes4s.kernel.Unit2D
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.Recipe

/** One analysis revision's study as eyes4s prepared it: the plan, its
  * schedules ([[StudyPreview]]) and its exact counts ([[StudyCounts]]), over
  * the admitted input of the revision's dataset.
  */
final class RealPrepared private (
    val revision: AnalysisRevision,
    val dataset: DatasetRevision,
    val recipe: Recipe,
    val plan: RealPlan.Plan,
    val method: ComparisonMethod,
    val admitted: RealDatasetContext,
    val work: RealPlan.Work,
    val preview: StudyPreview[CoreKey, Unit2D.Px],
    val counts: StudyCounts[CoreKey],
    val summary: PreviewSummary
):
  /** The native pairing refusal retained with these exact counts, if any. */
  val pairingRefusal: Option[eyes4s.plan.PlanError] = counts.cardinality.refusal(plan.layout)

  /** eyes4s's codecs for this study: the trial-keyed route of its method. */
  def plans   = StudyCodecs.trialSimilarity[Unit2D.Px](method)
  def inputs  = StudyInputCodecs.trial[Unit2D.Px]
  def results = StudyResultCodecs.trialRegistered[Unit2D.Px](method)

  /** The canonical digest of a result of this study, as eyes4s encodes it. */
  def digest(result: RealExecution.Result): Either[CodecError, String] =
    results.codec.digest(result).map(_.sha256.hex)

object RealPrepared:

  /** Prepare without visiting pairs; bounded preview counting begins here. */
  def configure(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      recipe: Recipe,
      admitted: AdmittedDataset
  ): Either[BackendError, RealConfigured] =
    for
      planned <- RealPlan.plan(revision, recipe, admitted.screen, admitted.input)
      (plan, method) = planned
      work    <- plan.prepare(admitted.input).leftMap(refused(revision, "preparation"))
      preview <- work.preview.leftMap(refused(revision, "preview"))
    yield new RealConfigured(revision, dataset, recipe, plan, method, admitted, work, preview)

  /** Synchronous compatibility route; bounded callers supply completed counts. */
  def of(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      recipe: Recipe,
      admitted: AdmittedDataset
  ): Either[BackendError, RealPrepared] =
    for
      configured <- configure(revision, dataset, recipe, admitted)
      counts     <- configured.work.counts.leftMap(refused(revision, "counts"))
      result     <- fromCounts(configured, counts)
    yield result

  private[real] def refused(revision: AnalysisRevision, step: String)(
      e: eyes4s.plan.PlanError
  ): BackendError =
    BackendError.Unavailable(
      DiagnosticLocus.Artifact(s"${revision.label} $step: ${RealPlan.reason(e)}")
    )

  private[real] def fromCounts(
      configured: RealConfigured,
      counts: StudyCounts[CoreKey]
  ): Either[BackendError, RealPrepared] =
    configured.work.preview(counts).leftMap(refused(configured.revision, "preview")).map {
      preview =>
        complete(
          configured.revision,
          configured.dataset,
          configured.recipe,
          configured.plan,
          configured.method,
          configured.admitted,
          configured.work,
          preview,
          counts
        )
    }

  /** Prepare only the stored input's schedules/read context, never its estimates
    * or pair scores (bead q-native-archive-readback).
    */
  private[real] def archived(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      recipe: Recipe,
      plan: RealPlan.Plan,
      method: ComparisonMethod,
      context: RealDatasetContext
  ): Either[BackendError, RealPrepared] =
    for
      work    <- plan.prepare(context.input).leftMap(refused(revision, "archived preparation"))
      counts  <- work.counts.leftMap(refused(revision, "archived counts"))
      preview <- work.preview(counts).leftMap(refused(revision, "archived preview"))
    yield complete(revision, dataset, recipe, plan, method, context, work, preview, counts)

  private def complete(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      recipe: Recipe,
      plan: RealPlan.Plan,
      method: ComparisonMethod,
      context: RealDatasetContext,
      work: RealPlan.Work,
      preview: StudyPreview[CoreKey, Unit2D.Px],
      counts: StudyCounts[CoreKey]
  ): RealPrepared = new RealPrepared(
    revision,
    dataset,
    recipe,
    plan,
    method,
    context,
    work,
    preview,
    counts,
    PreviewSummary(
      revision,
      dataset,
      recipe.scales.values.map(s => Degrees.label(s.degrees)),
      focalTrials = preview.focalKeys.size,
      referenceTrials = preview.referenceKeys.size,
      requestedQueries = context.ledger.count(_.trial.phase == recipe.phases.focal),
      eligibleQueries = counts.eligibleQueries.toInt,
      candidatePairsPerScale = preview.matched.candidatePairCount,
      pairRowsPerScale = counts.pairRowsPerScale,
      pairRows = counts.totalPairs
    )
  )

  /** A scale's label as the recipe states it: `0.5°`, `1°`. */
  private object Degrees:
    def label(value: Double): String =
      (if value == math.rint(value) && math.abs(value) < 1e15 then value.toLong.toString
       else value.toString) + "°"

/** An immutable prepared plan/input whose exact schedule counts are not yet known. */
final class RealConfigured private[real] (
    val revision: AnalysisRevision,
    val dataset: DatasetRevision,
    val recipe: Recipe,
    val plan: RealPlan.Plan,
    val method: ComparisonMethod,
    val admitted: AdmittedDataset,
    val work: RealPlan.Work,
    val preview: StudyPreview[CoreKey, Unit2D.Px]
):
  def plans  = StudyCodecs.trialSimilarity[Unit2D.Px](method)
  def inputs = StudyInputCodecs.trial[Unit2D.Px]

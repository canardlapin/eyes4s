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
import eyes4s.plan.{StudyCounts, StudyPreview, TrialKey as CoreKey}
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
    val plan: RealPlan.Plan,
    val work: RealPlan.Work,
    val preview: StudyPreview[CoreKey, Unit2D.Px],
    val counts: StudyCounts[CoreKey],
    val summary: PreviewSummary
)

object RealPrepared:

  /** Configure and prepare `recipe` over `admitted`; every count is eyes4s's. */
  def of(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      recipe: Recipe,
      admitted: AdmittedDataset
  ): Either[BackendError, RealPrepared] =
    val refused = BackendError.Unavailable(DiagnosticLocus.Revision(revision))
    for
      plan    <- RealPlan.plan(revision, recipe, admitted.screen, admitted.input)
      work    <- plan.prepare(admitted.input).leftMap(_ => refused)
      counts  <- work.counts.leftMap(_ => refused)
      preview <- work.preview(counts).leftMap(_ => refused)
    yield
      val focal = recipe.phases.focal
      new RealPrepared(
        revision,
        dataset,
        plan,
        work,
        preview,
        counts,
        PreviewSummary(
          revision,
          dataset,
          recipe.scales.values.map(s => Degrees.label(s.degrees)),
          focalTrials = preview.focalKeys.size,
          referenceTrials = preview.referenceKeys.size,
          // Every inventory trial of the focal phase is a requested query,
          // admitted or not.
          requestedQueries = admitted.ledger.count(_.trial.phase == focal),
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

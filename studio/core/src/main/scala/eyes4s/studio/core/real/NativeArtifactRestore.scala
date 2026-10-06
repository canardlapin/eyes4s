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

import eyes4s.studio.core.artifacts.NativeArtifactPackage
import eyes4s.studio.core.backend.{BackendError, DiagnosticLocus}
import eyes4s.studio.core.document.*

/** Materialize verified stored science against the current immutable document
  * scope. No source port or study runner is involved (bead q-native-archive-readback).
  */
object NativeArtifactRestore:
  def of(
      artifacts: NativeArtifactPackage,
      ref: RunRef,
      revision: AnalysisRevisionSpec,
      dataset: DatasetRevisionSpec
  ): Either[BackendError, RealStudyBackend.RealRun] =
    val facts = artifacts.facts
    def require(field: String, expected: String, found: String, holds: Boolean) =
      Either.cond(
        holds,
        (),
        BackendError.RegistryRefused(
          DiagnosticLocus.Run(ref.id),
          s"Native archive $field expected $expected, found $found."
        )
      )
    for
      _ <- require("run", ref.id.label, facts.run.label, ref.id == facts.run)
      _ <- require(
        "lifecycle",
        RunLifecycle.Completed.toString,
        ref.state.toString,
        ref.state == RunLifecycle.Completed
      )
      _ <- require(
        "analysis",
        ref.analysis.label,
        s"${revision.id.label}/${facts.revision.label}",
        ref.analysis == revision.id && ref.analysis == facts.revision
      )
      _ <- require(
        "dataset",
        ref.dataset.label,
        s"${dataset.id.label}/${revision.dataset.label}/${facts.dataset.label}",
        ref.dataset == dataset.id && ref.dataset == revision.dataset && ref.dataset == facts.dataset
      )
      _ <- require(
        "plan binding",
        facts.stamp.plan.render,
        revision.plan.render,
        revision.plan == facts.stamp.plan
      )
      _ <- require(
        "result binding",
        facts.result.display,
        ref.archive.render,
        ref.archive == CoreBinding.Bound(facts.result)
      )
      _ <- require(
        "recipe",
        facts.recipeSnapshot.copy(input = None).toString,
        revision.recipe.copy(input = None).toString,
        revision.recipe.copy(input = None) == facts.recipeSnapshot.copy(input = None)
      )
      _ <- require(
        "semantic source",
        facts.source.value,
        revision.recipe.input.fold("unbound")(_.value),
        revision.recipe.input.contains(facts.source)
      )
      snapshot = artifacts.checked
      context  <- RealAdmission.archived(dataset, snapshot.input, snapshot.ledger)
      prepared <- RealPrepared.archived(
        revision.id,
        dataset.id,
        revision.recipe,
        snapshot.plan,
        snapshot.method,
        context
      )
    yield RealStudyBackend.RealRun(
      prepared,
      snapshot.result,
      RealStudyBackend.RunOrigin.Restored(artifacts.manifestAddress)
    )

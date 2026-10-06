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

package eyes4s.studio.core.execution

import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision}
import eyes4s.studio.core.document.{CoreBinding, StudioDocument, StudyPlanArtifact}
import io.circe.Codec

/** Marker for the eyes4s `StudyInput` a run reads: the admitted trials whose
  * digest the DESIGN_SPEC shows as "input digest sha256:…".
  */
sealed trait StudyInputArtifact

/** What a run was asked to compute (UI-D `RunStamp`, with studio's revision
  * names): an analysis revision on a dataset revision, the digest of the plan
  * that revision binds and the digest of the study input.
  *
  * A stamp is compared whole. Two stamps whose digests are both `Unbound` (on
  * `FakeStudyBackend`, before eyes4s has produced the artifacts) are told
  * apart only by their revisions; a digest is never invented to fill the gap.
  */
final case class RunStamp(
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    plan: CoreBinding[StudyPlanArtifact],
    input: CoreBinding[StudyInputArtifact]
) derives CanEqual,
      Codec.AsObject:
  def label: String = s"${revision.label} · data ${dataset.label}"

  /** Check every declared identity, allowing a backend to bind an artifact
    * that a document has not produced yet. The caller must also compare the
    * exact recipe snapshot before accepting newly bound preview identities.
    */
  def agreesWithDeclarations(expected: RunStamp): Boolean =
    def binding[A](actual: CoreBinding[A], declared: CoreBinding[A]): Boolean = declared match
      case CoreBinding.Unbound() => true
      case CoreBinding.Bound(_)  => actual == declared
    revision == expected.revision && dataset == expected.dataset &&
    binding(plan, expected.plan) && binding(input, expected.input)

object RunStamp:

  /** The stamp of `revision` as `document` saves it: its dataset revision and
    * plan binding, with the study input's digest as the backend reports it.
    */
  def of(
      document: StudioDocument,
      revision: AnalysisRevision,
      input: CoreBinding[StudyInputArtifact]
  ): Either[ExecutionError, RunStamp] =
    document
      .analysis(revision)
      .map(spec => RunStamp(spec.id, spec.dataset, spec.plan, input))
      .toRight(ExecutionError.UnsavedRevision(revision, document.analyses.map(_.id)))

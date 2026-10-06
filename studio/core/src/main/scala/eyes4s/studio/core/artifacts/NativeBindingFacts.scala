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

package eyes4s.studio.core.artifacts

import eyes4s.codec.{CanonicalDigest, CodecError, VersionedCodec}
import eyes4s.plan.DefinitionId
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DigestJson.given
import eyes4s.studio.core.execution.{RunStamp, StudyInputArtifact}
import io.circe.{Decoder, Encoder}

/** Canonical values and semantic source identity are distinct from stored byte SHA.
  * A job id is session-local and has no place in these persisted facts.
  */
final case class NativeBindingFacts private (
    run: RunId,
    stamp: RunStamp,
    source: SemanticIdentity,
    result: CanonicalDigest[ResultArchiveArtifact],
    recipeSnapshot: Recipe,
    private val checkedPlan: CanonicalDigest[StudyPlanArtifact],
    private val checkedInput: CanonicalDigest[StudyInputArtifact]
) derives CanEqual:
  def revision       = stamp.revision
  def dataset        = stamp.dataset
  def planCanonical  = checkedPlan
  def inputCanonical = checkedInput

object NativeBindingFacts:
  def of(
      run: RunId,
      stamp: RunStamp,
      source: SemanticIdentity,
      result: CanonicalDigest[ResultArchiveArtifact],
      recipeSnapshot: Recipe
  ): Either[NativeArtifactError, NativeBindingFacts] =
    (stamp.plan, stamp.input) match
      case (CoreBinding.Bound(plan), CoreBinding.Bound(input)) =>
        if run.number <= 0 || stamp.revision.number <= 0 || stamp.dataset.number <= 0 then
          Left(
            NativeArtifactError.InvalidFacts(
              run,
              "identity",
              s"Expected positive run/revision/dataset, received ${stamp.label}."
            )
          )
        else if recipeSnapshot.input.exists(_ != source) then
          Left(
            NativeArtifactError.InvalidFacts(
              run,
              "semantic source",
              s"Recipe input ${recipeSnapshot.input.fold("unbound")(_.value)} differs from parsed source-record identity ${source.value}."
            )
          )
        else
          Right(new NativeBindingFacts(run, stamp, source, result, recipeSnapshot, plan, input))
      case _ =>
        Left(
          NativeArtifactError.InvalidFacts(
            run,
            "stamp",
            s"Canonical plan and input must be bound: ${stamp.label}."
          )
        )

  given Encoder.AsObject[NativeBindingFacts] =
    Encoder.forProduct5("run", "stamp", "source", "result", "recipeSnapshot")(f =>
      (f.run, f.stamp, f.source, f.result, f.recipeSnapshot)
    )
  given Decoder[NativeBindingFacts] = Decoder
    .forProduct5("run", "stamp", "source", "result", "recipeSnapshot")(of)
    .emap(_.left.map(_.message))
  val codec: Either[CodecError, VersionedCodec[NativeBindingFacts]] =
    DefinitionId.of("studio.native-binding-facts", 1).left.map(CodecError.Definition(_)).map {
      schema =>
        VersionedCodec.checked[NativeBindingFacts](schema)(f =>
          Right(CanonicalJson(summon[Encoder[NativeBindingFacts]].apply(f)))
        )(json =>
          json
            .as[NativeBindingFacts]
            .left
            .map(e => CodecError.Field("native artifact facts", json, e.message))
        )
    }

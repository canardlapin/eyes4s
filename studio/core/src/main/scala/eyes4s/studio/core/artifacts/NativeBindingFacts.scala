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

import cats.syntax.all.*
import eyes4s.codec.{CanonicalDigest, CodecError, SchemaLadder, VersionedCodec}
import eyes4s.plan.DefinitionId
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DigestJson.given
import eyes4s.studio.core.execution.{RunStamp, StudyInputArtifact}
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/** Canonical values and semantic source identity are distinct from stored byte SHA.
  * A job id is session-local and has no place in these persisted facts.
  */
final case class NativeBindingFacts private (
    run: RunId,
    stamp: RunStamp,
    source: SemanticIdentity,
    result: CanonicalDigest[ResultArchiveArtifact],
    recipeSnapshot: Recipe,
    datasetDefinition: Option[NativeDatasetDefinition],
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
      recipeSnapshot: Recipe,
      datasetDefinition: Option[NativeDatasetDefinition] = None
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
          Right(
            new NativeBindingFacts(
              run,
              stamp,
              source,
              result,
              recipeSnapshot,
              datasetDefinition,
              plan,
              input
            )
          )
      case _ =>
        Left(
          NativeArtifactError.InvalidFacts(
            run,
            "stamp",
            s"Canonical plan and input must be bound: ${stamp.label}."
          )
        )

  private val legacyEncoder: Encoder.AsObject[NativeBindingFacts] =
    Encoder.forProduct5("run", "stamp", "source", "result", "recipeSnapshot")(f =>
      (f.run, f.stamp, f.source, f.result, f.recipeSnapshot)
    )
  given Encoder.AsObject[NativeBindingFacts] = Encoder.AsObject.instance { f =>
    val base = legacyEncoder.encodeObject(f)
    f.datasetDefinition.fold(base)(d =>
      base.add("datasetDefinition", summon[Encoder[NativeDatasetDefinition]].apply(d))
    )
  }
  given Decoder[NativeBindingFacts] = Decoder.instance { c =>
    for
      run        <- c.get[RunId]("run")
      stamp      <- c.get[RunStamp]("stamp")
      source     <- c.get[SemanticIdentity]("source")
      result     <- c.get[CanonicalDigest[ResultArchiveArtifact]]("result")
      recipe     <- c.get[Recipe]("recipeSnapshot")
      definition <- c.downField("datasetDefinition").focus match
        case None                        => Right(None)
        case Some(value) if value.isNull =>
          Left(
            DecodingFailure(
              "Bare binding facts omit absent datasetDefinition; null is not a definition.",
              c.history
            )
          )
        case Some(value) => value.as[NativeDatasetDefinition].map(Some(_))
      facts <- of(run, stamp, source, result, recipe, definition)
        .leftMap(e => DecodingFailure(e.message, c.history))
    yield facts
  }

  private def read(json: Json): Either[CodecError, NativeBindingFacts] =
    json
      .as[NativeBindingFacts]
      .leftMap(e => CodecError.Field("native artifact facts", json, e.message))
  private val requiresV2 = CodecError.Unsupported(
    "native artifact facts",
    "a complete dataset definition needs version 2"
  )

  /** V1 retains its original bytes and absence of dataset evidence. Lifting
    * explicitly carries that absence; it cannot manufacture a definition.
    */
  val ladder: Either[CodecError, SchemaLadder[NativeBindingFacts]] =
    DefinitionId.of("studio.native-binding-facts", 1).leftMap(CodecError.Definition(_)).map {
      schema =>
        SchemaLadder
          .of[NativeBindingFacts]("native artifact facts", schema)(f =>
            Either
              .cond(f.datasetDefinition.isEmpty, CanonicalJson(legacyEncoder(f)), requiresV2)
          )(json =>
            if json.hcursor.downField("datasetDefinition").succeeded then Left(requiresV2)
            else read(json)
          )
          .next(
            _.datasetDefinition.isEmpty,
            _.mapObject(_.add("datasetDefinition", Json.Null))
          )(f =>
            Right(CanonicalJson(summon[Encoder[NativeBindingFacts]].apply(f).mapObject { o =>
              if f.datasetDefinition.isEmpty then o.add("datasetDefinition", Json.Null) else o
            }))
          )(json =>
            if json.hcursor.downField("datasetDefinition").focus.contains(Json.Null) then
              read(json.mapObject(_.remove("datasetDefinition")))
            else if json.hcursor.downField("datasetDefinition").succeeded then read(json)
            else
              Left(
                CodecError.Field(
                  "datasetDefinition",
                  json,
                  "Version 2 requires explicit dataset evidence or legacy null."
                )
              )
          )
    }
  val codec: Either[CodecError, VersionedCodec[NativeBindingFacts]] = ladder.map(_.codec)

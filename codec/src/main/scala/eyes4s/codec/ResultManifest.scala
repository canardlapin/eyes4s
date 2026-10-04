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

package eyes4s.codec

import cats.syntax.all.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** A packed study result and its manifest-owned density chunks. */
final case class ResultArtifacts(
    result: StoredArtifact,
    payloads: Vector[StoredArtifact],
    relations: Vector[ManifestRelation]
)

/** Builders of the manifest artifacts for a study result: the result document, its
  * density payloads and the `ResultPayloadOf` relations joining them.
  */
object ResultManifest:
  /** Pack densities with the established participant-chunk policy. Names use
    * deterministic indices, so participant labels never become storage paths.
    * Add the ordinary ResultOf relation when joining the saved plan and input.
    */
  def packed[K, U <: Unit2D: UnitLabel, P, S, D](
      name: String,
      persistence: StudyResultCodec[K, U, P, S, D],
      value: StudyResult[K, U, S, D]
  ): Either[CodecError, ResultArtifacts] =
    val codec = new DensityArchiveCodec(persistence)
    for
      bundle   <- codec.encode(value, DensityStorage.Packed)
      json     <- codec.codec.encode(bundle.archive)
      result   <- StoredArtifact.document(name, ArtifactRole.StudyResult, json, None)
      payloads <- bundle.chunks.zipWithIndex.traverse { case ((_, chunk), index) =>
        StoredArtifact.resultPayload(s"$name.payload.$index", chunk)
      }
    yield ResultArtifacts(
      result,
      payloads,
      payloads.map(p => ManifestRelation.ResultPayloadOf(result.name, p.name))
    )

  /** Persist a completion-bound result and its canonical run claim. */
  def stamped[K, U <: Unit2D: UnitLabel, P, S, D](
      name: String,
      persistence: StudyResultCodec[K, U, P, S, D],
      value: StampedStudyResult[K, U, P, S, D],
      storage: DensityStorage = DensityStorage.Packed
  ): Either[CodecError, ResultArtifacts] =
    val codec = new DensityArchiveCodec(persistence)
    for
      bundle   <- codec.encodeStamped(value, storage)
      json     <- codec.codec.encode(bundle.archive)
      result   <- StoredArtifact.document(name, ArtifactRole.StudyResult, json, None)
      payloads <- bundle.chunks.zipWithIndex.traverse { case ((_, chunk), index) =>
        StoredArtifact.resultPayload(s"$name.payload.$index", chunk)
      }
    yield ResultArtifacts(
      result,
      payloads,
      payloads.map(p => ManifestRelation.ResultPayloadOf(result.name, p.name))
    )

  /** References used by the archive, not merely edges asserted by a caller.
    * Full geometry and completed-result validation still belongs to its codec.
    */
  private[codec] def references(document: Json): Either[CodecError, Vector[PayloadRef]] = for
    schema <- Wire.definition(document, "schema")
    _      <- Either.cond(
      schema == DensityArchiveDefinitions.studyResultV2,
      (),
      CodecError.UnsupportedSchema(
        "result-payload",
        schema,
        Vector(DensityArchiveDefinitions.studyResultV2)
      )
    )
    value      <- Wire.field[Json](document, "value")
    scales     <- Wire.field[Vector[Json]](value, "scales")
    references <- scales.traverse { scale =>
      Wire
        .field[Vector[Json]](scale, "estimation")
        .flatMap(_.traverse { row =>
          for
            outcome <- Wire.field[Json](row, "outcome")
            kind    <- Wire.field[String](outcome, "kind")
            refs    <- kind match
              case "failure" => Right(Vector.empty)
              case "mass"    =>
                for
                  storage <- Wire.field[Json](outcome, "storage")
                  mode    <- Wire.field[String](storage, "kind")
                  refs    <- mode match
                    case "packed" =>
                      Wire.field[Json](storage, "chunk").flatMap(PayloadRef.read).map(Vector(_))
                    case "inline" | "recomputable" => Right(Vector.empty)
                    case other                     =>
                      Left(CodecError.Field("storage.kind", storage, s"unknown $other"))
                yield refs
              case other =>
                Left(CodecError.Field("kind", outcome, s"unknown density outcome $other"))
          yield refs
        }.map(_.flatten))
    }
  yield references.flatten.distinct

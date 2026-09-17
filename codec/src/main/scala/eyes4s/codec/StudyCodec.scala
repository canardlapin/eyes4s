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
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import eyes4s.surface.*
import io.circe.Json

/** Codecs for the concrete geometry needed by a fixation study. */
object StudyCodecs:
  def cosine[U <: Unit2D](using
      UnitLabel[U]
  ): StudyCodec[StudyKey, U, Unit, Similarity, SignedDifference] =
    new StudyCodec(
      DefinitionId.study,
      StudyKey.layout(DefinitionId.studyLayout),
      key(DefinitionId.studyKey),
      StudyMethod.cosine[U](DefinitionId.cosine),
      VersionedCodec.unit(DefinitionId.unit)
    )

  def key(schema: DefinitionId): VersionedCodec[StudyKey] =
    VersionedCodec.of[StudyKey](schema)(k =>
      Json.obj(
        "participant" -> Json.fromString(k.participant),
        "stimulus"    -> Json.fromString(k.stimulus),
        "phase"       -> Json.fromString(k.phase)
      )
    ) { json =>
      for
        participant <- Wire.field[String](json, "participant")
        stimulus    <- Wire.field[String](json, "stimulus")
        phase       <- Wire.field[String](json, "phase")
      yield StudyKey(participant, stimulus, phase)
    }

  private[codec] def frame[U <: Unit2D: UnitLabel](f: Frame[U]): Json = DomainWire.frame(f)

  private[codec] def readFrame[U <: Unit2D: UnitLabel](
      json: Json
  ): Either[CodecError, Frame[U]] =
    DomainWire.readFrame[U](json)

/** Persistence registration captures the typed key and parameter codecs with one method.
  * Plan has no circe dependency. Runtime lookup selects this already typed closure;
  * no Any-valued parameter registry or cast is involved (bead bd-01KYDZ7GGMYKTXYWNRR40Q8X9M).
  */
final class StudyCodec[K, U <: Unit2D, P, S, D](
    val schema: DefinitionId,
    val layout: StudyLayout[K],
    val keys: VersionedCodec[K],
    val method: StudyMethod[P, U, S, D],
    val parameters: VersionedCodec[P]
)(using unit: UnitLabel[U]):
  val codec: VersionedCodec[StudyPlan[K, U, P, S, D]] =
    VersionedCodec.checked(schema)(write)(read)

  private def write(plan: StudyPlan[K, U, P, S, D]): Either[CodecError, Json] = for
    _ <- Either.cond(
      plan.layout.id == layout.id,
      (),
      CodecError.Schema(layout.id, plan.layout.id)
    )
    _ <- Either.cond(
      plan.method.id == method.id,
      (),
      CodecError.Schema(method.id, plan.method.id)
    )
    encodedParameters <- parameters.encode(plan.parameters)
  yield Json.obj(
    "layout"         -> Wire.id(plan.layout.id),
    "keySchema"      -> Wire.id(keys.schema),
    "method"         -> Wire.id(plan.method.id),
    "input"          -> Json.fromString(plan.input.digest),
    "frame"          -> StudyCodecs.frame(plan.grid.frame),
    "gridId"         -> Json.fromString(plan.grid.id.name),
    "nx"             -> Json.fromInt(plan.grid.nx),
    "ny"             -> Json.fromInt(plan.grid.ny),
    "focalPhase"     -> Json.fromString(plan.focalPhase),
    "referencePhase" -> Json.fromString(plan.referencePhase),
    "weight"         -> Json.fromString(plan.weight.toString),
    "policy"         -> (plan.policy match
      case FailurePolicy.RequireAll => Json.obj("kind" -> Json.fromString("requireAll"))
      case FailurePolicy.SuccessfulOnly(minimum) =>
        Json.obj(
          "kind"    -> Json.fromString("successfulOnly"),
          "minimum" -> Json.fromInt(minimum.value)
        )),
    "estimates" -> Json.arr(plan.estimates.map {
      case StudyEstimate.Binned()               => Json.obj("kind" -> Json.fromString("binned"))
      case StudyEstimate.Gaussian(sigma, edges) =>
        Json.obj(
          "kind"  -> Json.fromString("gaussian"),
          "sigma" -> Json.fromDoubleOrNull(sigma.value),
          "edges" -> Json.fromString(edges.toString)
        )
    }*),
    "parameters" -> encodedParameters
  )

  private def requireId(
      json: Json,
      field: String,
      expected: DefinitionId
  ): Either[CodecError, Unit] =
    Wire
      .definition(json, field)
      .flatMap(found => Either.cond(found == expected, (), CodecError.Schema(expected, found)))

  private def read(json: Json): Either[CodecError, StudyPlan[K, U, P, S, D]] = for
    _      <- requireId(json, "layout", layout.id)
    _      <- requireId(json, "keySchema", keys.schema)
    _      <- requireId(json, "method", method.id)
    digest <- Wire.field[String](json, "input")
    input  <- ArtifactRef.parse[StudyInput[K, U]](digest).left.map(CodecError.Definition.apply)
    frameJson <- Wire.field[Json](json, "frame")
    frame     <- StudyCodecs.readFrame[U](frameJson)
    gridName  <- Wire.field[String](json, "gridId")
    nx        <- Wire.field[Int](json, "nx")
    ny        <- Wire.field[Int](json, "ny")
    grid      <- Grid
      .of(GridId(gridName), frame, nx, ny)
      .left
      .map(e => CodecError.Field("grid", json, e.message))
    focal      <- Wire.field[String](json, "focalPhase")
    reference  <- Wire.field[String](json, "referencePhase")
    weightName <- Wire.field[String](json, "weight")
    weight     <- Weight.values
      .find(_.toString == weightName)
      .toRight(CodecError.Field("weight", json, s"unknown weight $weightName"))
    policyJson <- Wire.field[Json](json, "policy")
    kind       <- Wire.field[String](policyJson, "kind")
    policy     <- kind match
      case "requireAll"     => Right(FailurePolicy.RequireAll)
      case "successfulOnly" =>
        Wire
          .field[Int](policyJson, "minimum")
          .flatMap(n =>
            FailurePolicy
              .successfulOnly(n)
              .left
              .map(e => CodecError.Field("minimum", policyJson, e.message))
          )
      case other => Left(CodecError.Field("policy", policyJson, s"unknown policy $other"))
    estimateJson  <- Wire.field[Vector[Json]](json, "estimates")
    estimates     <- estimateJson.traverse(readEstimate)
    parameterJson <- Wire.field[Json](json, "parameters")
    params        <- parameters.decode(parameterJson)
    plan          <- StudyPlan
      .of(input, layout, grid, focal, reference, weight, estimates, policy, method, params)
      .left
      .map(CodecError.Definition.apply)
  yield plan

  private def readEstimate(json: Json): Either[CodecError, StudyEstimate[U]] =
    Wire.field[String](json, "kind").flatMap {
      case "binned"   => Right(StudyEstimate.Binned())
      case "gaussian" =>
        for
          value <- Wire.field[Double](json, "sigma")
          sigma <- Sigma.of[U](value).left.map(e => CodecError.Field("sigma", json, e.message))
          edgesName <- Wire.field[String](json, "edges")
          edges     <- EdgePolicy.values
            .find(_.toString == edgesName)
            .toRight(CodecError.Field("edges", json, s"unknown edge policy $edgesName"))
        yield StudyEstimate.Gaussian(sigma, edges)
      case other => Left(CodecError.Field("estimate", json, s"unknown estimator $other"))
    }

  def registration: StudyRegistration[K, U] =
    new StudyRegistration[K, U]:
      val id                                                        = method.id
      def decode(json: Json): Either[CodecError, LoadedStudy[K, U]] =
        codec.decode(json).map { value =>
          new LoadedStudy[K, U]:
            type Score      = S
            type Difference = D
            def description                                    = value.description
            def encode                                         = codec.encode(value)
            def prerequisites(input: Option[StudyInput[K, U]]) = value.prerequisites(input)
            def run(input: StudyInput[K, U])                   = value.run(input)
        }

/** Existential method output remains typed inside this value after runtime lookup. */
trait LoadedStudy[K, U <: Unit2D]:
  type Score
  type Difference
  def description: Vector[(String, Vector[Provenance.Param])]
  def encode: Either[CodecError, Json]
  def prerequisites(input: Option[StudyInput[K, U]]): Vector[PlanError]
  def run(input: StudyInput[K, U]): Either[PlanError, StudyResult[K, U, Score, Difference]]

sealed trait StudyRegistration[K, U <: Unit2D]:
  def id: DefinitionId
  def decode(json: Json): Either[CodecError, LoadedStudy[K, U]]

final class StudyRegistry[K, U <: Unit2D] private (
    val entries: Vector[StudyRegistration[K, U]]
):
  def register(entry: StudyRegistration[K, U]): Either[CodecError, StudyRegistry[K, U]] =
    if entries.exists(_.id == entry.id) then Left(CodecError.DuplicateMethod(entry.id))
    else Right(new StudyRegistry(entries :+ entry))
  def decode(json: Json): Either[CodecError, LoadedStudy[K, U]] = for
    payload    <- Wire.field[Json](json, "value")
    method     <- Wire.definition(payload, "method")
    registered <- entries.find(_.id == method).toRight(CodecError.MissingMethod(method))
    result     <- registered.decode(json)
  yield result
object StudyRegistry:
  def empty[K, U <: Unit2D]: StudyRegistry[K, U] = new StudyRegistry(Vector.empty)

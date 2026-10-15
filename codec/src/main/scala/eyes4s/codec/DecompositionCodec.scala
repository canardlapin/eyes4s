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
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

object DecompositionCodecDefinitions:
  val plan: DefinitionId   = DefinitionId.builtIn("eyes4s.decomposition-plan", 1)
  val result: DefinitionId = DefinitionId.builtIn("eyes4s.decomposition-result", 1)

final class DecompositionArchive[U <: Unit2D] private (
    val plan: DecompositionPlan[U],
    val result: DecompositionResult[U]
)
object DecompositionArchive:
  def run[U <: Unit2D](
      plan: DecompositionPlan[U]
  ): Either[DecompositionError, DecompositionArchive[U]] =
    plan.run.map(new DecompositionArchive(plan, _))
  def completed[U <: Unit2D](
      plan: DecompositionPlan[U],
      result: DecompositionResult[U]
  ): Either[CodecError, DecompositionArchive[U]] =
    val kind = (plan.spec, result.fit) match
      case (DecompositionSpec.Ols(_, _), DecompositionFit.Ols(_))                 => true
      case (DecompositionSpec.NonNegative(_, _), DecompositionFit.NonNegative(_)) => true
      case (DecompositionSpec.Mixture(_, _, _), DecompositionFit.Mixture(_))      => true
      case _                                                                      => false
    Either.cond(
      kind && result.inputHash == plan.inputHash && result.planHash == plan.planHash,
      new DecompositionArchive(plan, result),
      CodecError.Field(
        "decompositionResult",
        Json.fromString(result.planHash.render),
        s"Expected input=${plan.inputHash.render}, plan=${plan.planHash.render}, method=${plan.spec.method}; found input=${result.inputHash.render}, plan=${result.planHash.render}, or a different fit kind."
      )
    )

/** Complete response/predictor inputs, method-specific settings and stamped
  * replay-checked results. Each solve remains one whole synchronous operation.
  */
final class DecompositionCodec[U <: Unit2D: UnitLabel]:
  private def mass(value: Mass[U]): Json = Json.obj(
    "values"     -> Json.arr(value.values.toVector.map(ResultWire.double)*),
    "provenance" -> ResultWire.provenance(value.provenance)
  )
  private def readMass(json: Json, grid: Grid[U]): Either[CodecError, Mass[U]] = for
    values     <- Wire.field[Vector[Double]](json, "values")
    provenance <- Wire.field[Json](json, "provenance").flatMap(ResultWire.readProvenance)
    mass       <- Surface
      .mass(grid, IArray.from(values), provenance)
      .left
      .map(e => CodecError.Field("mass", json, e.message))
  yield mass
  private def spec(value: DecompositionSpec): Json = value match
    case DecompositionSpec.Ols(i, r) =>
      ResultWire.tagged(
        "ols",
        "intercept"     -> Json.fromString(i.toString),
        "rankTolerance" -> ResultWire.double(r.value)
      )
    case DecompositionSpec.NonNegative(r, d) =>
      ResultWire.tagged(
        "nnls",
        "rankTolerance" -> ResultWire.double(r.value),
        "dualTolerance" -> ResultWire.double(d.value)
      )
    case DecompositionSpec.Mixture(i, r, d) =>
      ResultWire.tagged(
        "mixture",
        "intercept"     -> Json.fromString(i.toString),
        "rankTolerance" -> ResultWire.double(r.value),
        "dualTolerance" -> ResultWire.double(d.value)
      )
  private def readSpec(json: Json): Either[CodecError, DecompositionSpec] =
    def rank = Wire
      .field[Double](json, "rankTolerance")
      .flatMap(v =>
        RelativeRankTolerance
          .of(v)
          .left
          .map(e => CodecError.Field("rankTolerance", json, e.message))
      )
    def dual = Wire
      .field[Double](json, "dualTolerance")
      .flatMap(v =>
        RelativeDualTolerance
          .of(v)
          .left
          .map(e => CodecError.Field("dualTolerance", json, e.message))
      )
    def intercept = Wire
      .field[String](json, "intercept")
      .flatMap(name =>
        Intercept.values
          .find(_.toString == name)
          .toRight(CodecError.Field("intercept", json, s"Unknown intercept=$name"))
      )
    def fields(expected: Set[String]): Either[CodecError, Unit] =
      val found = json.asObject.fold(Set.empty[String])(_.keys.toSet)
      Either.cond(
        found == expected,
        (),
        CodecError.Field(
          "spec",
          json,
          s"Expected fields=${expected.toVector.sorted}; found=${found.toVector.sorted}"
        )
      )
    ResultWire.kind(json).flatMap {
      case "ols" =>
        for _ <- fields(Set("kind", "intercept", "rankTolerance")); i <- intercept; r <- rank
        yield DecompositionSpec.Ols(i, r)
      case "nnls" =>
        for _ <- fields(Set("kind", "rankTolerance", "dualTolerance")); r <- rank; d <- dual
        yield DecompositionSpec.NonNegative(r, d)
      case "mixture" =>
        for
          _ <- fields(Set("kind", "intercept", "rankTolerance", "dualTolerance"));
          i <- intercept; r <- rank; d <- dual
        yield DecompositionSpec.Mixture(i, r, d)
      case other => Left(CodecError.Field("spec", json, s"Unknown decomposition method=$other"))
    }
  private def exact(json: Json, field: String, expected: String): Either[CodecError, Unit] =
    Wire
      .field[String](json, field)
      .flatMap(found =>
        Either.cond(
          found == expected,
          (),
          CodecError.Field(field, json, s"Expected $expected, found $found")
        )
      )
  val plans: VersionedCodec[DecompositionPlan[U]] =
    VersionedCodec.checked[DecompositionPlan[U]](DecompositionCodecDefinitions.plan) { plan =>
      val grid = plan.response.grid
      Right(
        Json.obj(
          "algorithmRevision" -> Json.fromInt(1),
          "normConvention"    -> Json.fromString(LeastSquares.normConvention.toString),
          "frame"             -> DomainWire.frame(grid.frame),
          "grid"              -> Json.obj(
            "id" -> Json.fromString(grid.id.name),
            "nx" -> Json.fromInt(grid.nx),
            "ny" -> Json.fromInt(grid.ny)
          ),
          "response"   -> mass(plan.response),
          "predictors" -> Json.arr(
            plan.predictors.entries.map((id, value) =>
              Json.obj("id" -> Json.fromString(id.value), "mass" -> mass(value))
            )*
          ),
          "spec"      -> spec(plan.spec),
          "inputHash" -> Json.fromString(plan.inputHash.render),
          "planHash"  -> Json.fromString(plan.planHash.render)
        )
      )
    } { json =>
      for
        revision <- Wire.field[Int](json, "algorithmRevision")
        _        <- Either.cond(
          revision == 1,
          (),
          CodecError.Field(
            "algorithmRevision",
            json,
            s"Expected algorithmRevision=1, found=$revision"
          )
        )
        _        <- exact(json, "normConvention", LeastSquares.normConvention.toString)
        frame    <- Wire.field[Json](json, "frame").flatMap(DomainWire.readFrame[U])
        grid     <- Wire.field[Json](json, "grid").flatMap(DomainWire.readGrid(_, frame))
        response <- Wire.field[Json](json, "response").flatMap(readMass(_, grid))
        entries  <- Wire
          .field[Vector[Json]](json, "predictors")
          .flatMap(_.traverse { entry =>
            for
              name <- Wire.field[String](entry, "id")
              id   <- PredictorId
                .of(name)
                .left
                .map(e => CodecError.Field("predictor", entry, e.message))
              value <- Wire.field[Json](entry, "mass").flatMap(readMass(_, grid))
            yield id -> value
          })
        predictors <- PredictorSet
          .of(entries)
          .left
          .map(e => CodecError.Field("predictors", json, e.message))
        specification <- Wire.field[Json](json, "spec").flatMap(readSpec)
        plan          <- DecompositionPlan
          .of(response, predictors, specification)
          .left
          .map(e => CodecError.Field("decompositionPlan", json, e.message))
        _ <- exact(json, "inputHash", plan.inputHash.render)
        _ <- exact(json, "planHash", plan.planHash.render)
      yield plan
    }
  def stamp(
      plan: DecompositionPlan[U]
  ): Either[CodecError, RunStamp[DecompositionPlan[U], DecompositionPlan[U]]] =
    RunStamp.of(plan, plan, plans, plans)
  private def components(values: Vector[(PredictorId, Double)]): Json = Json.arr(
    values.map((id, value) =>
      Json.obj("id" -> Json.fromString(id.value), "value" -> ResultWire.double(value))
    )*
  )
  private def cells(value: Surface[U]): Json =
    Json.arr(value.values.toVector.map(ResultWire.double)*)
  private def constrained(value: ConstrainedDiagnostics): Json = Json.obj(
    "rank"                -> Json.fromInt(value.rank),
    "active"              -> Json.fromInt(value.active),
    "iterations"          -> Json.fromInt(value.iterations),
    "dualViolation"       -> ResultWire.double(value.dualViolation),
    "residualSumSquares"  -> ResultWire.double(value.residualSumSquares),
    "rSquared"            -> value.rSquared.fold(Json.Null)(ResultWire.double),
    "rSquaredReference"   -> Json.fromString(value.rSquaredReference.toString),
    "scaledDiagonalRatio" -> ResultWire.double(value.scaledDiagonalRatio)
  )
  private[codec] def resultJson(result: DecompositionResult[U]): Json =
    val fitted = result.fit match
      case DecompositionFit.Ols(value) =>
        ResultWire.tagged(
          "ols",
          "coefficients" -> components(value.coefficients),
          "intercept"    -> value.intercept.fold(Json.Null)(ResultWire.double),
          "fitted"       -> cells(value.fitted),
          "residual"     -> cells(value.residual),
          "diagnostics"  -> Json.obj(
            "rank"               -> Json.fromInt(value.diagnostics.rank),
            "residualSumSquares" -> ResultWire.double(value.diagnostics.residualSumSquares),
            "rSquared" -> value.diagnostics.rSquared.fold(Json.Null)(ResultWire.double),
            "scaledDiagonalRatio" -> ResultWire.double(value.diagnostics.scaledDiagonalRatio)
          )
        )
      case DecompositionFit.NonNegative(value) =>
        ResultWire.tagged(
          "nnls",
          "coefficients" -> components(value.coefficients),
          "fitted"       -> cells(value.fitted),
          "residual"     -> cells(value.residual),
          "diagnostics"  -> constrained(value.diagnostics)
        )
      case DecompositionFit.Mixture(value) =>
        ResultWire.tagged(
          "mixture",
          "weights"     -> components(value.weights),
          "background"  -> value.background.fold(Json.Null)(ResultWire.double),
          "fitted"      -> cells(value.fitted),
          "residual"    -> cells(value.residual),
          "diagnostics" -> constrained(value.diagnostics)
        )
    Json.obj(
      "inputHash" -> Json.fromString(result.inputHash.render),
      "planHash"  -> Json.fromString(result.planHash.render),
      "fit"       -> fitted
    )
  val results: VersionedCodec[DecompositionArchive[U]] =
    VersionedCodec.checked[DecompositionArchive[U]](DecompositionCodecDefinitions.result) {
      archive =>
        for
          _     <- DecompositionArchive.completed(archive.plan, archive.result)
          claim <- stamp(archive.plan)
          plan  <- plans.encode(archive.plan)
        yield Json.obj(
          "runStamp" -> RunStampWire.write(claim),
          "plan"     -> plan,
          "result"   -> resultJson(archive.result)
        )
    } { json =>
      for
        claim <- Wire
          .field[Json](json, "runStamp")
          .flatMap(RunStampWire.read[DecompositionPlan[U], DecompositionPlan[U]])
          .left
          .map(Wire.at("runStamp"))
        plan <- Wire.field[Json](json, "plan").flatMap(plans.decode).left.map(Wire.at("plan"))
        current  <- stamp(plan)
        _        <- claim.check(current, Vector.empty).left.map(CodecError.Stamp(_))
        expected <- Wire.field[Json](json, "result")
        archive  <- DecompositionArchive
          .run(plan)
          .left
          .map(e => CodecError.Field("decompositionResult", json, e.message))
        actual = resultJson(archive.result)
        _ <- Either.cond(
          actual == expected,
          (),
          CodecError.Derived("decompositionResult", expected, actual)
        )
      yield archive
    }
  def registration: AnalysisRegistration = AnalysisRegistration.of(plans, results)(
    _.description,
    _.plan.description,
    archive => Vector(archive.plan.inputHash),
    embedded = Some(plan => Vector(plan.inputHash))
  )

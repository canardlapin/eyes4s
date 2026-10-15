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

/** Fixed and learned-map subroutes can coexist in one analysis registry. */
object TemplateFitCodecDefinitions:
  val recipeV1: DefinitionId      = DefinitionId.builtIn("eyes4s.template-recipe", 1)
  val recipeV2: DefinitionId      = DefinitionId.builtIn("eyes4s.template-recipe", 2)
  val fixedPlan: DefinitionId     = DefinitionId.builtIn("eyes4s.fixed-template-plan", 1)
  val fixedResult: DefinitionId   = DefinitionId.builtIn("eyes4s.fixed-template-result", 1)
  val meanMapPlan: DefinitionId   = DefinitionId.builtIn("eyes4s.mean-map-template-plan", 1)
  val meanMapResult: DefinitionId = DefinitionId.builtIn("eyes4s.mean-map-template-result", 1)

/** A completed fit with its complete source recipe and declared route. */
final class TemplateFitArchive[K, X] private (
    val plan: TemplateFitPlan[K, X],
    val result: TemplateFitResult[K, X]
):
  def validation: TemplateValidation[K, X] = result.validation
object TemplateFitArchive:
  def run[K, X](plan: TemplateFitPlan[K, X]): Either[TemplateError, TemplateFitArchive[K, X]] =
    plan.run.map(new TemplateFitArchive(plan, _))

  /** Identity checks do not refit. Archive reading independently refits and
    * verifies all persisted coefficients, features and predictions.
    */
  def completed[K, X](
      plan: TemplateFitPlan[K, X],
      result: TemplateFitResult[K, X]
  ): Either[CodecError, TemplateFitArchive[K, X]] =
    val split      = plan.split
    val validation = result.validation
    val fitted     = validation.fitted
    val evaluation = validation.evaluation
    val same       =
      result.inputHash == plan.inputHash && result.planHash == plan.planHash && fitted.training.hash == split.training.hash && evaluation.trainingHash == split.training.hash &&
        evaluation.heldOutHash == split.heldOut.hash && fitted.method == split.design.method &&
        fitted.training.rows.map(_.key) == split.training.rows.map(_.key) &&
        evaluation.rows.map(_.key) == split.heldOut.rows.map(_.key)
    Either.cond(
      same,
      new TemplateFitArchive(plan, result),
      CodecError.Field(
        "templateFitResult",
        Json.fromString(evaluation.trainingHash.render),
        s"Expected input=${plan.inputHash.render}, plan=${plan.planHash.render}, training=${split.training.hash.render}, heldOut=${split.heldOut.hash.render}, method=${split.design.method}; found input=${result.inputHash.render}, plan=${result.planHash.render}, training=${evaluation.trainingHash.render}, heldOut=${evaluation.heldOutHash.render}, method=${fitted.method}, or ordered row keys differ."
      )
    )

/** Complete saved recipe and replay-checked result, with a canonical stamp.
  * Learned maps are stored and rebuilt from the training recipe for verification;
  * neither coefficients nor a derived map alone is evidence of fitting.
  */
final class TemplateFitCodec[K: KeyDigest, X] private[codec] (
    val planSchema: DefinitionId,
    val resultSchema: DefinitionId,
    val keys: VersionedCodec[K],
    decodeImported: Option[
      (TemplateSplit[K, X], Json) => Either[CodecError, TemplateFitPlan[K, X]]
    ],
    templateOutput: X => Json
)(using TemplateInputWire[X]):
  val recipes: VersionedCodec[TemplateSplit[K, X]] =
    TemplateRecipeCodec.of(TemplateFitCodecDefinitions.recipeV1, keys)
  private def fitting(plan: TemplateFitPlan[K, X]): Json = plan.route match
    case TemplateFitRoute.Native() =>
      ResultWire.tagged(
        "native",
        "normConvention" -> Json.fromString(LeastSquares.normConvention.toString)
      )
    case TemplateFitRoute.ImportedFixed(value) =>
      ResultWire.tagged(
        "imported-fixed",
        "trainingHash" -> Json.fromString(value.trainingHash),
        "columns"      -> Json.arr(value.columns.map(Json.fromString)*),
        "coefficients" -> Json.arr(value.coefficients.map(ResultWire.double)*),
        "rank"         -> Json.fromInt(value.rank),
        "observations" -> Json.fromInt(value.observations),
        "backend"      -> Json.fromString(value.backend)
      )
  val plans: VersionedCodec[TemplateFitPlan[K, X]] =
    VersionedCodec.checked[TemplateFitPlan[K, X]](planSchema) { plan =>
      recipes
        .encode(plan.split)
        .map(recipe => Json.obj("recipe" -> recipe, "fitting" -> fitting(plan)))
    } { json =>
      for
        split   <- Wire.field[Json](json, "recipe").flatMap(recipes.decode)
        fitting <- Wire.field[Json](json, "fitting")
        kind    <- ResultWire.kind(fitting)
        plan    <- kind match
          case "native" =>
            for
              norm <- Wire.field[String](fitting, "normConvention")
              _    <- Either.cond(
                norm == LeastSquares.normConvention.toString,
                (),
                CodecError.Field(
                  "normConvention",
                  fitting,
                  s"Expected ${LeastSquares.normConvention}, found=$norm"
                )
              )
              plan <- TemplateFitPlan
                .of(split)
                .left
                .map(e => CodecError.Field("templateFitPlan", json, e.message))
            yield plan
          case "imported-fixed" =>
            decodeImported
              .toRight(
                CodecError.Field(
                  "fitting",
                  fitting,
                  "Imported fixed-feature receipts are unavailable for this input type"
                )
              )
              .flatMap(read => read(split, fitting))
          case other =>
            Left(CodecError.Field("fitting", fitting, s"Unknown fitting route=$other"))
      yield plan
    }
  def stamp(
      plan: TemplateFitPlan[K, X]
  ): Either[CodecError, RunStamp[TemplateFitPlan[K, X], TemplateFitPlan[K, X]]] =
    RunStamp.of(plan, plan, plans, plans)

  /** These are precisely the failures the sealed native designs can emit
    * during held-out prediction. Unexpected errors are refused explicitly.
    */
  private def predictionError(error: TemplateError): Either[CodecError, Json] = error match
    case TemplateError.Numerical(key, prediction, response) =>
      Right(
        ResultWire.tagged(
          "numerical",
          "key"        -> Json.fromString(key),
          "prediction" -> ResultWire.double(prediction),
          "response"   -> ResultWire.double(response)
        )
      )
    case TemplateError.Feature(key, underlying) =>
      Right(
        ResultWire.tagged(
          "feature",
          "key"   -> Json.fromString(key),
          "error" -> ResultWire.compareError(underlying)
        )
      )
    case TemplateError.Route(method, route) =>
      Right(
        ResultWire.tagged(
          "route",
          "method" -> Json.fromString(method),
          "route"  -> Json.fromString(route)
        )
      )
    case other =>
      Left(
        CodecError.Field(
          "templatePrediction",
          Json.fromString(other.message),
          s"Unsupported native prediction failure: ${other.message}"
        )
      )

  private[codec] def resultJson(result: TemplateFitResult[K, X]): Either[CodecError, Json] =
    val validation = result.validation
    for
      features <- validation.fitted.trainingFeatures.traverse { (key, values) =>
        keys
          .encode(key)
          .map(k =>
            Json.obj("key" -> k, "features" -> Json.arr(values.map(ResultWire.double)*))
          )
      }
      rows <- validation.evaluation.rows.traverse { row =>
        for
          key    <- keys.encode(row.key)
          result <- row.result match
            case Right((prediction, residual)) =>
              Right(
                ResultWire.tagged(
                  "success",
                  "prediction" -> ResultWire.double(prediction),
                  "residual"   -> ResultWire.double(residual)
                )
              )
            case Left(error) =>
              predictionError(error).map(e => ResultWire.tagged("failure", "error" -> e))
        yield Json.obj(
          "key"        -> key,
          "splitGroup" -> Json.fromString(row.splitGroup),
          "matchGroup" -> row.matchGroup.fold(Json.Null)(Json.fromString),
          "observed"   -> ResultWire.double(row.observed),
          "result"     -> result
        )
      }
    yield Json.obj(
      "inputHash"        -> Json.fromString(result.inputHash.render),
      "planHash"         -> Json.fromString(result.planHash.render),
      "method"           -> Json.fromString(validation.fitted.method),
      "backend"          -> Json.fromString(validation.fitted.backend),
      "trainingHash"     -> Json.fromString(validation.evaluation.trainingHash.render),
      "heldOutHash"      -> Json.fromString(validation.evaluation.heldOutHash.render),
      "coefficients"     -> Json.arr(validation.fitted.coefficients.map(ResultWire.double)*),
      "learnedTemplate"  -> validation.fitted.template.fold(Json.Null)(templateOutput),
      "trainingFeatures" -> Json.arr(features*),
      "predictions"      -> Json.arr(rows*)
    )

  val results: VersionedCodec[TemplateFitArchive[K, X]] =
    VersionedCodec.checked[TemplateFitArchive[K, X]](resultSchema) { archive =>
      for
        _      <- TemplateFitArchive.completed(archive.plan, archive.result)
        claim  <- stamp(archive.plan)
        plan   <- plans.encode(archive.plan)
        result <- resultJson(archive.result)
      yield Json.obj(
        "runStamp" -> RunStampWire.write(claim),
        "plan"     -> plan,
        "result"   -> result
      )
    } { json =>
      for
        claim <- Wire
          .field[Json](json, "runStamp")
          .flatMap(RunStampWire.read[TemplateFitPlan[K, X], TemplateFitPlan[K, X]])
          .left
          .map(Wire.at("runStamp"))
        plan <- Wire.field[Json](json, "plan").flatMap(plans.decode).left.map(Wire.at("plan"))
        current  <- stamp(plan)
        _        <- claim.check(current, Vector.empty).left.map(CodecError.Stamp(_))
        expected <- Wire.field[Json](json, "result")
        archive  <- TemplateFitArchive
          .run(plan)
          .left
          .map(e => CodecError.Field("templateFitResult", json, e.message))
        actual <- resultJson(archive.result)
        _      <- Either.cond(
          actual == expected,
          (),
          CodecError.Derived("templateFitResult", expected, actual)
        )
      yield archive
    }

  def registration: AnalysisRegistration = AnalysisRegistration.of(plans, results)(
    _.description,
    _.plan.description,
    archive => Vector(archive.plan.inputHash),
    embedded = Some(plan => Vector(plan.inputHash))
  )

object TemplateFitCodec:
  private def imported[K: KeyDigest](
      split: TemplateSplit[K, Vector[Double]],
      json: Json
  ): Either[CodecError, TemplateFitPlan[K, Vector[Double]]] = for
    hash         <- Wire.field[String](json, "trainingHash")
    columns      <- Wire.field[Vector[String]](json, "columns")
    coefficients <- Wire.field[Vector[Double]](json, "coefficients")
    rank         <- Wire.field[Int](json, "rank")
    observations <- Wire.field[Int](json, "observations")
    backend      <- Wire.field[String](json, "backend")
    receipt      <- TemplateImportedFit
      .of(split.training, hash, columns, coefficients, rank, observations, backend)
      .left
      .map(e => CodecError.Field("importedReceipt", json, e.message))
    plan <- TemplateFitPlan
      .imported(split, receipt)
      .left
      .map(e => CodecError.Field("importedReceipt", json, e.message))
  yield plan
  def fixed[K: KeyDigest](keys: VersionedCodec[K]): TemplateFitCodec[K, Vector[Double]] =
    new TemplateFitCodec[K, Vector[Double]](
      TemplateFitCodecDefinitions.fixedPlan,
      TemplateFitCodecDefinitions.fixedResult,
      keys,
      Some((split, json) => imported(split, json)),
      values => Json.arr(values.map(ResultWire.double)*)
    )
  def meanMap[K: KeyDigest, U <: Unit2D: UnitLabel](
      keys: VersionedCodec[K]
  ): TemplateFitCodec[K, Mass[U]] =
    new TemplateFitCodec[K, Mass[U]](
      TemplateFitCodecDefinitions.meanMapPlan,
      TemplateFitCodecDefinitions.meanMapResult,
      keys,
      None,
      mass =>
        Json.obj(
          "values"     -> Json.arr(mass.values.toVector.map(ResultWire.double)*),
          "provenance" -> ResultWire.provenance(mass.provenance)
        )
    )

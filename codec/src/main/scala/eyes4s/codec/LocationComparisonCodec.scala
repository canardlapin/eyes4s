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

import eyes4s.plan.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import io.circe.Json
import cats.syntax.all.*

/** Stable tagged wire forms for the location-only failures retained in an
  * archive. The enclosing archive decoder feeds these values to the plan's
  * checked cold reconstruction rather than rerunning the analysis.
  */
private[codec] object LocationComparisonCodec:
  private def trial(value: LocationTrialId): Json = Json.fromString(value.value)

  private def readTrial(json: Json, field: String): Either[CodecError, LocationTrialId] =
    Wire
      .field[String](json, field)
      .flatMap(value =>
        LocationTrialId
          .of(value)
          .left
          .map(error => CodecError.Field(field, json, error.message))
      )

  def mapError(error: LocationMapError): Json = error match
    case LocationMapError.Measure(trialId, underlying) =>
      ResultWire.tagged(
        "measure",
        "trial" -> trial(trialId),
        "error" -> ResultWire.surfaceError(underlying)
      )
    case LocationMapError.Binning(trialId, underlying) =>
      ResultWire.tagged(
        "binning",
        "trial" -> trial(trialId),
        "error" -> ResultWire.geometryError(underlying)
      )
    case LocationMapError.Surface(trialId, underlying) =>
      ResultWire.tagged(
        "surface",
        "trial" -> trial(trialId),
        "error" -> ResultWire.surfaceError(underlying)
      )

  def readMapError(json: Json): Either[CodecError, LocationMapError] =
    ResultWire.kind(json).flatMap {
      case "measure" =>
        for
          trialId <- readTrial(json, "trial")
          error   <- Wire.field[Json](json, "error").flatMap(ResultWire.readSurfaceError)
        yield LocationMapError.Measure(trialId, error)
      case "binning" =>
        for
          trialId <- readTrial(json, "trial")
          error   <- Wire.field[Json](json, "error").flatMap(ResultWire.readGeometryError)
        yield LocationMapError.Binning(trialId, error)
      case "surface" =>
        for
          trialId <- readTrial(json, "trial")
          error   <- Wire.field[Json](json, "error").flatMap(ResultWire.readSurfaceError)
        yield LocationMapError.Surface(trialId, error)
      case other => Left(ResultWire.unknown(json, "location map failure", other))
    }

  def runError(error: LocationRunError): Json = error match
    case LocationRunError.Map(query, role, trialId, underlying) =>
      ResultWire.tagged(
        "map",
        "query" -> Json.fromString(query),
        "role"  -> Json.fromString(role),
        "trial" -> trial(trialId),
        "error" -> mapError(underlying)
      )
    case LocationRunError.Comparison(query, role, focal, reference, underlying) =>
      ResultWire.tagged(
        "comparison",
        "query"     -> Json.fromString(query),
        "role"      -> Json.fromString(role),
        "focal"     -> trial(focal),
        "reference" -> trial(reference),
        "error"     -> ResultWire.compareError(underlying)
      )

  def readRunError(json: Json): Either[CodecError, LocationRunError] =
    ResultWire.kind(json).flatMap {
      case "map" =>
        for
          query   <- Wire.field[String](json, "query")
          role    <- Wire.field[String](json, "role")
          trialId <- readTrial(json, "trial")
          error   <- Wire.field[Json](json, "error").flatMap(readMapError)
        yield LocationRunError.Map(query, role, trialId, error)
      case "comparison" =>
        for
          query     <- Wire.field[String](json, "query")
          role      <- Wire.field[String](json, "role")
          focal     <- readTrial(json, "focal")
          reference <- readTrial(json, "reference")
          error     <- Wire.field[Json](json, "error").flatMap(ResultWire.readCompareError)
        yield LocationRunError.Comparison(query, role, focal, reference, error)
      case other => Left(ResultWire.unknown(json, "location run failure", other))
    }

/** Versioned checked locations, without timing, pairing or analysis parameters. */
object LocationInputCodec:
  def of(schema: DefinitionId): VersionedCodec[LocationInput] =
    VersionedCodec.checked[LocationInput](schema)(input =>
      Right(LocationInputWire.write(input))
    )(LocationInputWire.read)

private[codec] object LocationInputWire:
  def write(input: LocationInput): Json = Json.obj(
    "frame"  -> DomainWire.frame(input.frame),
    "trials" -> Json.arr(input.trials.map { t =>
      Json.obj(
        "id"           -> Json.fromString(t.id.value),
        "observations" -> Json.arr(t.observations.map { o =>
          Json.obj(
            "source"  -> Json.fromString(o.source.value),
            "ordinal" -> ResultWire.long(o.sourceOrdinal),
            "x"       -> Json.fromDoubleOrNull(o.position.x),
            "y"       -> Json.fromDoubleOrNull(o.position.y)
          )
        }*)
      )
    }*)
  )
  def read(json: Json): Either[CodecError, LocationInput] =
    for
      frame  <- Wire.field[Json](json, "frame").flatMap(DomainWire.readFrame[Px])
      trials <- Wire
        .field[Vector[Json]](json, "trials")
        .flatMap(_.traverse { t =>
          for
            trialId <- Wire
              .field[String](t, "id")
              .flatMap(v =>
                LocationTrialId.of(v).left.map(e => CodecError.Field("id", t, e.message))
              )
            observations <- Wire
              .field[Vector[Json]](t, "observations")
              .flatMap(_.traverse { o =>
                for
                  source <- Wire
                    .field[String](o, "source")
                    .flatMap(v =>
                      LocationSourceId
                        .of(v)
                        .left
                        .map(e => CodecError.Field("source", o, e.message))
                    )
                  ordinal <- ResultWire.readLong(o, "ordinal");
                  x       <- DomainWire.finite(o, "x"); y <- DomainWire.finite(o, "y")
                yield LocationObservation(source, ordinal, Pt[Px](x, y))
              })
            trial <- LocationTrial
              .of(trialId, observations)
              .left
              .map(e => CodecError.Field("trial", t, e.message))
          yield trial
        })
      input <- LocationInput
        .of(frame, trials)
        .left
        .map(e => CodecError.Field("trials", json, e.message))
    yield input

/** Versioned plan document for the location-only, pixel-space analysis. */
object LocationComparisonPlanCodec:
  def of(schema: DefinitionId): VersionedCodec[LocationComparisonPlan] =
    VersionedCodec.checked[LocationComparisonPlan](schema) { plan =>
      Right(
        Json.obj(
          "frame"   -> DomainWire.frame(plan.input.frame),
          "columns" -> Json.fromInt(plan.grid.nx),
          "rows"    -> Json.fromInt(plan.grid.ny),
          "trials"  -> Json.arr(plan.input.trials.map { t =>
            Json.obj(
              "id"           -> Json.fromString(t.id.value),
              "observations" -> Json.arr(t.observations.map { o =>
                Json.obj(
                  "source"  -> Json.fromString(o.source.value),
                  "ordinal" -> ResultWire.long(o.sourceOrdinal),
                  "x"       -> Json.fromDoubleOrNull(o.position.x),
                  "y"       -> Json.fromDoubleOrNull(o.position.y)
                )
              }*)
            )
          }*),
          "queries" -> Json.arr(plan.queries.map { q =>
            Json.obj(
              "id"      -> Json.fromString(q.id),
              "focal"   -> Json.fromString(q.focal.value),
              "matched" -> Json.fromString(q.matched.value),
              "control" -> Json.fromString(q.control.value)
            )
          }*),
          "inputHash" -> Json.fromString(plan.inputHash.render),
          "planHash"  -> Json.fromString(plan.planHash.render)
        )
      )
    } { json =>
      for
        input   <- LocationInputWire.read(json)
        columns <- Wire.field[Int](json, "columns"); rows <- Wire.field[Int](json, "rows")
        queries <- Wire
          .field[Vector[Json]](json, "queries")
          .flatMap(_.traverse { q =>
            for
              focal <- Wire
                .field[String](q, "focal")
                .flatMap(v =>
                  LocationTrialId.of(v).left.map(e => CodecError.Field("focal", q, e.message))
                )
              matched <- Wire
                .field[String](q, "matched")
                .flatMap(v =>
                  LocationTrialId.of(v).left.map(e => CodecError.Field("matched", q, e.message))
                )
              control <- Wire
                .field[String](q, "control")
                .flatMap(v =>
                  LocationTrialId.of(v).left.map(e => CodecError.Field("control", q, e.message))
                )
              name  <- Wire.field[String](q, "id")
              query <- LocationComparisonQuery
                .of(name, focal, matched, control)
                .left
                .map(e => CodecError.Field("query", q, e.message))
            yield query
          })
        plan <- LocationComparisonPlan
          .binned(input, columns, rows, queries)
          .left
          .map(e => CodecError.Field("plan", json, e.message))
        declaredInput <- ResultWire.hash(json, "inputHash");
        declaredPlan  <- ResultWire.hash(json, "planHash")
        _             <- Either.cond(
          declaredInput == plan.inputHash,
          (),
          CodecError.InputIdentity(declaredInput.render, plan.inputHash.render)
        )
        _ <- Either.cond(
          declaredPlan == plan.planHash,
          (),
          CodecError.InputIdentity(declaredPlan.render, plan.planHash.render)
        )
      yield plan
    }

/** A saved location run retains its checked input, exact triples and stored outcomes. */
final case class LocationComparisonRun(
    plan: LocationComparisonPlan,
    result: LocationComparisonResult
)

/** Cold result reconstruction never invokes the plan's scientific execution. */
object LocationComparisonResultCodec:
  val plans: VersionedCodec[LocationComparisonPlan] =
    LocationComparisonPlanCodec.of(LocationComparisonDefinitions.plan)

  private def stored(result: LocationComparisonResult): Vector[LocationStoredOutcome] =
    result.outcomes.map(o =>
      LocationStoredOutcome(o.query.id, o.matched.map(_.value), o.control.map(_.value))
    )
  private def maps(result: LocationComparisonResult) =
    result.maps.map(m => m.trial -> m.result.map(_.bins))
  private def validate(
      run: LocationComparisonRun
  ): Either[CodecError, LocationComparisonResult] =
    LocationComparisonResult
      .restore(
        run.plan,
        run.result.inputHash,
        run.result.planHash,
        maps(run.result),
        stored(run.result)
      )
      .left
      .map(e => CodecError.Field("result", Json.Null, e.message))

  private def score(value: Either[LocationRunError, Double]): Json = value match
    case Right(number) => ResultWire.tagged("score", "value" -> ResultWire.double(number))
    case Left(error)   =>
      ResultWire.tagged("failure", "error" -> LocationComparisonCodec.runError(error))
  private def readScore(json: Json): Either[CodecError, Either[LocationRunError, Double]] =
    ResultWire.kind(json).flatMap {
      case "score"   => ResultWire.readDouble(json, "value").map(Right(_))
      case "failure" =>
        Wire
          .field[Json](json, "error")
          .flatMap(LocationComparisonCodec.readRunError)
          .map(Left(_))
      case other => Left(ResultWire.unknown(json, "location score", other))
    }

  val codec: VersionedCodec[LocationComparisonRun] =
    VersionedCodec.checked[LocationComparisonRun](LocationComparisonDefinitions.result) { run =>
      for
        checked <- validate(run)
        plan    <- plans.encode(run.plan)
      yield Json.obj(
        "plan"      -> plan,
        "inputHash" -> Json.fromString(checked.inputHash.render),
        "planHash"  -> Json.fromString(checked.planHash.render),
        "maps"      -> Json.arr(maps(checked).map { (trial, value) =>
          val outcome = value match
            case Right(bins) =>
              ResultWire.tagged("bins", "values" -> Json.arr(bins.map(ResultWire.double)*))
            case Left(error) =>
              ResultWire.tagged("failure", "error" -> LocationComparisonCodec.mapError(error))
          Json.obj("trial" -> Json.fromString(trial.value), "outcome" -> outcome)
        }*),
        "outcomes" -> Json.arr(
          stored(checked).map(o =>
            Json.obj(
              "query"   -> Json.fromString(o.query),
              "matched" -> score(o.matched),
              "control" -> score(o.control)
            )
          )*
        )
      )
    } { json =>
      for
        plan      <- Wire.field[Json](json, "plan").flatMap(plans.decode)
        inputHash <- ResultWire.hash(json, "inputHash")
        planHash  <- ResultWire.hash(json, "planHash")
        maps      <- Wire
          .field[Vector[Json]](json, "maps")
          .flatMap(_.traverse { entry =>
            for
              name  <- Wire.field[String](entry, "trial")
              trial <- LocationTrialId
                .of(name)
                .left
                .map(e => CodecError.Field("trial", entry, e.message))
              outcome <- Wire.field[Json](entry, "outcome")
              kind    <- ResultWire.kind(outcome)
              value   <- kind match
                case "bins"    => Wire.field[Vector[Double]](outcome, "values").map(Right(_))
                case "failure" =>
                  Wire
                    .field[Json](outcome, "error")
                    .flatMap(LocationComparisonCodec.readMapError)
                    .map(Left(_))
                case other => Left(ResultWire.unknown(outcome, "location map", other))
            yield trial -> value
          })
        outcomes <- Wire
          .field[Vector[Json]](json, "outcomes")
          .flatMap(_.traverse { entry =>
            for
              query   <- Wire.field[String](entry, "query")
              matched <- Wire.field[Json](entry, "matched").flatMap(readScore)
              control <- Wire.field[Json](entry, "control").flatMap(readScore)
            yield LocationStoredOutcome(query, matched, control)
          })
        result <- LocationComparisonResult
          .restore(plan, inputHash, planHash, maps, outcomes)
          .left
          .map(e => CodecError.Field("result", json, e.message))
      yield LocationComparisonRun(plan, result)
    }

  private def description(
      plan: LocationComparisonPlan
  ): Vector[(String, Vector[Provenance.Param])] =
    Vector("location.plan" -> Vector(Provenance.Param.Text(plan.planHash.render)))

  val registration: AnalysisRegistration = AnalysisRegistration.of(plans, codec)(
    description,
    run => description(run.plan),
    run => Vector(run.result.inputHash),
    embedded = Some(plan => Vector(plan.inputHash))
  )

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
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

object ScanpathMethodCodecDefinitions:
  val method: DefinitionId = DefinitionId.builtIn("eyes4s.scanpath-method", 1)

/** Complete typed method settings. Exact signed microseconds and Long work
  * limits use decimal strings, never an intermediate JavaScript Number.
  */
final class ScanpathMethodCodec[U <: Unit2D: UnitLabel]:
  private def field[A: io.circe.Decoder](json: Json, name: String): Either[CodecError, A] =
    Wire.field[A](json, name)
  private def long(json: Json, name: String): Either[CodecError, Long] =
    field[String](json, name).flatMap(value =>
      value.toLongOption.toRight(
        CodecError.Field(name, json, s"Expected signed64 decimal, found=$value")
      )
    )
  private def enumeration[A](
      json: Json,
      name: String,
      values: Vector[A]
  ): Either[CodecError, A] =
    field[String](json, name).flatMap(value =>
      values
        .find(_.toString == value)
        .toRight(CodecError.Field(name, json, s"Unknown $name=$value"))
    )
  private def fixation[A](
      json: Json,
      name: String,
      value: Either[FixationComparisonError, A]
  ): Either[CodecError, A] =
    value.left.map(error => CodecError.Field(name, json, error.message))
  private def alignment[A](
      json: Json,
      name: String,
      value: Either[ScanpathComparisonError, A]
  ): Either[CodecError, A] =
    value.left.map(error => CodecError.Field(name, json, error.message))
  private def fields(json: Json, expected: Set[String]): Either[CodecError, Unit] =
    val found = json.asObject.fold(Set.empty[String])(_.keys.toSet)
    Either.cond(
      found == expected,
      (),
      CodecError.Field(
        "parameters",
        json,
        s"Expected fields=${expected.toVector.sorted}; found=${found.toVector.sorted}"
      )
    )
  private def write(method: ScanpathMethod[U]): Json =
    val parameters = method match
      case ScanpathMethod.MultiMatch(cardinality, limit) =>
        Json.obj(
          "cardinality"           -> Json.fromString(cardinality.toString),
          "maximumAlignmentCells" -> Json.fromString(limit.value.toString),
          "alignmentConvention"   -> Json.fromString(MultiMatch.alignmentConvention.toString)
        )
      case ScanpathMethod.Overlap(spec) =>
        Json.obj(
          "clock"   -> Json.fromString(spec.clock.name),
          "queries" -> Json.arr(spec.queries.map(at => Json.fromString(at.toMicros.toString))*),
          "threshold"      -> ResultWire.double(spec.threshold.value),
          "distance"       -> Json.fromString(spec.distance.toString),
          "normConvention" -> Json.fromString(spec.distance.normConvention.toString),
          "endpoint"       -> Json.fromString(spec.endpoint.toString),
          "missing"        -> Json.fromString(spec.missing.toString)
        )
      case ScanpathMethod.Transport(config) =>
        Json.obj(
          "xScale"            -> ResultWire.double(config.xScale),
          "yScale"            -> ResultWire.double(config.yScale),
          "timeScaleMicros"   -> Json.fromString(config.timeScale.toMicros.toString),
          "timeWeight"        -> ResultWire.double(config.timeWeight),
          "lambda"            -> ResultWire.double(config.lambda),
          "maximumIterations" -> Json.fromInt(config.maximumIterations),
          "marginalTolerance" -> ResultWire.double(config.marginalTolerance),
          "maximumCosts"      -> Json.fromInt(config.maximumCosts),
          "maximumWork"       -> Json.fromString(config.maximumWork.toString)
        )
    Json.obj(
      "unit"   -> Json.fromString(summon[UnitLabel[U]].symbol),
      "method" -> Json.obj(
        "name"    -> Json.fromString(method.id.name),
        "version" -> Json.fromInt(method.id.version)
      ),
      "parameters" -> parameters
    )
  val codec: VersionedCodec[ScanpathMethod[U]] = VersionedCodec.checked[ScanpathMethod[U]](
    ScanpathMethodCodecDefinitions.method
  )(method => Right(write(method))) { json =>
    for
      unit <- field[String](json, "unit")
      _    <- Either.cond(
        unit == summon[UnitLabel[U]].symbol,
        (),
        CodecError.Field("unit", json, s"Expected ${summon[UnitLabel[U]].symbol}, found=$unit")
      )
      definition <- field[Json](json, "method")
      name       <- field[String](definition, "name")
      version    <- field[Int](definition, "version")
      id         <- DefinitionId
        .of(name, version)
        .left
        .map(error => CodecError.Field("method", definition, error.message))
      kind <- ScanpathMethods
        .resolve(id)
        .toRight(
          CodecError.Field(
            "method",
            definition,
            s"Unregistered scanpath method=${id.name}@${id.version}"
          )
        )
      params <- field[Json](json, "parameters")
      result <- read(kind, params)
    yield result
  }

  private def read(
      kind: ScanpathMethodKind,
      json: Json
  ): Either[CodecError, ScanpathMethod[U]] = kind match
    case ScanpathMethodKind.MultiMatch =>
      for
        _ <- fields(json, Set("cardinality", "maximumAlignmentCells", "alignmentConvention"))
        cardinality <- enumeration(json, "cardinality", MultiMatchCardinality.values.toVector)
        limit       <- long(json, "maximumAlignmentCells").flatMap(value =>
          alignment(json, "maximumAlignmentCells", AlignmentCellLimit.of(value))
        )
        convention <- field[String](json, "alignmentConvention")
        _          <- Either.cond(
          convention == MultiMatch.alignmentConvention.toString,
          (),
          CodecError.Field(
            "alignmentConvention",
            json,
            s"Expected ${MultiMatch.alignmentConvention}, found=$convention"
          )
        )
      yield ScanpathMethod.MultiMatch(cardinality, limit)
    case ScanpathMethodKind.Overlap =>
      for
        _ <- fields(
          json,
          Set(
            "clock",
            "queries",
            "threshold",
            "distance",
            "normConvention",
            "endpoint",
            "missing"
          )
        )
        clock   <- field[String](json, "clock")
        values  <- field[Vector[String]](json, "queries")
        queries <- values.zipWithIndex.traverse { (value, index) =>
          value.toLongOption
            .map(Instant.micros)
            .toRight(
              CodecError.Field(
                s"queries[$index]",
                json,
                s"Expected signed64 microseconds, found=$value"
              )
            )
        }
        threshold <- field[Double](json, "threshold").flatMap(value =>
          fixation(json, "threshold", OverlapThreshold.of[U](value))
        )
        distance <- enumeration(json, "distance", FixationGroundDistance.values.toVector)
        norm     <- field[String](json, "normConvention")
        _        <- Either.cond(
          norm == distance.normConvention.toString,
          (),
          CodecError.Field(
            "normConvention",
            json,
            s"Expected ${distance.normConvention}, found=$norm"
          )
        )
        endpoint <- enumeration(json, "endpoint", TrajectoryEndpoint.values.toVector)
        missing  <- enumeration(json, "missing", MissingOverlapPolicy.values.toVector)
        spec     <- fixation(
          json,
          "overlap",
          FixationOverlapSpec
            .of(ClockId(clock), queries, threshold, distance, endpoint, missing)
        )
      yield ScanpathMethod.Overlap(spec)
    case ScanpathMethodKind.Transport =>
      for
        _ <- fields(
          json,
          Set(
            "xScale",
            "yScale",
            "timeScaleMicros",
            "timeWeight",
            "lambda",
            "maximumIterations",
            "marginalTolerance",
            "maximumCosts",
            "maximumWork"
          )
        )
        x          <- field[Double](json, "xScale")
        y          <- field[Double](json, "yScale")
        time       <- long(json, "timeScaleMicros")
        weight     <- field[Double](json, "timeWeight")
        lambda     <- field[Double](json, "lambda")
        iterations <- field[Int](json, "maximumIterations")
        tolerance  <- field[Double](json, "marginalTolerance")
        costs      <- field[Int](json, "maximumCosts")
        work       <- long(json, "maximumWork")
        config     <- fixation(
          json,
          "transport",
          FixationTransportConfig
            .of[U](x, y, Span.micros(time), weight, lambda, iterations, tolerance, costs, work)
        )
      yield ScanpathMethod.Transport(config)

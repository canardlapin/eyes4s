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
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import eyes4s.surface.*
import io.circe.Json

/** A saved result is bound to its complete recipe and checked again by deterministic replay. */
final class PointSamplingArchive[K, U <: Unit2D] private (
    val plan: PointSamplingPlan[K, U],
    val result: PointSamplingResult[K, U]
)
object PointSamplingArchive:
  def run[K, U <: Unit2D](plan: PointSamplingPlan[K, U]): PointSamplingArchive[K, U] =
    new PointSamplingArchive(plan, plan.run)

final class PointSamplingCodec[K, U <: Unit2D: UnitLabel](
    val schema: DefinitionId,
    val resultSchema: DefinitionId,
    val layout: StudyLayout[K],
    val keys: VersionedCodec[K]
):
  private val sourceCodec = new StudyInputCodec[K, U](
    DefinitionId.studyInput,
    DefinitionId.admissionLedger,
    layout,
    keys
  )
  private def domain[A](json: Json)(
      value: Either[PointSamplingError, A]
  ): Either[CodecError, A] =
    value.left.map(e => CodecError.Field("pointSampling", json, e.message))
  private def named[A](json: Json, field: String, values: Array[A]): Either[CodecError, A] =
    Wire
      .field[String](json, field)
      .flatMap(name =>
        values
          .find(_.toString == name)
          .toRight(CodecError.Field(field, json, s"unknown value $name"))
      )
  private def exact(json: Json, field: String, expected: String): Either[CodecError, Unit] =
    Wire
      .field[String](json, field)
      .flatMap(found =>
        Either.cond(
          found == expected,
          (),
          CodecError.Field(field, json, s"expected $expected, found $found")
        )
      )
  private def instants(json: Json, field: String): Either[CodecError, Vector[Instant]] =
    Wire
      .field[Vector[Json]](json, field)
      .flatMap(_.traverse { value => DomainWire.readTime(value, field).map(Instant.micros) })
  private def times(values: Vector[Instant]): Json =
    Json.arr(values.map(t => ResultWire.long(t.toMicros))*)

  val plan: VersionedCodec[PointSamplingPlan[K, U]] =
    VersionedCodec.checked[PointSamplingPlan[K, U]](schema) { p =>
      for
        _ <- Either.cond(
          p.layout.id == layout.id,
          (),
          CodecError.Schema(layout.id, p.layout.id)
        )
        source    <- sourceCodec.input.encode(p.source)
        templates <- p.templates.rows.traverse { row =>
          keys.encode(row.key).map { key =>
            Json.obj(
              "key"   -> key,
              "frame" -> DomainWire.frame(row.value.grid.frame),
              "grid"  -> Json.obj(
                "id" -> Json.fromString(row.value.grid.id.name),
                "nx" -> Json.fromInt(row.value.grid.nx),
                "ny" -> Json.fromInt(row.value.grid.ny)
              ),
              "values"     -> Json.arr(row.value.values.toVector.map(Json.fromDoubleOrNull)*),
              "provenance" -> ResultWire.provenance(row.value.provenance)
            )
          }
        }
      yield Json.obj(
        "layout"    -> Wire.id(layout.id),
        "keySchema" -> Wire.id(keys.schema),
        "method"    -> Json.fromString(PointSamplingSpec.method),
        "source"    -> source,
        "templates" -> Json.arr(templates*),
        "clock"     -> Json.fromString(p.spec.clock.name),
        "queries"   -> times(p.spec.queries),
        "bins"      -> p.spec.bins.fold(Json.Null)(b =>
          Json.obj(
            "clock"      -> Json.fromString(b.clock.name),
            "boundaries" -> times(b.boundaries),
            "endpoint"   -> Json.fromString(b.endpoint.toString)
          )
        ),
        "normalization"      -> Json.fromString(p.spec.normalization.toString),
        "lookup"             -> Json.fromString(p.spec.lookup.toString),
        "trajectoryEndpoint" -> Json.fromString(p.spec.endpoint.toString),
        "controls"           -> (p.spec.controls match
          case PointControlSelection.Disabled      => ResultWire.tagged("disabled")
          case PointControlSelection.Candidates(s) =>
            ResultWire.tagged("candidates", "selection" -> ResultWire.selection(s))),
        "controlPolicy" -> StudyWire.policy(p.spec.controlPolicy),
        "binPolicy"     -> StudyWire.policy(p.spec.binPolicy),
        "inputHash"     -> Json.fromString(p.inputHash.render),
        "planHash"      -> Json.fromString(p.planHash.render)
      )
    } { json =>
      for
        _         <- Wire.requireId(json, "layout", layout.id)
        _         <- Wire.requireId(json, "keySchema", keys.schema)
        _         <- exact(json, "method", PointSamplingSpec.method)
        source    <- Wire.field[Json](json, "source").flatMap(sourceCodec.input.decode)
        templates <- Wire
          .field[Vector[Json]](json, "templates")
          .flatMap(_.traverse { row =>
            for
              key        <- Wire.field[Json](row, "key").flatMap(keys.decode)
              frame      <- Wire.field[Json](row, "frame").flatMap(DomainWire.readFrame[U])
              grid       <- Wire.field[Json](row, "grid").flatMap(DomainWire.readGrid(_, frame))
              values     <- Wire.field[Vector[Double]](row, "values")
              provenance <- Wire
                .field[Json](row, "provenance")
                .flatMap(ResultWire.readProvenance)
              surface <- Surface
                .signed(grid, IArray.from(values), provenance)
                .left
                .map(e => CodecError.Field("values", row, e.message))
            yield Trial(key, (), surface)
          })
        clock   <- Wire.field[String](json, "clock").map(ClockId.apply)
        queries <- instants(json, "queries")
        bins    <- ResultWire.optional(json, "bins") { value =>
          for
            name       <- Wire.field[String](value, "clock")
            boundaries <- instants(value, "boundaries")
            endpoint   <- named(value, "endpoint", PointBinEndpoint.values)
            result     <- domain(value)(PointBins.of(ClockId(name), boundaries, endpoint))
          yield result
        }
        normalization <- named(json, "normalization", DensityNormalization.values)
        lookup        <- named(json, "lookup", DensityLookupPolicy.values)
        endpoint      <- named(json, "trajectoryEndpoint", TrajectoryEndpoint.values)
        controls      <- Wire.field[Json](json, "controls").flatMap { value =>
          ResultWire.kind(value).flatMap {
            case "disabled"   => Right(PointControlSelection.Disabled)
            case "candidates" =>
              Wire
                .field[Json](value, "selection")
                .flatMap(ResultWire.readSelection)
                .map(PointControlSelection.Candidates.apply)
            case other => Left(CodecError.Field("controls", value, s"unknown kind $other"))
          }
        }
        controlPolicy <- Wire.field[Json](json, "controlPolicy").flatMap(StudyWire.readPolicy)
        binPolicy     <- Wire.field[Json](json, "binPolicy").flatMap(StudyWire.readPolicy)
        spec          <- domain(json)(
          PointSamplingSpec.of(
            clock,
            queries,
            bins,
            normalization,
            lookup,
            endpoint,
            controls,
            controlPolicy,
            binPolicy
          )
        )
        p = PointSamplingPlan.of(layout, source, Trials(templates), spec)
        _ <- exact(json, "inputHash", p.inputHash.render)
        _ <- exact(json, "planHash", p.planHash.render)
      yield p
    }

  val archive: VersionedCodec[PointSamplingArchive[K, U]] =
    VersionedCodec.checked[PointSamplingArchive[K, U]](resultSchema) { a =>
      for p <- plan.encode(a.plan); r <- resultJson(a.result)
      yield Json.obj("plan" -> p, "result" -> r)
    } { json =>
      for
        p        <- Wire.field[Json](json, "plan").flatMap(plan.decode)
        expected <- Wire.field[Json](json, "result")
        a = PointSamplingArchive.run(p)
        actual <- resultJson(a.result)
        _      <- Either.cond(
          actual == expected,
          (),
          CodecError.Derived("pointSamplingResult", expected, actual)
        )
      yield a
    }

  private[eyes4s] def error(value: PointSamplingError): Json =
    import ResultWire.{tagged, double, long}
    value match
      case PointSamplingError.Boundaries(v) =>
        tagged("boundaries", "values" -> Json.arr(v.map(long)*))
      case PointSamplingError.Clock(e)  => tagged("clock", "error" -> ResultWire.timeError(e))
      case PointSamplingError.Frames(e) =>
        tagged("frames", "error" -> ResultWire.geometryError(e))
      case PointSamplingError.MissingTemplate(p, s) =>
        tagged(
          "missingTemplate",
          "participant" -> Json.fromString(p),
          "stimulus"    -> Json.fromString(s)
        )
      case PointSamplingError.AmbiguousTemplate(p, s, i) =>
        tagged(
          "ambiguousTemplate",
          "participant" -> Json.fromString(p),
          "stimulus"    -> Json.fromString(s),
          "indices"     -> Json.arr(i.map(Json.fromInt)*)
        )
      case PointSamplingError.DuplicateSource(i) =>
        tagged("duplicateSource", "indices" -> Json.arr(i.map(Json.fromInt)*))
      case PointSamplingError.Density(i, e) =>
        val detail = e match
          case DensityLookupError.Arithmetic(n, o, v) =>
            tagged(
              "arithmetic",
              "normalization" -> Json.fromString(n.toString),
              "operand"       -> Json.fromString(o),
              "value"         -> double(v)
            )
          case DensityLookupError.Surface(s) =>
            tagged("surface", "error" -> ResultWire.surfaceError(s))
        tagged("density", "templateIndex" -> Json.fromInt(i), "error" -> detail)
      case PointSamplingError.Point(e) =>
        e match
          case DensityPointFailure.NonFinitePoint(x, y) =>
            tagged("nonfinitePoint", "x" -> double(x), "y" -> double(y))
          case DensityPointFailure.OutsideGrid(g, x, y) =>
            tagged(
              "outsideGrid",
              "grid" -> Json.fromString(g.name),
              "x"    -> double(x),
              "y"    -> double(y)
            )
          case DensityPointFailure.Trajectory(reason) =>
            tagged("trajectory", "reason" -> Json.fromString(reason.toString))
      case PointSamplingError.Insufficient(l, n, k, m) =>
        tagged(
          "insufficient",
          "level"      -> Json.fromString(l),
          "requested"  -> Json.fromInt(n),
          "successful" -> Json.fromInt(k),
          "minimum"    -> Json.fromInt(m)
        )
      case PointSamplingError.NonFinite(l, i, v) =>
        tagged(
          "nonfinite",
          "level" -> Json.fromString(l),
          "index" -> Json.fromInt(i),
          "value" -> double(v)
        )
  private def value(v: Either[PointSamplingError, Double]): Json = v match
    case Right(x) => ResultWire.tagged("success", "value" -> Json.fromDoubleOrNull(x))
    case Left(e)  => ResultWire.tagged("failure", "error" -> error(e))
  private def mean(m: PointMean): Json = Json.obj(
    "requested"    -> Json.fromInt(m.requested),
    "successful"   -> Json.fromInt(m.successful),
    "contributing" -> Json.fromInt(m.contributing),
    "policy"       -> StudyWire.policy(m.policy),
    "result"       -> value(m.result)
  )
  private def keyed(v: Either[PointSamplingError, K]): Either[CodecError, Json] = v match
    case Right(k) => keys.encode(k).map(j => ResultWire.tagged("success", "key" -> j))
    case Left(e)  => Right(ResultWire.tagged("failure", "error" -> error(e)))
  private def resultJson(r: PointSamplingResult[K, U]): Either[CodecError, Json] = for
    rows <- r.rows.traverse { row =>
      for
        key      <- keys.encode(row.key)
        template <- keyed(row.template)
        controls <- row.controls.traverse { c =>
          for
            occurrence <- keys.encode(c.occurrence)
            template   <- keys.encode(c.template)
          yield Json.obj(
            "occurrence" -> occurrence,
            "template"   -> template,
            "values"     -> Json.arr(c.values.map(value)*)
          )
        }
      yield Json.obj(
        "index"            -> Json.fromInt(row.index),
        "key"              -> key,
        "template"         -> template,
        "eligibleControls" -> Json.fromInt(row.eligibleControls),
        "selectedControls" -> Json.fromInt(row.controls.size),
        "controls"         -> Json.arr(controls*),
        "points"           -> Json.arr(
          row.points.map(p =>
            Json.obj(
              "index"      -> Json.fromInt(p.index),
              "timeMicros" -> ResultWire.long(p.time.toMicros),
              "point"      -> p.point.fold(Json.Null)(x =>
                Json.arr(Json.fromDoubleOrNull(x.x), Json.fromDoubleOrNull(x.y))
              ),
              "value"   -> value(p.value),
              "control" -> p.control.fold(Json.Null)(mean),
              "bin"     -> p.bin.fold(Json.Null)(Json.fromInt)
            )
          )*
        ),
        "bins" -> Json.arr(
          row.bins.map(b =>
            Json.obj(
              "index"                 -> Json.fromInt(b.index),
              "fromMicros"            -> ResultWire.long(b.from.toMicros),
              "untilMicros"           -> ResultWire.long(b.until.toMicros),
              "includesFinalEndpoint" -> Json.fromBoolean(b.includesFinalEndpoint),
              "queries"               -> Json.arr(b.queries.map(Json.fromInt)*),
              "observed"              -> mean(b.observed),
              "control"               -> b.control.fold(Json.Null)(mean),
              "difference"            -> b.difference.fold(Json.Null)(value)
            )
          )*
        )
      )
    }
    pairing <- r.controlPairing.traverse(ResultWire.pairingReport(keys))
  yield Json.obj(
    "rows"           -> Json.arr(rows*),
    "controlPairing" -> pairing.getOrElse(Json.Null),
    "unbinned"       -> Json.arr(r.unbinned.map(Json.fromInt)*),
    "inputHash"      -> Json.fromString(r.inputHash.render),
    "planHash"       -> Json.fromString(r.planHash.render),
    "provenance"     -> ResultWire.provenance(r.provenance)
  )

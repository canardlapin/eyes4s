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
import eyes4s.aoi.*
import eyes4s.kernel.*
import eyes4s.plan.DefinitionId
import io.circe.Json

/** Versioned fixation AOI persistence. Callers supply their registered envelope
  * schema, as for DomainCodecs. The nested scanpath uses the established schema and
  * source-supported replay checks; nominal frames and clocks share one identity table.
  * Decode reruns assignment and refuses altered memberships, accounting or measures.
  */
object FixationAoiCodecs:
  def assignment[U <: Unit2D: UnitLabel](
      schema: DefinitionId
  ): VersionedCodec[FixationAoiAssignment[U]] =
    val input = StudyInputCodecs.study[U]
    VersionedCodec.checked[FixationAoiAssignment[U]](schema)(a =>
      for
        identities <- DocumentIdentities.empty
          .addFrame(a.scanpath.frame)
          .map(_.addClock(a.scanpath.clock))
        path <- input.scanpaths(identities).encode(a.scanpath)
      yield Json.obj(
        "identities" -> identities.json,
        "scanpath"   -> path,
        "areas"      -> Json.arr(a.aoiSet.areas.map(RecordingResultWire.area)*),
        "policy"     -> RecordingResultWire.policy(a.policy),
        "derived"    -> derived(a)
      )
    )(json =>
      for
        identities <- Wire.field[Json](json, "identities").flatMap(DocumentIdentities.read)
        path       <- Wire
          .field[Json](json, "scanpath")
          .flatMap(input.scanpaths(identities).decode)
          .left
          .map(Wire.at("scanpath"))
        rawAreas <- Wire.field[Vector[Json]](json, "areas")
        areas    <- rawAreas.zipWithIndex.traverse { (area, index) =>
          RecordingResultWire.readArea[U](area, identities).left.map(Wire.at(s"areas[$index]"))
        }
        aoiSet     <- AoiSet.of(areas).left.map(e => CodecError.Field("areas", json, e.message))
        policyJson <- Wire.field[Json](json, "policy")
        policy     <- readPolicy(policyJson)
        assignment <- aoiSet
          .assign(path, policy)
          .left
          .map(e => CodecError.Field("assignment", json, e.message))
        declared <- Wire.field[Json](json, "derived")
        expected = derived(assignment)
        _ <- Vector("memberships", "report", "metrics", "transitions").traverse_ { field =>
          for
            found  <- Wire.field[Json](declared, field)
            actual <- Wire.field[Json](expected, field)
            _      <- Either.cond(
              found == actual,
              (),
              CodecError.Field(
                s"derived.$field",
                found,
                "declared value differs from checked fixation AOI replay"
              )
            )
          yield ()
        }
      yield assignment
    )

  private def readPolicy(json: Json): Either[CodecError, MembershipPolicy] =
    ResultWire.kind(json).flatMap {
      case "multiple"            => Right(MembershipPolicy.Multiple)
      case "exclusiveByPriority" => Right(MembershipPolicy.ExclusiveByPriority)
      case "rejectOverlap"       => Right(MembershipPolicy.RejectOverlap)
      case "smallestContaining"  =>
        for
          nx         <- Wire.field[Int](json, "nx")
          ny         <- Wire.field[Int](json, "ny")
          resolution <- AoiResolution
            .of(nx, ny)
            .left
            .map(e => CodecError.Field("resolution", json, e.message))
        yield MembershipPolicy.SmallestContaining(resolution)
      case other =>
        Left(CodecError.Field("policy", json, s"unknown membership policy '$other'"))
    }

  private def micros(span: Span): Json               = Json.fromString(span.toMicros.toString)
  private def optionalSpan(span: Option[Span]): Json = span.fold(Json.Null)(micros)

  private def derived[U <: Unit2D](a: FixationAoiAssignment[U]): Json =
    val measurements = a.measure
    Json.obj(
      "memberships" -> Json.arr(a.toVector.map {
        case FixationMembership.Areas(ids) =>
          Json.obj(
            "kind" -> Json.fromString("areas"),
            "ids"  -> Json.arr(ids.map(id => Json.fromString(id.value))*)
          )
        case FixationMembership.Background => Json.obj("kind" -> Json.fromString("background"))
        case FixationMembership.OffSurface => Json.obj("kind" -> Json.fromString("offSurface"))
      }*),
      "report" -> Json.obj(
        "aoiUnionMicros"      -> micros(a.report.aoiUnionTime),
        "backgroundMicros"    -> micros(a.report.backgroundTime),
        "offSurfaceMicros"    -> micros(a.report.offSurfaceTime),
        "interFixationMicros" -> micros(a.report.interFixationTime),
        "duplicatedAoiMicros" -> micros(a.report.duplicatedAoiTime)
      ),
      "metrics" -> Json.arr(measurements.areas.map { metric =>
        Json.obj(
          "id"              -> Json.fromString(metric.id.value),
          "dwellMicros"     -> micros(metric.dwell),
          "dwellProportion" -> metric.dwellProportion.fold(Json.Null)(Json.fromDoubleOrNull),
          "firstEntryLatencyMicros" -> optionalSpan(metric.firstEntryLatency),
          "fixationCount"           -> Json.fromInt(metric.fixationCount),
          "entryCount"              -> Json.fromInt(metric.entryCount),
          "visitCount"              -> Json.fromInt(metric.visitCount)
        )
      }*),
      "transitions" -> Json.arr(measurements.transitions.map { transition =>
        Json.obj(
          "from"  -> Json.fromString(transition.from.value),
          "to"    -> Json.fromString(transition.to.value),
          "count" -> Json.fromInt(transition.count)
        )
      }*)
    )

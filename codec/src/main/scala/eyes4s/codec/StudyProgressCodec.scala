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

import eyes4s.compare.ComparisonQuantum
import eyes4s.design.PairQuantum
import eyes4s.plan.*
import io.circe.Json

object StudyProgressDefinitions:
  val progress: DefinitionId = DefinitionId.builtIn("eyes4s.study-progress", 1)

/** A portable committed progress snapshot. Decoded stamps are identity claims.
  * Counting has visited schedule units; running always has its scientific meter.
  * Long counters use canonical decimal strings to survive JavaScript JSON parsing.
  */
final class StudyProgressSnapshot[Plan, Input] private (
    val stamp: RunStamp[Plan, Input],
    val pairQuantum: PairQuantum,
    val comparisonQuantum: ComparisonQuantum,
    val step: Long,
    val stage: StudyRunStage,
    val stepUnits: Int,
    val segmentUnits: Long,
    val segmentTotal: SegmentTotal,
    val totalUnits: Long
)

object StudyProgressSnapshot:
  def of[P, I](
      stamp: RunStamp[P, I],
      pairQuantum: PairQuantum,
      comparisonQuantum: ComparisonQuantum,
      step: Long,
      stage: StudyRunStage,
      stepUnits: Int,
      segmentUnits: Long,
      segmentTotal: SegmentTotal,
      totalUnits: Long
  ): Either[CodecError, StudyProgressSnapshot[P, I]] =
    def require(
        valid: Boolean,
        path: String,
        value: Json,
        reason: String
    ): Either[CodecError, Unit] =
      Either.cond(valid, (), CodecError.Field(path, value, reason))
    val stageValid = stage match
      case StudyRunStage.Counting(_, visited) =>
        visited >= 0L && segmentTotal == SegmentTotal.Counting
      case StudyRunStage.Running(scientific, meter) =>
        val (kind, unit, indicesValid) = scientific match
          case StudyStage.Estimating(scale, trial) =>
            (StageKind.Estimating, CountUnit.Maps, scale >= 0 && trial >= 0)
          case StudyStage.Comparing(scale, _) =>
            (StageKind.Comparing, CountUnit.Pairs, scale >= 0)
          case StudyStage.Reducing(scale, _) => (StageKind.Reducing, CountUnit.Keys, scale >= 0)
          case StudyStage.Contrasting(scale) =>
            (StageKind.Contrasting, CountUnit.Rows, scale >= 0)
        meter.kind == kind && meter.unit == unit && indicesValid && segmentTotal != SegmentTotal.Counting && meter.total != SegmentTotal.Counting
    val totalValid = segmentTotal match
      case SegmentTotal.Exact(n)                        => n >= segmentUnits
      case SegmentTotal.AtMost(n)                       => n >= segmentUnits
      case SegmentTotal.Counting | SegmentTotal.Unknown => true
    for
      _ <- require(
        step > 0,
        "step",
        Json.fromString(step.toString),
        "expected a positive committed step"
      )
      _ <- require(
        stepUnits >= 0,
        "stepUnits",
        Json.fromInt(stepUnits),
        "expected nonnegative work units"
      )
      _ <- require(
        segmentUnits >= stepUnits,
        "segmentUnits",
        Json.fromString(segmentUnits.toString),
        s"must include stepUnits=$stepUnits"
      )
      _ <- require(
        totalUnits >= segmentUnits,
        "totalUnits",
        Json.fromString(totalUnits.toString),
        s"must include segmentUnits=$segmentUnits"
      )
      _ <- require(
        totalValid,
        "segmentTotal",
        StudyProgressCodec.writeTotal(segmentTotal),
        s"bound must include segmentUnits=$segmentUnits"
      )
      _ <- require(
        stageValid,
        "stage",
        StudyProgressCodec.writeStage(stage),
        stage match
          case StudyRunStage.Counting(_, visited) =>
            s"visited=$visited must be nonnegative and segmentTotal=$segmentTotal must be Counting"
          case StudyRunStage.Running(_, meter) =>
            s"stage indices must be nonnegative and agree with meter kind=${meter.kind}, unit=${meter.unit}, total=${meter.total}; segmentTotal=$segmentTotal must not be Counting"
      )
    yield new StudyProgressSnapshot(
      stamp,
      pairQuantum,
      comparisonQuantum,
      step,
      stage,
      stepUnits,
      segmentUnits,
      segmentTotal,
      totalUnits
    )

object StudyProgressCodec:
  def codec[P, I]: VersionedCodec[StudyProgressSnapshot[P, I]] =
    VersionedCodec.of[StudyProgressSnapshot[P, I]](StudyProgressDefinitions.progress)(value =>
      Json.obj(
        "stamp"             -> RunStampWire.write(value.stamp),
        "pairQuantum"       -> Json.fromInt(value.pairQuantum.value),
        "comparisonQuantum" -> Json.fromInt(value.comparisonQuantum.value),
        "step"              -> Json.fromString(value.step.toString),
        "stage"             -> writeStage(value.stage),
        "stepUnits"         -> Json.fromInt(value.stepUnits),
        "segmentUnits"      -> Json.fromString(value.segmentUnits.toString),
        "segmentTotal"      -> writeTotal(value.segmentTotal),
        "totalUnits"        -> Json.fromString(value.totalUnits.toString)
      )
    ) { json =>
      for
        stamp <- Wire.field[Json](json, "stamp").flatMap(RunStampWire.read[P, I])
        pair  <- Wire
          .field[Int](json, "pairQuantum")
          .flatMap(n =>
            PairQuantum
              .of(n)
              .left
              .map(e => CodecError.Field("pairQuantum", Json.fromInt(n), e.message))
          )
        comparison <- Wire
          .field[Int](json, "comparisonQuantum")
          .flatMap(n =>
            ComparisonQuantum
              .of(n)
              .left
              .map(e => CodecError.Field("comparisonQuantum", Json.fromInt(n), e.message))
          )
        step         <- long(json, "step")
        stage        <- Wire.field[Json](json, "stage").flatMap(readStage)
        units        <- Wire.field[Int](json, "stepUnits")
        segmentUnits <- long(json, "segmentUnits")
        total        <- Wire.field[Json](json, "segmentTotal").flatMap(readTotal)
        totalUnits   <- long(json, "totalUnits")
        result       <- StudyProgressSnapshot.of(
          stamp,
          pair,
          comparison,
          step,
          stage,
          units,
          segmentUnits,
          total,
          totalUnits
        )
      yield result
    }

  private def long(json: Json, field: String): Either[CodecError, Long] =
    Wire
      .field[String](json, field)
      .flatMap(raw =>
        raw.toLongOption
          .filter(_.toString == raw)
          .toRight(
            CodecError.Field(
              field,
              Json.fromString(raw),
              "expected a canonical signed 64-bit decimal string"
            )
          )
      )

  private[codec] def writeTotal(total: SegmentTotal): Json = total match
    case SegmentTotal.Counting => Json.obj("kind" -> Json.fromString("counting"))
    case SegmentTotal.Unknown  => Json.obj("kind" -> Json.fromString("unknown"))
    case SegmentTotal.Exact(n) =>
      Json.obj("kind" -> Json.fromString("exact"), "value" -> Json.fromString(n.toString))
    case SegmentTotal.AtMost(n) =>
      Json.obj("kind" -> Json.fromString("at-most"), "value" -> Json.fromString(n.toString))

  private def readTotal(json: Json): Either[CodecError, SegmentTotal] =
    Wire.field[String](json, "kind").flatMap {
      case "counting" => Right(SegmentTotal.Counting)
      case "unknown"  => Right(SegmentTotal.Unknown)
      case "exact"    => long(json, "value").map(SegmentTotal.Exact(_))
      case "at-most"  => long(json, "value").map(SegmentTotal.AtMost(_))
      case other => Left(CodecError.Field("kind", Json.fromString(other), "unknown total kind"))
    }

  private def writeDesign(design: StudyDesign): Json = Json.fromString(design match
    case StudyDesign.Matched => "matched"
    case StudyDesign.Control => "control")
  private def readDesign(json: Json): Either[CodecError, StudyDesign] =
    Wire.field[String](json, "design").flatMap {
      case "matched" => Right(StudyDesign.Matched)
      case "control" => Right(StudyDesign.Control)
      case other     =>
        Left(CodecError.Field("design", Json.fromString(other), "unknown study design"))
    }

  private[codec] def writeStage(stage: StudyRunStage): Json = stage match
    case StudyRunStage.Counting(design, visited) =>
      Json.obj(
        "kind"    -> Json.fromString("counting"),
        "design"  -> writeDesign(design),
        "visited" -> Json.fromString(visited.toString)
      )
    case StudyRunStage.Running(scientific, meter) =>
      val (kind, scale, detail) = scientific match
        case StudyStage.Estimating(scale, trial) =>
          ("estimating", scale, Vector("trial" -> Json.fromInt(trial)))
        case StudyStage.Comparing(scale, design) =>
          ("comparing", scale, Vector("design" -> writeDesign(design)))
        case StudyStage.Reducing(scale, design) =>
          ("reducing", scale, Vector("design" -> writeDesign(design)))
        case StudyStage.Contrasting(scale) => ("contrasting", scale, Vector.empty)
      Json.obj(
        (Vector("kind" -> Json.fromString(kind), "scale" -> Json.fromInt(scale)) ++ detail :+
          ("meter" -> Json.obj(
            "unit"  -> Json.fromString(meter.unit.toString.toLowerCase),
            "done"  -> Json.fromString(meter.done.toString),
            "total" -> writeTotal(meter.total)
          )))*
      )

  private def readStage(json: Json): Either[CodecError, StudyRunStage] =
    Wire.field[String](json, "kind").flatMap {
      case "counting" =>
        for
          design  <- readDesign(json)
          visited <- long(json, "visited")
        yield StudyRunStage.Counting(design, visited)
      case kind =>
        val scientific = for
          scale <- Wire.field[Int](json, "scale")
          stage <- kind match
            case "estimating" =>
              Wire.field[Int](json, "trial").map(StudyStage.Estimating(scale, _))
            case "comparing"   => readDesign(json).map(StudyStage.Comparing(scale, _))
            case "reducing"    => readDesign(json).map(StudyStage.Reducing(scale, _))
            case "contrasting" => Right(StudyStage.Contrasting(scale))
            case other         =>
              Left(CodecError.Field("kind", Json.fromString(other), "unknown scientific stage"))
        yield stage
        for
          stage     <- scientific
          meterJson <- Wire.field[Json](json, "meter")
          rawUnit   <- Wire.field[String](meterJson, "unit")
          unit      <- CountUnit.values
            .find(_.toString.toLowerCase == rawUnit)
            .toRight(
              CodecError
                .Field("unit", Json.fromString(rawUnit), "unknown scientific count unit")
            )
          done  <- long(meterJson, "done")
          total <- Wire.field[Json](meterJson, "total").flatMap(readTotal)
          stageKind = stage match
            case StudyStage.Estimating(_, _) => StageKind.Estimating
            case StudyStage.Comparing(_, _)  => StageKind.Comparing
            case StudyStage.Reducing(_, _)   => StageKind.Reducing
            case StudyStage.Contrasting(_)   => StageKind.Contrasting
          meter <- StageMeter
            .of(stageKind, unit, done, total)
            .left
            .map(e => CodecError.Field("meter", meterJson, e.message))
        yield StudyRunStage.Running(stage, meter)
    }

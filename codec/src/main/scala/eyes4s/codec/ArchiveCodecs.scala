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
import eyes4s.compare.Similarity
import eyes4s.core.*
import eyes4s.design.SignedDifference
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg
import eyes4s.plan.*
import io.circe.Json

/** Versioned archive of one completed [[RecordingAnalysis]]
  * (`eyes4s.recording-result@1`).
  *
  * The archive embeds the plan the analysis ran (through the plan family's
  * codec, so the detector parameters keep their registered types) and every
  * stage's output: the synchronization evidence, the angular and prepared
  * recordings, the detection with its events, their sample support, labels,
  * report and provenance, and the area assignment with the areas, every
  * sample's membership and the time ledger.
  *
  * Decoding rebuilds the analysis through `RecordingAnalysis.reconstruct`
  * from the plan, the two recordings, the events with their support and the
  * areas, which re-derives the synchronization from the plan's marks, the
  * detection's labels, report and provenance from the events and samples, and
  * the memberships from the areas; every archived member the reconstruction
  * derives must then equal the derived value (`CodecError.Derived`). Nothing
  * numerical is re-run but the refit of the marks, the fixation summaries
  * `EventSeries.of` re-derives and the exact point-in-region assignment.
  */
final class RecordingResultCodec[P](
    val schema: DefinitionId,
    val plans: RecordingPlanCodec[P]
):
  val codec: VersionedCodec[RecordingAnalysis[P]] =
    VersionedCodec.checked(schema)(write)(read)

  private def write(value: RecordingAnalysis[P]): Either[CodecError, Json] = for
    plan  <- plans.codec.encode(value.plan)
    table <- DocumentIdentities.empty
      .addFrame(value.angular.frame)
      .map(_.addClock(value.angular.clock))
    angular   <- RecordingInputWire.writeRecording(value.angular)
    prepared  <- RecordingInputWire.writeRecording(value.prepared)
    detection <- RecordingResultWire.detection(value.detection).left.map(Wire.at("detection"))
  yield Json.obj(
    "method"          -> Wire.id(value.plan.method.id),
    "plan"            -> plan,
    "identities"      -> table.json,
    "synchronization" -> RecordingResultWire.synchronization(value.synchronization),
    "angular"         -> angular,
    "prepared"        -> prepared,
    "detection"       -> detection,
    "assignment"      -> RecordingResultWire.assignment(value.assignment)
  )

  private def read(json: Json): Either[CodecError, RecordingAnalysis[P]] = for
    _    <- Wire.requireId(json, "method", plans.method.id)
    plan <- Wire.field[Json](json, "plan").flatMap(plans.codec.decode).left.map(Wire.at("plan"))
    table     <- Wire.field[Json](json, "identities").flatMap(DocumentIdentities.read)
    angular   <- recording(json, "angular", table)
    prepared  <- recording(json, "prepared", table)
    detected  <- Wire.field[Json](json, "detection")
    rawEvents <- Wire.field[Vector[Json]](detected, "events").left.map(Wire.at("detection"))
    events    <- rawEvents.zipWithIndex.traverse { case (entry, index) =>
      RecordingResultWire
        .readEvent[Deg](entry, prepared.clock)
        .left
        .map(Wire.at(s"detection.events[$index]"))
    }
    assigned <- Wire.field[Json](json, "assignment")
    rawAreas <- Wire.field[Vector[Json]](assigned, "areas").left.map(Wire.at("assignment"))
    areas    <- rawAreas.zipWithIndex.traverse { case (entry, index) =>
      RecordingResultWire
        .readArea[Deg](entry, table)
        .left
        .map(Wire.at(s"assignment.areas[$index]"))
    }
    analysis <- RecordingAnalysis
      .reconstruct(plan, angular, prepared, events.map(_._1), events.map(_._2), areas)
      .left
      .map(CodecError.RecordingResult.apply)
    declaredSync <- Wire.field[Json](json, "synchronization")
    _            <- Derived.check(
      "synchronization",
      RecordingResultWire.synchronization(analysis.synchronization),
      declaredSync
    )
    detection <- RecordingResultWire
      .detection(analysis.detection)
      .left
      .map(Wire.at("detection"))
    _ <- Derived.check("detection", detection, detected)
    _ <- Derived.check(
      "assignment",
      RecordingResultWire.assignment(analysis.assignment),
      assigned
    )
  yield analysis

  private def recording(
      json: Json,
      field: String,
      table: DocumentIdentities
  ): Either[CodecError, Recording[Deg]] =
    Wire
      .field[Json](json, field)
      .flatMap(RecordingInputWire.readRecording[Deg](_, table))
      .left
      .map(Wire.at(field))

  def registration: RecordingResultRegistration =
    val registered = this
    new RecordingResultRegistration:
      val methodId: DefinitionId = registered.plans.method.id
      def decode(json: Json): Either[CodecError, LoadedRecordingResult] =
        registered.codec.decode(json).map { value =>
          new LoadedRecordingResult:
            type Parameters = P
            val analysis: RecordingAnalysis[P]   = value
            def encode: Either[CodecError, Json] = registered.codec.encode(value)
        }

/** A decoded recording analysis whose detector parameters stay abstract. */
trait LoadedRecordingResult:
  type Parameters
  val analysis: RecordingAnalysis[Parameters]
  def encode: Either[CodecError, Json]

sealed trait RecordingResultRegistration:
  val methodId: DefinitionId
  def decode(json: Json): Either[CodecError, LoadedRecordingResult]

/** Recording result codecs by recording method; lookup reads the archive's
  * `method` and refuses a missing or duplicate registration.
  */
final class RecordingResultRegistry private (
    val entries: Vector[RecordingResultRegistration]
):
  def register(
      entry: RecordingResultRegistration
  ): Either[CodecError, RecordingResultRegistry] =
    if entries.exists(_.methodId == entry.methodId) then
      Left(CodecError.DuplicateResultCodec(entry.methodId))
    else Right(new RecordingResultRegistry(entries :+ entry))

  def decode(json: Json): Either[CodecError, LoadedRecordingResult] = for
    payload    <- Wire.field[Json](json, "value")
    method     <- Wire.definition(payload, "method")
    registered <- entries
      .find(_.methodId == method)
      .toRight(CodecError.MissingResultCodec(method))
    result <- registered.decode(json)
  yield result

object RecordingResultRegistry:
  val empty: RecordingResultRegistry = new RecordingResultRegistry(Vector.empty)

/** Built-in temporal result archives. */
object TemporalResultCodecs:
  /** The ordinary cosine route over the temporal plan schema the caller supplies. */
  def cosine[U <: Unit2D: UnitLabel](
      planSchema: DefinitionId
  ): TemporalResultCodec[StudyKey, U, Unit, Similarity, SignedDifference] =
    new TemporalStudyCodec(planSchema, StudyCodecs.cosine[U])
      .results(StudyResultCodecs.similarity(), StudyResultCodecs.signedDifference())

/** Versioned archive of one completed [[TemporalStudyResult]]
  * (`eyes4s.temporal-result@1`).
  *
  * The archive embeds the temporal plan (through the plan family's codec)
  * and, per cell in plan order (repetitions outer, windows inner), the names
  * of its repetition and window, every trial's occupancy (its resolved
  * window, boundary, observed and missing time, the complete fixation ledger
  * and the retained fixations' positions) or the typed `TemporalStudyError`
  * the occupancy failed with, and the cell's completed study result in the
  * `study-result@1` wire.
  *
  * Decoding rebuilds every occupancy through `WindowOccupancy.reconstruct`
  * (weights and the measure digest re-derived from the ledger), every cell's
  * study result through the checked study reconstruction in the cell's
  * provenance context, and the whole through `TemporalStudyResult.reconstruct`,
  * which re-derives the cell layout, each repetition's plan and context from
  * the embedded plan and ties every density to its occupancy.
  */
final class TemporalResultCodec[K, U <: Unit2D, P, S, D](
    val schema: DefinitionId,
    val plans: TemporalStudyCodec[K, U, P, S, D],
    val scores: VersionedCodec[S],
    val differences: VersionedCodec[D]
)(using unit: UnitLabel[U]):
  private val results = plans.study.results(scores, differences)
  private val keys    = plans.study.keys

  val codec: VersionedCodec[TemporalStudyResult[K, U, P, S, D]] =
    VersionedCodec.checked(schema)(write)(read)

  private def write(value: TemporalStudyResult[K, U, P, S, D]): Either[CodecError, Json] =
    for
      plan  <- plans.codec.encode(value.plan)
      table <- value.cells
        .flatMap(_.occupancy)
        .collect { case (_, Right(occupancy)) => occupancy.measure.frame }
        .foldLeft[Either[CodecError, DocumentIdentities]](Right(DocumentIdentities.empty))(
          (table, frame) => table.flatMap(_.addFrame(frame))
        )
      cells <- value.cells.zipWithIndex.traverse { case (cell, index) =>
        writeCell(cell).left.map(Wire.at(s"cells[$index]"))
      }
    yield Json.obj(
      "method"           -> Wire.id(value.plan.base.method.id),
      "scoreSchema"      -> Wire.id(scores.schema),
      "differenceSchema" -> Wire.id(differences.schema),
      "plan"             -> plan,
      "identities"       -> table.json,
      "cells"            -> Json.arr(cells*)
    )

  private def writeCell(cell: TemporalCell[K, U, P, S, D]): Either[CodecError, Json] = for
    occupancy <- cell.occupancy.zipWithIndex.traverse { case ((key, outcome), index) =>
      keys
        .encode(key)
        .map(k =>
          Json.obj(
            "key"     -> k,
            "outcome" -> (outcome match
              case Right(value) => TemporalWire.occupancy(value)
              case Left(error)  =>
                ResultWire.tagged("failure", "error" -> TemporalWire.temporalStudyError(error)))
          )
        )
        .left
        .map(Wire.at(s"occupancy[$index]"))
    }
    result <- results.codec.encode(cell.result).left.map(Wire.at("result"))
  yield Json.obj(
    "repetition" -> Json.fromString(cell.repetition.name),
    "window"     -> Json.fromString(cell.window.name),
    "occupancy"  -> Json.arr(occupancy*),
    "result"     -> result
  )

  private def read(json: Json): Either[CodecError, TemporalStudyResult[K, U, P, S, D]] = for
    _    <- Wire.requireId(json, "method", plans.study.method.id)
    _    <- Wire.requireId(json, "scoreSchema", scores.schema)
    _    <- Wire.requireId(json, "differenceSchema", differences.schema)
    plan <- Wire.field[Json](json, "plan").flatMap(plans.codec.decode).left.map(Wire.at("plan"))
    table   <- Wire.field[Json](json, "identities").flatMap(DocumentIdentities.read)
    entries <- Wire.field[Vector[Json]](json, "cells")
    cells   <- entries.zipWithIndex.traverse { case (entry, index) =>
      readCell(plan, table, entry).left.map(Wire.at(s"cells[$index]"))
    }
    result <- TemporalStudyResult
      .reconstruct(plan, cells)
      .left
      .map(e => CodecError.TemporalResult(e))
  yield result

  private def readCell(
      plan: TemporalStudyPlan[K, U, P, S, D],
      table: DocumentIdentities,
      json: Json
  ): Either[CodecError, TemporalCellRecord[K, U, S, D]] = for
    repetitionName <- Wire.field[String](json, "repetition")
    windowName     <- Wire.field[String](json, "window")
    repetition     <- plan.repetitions
      .find(_.name == repetitionName)
      .toRight(
        CodecError.Field("repetition", json, s"the plan has no repetition $repetitionName")
      )
    window <- plan.windows
      .find(_.name == windowName)
      .toRight(CodecError.Field("window", json, s"the plan has no window $windowName"))
    rows      <- Wire.field[Vector[Json]](json, "occupancy")
    occupancy <- rows.zipWithIndex.traverse { case (row, index) =>
      readOccupancy(table, row).left.map(Wire.at(s"occupancy[$index]"))
    }
    result <- Wire
      .field[Json](json, "result")
      .flatMap(results.decodeIn(_, plan.provenanceContext(repetition, window)))
      .left
      .map(Wire.at("result"))
  yield TemporalCellRecord(repetitionName, windowName, occupancy, result)

  private def readOccupancy(
      table: DocumentIdentities,
      json: Json
  ): Either[CodecError, (K, Either[TemporalStudyError, WindowOccupancy[U]])] = for
    key     <- ResultWire.keyed(keys, json, "key")
    outcome <- Wire.field[Json](json, "outcome")
    value   <- ResultWire.kind(outcome).flatMap {
      case "occupancy" => TemporalWire.readOccupancy[U](outcome, table).map(Right(_))
      case "failure"   =>
        Wire
          .field[Json](outcome, "error")
          .flatMap(TemporalWire.readTemporalStudyError)
          .map(Left(_))
      case other => Left(ResultWire.unknown(outcome, "occupancy outcome", other))
    }
  yield key -> value

  def registration: TemporalResultRegistration[K, U] =
    val registered = this
    new TemporalResultRegistration[K, U]:
      val id: DefinitionId = registered.plans.study.method.id
      def decode(json: Json): Either[CodecError, LoadedTemporalResult[K, U]] =
        registered.codec.decode(json).map { value =>
          new LoadedTemporalResult[K, U]:
            type Parameters = P
            type Score      = S
            type Difference = D
            val result: TemporalStudyResult[K, U, P, S, D] = value
            def encode: Either[CodecError, Json]           = registered.codec.encode(value)
        }

/** A decoded temporal result whose parameter, score and difference types stay abstract. */
trait LoadedTemporalResult[K, U <: Unit2D]:
  type Parameters
  type Score
  type Difference
  val result: TemporalStudyResult[K, U, Parameters, Score, Difference]
  def encode: Either[CodecError, Json]

sealed trait TemporalResultRegistration[K, U <: Unit2D]:
  def id: DefinitionId
  def decode(json: Json): Either[CodecError, LoadedTemporalResult[K, U]]

/** Temporal result codecs by the method of their base study; lookup reads the
  * archive's `method` and refuses missing or duplicate registrations.
  */
final class TemporalResultRegistry[K, U <: Unit2D] private (
    val entries: Vector[TemporalResultRegistration[K, U]]
):
  def register(
      entry: TemporalResultRegistration[K, U]
  ): Either[CodecError, TemporalResultRegistry[K, U]] =
    if entries.exists(_.id == entry.id) then Left(CodecError.DuplicateResultCodec(entry.id))
    else Right(new TemporalResultRegistry(entries :+ entry))

  def decode(json: Json): Either[CodecError, LoadedTemporalResult[K, U]] = for
    payload    <- Wire.field[Json](json, "value")
    method     <- Wire.definition(payload, "method")
    registered <- entries.find(_.id == method).toRight(CodecError.MissingResultCodec(method))
    result     <- registered.decode(json)
  yield result

object TemporalResultRegistry:
  def empty[K, U <: Unit2D]: TemporalResultRegistry[K, U] =
    new TemporalResultRegistry(Vector.empty)

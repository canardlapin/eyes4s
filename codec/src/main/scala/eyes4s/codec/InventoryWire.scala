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
import eyes4s.plan.*
import io.circe.Json

/** Built-in identities introduced with the trial inventory. */
object InventoryDefinitions:
  /** The admission ledger that also records a trial inventory: every
    * inventory trial with its disposition, the trials only the fixation
    * table names, and the attributes of admitted records.
    */
  val admissionLedgerV3: DefinitionId = DefinitionId.builtIn("eyes4s.admission-ledger", 3)

/** The `inventory` member of a version-3 admission ledger. Integer attributes
  * are decimal strings, so every signed 64-bit value survives JavaScript.
  */
private[codec] object InventoryWire:
  def write(value: InventoryLedger): Either[CodecError, Json] =
    Right(
      Json.obj(
        "source" -> Json.obj(
          "label"   -> Json.fromString(value.source.label),
          "records" -> Json.fromString(value.source.records.digest)
        ),
        "header" -> Json.arr(value.header.map(Json.fromString)*),
        "trials" -> Json.arr(value.trials.map { t =>
          Json.fromFields(
            identity(t.identity) ++ Vector(
              "rows"          -> ints(t.rows),
              "inventoryItem" -> t.inventoryItem.fold(Json.Null)(Json.fromString),
              "attributes"    -> attributes(t.attributes),
              "recordItems"   -> Json.arr(t.recordItems.map(Json.fromString)*),
              "records"       -> ints(t.records),
              "disposition"   -> disposition(t.disposition)
            )
          )
        }*),
        "unlisted" -> Json.arr(value.unlisted.map { u =>
          Json.fromFields(
            identity(u.identity) ++ Vector(
              "recordItems" -> Json.arr(u.recordItems.map(Json.fromString)*),
              "records"     -> ints(u.records)
            )
          )
        }*),
        "recordAttributes" -> Json.arr(value.recordAttributes.map { entry =>
          Json.obj(
            "record"     -> Json.fromInt(entry.record),
            "attributes" -> attributes(entry.attributes)
          )
        }*)
      )
    )

  def read(json: Json): Either[CodecError, InventoryLedger] =
    for
      source <- Wire.field[Json](json, "source")
      label  <- Wire.field[String](source, "label")
      digest <- Wire.field[String](source, "records")
      ref    <- ArtifactRef
        .parse[Vector[Vector[String]]](digest)
        .left
        .map(CodecError.Definition.apply)
      header  <- Wire.field[Vector[String]](json, "header")
      entries <- Wire.field[Vector[Json]](json, "trials")
      trials  <- entries.zipWithIndex.traverse { (entry, index) =>
        (for
          id            <- readIdentity(entry)
          rows          <- Wire.field[Vector[Int]](entry, "rows")
          inventoryItem <- Wire.field[Option[String]](entry, "inventoryItem")
          values        <- Wire.field[Json](entry, "attributes").flatMap(readAttributes)
          recordItems   <- Wire.field[Vector[String]](entry, "recordItems")
          records       <- Wire.field[Vector[Int]](entry, "records")
          outcome       <- Wire.field[Json](entry, "disposition").flatMap(readDisposition)
        yield InventoryTrial(
          id,
          rows,
          inventoryItem,
          values,
          recordItems,
          records,
          outcome
        )).left
          .map(Wire.at(s"trials[$index]"))
      }
      others   <- Wire.field[Vector[Json]](json, "unlisted")
      unlisted <- others.zipWithIndex.traverse { (entry, index) =>
        (for
          id          <- readIdentity(entry)
          recordItems <- Wire.field[Vector[String]](entry, "recordItems")
          records     <- Wire.field[Vector[Int]](entry, "records")
        yield UnlistedTrial(id, recordItems, records)).left.map(Wire.at(s"unlisted[$index]"))
      }
      listed           <- Wire.field[Vector[Json]](json, "recordAttributes")
      recordAttributes <- listed.zipWithIndex.traverse { (entry, index) =>
        (for
          record <- Wire.field[Int](entry, "record")
          values <- Wire.field[Json](entry, "attributes").flatMap(readAttributes)
        yield RecordAttributes(record, values)).left.map(Wire.at(s"recordAttributes[$index]"))
      }
      ledger <- InventoryLedger
        .of(SourceRef(label, ref), header, trials, unlisted, recordAttributes)
        .left
        .map(e => CodecError.Admission(AdmissionError.Inventory(e)))
    yield ledger

  private def ints(values: Vector[Int]): Json = Json.arr(values.map(Json.fromInt)*)

  private def identity(id: TrialIdentity): Vector[(String, Json)] = Vector(
    "participant" -> Json.fromString(id.participant),
    "phase"       -> Json.fromString(id.phase),
    "trial"       -> Json.fromString(id.trial),
    "occurrence"  -> Json.fromInt(id.occurrence.value)
  )

  private def readIdentity(json: Json): Either[CodecError, TrialIdentity] =
    for
      participant <- Wire.field[String](json, "participant")
      phase       <- Wire.field[String](json, "phase")
      trial       <- Wire.field[String](json, "trial")
      number      <- Wire.field[Int](json, "occurrence")
      occurrence  <- TrialOccurrence.of(number).left.map(CodecError.Definition.apply)
      id          <- TrialIdentity
        .of(participant, phase, trial, occurrence)
        .left
        .map(CodecError.Definition.apply)
    yield id

  private def attributes(values: Attributes): Json =
    Json.arr(values.entries.map { (name, value) =>
      val (kind, member) = value match
        case AttributeValue.Text(text) => "text"    -> Some(Json.fromString(text))
        case AttributeValue.Integer(n) => "integer" -> Some(Json.fromString(n.toString))
        case AttributeValue.Number(x)  => "number"  -> Some(Json.fromDoubleOrNull(x))
        case AttributeValue.Blank      => "blank"   -> None
      Json.fromFields(
        Vector("name" -> Json.fromString(name), "kind" -> Json.fromString(kind)) ++
          member.map("value" -> _)
      )
    }*)

  private def readAttributes(json: Json): Either[CodecError, Attributes] =
    for
      entries <- json.asArray
        .map(_.toVector)
        .toRight(CodecError.Field("attributes", json, "expected an array"))
      values <- entries.zipWithIndex.traverse { (entry, index) =>
        (for
          name  <- Wire.field[String](entry, "name")
          kind  <- Wire.field[String](entry, "kind")
          value <- kind match
            case "text"    => Wire.field[String](entry, "value").map(AttributeValue.Text.apply)
            case "integer" =>
              Wire
                .field[String](entry, "value")
                .flatMap(raw =>
                  raw.toLongOption
                    .filter(_.toString == raw)
                    .map(AttributeValue.Integer.apply)
                    .toRight(
                      CodecError.Field("value", entry, s"'$raw' is not a canonical integer")
                    )
                )
            case "number" => DomainWire.finite(entry, "value").map(AttributeValue.Number.apply)
            case "blank"  => Right(AttributeValue.Blank)
            case other    =>
              Left(CodecError.Field("kind", entry, s"unknown attribute kind $other"))
        yield name -> value).left.map(Wire.at(s"attributes[$index]"))
      }
      result <- Attributes
        .of(values)
        .left
        .map(e => CodecError.Admission(AdmissionError.Inventory(e)))
    yield result

  private def disposition(value: TrialDisposition): Json = value match
    case TrialDisposition.Admitted           => Json.obj("kind" -> Json.fromString("admitted"))
    case TrialDisposition.Quarantined(cause) =>
      Json.obj(
        "kind"  -> Json.fromString("quarantined"),
        "cause" -> StudyInputCodec.cause(cause)
      )
    case TrialDisposition.NoFixations => Json.obj("kind" -> Json.fromString("noFixations"))
    case TrialDisposition.Absent      => Json.obj("kind" -> Json.fromString("absent"))

  private def readDisposition(json: Json): Either[CodecError, TrialDisposition] =
    Wire.field[String](json, "kind").flatMap {
      case "admitted"    => Right(TrialDisposition.Admitted)
      case "quarantined" =>
        Wire
          .field[Json](json, "cause")
          .flatMap(StudyInputCodec.readCause)
          .map(TrialDisposition.Quarantined.apply)
      case "noFixations" => Right(TrialDisposition.NoFixations)
      case "absent"      => Right(TrialDisposition.Absent)
      case other => Left(CodecError.Field("kind", json, s"unknown trial disposition $other"))
    }

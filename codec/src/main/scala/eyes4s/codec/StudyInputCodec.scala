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
import io.circe.Json

/** Built-in schemas for the fixation-study input route. */
object StudyInputCodecs:
  val input: DefinitionId    = DefinitionId.studyInput
  val trials: DefinitionId   = DefinitionId.trials
  val scanpath: DefinitionId = DefinitionId.scanpath
  val ledger: DefinitionId   = DefinitionId.admissionLedger

  /** The ordinary participant/stimulus/phase route, matching `StudyCodecs.cosine`. */
  def study[U <: Unit2D: UnitLabel]: StudyInputCodec[StudyKey, U] =
    new StudyInputCodec(
      input,
      ledger,
      StudyKey.layout(DefinitionId.studyLayout),
      StudyCodecs.key(DefinitionId.studyKey)
    )

/** Versioned codecs for one key layout: the fixation-study input payload and
  * its admission ledger. Trials keep input order and typed keys through the
  * registered key codec; frames and clocks are declared once in a document
  * identity table and referenced by nominal ID. Decoding rebuilds the input
  * through `StudyInput` and refuses a payload whose declared digest differs
  * from the reconstructed one. A source-supported scanpath embeds its exact
  * recording and half-open sample ranges and is rebuilt through
  * `EventSeries.of`, so its summaries are re-derived from the samples rather
  * than trusted; a summary recomputed under a warp remains refused.
  */
final class StudyInputCodec[K, U <: Unit2D](
    val schema: DefinitionId,
    val ledgerSchema: DefinitionId,
    val layout: StudyLayout[K],
    val keys: VersionedCodec[K]
)(using unit: UnitLabel[U]):
  private given KeyDigest[K] = layout.digest

  val input: VersionedCodec[StudyInput[K, U]] =
    VersionedCodec.checked(schema)(writeInput)(readInput)

  val ledger: VersionedCodec[AdmissionLedger[K]] =
    VersionedCodec.checked(ledgerSchema)(writeLedger)(readLedger)

  private def rows(table: DocumentIdentities): VersionedCodec[Trials[K, Unit, Scanpath[U]]] =
    VersionedCodec.trials(
      StudyInputCodecs.trials,
      keys,
      VersionedCodec.unit(DefinitionId.unit),
      scanpaths(table)
    )

  private def writeInput(value: StudyInput[K, U]): Either[CodecError, Json] = for
    table <- value.trials.rows.zipWithIndex
      .foldLeft[Either[CodecError, DocumentIdentities]](Right(DocumentIdentities.empty)) {
        case (acc, (trial, index)) =>
          acc.flatMap(t =>
            t.addFrame(trial.value.frame)
              .map(_.addClock(trial.value.clock))
              .left
              .map(Wire.at(s"trials.rows[$index]"))
          )
      }
    trials <- rows(table).encode(value.trials).left.map(Wire.at("trials"))
  yield Json.obj(
    "layout"     -> Wire.id(layout.id),
    "keySchema"  -> Wire.id(keys.schema),
    "unit"       -> Json.fromString(unit.symbol),
    "input"      -> Json.fromString(value.reference.digest),
    "identities" -> table.json,
    "trials"     -> trials
  )

  private def readInput(json: Json): Either[CodecError, StudyInput[K, U]] = for
    _      <- Wire.requireId(json, "layout", layout.id)
    _      <- Wire.requireId(json, "keySchema", keys.schema)
    symbol <- Wire.field[String](json, "unit")
    _      <- Either.cond(
      symbol == unit.symbol,
      (),
      CodecError.Field("unit", json, s"expected ${unit.symbol}, got $symbol")
    )
    declared <- Wire.field[String](json, "input")
    _     <- ArtifactRef.parse[StudyInput[K, U]](declared).left.map(CodecError.Definition.apply)
    table <- Wire.field[Json](json, "identities").flatMap(DocumentIdentities.read)
    trials <- Wire
      .field[Json](json, "trials")
      .flatMap(rows(table).decode)
      .left
      .map(Wire.at("trials"))
    value = StudyInput(trials)
    _ <- Either.cond(
      value.reference.digest == declared,
      (),
      CodecError.InputIdentity(declared, value.reference.digest)
    )
  yield value

  private def scanpaths(table: DocumentIdentities): VersionedCodec[Scanpath[U]] =
    VersionedCodec.checked[Scanpath[U]](StudyInputCodecs.scanpath)(path =>
      for
        fixations <- path.fixations.toVector.zipWithIndex
          .traverse { case (f, index) => fixation(f).left.map(Wire.at(s"fixations[$index]")) }
        source <- (path.source, path.sourceRecording, path.sampleSupport) match
          case (Some(ref), Some(recording), Some(support)) =>
            RecordingInputWire
              .writeRecording(recording)
              .map(body =>
                Json.obj(
                  "ref"       -> Json.fromString(ref.value),
                  "recording" -> body,
                  "support"   -> Json.arr(
                    support.map(range =>
                      Json.obj(
                        "from"  -> Json.fromInt(range.from),
                        "until" -> Json.fromInt(range.until)
                      )
                    )*
                  )
                )
              )
              .map(Some(_))
              .left
              .map(Wire.at("source"))
          case _ => Right(None)
      yield Json.fromFields(
        Vector(
          "frame"     -> Json.fromString(path.frame.id.name),
          "clock"     -> Json.fromString(path.clock.name),
          "fixations" -> Json.arr(fixations*)
        ) ++ source.map("source" -> _)
      )
    ) { json =>
      for
        frameName <- Wire.field[String](json, "frame")
        frame     <- table.frame[U](FrameId(frameName))
        clockName <- Wire.field[String](json, "clock")
        clock     <- table.clock(ClockId(clockName))
        entries   <- Wire.field[Vector[Json]](json, "fixations")
        fixations <- entries.zipWithIndex.traverse { case (entry, index) =>
          readFixation(entry, clock).left.map(Wire.at(s"fixations[$index]"))
        }
        sourceJson <- Wire.field[Option[Json]](json, "source")
        path       <- sourceJson match
          case None =>
            Scanpath
              .of(frame, clock, IArray.from(fixations))
              .left
              .map(e => CodecError.Field("fixations", json, e.message))
          case Some(source) =>
            readSourceSupported(source, table, frame, clock, fixations).left.map(
              Wire.at("source")
            )
      yield path
    }

  /** Rebuild a source-supported scanpath from its recording and sample
    * ranges; the declared fixation summaries must equal the ones the source
    * samples derive, so a payload cannot detach a summary from its evidence.
    */
  private def readSourceSupported(
      json: Json,
      table: DocumentIdentities,
      frame: Frame[U],
      clock: ClockId,
      declared: Vector[Event.Fixation[U]]
  ): Either[CodecError, Scanpath[U]] =
    for
      ref       <- Wire.field[String](json, "ref")
      recording <- Wire
        .field[Json](json, "recording")
        .flatMap(RecordingInputWire.readRecording[U](_, table))
        .left
        .map(Wire.at("recording"))
      _ <- Either.cond(
        recording.frame.id == frame.id && recording.clock == clock,
        (),
        CodecError.Field(
          "recording",
          json,
          s"source recording is in frame ${recording.frame.id.name} on clock ${recording.clock.name}, " +
            s"but the scanpath is in frame ${frame.id.name} on clock ${clock.name}"
        )
      )
      raw     <- Wire.field[Vector[Json]](json, "support")
      support <- raw.zipWithIndex.traverse { case (range, index) =>
        (for
          from  <- Wire.field[Int](range, "from")
          until <- Wire.field[Int](range, "until")
          value <- SampleRange.of(from, until).left.map(CodecError.Support("support", _))
        yield value).left.map(Wire.at(s"support[$index]"))
      }
      series <- EventSeries
        .of(recording, RecordingRef(ref), declared.map(f => f: Event[U]), support)
        .left
        .map(CodecError.Support("support", _))
      path <- Scanpath
        .fromEvents(series)
        .left
        .map(e => CodecError.Field("fixations", json, e.message))
      _ <- path.fixations.toVector.zip(declared).zipWithIndex.traverse {
        case ((derived, expected), index) =>
          Either.cond(
            derived.span == expected.span && derived.centre == expected.centre &&
              derived.sampleCount == expected.sampleCount &&
              derived.dispersion == expected.dispersion,
            (),
            CodecError.Field(
              s"fixations[$index]",
              json,
              s"source samples derive centre=(${derived.centre.x}, ${derived.centre.y}) " +
                s"sampleCount=${derived.sampleCount} dispersion=${derived.dispersion}, " +
                s"but the payload declares centre=(${expected.centre.x}, ${expected.centre.y}) " +
                s"sampleCount=${expected.sampleCount} dispersion=${expected.dispersion}"
            )
          )
      }
    yield path

  private def fixation(f: Event.Fixation[U]): Either[CodecError, Json] =
    val base = Json.obj(
      "onsetMicros"  -> DomainWire.time(f.span.onset.toMicros),
      "offsetMicros" -> DomainWire.time(f.span.offset.toMicros),
      "x"            -> Json.fromDoubleOrNull(f.centre.x),
      "y"            -> Json.fromDoubleOrNull(f.centre.y),
      "sampleCount"  -> Json.fromInt(f.sampleCount)
    )
    f.dispersionStatus match
      case DispersionStatus.Unavailable(DispersionUnavailable.NotReported) => Right(base)
      case DispersionStatus.Available(value, SummaryEvidence.Declared)     =>
        Right(
          base.mapObject(
            _.add(
              "dispersion",
              Json.obj(
                "value"  -> Json.fromDoubleOrNull(value.value),
                "method" -> Json.fromString(StudyInputCodec.dispersionMethods(value.method))
              )
            )
          )
        )
      case DispersionStatus.Available(value, SummaryEvidence.SourceSupported(_, _)) =>
        Right(
          base.mapObject(
            _.add(
              "dispersion",
              Json.obj(
                "value"  -> Json.fromDoubleOrNull(value.value),
                "method" -> Json.fromString(StudyInputCodec.dispersionMethods(value.method))
              )
            )
          )
        )
      case DispersionStatus.Available(_, evidence: SummaryEvidence.Recomputed) =>
        Left(
          CodecError.Unsupported(
            "dispersion",
            s"evidence $evidence records a warp of source samples; a recomputed summary is a " +
              "result of transformation and is not an input payload"
          )
        )
      case DispersionStatus.Unavailable(reason) =>
        Left(
          CodecError.Unsupported(
            "dispersion",
            s"status $reason depends on a source transform that belongs to the recording payload"
          )
        )

  private def readFixation(json: Json, clock: ClockId): Either[CodecError, Event.Fixation[U]] =
    for
      onset  <- DomainWire.micros(json, "onsetMicros")
      offset <- DomainWire.micros(json, "offsetMicros")
      x      <- DomainWire.finite(json, "x")
      y      <- DomainWire.finite(json, "y")
      count  <- Wire.field[Int](json, "sampleCount")
      span   <- Interval
        .of(clock, Instant.micros(onset), Instant.micros(offset))
        .left
        .map(e => CodecError.Field("span", json, e.message))
      fixation <- json.asObject.flatMap(_.apply("dispersion")) match
        case None =>
          Event.Fixation
            .withoutDispersion(span, Pt[U](x, y), count)
            .left
            .map(e => CodecError.Field("fixation", json, e.message))
        case Some(spread) =>
          for
            value  <- DomainWire.finite(spread, "value")
            name   <- Wire.field[String](spread, "method")
            method <- StudyInputCodec.dispersionMethods
              .collectFirst { case (m, n) if n == name => m }
              .toRight(CodecError.Field("method", spread, s"unknown dispersion method $name"))
            result <- Event.Fixation
              .of(span, Pt[U](x, y), value, method, count)
              .left
              .map(e => CodecError.Field("fixation", json, e.message))
          yield result
    yield fixation

  private def writeLedger(ledger: AdmissionLedger[K]): Either[CodecError, Json] =
    ledger.records.zipWithIndex
      .traverse { case (SourceRecord(number, disposition), index) =>
        (disposition match
          case Disposition.Admitted(key, ordinal) =>
            keys
              .encode(key)
              .map(k =>
                Json.obj(
                  "record"  -> Json.fromInt(number),
                  "kind"    -> Json.fromString("admitted"),
                  "key"     -> k,
                  "ordinal" -> Json.fromInt(ordinal)
                )
              )
          case Disposition.Rejected(raw, key, reason) =>
            key
              .traverse(keys.encode)
              .map(k =>
                Json.obj(
                  "record" -> Json.fromInt(number),
                  "kind"   -> Json.fromString("rejected"),
                  "raw"    -> Json.arr(raw.map(Json.fromString)*),
                  "key"    -> k.getOrElse(Json.Null),
                  "reason" -> StudyInputCodec.reason(reason)
                )
              )
        ).left.map(Wire.at(s"records[$index]"))
      }
      .map(records =>
        Json.obj(
          "keySchema" -> Wire.id(keys.schema),
          "source"    -> Json.obj(
            "label"   -> Json.fromString(ledger.source.label),
            "records" -> Json.fromString(ledger.source.records.digest)
          ),
          "header"  -> Json.arr(ledger.header.map(Json.fromString)*),
          "outcome" -> Json.fromString(StudyInputCodec.outcomes.toMap.apply(ledger.outcome)),
          "records" -> Json.arr(records*)
        )
      )

  private def readLedger(json: Json): Either[CodecError, AdmissionLedger[K]] = for
    _      <- Wire.requireId(json, "keySchema", keys.schema)
    source <- Wire.field[Json](json, "source")
    label  <- Wire.field[String](source, "label")
    digest <- Wire.field[String](source, "records")
    ref    <- ArtifactRef
      .parse[Vector[Vector[String]]](digest)
      .left
      .map(CodecError.Definition.apply)
    header  <- Wire.field[Vector[String]](json, "header")
    name    <- Wire.field[String](json, "outcome")
    outcome <- StudyInputCodec.outcomes
      .collectFirst { case (o, n) if n == name => o }
      .toRight(CodecError.Field("outcome", json, s"unknown admission outcome $name"))
    entries <- Wire.field[Vector[Json]](json, "records")
    records <- entries.zipWithIndex.traverse { case (entry, index) =>
      readRecord(entry).left.map(Wire.at(s"records[$index]"))
    }
    ledger <- AdmissionLedger
      .of(SourceRef(label, ref), header, records, outcome)
      .left
      .map(CodecError.Admission.apply)
  yield ledger

  private def readRecord(json: Json): Either[CodecError, SourceRecord[K]] = for
    number      <- Wire.field[Int](json, "record")
    kind        <- Wire.field[String](json, "kind")
    disposition <- kind match
      case "admitted" =>
        for
          key     <- Wire.field[Json](json, "key").flatMap(keys.decode)
          ordinal <- Wire.field[Int](json, "ordinal")
        yield Disposition.Admitted(key, ordinal)
      case "rejected" =>
        for
          raw     <- Wire.field[Vector[String]](json, "raw")
          keyJson <- Wire.field[Json](json, "key")
          key     <- if keyJson.isNull then Right(None) else keys.decode(keyJson).map(Some(_))
          reason  <- Wire.field[Json](json, "reason").flatMap(StudyInputCodec.readReason)
        yield Disposition.Rejected(raw, key, reason)
      case other => Left(CodecError.Field("kind", json, s"unknown disposition $other"))
  yield SourceRecord(number, disposition)

private[codec] object StudyInputCodec:
  val outcomes: Vector[(AdmissionOutcome, String)] = Vector(
    AdmissionOutcome.Complete           -> "complete",
    AdmissionOutcome.Refused            -> "refused",
    AdmissionOutcome.ReviewedExclusions -> "reviewedExclusions"
  )

  def reason(value: AdmissionReason): Json = value match
    case AdmissionReason.Width(expected, actual) =>
      Json.obj(
        "kind"     -> Json.fromString("width"),
        "expected" -> Json.fromInt(expected),
        "actual"   -> Json.fromInt(actual)
      )
    case AdmissionReason.Key(reason) =>
      Json.obj("kind" -> Json.fromString("key"), "reason" -> Json.fromString(reason))
    case AdmissionReason.Number(column, text, requirement) =>
      Json.obj(
        "kind"        -> Json.fromString("number"),
        "column"      -> Json.fromString(column),
        "value"       -> Json.fromString(text),
        "requirement" -> Json.fromString(requirement)
      )
    case AdmissionReason.Time(onset, duration, unit, reason) =>
      Json.obj(
        "kind"     -> Json.fromString("time"),
        "onset"    -> Json.fromString(onset),
        "duration" -> Json.fromString(duration),
        "unit"     -> Json.fromString(unit),
        "reason"   -> Json.fromString(reason)
      )
    case AdmissionReason.Position(x, y, frame) =>
      Json.obj(
        "kind"  -> Json.fromString("position"),
        "x"     -> Json.fromDoubleOrNull(x),
        "y"     -> Json.fromDoubleOrNull(y),
        "frame" -> Json.fromString(frame.name)
      )
    case AdmissionReason.Event(reason) =>
      Json.obj("kind" -> Json.fromString("event"), "reason" -> Json.fromString(reason))
    case AdmissionReason.Quarantined(records, cause) =>
      Json.obj(
        "kind"    -> Json.fromString("quarantined"),
        "records" -> Json.arr(records.map(Json.fromInt)*),
        "cause"   -> (cause match
          case QuarantineCause.RejectedRecords =>
            Json.obj("kind" -> Json.fromString("rejectedRecords"))
          case QuarantineCause.DuplicateOrdinals =>
            Json.obj("kind" -> Json.fromString("duplicateOrdinals"))
          case QuarantineCause.Overlap(index, previous, current) =>
            Json.obj(
              "kind"     -> Json.fromString("overlap"),
              "index"    -> Json.fromInt(index),
              "previous" -> Json.fromString(previous),
              "current"  -> Json.fromString(current)
            )
          case QuarantineCause.NoFixations =>
            Json.obj("kind" -> Json.fromString("noFixations"))
          case QuarantineCause.WrongClock(index, expected, actual) =>
            Json.obj(
              "kind"     -> Json.fromString("wrongClock"),
              "index"    -> Json.fromInt(index),
              "expected" -> Json.fromString(expected),
              "actual"   -> Json.fromString(actual)
            )
          case QuarantineCause.InvalidTransition(index, reason) =>
            Json.obj(
              "kind"   -> Json.fromString("invalidTransition"),
              "index"  -> Json.fromInt(index),
              "reason" -> Json.fromString(reason)
            )
          case QuarantineCause.InvalidExtent(reason) =>
            Json.obj(
              "kind"   -> Json.fromString("invalidExtent"),
              "reason" -> Json.fromString(reason)
            )
          case QuarantineCause.UnmappableFixation(index, from, to, x, y) =>
            Json.obj(
              "kind"  -> Json.fromString("unmappableFixation"),
              "index" -> Json.fromInt(index),
              "from"  -> Json.fromString(from.name),
              "to"    -> Json.fromString(to.name),
              "x"     -> Json.fromDoubleOrNull(x),
              "y"     -> Json.fromDoubleOrNull(y)
            ))
      )

  /** Explicit wire names: an enum rename cannot change the format. */
  val dispersionMethods: Map[DispersionMethod, String] = Map(
    DispersionMethod.RmsRadius               -> "rmsRadius",
    DispersionMethod.BoundingBoxWidth        -> "boundingBoxWidth",
    DispersionMethod.BoundingBoxDiagonal     -> "boundingBoxDiagonal",
    DispersionMethod.MedianAbsoluteDeviation -> "medianAbsoluteDeviation"
  )

  def readReason(json: Json): Either[CodecError, AdmissionReason] =
    Wire.field[String](json, "kind").flatMap {
      case "width" =>
        for
          expected <- Wire.field[Int](json, "expected")
          actual   <- Wire.field[Int](json, "actual")
        yield AdmissionReason.Width(expected, actual)
      case "key"    => Wire.field[String](json, "reason").map(AdmissionReason.Key.apply)
      case "number" =>
        for
          column      <- Wire.field[String](json, "column")
          text        <- Wire.field[String](json, "value")
          requirement <- Wire.field[String](json, "requirement")
        yield AdmissionReason.Number(column, text, requirement)
      case "time" =>
        for
          onset    <- Wire.field[String](json, "onset")
          duration <- Wire.field[String](json, "duration")
          unit     <- Wire.field[String](json, "unit")
          reason   <- Wire.field[String](json, "reason")
        yield AdmissionReason.Time(onset, duration, unit, reason)
      case "position" =>
        for
          x     <- DomainWire.finite(json, "x")
          y     <- DomainWire.finite(json, "y")
          frame <- Wire.field[String](json, "frame")
        yield AdmissionReason.Position(x, y, FrameId(frame))
      case "event"       => Wire.field[String](json, "reason").map(AdmissionReason.Event.apply)
      case "quarantined" =>
        for
          records <- Wire.field[Vector[Int]](json, "records")
          cause   <- Wire.field[Json](json, "cause")
          kind    <- Wire.field[String](cause, "kind")
          value   <- kind match
            case "rejectedRecords"   => Right(QuarantineCause.RejectedRecords)
            case "duplicateOrdinals" => Right(QuarantineCause.DuplicateOrdinals)
            case "overlap"           =>
              for
                index    <- Wire.field[Int](cause, "index")
                previous <- Wire.field[String](cause, "previous")
                current  <- Wire.field[String](cause, "current")
              yield QuarantineCause.Overlap(index, previous, current)
            case "noFixations" => Right(QuarantineCause.NoFixations)
            case "wrongClock"  =>
              for
                index    <- Wire.field[Int](cause, "index")
                expected <- Wire.field[String](cause, "expected")
                actual   <- Wire.field[String](cause, "actual")
              yield QuarantineCause.WrongClock(index, expected, actual)
            case "invalidTransition" =>
              for
                index  <- Wire.field[Int](cause, "index")
                reason <- Wire.field[String](cause, "reason")
              yield QuarantineCause.InvalidTransition(index, reason)
            case "invalidExtent" =>
              Wire.field[String](cause, "reason").map(QuarantineCause.InvalidExtent.apply)
            case "unmappableFixation" =>
              for
                index <- Wire.field[Int](cause, "index")
                from  <- Wire.field[String](cause, "from")
                to    <- Wire.field[String](cause, "to")
                x     <- DomainWire.finite(cause, "x")
                y     <- DomainWire.finite(cause, "y")
              yield QuarantineCause.UnmappableFixation(index, FrameId(from), FrameId(to), x, y)
            case other =>
              Left(CodecError.Field("kind", cause, s"unknown quarantine cause $other"))
        yield AdmissionReason.Quarantined(records, value)
      case other => Left(CodecError.Field("kind", json, s"unknown admission reason $other"))
    }

/** Registered key schemas for input and ledger payloads; lookup reads the
  * payload's `keySchema` and refuses unknown or duplicate registrations.
  */
final class StudyInputRegistry[K, U <: Unit2D] private (
    val entries: Vector[StudyInputCodec[K, U]]
):
  def register(entry: StudyInputCodec[K, U]): Either[CodecError, StudyInputRegistry[K, U]] =
    if entries.exists(_.keys.schema == entry.keys.schema) then
      Left(CodecError.DuplicateKeySchema(entry.keys.schema))
    else Right(new StudyInputRegistry(entries :+ entry))

  private def lookup(json: Json): Either[CodecError, StudyInputCodec[K, U]] = for
    payload <- Wire.field[Json](json, "value")
    schema  <- Wire.definition(payload, "keySchema")
    entry <- entries.find(_.keys.schema == schema).toRight(CodecError.MissingKeySchema(schema))
  yield entry

  def decodeInput(json: Json): Either[CodecError, StudyInput[K, U]] =
    lookup(json).flatMap(_.input.decode(json))

  def decodeLedger(json: Json): Either[CodecError, AdmissionLedger[K]] =
    lookup(json).flatMap(_.ledger.decode(json))

object StudyInputRegistry:
  def empty[K, U <: Unit2D]: StudyInputRegistry[K, U] = new StudyInputRegistry(Vector.empty)

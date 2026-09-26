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

/** Built-in identities introduced with the admission policy. */
object StudyInputDefinitions:
  /** The admission ledger with its admission policy (off-screen handling
    * and coordinate corrections) and the admitted records outside the frame.
    */
  val admissionLedgerV2: DefinitionId = DefinitionId.builtIn("eyes4s.admission-ledger", 2)

/** Built-in schemas for the fixation-study input route. */
object StudyInputCodecs:
  val input: DefinitionId    = DefinitionId.studyInput
  val trials: DefinitionId   = DefinitionId.trials
  val scanpath: DefinitionId = DefinitionId.scanpath
  val ledger: DefinitionId   = DefinitionId.admissionLedger

  /** The trial-keyed route, matching `StudyCodecs.trialCosine`. */
  def trial[U <: Unit2D: UnitLabel]: StudyInputCodec[TrialKey, U] =
    new StudyInputCodec(
      input,
      ledger,
      TrialKey.layout(TrialKeyDefinitions.trialLayout),
      StudyCodecs.trialKey(TrialKeyDefinitions.trialKey)
    )

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

  /** The ledger schema that also records the admission policy. */
  val ledgerSchemaV2: DefinitionId =
    DefinitionId.builtIn(ledgerSchema.name, ledgerSchema.version + 1)

  /** The ledger schema that also records a trial inventory. */
  val ledgerSchemaV3: DefinitionId =
    DefinitionId.builtIn(ledgerSchema.name, ledgerSchema.version + 2)

  /** Reads all three ledger versions; a version-1 ledger is an admission
    * under `AdmissionPolicy.version1`, and only a version-3 ledger carries a
    * trial inventory. A ledger is written under the earliest version that
    * expresses it (`AdmissionLedger.version`), so a version-1 or version-2
    * ledger re-encodes to its own bytes.
    */
  val ledger: VersionedCodec[AdmissionLedger[K]] =
    VersionedCodec
      .versions[AdmissionLedger[K]]("admission ledger", ledgerSchema, ledgerSchema.version + 2)(
        value =>
          writeLedger(value).flatMap(json =>
            value.version match
              case 1 => Right(ledgerSchema -> json)
              case 2 =>
                writePolicy(value).map(policy => ledgerSchemaV2 -> Wire.append(json, policy))
              case _ =>
                for
                  policy    <- writePolicy(value)
                  inventory <- value.inventory
                    .toRight(
                      CodecError.Unsupported(
                        "inventory",
                        "only a ledger with a trial inventory is written as version 3"
                      )
                    )
                    .flatMap(inventory =>
                      trialIdentity
                        .toRight(noTrialProjection)
                        .flatMap(_ => InventoryWire.write(inventory))
                        .left
                        .map(Wire.at("inventory"))
                    )
                yield ledgerSchemaV3 -> Wire.append(
                  Wire.append(json, policy),
                  Json.obj("inventory" -> inventory)
                )
          )
      )((version, json) => readLedger(json, version.version - ledgerSchema.version + 1))

  /** A trial's identity under this layout, when the layout names a trial. */
  private val trialIdentity: Option[K => TrialIdentity] = TrialIdentity.projection(layout)

  private def noTrialProjection: CodecError =
    CodecError.Admission(AdmissionError.Inventory(InventoryError.NoTrialProjection(layout.id)))

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
          .traverse { case (f, index) =>
            fixation(f, path.source.isDefined).left.map(Wire.at(s"fixations[$index]"))
          }
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
        frameName  <- Wire.field[String](json, "frame")
        frame      <- table.frame[U](FrameId(frameName))
        clockName  <- Wire.field[String](json, "clock")
        clock      <- table.clock(ClockId(clockName))
        entries    <- Wire.field[Vector[Json]](json, "fixations")
        sourceJson <- Wire.field[Option[Json]](json, "source")
        fixations  <- entries.zipWithIndex.traverse { case (entry, index) =>
          readFixation(entry, clock, sourceJson.isDefined).left
            .map(Wire.at(s"fixations[$index]"))
        }
        path <- sourceJson match
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
    * ranges. The declared spans, centres and sample counts must equal the
    * ones the source samples derive, so a payload cannot detach a summary
    * from its evidence. Dispersion is carried by method only: its value is
    * re-derived from the samples, because the spread statistics use `hypot`
    * and `pow`, whose rounding is not identical across platforms.
    */
  private def readSourceSupported(
      json: Json,
      table: DocumentIdentities,
      frame: Frame[U],
      clock: ClockId,
      declared: Vector[Event.Fixation[U]]
  ): Either[CodecError, Scanpath[U]] =
    for
      ref <- Wire.field[String](json, "ref")
      _   <- Either.cond(
        ref.trim.nonEmpty,
        (),
        CodecError.Field("ref", json, "a source recording needs a non-empty nominal name")
      )
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
              derived.dispersion.map(_.method) == expected.dispersion.map(_.method),
            (),
            CodecError.Field(
              s"fixations[$index]",
              Json.obj(
                "onsetMicros"  -> DomainWire.time(expected.span.onset.toMicros),
                "offsetMicros" -> DomainWire.time(expected.span.offset.toMicros),
                "x"            -> Json.fromDoubleOrNull(expected.centre.x),
                "y"            -> Json.fromDoubleOrNull(expected.centre.y),
                "sampleCount"  -> Json.fromInt(expected.sampleCount)
              ),
              s"source samples derive centre=(${derived.centre.x}, ${derived.centre.y}) " +
                s"sampleCount=${derived.sampleCount}, but the payload declares " +
                s"centre=(${expected.centre.x}, ${expected.centre.y}) " +
                s"sampleCount=${expected.sampleCount}"
            )
          )
      }
    yield path

  private def fixation(
      f: Event.Fixation[U],
      sourceSupported: Boolean
  ): Either[CodecError, Json] =
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
      case DispersionStatus.Available(value, SummaryEvidence.SourceSupported(_, _))
          if sourceSupported =>
        Right(
          base.mapObject(
            _.add(
              "dispersion",
              Json.obj(
                "method" -> Json.fromString(StudyInputCodec.dispersionMethods(value.method))
              )
            )
          )
        )
      case DispersionStatus.Available(_, evidence: SummaryEvidence.SourceSupported) =>
        Left(
          CodecError.Unsupported(
            "dispersion",
            s"evidence $evidence refers to source samples this scanpath does not carry"
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

  /** A source-supported entry declares its dispersion method only; the
    * value is re-derived from the samples, so a placeholder of zero stands
    * in until `EventSeries.of` rebuilds the summary.
    */
  private def readFixation(
      json: Json,
      clock: ClockId,
      sourceSupported: Boolean
  ): Either[CodecError, Event.Fixation[U]] =
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
            value <-
              if sourceSupported then
                Either.cond(
                  spread.asObject.exists(!_.contains("value")),
                  0.0,
                  CodecError.Field(
                    "value",
                    spread,
                    "a source-supported dispersion carries its method only; its value is derived"
                  )
                )
              else DomainWire.finite(spread, "value")
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

  private def writePolicy(ledger: AdmissionLedger[K]): Either[CodecError, Json] =
    ledger.policy.corrections.zipWithIndex
      .traverse { case (rule, index) =>
        val scope = rule.scope match
          case CorrectionScope.AllTrials() =>
            Right(Json.obj("kind" -> Json.fromString("allTrials")))
          case CorrectionScope.Participant(p) =>
            Right(
              Json.obj(
                "kind"        -> Json.fromString("participant"),
                "participant" -> Json.fromString(p)
              )
            )
          case CorrectionScope.Trial(key) =>
            keys
              .encode(key)
              .map(k => Json.obj("kind" -> Json.fromString("trial"), "key" -> k))
        scope
          .map(s =>
            Json.obj("scope" -> s, "correction" -> StudyInputCodec.correction(rule.correction))
          )
          .left
          .map(Wire.at(s"corrections[$index]"))
      }
      .map(rules =>
        Json.obj(
          "offScreen" -> Json.fromString(
            StudyInputCodec.offScreenPolicies.toMap.apply(ledger.policy.offScreen)
          ),
          "corrections"  -> Json.arr(rules*),
          "outsideFrame" -> Json.arr(ledger.outsideFrame.map { entry =>
            Json.obj(
              "record" -> Json.fromInt(entry.record),
              "x"      -> Json.fromDoubleOrNull(entry.x),
              "y"      -> Json.fromDoubleOrNull(entry.y),
              "frame"  -> Json.fromString(entry.frame.name)
            )
          }*)
        )
      )

  private def readPolicy(
      json: Json
  ): Either[CodecError, (AdmissionPolicy[K], Vector[OutsideFrame])] =
    for
      name   <- Wire.field[String](json, "offScreen")
      policy <- StudyInputCodec.offScreenPolicies
        .collectFirst { case (p, n) if n == name => p }
        .toRight(CodecError.Field("offScreen", json, s"unknown off-screen policy $name"))
      rules       <- Wire.field[Vector[Json]](json, "corrections")
      corrections <- rules.zipWithIndex.traverse { case (rule, index) =>
        (for
          scope  <- Wire.field[Json](rule, "scope")
          kind   <- Wire.field[String](scope, "kind")
          scoped <- kind match
            case "allTrials"   => Right(CorrectionScope.AllTrials[K]())
            case "participant" =>
              Wire.field[String](scope, "participant").map(CorrectionScope.Participant[K](_))
            case "trial" =>
              Wire.field[Json](scope, "key").flatMap(keys.decode).map(CorrectionScope.Trial(_))
            case other =>
              Left(CodecError.Field("kind", scope, s"unknown correction scope $other"))
          correction <- Wire
            .field[Json](rule, "correction")
            .flatMap(StudyInputCodec.readCorrection)
        yield AppliedCorrection(scoped, correction)).left.map(Wire.at(s"corrections[$index]"))
      }
      entries      <- Wire.field[Vector[Json]](json, "outsideFrame")
      outsideFrame <- entries.zipWithIndex.traverse { case (entry, index) =>
        (for
          record <- Wire.field[Int](entry, "record")
          x      <- DomainWire.finite(entry, "x")
          y      <- DomainWire.finite(entry, "y")
          frame  <- Wire.field[String](entry, "frame")
        yield OutsideFrame(record, x, y, FrameId(frame))).left
          .map(Wire.at(s"outsideFrame[$index]"))
      }
    yield AdmissionPolicy(policy, corrections) -> outsideFrame

  private def readLedger(
      json: Json,
      version: Int
  ): Either[CodecError, AdmissionLedger[K]] = for
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
      readRecord(entry, version).left.map(Wire.at(s"records[$index]"))
    }
    admission <-
      if version >= 2 then readPolicy(json)
      else Right(AdmissionPolicy.version1[K] -> Vector.empty[OutsideFrame])
    ledger <-
      if version < 3 then
        AdmissionLedger
          .of(SourceRef(label, ref), header, records, outcome, admission._1, admission._2)
          .left
          .map(CodecError.Admission.apply)
      else
        // A version-3 ledger is exactly one with an inventory.
        for
          body <- Wire.field[Json](json, "inventory")
          _    <- Either.cond(
            !body.isNull,
            (),
            CodecError
              .Field("inventory", json, "a version-3 ledger carries its trial inventory")
          )
          project   <- trialIdentity.toRight(noTrialProjection)
          inventory <- InventoryWire.read(body).left.map(Wire.at("inventory"))
          joined    <- AdmissionLedger
            .inventoried(
              SourceRef(label, ref),
              header,
              records,
              outcome,
              admission._1,
              admission._2,
              inventory,
              project,
              layout.stimulus(_)
            )
            .left
            .map(CodecError.Admission.apply)
        yield joined
    _ <- ledger.checkCorrections(layout.participant(_)).left.map(CodecError.Admission.apply)
  yield ledger

  private def readRecord(
      json: Json,
      version: Int
  ): Either[CodecError, SourceRecord[K]] = for
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
          reason  <- Wire
            .field[Json](json, "reason")
            .flatMap(StudyInputCodec.readReason(_, version))
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
        "cause"   -> StudyInputCodec.cause(cause)
      )

  def cause(value: QuarantineCause): Json = value match
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
      )
    case QuarantineCause.CorrectionConflict(first, second) =>
      Json.obj(
        "kind"   -> Json.fromString("correctionConflict"),
        "first"  -> Json.fromInt(first),
        "second" -> Json.fromInt(second)
      )
    case QuarantineCause.ItemConflict(items) =>
      Json.obj(
        "kind"  -> Json.fromString("itemConflict"),
        "items" -> Json.arr(items.map(Json.fromString)*)
      )
    case QuarantineCause.OccurrenceConflict(occurrences) =>
      Json.obj(
        "kind"        -> Json.fromString("occurrenceConflict"),
        "occurrences" -> Json.arr(occurrences.map(Json.fromInt)*)
      )
    case QuarantineCause.NotInInventory(participant, phase, trial, occurrence) =>
      Json.obj(
        "kind"        -> Json.fromString("notInInventory"),
        "participant" -> Json.fromString(participant),
        "phase"       -> Json.fromString(phase),
        "trial"       -> Json.fromString(trial),
        "occurrence"  -> Json.fromInt(occurrence)
      )
    case QuarantineCause.InventoryItemConflict(inventory, records) =>
      Json.obj(
        "kind"      -> Json.fromString("inventoryItemConflict"),
        "inventory" -> Json.fromString(inventory),
        "records"   -> Json.arr(records.map(Json.fromString)*)
      )

  val offScreenPolicies: Vector[(OffScreenPolicy, String)] = Vector(
    OffScreenPolicy.ExcludeRecord   -> "excludeRecord",
    OffScreenPolicy.QuarantineTrial -> "quarantineTrial"
  )

  def correction(value: Correction): Json = value match
    case Correction.FlipX             => Json.obj("kind" -> Json.fromString("flipX"))
    case Correction.FlipY             => Json.obj("kind" -> Json.fromString("flipY"))
    case Correction.Translate(dx, dy) =>
      Json.obj(
        "kind" -> Json.fromString("translate"),
        "dx"   -> Json.fromDoubleOrNull(dx),
        "dy"   -> Json.fromDoubleOrNull(dy)
      )

  def readCorrection(json: Json): Either[CodecError, Correction] =
    Wire.field[String](json, "kind").flatMap {
      case "flipX"     => Right(Correction.FlipX)
      case "flipY"     => Right(Correction.FlipY)
      case "translate" =>
        for
          dx     <- DomainWire.finite(json, "dx")
          dy     <- DomainWire.finite(json, "dy")
          result <- Correction
            .translate(dx, dy)
            .left
            .map(e => CodecError.Field("translate", json, e.message))
        yield result
      case other => Left(CodecError.Field("kind", json, s"unknown correction $other"))
    }

  /** Explicit wire names: an enum rename cannot change the format. */
  val dispersionMethods: Map[DispersionMethod, String] = Map(
    DispersionMethod.RmsRadius               -> "rmsRadius",
    DispersionMethod.BoundingBoxWidth        -> "boundingBoxWidth",
    DispersionMethod.BoundingBoxDiagonal     -> "boundingBoxDiagonal",
    DispersionMethod.MedianAbsoluteDeviation -> "medianAbsoluteDeviation"
  )

  /** A reason as a ledger of the given version may name it: a ledger never
    * names a cause that arrived with a later version.
    */
  def readReason(json: Json, version: Int): Either[CodecError, AdmissionReason] =
    readReason(json).flatMap {
      case AdmissionReason.Quarantined(_, cause) if QuarantineCause.version(cause) > version =>
        Left(
          CodecError.Field(
            "cause",
            json,
            s"a version-$version ledger cannot name ${cause.productPrefix}"
          )
        )
      case reason => Right(reason)
    }

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
          value   <- readCause(cause)
        yield AdmissionReason.Quarantined(records, value)
      case other => Left(CodecError.Field("kind", json, s"unknown admission reason $other"))
    }

  def readCause(cause: Json): Either[CodecError, QuarantineCause] =
    Wire.field[String](cause, "kind").flatMap {
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
      case "correctionConflict" =>
        for
          first  <- Wire.field[Int](cause, "first")
          second <- Wire.field[Int](cause, "second")
        yield QuarantineCause.CorrectionConflict(first, second)
      case "itemConflict" =>
        Wire.field[Vector[String]](cause, "items").map(QuarantineCause.ItemConflict.apply)
      case "occurrenceConflict" =>
        Wire
          .field[Vector[Int]](cause, "occurrences")
          .map(QuarantineCause.OccurrenceConflict.apply)
      case "notInInventory" =>
        for
          participant <- Wire.field[String](cause, "participant")
          phase       <- Wire.field[String](cause, "phase")
          trial       <- Wire.field[String](cause, "trial")
          occurrence  <- Wire.field[Int](cause, "occurrence")
        yield QuarantineCause.NotInInventory(participant, phase, trial, occurrence)
      case "inventoryItemConflict" =>
        for
          inventory <- Wire.field[String](cause, "inventory")
          records   <- Wire.field[Vector[String]](cause, "records")
        yield QuarantineCause.InventoryItemConflict(inventory, records)
      case other =>
        Left(CodecError.Field("kind", cause, s"unknown quarantine cause $other"))
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

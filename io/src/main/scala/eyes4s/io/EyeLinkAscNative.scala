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

package eyes4s.io

import eyes4s.kernel.Detector
import eyes4s.kernel.Machine

import scala.util.Try

/** The producer of SFIX/EFIX, SSACC/ESACC, and SBLINK/EBLINK records.
  *
  * This identity is intentionally distinct from eyes4s event detectors. These
  * records are classifications supplied by EyeLink, not events inferred by an
  * eyes4s algorithm.
  */
enum AscNativeEventProducer derives CanEqual:
  case EyeLinkOnlineParser

enum AscNativeEventType derives CanEqual:
  case Fixation
  case Saccade
  case Blink

enum AscNativeEventPhase derives CanEqual:
  case Start
  case End
  case Update

/** Exact tracker-time interval from an EyeLink end-event record. */
final class AscNativeInterval private[io] (
    val start: BigDecimal,
    val end: BigDecimal,
    val duration: BigDecimal
)

/** One fixation summary in one coordinate representation. HREF end events
  * contain both an HREF summary and a GAZE summary.
  */
final class AscNativeFixationSummary private[io] (
    val position: AscNativePair,
    val pupil: AscNativeScalar
)

/** One saccade summary in one coordinate representation. HREF end events
  * contain both an HREF summary and a GAZE summary.
  */
final class AscNativeSaccadeSummary private[io] (
    val startPosition: AscNativePair,
    val endPosition: AscNativePair,
    val amplitude: AscNativeScalar,
    val peakVelocity: AscNativeScalar
)

enum AscNativeEventBody:
  case StartOnly
  case FixationEnd(
      primary: AscNativeFixationSummary,
      gaze: Option[AscNativeFixationSummary],
      resolution: Option[AscNativePair]
  )
  case SaccadeEnd(
      primary: AscNativeSaccadeSummary,
      gaze: Option[AscNativeSaccadeSummary],
      resolution: Option[AscNativePair]
  )
  case BlinkEnd
  case PreservedUpdate(fields: Vector[String])

/** One losslessly sourced EyeLink native event record. */
final class AscParsedNativeEvent private[io] (
    val producer: AscNativeEventProducer,
    val source: AscSourceLine,
    val blockNumber: Long,
    val coordinateMode: AscCoordinateMode,
    val eventType: AscNativeEventType,
    val phase: AscNativeEventPhase,
    val eye: AscRecordedEye,
    val onset: BigDecimal,
    val interval: Option[AscNativeInterval],
    val body: AscNativeEventBody
)

enum AscMessageCategory derives CanEqual:
  case Trial
  case DataViewerIntegration
  case Calibration
  case Validation
  case DriftCorrection
  case RecordingMetadata
  case System
  case Native

/** A signed integration offset is recognized only when its token has an
  * explicit `+` or `-`. An unsigned numeric-leading payload therefore remains
  * payload rather than being silently reinterpreted as time.
  */
final class AscIntegrationOffset private[io] (val value: BigDecimal)

/** One MSG record with both logged and effective tracker time. The exact
  * payload bytes after the interpreted time fields remain authoritative.
  */
final class AscParsedMessage private[io] (
    val source: AscSourceLine,
    val blockNumber: Option[Long],
    val loggedTime: BigDecimal,
    val integrationOffset: Option[AscIntegrationOffset],
    val effectiveTime: BigDecimal,
    val payload: AscPayload,
    val category: AscMessageCategory
)

enum AscNativeMetadataKind derives CanEqual:
  case Button
  case Input
  case LostData

/** Native event-stream metadata whose fields are retained exactly without
  * assigning undocumented meaning.
  */
final class AscParsedNativeMetadata private[io] (
    val source: AscSourceLine,
    val blockNumber: Long,
    val kind: AscNativeMetadataKind,
    val fields: Vector[String]
)

enum AscParsedNativeRecord:
  case Event(value: AscParsedNativeEvent)
  case Message(value: AscParsedMessage)
  case Metadata(value: AscParsedNativeMetadata)

enum AscNativeDiagnostic:
  case NotNativeRecord(source: String, line: Long, record: String)
  case BlockRequirement(requirement: AscBlockRequirement)
  case MalformedKnownRecord(source: String, line: Long, record: String)
  case EventBlockMissing(source: String, line: Long)
  case EventLayoutMissing(source: String, line: Long, block: Long)
  case NonAsciiField(source: String, line: Long, block: Option[Long], index: Int)
  case UnexpectedFieldCount(
      source: String,
      line: Long,
      block: Long,
      record: AscNativeEventKind,
      expected: Int,
      actual: Int
  )
  case InvalidDecimal(
      source: String,
      line: Long,
      block: Option[Long],
      field: String,
      index: Int,
      value: String
  )
  case NegativeTime(
      source: String,
      line: Long,
      block: Option[Long],
      field: String,
      value: String
  )
  case InvalidEye(
      source: String,
      line: Long,
      block: Long,
      value: String,
      declared: Option[AscEyeLayout]
  )
  case EndBeforeStart(
      source: String,
      line: Long,
      block: Long,
      start: String,
      end: String
  )
  case DurationMismatch(
      source: String,
      line: Long,
      block: Long,
      start: String,
      end: String,
      duration: String
  )
  case PartialMissingPair(
      source: String,
      line: Long,
      block: Long,
      field: String,
      x: String,
      y: String
  )
  case InvalidIntegrationOffset(source: String, line: Long, index: Int, value: String)

  def severity: AscDiagnosticSeverity = AscDiagnosticSeverity.Error

  def message: String = this match
    case NotNativeRecord(source, line, record) =>
      s"ASC source='$source' line=$line record=$record is not a native event, message, or event-metadata row."
    case BlockRequirement(requirement)              => requirement.message
    case MalformedKnownRecord(source, line, record) =>
      s"ASC source='$source' line=$line contains malformed known record=$record."
    case EventBlockMissing(source, line) =>
      s"ASC source='$source' line=$line native event has no recording block context."
    case EventLayoutMissing(source, line, block) =>
      s"ASC source='$source' line=$line block=$block native event has no parsed EVENTS layout."
    case NonAsciiField(source, line, block, index) =>
      s"ASC source='$source' line=$line block=${renderBlock(block)} native field index=$index is not ASCII."
    case UnexpectedFieldCount(source, line, block, record, expected, actual) =>
      s"ASC source='$source' line=$line block=$block record=$record has fields=$actual, expected=$expected."
    case InvalidDecimal(source, line, block, field, index, value) =>
      s"ASC source='$source' line=$line block=${renderBlock(block)} field=$field index=$index has invalid decimal='$value'."
    case NegativeTime(source, line, block, field, value) =>
      s"ASC source='$source' line=$line block=${renderBlock(block)} field=$field has negative tracker time='$value'."
    case InvalidEye(source, line, block, value, declared) =>
      s"ASC source='$source' line=$line block=$block eye='$value' is invalid for declared event eye layout=$declared."
    case EndBeforeStart(source, line, block, start, end) =>
      s"ASC source='$source' line=$line block=$block native interval start=$start is later than end=$end."
    case DurationMismatch(source, line, block, start, end, duration) =>
      s"ASC source='$source' line=$line block=$block native interval start=$start end=$end disagrees with duration=$duration."
    case PartialMissingPair(source, line, block, field, x, y) =>
      s"ASC source='$source' line=$line block=$block field=$field is partially missing: x='$x', y='$y'."
    case InvalidIntegrationOffset(source, line, index, value) =>
      s"ASC source='$source' line=$line message integration offset index=$index is invalid: '$value'."

  private def renderBlock(value: Option[Long]): String = value.fold("outside")(_.toString)

end AscNativeDiagnostic

final class AscNativeParseResult private[io] (
    val line: AscBlockLine,
    val record: Option[AscParsedNativeRecord],
    val diagnostics: Vector[AscNativeDiagnostic]
):
  def hasErrors: Boolean = diagnostics.exists(_.severity == AscDiagnosticSeverity.Error)

enum AscNativeEmission:
  case Parsed(value: AscNativeParseResult)
  case Preserved(value: AscBlockEmission)

/** Lossless interpretation of EyeLink-native events, messages, and event
  * metadata. Pairing start/end records and mapping them to an experiment clock
  * are deliberately later operations.
  */
object EyeLinkAscNative:

  val machine: Machine[AscBlockEmission, AscNativeEmission] =
    Machine(
      new Detector[Unit, AscBlockEmission, AscNativeEmission]:
        def init: Unit = ()

        def step(
            state: Unit,
            input: AscBlockEmission
        ): (Unit, Vector[AscNativeEmission]) =
          input match
            case AscBlockEmission.Line(line) if isNative(line.line.record) =>
              (state, Vector(AscNativeEmission.Parsed(parse(line))))
            case _ => (state, Vector(AscNativeEmission.Preserved(input)))

        def flush(state: Unit): Vector[AscNativeEmission] = Vector.empty
    )

  def parse(line: AscBlockLine): AscNativeParseResult =
    line.line.record match
      case AscRecord.Message(fields)           => parseMessage(line, fields)
      case AscRecord.NativeEvent(kind, fields) => parseEvent(line, kind, fields)
      case AscRecord.Button(fields)            =>
        parseMetadata(line, AscNativeMetadataKind.Button, fields)
      case AscRecord.Input(fields) =>
        parseMetadata(line, AscNativeMetadataKind.Input, fields)
      case AscRecord.LostData(fields) =>
        parseMetadata(line, AscNativeMetadataKind.LostData, fields)
      case AscRecord.MalformedKnown(kind, _) if isNativeKnown(kind) =>
        new AscNativeParseResult(
          line,
          None,
          Vector(
            AscNativeDiagnostic.MalformedKnownRecord(
              line.line.source.source,
              line.line.source.number,
              kind.toString
            )
          )
        )
      case other =>
        new AscNativeParseResult(
          line,
          None,
          Vector(
            AscNativeDiagnostic.NotNativeRecord(
              line.line.source.source,
              line.line.source.number,
              recordName(other)
            )
          )
        )

  private def parseMessage(line: AscBlockLine, fields: AscFields): AscNativeParseResult =
    val source      = line.line.source
    val diagnostics = Vector.newBuilder[AscNativeDiagnostic]
    val timestamp   = decimal(
      fields,
      0,
      "logged-time",
      line.block.map(_.start.blockNumber),
      diagnostics
    )
    timestamp.foreach(value =>
      requireNonnegative(value, source, None, "logged-time", diagnostics)
    )
    val offsetToken = fields.token(1).flatMap(_.ascii)
    val offset      = offsetToken match
      case Some(value) if isExplicitOffsetCandidate(value) =>
        Try(BigDecimal(value)).toOption match
          case Some(parsed) => Some(new AscIntegrationOffset(parsed))
          case None         =>
            diagnostics += AscNativeDiagnostic.InvalidIntegrationOffset(
              source.source,
              source.number,
              1,
              AscDiagnosticText.bounded(value)
            )
            None
      case _ => None
    val offsetWasCandidate = offsetToken.exists(isExplicitOffsetCandidate)
    val payloadIndex       = if offsetWasCandidate then 1 else 0
    val payload = fields.remainderAfter(payloadIndex).getOrElse(new AscPayload(IArray.empty))
    for
      logged <- timestamp
      shift  <- offset
    do requireNonnegative(logged + shift.value, source, None, "effective-time", diagnostics)
    val allDiagnostics = diagnostics.result()
    val parsed         =
      if allDiagnostics.nonEmpty then None
      else
        timestamp.map { logged =>
          val effective = logged + offset.fold(BigDecimal(0))(_.value)
          new AscParsedMessage(
            source,
            line.block.map(_.start.blockNumber),
            logged,
            offset,
            effective,
            payload,
            classify(payload)
          )
        }
    new AscNativeParseResult(
      line,
      parsed.map(AscParsedNativeRecord.Message.apply),
      allDiagnostics
    )

  private def parseEvent(
      line: AscBlockLine,
      kind: AscNativeEventKind,
      fields: AscFields
  ): AscNativeParseResult =
    val source      = line.line.source
    val inherited   = line.requirements.map(AscNativeDiagnostic.BlockRequirement.apply)
    val diagnostics = Vector.newBuilder[AscNativeDiagnostic]
    diagnostics ++= inherited
    line.block match
      case None =>
        diagnostics += AscNativeDiagnostic.EventBlockMissing(source.source, source.number)
        new AscNativeParseResult(line, None, diagnostics.result())
      case Some(block) =>
        block.configuration.events match
          case None =>
            diagnostics += AscNativeDiagnostic.EventLayoutMissing(
              source.source,
              source.number,
              block.start.blockNumber
            )
            new AscNativeParseResult(line, None, diagnostics.result())
          case Some(layout) =>
            val values = fields.tokens.map(_.ascii)
            values.zipWithIndex.foreach {
              case (None, index) =>
                diagnostics += AscNativeDiagnostic.NonAsciiField(
                  source.source,
                  source.number,
                  Some(block.start.blockNumber),
                  index
                )
              case _ => ()
            }
            if values.exists(_.isEmpty) || inherited.nonEmpty then
              new AscNativeParseResult(line, None, diagnostics.result())
            else
              layout.coordinateMode match
                case Some(mode) =>
                  parseConfiguredEvent(
                    line,
                    kind,
                    values.flatten,
                    block,
                    layout,
                    mode,
                    diagnostics
                  )
                case None =>
                  diagnostics += AscNativeDiagnostic.BlockRequirement(
                    AscBlockRequirement.CoordinateModeMissing(
                      source.source,
                      source.number,
                      block.start.blockNumber,
                      "events"
                    )
                  )
                  new AscNativeParseResult(line, None, diagnostics.result())

  private def parseConfiguredEvent(
      line: AscBlockLine,
      kind: AscNativeEventKind,
      values: Vector[String],
      block: AscBlockSnapshot,
      layout: AscDataLayout,
      mode: AscCoordinateMode,
      diagnostics: scala.collection.mutable.Builder[AscNativeDiagnostic, Vector[
        AscNativeDiagnostic
      ]]
  ): AscNativeParseResult =
    val source      = line.line.source
    val blockNumber = block.start.blockNumber
    val expected    = expectedEventFields(kind, mode, layout)
    if expected.exists(_ != values.length) then
      diagnostics += AscNativeDiagnostic.UnexpectedFieldCount(
        source.source,
        source.number,
        blockNumber,
        kind,
        expected.getOrElse(2),
        values.length
      )
      new AscNativeParseResult(line, None, diagnostics.result())
    else
      val cursor = new EventCursor(source, blockNumber, values, diagnostics)
      val eye    = cursor.eye(layout.eyeLayout)
      val record = kind match
        case AscNativeEventKind.FixationStart =>
          parseStart(cursor, AscNativeEventType.Fixation, eye, source, blockNumber, mode)
        case AscNativeEventKind.SaccadeStart =>
          parseStart(cursor, AscNativeEventType.Saccade, eye, source, blockNumber, mode)
        case AscNativeEventKind.BlinkStart =>
          parseStart(cursor, AscNativeEventType.Blink, eye, source, blockNumber, mode)
        case AscNativeEventKind.FixationEnd =>
          parseFixationEnd(cursor, eye, source, blockNumber, mode, layout)
        case AscNativeEventKind.SaccadeEnd =>
          parseSaccadeEnd(cursor, eye, source, blockNumber, mode, layout)
        case AscNativeEventKind.BlinkEnd =>
          parseBlinkEnd(cursor, eye, source, blockNumber, mode)
        case AscNativeEventKind.FixationUpdate =>
          parseUpdate(
            cursor,
            AscNativeEventType.Fixation,
            eye,
            source,
            blockNumber,
            mode,
            values.drop(2)
          )
        case AscNativeEventKind.SaccadeUpdate =>
          parseUpdate(
            cursor,
            AscNativeEventType.Saccade,
            eye,
            source,
            blockNumber,
            mode,
            values.drop(2)
          )
      val allDiagnostics = diagnostics.result()
      new AscNativeParseResult(
        line,
        Option
          .when(allDiagnostics.isEmpty)(record)
          .flatten
          .map(AscParsedNativeRecord.Event.apply),
        allDiagnostics
      )

  private def parseStart(
      cursor: EventCursor,
      eventType: AscNativeEventType,
      eye: Option[AscRecordedEye],
      source: AscSourceLine,
      block: Long,
      mode: AscCoordinateMode
  ): Option[AscParsedNativeEvent] =
    val onset = cursor.time("onset")
    for recordedEye <- eye; value <- onset
    yield new AscParsedNativeEvent(
      AscNativeEventProducer.EyeLinkOnlineParser,
      source,
      block,
      mode,
      eventType,
      AscNativeEventPhase.Start,
      recordedEye,
      value,
      None,
      AscNativeEventBody.StartOnly
    )

  private def parseFixationEnd(
      cursor: EventCursor,
      eye: Option[AscRecordedEye],
      source: AscSourceLine,
      block: Long,
      mode: AscCoordinateMode,
      layout: AscDataLayout
  ): Option[AscParsedNativeEvent] =
    val interval = cursor.interval()
    val primary  = cursor.fixation("primary")
    val gaze     =
      Option.when(mode == AscCoordinateMode.HeadReference)(cursor.fixation("gaze")).flatten
    val res = resolution(cursor, layout)
    for recordedEye <- eye; time <- interval; summary <- primary
    yield event(
      source,
      block,
      mode,
      AscNativeEventType.Fixation,
      recordedEye,
      time,
      AscNativeEventBody.FixationEnd(summary, gaze, res)
    )

  private def parseSaccadeEnd(
      cursor: EventCursor,
      eye: Option[AscRecordedEye],
      source: AscSourceLine,
      block: Long,
      mode: AscCoordinateMode,
      layout: AscDataLayout
  ): Option[AscParsedNativeEvent] =
    val interval = cursor.interval()
    val primary  = cursor.saccade("primary")
    val gaze     =
      Option.when(mode == AscCoordinateMode.HeadReference)(cursor.saccade("gaze")).flatten
    val res = resolution(cursor, layout)
    for recordedEye <- eye; time <- interval; summary <- primary
    yield event(
      source,
      block,
      mode,
      AscNativeEventType.Saccade,
      recordedEye,
      time,
      AscNativeEventBody.SaccadeEnd(summary, gaze, res)
    )

  private def parseBlinkEnd(
      cursor: EventCursor,
      eye: Option[AscRecordedEye],
      source: AscSourceLine,
      block: Long,
      mode: AscCoordinateMode
  ): Option[AscParsedNativeEvent] =
    for recordedEye <- eye; time <- cursor.interval()
    yield event(
      source,
      block,
      mode,
      AscNativeEventType.Blink,
      recordedEye,
      time,
      AscNativeEventBody.BlinkEnd
    )

  private def parseUpdate(
      cursor: EventCursor,
      eventType: AscNativeEventType,
      eye: Option[AscRecordedEye],
      source: AscSourceLine,
      block: Long,
      mode: AscCoordinateMode,
      remaining: Vector[String]
  ): Option[AscParsedNativeEvent] =
    val onset = cursor.time("update-time")
    for recordedEye <- eye; value <- onset
    yield new AscParsedNativeEvent(
      AscNativeEventProducer.EyeLinkOnlineParser,
      source,
      block,
      mode,
      eventType,
      AscNativeEventPhase.Update,
      recordedEye,
      value,
      None,
      AscNativeEventBody.PreservedUpdate(remaining)
    )

  private def event(
      source: AscSourceLine,
      block: Long,
      mode: AscCoordinateMode,
      eventType: AscNativeEventType,
      eye: AscRecordedEye,
      interval: AscNativeInterval,
      body: AscNativeEventBody
  ): AscParsedNativeEvent =
    new AscParsedNativeEvent(
      AscNativeEventProducer.EyeLinkOnlineParser,
      source,
      block,
      mode,
      eventType,
      AscNativeEventPhase.End,
      eye,
      interval.start,
      Some(interval),
      body
    )

  private def resolution(
      cursor: EventCursor,
      layout: AscDataLayout
  ): Option[AscNativePair] =
    Option
      .when(layout.optionalColumns.contains(AscOptionalColumn.Resolution))(
        cursor.pair("resolution")
      )
      .flatten

  private def parseMetadata(
      line: AscBlockLine,
      kind: AscNativeMetadataKind,
      fields: AscFields
  ): AscNativeParseResult =
    val source      = line.line.source
    val diagnostics = Vector.newBuilder[AscNativeDiagnostic]
    diagnostics ++= line.requirements.map(AscNativeDiagnostic.BlockRequirement.apply)
    val values = fields.tokens.map(_.ascii)
    values.zipWithIndex.foreach {
      case (None, index) =>
        diagnostics += AscNativeDiagnostic.NonAsciiField(
          source.source,
          source.number,
          line.block.map(_.start.blockNumber),
          index
        )
      case _ => ()
    }
    val allDiagnostics = diagnostics.result()
    val parsed         = for
      block <- line.block
      if allDiagnostics.isEmpty && values.forall(_.nonEmpty)
    yield AscParsedNativeRecord.Metadata(
      new AscParsedNativeMetadata(
        source,
        block.start.blockNumber,
        kind,
        values.flatten
      )
    )
    new AscNativeParseResult(line, parsed, allDiagnostics)

  private final class EventCursor(
      source: AscSourceLine,
      block: Long,
      values: Vector[String],
      diagnostics: scala.collection.mutable.Builder[AscNativeDiagnostic, Vector[
        AscNativeDiagnostic
      ]]
  ):
    private var index = 0

    def eye(declared: Option[AscEyeLayout]): Option[AscRecordedEye] =
      val value = raw()
      value.flatMap {
        case "L"
            if declared
              .exists(value => value == AscEyeLayout.Left || value == AscEyeLayout.Binocular) =>
          Some(AscRecordedEye.Left)
        case "R"
            if declared.exists(value =>
              value == AscEyeLayout.Right || value == AscEyeLayout.Binocular
            ) =>
          Some(AscRecordedEye.Right)
        case actual =>
          diagnostics += AscNativeDiagnostic.InvalidEye(
            source.source,
            source.number,
            block,
            AscDiagnosticText.bounded(actual),
            declared
          )
          None
      }

    def time(field: String): Option[BigDecimal] =
      val at = index
      raw().flatMap { value =>
        parseDecimal(field, at, value).flatMap { parsed =>
          if parsed < 0 then
            diagnostics += AscNativeDiagnostic.NegativeTime(
              source.source,
              source.number,
              Some(block),
              field,
              AscDiagnosticText.bounded(value)
            )
            None
          else Some(parsed)
        }
      }

    def interval(): Option[AscNativeInterval] =
      val start    = time("start-time")
      val end      = time("end-time")
      val duration = time("duration")
      (start, end, duration) match
        case (Some(from), Some(until), Some(span)) if until < from =>
          diagnostics += AscNativeDiagnostic.EndBeforeStart(
            source.source,
            source.number,
            block,
            AscDiagnosticText.bounded(from.toString),
            AscDiagnosticText.bounded(until.toString)
          )
          None
        case (Some(from), Some(until), Some(span)) if until - from != span =>
          diagnostics += AscNativeDiagnostic.DurationMismatch(
            source.source,
            source.number,
            block,
            AscDiagnosticText.bounded(from.toString),
            AscDiagnosticText.bounded(until.toString),
            AscDiagnosticText.bounded(span.toString)
          )
          None
        case (Some(from), Some(until), Some(span)) =>
          Some(new AscNativeInterval(from, until, span))
        case _ => None

    def fixation(prefix: String): Option[AscNativeFixationSummary] =
      for
        position <- pair(s"$prefix fixation position")
        pupil    <- scalar(s"$prefix fixation pupil")
      yield new AscNativeFixationSummary(position, pupil)

    def saccade(prefix: String): Option[AscNativeSaccadeSummary] =
      for
        start     <- pair(s"$prefix saccade start")
        end       <- pair(s"$prefix saccade end")
        amplitude <- scalar(s"$prefix saccade amplitude")
        velocity  <- scalar(s"$prefix saccade peak velocity")
      yield new AscNativeSaccadeSummary(start, end, amplitude, velocity)

    def pair(field: String): Option[AscNativePair] =
      val xIndex = index
      val x      = raw()
      val yIndex = index
      val y      = raw()
      (x, y) match
        case (Some("."), Some("."))    => Some(AscNativePair.MissingDot)
        case (Some("."), Some(yValue)) =>
          diagnostics += AscNativeDiagnostic.PartialMissingPair(
            source.source,
            source.number,
            block,
            field,
            ".",
            AscDiagnosticText.bounded(yValue)
          )
          None
        case (Some(xValue), Some(".")) =>
          diagnostics += AscNativeDiagnostic.PartialMissingPair(
            source.source,
            source.number,
            block,
            field,
            AscDiagnosticText.bounded(xValue),
            "."
          )
          None
        case (Some(xValue), Some(yValue)) =>
          for
            parsedX <- parseDecimal(s"$field x", xIndex, xValue)
            parsedY <- parseDecimal(s"$field y", yIndex, yValue)
          yield AscNativePair.Values(parsedX, parsedY)
        case _ => None

    def scalar(field: String): Option[AscNativeScalar] =
      val at = index
      raw().flatMap {
        case "."   => Some(AscNativeScalar.MissingDot)
        case value => parseDecimal(field, at, value).map(AscNativeScalar.Value.apply)
      }

    private def raw(): Option[String] =
      val value = values.lift(index)
      index += 1
      value

    private def parseDecimal(field: String, at: Int, value: String): Option[BigDecimal] =
      Try(BigDecimal(value)).toOption match
        case parsed @ Some(_) => parsed
        case None             =>
          diagnostics += AscNativeDiagnostic.InvalidDecimal(
            source.source,
            source.number,
            Some(block),
            field,
            at,
            AscDiagnosticText.bounded(value)
          )
          None

  private def decimal(
      fields: AscFields,
      index: Int,
      field: String,
      block: Option[Long],
      diagnostics: scala.collection.mutable.Builder[AscNativeDiagnostic, Vector[
        AscNativeDiagnostic
      ]]
  ): Option[BigDecimal] =
    fields.token(index).flatMap(_.ascii) match
      case Some(value) =>
        Try(BigDecimal(value)).toOption match
          case parsed @ Some(_) => parsed
          case None             =>
            diagnostics += AscNativeDiagnostic.InvalidDecimal(
              fields.source.source,
              fields.source.number,
              block,
              field,
              index,
              AscDiagnosticText.bounded(value)
            )
            None
      case None =>
        diagnostics += AscNativeDiagnostic.NonAsciiField(
          fields.source.source,
          fields.source.number,
          block,
          index
        )
        None

  private def requireNonnegative(
      value: BigDecimal,
      source: AscSourceLine,
      block: Option[Long],
      field: String,
      diagnostics: scala.collection.mutable.Builder[AscNativeDiagnostic, Vector[
        AscNativeDiagnostic
      ]]
  ): Unit =
    if value < 0 then
      diagnostics += AscNativeDiagnostic.NegativeTime(
        source.source,
        source.number,
        block,
        field,
        AscDiagnosticText.bounded(value.toString)
      )

  private def expectedEventFields(
      kind: AscNativeEventKind,
      mode: AscCoordinateMode,
      layout: AscDataLayout
  ): Option[Int] =
    val resolution =
      if layout.optionalColumns.contains(AscOptionalColumn.Resolution) then 2 else 0
    kind match
      case AscNativeEventKind.FixationStart | AscNativeEventKind.SaccadeStart |
          AscNativeEventKind.BlinkStart =>
        Some(2)
      case AscNativeEventKind.BlinkEnd    => Some(4)
      case AscNativeEventKind.FixationEnd =>
        Some(4 + (if mode == AscCoordinateMode.HeadReference then 6 else 3) + resolution)
      case AscNativeEventKind.SaccadeEnd =>
        Some(4 + (if mode == AscCoordinateMode.HeadReference then 12 else 6) + resolution)
      case AscNativeEventKind.FixationUpdate | AscNativeEventKind.SaccadeUpdate => None

  private def classify(payload: AscPayload): AscMessageCategory =
    payload.ascii
      .flatMap(_.trim.split("[ \\t]+", 2).headOption)
      .map(upperAscii) match
      case Some("TRIALID" | "TRIAL_RESULT") => AscMessageCategory.Trial
      case Some("!V")                       => AscMessageCategory.DataViewerIntegration
      case Some("!CAL" | "!CALIBRATION")    => AscMessageCategory.Calibration
      case Some("!VAL" | "!VALIDATION")     => AscMessageCategory.Validation
      case Some("DRIFT" | "DRIFTCORRECT" | "DRIFT_CORRECT" | "!DRIFT") =>
        AscMessageCategory.DriftCorrection
      case Some("RECCFG" | "ELCLCFG" | "DISPLAY_COORDS" | "GAZE_COORDS") =>
        AscMessageCategory.RecordingMetadata
      case Some("EYELINK" | "VERSION" | "SERIAL" | "CAMERA") => AscMessageCategory.System
      case _                                                 => AscMessageCategory.Native

  private def isExplicitOffsetCandidate(value: String): Boolean =
    value.lengthCompare(1) > 0 &&
      (value.charAt(0) == '+' || value.charAt(0) == '-') &&
      (value.charAt(1).isDigit || value.charAt(1) == '.')

  private def upperAscii(value: String): String =
    value.map { character =>
      if character >= 'a' && character <= 'z' then (character - ('a' - 'A')).toChar
      else character
    }

  private def isNative(record: AscRecord): Boolean = record match
    case AscRecord.Message(_) | AscRecord.NativeEvent(_, _) | AscRecord.Button(_) |
        AscRecord.Input(_) | AscRecord.LostData(_) =>
      true
    case AscRecord.MalformedKnown(kind, _) => isNativeKnown(kind)
    case _                                 => false

  private def isNativeKnown(kind: AscKnownRecord): Boolean = kind match
    case AscKnownRecord.Message | AscKnownRecord.NativeEvent(_) | AscKnownRecord.Button |
        AscKnownRecord.Input | AscKnownRecord.LostData =>
      true
    case _ => false

  private def recordName(record: AscRecord): String = record match
    case AscRecord.Blank                   => "Blank"
    case AscRecord.Comment(_)              => "Comment"
    case AscRecord.Sample(_)               => "Sample"
    case AscRecord.Message(_)              => "Message"
    case AscRecord.Boundary(kind, _)       => kind.toString
    case AscRecord.Configuration(kind, _)  => kind.toString
    case AscRecord.NativeEvent(kind, _)    => kind.toString
    case AscRecord.Button(_)               => "Button"
    case AscRecord.Input(_)                => "Input"
    case AscRecord.LostData(_)             => "LostData"
    case AscRecord.Unknown(_, _)           => "Unknown"
    case AscRecord.MalformedKnown(kind, _) => s"MalformedKnown($kind)"

end EyeLinkAscNative

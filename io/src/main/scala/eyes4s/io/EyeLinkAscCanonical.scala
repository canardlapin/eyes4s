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

/** Deterministic, platform-neutral evidence for one complete ASC
  * materialization. Values use a length-prefixed encoding; source and payload
  * bytes are hexadecimal, so neither host text decoding nor newline handling
  * can alter the artifact.
  */
final class EyeLinkAscCanonicalManifest private (
    private val facts: Vector[EyeLinkAscCanonicalManifest.Fact]
):
  val schemaVersion: String = EyeLinkAscCanonicalManifest.SchemaVersion
  val factCount: Int        = facts.length

  lazy val canonical: String =
    val body = facts.map { fact =>
      s"${fact.path.length}:${fact.path}${fact.value.length}:${fact.value}\n"
    }.mkString
    s"$schemaVersion\n$body"

  lazy val digest: Sha256 = Sha256.ofUtf8(canonical)

  /** Structured access for conformance projections. The canonical manifest
    * remains the lossless byte-sensitive artifact; callers in this package can
    * deliberately project only the semantics an independent reader exposes.
    */
  private[io] def entries: Vector[(String, String)] =
    facts.map(fact => fact.path -> fact.value)

object EyeLinkAscCanonicalManifest:
  val SchemaVersion: String = "eyes4s-eyelink-canonical-v1"

  private final case class Fact(path: String, value: String)

  def from(result: EyeLinkAscSessionMaterialization): EyeLinkAscCanonicalManifest =
    val output = Vector.newBuilder[Fact]

    def add(path: String, value: String): Unit         = output += Fact(path, value)
    def addBoolean(path: String, value: Boolean): Unit = add(path, value.toString)
    def addOption[A](path: String, value: Option[A])(render: A => String): Unit =
      add(path, value.fold("absent")(render))

    val raw = result.raw
    add("source.origin", origin(raw.origin))
    add("source.declared-digest", raw.origin.ascDigest.hex)
    addOption("source.computed-digest", raw.computedDigest)(_.hex)

    raw.lines.zipWithIndex.foreach { case (line, index) =>
      val prefix = s"line.$index"
      add(s"$prefix.source", line.source.source)
      add(s"$prefix.number", line.source.number.toString)
      add(s"$prefix.byte-offset", line.source.byteOffset.toString)
      add(s"$prefix.terminator", terminator(line.source.terminator))
      add(s"$prefix.bytes", hex(line.source.bytes))
      add(s"$prefix.record", record(line.record))
      line.diagnostics.zipWithIndex.foreach { case (diagnostic, diagnosticIndex) =>
        add(s"$prefix.lexical.$diagnosticIndex", diagnostic.message)
      }
    }

    raw.blockEmissions.zipWithIndex.foreach { case (emission, index) =>
      val prefix = s"block-emission.$index"
      emission match
        case AscBlockEmission.Line(line) =>
          add(s"$prefix.kind", "line")
          add(s"$prefix.source-line", line.line.source.number.toString)
          addOption(s"$prefix.block", line.block)(_.start.blockNumber.toString)
          line.block.foreach { block =>
            val start = block.start
            addOption(s"$prefix.start.time-token", start.trackerTimeToken)(identity)
            addOption(s"$prefix.start.eyes", start.declaredEyeLayout)(eyeLayout)
            addBoolean(s"$prefix.start.samples", start.declaresSamples)
            addBoolean(s"$prefix.start.events", start.declaresEvents)
            add(s"$prefix.config.revision", block.configuration.revision.toString)
            addLayout(s"$prefix.config.samples", block.configuration.samples, add)
            addLayout(s"$prefix.config.events", block.configuration.events, add)
            addOption(s"$prefix.config.prescaler", block.configuration.prescaler)(value =>
              decimal(value.value)
            )
            addOption(
              s"$prefix.config.velocity-prescaler",
              block.configuration.velocityPrescaler
            )(value => decimal(value.value))
            addOption(s"$prefix.config.pupil", block.configuration.pupil)(pupilRepresentation)
          }
          line.requirements.zipWithIndex.foreach { case (requirement, requirementIndex) =>
            add(s"$prefix.requirement.$requirementIndex", requirement.message)
          }
          line.outcomes.zipWithIndex.foreach { case (outcome, outcomeIndex) =>
            add(s"$prefix.outcome.$outcomeIndex", outcome.message)
          }
        case AscBlockEmission.Closed(closed) =>
          add(s"$prefix.kind", "closed")
          add(s"$prefix.block", closed.block.start.blockNumber.toString)
          add(s"$prefix.reason", closure(closed.reason))
    }

    raw.sampleRows.zipWithIndex.foreach { case (row, index) =>
      val prefix = s"sample.$index"
      add(s"$prefix.source-line", row.line.line.source.number.toString)
      addBoolean(s"$prefix.parsed", row.sample.nonEmpty)
      row.diagnostics.zipWithIndex.foreach { case (diagnostic, diagnosticIndex) =>
        add(s"$prefix.diagnostic.$diagnosticIndex", diagnostic.message)
      }
      row.sample.foreach { sample =>
        add(s"$prefix.block", sample.blockNumber.toString)
        add(s"$prefix.time", decimal(sample.timestamp))
        addOption(s"$prefix.left", sample.left)(eyeSample)
        addOption(s"$prefix.right", sample.right)(eyeSample)
        addOption(s"$prefix.resolution", sample.resolution)(pair)
        addOption(s"$prefix.input", sample.input)(integer)
        addOption(s"$prefix.buttons", sample.buttons)(integer)
        addOption(s"$prefix.status", sample.status)(integer)
        addOption(s"$prefix.head-target", sample.headTarget)(headTarget)
      }
    }

    raw.nativeRows.zipWithIndex.foreach { case (row, index) =>
      val prefix = s"native.$index"
      add(s"$prefix.source-line", row.line.line.source.number.toString)
      addBoolean(s"$prefix.parsed", row.record.nonEmpty)
      row.diagnostics.zipWithIndex.foreach { case (diagnostic, diagnosticIndex) =>
        add(s"$prefix.diagnostic.$diagnosticIndex", diagnostic.message)
      }
      row.record.foreach {
        case AscParsedNativeRecord.Event(event) =>
          add(s"$prefix.kind", "event")
          add(s"$prefix.block", event.blockNumber.toString)
          add(s"$prefix.mode", coordinateMode(event.coordinateMode))
          add(s"$prefix.type", event.eventType.toString)
          add(s"$prefix.phase", event.phase.toString)
          add(s"$prefix.eye", recordedEye(event.eye))
          add(s"$prefix.onset", decimal(event.onset))
          addOption(s"$prefix.interval", event.interval)(interval)
          add(s"$prefix.body", eventBody(event.body))
        case AscParsedNativeRecord.Message(message) =>
          add(s"$prefix.kind", "message")
          addOption(s"$prefix.block", message.blockNumber)(_.toString)
          add(s"$prefix.logged-time", decimal(message.loggedTime))
          addOption(s"$prefix.offset", message.integrationOffset)(value => decimal(value.value))
          add(s"$prefix.effective-time", decimal(message.effectiveTime))
          add(s"$prefix.payload", hex(message.payload.bytes))
          add(s"$prefix.category", message.category.toString)
        case AscParsedNativeRecord.Metadata(metadata) =>
          add(s"$prefix.kind", "metadata")
          add(s"$prefix.block", metadata.blockNumber.toString)
          add(s"$prefix.metadata-kind", metadata.kind.toString)
          metadata.fields.zipWithIndex.foreach { case (field, fieldIndex) =>
            add(s"$prefix.field.$fieldIndex", field)
          }
      }
    }

    addReport(result.report, add)
    result.diagnostics.zipWithIndex.foreach { case (diagnostic, index) =>
      add(s"diagnostic.$index.severity", diagnostic.severity.toString)
      add(s"diagnostic.$index.message", diagnostic.message)
    }

    result.trusted match
      case Left(errors) =>
        addBoolean("trusted", false)
        add("trusted.error-count", errors.length.toString)
      case Right(session) =>
        addBoolean("trusted", true)
        session.observedMessages.marks.zipWithIndex.foreach { case (mark, index) =>
          add(s"observed-message.$index.time-micros", mark.at.toMicros.toString)
          add(s"observed-message.$index.source-line", mark.value.source.number.toString)
          add(s"observed-message.$index.payload", hex(mark.value.payload.bytes))
        }
        session.recordingBlocks.zipWithIndex.foreach { case (block, index) =>
          val prefix = s"recording.$index"
          add(s"$prefix.block", block.blockNumber.toString)
          add(s"$prefix.mode", coordinateMode(block.specification.coordinateMode))
          add(s"$prefix.eyes", eyeLayout(block.specification.eyeLayout))
          add(
            s"$prefix.rate",
            decimal(BigDecimal(block.specification.rate.value.toString))
          )
          add(s"$prefix.samples", block.sampleSources.length.toString)
          add(s"$prefix.artifact", recordingArtifact(block.recording))
        }

    new EyeLinkAscCanonicalManifest(output.result())

  private def addLayout(
      prefix: String,
      layout: Option[AscDataLayout],
      add: (String, String) => Unit
  ): Unit =
    layout match
      case None        => add(s"$prefix.present", "false")
      case Some(value) =>
        add(s"$prefix.present", "true")
        add(s"$prefix.declared-line", value.declaredAt.number.toString)
        add(s"$prefix.mode", value.coordinateMode.fold("absent")(coordinateMode))
        add(s"$prefix.eyes", value.eyeLayout.fold("absent")(eyeLayout))
        add(s"$prefix.rate", value.rateHz.fold("absent")(rate => decimal(rate.value)))
        add(s"$prefix.tracking", value.trackingEvidence.getOrElse("absent"))
        add(s"$prefix.filter", value.filterEvidence.getOrElse("absent"))
        add(
          s"$prefix.optional",
          value.optionalColumns.toVector.map(_.toString).sorted.mkString(",")
        )
        value.unrecognizedTokens.zipWithIndex.foreach { case (token, index) =>
          add(s"$prefix.unknown.$index", token)
        }

  private def addReport(
      report: EyeLinkAscImportReport,
      add: (String, String) => Unit
  ): Unit =
    val fields = Vector(
      "physical-lines"           -> report.physicalLines,
      "parsed-lines"             -> report.parsedLines,
      "framing-rejected-lines"   -> report.framingRejectedLines,
      "typed-records"            -> report.typedRecords,
      "unknown-records"          -> report.unknownRecords,
      "blank-records"            -> report.blankRecords,
      "sample-records"           -> report.sampleRecords,
      "parsed-samples"           -> report.parsedSamples,
      "parser-rejected-samples"  -> report.parserRejectedSamples,
      "materialized-samples"     -> report.materializedSamples,
      "excluded-samples"         -> report.excludedSamples,
      "native-candidate-records" -> report.nativeCandidateRecords,
      "parsed-native-records"    -> report.parsedNativeRecords,
      "rejected-native-records"  -> report.rejectedNativeRecords,
      "native-events"            -> report.nativeEvents,
      "messages"                 -> report.messages,
      "metadata"                 -> report.metadata,
      "recording-blocks"         -> report.recordingBlocks,
      "materialized-recording"   -> report.materializedRecordingBlocks,
      "excluded-records"         -> report.excludedRecords,
      "warnings"                 -> report.warnings,
      "errors"                   -> report.errors
    )
    fields.foreach { case (name, value) => add(s"report.$name", value.toString) }
    add("report.reconciled", report.isReconciled.toString)

  private def origin(value: EyeLinkAscOrigin): String = value match
    case EyeLinkAscOrigin.Converted(receipt) => s"converted:${receipt.canonicalDigest.hex}"
    case EyeLinkAscOrigin.Unidentified(_)    => "unidentified"

  private def record(value: AscRecord): String = value match
    case AscRecord.Blank                   => "blank"
    case AscRecord.Comment(_)              => "comment"
    case AscRecord.Sample(_)               => "sample"
    case AscRecord.Message(_)              => "message"
    case AscRecord.Boundary(kind, _)       => s"boundary:${kind.toString}"
    case AscRecord.Configuration(kind, _)  => s"configuration:${kind.toString}"
    case AscRecord.NativeEvent(kind, _)    => s"native:${kind.toString}"
    case AscRecord.Button(_)               => "button"
    case AscRecord.Input(_)                => "input"
    case AscRecord.LostData(_)             => "lost-data"
    case AscRecord.Unknown(token, _)       => s"unknown:${token.ascii.getOrElse("non-ascii")}"
    case AscRecord.MalformedKnown(kind, _) => s"malformed:${kind.toString}"

  private def terminator(value: AscLineTerminator): String = value match
    case AscLineTerminator.LineFeed               => "lf"
    case AscLineTerminator.CarriageReturnLineFeed => "crlf"
    case AscLineTerminator.EndOfFile              => "eof"

  private def closure(value: AscBlockClosureReason): String = value match
    case AscBlockClosureReason.EndRecord(source)             => s"end:${source.number}"
    case AscBlockClosureReason.ReplacedByNestedStart(source) => s"nested:${source.number}"
    case AscBlockClosureReason.EndOfInput                    => "eof"

  private def eyeLayout(value: AscEyeLayout): String = value match
    case AscEyeLayout.Left      => "left"
    case AscEyeLayout.Right     => "right"
    case AscEyeLayout.Binocular => "binocular"

  private def recordedEye(value: AscRecordedEye): String = value match
    case AscRecordedEye.Left  => "left"
    case AscRecordedEye.Right => "right"

  private def coordinateMode(value: AscCoordinateMode): String = value match
    case AscCoordinateMode.Gaze          => "gaze"
    case AscCoordinateMode.HeadReference => "href"
    case AscCoordinateMode.RawPupil      => "raw"

  private def pupilRepresentation(value: AscPupilRepresentation): String = value match
    case AscPupilRepresentation.Area                => "area"
    case AscPupilRepresentation.Diameter            => "diameter"
    case AscPupilRepresentation.Undocumented(token) => s"undocumented:$token"
    case AscPupilRepresentation.Unspecified         => "unspecified"

  private def eyeSample(value: AscNativeEyeSample): String =
    Vector(
      recordedEye(value.eye),
      coordinateMode(value.coordinates.mode),
      pair(value.coordinates.position),
      pupil(value.pupil),
      value.velocity.fold("absent")(pair)
    ).mkString("|")

  private def pupil(value: AscNativePupil): String =
    val observation = value.observation match
      case AscPupilObservation.MissingDot         => "missing-dot"
      case AscPupilObservation.MissingZero        => "missing-zero"
      case AscPupilObservation.Measured(measured) => s"measured:${decimal(measured)}"
    s"${pupilRepresentation(value.representation)}:$observation"

  private def pair(value: AscNativePair): String = value match
    case AscNativePair.MissingDot   => "missing-dot"
    case AscNativePair.Values(x, y) => s"${decimal(x)},${decimal(y)}"

  private def scalar(value: AscNativeScalar): String = value match
    case AscNativeScalar.MissingDot   => "missing-dot"
    case AscNativeScalar.Value(value) => decimal(value)

  private def integer(value: AscNativeInteger): String = value match
    case AscNativeInteger.MissingDot   => "missing-dot"
    case AscNativeInteger.Value(value) => value.toString

  private def headTarget(value: AscHeadTarget): String =
    s"${pair(value.position)}|${scalar(value.distance)}|${value.flags}"

  private def interval(value: AscNativeInterval): String =
    s"${decimal(value.start)},${decimal(value.end)},${decimal(value.duration)}"

  private def fixation(value: AscNativeFixationSummary): String =
    s"${pair(value.position)}|${scalar(value.pupil)}"

  private def saccade(value: AscNativeSaccadeSummary): String =
    Vector(
      pair(value.startPosition),
      pair(value.endPosition),
      scalar(value.amplitude),
      scalar(value.peakVelocity)
    ).mkString("|")

  private def eventBody(value: AscNativeEventBody): String = value match
    case AscNativeEventBody.StartOnly               => "start"
    case AscNativeEventBody.BlinkEnd                => "blink-end"
    case AscNativeEventBody.PreservedUpdate(fields) =>
      s"update:${fields.map(field => s"${field.length}:$field").mkString}"
    case AscNativeEventBody.FixationEnd(primary, gaze, resolution) =>
      Vector(
        "fixation-end",
        fixation(primary),
        gaze.fold("absent")(fixation),
        resolution.fold("absent")(pair)
      ).mkString(":")
    case AscNativeEventBody.SaccadeEnd(primary, gaze, resolution) =>
      Vector(
        "saccade-end",
        saccade(primary),
        gaze.fold("absent")(saccade),
        resolution.fold("absent")(pair)
      ).mkString(":")

  private def recordingArtifact(value: EyeLinkAscRecordingArtifact): String = value match
    case EyeLinkAscRecordingArtifact.Monocular(recording) => s"monocular:${recording.size}"
    case EyeLinkAscRecordingArtifact.Binocular(recording) => s"binocular:${recording.size}"

  private def decimal(value: BigDecimal): String =
    if value == 0 then "0"
    else value.bigDecimal.stripTrailingZeros.toPlainString

  private def hex(bytes: IArray[Byte]): String =
    val digits = "0123456789abcdef"
    val output = new java.lang.StringBuilder(bytes.length * 2)
    var index  = 0
    while index < bytes.length do
      val value = bytes(index) & 0xff
      output.append(digits.charAt(value >>> 4))
      output.append(digits.charAt(value & 0x0f))
      index += 1
    output.toString

end EyeLinkAscCanonicalManifest

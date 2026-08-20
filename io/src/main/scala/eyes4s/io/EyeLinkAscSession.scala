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

import cats.data.NonEmptyVector
import eyes4s.core.BinocularRecording
import eyes4s.core.Eye
import eyes4s.core.Gaze
import eyes4s.core.PupilUnit
import eyes4s.core.Rate
import eyes4s.core.Recording
import eyes4s.core.RecordingError
import eyes4s.core.Sample
import eyes4s.core.SamplingTolerance
import eyes4s.kernel.ClockId
import eyes4s.kernel.Frame
import eyes4s.kernel.Hz
import eyes4s.kernel.ObservedTimeline
import eyes4s.kernel.Unit2D

/** What to do when sample values contain measured pupil data but the ASC block
  * never declares whether that value is area, diameter, or a vendor unit.
  */
enum AscUnspecifiedPupilPolicy derives CanEqual:
  case RejectMeasuredValues
  case TreatMeasuredValuesAsArbitrary

/** Complete caller-supplied context needed to turn native ASC values into
  * trusted eyes4s artifacts.
  */
final class EyeLinkAscSessionConfig private (
    val frame: Frame[Unit2D.Px],
    val clock: ClockId,
    val conversionEvidence: ConversionEvidencePolicy,
    val blinkReconciliation: AscBlinkReconciliationPolicy,
    val unspecifiedPupil: AscUnspecifiedPupilPolicy,
    val samplingTolerance: SamplingTolerance
)

object EyeLinkAscSessionConfig:
  def of(
      frame: Frame[Unit2D.Px],
      clock: ClockId,
      conversionEvidence: ConversionEvidencePolicy,
      blinkReconciliation: AscBlinkReconciliationPolicy,
      unspecifiedPupil: AscUnspecifiedPupilPolicy,
      samplingTolerance: SamplingTolerance = SamplingTolerance.TimestampQuantisation
  ): Either[EyeLinkAscSessionConfigError, EyeLinkAscSessionConfig] =
    if frame.id.name.trim.isEmpty then
      Left(EyeLinkAscSessionConfigError.BlankFrame(frame.id.name))
    else if clock.name.trim.isEmpty then
      Left(EyeLinkAscSessionConfigError.BlankClock(clock.name))
    else
      Right(
        new EyeLinkAscSessionConfig(
          frame,
          clock,
          conversionEvidence,
          blinkReconciliation,
          unspecifiedPupil,
          samplingTolerance
        )
      )

enum EyeLinkAscSessionConfigError derives CanEqual:
  case BlankFrame(value: String)
  case BlankClock(value: String)

  def message: String = this match
    case BlankFrame(value) => s"EyeLink session pixel frame='$value' is blank."
    case BlankClock(value) => s"EyeLink session tracker clock='$value' is blank."

end EyeLinkAscSessionConfigError

/** Lossless intermediate session. Parsed and rejected physical lines, block
  * snapshots, sample parse results, and native parse results all remain
  * inspectable even when no trusted session can be assembled.
  */
final class EyeLinkAscRawSession private[io] (
    val origin: EyeLinkAscOrigin,
    val computedDigest: Option[Sha256],
    val framing: Vector[AscFramingEmission],
    val lines: Vector[AscLineResult],
    val blockEmissions: Vector[AscBlockEmission],
    val sampleRows: Vector[AscSampleParseResult],
    val nativeRows: Vector[AscNativeParseResult]
)

/** Exact native sample configuration retained on a materialized recording
  * block.
  */
final class EyeLinkAscBlockSpecification private[io] (
    val coordinateMode: AscCoordinateMode,
    val eyeLayout: AscEyeLayout,
    val rate: Hz,
    val pupilRepresentation: AscPupilRepresentation,
    val pupilUnit: Option[PupilUnit],
    val declaredAt: AscSourceLine
)

enum EyeLinkAscRecordingArtifact:
  case Monocular(value: Recording[Unit2D.Px])
  case Binocular(value: BinocularRecording[Unit2D.Px])

/** One validated recording block with a one-to-one source row index and the
  * native records that occurred in that block.
  */
final class EyeLinkAscRecordingBlock private[io] (
    val blockNumber: Long,
    val openedAt: AscSourceLine,
    val closedBy: AscBlockClosureReason,
    val specification: EyeLinkAscBlockSpecification,
    val sampleSources: IArray[AscSourceLine],
    val recording: EyeLinkAscRecordingArtifact,
    val nativeEvents: Vector[AscParsedNativeEvent],
    val messages: Vector[AscParsedMessage],
    val metadata: Vector[AscParsedNativeMetadata]
)

/** Trusted result. Vendor-native events remain a separate ledger and messages
  * remain an observed tracker-clock timeline.
  */
final class EyeLinkAscSession private[io] (
    val raw: EyeLinkAscRawSession,
    val conversion: ConversionEvidenceReport,
    val frame: Frame[Unit2D.Px],
    val clock: ClockId,
    val recordingBlocks: Vector[EyeLinkAscRecordingBlock],
    val nativeEvents: AscNativeEventLedger,
    val observedMessages: ObservedTimeline[AscParsedMessage],
    val metadata: Vector[AscParsedNativeMetadata]
)

enum EyeLinkAscDiagnosticSeverity derives CanEqual:
  case Warning
  case Error

enum EyeLinkAscSessionDiagnostic:
  case ConversionFailure(value: Edf2AscProvenanceError)
  case ConversionWarning(value: ConversionEvidenceWarning)
  case Framing(value: AscFramingDiagnostic)
  case Lexical(value: AscLexicalDiagnostic)
  case BlockRequirement(value: AscBlockRequirement)
  case BlockOutcome(value: AscBlockOutcome)
  case Sample(value: AscSampleDiagnostic)
  case Native(value: AscNativeDiagnostic)
  case Pairing(value: AscNativePairingDiagnostic)
  case Timeline(value: AscNativeTimelineError)
  case UnknownRecord(source: String, line: Long, token: String)
  case SourceChanged(expected: String, actual: String, line: Long)
  case SourceSequenceMismatch(
      source: String,
      expectedLine: Long,
      actualLine: Long,
      expectedByteOffset: Long,
      actualByteOffset: Long
  )
  case TerminatorBeforeFinalLine(source: String, line: Long)
  case SourceDigestMismatch(source: String, declared: Sha256, computed: Sha256)
  case BlockReplacedByNestedStart(
      source: String,
      line: Long,
      block: Long,
      replacementSource: String,
      replacementLine: Long
  )
  case BlockUnclosedAtEndOfInput(source: String, line: Long, block: Long)
  case UnsupportedCoordinateMode(
      source: String,
      line: Long,
      block: Long,
      mode: AscCoordinateMode,
      frame: String
  )
  case ConfigurationChanged(
      source: String,
      line: Long,
      block: Long,
      initialMode: AscCoordinateMode,
      currentMode: AscCoordinateMode,
      initialEyes: AscEyeLayout,
      currentEyes: AscEyeLayout,
      initialRate: BigDecimal,
      currentRate: BigDecimal,
      initialPupil: AscPupilRepresentation,
      currentPupil: AscPupilRepresentation
  )
  case RateOutsideDoubleRange(source: String, line: Long, block: Long, rate: String)
  case UnspecifiedMeasuredPupil(source: String, line: Long, block: Long)
  case PupilTreatedAsArbitrary(source: String, line: Long, block: Long)
  case UndocumentedPupilUnit(source: String, line: Long, block: Long, token: String)
  case OffSurfacePupilExcluded(
      source: String,
      line: Long,
      block: Long,
      eye: AscRecordedEye,
      value: BigDecimal
  )
  case PupilOutsideDoubleRange(
      source: String,
      line: Long,
      block: Long,
      eye: AscRecordedEye,
      value: String
  )
  case MissingEyeSample(source: String, line: Long, block: Long, eye: AscRecordedEye)
  case SampleMaterialization(
      source: String,
      line: Long,
      block: Long,
      value: AscSampleMaterializationError
  )
  case RecordingConstruction(block: Long, source: String, value: RecordingError)
  case EmptyDeclaredSampleBlock(source: String, line: Long, block: Long)
  case NoRecordingBlocks(sourceDigest: Sha256)
  case AssemblyUnavailable(sourceDigest: Sha256)

  def severity: EyeLinkAscDiagnosticSeverity = this match
    case ConversionWarning(_) | UnknownRecord(_, _, _) | PupilTreatedAsArbitrary(_, _, _) |
        UndocumentedPupilUnit(_, _, _, _) | OffSurfacePupilExcluded(_, _, _, _, _) =>
      EyeLinkAscDiagnosticSeverity.Warning
    case BlockOutcome(AscBlockOutcome.DuplicateConfiguration(_, _, _, _, _, false)) =>
      EyeLinkAscDiagnosticSeverity.Warning
    case Sample(AscSampleDiagnostic.PupilRepresentationUndeclared(_, _, _)) =>
      EyeLinkAscDiagnosticSeverity.Warning
    case ConversionFailure(_) | Framing(_) | Lexical(_) | BlockRequirement(_) | BlockOutcome(
          _
        ) | Sample(_) | Native(_) | Pairing(_) | Timeline(_) | SourceChanged(_, _, _) |
        SourceSequenceMismatch(_, _, _, _, _) | TerminatorBeforeFinalLine(_, _) |
        SourceDigestMismatch(_, _, _) | BlockReplacedByNestedStart(_, _, _, _, _) |
        BlockUnclosedAtEndOfInput(_, _, _) | UnsupportedCoordinateMode(_, _, _, _, _) |
        ConfigurationChanged(_, _, _, _, _, _, _, _, _, _, _) |
        RateOutsideDoubleRange(_, _, _, _) | UnspecifiedMeasuredPupil(_, _, _) |
        PupilOutsideDoubleRange(_, _, _, _, _) | MissingEyeSample(_, _, _, _) |
        SampleMaterialization(_, _, _, _) | RecordingConstruction(_, _, _) |
        EmptyDeclaredSampleBlock(_, _, _) | NoRecordingBlocks(_) | AssemblyUnavailable(_) =>
      EyeLinkAscDiagnosticSeverity.Error

  def message: String = this match
    case ConversionFailure(value)           => value.message
    case ConversionWarning(value)           => value.message
    case Framing(value)                     => value.message
    case Lexical(value)                     => value.message
    case BlockRequirement(value)            => value.message
    case BlockOutcome(value)                => value.message
    case Sample(value)                      => value.message
    case Native(value)                      => value.message
    case Pairing(value)                     => value.message
    case Timeline(value)                    => value.message
    case UnknownRecord(source, line, token) =>
      s"ASC source='$source' line=$line has unknown record token='$token'; the raw line is retained."
    case SourceChanged(expected, actual, line) =>
      s"ASC session expected source='$expected' but line=$line belongs to source='$actual'."
    case SourceSequenceMismatch(
          source,
          expectedLine,
          actualLine,
          expectedOffset,
          actualOffset
        ) =>
      s"ASC source='$source' expected line=$expectedLine byteOffset=$expectedOffset but found line=$actualLine byteOffset=$actualOffset."
    case TerminatorBeforeFinalLine(source, line) =>
      s"ASC source='$source' line=$line has an end-of-file terminator before the final physical line."
    case SourceDigestMismatch(source, declared, computed) =>
      s"ASC source='$source' declared digest=${declared.hex} disagrees with reconstructed digest=${computed.hex}."
    case BlockReplacedByNestedStart(source, line, block, replacementSource, replacementLine) =>
      s"ASC source='$source' line=$line block=$block was replaced by nested START source='$replacementSource' line=$replacementLine rather than closed by END."
    case BlockUnclosedAtEndOfInput(source, line, block) =>
      s"ASC source='$source' line=$line block=$block reached end of input without an END record."
    case UnsupportedCoordinateMode(source, line, block, mode, frame) =>
      s"ASC source='$source' line=$line block=$block coordinate mode=$mode cannot materialize in pixel frame='$frame'."
    case ConfigurationChanged(
          source,
          line,
          block,
          initialMode,
          currentMode,
          initialEyes,
          currentEyes,
          initialRate,
          currentRate,
          initialPupil,
          currentPupil
        ) =>
      s"ASC source='$source' line=$line block=$block sample configuration changed from mode=$initialMode eyes=$initialEyes rate=${AscDiagnosticText.bounded(initialRate.toString)} pupil=$initialPupil to mode=$currentMode eyes=$currentEyes rate=${AscDiagnosticText.bounded(currentRate.toString)} pupil=$currentPupil."
    case RateOutsideDoubleRange(source, line, block, rate) =>
      s"ASC source='$source' line=$line block=$block rate=$rate Hz is outside the finite Double range."
    case UnspecifiedMeasuredPupil(source, line, block) =>
      s"ASC source='$source' line=$line block=$block has measured pupil values but no declared pupil representation."
    case PupilTreatedAsArbitrary(source, line, block) =>
      s"ASC source='$source' line=$line block=$block has measured undeclared pupil values explicitly treated as arbitrary units."
    case UndocumentedPupilUnit(source, line, block, token) =>
      s"ASC source='$source' line=$line block=$block pupil unit='$token' is retained and materialized as arbitrary units."
    case OffSurfacePupilExcluded(source, line, block, eye, value) =>
      s"ASC source='$source' line=$line block=$block eye=$eye pupil=${AscDiagnosticText.bounded(value.toString)} is retained in raw evidence but excluded from the core off-surface gaze value."
    case PupilOutsideDoubleRange(source, line, block, eye, value) =>
      s"ASC source='$source' line=$line block=$block eye=$eye pupil=$value exceeds the finite positive Double range."
    case MissingEyeSample(source, line, block, eye) =>
      s"ASC source='$source' line=$line block=$block has no parsed $eye sample required by its eye layout."
    case SampleMaterialization(source, line, block, value) =>
      s"ASC source='$source' line=$line block=$block sample materialization failed: ${value.message}"
    case RecordingConstruction(block, source, value) =>
      s"ASC source='$source' block=$block recording construction failed: ${value.message}"
    case EmptyDeclaredSampleBlock(source, line, block) =>
      s"ASC source='$source' line=$line block=$block declares a sample stream but contains no sample records."
    case NoRecordingBlocks(digest) =>
      s"ASC digest=${digest.hex} contains no sample block that can become a recording."
    case AssemblyUnavailable(digest) =>
      s"ASC digest=${digest.hex} could not assemble a trusted session despite having no reported materialization result."

end EyeLinkAscSessionDiagnostic

/** Reconciled accounting for each lossy boundary in the ingest path. */
final class EyeLinkAscImportReport private[io] (
    val physicalLines: Int,
    val parsedLines: Int,
    val framingRejectedLines: Int,
    val typedRecords: Int,
    val unknownRecords: Int,
    val blankRecords: Int,
    val sampleRecords: Int,
    val parsedSamples: Int,
    val parserRejectedSamples: Int,
    val materializedSamples: Int,
    val excludedSamples: Int,
    val nativeCandidateRecords: Int,
    val parsedNativeRecords: Int,
    val rejectedNativeRecords: Int,
    val nativeEvents: Int,
    val messages: Int,
    val metadata: Int,
    val recordingBlocks: Int,
    val materializedRecordingBlocks: Int,
    val excludedRecords: Int,
    val warnings: Int,
    val errors: Int
):
  def physicalLinesReconcile: Boolean =
    physicalLines == parsedLines + framingRejectedLines

  def parsedLinesReconcile: Boolean =
    parsedLines == typedRecords + unknownRecords + blankRecords

  def sampleParsingReconcile: Boolean =
    sampleRecords == parsedSamples + parserRejectedSamples

  def sampleMaterializationReconcile: Boolean =
    sampleRecords == materializedSamples + excludedSamples

  def nativeRecordsReconcile: Boolean =
    nativeCandidateRecords == parsedNativeRecords + rejectedNativeRecords

  def isReconciled: Boolean =
    physicalLinesReconcile && parsedLinesReconcile && sampleParsingReconcile &&
      sampleMaterializationReconcile && nativeRecordsReconcile

/** Raw evidence, accumulated diagnostics and (only when error-free) the trusted
  * session. Warnings never disappear into successful field access.
  */
final class EyeLinkAscSessionMaterialization private[io] (
    val raw: EyeLinkAscRawSession,
    val report: EyeLinkAscImportReport,
    val diagnostics: Vector[EyeLinkAscSessionDiagnostic],
    private val assembled: Option[EyeLinkAscSession]
):
  def warnings: Vector[EyeLinkAscSessionDiagnostic] =
    diagnostics.filter(_.severity == EyeLinkAscDiagnosticSeverity.Warning)

  def errors: Vector[EyeLinkAscSessionDiagnostic] =
    diagnostics.filter(_.severity == EyeLinkAscDiagnosticSeverity.Error)

  def trusted: Either[NonEmptyVector[EyeLinkAscSessionDiagnostic], EyeLinkAscSession] =
    NonEmptyVector.fromVector(errors) match
      case Some(values) => Left(values)
      case None         =>
        assembled match
          case Some(value) => Right(value)
          case None        =>
            Left(
              NonEmptyVector.one(
                EyeLinkAscSessionDiagnostic.AssemblyUnavailable(raw.origin.ascDigest)
              )
            )

object EyeLinkAscSessions:

  private final case class CandidateSpec(
      mode: AscCoordinateMode,
      eyes: AscEyeLayout,
      rate: BigDecimal,
      pupil: AscPupilRepresentation,
      declaredAt: AscSourceLine
  ):
    def agreesWith(other: CandidateSpec): Boolean =
      mode == other.mode && eyes == other.eyes && rate == other.rate && pupil == other.pupil

  private final case class BlockResult(
      block: Option[EyeLinkAscRecordingBlock],
      diagnostics: Vector[EyeLinkAscSessionDiagnostic]
  )

  private final case class SourceValidation(
      digest: Option[Sha256],
      diagnostics: Vector[EyeLinkAscSessionDiagnostic]
  )

  /** Assemble one already-framed ASC source. Every framing emission is retained
    * in the raw session and every scientific exclusion is counted and named.
    */
  def materialize(
      origin: EyeLinkAscOrigin,
      config: EyeLinkAscSessionConfig,
      framing: Vector[AscFramingEmission]
  ): EyeLinkAscSessionMaterialization =
    val lines          = framing.collect { case AscFramingEmission.Parsed(value) => value }
    val blockEmissions = EyeLinkAscBlocks.machine.runAll(lines)
    val sampleRows     = EyeLinkAscSamples.machine
      .runAll(blockEmissions)
      .collect { case AscSampleEmission.Parsed(value) => value }
    val nativeRows = EyeLinkAscNative.machine
      .runAll(blockEmissions)
      .collect { case AscNativeEmission.Parsed(value) => value }
    val lineAccounting   = EyeLinkAscAccounting.audit(framing)
    val sourceValidation = validateSource(framing)
    val raw              = new EyeLinkAscRawSession(
      origin,
      sourceValidation.digest,
      framing,
      lines,
      blockEmissions,
      sampleRows,
      nativeRows
    )

    val diagnostics = Vector.newBuilder[EyeLinkAscSessionDiagnostic]
    diagnostics ++= sourceValidation.diagnostics
    sourceValidation.digest.foreach { computed =>
      if computed != origin.ascDigest then
        diagnostics += EyeLinkAscSessionDiagnostic.SourceDigestMismatch(
          lines.headOption.fold("<empty>")(_.source.source),
          origin.ascDigest,
          computed
        )
    }
    framing.foreach {
      case AscFramingEmission.Rejected(value) =>
        diagnostics += EyeLinkAscSessionDiagnostic.Framing(value)
      case AscFramingEmission.Parsed(_) => ()
    }
    lines.foreach { line =>
      diagnostics ++= line.diagnostics.map(EyeLinkAscSessionDiagnostic.Lexical.apply)
      line.record match
        case AscRecord.Unknown(token, _) =>
          diagnostics += EyeLinkAscSessionDiagnostic.UnknownRecord(
            line.source.source,
            line.source.number,
            AscDiagnosticText.bounded(token.ascii.getOrElse(line.source.diagnosticExcerpt))
          )
        case AscRecord.Blank | AscRecord.Comment(_) | AscRecord.Sample(_) |
            AscRecord.Message(_) | AscRecord.Boundary(_, _) | AscRecord.Configuration(_, _) |
            AscRecord.NativeEvent(_, _) | AscRecord.Button(_) | AscRecord.Input(_) |
            AscRecord.LostData(_) | AscRecord.MalformedKnown(_, _) =>
          ()
    }
    blockEmissions.foreach {
      case AscBlockEmission.Line(value) =>
        diagnostics ++= value.requirements.map(
          EyeLinkAscSessionDiagnostic.BlockRequirement.apply
        )
        diagnostics ++= value.outcomes.map(EyeLinkAscSessionDiagnostic.BlockOutcome.apply)
      case AscBlockEmission.Closed(value) =>
        value.reason match
          case AscBlockClosureReason.EndRecord(_)                       => ()
          case AscBlockClosureReason.ReplacedByNestedStart(replacement) =>
            diagnostics += EyeLinkAscSessionDiagnostic.BlockReplacedByNestedStart(
              value.block.start.source.source,
              value.block.start.source.number,
              value.block.start.blockNumber,
              replacement.source,
              replacement.number
            )
          case AscBlockClosureReason.EndOfInput =>
            diagnostics += EyeLinkAscSessionDiagnostic.BlockUnclosedAtEndOfInput(
              value.block.start.source.source,
              value.block.start.source.number,
              value.block.start.blockNumber
            )
    }
    sampleRows.foreach(row =>
      row.diagnostics.foreach {
        case AscSampleDiagnostic.BlockRequirement(_) => ()
        case value => diagnostics += EyeLinkAscSessionDiagnostic.Sample(value)
      }
    )
    nativeRows.foreach(row =>
      row.diagnostics.foreach {
        case AscNativeDiagnostic.BlockRequirement(_) => ()
        case value => diagnostics += EyeLinkAscSessionDiagnostic.Native(value)
      }
    )

    val conversion = origin.assess(config.conversionEvidence) match
      case Left(error) =>
        diagnostics += EyeLinkAscSessionDiagnostic.ConversionFailure(error)
        None
      case Right(report) =>
        diagnostics ++= report.warnings.map(EyeLinkAscSessionDiagnostic.ConversionWarning.apply)
        Some(report)

    val parsedNative = nativeRows.flatMap(_.record)
    val events       = parsedNative.collect { case AscParsedNativeRecord.Event(value) => value }
    val messages = parsedNative.collect { case AscParsedNativeRecord.Message(value) => value }
    val metadata = parsedNative.collect { case AscParsedNativeRecord.Metadata(value) => value }
    val ledger   = AscNativeEventLedger.reconcile(events)
    diagnostics ++= ledger.diagnostics.map(EyeLinkAscSessionDiagnostic.Pairing.apply)

    val observed = EyeLinkAscNativeTimeline.observedMessages(config.clock, messages) match
      case Left(errors) =>
        diagnostics ++= errors.toVector.map(EyeLinkAscSessionDiagnostic.Timeline.apply)
        None
      case Right(value) => Some(value)

    val closures = blockEmissions.collect { case AscBlockEmission.Closed(value) =>
      value.block.start.blockNumber -> value
    }.toMap
    val sampleGroups = sampleRows
      .flatMap(row => row.line.block.map(_.start.blockNumber -> row))
      .groupMap(_._1)(_._2)
      .toVector
      .sortBy(_._1)
    val sampledBlockNumbers = sampleGroups.map(_._1).toSet
    closures.values.foreach { closed =>
      val start = closed.block.start
      if start.declaresSamples && !sampledBlockNumbers.contains(start.blockNumber) then
        diagnostics += EyeLinkAscSessionDiagnostic.EmptyDeclaredSampleBlock(
          start.source.source,
          start.source.number,
          start.blockNumber
        )
    }
    val blockResults = sampleGroups.map { case (blockNumber, rows) =>
      materializeBlock(
        blockNumber,
        rows,
        closures.get(blockNumber),
        config,
        ledger,
        events,
        messages,
        metadata
      )
    }
    diagnostics ++= blockResults.flatMap(_.diagnostics)
    val materializedBlocks = blockResults.flatMap(_.block)
    if sampleGroups.isEmpty && closures.isEmpty then
      diagnostics += EyeLinkAscSessionDiagnostic.NoRecordingBlocks(origin.ascDigest)

    val allDiagnostics = diagnostics.result()
    val hasErrors      = allDiagnostics.exists(
      _.severity == EyeLinkAscDiagnosticSeverity.Error
    )
    val session =
      if hasErrors then None
      else
        for
          acceptedConversion <- conversion
          acceptedObserved   <- observed
        yield new EyeLinkAscSession(
          raw,
          acceptedConversion,
          config.frame,
          config.clock,
          materializedBlocks,
          ledger,
          acceptedObserved,
          metadata
        )

    val parsedNativeCount   = nativeRows.count(_.record.nonEmpty)
    val parsedSamples       = sampleRows.count(_.sample.nonEmpty)
    val materializedSamples = materializedBlocks.map(_.sampleSources.length).sum
    val excludedSamples     = sampleRows.length - materializedSamples
    val report              = new EyeLinkAscImportReport(
      physicalLines = lineAccounting.physicalLines,
      parsedLines = lines.length,
      framingRejectedLines = lineAccounting.rejectedLines,
      typedRecords = lineAccounting.typedRecords,
      unknownRecords = lineAccounting.preservedUnknown,
      blankRecords = lineAccounting.blankLines,
      sampleRecords = sampleRows.length,
      parsedSamples = parsedSamples,
      parserRejectedSamples = sampleRows.length - parsedSamples,
      materializedSamples = materializedSamples,
      excludedSamples = excludedSamples,
      nativeCandidateRecords = nativeRows.length,
      parsedNativeRecords = parsedNativeCount,
      rejectedNativeRecords = nativeRows.length - parsedNativeCount,
      nativeEvents = events.length,
      messages = messages.length,
      metadata = metadata.length,
      recordingBlocks = closures.size,
      materializedRecordingBlocks = materializedBlocks.length,
      excludedRecords =
        lineAccounting.rejectedLines + excludedSamples + nativeRows.length - parsedNativeCount,
      warnings = allDiagnostics.count(_.severity == EyeLinkAscDiagnosticSeverity.Warning),
      errors = allDiagnostics.count(_.severity == EyeLinkAscDiagnosticSeverity.Error)
    )
    new EyeLinkAscSessionMaterialization(raw, report, allDiagnostics, session)

  private def materializeBlock(
      blockNumber: Long,
      rows: Vector[AscSampleParseResult],
      closure: Option[AscClosedBlock],
      config: EyeLinkAscSessionConfig,
      ledger: AscNativeEventLedger,
      events: Vector[AscParsedNativeEvent],
      messages: Vector[AscParsedMessage],
      metadata: Vector[AscParsedNativeMetadata]
  ): BlockResult =
    val diagnostics = Vector.newBuilder[EyeLinkAscSessionDiagnostic]
    val samples     = rows.flatMap(_.sample)
    val specs       = rows.flatMap { row =>
      for
        sampleBlock <- row.line.block
        layout      <- sampleBlock.configuration.samples
        mode        <- layout.coordinateMode
        eyes        <- layout.eyeLayout
        rate        <- layout.rateHz
      yield CandidateSpec(
        mode,
        eyes,
        rate.value,
        sampleBlock.configuration.pupil.getOrElse(AscPupilRepresentation.Unspecified),
        layout.declaredAt
      )
    }
    val initial = specs.headOption
    initial.foreach { first =>
      specs.drop(1).foreach { current =>
        if !first.agreesWith(current) then
          diagnostics += EyeLinkAscSessionDiagnostic.ConfigurationChanged(
            current.declaredAt.source,
            current.declaredAt.number,
            blockNumber,
            first.mode,
            current.mode,
            first.eyes,
            current.eyes,
            first.rate,
            current.rate,
            first.pupil,
            current.pupil
          )
      }
    }

    val artifact = for
      spec      <- initial
      closed    <- closure
      fixedRate <- rate(spec, blockNumber, diagnostics)
      pupilUnit <- pupilUnit(spec, blockNumber, samples, config, diagnostics)
      recording <- recording(
        blockNumber,
        samples,
        spec,
        fixedRate,
        pupilUnit,
        config,
        ledger,
        diagnostics
      )
    yield new EyeLinkAscRecordingBlock(
      blockNumber,
      closed.block.start.source,
      closed.reason,
      new EyeLinkAscBlockSpecification(
        spec.mode,
        spec.eyes,
        fixedRate,
        spec.pupil,
        pupilUnit,
        spec.declaredAt
      ),
      IArray.from(samples.map(_.source)),
      recording,
      events.filter(_.blockNumber == blockNumber),
      messages.filter(_.blockNumber.contains(blockNumber)),
      metadata.filter(_.blockNumber == blockNumber)
    )
    BlockResult(artifact, diagnostics.result())

  private def rate(
      spec: CandidateSpec,
      block: Long,
      diagnostics: scala.collection.mutable.Builder[EyeLinkAscSessionDiagnostic, Vector[
        EyeLinkAscSessionDiagnostic
      ]]
  ): Option[Hz] =
    val value = spec.rate.toDouble
    Hz(value) match
      case Left(_) =>
        diagnostics += EyeLinkAscSessionDiagnostic.RateOutsideDoubleRange(
          spec.declaredAt.source,
          spec.declaredAt.number,
          block,
          AscDiagnosticText.bounded(spec.rate.toString)
        )
        None
      case Right(rate) => Some(rate)

  private def pupilUnit(
      spec: CandidateSpec,
      block: Long,
      samples: Vector[AscNativeSample],
      config: EyeLinkAscSessionConfig,
      diagnostics: scala.collection.mutable.Builder[EyeLinkAscSessionDiagnostic, Vector[
        EyeLinkAscSessionDiagnostic
      ]]
  ): Option[Option[PupilUnit]] =
    val measured = samples.exists(sample =>
      (sample.left.toVector ++ sample.right.toVector).exists(_.pupil.observation match
        case AscPupilObservation.Measured(_) => true
        case AscPupilObservation.MissingDot  => false
        case AscPupilObservation.MissingZero => false)
    )
    spec.pupil match
      case AscPupilRepresentation.Area                => Some(Some(PupilUnit.Area))
      case AscPupilRepresentation.Diameter            => Some(Some(PupilUnit.Diameter))
      case AscPupilRepresentation.Undocumented(token) =>
        diagnostics += EyeLinkAscSessionDiagnostic.UndocumentedPupilUnit(
          spec.declaredAt.source,
          spec.declaredAt.number,
          block,
          AscDiagnosticText.bounded(token)
        )
        Some(Some(PupilUnit.Arbitrary))
      case AscPupilRepresentation.Unspecified if !measured => Some(None)
      case AscPupilRepresentation.Unspecified              =>
        config.unspecifiedPupil match
          case AscUnspecifiedPupilPolicy.RejectMeasuredValues =>
            diagnostics += EyeLinkAscSessionDiagnostic.UnspecifiedMeasuredPupil(
              spec.declaredAt.source,
              spec.declaredAt.number,
              block
            )
            None
          case AscUnspecifiedPupilPolicy.TreatMeasuredValuesAsArbitrary =>
            diagnostics += EyeLinkAscSessionDiagnostic.PupilTreatedAsArbitrary(
              spec.declaredAt.source,
              spec.declaredAt.number,
              block
            )
            Some(Some(PupilUnit.Arbitrary))

  private def recording(
      block: Long,
      samples: Vector[AscNativeSample],
      spec: CandidateSpec,
      rate: Hz,
      pupilUnit: Option[PupilUnit],
      config: EyeLinkAscSessionConfig,
      ledger: AscNativeEventLedger,
      diagnostics: scala.collection.mutable.Builder[EyeLinkAscSessionDiagnostic, Vector[
        EyeLinkAscSessionDiagnostic
      ]]
  ): Option[EyeLinkAscRecordingArtifact] =
    if spec.mode != AscCoordinateMode.Gaze then
      diagnostics += EyeLinkAscSessionDiagnostic.UnsupportedCoordinateMode(
        spec.declaredAt.source,
        spec.declaredAt.number,
        block,
        spec.mode,
        config.frame.id.name
      )
      None
    else
      spec.eyes match
        case AscEyeLayout.Left =>
          monocular(
            block,
            samples,
            AscRecordedEye.Left,
            Eye.Left,
            rate,
            pupilUnit,
            config,
            ledger,
            diagnostics
          ).map(EyeLinkAscRecordingArtifact.Monocular.apply)
        case AscEyeLayout.Right =>
          monocular(
            block,
            samples,
            AscRecordedEye.Right,
            Eye.Right,
            rate,
            pupilUnit,
            config,
            ledger,
            diagnostics
          ).map(EyeLinkAscRecordingArtifact.Monocular.apply)
        case AscEyeLayout.Binocular =>
          binocular(block, samples, rate, pupilUnit, config, ledger, diagnostics)
            .map(EyeLinkAscRecordingArtifact.Binocular.apply)

  private def monocular(
      block: Long,
      native: Vector[AscNativeSample],
      recordedEye: AscRecordedEye,
      eye: Eye,
      rate: Hz,
      pupilUnit: Option[PupilUnit],
      config: EyeLinkAscSessionConfig,
      ledger: AscNativeEventLedger,
      diagnostics: scala.collection.mutable.Builder[EyeLinkAscSessionDiagnostic, Vector[
        EyeLinkAscSessionDiagnostic
      ]]
  ): Option[Recording[Unit2D.Px]] =
    val converted =
      native.map(sample => sampleOf(sample, recordedEye, config.frame, diagnostics))
    if converted.exists(_.isEmpty) then None
    else
      val values = IArray.from(converted.flatten)
      EyeLinkAscNativeTimeline
        .reconcileBlinkSamples(
          block,
          recordedEye,
          values,
          ledger,
          config.blinkReconciliation
        )
        .fold(
          errors =>
            diagnostics ++= errors.toVector.map(EyeLinkAscSessionDiagnostic.Timeline.apply)
            None
          ,
          reconciled =>
            Recording
              .of(
                config.frame,
                config.clock,
                Rate.Fixed(rate),
                eye,
                pupilUnit,
                reconciled,
                config.samplingTolerance
              )
              .fold(
                error =>
                  diagnostics += EyeLinkAscSessionDiagnostic.RecordingConstruction(
                    block,
                    native.headOption.fold("<unknown>")(_.source.source),
                    error
                  )
                  None
                ,
                Some.apply
              )
        )

  private def binocular(
      block: Long,
      native: Vector[AscNativeSample],
      rate: Hz,
      pupilUnit: Option[PupilUnit],
      config: EyeLinkAscSessionConfig,
      ledger: AscNativeEventLedger,
      diagnostics: scala.collection.mutable.Builder[EyeLinkAscSessionDiagnostic, Vector[
        EyeLinkAscSessionDiagnostic
      ]]
  ): Option[BinocularRecording[Unit2D.Px]] =
    val left =
      native.map(sample => sampleOf(sample, AscRecordedEye.Left, config.frame, diagnostics))
    val right =
      native.map(sample => sampleOf(sample, AscRecordedEye.Right, config.frame, diagnostics))
    if left.exists(_.isEmpty) || right.exists(_.isEmpty) then None
    else
      val leftSamples  = IArray.from(left.flatten)
      val rightSamples = IArray.from(right.flatten)
      val reconciled   = for
        acceptedLeft <- EyeLinkAscNativeTimeline.reconcileBlinkSamples(
          block,
          AscRecordedEye.Left,
          leftSamples,
          ledger,
          config.blinkReconciliation
        )
        acceptedRight <- EyeLinkAscNativeTimeline.reconcileBlinkSamples(
          block,
          AscRecordedEye.Right,
          rightSamples,
          ledger,
          config.blinkReconciliation
        )
      yield acceptedLeft -> acceptedRight
      reconciled.fold(
        errors =>
          diagnostics ++= errors.toVector.map(EyeLinkAscSessionDiagnostic.Timeline.apply)
          None
        ,
        pair =>
          BinocularRecording
            .of(
              config.frame,
              config.clock,
              Rate.Fixed(rate),
              pupilUnit,
              IArray.from(pair._1.iterator.map(_.t)),
              IArray.from(pair._1.iterator.map(_.gaze)),
              IArray.from(pair._2.iterator.map(_.gaze)),
              config.samplingTolerance
            )
            .fold(
              error =>
                diagnostics += EyeLinkAscSessionDiagnostic.RecordingConstruction(
                  block,
                  native.headOption.fold("<unknown>")(_.source.source),
                  error
                )
                None
              ,
              Some.apply
            )
      )

  private def sampleOf(
      sample: AscNativeSample,
      eye: AscRecordedEye,
      frame: Frame[Unit2D.Px],
      diagnostics: scala.collection.mutable.Builder[EyeLinkAscSessionDiagnostic, Vector[
        EyeLinkAscSessionDiagnostic
      ]]
  ): Option[Sample[Unit2D.Px]] =
    val nativeEye = eye match
      case AscRecordedEye.Left  => sample.left
      case AscRecordedEye.Right => sample.right
    nativeEye match
      case None =>
        diagnostics += EyeLinkAscSessionDiagnostic.MissingEyeSample(
          sample.source.source,
          sample.source.number,
          sample.blockNumber,
          eye
        )
        None
      case Some(value) =>
        val time = AscTrackerTime.instant(sample.source, "sample-time", sample.timestamp) match
          case Left(error) =>
            diagnostics += EyeLinkAscSessionDiagnostic.Timeline(error)
            None
          case Right(instant) => Some(instant)
        val pupil = value.pupil.observation match
          case AscPupilObservation.MissingDot | AscPupilObservation.MissingZero => Some(None)
          case AscPupilObservation.Measured(nativeValue)                        =>
            val converted = nativeValue.toDouble
            if converted.isFinite && converted > 0.0 then Some(Some(converted))
            else
              diagnostics += EyeLinkAscSessionDiagnostic.PupilOutsideDoubleRange(
                sample.source.source,
                sample.source.number,
                sample.blockNumber,
                eye,
                AscDiagnosticText.bounded(nativeValue.toString)
              )
              None
        val gaze = value.pixelPosition(frame) match
          case Left(error) =>
            diagnostics += EyeLinkAscSessionDiagnostic.SampleMaterialization(
              sample.source.source,
              sample.source.number,
              sample.blockNumber,
              error
            )
            None
          case Right(AscPixelPosition.MissingNativeGaze)  => Some(Gaze.Lost[Unit2D.Px]())
          case Right(AscPixelPosition.InsideFrame(point)) =>
            pupil.map(value => Gaze.Tracked(point, value))
          case Right(AscPixelPosition.OffSurface(point)) =>
            value.pupil.observation match
              case AscPupilObservation.Measured(measured) if pupil.nonEmpty =>
                diagnostics += EyeLinkAscSessionDiagnostic.OffSurfacePupilExcluded(
                  sample.source.source,
                  sample.source.number,
                  sample.blockNumber,
                  eye,
                  measured
                )
              case AscPupilObservation.Measured(_) | AscPupilObservation.MissingDot |
                  AscPupilObservation.MissingZero =>
                ()
            Some(Gaze.OffScreen(point))
        for acceptedTime <- time; acceptedGaze <- gaze
        yield Sample(acceptedTime, acceptedGaze)

  private def validateSource(framing: Vector[AscFramingEmission]): SourceValidation =
    val parsed   = framing.collect { case AscFramingEmission.Parsed(value) => value }
    val rejected = framing.exists {
      case AscFramingEmission.Rejected(_) => true
      case AscFramingEmission.Parsed(_)   => false
    }
    if rejected then SourceValidation(None, Vector.empty)
    else
      val diagnostics  = Vector.newBuilder[EyeLinkAscSessionDiagnostic]
      val bytes        = Vector.newBuilder[Byte]
      val expectedName = parsed.headOption.map(_.source.source)
      var expectedLine = 1L
      var expectedByte = 0L
      parsed.zipWithIndex.foreach { case (line, index) =>
        expectedName.foreach { source =>
          if line.source.source != source then
            diagnostics += EyeLinkAscSessionDiagnostic.SourceChanged(
              source,
              line.source.source,
              line.source.number
            )
        }
        if line.source.number != expectedLine || line.source.byteOffset != expectedByte then
          diagnostics += EyeLinkAscSessionDiagnostic.SourceSequenceMismatch(
            line.source.source,
            expectedLine,
            line.source.number,
            expectedByte,
            line.source.byteOffset
          )
        bytes ++= line.source.bytes.iterator
        val terminatorLength = line.source.terminator match
          case AscLineTerminator.LineFeed =>
            bytes += '\n'.toByte
            1L
          case AscLineTerminator.CarriageReturnLineFeed =>
            bytes += '\r'.toByte
            bytes += '\n'.toByte
            2L
          case AscLineTerminator.EndOfFile =>
            if index != parsed.length - 1 then
              diagnostics += EyeLinkAscSessionDiagnostic.TerminatorBeforeFinalLine(
                line.source.source,
                line.source.number
              )
            0L
        expectedLine += 1L
        expectedByte += line.source.bytes.length.toLong + terminatorLength
      }
      val failures = diagnostics.result()
      if failures.nonEmpty then SourceValidation(None, failures)
      else SourceValidation(Some(Sha256.ofBytes(IArray.from(bytes.result()))), Vector.empty)

end EyeLinkAscSessions

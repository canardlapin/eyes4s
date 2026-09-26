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

import eyes4s.plan.*

/** The code table of io's error families: fixation and result CSV import and
  * export, delimited sample schemas, the psychology workflow, EyeLink ASC
  * import and conversion evidence, and the EyeLink oracle, conformance and
  * corpus manifests. Codes are unique across this table, [[DiagnosticCatalog]]
  * and `CodecDiagnosticCatalog`, and codes are only ever appended.
  */
object IoDiagnosticCatalog:
  import DiagnosticFamily.error

  val fixationImport: DiagnosticFamily = error("fixation-import")(
    "Csv",
    "Columns",
    "Header",
    "Incomplete",
    "ParticipantScope",
    "Inventory",
    "NoItemColumn"
  )
  val fixationRow: DiagnosticFamily = error("fixation-row")(
    "Width",
    "Key",
    "Number",
    "Time",
    "Position",
    "Event",
    "Trial"
  )
  val tidyCsv: DiagnosticFamily = error("tidy-csv")(
    "MissingHeader",
    "UnexpectedHeader",
    "WrongColumnCount",
    "UnexpectedSchema",
    "MissingRequiredContext",
    "InvalidValueCells",
    "InvalidScientificField",
    "MalformedCsv",
    "UnterminatedQuotedField"
  )
  val tidyResult: DiagnosticFamily = error("tidy-result")(
    "BlankParticipant",
    "BlankTrial",
    "BlankConditionKey",
    "BlankConditionValue",
    "DuplicateConditionKey",
    "NoValidatedRecording",
    "FrameConflict",
    "ClockConflict",
    "AnalysisRecordingMismatch",
    "MissingSynchronization",
    "SynchronizationMismatch",
    "CustomDetectorCannotProduceScientificExport",
    "TemporalSupportMismatch",
    "MissingDetectorProvenance",
    "BlankDetectorParameter",
    "DuplicateDetectorParameter",
    "BlankWarning",
    "BlankOperation",
    "InvalidOperationParameters"
  )
  val contrastExport: DiagnosticFamily = error("contrast-export")(
    "Codec",
    "PlanMismatch",
    "Components",
    "Values"
  )
  val resultExport: DiagnosticFamily = error("result-export")(
    "Schema",
    "Cell",
    "Width",
    "Codec",
    "Score",
    "Context"
  )
  val templateCsv: DiagnosticFamily = error("template-csv")(
    "Csv",
    "Header",
    "Row",
    "Fit"
  )
  val delimitedSchema: DiagnosticFamily = error("delimited-schema")(
    "NoTrackedValidityToken",
    "BlankValidityToken",
    "AmbiguousValidityToken",
    "BlankColumnName",
    "DuplicateLogicalColumn",
    "EmptySuppliedHeader",
    "BlankSuppliedHeader",
    "DuplicateSuppliedHeader",
    "UnknownMissingTokenColumn",
    "MissingValidityOverlap"
  )
  val psychologyWorkflow: DiagnosticFamily = error("psychology-workflow")(
    "AnalysisFailed",
    "BlankSourceName",
    "InvalidStudy",
    "MillisecondsOutsideRange",
    "BlankClock",
    "InvalidDisplay",
    "InvalidViewing",
    "InvalidInterpolation",
    "InvalidVelocity",
    "InvalidDetectorConfiguration",
    "InvalidSyncMark",
    "InvalidSynchronization",
    "BlankAoiId",
    "BlankAoiLabel",
    "NonFiniteAoiBounds",
    "DegenerateAoiBounds",
    "NoAreas",
    "DuplicateArea",
    "AreaOutsideDisplay",
    "InvalidSourceMetadata",
    "ReservedSourceMetadata",
    "DuplicateSourceMetadata",
    "SchemaFailed",
    "ImportFailed",
    "SynchronizedRecordingFailed",
    "AngularFrameFailed",
    "WarpFailed",
    "PreprocessingCardinality",
    "PreprocessedRecordingFailed",
    "DetectionFailed",
    "AreaWarpUndefined",
    "AreaRegionFailed",
    "AreaConstructionFailed",
    "AssignmentFailed",
    "TidyResultFailed",
    "ExportFailed",
    "ExportRoundTripMismatch"
  )
  val sha256: DiagnosticFamily = error("sha256")(
    "WrongLength",
    "InvalidCharacter"
  )
  val edf2AscProvenance: DiagnosticFamily = error("edf2asc-provenance")(
    "BlankConverterName",
    "BlankConverterVersion",
    "BlankConverterArgument",
    "InvalidConverterArgument",
    "BlankPlatform",
    "UnsuccessfulConversion",
    "MissingEdfDigest",
    "MissingConversionReceipt"
  )
  val ascSampleMaterialization: DiagnosticFamily = error("asc-sample-materialization")(
    "CoordinateModeIsNotScreenPixels",
    "PixelValueOutsideDoubleRange"
  )
  val ascSourceLine: DiagnosticFamily = error("asc-source-line")(
    "BlankSource",
    "NonPositiveLineNumber",
    "NegativeByteOffset",
    "NonPositiveLineLimit",
    "LineTooLong",
    "EmbeddedLineTerminator"
  )
  val ascStreamConfiguration: DiagnosticFamily = error("asc-stream-configuration")(
    "BlankSource",
    "InvalidLineLimit",
    "NonPositiveReadChunk"
  )
  val ascNativeTimeline: DiagnosticFamily = error("asc-native-timeline")(
    "FractionalMicrosecond",
    "InstantOutsideLongRange",
    "InvalidTimeline"
  )
  val ascPerformanceValidation: DiagnosticFamily = error("asc-performance-validation")(
    "Blank",
    "NonPositive",
    "Negative",
    "AllocationMeasurementUnavailable"
  )
  val eyeLinkSessionConfig: DiagnosticFamily = error("eyelink-session-config")(
    "BlankFrame",
    "BlankClock"
  )
  val eyeLinkOracle: DiagnosticFamily = error("eyelink-oracle")(
    "InvalidPreamble",
    "InvalidHeader",
    "WrongFieldCount",
    "InvalidEscape",
    "InvalidValue",
    "InvalidDigest",
    "InvalidDescriptor",
    "InvalidFact",
    "EmptyManifest",
    "NonContiguousRecords",
    "NonContiguousFields",
    "InconsistentRecordMetadata",
    "DuplicateFieldPath",
    "OrderingConflict",
    "MissingOrderingDisclosure"
  )
  val eyeLinkConformance: DiagnosticFamily = error("eyelink-conformance")(
    "InvalidOperand",
    "InvalidAbsentValue",
    "InvalidFieldPath",
    "DuplicateField",
    "EmptyManifest",
    "InvalidTolerance",
    "InvalidSummaryRow",
    "DuplicateSummaryFixture",
    "EmptySummary"
  )
  val eyeLinkCorpus: DiagnosticFamily = error("eyelink-corpus")(
    "InvalidPreamble",
    "InvalidHeader",
    "WrongFieldCount",
    "InvalidEscape",
    "InvalidValue",
    "InvalidDigest",
    "PartialConverterEvidence",
    "InvalidConverterEvidence",
    "InvalidFixture",
    "UnsafeLocalPath",
    "DuplicateFixtureId",
    "DuplicateLocalPath",
    "EmptyManifest"
  )

  /** Every family, in the order documented. */
  val families: Vector[DiagnosticFamily] = Vector(
    fixationImport,
    fixationRow,
    tidyCsv,
    tidyResult,
    contrastExport,
    resultExport,
    templateCsv,
    delimitedSchema,
    psychologyWorkflow,
    sha256,
    edf2AscProvenance,
    ascSampleMaterialization,
    ascSourceLine,
    ascStreamConfiguration,
    ascNativeTimeline,
    ascPerformanceValidation,
    eyeLinkSessionConfig,
    eyeLinkOracle,
    eyeLinkConformance,
    eyeLinkCorpus
  )

  /** Every stable code, in catalog order. */
  val codes: Vector[DiagnosticCode] = families.flatMap(_.codes)

/** io's [[Diagnose]] instances. Import `IoDiagnostics.given` for
  * `Diagnostic.of` over io's families. Each field becomes a typed operand
  * under its own name, and a wrapped error keeps its subject. A case that
  * names a text source and a line has the subject `Locus.Line`; a tidy CSV
  * error names its logical record (the header is record 1) as `Locus.Record`;
  * and a fixation import refused for rejected rows names them as
  * `Locus.Records`. Export errors that wrap a `CodecError` carry its trial
  * keys as [[ErasedKey]]s.
  */
object IoDiagnostics:
  import IoDiagnosticCatalog as C
  import eyes4s.codec.CodecDiagnostics.given

  /** A SHA-256 digest is an artifact identity, rendered in lower-case hex. */
  private given DiagnosticOperand[Sha256, Nothing] =
    DiagnosticOperand.of(digest => Operand.Artifact(digest.hex))

  given fixationImport: Diagnose[FixationImportError, Nothing] =
    Diagnose.derived[FixationImportError, Nothing](C.fixationImport, rejectedRecords)(_.message)
  given fixationRow: Diagnose[FixationRowError, Nothing] =
    Diagnose.derived[FixationRowError, Nothing](C.fixationRow)(_.message)
  given tidyCsv: Diagnose[TidyCsvError, Nothing] =
    Diagnose.derived[TidyCsvError, Nothing](C.tidyCsv, csvRecord)(_.message)
  given tidyResult: Diagnose[TidyResultError, Nothing] =
    Diagnose.derived[TidyResultError, Nothing](C.tidyResult)(_.message)
  given contrastExport: Diagnose[ContrastExportError, ErasedKey] =
    Diagnose.derived[ContrastExportError, ErasedKey](C.contrastExport)(_.message)
  given resultExport: Diagnose[ResultExportError, ErasedKey] =
    Diagnose.derived[ResultExportError, ErasedKey](C.resultExport)(_.message)
  given templateCsv: Diagnose[TemplateCsvError, Nothing] =
    Diagnose.derived[TemplateCsvError, Nothing](C.templateCsv)(_.message)
  given delimitedSchema: Diagnose[DelimitedSchemaError, Nothing] =
    Diagnose.derived[DelimitedSchemaError, Nothing](C.delimitedSchema)(_.message)
  given psychologyWorkflow: Diagnose[PsychologyWorkflowError, Nothing] =
    Diagnose.derived[PsychologyWorkflowError, Nothing](C.psychologyWorkflow)(_.message)
  given sha256: Diagnose[Sha256Error, Nothing] =
    Diagnose.derived[Sha256Error, Nothing](C.sha256)(_.message)
  given edf2AscProvenance: Diagnose[Edf2AscProvenanceError, Nothing] =
    Diagnose.derived[Edf2AscProvenanceError, Nothing](C.edf2AscProvenance)(_.message)
  given ascSampleMaterialization: Diagnose[AscSampleMaterializationError, Nothing] =
    Diagnose.derived[AscSampleMaterializationError, Nothing](C.ascSampleMaterialization)(
      _.message
    )
  given ascSourceLine: Diagnose[AscSourceLineError, Nothing] =
    Diagnose.derived[AscSourceLineError, Nothing](
      C.ascSourceLine,
      ascLine
    )(_.message)
  given ascStreamConfiguration: Diagnose[AscStreamConfigurationError, Nothing] =
    Diagnose.derived[AscStreamConfigurationError, Nothing](C.ascStreamConfiguration)(_.message)
  given ascNativeTimeline: Diagnose[AscNativeTimelineError, Nothing] =
    Diagnose.derived[AscNativeTimelineError, Nothing](
      C.ascNativeTimeline,
      timelineLine
    )(_.message)
  given ascPerformanceValidation: Diagnose[AscPerformanceValidationError, Nothing] =
    Diagnose.derived[AscPerformanceValidationError, Nothing](C.ascPerformanceValidation)(
      _.message
    )
  given eyeLinkSessionConfig: Diagnose[EyeLinkAscSessionConfigError, Nothing] =
    Diagnose.derived[EyeLinkAscSessionConfigError, Nothing](C.eyeLinkSessionConfig)(_.message)
  given eyeLinkOracle: Diagnose[EyeLinkOracleError, Nothing] =
    Diagnose.derived[EyeLinkOracleError, Nothing](
      C.eyeLinkOracle,
      oracleLine
    )(_.message)
  given eyeLinkConformance: Diagnose[EyeLinkConformanceError, Nothing] =
    Diagnose.derived[EyeLinkConformanceError, Nothing](C.eyeLinkConformance)(_.message)
  given eyeLinkCorpus: Diagnose[EyeLinkCorpusError, Nothing] =
    Diagnose.derived[EyeLinkCorpusError, Nothing](
      C.eyeLinkCorpus,
      corpusLine
    )(_.message)

  private def line(source: String, number: Long): Vector[Locus[Nothing]] =
    Vector(Locus.Line(source, number))

  private def ascLine(error: AscSourceLineError): Vector[Locus[Nothing]] =
    import AscSourceLineError.*
    error match
      case LineTooLong(source, number, _, _)            => line(source, number)
      case EmbeddedLineTerminator(source, number, _, _) => line(source, number)
      // An invalid line number or offset names no line; the others no source.
      case BlankSource(_) | NonPositiveLineNumber(_, _) | NegativeByteOffset(_, _) |
          NonPositiveLineLimit(_) =>
        Vector.empty

  private def timelineLine(error: AscNativeTimelineError): Vector[Locus[Nothing]] =
    import AscNativeTimelineError.*
    error match
      case FractionalMicrosecond(source, number, _, _)   => line(source, number)
      case InstantOutsideLongRange(source, number, _, _) => line(source, number)
      case InvalidTimeline(_, _)                         => Vector.empty

  private def oracleLine(error: EyeLinkOracleError): Vector[Locus[Nothing]] =
    import EyeLinkOracleError.*
    error match
      case InvalidHeader(source, number, _, _)    => line(source, number)
      case WrongFieldCount(source, number, _, _)  => line(source, number)
      case InvalidEscape(source, number, _, _, _) => line(source, number)
      case InvalidValue(source, number, _, _, _)  => line(source, number)
      case InvalidDigest(source, number, _, _)    => line(source, number)
      case InvalidPreamble(_, _) | InvalidDescriptor(_, _, _, _) | InvalidFact(_, _, _, _, _) |
          EmptyManifest(_) | NonContiguousRecords(_, _, _) | NonContiguousFields(_, _, _, _) |
          InconsistentRecordMetadata(_, _, _, _) | DuplicateFieldPath(_, _, _) |
          OrderingConflict(_, _, _) | MissingOrderingDisclosure(_, _) =>
        Vector.empty

  private def corpusLine(error: EyeLinkCorpusError): Vector[Locus[Nothing]] =
    import EyeLinkCorpusError.*
    error match
      case InvalidHeader(source, number, _, _)         => line(source, number)
      case WrongFieldCount(source, number, _, _)       => line(source, number)
      case InvalidEscape(source, number, _, _, _)      => line(source, number)
      case InvalidValue(source, number, _, _, _)       => line(source, number)
      case InvalidDigest(source, number, _, _)         => line(source, number)
      case PartialConverterEvidence(source, number)    => line(source, number)
      case InvalidConverterEvidence(source, number, _) => line(source, number)
      case InvalidFixture(source, number, _, _)        => line(source, number)
      case UnsafeLocalPath(source, number, _, _)       => line(source, number)
      case DuplicateFixtureId(source, _, numbers)      => numbers.flatMap(line(source, _))
      case InvalidPreamble(_, _) | DuplicateLocalPath(_, _, _) | EmptyManifest(_) =>
        Vector.empty

  /** The logical CSV record (the header is record 1) a tidy CSV error names. */
  private def csvRecord(error: TidyCsvError): Vector[Locus[Nothing]] =
    import TidyCsvError.*
    error match
      case WrongColumnCount(record, _, _)          => Vector(Locus.Record(record))
      case UnexpectedSchema(record, _, _)          => Vector(Locus.Record(record))
      case MissingRequiredContext(record, _)       => Vector(Locus.Record(record))
      case InvalidValueCells(record, _, _, _)      => Vector(Locus.Record(record))
      case InvalidScientificField(record, _, _, _) => Vector(Locus.Record(record))
      case MissingHeader | UnexpectedHeader(_, _) | MalformedCsv(_, _) |
          UnterminatedQuotedField(_) =>
        Vector.empty

  private def rejectedRecords(error: FixationImportError): Vector[Locus[Nothing]] =
    error match
      case FixationImportError.Incomplete(rows) => Vector(Locus.Records(rows))
      case _                                    => Vector.empty

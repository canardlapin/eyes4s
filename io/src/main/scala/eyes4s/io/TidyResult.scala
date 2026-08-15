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

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*

import scala.collection.mutable.ArrayBuffer

/** Participant, trial, and condition identity for one psychology result. */
final class StudyTrial private (
    val participant: String,
    val trial: String,
    val conditions: Vector[(String, String)]
) derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case value: StudyTrial =>
      participant == value.participant && trial == value.trial && conditions == value.conditions
    case _ => false

  override def hashCode: Int =
    (participant.hashCode * 31 + trial.hashCode) * 31 + conditions.hashCode

object StudyTrial:
  def of(
      participant: String,
      trial: String,
      conditions: Vector[(String, String)] = Vector.empty
  ): Either[TidyResultError, StudyTrial] =
    val cleanParticipant = participant.trim
    val cleanTrial       = trial.trim
    if cleanParticipant.isEmpty then Left(TidyResultError.BlankParticipant(participant))
    else if cleanTrial.isEmpty then Left(TidyResultError.BlankTrial(cleanParticipant, trial))
    else
      conditions.zipWithIndex.collectFirst {
        case ((key, _), index) if key.trim.isEmpty =>
          TidyResultError.BlankConditionKey(cleanParticipant, cleanTrial, index, key)
        case ((key, value), index) if value.trim.isEmpty =>
          TidyResultError.BlankConditionValue(cleanParticipant, cleanTrial, index, key, value)
        case ((key, _), second) if conditions.take(second).exists(_._1.trim == key.trim) =>
          TidyResultError.DuplicateConditionKey(
            cleanParticipant,
            cleanTrial,
            key.trim,
            conditions.indexWhere(_._1.trim == key.trim),
            second
          )
      } match
        case Some(error) => Left(error)
        case None        =>
          Right(
            new StudyTrial(
              cleanParticipant,
              cleanTrial,
              conditions.map((key, value) => key.trim -> value.trim)
            )
          )

/** Complete coordinate evidence repeated by every exported result row. */
final case class ResultFrameSpec(
    id: FrameId,
    specification: FrameSpec,
    unitSymbol: String,
    unitName: String
) derives CanEqual

/** Native source-clock metadata and the clock on which analysis was performed. */
final case class ResultClockSpec(
    source: DelimitedClockSpec,
    analysis: ClockId,
    synchronization: Option[SyncEvidence]
)

/** Overlapping counts of each derivation step in the analysed trajectory.
  *
  * `sampleCount` is the denominator. The four lineage-step counts may overlap:
  * a measured sample that is smoothed and projected contributes to all three
  * corresponding fields. `derived` counts samples with any derived value.
  */
final case class SampleOriginCounts private[io] (
    sampleCount: Int,
    measured: Int,
    interpolated: Int,
    smoothed: Int,
    projected: Int,
    derived: Int
) derives CanEqual

object SampleOriginCounts:
  def from[U <: Unit2D](recording: Recording[U]): SampleOriginCounts =
    val lineages = recording.samples.iterator.map(_.lineage).toVector
    SampleOriginCounts(
      lineages.length,
      lineages.count(_.contains(SampleOrigin.Measured)),
      lineages.count(_.contains(SampleOrigin.Interpolated)),
      lineages.count(_.contains(SampleOrigin.Smoothed)),
      lineages.count(_.contains(SampleOrigin.Projected)),
      lineages.count(_.toVector != Vector(SampleOrigin.Measured))
    )

/** Quantity represented by one tidy row. */
enum AoiEstimand(val label: String) derives CanEqual:
  case Dwell             extends AoiEstimand("dwell")
  case DwellProportion   extends AoiEstimand("dwell_proportion")
  case FirstEntryLatency extends AoiEstimand("first_entry_latency")
  case RunCount          extends AoiEstimand("run_count")
  case TransitionCount   extends AoiEstimand("transition_count")

/** Unit of the value cell, including missing values whose intended unit remains known. */
enum ResultUnit(val label: String) derives CanEqual:
  case Microseconds extends ResultUnit("microseconds")
  case Proportion   extends ResultUnit("proportion")
  case Count        extends ResultUnit("count")

/** A value is present with its scalar kind or absent for an explicit reason. */
enum TidyValue derives CanEqual:
  case Duration(value: Span)
  case Proportion(value: Double)
  case Count(value: Int)
  case Missing(reason: String)

/** The spatial subject of an AOI result row. */
enum TidyTarget derives CanEqual:
  case Area(id: AoiId, label: String)
  case Transition(from: AoiId, fromLabel: String, to: AoiId, toLabel: String)

/** Evidence that every row carries, rather than looking up from an external side table. */
final class TidyEvidence private[io] (
    val frame: ResultFrameSpec,
    val clock: ResultClockSpec,
    val detectorCard: AlgorithmCard,
    val detectorConfiguration: Vector[(String, Provenance.Param)],
    val excludedTime: Span,
    val unclassifiedTime: Span,
    val sampleOrigins: SampleOriginCounts,
    val warnings: Vector[String],
    val provenance: Provenance,
    val inputIdentity: Sha256
)

/** One tidy observation with all scientific interpretation retained. */
final class TidyAoiRow private[io] (
    val study: StudyTrial,
    val target: TidyTarget,
    val estimand: AoiEstimand,
    val value: TidyValue,
    val unit: ResultUnit,
    val evidence: TidyEvidence
)

/** A kernel-free report suitable for an application summary panel. */
final case class PsychologyResultReport(
    participant: String,
    trial: String,
    conditions: Vector[(String, String)],
    detector: String,
    detectorVersion: String,
    resultRows: Int,
    areaRows: Int,
    transitionRows: Int,
    excludedMilliseconds: Double,
    unclassifiedMilliseconds: Double,
    measuredSamples: Int,
    derivedSamples: Int,
    warnings: Vector[String],
    inputSha256: String
) derives CanEqual

/** Typed AOI measurements ready for inspection or deterministic export. */
final class TidyAoiResult private (
    val study: StudyTrial,
    val rows: Vector[TidyAoiRow],
    val evidence: TidyEvidence
):
  def report: PsychologyResultReport =
    PsychologyResultReport(
      study.participant,
      study.trial,
      study.conditions,
      evidence.detectorCard.name,
      evidence.detectorCard.version.render,
      rows.length,
      rows.count(_.estimand != AoiEstimand.TransitionCount),
      rows.count(_.estimand == AoiEstimand.TransitionCount),
      evidence.excludedTime.toMillis,
      evidence.unclassifiedTime.toMillis,
      evidence.sampleOrigins.measured,
      evidence.sampleOrigins.derived,
      evidence.warnings,
      evidence.inputIdentity.hex
    )

  def explain: String =
    val summary       = report
    val conditionText =
      if summary.conditions.isEmpty then "no condition labels"
      else summary.conditions.map((key, value) => s"$key=$value").mkString(", ")
    val warningText =
      if summary.warnings.isEmpty then "No warnings were recorded."
      else s"Warnings: ${summary.warnings.mkString(" | ")}"
    s"Participant '${summary.participant}', trial '${summary.trial}' ($conditionText) produced " +
      s"${summary.resultRows} AOI result rows with ${summary.detector} " +
      s"${summary.detectorVersion}. Excluded time was ${summary.excludedMilliseconds} ms and " +
      s"unclassified time was ${summary.unclassifiedMilliseconds} ms. $warningText " +
      s"Input SHA-256: ${summary.inputSha256}."

object TidyAoiResult:
  def from[Native <: Unit2D, U <: Unit2D: UnitLabel](
      study: StudyTrial,
      imported: DelimitedImport[Native],
      detection: DetectionResult[U],
      assignment: AoiAssignment[U],
      synchronization: Option[SyncEvidence] = None,
      upstreamOperations: Vector[Provenance.Step] = Vector.empty,
      extraWarnings: Vector[String] = Vector.empty
  ): Either[TidyResultError, TidyAoiResult] =
    for
      _ <- imported.recording.toRight(TidyResultError.NoValidatedRecording(imported.raw.source))
      _ <- Agreement
        .frames(assignment.recording.frame, detection.eventSeries.frame)
        .left
        .map(TidyResultError.FrameConflict(imported.raw.source, _))
      _ <- Agreement
        .clocks(assignment.recording.clock, detection.eventSeries.clock)
        .left
        .map(TidyResultError.ClockConflict(imported.raw.source, _))
      _ <- Either.cond(
        assignment.recording.contentHash == detection.eventSeries.recording.contentHash,
        (),
        TidyResultError.AnalysisRecordingMismatch(
          imported.raw.source,
          assignment.recording.contentHash,
          detection.eventSeries.recording.contentHash
        )
      )
      _ <- validateSynchronization(
        imported.raw.source,
        imported.clock,
        detection.eventSeries.clock,
        synchronization
      )
      detectorCard <- detection.identity.algorithmCard.toRight(
        TidyResultError.CustomDetectorCannotProduceScientificExport(
          imported.raw.source,
          detection.report.detector
        )
      )
      _ <- Either.cond(
        detection.report.temporalSupport == assignment.support.policy,
        (),
        TidyResultError.TemporalSupportMismatch(
          imported.raw.source,
          detection.report.temporalSupport,
          assignment.support.policy
        )
      )
      detectorStep <- detection.provenance.steps.lastOption
        .filter(_.operation == "detect")
        .toRight(
          TidyResultError.MissingDetectorProvenance(
            imported.raw.source,
            detection.report.detector
          )
        )
      _ <- validateParameters(imported.raw.source, detectorStep.params)
      _ <- validateOperations(imported.raw.source, upstreamOperations)
      _ <- validateWarnings(imported.raw.source, extraWarnings)
      rowsAndEvidence = build(
        study,
        imported,
        detection,
        detectorCard,
        detectorStep.params,
        assignment.measure,
        assignment.recording,
        synchronization,
        upstreamOperations,
        extraWarnings
      )
    yield rowsAndEvidence

  private def validateParameters(
      source: String,
      parameters: Vector[(String, Provenance.Param)]
  ): Either[TidyResultError, Unit] =
    parameters.zipWithIndex
      .collectFirst {
        case ((name, _), index) if name.trim.isEmpty =>
          TidyResultError.BlankDetectorParameter(source, index, name)
        case ((name, _), second) if parameters.take(second).exists(_._1.trim == name.trim) =>
          TidyResultError.DuplicateDetectorParameter(
            source,
            name.trim,
            parameters.indexWhere(_._1.trim == name.trim),
            second
          )
      }
      .toLeft(())

  private def validateWarnings(
      source: String,
      warnings: Vector[String]
  ): Either[TidyResultError, Unit] =
    warnings.zipWithIndex
      .collectFirst {
        case (warning, index) if warning.trim.isEmpty =>
          TidyResultError.BlankWarning(source, index, warning)
      }
      .toLeft(())

  private def validateOperations(
      source: String,
      operations: Vector[Provenance.Step]
  ): Either[TidyResultError, Unit] =
    operations.zipWithIndex.foldLeft[Either[TidyResultError, Unit]](Right(())) {
      case (left @ Left(_), _)            => left
      case (Right(_), (operation, index)) =>
        if operation.operation.trim.isEmpty then
          Left(TidyResultError.BlankOperation(source, index, operation.operation))
        else
          validateParameters(source, operation.params).left.map(
            TidyResultError.InvalidOperationParameters(source, index, operation.operation, _)
          )
    }

  private def validateSynchronization(
      source: String,
      nativeClock: ClockId,
      analysisClock: ClockId,
      synchronization: Option[SyncEvidence]
  ): Either[TidyResultError, Unit] =
    synchronization match
      case Some(evidence)
          if evidence.source != nativeClock || evidence.target != analysisClock =>
        Left(
          TidyResultError.SynchronizationMismatch(
            source,
            nativeClock,
            analysisClock,
            evidence.source,
            evidence.target
          )
        )
      case Some(_) => Right(())
      case None    =>
        Agreement
          .clocks(nativeClock, analysisClock)
          .map(_ => ())
          .left
          .map(_ => TidyResultError.MissingSynchronization(source, nativeClock, analysisClock))

  private def build[Native <: Unit2D, U <: Unit2D: UnitLabel](
      study: StudyTrial,
      imported: DelimitedImport[Native],
      detection: DetectionResult[U],
      detectorCard: AlgorithmCard,
      detectorConfiguration: Vector[(String, Provenance.Param)],
      measurements: AoiMeasurements,
      recording: Recording[U],
      synchronization: Option[SyncEvidence],
      upstreamOperations: Vector[Provenance.Step],
      extraWarnings: Vector[String]
  ): TidyAoiResult =
    val unit  = summon[UnitLabel[U]]
    val frame = ResultFrameSpec(
      detection.eventSeries.frame.id,
      detection.eventSeries.frame.spec,
      unit.symbol,
      unit.name
    )
    val unclassified = detection.report.classDurations
      .collectFirst { case (SampleClass.Unclassified, duration) =>
        duration
      }
      .getOrElse(Span.zero)
    val warnings = (
      imported.diagnostics.map(_.message) ++
        detection.report.warnings.map(_.message) ++
        extraWarnings.map(_.trim)
    ).distinct
    val provenance = Provenance(
      detection.provenance.inputs,
      upstreamOperations ++ detection.provenance.steps
    ).andThen(
      Provenance.Step(
        "aoi-measure",
        Vector(
          "policy"          -> Provenance.Param.Text(renderPolicy(measurements.policy)),
          "areaCount"       -> Provenance.Param.Num(measurements.areas.length.toDouble),
          "transitionCount" -> Provenance.Param.Num(measurements.transitions.length.toDouble)
        )
      )
    )
    val evidence = new TidyEvidence(
      frame,
      ResultClockSpec(imported.clockSpec, detection.eventSeries.clock, synchronization),
      detectorCard,
      detectorConfiguration,
      measurements.report.excludedTime + measurements.report.policyCensoredTime,
      unclassified,
      SampleOriginCounts.from(recording),
      warnings,
      provenance,
      imported.sourceDigest
    )
    val labels   = measurements.areas.map(area => area.id -> area.label).toMap
    val areaRows = measurements.areas.flatMap { area =>
      Vector(
        row(
          study,
          TidyTarget.Area(area.id, area.label),
          AoiEstimand.Dwell,
          TidyValue.Duration(area.dwell),
          ResultUnit.Microseconds,
          evidence
        ),
        row(
          study,
          TidyTarget.Area(area.id, area.label),
          AoiEstimand.DwellProportion,
          area.dwellProportion.fold[TidyValue](
            TidyValue.Missing("no analysable time was available")
          )(TidyValue.Proportion.apply),
          ResultUnit.Proportion,
          evidence
        ),
        row(
          study,
          TidyTarget.Area(area.id, area.label),
          AoiEstimand.FirstEntryLatency,
          area.firstEntryLatency
            .fold[TidyValue](TidyValue.Missing("the AOI was never entered"))(
              TidyValue.Duration.apply
            ),
          ResultUnit.Microseconds,
          evidence
        ),
        row(
          study,
          TidyTarget.Area(area.id, area.label),
          AoiEstimand.RunCount,
          TidyValue.Count(area.runCount),
          ResultUnit.Count,
          evidence
        )
      )
    }
    val transitionRows = measurements.transitions.map { transition =>
      row(
        study,
        TidyTarget.Transition(
          transition.from,
          labels.getOrElse(transition.from, transition.from.value),
          transition.to,
          labels.getOrElse(transition.to, transition.to.value)
        ),
        AoiEstimand.TransitionCount,
        TidyValue.Count(transition.count),
        ResultUnit.Count,
        evidence
      )
    }
    new TidyAoiResult(study, areaRows ++ transitionRows, evidence)

  private def row(
      study: StudyTrial,
      target: TidyTarget,
      estimand: AoiEstimand,
      value: TidyValue,
      unit: ResultUnit,
      evidence: TidyEvidence
  ): TidyAoiRow = new TidyAoiRow(study, target, estimand, value, unit, evidence)

  private def renderPolicy(policy: MembershipPolicy): String = policy match
    case MembershipPolicy.Multiple                       => "multiple"
    case MembershipPolicy.ExclusiveByPriority            => "exclusive-by-priority"
    case MembershipPolicy.SmallestContaining(resolution) =>
      s"smallest-containing(${resolution.nx}x${resolution.ny})"
    case MembershipPolicy.RejectOverlap => "reject-overlap"

/** Fixed-schema CSV view. Rows remain raw strings so decoding and re-encoding is lossless. */
final class TidyCsvDocument private[io] (
    val header: Vector[String],
    val rows: Vector[Vector[String]]
):
  def encode: String = Rfc4180.encode(header +: rows)

/** One scientifically validated tidy row with its scalar interpretation decoded. */
final class DecodedTidyCsvRow private[io] (
    val raw: Vector[String],
    val estimand: AoiEstimand,
    val unit: ResultUnit,
    val value: TidyValue
)

/** A fixed-schema document whose scientific identity and numeric fields were validated. */
final class DecodedTidyCsvDocument private[io] (
    val raw: TidyCsvDocument,
    val scientificRows: Vector[DecodedTidyCsvRow]
):
  def header: Vector[String]       = raw.header
  def rows: Vector[Vector[String]] = raw.rows
  def encode: String               = raw.encode

object TidyCsv:
  val schemaVersion: String = "eyes4s-tidy-aoi/2"

  val header: Vector[String] = Vector(
    "schema_version",
    "participant_id",
    "trial_id",
    "conditions",
    "aoi_id",
    "aoi_label",
    "transition_to_aoi_id",
    "transition_to_aoi_label",
    "estimand",
    "value_status",
    "value",
    "missing_reason",
    "unit",
    "frame_id",
    "frame_x_min_bits",
    "frame_y_min_bits",
    "frame_x_max_bits",
    "frame_y_max_bits",
    "frame_y_axis",
    "frame_unit_symbol",
    "frame_unit_name",
    "source_clock_id",
    "source_clock_native_unit",
    "source_clock_rounding",
    "analysis_clock_id",
    "synchronization",
    "detector_id",
    "detector_version",
    "detector_name",
    "detector_card",
    "detector_configuration",
    "excluded_microseconds",
    "unclassified_microseconds",
    "origin_sample_count",
    "origin_measured",
    "origin_interpolated",
    "origin_smoothed",
    "origin_projected",
    "origin_derived",
    "warnings",
    "operation_provenance",
    "input_sha256"
  )

  def encode(result: TidyAoiResult): String = document(result).encode

  def document(result: TidyAoiResult): TidyCsvDocument =
    new TidyCsvDocument(header, result.rows.map(exportRow))

  /** Parse RFC-4180 structure and the fixed schema without claiming scientific validity. */
  def parseRaw(contents: String): Either[TidyCsvError, TidyCsvDocument] =
    Rfc4180.decode(contents).flatMap { parsed =>
      if parsed.isEmpty then Left(TidyCsvError.MissingHeader)
      else
        val actualHeader = parsed.head
        val rows         = parsed.tail
        if actualHeader != header then Left(TidyCsvError.UnexpectedHeader(header, actualHeader))
        else
          rows.zipWithIndex.collectFirst {
            case (row, index) if row.length != header.length =>
              TidyCsvError.WrongColumnCount(index + 2, header.length, row.length)
            case (row, index) if row.headOption.getOrElse("") != schemaVersion =>
              TidyCsvError.UnexpectedSchema(
                index + 2,
                schemaVersion,
                row.headOption.getOrElse("")
              )
            case (row, index) if requiredContextColumn(row).nonEmpty =>
              TidyCsvError.MissingRequiredContext(index + 2, requiredContextColumn(row).get)
            case (row, index) if !validValueCells(row) =>
              TidyCsvError.InvalidValueCells(
                index + 2,
                cell(row, "value_status"),
                cell(row, "value"),
                cell(row, "missing_reason")
              )
          } match
            case Some(error) => Left(error)
            case None        => Right(new TidyCsvDocument(header, rows))
    }

  /** Decode and validate every scientific field claimed by schema version 2. */
  def decode(contents: String): Either[TidyCsvError, DecodedTidyCsvDocument] =
    parseRaw(contents).flatMap { document =>
      document.rows.zipWithIndex
        .foldLeft[Either[TidyCsvError, Vector[DecodedTidyCsvRow]]](Right(Vector.empty)) {
          case (left @ Left(_), _)            => left
          case (Right(decoded), (row, index)) =>
            validateScientificRow(index + 2, row).map(decoded :+ _)
        }
        .map(new DecodedTidyCsvDocument(document, _))
    }

  private def requiredContextColumn(row: Vector[String]): Option[String] =
    Vector(
      "participant_id",
      "trial_id",
      "estimand",
      "unit",
      "frame_id",
      "source_clock_id",
      "analysis_clock_id",
      "synchronization",
      "detector_id",
      "detector_version",
      "detector_card",
      "detector_configuration",
      "operation_provenance",
      "input_sha256"
    ).find(name => cell(row, name).isEmpty)

  private def validValueCells(row: Vector[String]): Boolean =
    cell(row, "value_status") match
      case "present" => cell(row, "value").nonEmpty && cell(row, "missing_reason").isEmpty
      case "missing" => cell(row, "value").isEmpty && cell(row, "missing_reason").nonEmpty
      case _         => false

  private def validateScientificRow(
      line: Int,
      row: Vector[String]
  ): Either[TidyCsvError, DecodedTidyCsvRow] =
    for
      _ <- validatePackedPairs(line, "conditions", cell(row, "conditions"), allowEmpty = true)
      estimand <- parseEstimand(line, cell(row, "estimand"))
      unit     <- parseUnit(line, cell(row, "unit"))
      value    <- parseValue(line, row, estimand, unit)
      _        <- validateTarget(line, row, estimand)
      _        <- validateFrame(line, row)
      _        <- validateClock(line, row)
      _        <- validateDetector(line, row)
      _        <- validateNonNegativeLong(line, row, "excluded_microseconds")
      _        <- validateNonNegativeLong(line, row, "unclassified_microseconds")
      _        <- validateOriginCounts(line, row)
      _ <- validatePackedStrings(line, "warnings", cell(row, "warnings"), allowBlank = false)
      _ <- validateProvenance(line, cell(row, "operation_provenance"))
      _ <- validateSha256(line, cell(row, "input_sha256"))
    yield new DecodedTidyCsvRow(row, estimand, unit, value)

  private def parseEstimand(line: Int, value: String): Either[TidyCsvError, AoiEstimand] =
    AoiEstimand.values
      .find(_.label == value)
      .toRight(
        invalid(line, "estimand", value, "unknown estimand")
      )

  private def parseUnit(line: Int, value: String): Either[TidyCsvError, ResultUnit] =
    ResultUnit.values
      .find(_.label == value)
      .toRight(
        invalid(line, "unit", value, "unknown result unit")
      )

  private def parseValue(
      line: Int,
      row: Vector[String],
      estimand: AoiEstimand,
      unit: ResultUnit
  ): Either[TidyCsvError, TidyValue] =
    val expectedUnit = estimand match
      case AoiEstimand.Dwell | AoiEstimand.FirstEntryLatency  => ResultUnit.Microseconds
      case AoiEstimand.DwellProportion                        => ResultUnit.Proportion
      case AoiEstimand.RunCount | AoiEstimand.TransitionCount => ResultUnit.Count
    if unit != expectedUnit then
      Left(
        invalid(
          line,
          "unit",
          unit.label,
          s"estimand='${estimand.label}' requires unit='${expectedUnit.label}'"
        )
      )
    else if cell(row, "value_status") == "missing" then
      Right(TidyValue.Missing(cell(row, "missing_reason")))
    else
      val raw = cell(row, "value")
      unit match
        case ResultUnit.Microseconds =>
          parseLong(raw)
            .filter(_ >= 0L)
            .map(value => TidyValue.Duration(Span.micros(value)))
            .toRight(
              invalid(line, "value", raw, "duration must be a non-negative integer")
            )
        case ResultUnit.Count =>
          raw.toIntOption
            .filter(_ >= 0)
            .map(value => TidyValue.Count(value))
            .toRight(invalid(line, "value", raw, "count must be a non-negative integer"))
        case ResultUnit.Proportion =>
          raw.toDoubleOption
            .filter(value => value.isFinite && value >= 0.0 && value <= 1.0)
            .map(value => TidyValue.Proportion(value))
            .toRight(invalid(line, "value", raw, "proportion must be finite and in [0,1]"))

  private def validateTarget(
      line: Int,
      row: Vector[String],
      estimand: AoiEstimand
  ): Either[TidyCsvError, Unit] =
    val area = cell(row, "aoi_id").trim
    val to   = cell(row, "transition_to_aoi_id").trim
    if area.isEmpty then Left(invalid(line, "aoi_id", area, "AOI id must be non-blank"))
    else if estimand == AoiEstimand.TransitionCount && to.isEmpty then
      Left(invalid(line, "transition_to_aoi_id", to, "transition rows require a target AOI"))
    else if estimand != AoiEstimand.TransitionCount && to.nonEmpty then
      Left(
        invalid(
          line,
          "transition_to_aoi_id",
          to,
          "non-transition rows cannot name a target AOI"
        )
      )
    else Right(())

  private def validateFrame(line: Int, row: Vector[String]): Either[TidyCsvError, Unit] =
    for
      xMin <- parseFiniteBits(line, "frame_x_min_bits", cell(row, "frame_x_min_bits"))
      yMin <- parseFiniteBits(line, "frame_y_min_bits", cell(row, "frame_y_min_bits"))
      xMax <- parseFiniteBits(line, "frame_x_max_bits", cell(row, "frame_x_max_bits"))
      yMax <- parseFiniteBits(line, "frame_y_max_bits", cell(row, "frame_y_max_bits"))
      _    <- Either.cond(
        xMax > xMin && yMax > yMin,
        (),
        invalid(
          line,
          "frame_x_min_bits..frame_y_max_bits",
          s"$xMin,$yMin,$xMax,$yMax",
          "frame bounds must be finite and increasing"
        )
      )
      axis = cell(row, "frame_y_axis")
      _ <- Either.cond(
        axis == YAxis.Down.toString.toLowerCase || axis == YAxis.Up.toString.toLowerCase,
        (),
        invalid(line, "frame_y_axis", axis, "unknown y-axis convention")
      )
      _ <- nonBlank(line, row, "frame_unit_symbol")
      _ <- nonBlank(line, row, "frame_unit_name")
    yield ()

  private def validateClock(line: Int, row: Vector[String]): Either[TidyCsvError, Unit] =
    val nativeUnit = cell(row, "source_clock_native_unit")
    val rounding   = cell(row, "source_clock_rounding")
    for
      _ <- Either.cond(
        TimestampUnit.values.exists(_.label == nativeUnit),
        (),
        invalid(line, "source_clock_native_unit", nativeUnit, "unknown timestamp unit")
      )
      _ <- Either.cond(
        TimestampRounding.values.exists(_.toString == rounding),
        (),
        invalid(line, "source_clock_rounding", rounding, "unknown timestamp rounding policy")
      )
      _ <- validateSynchronization(line, row)
    yield ()

  private def validateSynchronization(
      line: Int,
      row: Vector[String]
  ): Either[TidyCsvError, Unit] =
    val source   = cell(row, "source_clock_id")
    val analysis = cell(row, "analysis_clock_id")
    val raw      = cell(row, "synchronization")
    unpack(line, "synchronization", raw).flatMap {
      case Vector("identity", clock) =>
        Either.cond(
          source == analysis && clock == analysis,
          (),
          invalid(
            line,
            "synchronization",
            raw,
            "identity synchronization must name equal source and analysis clocks"
          )
        )
      case values if values.length == 11 =>
        val mode = values(0)
        for
          _ <- Either.cond(
            mode == SyncFitMode.OffsetOnly.render || mode == SyncFitMode.Affine.render,
            (),
            invalid(line, "synchronization", raw, s"unknown fit mode='$mode'")
          )
          _ <- Either.cond(
            values(1) == source && values(2) == analysis,
            (),
            invalid(line, "synchronization", raw, "embedded clock ids disagree with columns")
          )
          scale <- parseFiniteBits(line, "synchronization.scale", values(3))
          _     <- Either.cond(
            scale > 0.0,
            (),
            invalid(line, "synchronization.scale", values(3), "scale must be positive")
          )
          _    <- requireLong(line, "synchronization.offset", values(4))
          used <- unpackNested(line, "synchronization.used_marks", values(5), 3)
          required = if mode == SyncFitMode.OffsetOnly.render then 1 else 2
          _ <- Either.cond(
            used.length >= required,
            (),
            invalid(
              line,
              "synchronization.used_marks",
              values(5),
              s"mode='$mode' requires at least $required marks"
            )
          )
          _         <- validateDecodedMarks(line, used)
          residuals <- unpackNested(line, "synchronization.residuals", values(6), 3)
          _         <- Either.cond(
            residuals.map(_.head) == used.map(_.head),
            (),
            invalid(
              line,
              "synchronization.residuals",
              values(6),
              "residual ids must equal retained mark ids in order"
            )
          )
          errors <- residuals.foldLeft[Either[TidyCsvError, Vector[Long]]](
            Right(Vector.empty)
          ) {
            case (left @ Left(_), _)      => left
            case (Right(found), residual) =>
              for
                _     <- requireLong(line, "synchronization.predicted_target", residual(1))
                error <- requireLong(line, "synchronization.residual_error", residual(2))
              yield found :+ error
          }
          rejected <- unpackNested(line, "synchronization.rejected_marks", values(7), 5)
          _        <- rejected.foldLeft[Either[TidyCsvError, Unit]](Right(())) {
            case (left @ Left(_), _)      => left
            case (Right(_), rejectedMark) =>
              for
                _ <- nonBlankValue(line, "synchronization.rejected_mark_id", rejectedMark(0))
                _ <- requireLong(line, "synchronization.rejected_source", rejectedMark(1))
                _ <- requireLong(line, "synchronization.rejected_target", rejectedMark(2))
                _ <- requireNonNegativeLong(
                  line,
                  "synchronization.rejected_residual",
                  rejectedMark(3)
                )
                _ <- requireNonNegativeLong(
                  line,
                  "synchronization.rejected_limit",
                  rejectedMark(4)
                )
              yield ()
          }
          rms         <- requireNonNegativeLong(line, "synchronization.rms", values(8))
          maximum     <- requireNonNegativeLong(line, "synchronization.maximum", values(9))
          uncertainty <- requireNonNegativeLong(line, "synchronization.uncertainty", values(10))
          expectedMaximum = errors.map(absoluteMicros).maxOption.getOrElse(0L)
          expectedRms     = math.round(
            math.sqrt(errors.iterator.map(value => square(value.toDouble)).sum / errors.length)
          )
          _ <- Either.cond(
            maximum == expectedMaximum && rms == expectedRms && uncertainty >= maximum,
            (),
            invalid(
              line,
              "synchronization",
              raw,
              s"diagnostics disagree with residuals: rms=$rms/$expectedRms, max=$maximum/$expectedMaximum, uncertainty=$uncertainty"
            )
          )
        yield ()
      case _ =>
        Left(
          invalid(
            line,
            "synchronization",
            raw,
            "expected identity pair or 11-field fitted evidence"
          )
        )
    }

  private def validateDecodedMarks(
      line: Int,
      marks: Vector[Vector[String]]
  ): Either[TidyCsvError, Unit] =
    marks.zipWithIndex
      .foldLeft[Either[TidyCsvError, Vector[(String, Long, Long)]]](
        Right(Vector.empty)
      ) {
        case (left @ Left(_), _)              => left
        case (Right(previous), (mark, index)) =>
          for
            _      <- nonBlankValue(line, "synchronization.mark_id", mark(0))
            source <- requireLong(line, "synchronization.mark_source", mark(1))
            target <- requireLong(line, "synchronization.mark_target", mark(2))
            _      <- Either.cond(
              !previous.exists(_._1 == mark(0)),
              (),
              invalid(line, "synchronization.mark_id", mark(0), s"duplicate at index=$index")
            )
            _ <- previous.lastOption.fold[Either[TidyCsvError, Unit]](Right(())) {
              case (_, previousSource, previousTarget) =>
                Either.cond(
                  source > previousSource && target > previousTarget,
                  (),
                  invalid(
                    line,
                    "synchronization.used_marks",
                    mark.mkString(","),
                    s"mark[$index] is not increasing on both clocks"
                  )
                )
            }
          yield previous :+ ((mark(0), source, target))
      }
      .map(_ => ())

  private def validateDetector(line: Int, row: Vector[String]): Either[TidyCsvError, Unit] =
    val id      = cell(row, "detector_id")
    val version = cell(row, "detector_version")
    val name    = cell(row, "detector_name")
    val rawCard = cell(row, "detector_card")
    for
      _    <- validateVersion(line, version)
      card <- unpack(line, "detector_card", rawCard)
      _    <- Either.cond(
        card.length == 8,
        (),
        invalid(line, "detector_card", rawCard, "algorithm card must contain 8 fields")
      )
      _ <- Either.cond(
        card(0) == id && card(1) == version && card(2) == name,
        (),
        invalid(line, "detector_card", rawCard, "card identity disagrees with detector columns")
      )
      citations <- unpackNested(line, "detector_card.citations", card(3), minimumFields = 4)
      _         <- Either.cond(
        citations.nonEmpty,
        (),
        invalid(line, "detector_card.citations", card(3), "at least one citation is required")
      )
      _ <- citations.foldLeft[Either[TidyCsvError, Unit]](Right(())) {
        case (left @ Left(_), _)  => left
        case (Right(_), citation) =>
          val year  = citation(citation.length - 3)
          val title = citation(citation.length - 2)
          val doi   = citation.last
          Either.cond(
            citation.dropRight(3).forall(_.trim.nonEmpty) &&
              year.toIntOption.exists(_ > 0) && title.trim.nonEmpty &&
              doi.startsWith("10.") && doi.contains("/"),
            (),
            invalid(line, "detector_card.citations", card(3), "invalid citation metadata")
          )
      }
      assumptions <- unpack(line, "detector_card.assumptions", card(4))
      _           <- Either.cond(
        assumptions.forall(value => DetectorAssumption.values.exists(_.toString == value)),
        (),
        invalid(line, "detector_card.assumptions", card(4), "unknown detector assumption")
      )
      deviations <- unpack(line, "detector_card.deviations", card(5))
      _          <- Either.cond(
        deviations.forall(value => DetectorDeviation.values.exists(_.toString == value)),
        (),
        invalid(line, "detector_card.deviations", card(5), "unknown detector deviation")
      )
      _ <- Either.cond(
        DetectorExecution.values.exists(_.toString == card(6)),
        (),
        invalid(line, "detector_card.execution", card(6), "unknown execution mode")
      )
      references <- unpackNested(line, "detector_card.references", card(7), minimumFields = 3)
      _          <- Either.cond(
        references.nonEmpty && references.forall(reference =>
          (reference.head == "publication" && reference.length == 3) ||
            (reference.head == "source" && reference.length == 5)
        ),
        (),
        invalid(line, "detector_card.references", card(7), "invalid designated reference")
      )
      _ <- validateCanonicalParams(
        line,
        "detector_configuration",
        cell(row, "detector_configuration")
      )
    yield ()

  private def validateOriginCounts(line: Int, row: Vector[String]): Either[TidyCsvError, Unit] =
    val columns = Vector(
      "origin_sample_count",
      "origin_measured",
      "origin_interpolated",
      "origin_smoothed",
      "origin_projected",
      "origin_derived"
    )
    columns
      .foldLeft[Either[TidyCsvError, Vector[Int]]](Right(Vector.empty)) {
        case (left @ Left(_), _)     => left
        case (Right(values), column) =>
          cell(row, column).toIntOption
            .filter(_ >= 0)
            .map(value => Right(values :+ value))
            .getOrElse(
              Left(invalid(line, column, cell(row, column), "count must be non-negative"))
            )
      }
      .flatMap { counts =>
        val total        = counts(0)
        val measured     = counts(1)
        val interpolated = counts(2)
        val smoothed     = counts(3)
        val projected    = counts(4)
        val derived      = counts(5)
        Either.cond(
          measured + interpolated == total &&
            Vector(measured, interpolated, smoothed, projected, derived).forall(_ <= total) &&
            derived >= interpolated && derived >= smoothed && derived >= projected,
          (),
          invalid(
            line,
            columns.mkString("+"),
            counts.mkString(","),
            "lineage counts are not coherent with sample_count"
          )
        )
      }

  private def validateProvenance(line: Int, raw: String): Either[TidyCsvError, Unit] =
    unpack(line, "operation_provenance", raw).flatMap { values =>
      if values.length < 2 || !isLowerHex(values.head, 16) then
        Left(
          invalid(
            line,
            "operation_provenance",
            raw,
            "requires a 64-bit input hash and at least one operation"
          )
        )
      else
        values.tail.zipWithIndex.foldLeft[Either[TidyCsvError, Unit]](Right(())) {
          case (left @ Left(_), _)          => left
          case (Right(_), (encoded, index)) =>
            unpack(line, s"operation_provenance.step[$index]", encoded).flatMap {
              case Vector(operation, parameters) if operation.trim.nonEmpty =>
                validateCanonicalParams(
                  line,
                  s"operation_provenance.step[$index].parameters",
                  parameters
                )
              case _ =>
                Left(
                  invalid(
                    line,
                    s"operation_provenance.step[$index]",
                    encoded,
                    "operation step must contain a non-blank id and parameter payload"
                  )
                )
            }
        }
    }

  private def validateCanonicalParams(
      line: Int,
      column: String,
      raw: String
  ): Either[TidyCsvError, Unit] =
    unpack(line, column, raw).flatMap { values =>
      if values.length % 2 != 0 then
        Left(invalid(line, column, raw, "parameter payload must contain name/value pairs"))
      else
        values
          .grouped(2)
          .zipWithIndex
          .foldLeft[Either[TidyCsvError, Vector[String]]](
            Right(Vector.empty)
          ) {
            case (left @ Left(_), _)           => left
            case (Right(names), (pair, index)) =>
              val name  = pair.head
              val value = pair(1)
              if name.trim.isEmpty || names.contains(name) then
                Left(
                  invalid(line, column, raw, s"blank or duplicate parameter at index=$index")
                )
              else validateCanonicalParam(line, column, value).map(_ => names :+ name)
          }
          .map(_ => ())
    }

  private def validateCanonicalParam(
      line: Int,
      column: String,
      value: String
  ): Either[TidyCsvError, Unit] =
    if value.startsWith("text:") then Right(())
    else if value == "boolean:true" || value == "boolean:false" then Right(())
    else if value.startsWith("number:") then
      parseFiniteBits(line, column, value.drop("number:".length)).map(_ => ())
    else Left(invalid(line, column, value, "unknown canonical parameter encoding"))

  private def validateVersion(line: Int, value: String): Either[TidyCsvError, Unit] =
    value.split("\\.", -1).toVector match
      case Vector(major, minor, patch)
          if Vector(major, minor, patch).forall(part => part.toIntOption.exists(_ >= 0)) =>
        Right(())
      case _ =>
        Left(
          invalid(line, "detector_version", value, "expected non-negative major.minor.patch")
        )

  private def validateSha256(line: Int, value: String): Either[TidyCsvError, Unit] =
    Either.cond(
      isLowerHex(value, 64),
      (),
      invalid(line, "input_sha256", value, "expected exactly 64 lowercase hexadecimal digits")
    )

  private def validatePackedPairs(
      line: Int,
      column: String,
      raw: String,
      allowEmpty: Boolean
  ): Either[TidyCsvError, Unit] =
    unpack(line, column, raw).flatMap { values =>
      Either.cond(
        (allowEmpty || values.nonEmpty) && values.length % 2 == 0 &&
          values.grouped(2).forall(pair => pair.head.trim.nonEmpty && pair(1).trim.nonEmpty) &&
          values.grouped(2).map(_.head).toVector.distinct.length == values.length / 2,
        (),
        invalid(line, column, raw, "expected unique, non-blank key/value pairs")
      )
    }

  private def validatePackedStrings(
      line: Int,
      column: String,
      raw: String,
      allowBlank: Boolean
  ): Either[TidyCsvError, Unit] =
    unpack(line, column, raw).flatMap(values =>
      Either.cond(
        allowBlank || values.forall(_.trim.nonEmpty),
        (),
        invalid(line, column, raw, "packed values cannot be blank")
      )
    )

  private def unpackNested(
      line: Int,
      column: String,
      raw: String,
      exactFields: Int = -1,
      minimumFields: Int = -1
  ): Either[TidyCsvError, Vector[Vector[String]]] =
    unpack(line, column, raw).flatMap(
      _.zipWithIndex
        .foldLeft[Either[TidyCsvError, Vector[Vector[String]]]](Right(Vector.empty)) {
          case (left @ Left(_), _)                => left
          case (Right(decoded), (encoded, index)) =>
            unpack(line, s"$column[$index]", encoded).flatMap { values =>
              val valid =
                (exactFields < 0 || values.length == exactFields) &&
                  (minimumFields < 0 || values.length >= minimumFields)
              Either.cond(
                valid,
                decoded :+ values,
                invalid(
                  line,
                  s"$column[$index]",
                  encoded,
                  s"unexpected nested field count=${values.length}"
                )
              )
            }
        }
    )

  private def unpack(
      line: Int,
      column: String,
      raw: String
  ): Either[TidyCsvError, Vector[String]] =
    val values                        = Vector.newBuilder[String]
    var index                         = 0
    var failure: Option[TidyCsvError] = None
    while index < raw.length && failure.isEmpty do
      val colon = raw.indexOf(':', index)
      if colon <= index || !raw.substring(index, colon).forall(_.isDigit) then
        failure = Some(invalid(line, column, raw, s"invalid length prefix at character=$index"))
      else
        val lengthText = raw.substring(index, colon)
        val length     = lengthText.toIntOption
        length match
          case None =>
            failure = Some(
              invalid(line, column, raw, s"length prefix overflows at character=$index")
            )
          case Some(size) =>
            val start = colon + 1
            val end   = start.toLong + size.toLong
            if end > raw.length.toLong then
              failure = Some(
                invalid(
                  line,
                  column,
                  raw,
                  s"field length=$size exceeds payload at character=$index"
                )
              )
            else
              values += raw.substring(start, end.toInt)
              index = end.toInt
    failure.toLeft(values.result())

  private def parseFiniteBits(
      line: Int,
      column: String,
      raw: String
  ): Either[TidyCsvError, Double] =
    if !isHex(raw, 16) then Left(invalid(line, column, raw, "expected 16 hexadecimal digits"))
    else
      var bits  = 0L
      var index = 0
      while index < raw.length do
        bits = (bits << 4) | Character.digit(raw.charAt(index), 16).toLong
        index += 1
      val value = java.lang.Double.longBitsToDouble(bits)
      Either.cond(
        value.isFinite,
        value,
        invalid(line, column, raw, "encoded floating-point value must be finite")
      )

  private def validateNonNegativeLong(
      line: Int,
      row: Vector[String],
      column: String
  ): Either[TidyCsvError, Unit] =
    requireNonNegativeLong(line, column, cell(row, column)).map(_ => ())

  private def requireNonNegativeLong(
      line: Int,
      column: String,
      raw: String
  ): Either[TidyCsvError, Long] =
    parseLong(raw)
      .filter(_ >= 0L)
      .toRight(invalid(line, column, raw, "expected a non-negative integer"))

  private def requireLong(
      line: Int,
      column: String,
      raw: String
  ): Either[TidyCsvError, Long] =
    parseLong(raw).toRight(invalid(line, column, raw, "expected an integer"))

  private def parseLong(raw: String): Option[Long] = raw.toLongOption

  private def nonBlank(
      line: Int,
      row: Vector[String],
      column: String
  ): Either[TidyCsvError, Unit] =
    nonBlankValue(line, column, cell(row, column))

  private def nonBlankValue(
      line: Int,
      column: String,
      value: String
  ): Either[TidyCsvError, Unit] =
    Either.cond(
      value.trim.nonEmpty,
      (),
      invalid(line, column, value, "value must be non-blank")
    )

  private def isLowerHex(value: String, length: Int): Boolean =
    value.length == length && value.forall(character =>
      character.isDigit || character >= 'a' && character <= 'f'
    )

  private def isHex(value: String, length: Int): Boolean =
    value.length == length && value.forall(character => Character.digit(character, 16) >= 0)

  private def absoluteMicros(value: Long): Long =
    if value == Long.MinValue then Long.MaxValue else math.abs(value)

  private def square(value: Double): Double = value * value

  private def invalid(
      line: Int,
      column: String,
      value: String,
      reason: String
  ): TidyCsvError = TidyCsvError.InvalidScientificField(line, column, value, reason)

  private def cell(row: Vector[String], name: String): String = row(header.indexOf(name))

  private def exportRow(row: TidyAoiRow): Vector[String] =
    val (areaId, areaLabel, toId, toLabel) = row.target match
      case TidyTarget.Area(id, label)                          => (id.value, label, "", "")
      case TidyTarget.Transition(from, fromLabel, to, toLabel) =>
        (from.value, fromLabel, to.value, toLabel)
    val (status, value, missing) = row.value match
      case TidyValue.Duration(duration)     => ("present", duration.toMicros.toString, "")
      case TidyValue.Proportion(proportion) =>
        ("present", renderProportion(proportion), "")
      case TidyValue.Count(count)    => ("present", count.toString, "")
      case TidyValue.Missing(reason) => ("missing", "", reason)
    val evidence = row.evidence
    val spec     = evidence.frame.specification
    Vector(
      schemaVersion,
      row.study.participant,
      row.study.trial,
      packPairs(row.study.conditions),
      areaId,
      areaLabel,
      toId,
      toLabel,
      row.estimand.label,
      status,
      value,
      missing,
      row.unit.label,
      evidence.frame.id.name,
      bits(spec.xMin),
      bits(spec.yMin),
      bits(spec.xMax),
      bits(spec.yMax),
      evidence.frame.specification.yAxis.toString.toLowerCase,
      evidence.frame.unitSymbol,
      evidence.frame.unitName,
      evidence.clock.source.clock.name,
      evidence.clock.source.nativeUnit.label,
      evidence.clock.source.rounding.toString,
      evidence.clock.analysis.name,
      canonicalSynchronization(evidence.clock),
      evidence.detectorCard.id.value,
      evidence.detectorCard.version.render,
      evidence.detectorCard.name,
      canonicalCard(evidence.detectorCard),
      canonicalParams(evidence.detectorConfiguration),
      evidence.excludedTime.toMicros.toString,
      evidence.unclassifiedTime.toMicros.toString,
      evidence.sampleOrigins.sampleCount.toString,
      evidence.sampleOrigins.measured.toString,
      evidence.sampleOrigins.interpolated.toString,
      evidence.sampleOrigins.smoothed.toString,
      evidence.sampleOrigins.projected.toString,
      evidence.sampleOrigins.derived.toString,
      pack(evidence.warnings),
      canonicalProvenance(evidence.provenance),
      evidence.inputIdentity.hex
    )

  private def renderProportion(value: Double): String =
    val scaled   = math.round(value * 1e12)
    val whole    = scaled / 1000000000000L
    val fraction = math.abs(scaled % 1000000000000L)
    val digits   = f"$fraction%012d".reverse.dropWhile(_ == '0').reverse
    if digits.isEmpty then whole.toString else s"$whole.$digits"

  private def bits(value: Double): String =
    val raw = java.lang.Double.doubleToLongBits(value)
    val hex = java.lang.Long.toHexString(raw)
    "0" * (16 - hex.length) + hex

  private def canonicalCard(card: AlgorithmCard): String =
    val citations = card.citations.map { citation =>
      pack(citation.authors :+ citation.year.toString :+ citation.title :+ citation.doi.value)
    }
    val references = card.references.map {
      case DesignatedReference.Publication(citation) =>
        pack(Vector("publication", citation.title, citation.doi.value))
      case DesignatedReference.SourceImplementation(repository, revision, path, license) =>
        pack(Vector("source", repository, revision, path, license))
    }
    pack(
      Vector(
        card.id.value,
        card.version.render,
        card.name,
        pack(citations),
        pack(card.assumptions.map(_.toString)),
        pack(card.deviations.map(_.toString)),
        card.execution.toString,
        pack(references)
      )
    )

  private def canonicalParams(parameters: Vector[(String, Provenance.Param)]): String =
    pack(parameters.flatMap((name, value) => Vector(name, canonicalParam(value))))

  private def canonicalProvenance(provenance: Provenance): String =
    pack(
      Vector(provenance.inputs.render) ++ provenance.steps.map { step =>
        pack(Vector(step.operation, canonicalParams(step.params)))
      }
    )

  private def canonicalSynchronization(clock: ResultClockSpec): String =
    clock.synchronization match
      case None           => pack(Vector("identity", clock.analysis.name))
      case Some(evidence) =>
        val used = evidence.usedMarks.map(mark =>
          pack(
            Vector(mark.id, mark.onSource.toMicros.toString, mark.onTarget.toMicros.toString)
          )
        )
        val residuals = evidence.residuals.map(residual =>
          pack(
            Vector(
              residual.mark.id,
              residual.predictedTarget.toMicros.toString,
              residual.error.toMicros.toString
            )
          )
        )
        val rejected = evidence.rejectedMarks.map(value =>
          pack(
            Vector(
              value.mark.id,
              value.mark.onSource.toMicros.toString,
              value.mark.onTarget.toMicros.toString,
              value.residual.toMicros.toString,
              value.limit.span.toMicros.toString
            )
          )
        )
        pack(
          Vector(
            evidence.mode.render,
            evidence.source.name,
            evidence.target.name,
            bits(evidence.scale),
            evidence.offset.toMicros.toString,
            pack(used),
            pack(residuals),
            pack(rejected),
            evidence.rootMeanSquareResidual.toMicros.toString,
            evidence.maximumAbsoluteResidual.toMicros.toString,
            evidence.uncertainty.toMicros.toString
          )
        )

  private def canonicalParam(parameter: Provenance.Param): String = parameter match
    case Provenance.Param.Num(value)  => s"number:${bits(value)}"
    case Provenance.Param.Text(value) => s"text:$value"
    case Provenance.Param.Flag(value) => if value then "boolean:true" else "boolean:false"

  private def packPairs(values: Vector[(String, String)]): String =
    pack(values.flatMap((key, value) => Vector(key, value)))

  private def pack(values: Vector[String]): String =
    values.map(value => s"${value.length}:$value").mkString

private object Rfc4180:
  def encode(rows: Vector[Vector[String]]): String =
    rows.map(_.map(quote).mkString(",")).mkString("\r\n") + "\r\n"

  def decode(contents: String): Either[TidyCsvError, Vector[Vector[String]]] =
    val rows                          = ArrayBuffer.empty[Vector[String]]
    val fields                        = ArrayBuffer.empty[String]
    val field                         = new java.lang.StringBuilder
    var quoted                        = false
    var closed                        = false
    var index                         = 0
    var failure: Option[TidyCsvError] = None

    def finishField(): Unit =
      fields += field.toString
      field.setLength(0)
      closed = false

    def finishRow(): Unit =
      finishField()
      rows += fields.toVector
      fields.clear()

    while index < contents.length && failure.isEmpty do
      val character = contents.charAt(index)
      if quoted then
        if character == '"' then
          if index + 1 < contents.length && contents.charAt(index + 1) == '"' then
            field.append('"')
            index += 1
          else
            quoted = false
            closed = true
        else field.append(character)
      else if closed then
        character match
          case ',' => finishField()
          case '\r' if index + 1 < contents.length && contents.charAt(index + 1) == '\n' =>
            finishRow()
            index += 1
          case '\n'  => finishRow()
          case other => failure = Some(TidyCsvError.MalformedCsv(index, other))
      else
        character match
          case ','                      => finishField()
          case '"' if field.length == 0 => quoted = true
          case '"' => failure = Some(TidyCsvError.MalformedCsv(index, character))
          case '\r' if index + 1 < contents.length && contents.charAt(index + 1) == '\n' =>
            finishRow()
            index += 1
          case '\n'  => finishRow()
          case other => field.append(other)
      index += 1

    failure match
      case Some(error)    => Left(error)
      case None if quoted => Left(TidyCsvError.UnterminatedQuotedField(index))
      case None           =>
        if field.length > 0 || fields.nonEmpty then finishRow()
        Right(rows.toVector)

  private def quote(value: String): String =
    if value.exists(character =>
        character == ',' || character == '"' || character == '\r' || character == '\n'
      )
    then s"\"${value.replace("\"", "\"\"")}\""
    else value

/** A result artifact could not be assembled without weakening its evidence. */
enum TidyResultError derives CanEqual:
  case BlankParticipant(value: String)
  case BlankTrial(participant: String, value: String)
  case BlankConditionKey(participant: String, trial: String, index: Int, value: String)
  case BlankConditionValue(
      participant: String,
      trial: String,
      index: Int,
      key: String,
      value: String
  )
  case DuplicateConditionKey(
      participant: String,
      trial: String,
      key: String,
      firstIndex: Int,
      secondIndex: Int
  )
  case NoValidatedRecording(source: String)
  case FrameConflict(source: String, underlying: GeometryError)
  case ClockConflict(source: String, underlying: TimeError)
  case AnalysisRecordingMismatch(
      source: String,
      assignment: ContentHash,
      detection: ContentHash
  )
  case MissingSynchronization(source: String, nativeClock: ClockId, analysisClock: ClockId)
  case SynchronizationMismatch(
      source: String,
      nativeClock: ClockId,
      analysisClock: ClockId,
      evidenceSource: ClockId,
      evidenceTarget: ClockId
  )
  case CustomDetectorCannotProduceScientificExport(source: String, detector: DetectorRef)
  case TemporalSupportMismatch(
      source: String,
      detection: TemporalSupport,
      assignment: TemporalSupport
  )
  case MissingDetectorProvenance(source: String, detector: DetectorRef)
  case BlankDetectorParameter(source: String, index: Int, name: String)
  case DuplicateDetectorParameter(
      source: String,
      name: String,
      firstIndex: Int,
      secondIndex: Int
  )
  case BlankWarning(source: String, index: Int, warning: String)
  case BlankOperation(source: String, index: Int, operation: String)
  case InvalidOperationParameters(
      source: String,
      operationIndex: Int,
      operation: String,
      underlying: TidyResultError
  )

  def message: String = this match
    case BlankParticipant(value) =>
      s"A tidy result requires a participant id, got value='$value'."
    case BlankTrial(participant, value) =>
      s"Participant '$participant' requires a trial id, got value='$value'."
    case BlankConditionKey(participant, trial, index, value) =>
      s"Participant '$participant' trial '$trial' condition[$index] has blank key='$value'."
    case BlankConditionValue(participant, trial, index, key, value) =>
      s"Participant '$participant' trial '$trial' condition[$index] key='$key' has blank value='$value'."
    case DuplicateConditionKey(participant, trial, key, first, second) =>
      s"Participant '$participant' trial '$trial' repeats condition key='$key' at indices $first and $second."
    case NoValidatedRecording(source) =>
      s"Source '$source' has no validated recording from which to build a tidy result."
    case FrameConflict(source, underlying) =>
      s"Source '$source' tidy-result frame conflict: ${underlying.message}"
    case ClockConflict(source, underlying) =>
      s"Source '$source' tidy-result clock conflict: ${underlying.message}"
    case AnalysisRecordingMismatch(source, assignment, detection) =>
      s"Source '$source' AOI assignment recording='${assignment.render}' differs from detector recording='${detection.render}'."
    case MissingSynchronization(source, nativeClock, analysisClock) =>
      s"Source '$source' uses native clock='$nativeClock' but analysis clock='$analysisClock' and supplies no synchronization evidence."
    case SynchronizationMismatch(
          source,
          nativeClock,
          analysisClock,
          evidenceSource,
          evidenceTarget
        ) =>
      s"Source '$source' requires synchronization '$nativeClock' -> '$analysisClock' but evidence maps '$evidenceSource' -> '$evidenceTarget'."
    case CustomDetectorCannotProduceScientificExport(source, detector) =>
      s"Source '$source' was detected by custom detector '${detector.render}', which has no " +
        "bound algorithm card for scientific export."
    case TemporalSupportMismatch(source, detection, assignment) =>
      s"Source '$source' detection used temporalSupport=${detection.render}, but AOI " +
        s"assignment used temporalSupport=${assignment.render}."
    case MissingDetectorProvenance(source, detector) =>
      s"Source '$source' detector '${detector.render}' has no terminal detect provenance step."
    case BlankDetectorParameter(source, index, name) =>
      s"Source '$source' detector parameter[$index] has blank name='$name'."
    case DuplicateDetectorParameter(source, name, first, second) =>
      s"Source '$source' repeats detector parameter='$name' at indices $first and $second."
    case BlankWarning(source, index, warning) =>
      s"Source '$source' result warning[$index] is blank value='$warning'."
    case BlankOperation(source, index, operation) =>
      s"Source '$source' upstream operation[$index] has blank id='$operation'."
    case InvalidOperationParameters(source, index, operation, underlying) =>
      s"Source '$source' upstream operation[$index]='$operation' has invalid parameters: ${underlying.message}"

/** A fixed-schema RFC-4180 document is malformed or scientifically incomplete. */
enum TidyCsvError derives CanEqual:
  case MissingHeader
  case UnexpectedHeader(expected: Vector[String], actual: Vector[String])
  case WrongColumnCount(line: Int, expected: Int, actual: Int)
  case UnexpectedSchema(line: Int, expected: String, actual: String)
  case MissingRequiredContext(line: Int, column: String)
  case InvalidValueCells(line: Int, status: String, value: String, missingReason: String)
  case InvalidScientificField(line: Int, column: String, value: String, reason: String)
  case MalformedCsv(characterIndex: Int, character: Char)
  case UnterminatedQuotedField(characterIndex: Int)

  def message: String = this match
    case MissingHeader                      => "Tidy AOI CSV has no header row."
    case UnexpectedHeader(expected, actual) =>
      s"Tidy AOI CSV header=${actual.mkString("[", ",", "]")} does not equal required header=${expected.mkString("[", ",", "]")}."
    case WrongColumnCount(line, expected, actual) =>
      s"Tidy AOI CSV line=$line has $actual columns; schema requires $expected."
    case UnexpectedSchema(line, expected, actual) =>
      s"Tidy AOI CSV line=$line declares schema='$actual'; expected '$expected'."
    case MissingRequiredContext(line, column) =>
      s"Tidy AOI CSV line=$line is missing required context column='$column'."
    case InvalidValueCells(line, status, value, reason) =>
      s"Tidy AOI CSV line=$line has inconsistent value_status='$status', value='$value', missing_reason='$reason'."
    case InvalidScientificField(line, column, value, reason) =>
      s"Tidy AOI CSV line=$line has invalid scientific field column='$column', value='$value': $reason."
    case MalformedCsv(index, character) =>
      s"Tidy AOI CSV character[$index]='$character' violates RFC-4180 field quoting."
    case UnterminatedQuotedField(index) =>
      s"Tidy AOI CSV ends at character[$index] inside a quoted field."

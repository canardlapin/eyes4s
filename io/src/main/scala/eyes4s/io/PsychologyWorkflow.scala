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
import eyes4s.kernel.Unit2D.{Deg, Px}

/** Psychology-facing choice between the two fitted clock models. */
enum SynchronizationModel derives CanEqual:
  case OffsetOnly
  case Affine

  private[io] def kernel: SyncFitMode = this match
    case OffsetOnly => SyncFitMode.OffsetOnly
    case Affine     => SyncFitMode.Affine

/** One common event stated in native source milliseconds and analysis milliseconds. */
final class WorkflowSyncMark private[io] (private[io] val mark: SyncMark)

object WorkflowSyncMark:
  def of(
      id: String,
      sourceMilliseconds: Long,
      analysisMilliseconds: Long
  ): Either[PsychologyWorkflowError, WorkflowSyncMark] =
    for
      source   <- WorkflowTime.milliseconds(s"sync mark '$id' source", sourceMilliseconds)
      analysis <- WorkflowTime.milliseconds(s"sync mark '$id' analysis", analysisMilliseconds)
      mark     <- SyncMark
        .of(id, source, analysis)
        .left
        .map(PsychologyWorkflowError.InvalidSyncMark.apply)
    yield new WorkflowSyncMark(mark)

private object WorkflowTime:
  private val MinimumMilliseconds = Long.MinValue / 1000L
  private val MaximumMilliseconds = Long.MaxValue / 1000L

  def milliseconds(
      operand: String,
      value: Long
  ): Either[PsychologyWorkflowError, Instant] =
    if value < MinimumMilliseconds || value > MaximumMilliseconds then
      Left(PsychologyWorkflowError.MillisecondsOutsideRange(operand, value))
    else Right(Instant.millis(value))

  def span(
      source: String,
      operand: String,
      value: Long
  ): Either[PsychologyWorkflowError, Span] =
    milliseconds(s"source '$source' $operand", value).map(time => Span.micros(time.toMicros))

/** One rectangular AOI stated in the source display's pixel coordinates. */
final class WorkflowAoi private[io] (
    val id: String,
    val label: String,
    val xMin: Double,
    val yMin: Double,
    val xMax: Double,
    val yMax: Double
)

object WorkflowAoi:
  def of(
      id: String,
      label: String,
      xMin: Double,
      yMin: Double,
      xMax: Double,
      yMax: Double
  ): Either[PsychologyWorkflowError, WorkflowAoi] =
    if id.trim.isEmpty then Left(PsychologyWorkflowError.BlankAoiId(id))
    else if label.trim.isEmpty then Left(PsychologyWorkflowError.BlankAoiLabel(id.trim, label))
    else if !Vector(xMin, yMin, xMax, yMax).forall(_.isFinite) then
      Left(PsychologyWorkflowError.NonFiniteAoiBounds(id.trim, xMin, yMin, xMax, yMax))
    else if xMax <= xMin || yMax <= yMin then
      Left(PsychologyWorkflowError.DegenerateAoiBounds(id.trim, xMin, yMin, xMax, yMax))
    else Right(new WorkflowAoi(id.trim, label.trim, xMin, yMin, xMax, yMax))

/** Validated, reusable description of one end-to-end AdSERP-style analysis.
  *
  * Callers state ordinary laboratory quantities. The plan proves every local
  * numeric invariant before a source file is touched; [[PsychologyWorkflow.run]]
  * then returns either a named stage failure or a complete auditable result.
  */
final class AdserpWorkflowPlan private[io] (
    private[io] val sourceName: String,
    private[io] val study: StudyTrial,
    private[io] val displayFrame: Frame[Px],
    private[io] val viewing: Viewing,
    private[io] val trackerClock: ClockId,
    private[io] val analysisClock: ClockId,
    private[io] val synchronizationModel: SynchronizationModel,
    private[io] val synchronizationMarks: Vector[WorkflowSyncMark],
    private[io] val residualLimit: Option[SyncResidualLimit],
    private[io] val interpolationGap: InterpolationGap,
    private[io] val velocityThreshold: IvtThreshold,
    private[io] val minimumEventDuration: MinimumEventDuration,
    private[io] val areas: Vector[WorkflowAoi],
    private[io] val sourceMetadata: Vector[(String, String)]
)

object AdserpWorkflowPlan:
  private val ReservedMetadata = Set("source", "sha256", "acceptedRows", "rejectedRows")

  def of(
      sourceName: String,
      participant: String,
      trial: String,
      conditions: Vector[(String, String)],
      displayWidthPixels: Int,
      displayHeightPixels: Int,
      viewingDistanceMillimetres: Double,
      displayWidthMillimetres: Double,
      displayHeightMillimetres: Double,
      trackerClock: String,
      analysisClock: String,
      synchronizationModel: SynchronizationModel,
      synchronizationMarks: Vector[WorkflowSyncMark],
      interpolationMaxGapMilliseconds: Long,
      velocityThresholdDegreesPerSecond: Double,
      minimumEventDurationMilliseconds: Long,
      areas: Vector[WorkflowAoi],
      sourceMetadata: Vector[(String, String)] = Vector.empty,
      synchronizationResidualLimitMilliseconds: Option[Long] = None
  ): Either[PsychologyWorkflowError, AdserpWorkflowPlan] =
    for
      _ <- Either.cond(
        sourceName.trim.nonEmpty,
        (),
        PsychologyWorkflowError.BlankSourceName(sourceName)
      )
      study <- StudyTrial
        .of(participant, trial, conditions)
        .left
        .map(PsychologyWorkflowError.InvalidStudy.apply)
      _ <- Either.cond(
        trackerClock.trim.nonEmpty,
        (),
        PsychologyWorkflowError.BlankClock(sourceName.trim, "tracker", trackerClock)
      )
      _ <- Either.cond(
        analysisClock.trim.nonEmpty,
        (),
        PsychologyWorkflowError.BlankClock(sourceName.trim, "analysis", analysisClock)
      )
      frame <- Frame
        .screen(sourceName.trim + ":display", displayWidthPixels, displayHeightPixels)
        .left
        .map(PsychologyWorkflowError.InvalidDisplay(sourceName.trim, _))
      viewing <- Viewing
        .millimetres(
          viewingDistanceMillimetres,
          displayWidthMillimetres,
          displayHeightMillimetres
        )
        .left
        .map(PsychologyWorkflowError.InvalidViewing(sourceName.trim, _))
      gapSpan <- WorkflowTime.span(
        sourceName.trim,
        "interpolation maximum gap",
        interpolationMaxGapMilliseconds
      )
      gap <- InterpolationGap
        .of(gapSpan)
        .left
        .map(PsychologyWorkflowError.InvalidInterpolation(sourceName.trim, _))
      velocity <- Velocity
        .degPerSecond(velocityThresholdDegreesPerSecond)
        .left
        .map(PsychologyWorkflowError.InvalidVelocity(sourceName.trim, _))
      threshold <- IvtThreshold
        .of(velocity)
        .left
        .map(PsychologyWorkflowError.InvalidDetectorConfiguration(sourceName.trim, _))
      durationSpan <- WorkflowTime.span(
        sourceName.trim,
        "minimum event duration",
        minimumEventDurationMilliseconds
      )
      duration <- MinimumEventDuration
        .of(durationSpan)
        .left
        .map(PsychologyWorkflowError.InvalidDetectorConfiguration(sourceName.trim, _))
      residual <- synchronizationResidualLimitMilliseconds match
        case None        => Right(None)
        case Some(value) =>
          for
            limitSpan <- WorkflowTime.span(
              sourceName.trim,
              "synchronization residual limit",
              value
            )
            limit <- SyncResidualLimit
              .of(limitSpan)
              .left
              .map(PsychologyWorkflowError.InvalidSynchronization(sourceName.trim, _))
          yield Some(limit)
      _ <- validateAreas(sourceName.trim, frame, areas)
      _ <- validateMetadata(sourceName.trim, sourceMetadata)
    yield new AdserpWorkflowPlan(
      sourceName.trim,
      study,
      frame,
      viewing,
      ClockId(trackerClock.trim),
      ClockId(analysisClock.trim),
      synchronizationModel,
      synchronizationMarks,
      residual,
      gap,
      threshold,
      duration,
      areas,
      sourceMetadata.map((key, value) => key.trim -> value.trim)
    )

  private def validateAreas(
      source: String,
      frame: Frame[Px],
      areas: Vector[WorkflowAoi]
  ): Either[PsychologyWorkflowError, Unit] =
    if areas.isEmpty then Left(PsychologyWorkflowError.NoAreas(source))
    else
      areas.zipWithIndex
        .collectFirst {
          case (area, second) if areas.take(second).exists(_.id == area.id) =>
            PsychologyWorkflowError.DuplicateArea(
              source,
              area.id,
              areas.indexWhere(_.id == area.id),
              second
            )
          case (area, _) if !frame.contains(Pt[Px](area.xMin, area.yMin)) =>
            PsychologyWorkflowError.AreaOutsideDisplay(
              source,
              area.id,
              area.xMin,
              area.yMin,
              frame.id
            )
          case (area, _) if area.xMax > frame.bounds.xMax || area.yMax > frame.bounds.yMax =>
            PsychologyWorkflowError.AreaOutsideDisplay(
              source,
              area.id,
              area.xMax,
              area.yMax,
              frame.id
            )
        }
        .toLeft(())

  private def validateMetadata(
      source: String,
      metadata: Vector[(String, String)]
  ): Either[PsychologyWorkflowError, Unit] =
    metadata.zipWithIndex
      .collectFirst {
        case ((key, value), index) if key.trim.isEmpty || value.trim.isEmpty =>
          PsychologyWorkflowError.InvalidSourceMetadata(source, index, key, value)
        case ((key, _), index) if ReservedMetadata.contains(key.trim) =>
          PsychologyWorkflowError.ReservedSourceMetadata(source, index, key.trim)
        case ((key, _), second) if metadata.take(second).exists(_._1.trim == key.trim) =>
          PsychologyWorkflowError.DuplicateSourceMetadata(
            source,
            key.trim,
            metadata.indexWhere(_._1.trim == key.trim),
            second
          )
      }
      .toLeft(())

/** Application summary of every stage in one completed workflow. */
final case class PsychologyWorkflowReport(
    source: String,
    sourceRows: Int,
    acceptedRows: Int,
    rejectedRows: Int,
    synchronizationMarks: Int,
    synchronizationRejectedMarks: Int,
    synchronizationRmsMilliseconds: Double,
    projectedSamples: Int,
    interpolatedSamples: Int,
    detectedEvents: Int,
    result: PsychologyResultReport
) derives CanEqual

/** Complete auditable stages plus the small report and export used by an application. */
final class PsychologyWorkflowResult private[io] (
    val imported: DelimitedImport[Px],
    val synchronization: SyncEvidence,
    val angularRecording: Recording[Deg],
    val preparedRecording: Recording[Deg],
    val detection: DetectionResult[Deg],
    val assignment: AoiAssignment[Deg],
    val tidy: TidyAoiResult,
    val csv: String
):
  def warnings: Vector[String] = tidy.evidence.warnings
  def provenance: Provenance   = tidy.evidence.provenance

  def report: PsychologyWorkflowReport =
    val origins = SampleOriginCounts.from(preparedRecording)
    PsychologyWorkflowReport(
      imported.raw.source,
      imported.acceptedCount + imported.rejectedCount,
      imported.acceptedCount,
      imported.rejectedCount,
      synchronization.usedMarks.length,
      synchronization.rejectedMarks.length,
      synchronization.rootMeanSquareResidual.toMillis,
      origins.projected,
      origins.interpolated,
      detection.eventSeries.size,
      tidy.report
    )

  def explain: String =
    val summary = report
    s"Accepted ${summary.acceptedRows} of ${summary.sourceRows} source rows from '${summary.source}' and retained " +
      s"${summary.rejectedRows} rejected rows as diagnostics. Clock synchronization used " +
      s"${summary.synchronizationMarks} marks with ${summary.synchronizationRmsMilliseconds} ms RMS error. " +
      s"Preprocessing retained ${summary.projectedSamples} projected and " +
      s"${summary.interpolatedSamples} interpolated samples; detection produced " +
      s"${summary.detectedEvents} events. ${tidy.explain}"

/** The narrow, reproducible path from an AdSERP-style source file to tidy AOI results. */
object PsychologyWorkflow:

  def run(
      plan: AdserpWorkflowPlan,
      contents: String
  ): Either[PsychologyWorkflowError, PsychologyWorkflowResult] =
    for
      schema <- schemaFor(plan.sourceName)
      raw = Delimited.parse(
        plan.sourceName,
        contents,
        schema,
        plan.sourceMetadata
      )
      imported = raw.validate(
        plan.displayFrame,
        plan.trackerClock,
        Rate.Irregular,
        Eye.Left
      )
      native <- imported.recording.toRight(
        PsychologyWorkflowError.ImportFailed(
          plan.sourceName,
          imported.diagnostics.map(_.message)
        )
      )
      synchronization <- SyncEvidence
        .fromCommonMarks(
          plan.trackerClock,
          plan.analysisClock,
          plan.synchronizationModel.kernel,
          plan.synchronizationMarks.map(_.mark),
          plan.residualLimit
        )
        .left
        .map(PsychologyWorkflowError.InvalidSynchronization(plan.sourceName, _))
      synchronized <- synchronize(plan.sourceName, native, synchronization)
      angularFrame <- Frame
        .angular(
          plan.sourceName + ":visual-angle",
          plan.viewing.horizontalExtent.toDegrees,
          plan.viewing.verticalExtent.toDegrees
        )
        .left
        .map(PsychologyWorkflowError.AngularFrameFailed(plan.sourceName, _))
      warp = Viewing.angularWarp(plan.viewing, plan.displayFrame, angularFrame)
      angular <- synchronized
        .warp(warp)
        .left
        .map(PsychologyWorkflowError.WarpFailed(plan.sourceName, _))
      prepared <- preprocess(plan.sourceName, angular, plan.interpolationGap)
      detector = Detectors.ivt(
        plan.velocityThreshold,
        plan.minimumEventDuration,
        plan.analysisClock
      )
      temporalSupport = prepared.representedSupport.policy
      detection <- Detection
        .run(
          RecordingRef(s"${plan.study.participant}/${plan.study.trial}"),
          prepared,
          detector,
          GapPolicy.Break,
          temporalSupport
        )
        .left
        .map(PsychologyWorkflowError.DetectionFailed(plan.sourceName, _))
      aoiSet     <- buildAreas(plan.sourceName, angularFrame, warp, plan.areas)
      assignment <- aoiSet
        .assign(prepared, MembershipPolicy.ExclusiveByPriority, temporalSupport)
        .left
        .map(PsychologyWorkflowError.AssignmentFailed(plan.sourceName, _))
      operations = upstreamOperations(plan, imported, synchronization, angularFrame)
      tidy <- TidyAoiResult
        .from(
          plan.study,
          imported,
          detection,
          assignment,
          Some(synchronization),
          operations
        )
        .left
        .map(PsychologyWorkflowError.TidyResultFailed(plan.sourceName, _))
      csv = TidyCsv.encode(tidy)
      decoded <- TidyCsv
        .decode(csv)
        .left
        .map(PsychologyWorkflowError.ExportFailed(plan.sourceName, _))
      _ <- Either.cond(
        decoded.encode == csv,
        (),
        PsychologyWorkflowError.ExportRoundTripMismatch(plan.sourceName)
      )
    yield new PsychologyWorkflowResult(
      imported,
      synchronization,
      angular,
      prepared,
      detection,
      assignment,
      tidy,
      csv
    )

  private def schemaFor(
      source: String
  ): Either[PsychologyWorkflowError, DelimitedSchema[Px]] =
    for
      validity <- ValidityCodebook
        .of(tracked = Set("1"), lost = Set("0"))
        .left
        .map(PsychologyWorkflowError.SchemaFailed(source, _))
      schema <- DelimitedSchema
        .of(
          Delimiter.Comma,
          HeaderMode.FirstLine,
          TimeColumn("timestamp", TimestampUnit.Milliseconds),
          PositionColumns("BPOGX", "BPOGY", CoordinateUnit.Pixels),
          ValidityColumn("LPV", validity),
          Some(PupilColumn("LPD", PupilUnit.Diameter)),
          missingTokens = Set("", "NA")
        )
        .left
        .map(PsychologyWorkflowError.SchemaFailed(source, _))
    yield schema

  private def synchronize(
      source: String,
      recording: Recording[Px],
      synchronization: SyncEvidence
  ): Either[PsychologyWorkflowError, Recording[Px]] =
    Recording
      .of(
        recording.frame,
        synchronization.target,
        recording.rate,
        recording.eye,
        recording.pupilUnit,
        recording.samples.map(sample => sample.copy(t = synchronization(sample.t)))
      )
      .left
      .map(PsychologyWorkflowError.SynchronizedRecordingFailed(source, _))

  private def preprocess(
      source: String,
      recording: Recording[Deg],
      gap: InterpolationGap
  ): Either[PsychologyWorkflowError, Recording[Deg]] =
    val samples = Filter.interpolateGaps[Deg](gap).runAll(recording.samples)
    if samples.length != recording.size then
      Left(
        PsychologyWorkflowError.PreprocessingCardinality(source, recording.size, samples.length)
      )
    else
      Recording
        .of(
          recording.frame,
          recording.clock,
          recording.rate,
          recording.eye,
          recording.pupilUnit,
          IArray.from(samples)
        )
        .left
        .map(PsychologyWorkflowError.PreprocessedRecordingFailed(source, _))

  private def buildAreas(
      source: String,
      angularFrame: Frame[Deg],
      warp: Warp[Px, Deg],
      specifications: Vector[WorkflowAoi]
  ): Either[PsychologyWorkflowError, AoiSet[Deg]] =
    specifications
      .foldLeft[Either[PsychologyWorkflowError, Vector[Aoi[Deg]]]](Right(Vector.empty)) {
        (acc, specification) =>
          for
            built <- acc
            first <- warp(Pt[Px](specification.xMin, specification.yMin)).toRight(
              PsychologyWorkflowError.AreaWarpUndefined(source, specification.id, "minimum")
            )
            second <- warp(Pt[Px](specification.xMax, specification.yMax)).toRight(
              PsychologyWorkflowError.AreaWarpUndefined(source, specification.id, "maximum")
            )
            lower = Pt[Deg](math.min(first.x, second.x), math.min(first.y, second.y))
            upper = Pt[Deg](math.max(first.x, second.x), math.max(first.y, second.y))
            region <- Region
              .rect(lower, upper)
              .left
              .map(PsychologyWorkflowError.AreaRegionFailed(source, specification.id, _))
            area <- Aoi
              .of(
                specification.id,
                specification.label,
                angularFrame,
                region,
                Map(
                  "nativeFrame"        -> warp.from.id.name,
                  "nativeBoundsPixels" ->
                    s"${specification.xMin},${specification.yMin},${specification.xMax},${specification.yMax}"
                )
              )
              .left
              .map(PsychologyWorkflowError.AreaConstructionFailed(source, _))
          yield built :+ area
      }
      .flatMap(
        AoiSet
          .of(_)
          .left
          .map(PsychologyWorkflowError.AreaConstructionFailed(source, _))
      )

  private def upstreamOperations(
      plan: AdserpWorkflowPlan,
      imported: DelimitedImport[Px],
      synchronization: SyncEvidence,
      angularFrame: Frame[Deg]
  ): Vector[Provenance.Step] =
    Vector(
      Provenance.Step(
        "import-delimited",
        Vector(
          "source"       -> Provenance.Param.Text(plan.sourceName),
          "sha256"       -> Provenance.Param.Text(imported.sourceDigest.hex),
          "acceptedRows" -> Provenance.Param.Num(imported.acceptedCount.toDouble),
          "rejectedRows" -> Provenance.Param.Num(imported.rejectedCount.toDouble)
        ) ++ plan.sourceMetadata.map((key, value) => key -> Provenance.Param.Text(value))
      ),
      Provenance.Step(
        "synchronize",
        Vector(
          "sourceClock"  -> Provenance.Param.Text(synchronization.source.name),
          "targetClock"  -> Provenance.Param.Text(synchronization.target.name),
          "model"        -> Provenance.Param.Text(synchronization.mode.render),
          "scale"        -> Provenance.Param.Num(synchronization.scale),
          "offsetMicros" -> Provenance.Param.Text(synchronization.offset.toMicros.toString)
        )
      ),
      Provenance.Step(
        "warp-visual-angle",
        Vector(
          "sourceFrame" -> Provenance.Param.Text(plan.displayFrame.id.name),
          "targetFrame" -> Provenance.Param.Text(angularFrame.id.name),
          "viewing"     -> Provenance.Param.Text(plan.viewing.render)
        )
      ),
      Provenance.Step(
        "interpolate-gaps",
        Vector(
          "maximumGapMicros" -> Provenance.Param.Num(
            plan.interpolationGap.span.toMicros.toDouble
          )
        )
      )
    )

/** A workflow stage failed without discarding its operand identities. */
enum PsychologyWorkflowError derives CanEqual:
  case BlankSourceName(value: String)
  case InvalidStudy(underlying: TidyResultError)
  case MillisecondsOutsideRange(operand: String, value: Long)
  case BlankClock(source: String, role: String, value: String)
  case InvalidDisplay(source: String, underlying: GeometryError)
  case InvalidViewing(source: String, underlying: GeometryError)
  case InvalidInterpolation(source: String, underlying: ConfigurationError)
  case InvalidVelocity(source: String, underlying: GeometryError)
  case InvalidDetectorConfiguration(source: String, underlying: ConfigurationError)
  case InvalidSyncMark(underlying: SyncEvidenceError)
  case InvalidSynchronization(source: String, underlying: SyncEvidenceError)
  case BlankAoiId(value: String)
  case BlankAoiLabel(id: String, value: String)
  case NonFiniteAoiBounds(
      id: String,
      xMin: Double,
      yMin: Double,
      xMax: Double,
      yMax: Double
  )
  case DegenerateAoiBounds(
      id: String,
      xMin: Double,
      yMin: Double,
      xMax: Double,
      yMax: Double
  )
  case NoAreas(source: String)
  case DuplicateArea(source: String, id: String, firstIndex: Int, secondIndex: Int)
  case AreaOutsideDisplay(source: String, id: String, x: Double, y: Double, frame: FrameId)
  case InvalidSourceMetadata(source: String, index: Int, key: String, value: String)
  case ReservedSourceMetadata(source: String, index: Int, key: String)
  case DuplicateSourceMetadata(
      source: String,
      key: String,
      firstIndex: Int,
      secondIndex: Int
  )
  case SchemaFailed(source: String, underlying: DelimitedSchemaError)
  case ImportFailed(source: String, diagnostics: Vector[String])
  case SynchronizedRecordingFailed(source: String, underlying: RecordingError)
  case AngularFrameFailed(source: String, underlying: GeometryError)
  case WarpFailed(source: String, underlying: CoreError)
  case PreprocessingCardinality(source: String, expected: Int, actual: Int)
  case PreprocessedRecordingFailed(source: String, underlying: RecordingError)
  case DetectionFailed(source: String, underlying: DetectionResultError)
  case AreaWarpUndefined(source: String, id: String, corner: String)
  case AreaRegionFailed(source: String, id: String, underlying: GeometryError)
  case AreaConstructionFailed(source: String, underlying: AoiError)
  case AssignmentFailed(source: String, underlying: AoiError)
  case TidyResultFailed(source: String, underlying: TidyResultError)
  case ExportFailed(source: String, underlying: TidyCsvError)
  case ExportRoundTripMismatch(source: String)

  def message: String = this match
    case BlankSourceName(value) =>
      s"Psychology workflow requires a source name, got value='$value'."
    case InvalidStudy(underlying) =>
      s"Psychology workflow study identity is invalid: ${underlying.message}"
    case MillisecondsOutsideRange(operand, value) =>
      s"Workflow operand '$operand' milliseconds=$value cannot be represented exactly as microseconds."
    case BlankClock(source, role, value) =>
      s"Source '$source' requires a non-empty $role clock, got value='$value'."
    case InvalidDisplay(source, underlying) =>
      s"Source '$source' display is invalid: ${underlying.message}"
    case InvalidViewing(source, underlying) =>
      s"Source '$source' viewing geometry is invalid: ${underlying.message}"
    case InvalidInterpolation(source, underlying) =>
      s"Source '$source' interpolation policy is invalid: ${underlying.message}"
    case InvalidVelocity(source, underlying) =>
      s"Source '$source' I-VT velocity is invalid: ${underlying.message}"
    case InvalidDetectorConfiguration(source, underlying) =>
      s"Source '$source' I-VT configuration is invalid: ${underlying.message}"
    case InvalidSyncMark(underlying) =>
      s"Workflow synchronization mark is invalid: ${underlying.message}"
    case InvalidSynchronization(source, underlying) =>
      s"Source '$source' synchronization is invalid: ${underlying.message}"
    case BlankAoiId(value) =>
      s"Workflow AOI requires a non-empty id, got value='$value'."
    case BlankAoiLabel(id, value) =>
      s"Workflow AOI '$id' requires a label, got value='$value'."
    case NonFiniteAoiBounds(id, xMin, yMin, xMax, yMax) =>
      s"Workflow AOI '$id' has non-finite pixel bounds=($xMin,$yMin,$xMax,$yMax)."
    case DegenerateAoiBounds(id, xMin, yMin, xMax, yMax) =>
      s"Workflow AOI '$id' has degenerate pixel bounds=($xMin,$yMin,$xMax,$yMax)."
    case NoAreas(source) =>
      s"Source '$source' workflow requires at least one AOI."
    case DuplicateArea(source, id, first, second) =>
      s"Source '$source' repeats AOI id='$id' at indices $first and $second."
    case AreaOutsideDisplay(source, id, x, y, frame) =>
      s"Source '$source' AOI '$id' corner=($x,$y) lies outside display frame='$frame'."
    case InvalidSourceMetadata(source, index, key, value) =>
      s"Source '$source' metadata[$index] has blank key='$key' or value='$value'."
    case ReservedSourceMetadata(source, index, key) =>
      s"Source '$source' metadata[$index] uses reserved provenance key='$key'."
    case DuplicateSourceMetadata(source, key, first, second) =>
      s"Source '$source' repeats metadata key='$key' at indices $first and $second."
    case SchemaFailed(source, underlying) =>
      s"Source '$source' fixed AdSERP schema is invalid: ${underlying.message}"
    case ImportFailed(source, diagnostics) =>
      s"Source '$source' produced no validated recording: ${diagnostics.mkString(" | ")}"
    case SynchronizedRecordingFailed(source, underlying) =>
      s"Source '$source' could not be placed on the analysis clock: ${underlying.message}"
    case AngularFrameFailed(source, underlying) =>
      s"Source '$source' angular frame is invalid: ${underlying.message}"
    case WarpFailed(source, underlying) =>
      s"Source '$source' could not be projected to visual angle: ${underlying.message}"
    case PreprocessingCardinality(source, expected, actual) =>
      s"Source '$source' preprocessing changed sample cardinality from expected=$expected to actual=$actual."
    case PreprocessedRecordingFailed(source, underlying) =>
      s"Source '$source' preprocessing produced an invalid recording: ${underlying.message}"
    case DetectionFailed(source, underlying) =>
      s"Source '$source' detector failed: ${underlying.message}"
    case AreaWarpUndefined(source, id, corner) =>
      s"Source '$source' AOI '$id' $corner corner has no visual-angle image."
    case AreaRegionFailed(source, id, underlying) =>
      s"Source '$source' AOI '$id' angular region is invalid: ${underlying.message}"
    case AreaConstructionFailed(source, underlying) =>
      s"Source '$source' AOI construction failed: ${underlying.message}"
    case AssignmentFailed(source, underlying) =>
      s"Source '$source' AOI assignment failed: ${underlying.message}"
    case TidyResultFailed(source, underlying) =>
      s"Source '$source' tidy result failed: ${underlying.message}"
    case ExportFailed(source, underlying) =>
      s"Source '$source' tidy CSV failed validation: ${underlying.message}"
    case ExportRoundTripMismatch(source) =>
      s"Source '$source' tidy CSV did not survive deterministic decode and re-encode."

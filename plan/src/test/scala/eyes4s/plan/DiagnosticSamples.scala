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

package eyes4s.plan

import cats.data.NonEmptyVector
import eyes4s.aoi.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.surface.EstimateError
import scala.compiletime.constValueTuple
import scala.deriving.Mirror

/** One cataloged enum: its catalog family (reached through the enum's own
  * `Diagnose` instance), the compiler's list of its cases, and one projected
  * sample per case.
  */
final case class FamilySamples(
    enumName: String,
    family: DiagnosticFamily,
    labels: Vector[String],
    samples: Vector[(scala.reflect.Enum, Diagnostic[Any])]
)

/** A sample of every case of every cataloged enum, projected through the
  * public `Diagnose` instances. The compiler supplies each enum's case labels,
  * so a new case appears in `labels` before anyone writes a code for it.
  */
object DiagnosticSamples:
  inline def labelsOf[E](using m: Mirror.SumOf[E]): Vector[String] =
    constValueTuple[m.MirroredElemLabels].toList.map(_.toString).toVector

  inline def family[E <: scala.reflect.Enum](enumName: String)(samples: E*)(using
      diagnose: Diagnose[E, ?],
      mirror: Mirror.SumOf[E]
  ): FamilySamples =
    FamilySamples(
      enumName,
      diagnose.family,
      labelsOf[E],
      samples.toVector.map(sample => sample -> (diagnose(sample): Diagnostic[Any]))
    )

  private def get[E, A](e: Either[E, A]): A =
    e.fold(error => throw new AssertionError(s"$error"), identity)

  private val Inf = Double.PositiveInfinity

  val k1: StudyKey        = StudyKey("p1", "a", "recall")
  val k2: StudyKey        = StudyKey("p1", "a", "encode")
  private val frame       = get(Frame.screen("sample-frame", 4, 3))
  private val otherFrame  = get(Frame.screen("sample-frame", 5, 3))
  private val grid        = get(Grid.over(frame, 2, 2))
  private val otherGrid   = get(Grid.over(frame, 3, 2))
  private val fid         = frame.id
  private val deg         = FrameId("sample-degrees")
  private val gid         = grid.id
  private val clk         = ClockId("sample-clock")
  private val clk2        = ClockId("sample-target")
  private val span        = get(Interval.of(clk, Instant.micros(10), Instant.micros(20)))
  private val span2       = get(Interval.of(clk, Instant.micros(0), Instant.micros(100)))
  private val range       = get(SampleRange.of(2, 5))
  private val range2      = get(SampleRange.of(4, 9))
  private val rec         = RecordingRef("sample-recording")
  private val det         = DetectorRef("sample-detector", "1")
  private val alg         = get(AlgorithmId.from("sample-algorithm"))
  private val aoiA        = get(AoiId.of("area-a"))
  private val aoiB        = get(AoiId.of("area-b"))
  private val digest      = "0123456789abcdef"
  private val digest2     = "fedcba9876543210"
  private val cosine2     = get(DefinitionId.of("eyes4s.cosine", 2))
  private val params      = Vector("weight" -> Provenance.Param.Text("duration"))
  private val step        = Provenance.Step("pair", Vector("k" -> Provenance.Param.Num(1)))
  private val provenance  = Provenance(ContentHash.ofString("inputs"), Vector(step))
  private val provenance2 = Provenance(ContentHash.ofString("other"), Vector.empty)
  private val successful  = get(FailurePolicy.successfulOnly(1))
  private val spec        = get(
    EvaluationSpec.of(
      "eyes4s.cosine",
      "1",
      params,
      Vector("value"),
      EvaluationGeometry.onGrid(grid),
      EvaluationTime.OrderFree
    )
  )
  private val spec2 = get(
    EvaluationSpec.of(
      "eyes4s.cosine",
      "2",
      Vector.empty,
      Vector("value"),
      EvaluationGeometry.Independent,
      EvaluationTime.SharedClock(clk)
    )
  )
  private val info        = EvaluationInfo("cosine", EvaluationScale.Count, Some(spec))
  private val studyRef    = get(ArtifactRef.parse[StudyInput[StudyKey, Px]](digest))
  private val studyRef2   = get(ArtifactRef.parse[StudyInput[StudyKey, Px]](digest2))
  private val recRef      = get(ArtifactRef.parse[Recording[Px]](digest))
  private val recRef2     = get(ArtifactRef.parse[Recording[Px]](digest2))
  private val temporalRef =
    get(ArtifactRef.parse[TemporalStudyInput[StudyKey, Px]](digest))
  private val temporalRef2 =
    get(ArtifactRef.parse[TemporalStudyInput[StudyKey, Px]](digest2))

  private val frameError = GeometryError.FrameMismatch(fid, deg)
  private val tally      = get(
    WindowTally.of(1, 2, 5, Span.micros(10L), Span.micros(20L), Span.micros(100L))
  )
  private val surfaceError  = SurfaceError.DegenerateTotal(0)
  private val timeError     = TimeError.ClockMismatch(clk, clk2)
  private val supportError  = DetectionSupportError.InvalidSampleRange(5, 2)
  private val coreError     = CoreError.OfEvent(EventError.NonPositiveSampleCount(0))
  private val sampleError   = CoreError.OfRecording(RecordingError.NonMonotonic(3, 10L, 5L))
  private val eventSupport  = DetectionSupportError.EventSpanHasNoSamples(rec, 2, span)
  private val syncError     = SyncEvidenceError.TooFewCommonMarks(clk, clk2, 1, 2)
  private val compatibility =
    ContrastCompatibilityError.Orientation(
      ReductionOrientation.ByLeft,
      ReductionOrientation.ByRight
    )

  val all: Vector[FamilySamples] = Vector(
    family[PlanError]("PlanError")(
      PlanError.InvalidDefinition(" ", 0),
      PlanError.InvalidArtifact("xyz"),
      PlanError.MissingArtifact(digest),
      PlanError.ArtifactMismatch(digest, digest2),
      PlanError.InvalidPhases("recall", " "),
      PlanError.EmptyScales(0),
      PlanError.DuplicateScales(Vector("binned", "binned")),
      PlanError.Specification(EvaluationSpecError.InvalidComponents(Vector.empty)),
      PlanError.Schedule(PairScheduleError.InvalidQuantum(0)),
      PlanError.StudyWorkBudget(2, 3, 1, 5L),
      PlanError.ChangedPreparedPlan(DefinitionId.cosine, DefinitionId.studyLayout),
      PlanError.ComparisonWork(ComparisonWorkError.InvalidQuantum(0)),
      PlanError.UnsupportedExecution(
        DefinitionId.cosine,
        ExecutionCapability.SynchronousWholeOperation
      ),
      PlanError.MissingAngularScale(1),
      PlanError.Geometry(frameError),
      PlanError.InvalidWindowTally(3, 2, 4, 10L, 20L, 25L),
      PlanError.InvalidOccurrence(0),
      PlanError.BlankKeyField("item"),
      PlanError
        .OccurrenceUnavailable(DefinitionId.studyLayout, MatchedReferences.SameOccurrence),
      PlanError.MatchItemConflict(Vector(digest, digest2)),
      PlanError
        .MatchedCardinality(MatchedReferences.RequireOne, Vector(digest), Vector(digest2)),
      PlanError.UnmatchedFocalRefused(Vector(digest))
    ),
    family[StudyFailure[StudyKey]]("StudyFailure")(
      StudyFailure.Frame(k1, frameError),
      StudyFailure.Occupancy(k1, surfaceError),
      StudyFailure.Temporal(k1, TemporalStudyError.MissingEpoch(digest)),
      StudyFailure.Estimation(k1, EstimateError.NoMass),
      StudyFailure.Comparison(k1, k2, CompareError.ZeroNorm("cosine", 0, 1)),
      StudyFailure.OffWindow(k1, tally)
    ),
    family[StudyResultError[StudyKey]]("StudyResultError")(
      StudyResultError.Description("grid", Vector.empty),
      StudyResultError.InputMismatch(digest, Vector(Provenance.Param.Text(digest2))),
      StudyResultError.LayoutMismatch(DefinitionId.studyLayout, DefinitionId.cosine),
      StudyResultError.ScaleCount(2, 1),
      StudyResultError.ScaleEstimate(
        Vector(Provenance.Param.Text("binned")),
        Vector(Provenance.Param.Text("gaussian"))
      ),
      StudyResultError.MassGrid(
        k1,
        Vector(Provenance.Param.Num(2)),
        Vector(Provenance.Param.Num(3))
      ),
      StudyResultError.MassProvenance(k1, Vector(step), Vector.empty),
      StudyResultError.FailureKey(k1, StudyFailure.Estimation(k2, EstimateError.NoMass)),
      StudyResultError.PairFailure(k1, k2, StudyFailure.Estimation(k1, EstimateError.NoMass)),
      StudyResultError.OrphanKey(k1),
      StudyResultError.OrphanPair(k1, k2),
      StudyResultError.SourceIdentity(StudyDesign.Matched),
      StudyResultError.ContrastAnalyses(StudyDesign.Control),
      StudyResultError.ProvenanceInputs(StudyDesign.Matched, digest, digest2),
      StudyResultError.MissingSpecification(StudyDesign.Control, info),
      StudyResultError
        .SpecificationMethod(StudyDesign.Matched, DefinitionId.cosine, "other", "2"),
      StudyResultError.SpecificationParameters(StudyDesign.Matched, params, Vector.empty),
      StudyResultError.Policy(StudyDesign.Matched, "require-all", successful),
      StudyResultError.Phase(k1, "recall", "encode"),
      StudyResultError.Reconstruction(ReconstructionError.RowCount(2, 3)),
      StudyResultError.Scale(0, StudyResultError.OrphanKey(k1)),
      StudyResultError.SpecificationTime(
        StudyDesign.Control,
        EvaluationTime.RelativeMicroseconds,
        EvaluationTime.OrderFree
      )
    ),
    family[TemporalStudyError]("TemporalStudyError")(
      TemporalStudyError.Input(PlanError.MissingArtifact(digest)),
      TemporalStudyError.Time(timeError),
      TemporalStudyError.Occupancy(WindowOccupancyError.Measure(surfaceError)),
      TemporalStudyError.InvalidWindow("early", 10L, 5L),
      TemporalStudyError.AnchorOverflow("early", Long.MaxValue, 0L, 10L),
      TemporalStudyError.InvalidRepetition("rep", "a", "b"),
      TemporalStudyError.WindowNames(Vector("early", "early")),
      TemporalStudyError.RepetitionNames(Vector.empty),
      TemporalStudyError.DuplicateEpochs(Vector(digest)),
      TemporalStudyError.DuplicateTrials(Vector(digest)),
      TemporalStudyError.UnknownEpochs(Vector(digest2)),
      TemporalStudyError.MissingEpoch(digest),
      TemporalStudyError.Weighting(Weight.Uniform)
    ),
    family[RecordingPlanError]("RecordingPlanError")(
      RecordingPlanError.Input(PlanError.MissingArtifact(digest)),
      RecordingPlanError.Geometry(frameError),
      RecordingPlanError.Time(timeError),
      RecordingPlanError.Synchronization(syncError),
      RecordingPlanError.Core("preprocess", sampleError),
      RecordingPlanError.Recording("angular", RecordingError.NonMonotonic(3, 10L, 5L)),
      RecordingPlanError.DetectorDefinition(DetectorDefinitionError.Configuration(alg, params)),
      RecordingPlanError.Detection(DetectionResultError.SourceSupport(rec, det, eventSupport)),
      RecordingPlanError.Areas(AoiError.EmptySet),
      RecordingPlanError.MissingViewing(rec),
      RecordingPlanError.MissingSynchronization(clk, clk2),
      RecordingPlanError.InvalidSource(RecordingRef(" ")),
      RecordingPlanError.InvalidArea(" ", "label"),
      RecordingPlanError.AreaNames(Vector("a", "a")),
      RecordingPlanError.AreaWarp("a", "Minimum"),
      RecordingPlanError.Cardinality(rec, 10, 9),
      RecordingPlanError.Parameters(DefinitionId.cosine, params)
    ),
    family[RecordingInputError]("RecordingInputError")(
      RecordingInputError.EmptySource(RecordingRef(" ")),
      RecordingInputError.SynchronizationTargetIsSource(clk),
      RecordingInputError.NoSynchronizationMarks(clk, clk2),
      RecordingInputError.Synchronization(syncError),
      RecordingInputError.PlanDisagreement("source", "a", "b"),
      RecordingInputError.BinocularChannels(rec),
      RecordingInputError.Plan(RecordingPlanError.MissingViewing(rec))
    ),
    family[RecordingResultError]("RecordingResultError")(
      RecordingResultError.Plan(RecordingPlanError.MissingViewing(rec)),
      RecordingResultError.Stage("prepared", "samples[2].tMicros", "4000", "4001")
    ),
    family[TemporalResultError[StudyKey]]("TemporalResultError")(
      TemporalResultError.CellCount(8, 7),
      TemporalResultError.CellLayout(3, "recall-encode", "late", "retest-recall", "early"),
      TemporalResultError.Repetition("recall-encode", TemporalStudyError.MissingEpoch(digest)),
      TemporalResultError.Plan(
        Vector(PlanChange("phases", Vector.empty, Vector(Provenance.Param.Text("recall"))))
      ),
      TemporalResultError.Result(StudyResultError.OrphanKey(k1)),
      TemporalResultError.OccupancyKeys(Vector(k1, k2), Vector(k2)),
      TemporalResultError.Boundary(
        k1,
        FixationBoundary.ClipDuration,
        FixationBoundary.FullyContained
      ),
      TemporalResultError.Width(k1, BigInt(300000), BigInt(350000)),
      TemporalResultError.Epoch(k1, Some(digest), None),
      TemporalResultError.Anchor(k1, clk, BigInt(0), clk2, BigInt(10)),
      TemporalResultError.Density(k1, Some(digest), digest2),
      TemporalResultError.Failure(k1, StudyFailure.Estimation(k1, EstimateError.NoMass)),
      TemporalResultError.Cell("recall-encode", "early", TemporalResultError.CellCount(2, 1))
    ),
    family[ReductionError[StudyKey]]("ReductionError")(
      ReductionError.NoSelectedScores(k1),
      ReductionError.AmbiguousKey(k1, Vector(0, 3)),
      ReductionError.FailedScores(k1, 1, 2),
      ReductionError.InsufficientSuccessful(k1, 3, 1, 2),
      ReductionError.MeanFailure(k1, ScoreMeanError.EmptyValues("value"))
    ),
    family[ReconstructionError[StudyKey]]("ReconstructionError")(
      ReconstructionError.PairCounts(1L, 2),
      ReconstructionError.RowCount(2, 3),
      ReconstructionError.Denominator(k1, 1, 2, 3),
      ReconstructionError.ResultKey(k1, k2),
      ReconstructionError.ResultCounts(k1, ReductionError.FailedScores(k1, 1, 1), 2, 0),
      ReconstructionError.DuplicateEntry(k1, Vector(0, 2)),
      ReconstructionError.ReportCount("contributionCount", 3L, 2L),
      ReconstructionError.FailedKeys(Vector(k1), Vector.empty),
      ReconstructionError.ProvenanceConflict("reduction", provenance, provenance2),
      ReconstructionError.ContrastDomain(Vector(k1), Vector(k2)),
      ReconstructionError.ContrastRowShape(
        k1,
        Some(Right(())),
        None,
        Left(ContrastRowError.MissingOperands(k1, Vector(ContrastOperand.Control)))
      ),
      ReconstructionError.ContrastOperand(k1, ContrastOperand.Matched),
      ReconstructionError.Incompatible(NonEmptyVector.of(compatibility)),
      ReconstructionError
        .Orientation(ReductionOrientation.ByLeft, ReductionOrientation.ByRight),
      ReconstructionError.KeyDomain(Vector(k1), Vector.empty),
      ReconstructionError.KeyDenominator(k1, 1, 0, 2, 3),
      ReconstructionError.OutcomeShape(k1, None, Some(ReductionError.NoSelectedScores(k1)))
    ),
    family[ContrastError[StudyKey]]("ContrastError")(
      ContrastError.Incompatible(NonEmptyVector.of(compatibility)),
      ContrastError.EmptyDomain(0, 1),
      ContrastError.IndistinguishableOrdering(k1, k2)
    ),
    family[ContrastRowError[StudyKey]]("ContrastRowError")(
      ContrastRowError.MissingOperands(k1, Vector(ContrastOperand.Matched)),
      ContrastRowError.ReductionFailures(k1, Some(ReductionError.NoSelectedScores(k1)), None),
      ContrastRowError.Arithmetic(k1, DifferenceError.NonFiniteOperands("value", Inf, 0))
    ),
    family[ContrastCompatibilityError]("ContrastCompatibilityError")(
      compatibility,
      ContrastCompatibilityError.Policy(FailurePolicy.RequireAll, successful),
      ContrastCompatibilityError.Scale(EvaluationScale.Count, EvaluationScale.Duration),
      ContrastCompatibilityError.MissingSpecification(ContrastOperand.Matched, info),
      ContrastCompatibilityError.Method(spec, spec2),
      ContrastCompatibilityError
        .Components(ContrastOperand.Control, Vector("a"), Vector("value")),
      ContrastCompatibilityError.SpatialConvention(
        EvaluationGeometry.Independent,
        EvaluationGeometry.onGrid(grid)
      ),
      ContrastCompatibilityError.Frames(frameError),
      ContrastCompatibilityError.Grids(SurfaceError.GridMismatch(gid, otherGrid.id)),
      ContrastCompatibilityError
        .Time(EvaluationTime.OrderFree, EvaluationTime.SharedClock(clk)),
      ContrastCompatibilityError.Clocks(timeError)
    ),
    family[DifferenceError]("DifferenceError")(
      DifferenceError.NonFiniteOperands("value", Inf, 1),
      DifferenceError.NonFiniteDifference("value", Double.MaxValue, -Double.MaxValue)
    ),
    family[ScoreMeanError]("ScoreMeanError")(
      ScoreMeanError.EmptyValues("similarity"),
      ScoreMeanError.NonFiniteValue("value", 2, Inf),
      ScoreMeanError.NonFiniteMean("value", Inf),
      ScoreMeanError.InvalidComparisonValue(
        "value",
        ComparisonValueError.NonFiniteSimilarity(Inf)
      )
    ),
    family[EvaluationSpecError]("EvaluationSpecError")(
      EvaluationSpecError.EmptyField("method", " "),
      EvaluationSpecError.InvalidComponents(Vector("a", "a")),
      EvaluationSpecError.InvalidParameters(Vector("x" -> Provenance.Param.Num(Inf)))
    ),
    family[PairScheduleError]("PairScheduleError")(
      PairScheduleError.InvalidBudget(0, 1L, 2),
      PairScheduleError.InvalidCounts(-1L, 2L),
      PairScheduleError.InvalidQuantum(0),
      PairScheduleError.SourceBudget(10L, 20L, 5),
      PairScheduleError.CandidateBudget(10L, 20L, 100L),
      PairScheduleError.SelectedBudget("relation", 10L, 5)
    ),
    family[ComparisonWorkError]("ComparisonWorkError")(
      ComparisonWorkError.InvalidQuantum(0),
      ComparisonWorkError.InvalidBudget(0L),
      ComparisonWorkError.WorkBudget("cosine", 10L, 5L)
    ),
    family[CompareError]("CompareError")(
      CompareError.Grids(SurfaceError.GridMismatch(gid, otherGrid.id)),
      CompareError.Frames(frameError),
      CompareError.Estimation(EstimateError.NoMass),
      CompareError.ConstantInput("cosine", CompareOperand.Left),
      CompareError.EmptyInput("cosine", CompareOperand.Right, 0),
      CompareError.ZeroNorm("cosine", 0, 1),
      CompareError.RelativeEntropySupport("kl", 3, 0.1, 0),
      CompareError.CostMatrixLimitExceeded("emd", 100, 10),
      CompareError.WorkLimitExceeded("distance correlation", 100, 4950L, 10L),
      CompareError.InvalidSubstitutionCost("dtw", 1, 2, Inf),
      CompareError.InvalidScore("cosine", ComparisonValueError.NonFiniteSimilarity(Inf)),
      CompareError.TooShort("path", 1, 2)
    ),
    family[ComparisonValueError]("ComparisonValueError")(
      ComparisonValueError.NonFiniteMeasureDistance(Inf),
      ComparisonValueError.NegativeMeasureDistance(-1),
      ComparisonValueError.NonFiniteSimilarity(Inf),
      ComparisonValueError.InvalidUnitSimilarity("shape", 2)
    ),
    family[EstimateError]("EstimateError")(
      EstimateError.FrameMismatch(fid, deg),
      EstimateError.NoMass,
      EstimateError.DegenerateBandwidth(0.1, 1.0),
      EstimateError.DegenerateAxisBandwidth(eyes4s.surface.SmoothingAxis.X, 0.1, 1.0),
      EstimateError.KernelSupportOverflow(eyes4s.surface.SmoothingAxis.Y, Double.MaxValue, 1.0),
      EstimateError.Surface(surfaceError)
    ),
    family[SurfaceError]("SurfaceError")(
      SurfaceError.LengthMismatch(4, 3),
      SurfaceError.NegativeWeight(1, -0.5),
      SurfaceError.NegativeValue(2, -1),
      SurfaceError.NonFiniteValue(3, Inf),
      surfaceError,
      SurfaceError.GridMismatch(gid, otherGrid.id),
      SurfaceError.GridIdentityConflict(gid, grid.spec, otherGrid.spec),
      SurfaceError.EmptyCollection("mean")
    ),
    family[GeometryError]("GeometryError")(
      GeometryError.DegenerateBounds(0, 1, 2, 3),
      GeometryError.NonFiniteBounds(0, 2, Inf, 1),
      GeometryError.BoundsExtentOverflow(-Double.MaxValue, 0, Double.MaxValue, 1),
      frameError,
      GeometryError.FrameIdentityConflict(fid, frame.spec, otherFrame.spec),
      GeometryError.NonFiniteLength(Inf, LengthUnit.Millimetres),
      GeometryError.NegativeLength(-1, LengthUnit.Centimetres),
      GeometryError.NonPositivePerspective(0, 1, 2),
      GeometryError.NotAffine("[[1,0,0],[0,1,0],[1,1,1]]"),
      GeometryError.NonFiniteSigma(Inf),
      GeometryError.NonPositiveSigma(0),
      GeometryError.DegenerateGrid(0, 2),
      GeometryError.GridCellCountOverflow(70000, 80000, 5600000000L),
      GeometryError.DegenerateEllipse(0, 1),
      GeometryError.DegeneratePolygon(2),
      GeometryError.NonFiniteRegion("circle"),
      GeometryError.NonFiniteVelocity(Inf),
      GeometryError.NegativeVelocity(-1),
      GeometryError.NonFiniteDistance(Inf),
      GeometryError.NegativeDistance(-2),
      GeometryError.SubframeOutsideParent(deg, 0, 1, 5, 4, fid, frame.spec),
      GeometryError.SubframeIdentity(fid),
      GeometryError.NonPositiveAngularScale(fid, 0),
      GeometryError.NonFiniteTranslation(Inf, 1)
    ),
    family[TimeError]("TimeError")(
      TimeError.ReversedInterval(clk, 10L, 5L),
      TimeError.ReversedWindow(10L, 5L),
      timeError,
      TimeError.WrongSourceClock(clk, clk2),
      TimeError.NonPositiveRate(0),
      TimeError.NonFiniteDrift(clk, clk2, Inf),
      TimeError.NonPositiveClockScale(clk, clk2, -1)
    ),
    family[WindowOccupancyError]("WindowOccupancyError")(
      WindowOccupancyError.Time(TimeError.ReversedWindow(1L, 0L)),
      WindowOccupancyError.Measure(surfaceError),
      WindowOccupancyError.InvalidWidth(span, BigInt(Long.MaxValue) * 2),
      WindowOccupancyError.EmptyCoverageInterval(clk, Vector(span)),
      WindowOccupancyError.OverlappingCoverage(clk, Vector(span, span2)),
      WindowOccupancyError.ObservedTime(span, 7L, 4L),
      WindowOccupancyError.Ledger(1, 2, BigInt(500), 700L, FixationBoundary.FullyContained),
      WindowOccupancyError.MeasureSupport(3, 2)
    ),
    family[SyncEvidenceError]("SyncEvidenceError")(
      SyncEvidenceError.EmptyMarkId(" "),
      SyncEvidenceError.NegativeResidualLimit(Span.micros(-1)),
      SyncEvidenceError.NegativeErrorMagnitude(Span.micros(-2)),
      syncError,
      SyncEvidenceError.TooFewRetainedMarks(clk, clk2, 3, 1, 2),
      SyncEvidenceError.DuplicateMarkId(clk, clk2, "m", 0, 1),
      SyncEvidenceError.NonIncreasingSourceMarks(
        clk,
        clk2,
        1,
        Instant.micros(5),
        Instant.micros(4)
      ),
      SyncEvidenceError.NonIncreasingTargetMarks(
        clk,
        clk2,
        2,
        Instant.micros(7),
        Instant.micros(6)
      ),
      SyncEvidenceError.DegenerateSourceVariance(clk, clk2, 0),
      SyncEvidenceError.NonFiniteFit(clk, clk2, Inf, 0),
      SyncEvidenceError.InvalidFittedSync(clk, clk2, TimeError.NonPositiveRate(0))
    ),
    family[CoreError]("CoreError")(
      CoreError.OfTime(timeError),
      CoreError.OfGeometry(frameError),
      CoreError.OfSurface(surfaceError),
      CoreError.OfRecording(RecordingError.NoSamples),
      CoreError.OfScanpath(ScanpathError.NoFixations),
      coreError,
      CoreError.OfDetectionSupport(supportError)
    ),
    family[RecordingError]("RecordingError")(
      RecordingError.NoSamples,
      RecordingError.NonMonotonic(3, 10L, 5L),
      RecordingError.UnpairedEyes(10, 9, 8),
      RecordingError.NonFinitePosition(
        RecordingChannel.LeftEye,
        2,
        PositionalGazeState.Tracked,
        Inf,
        0
      ),
      RecordingError.TrackedOutsideFrame(RecordingChannel.LeftEye, 3, fid, frame.spec, 9, 8),
      RecordingError.OffScreenInsideFrame(RecordingChannel.RightEye, 4, fid, frame.spec, 1, 2),
      RecordingError.InvalidPupil(RecordingChannel.Monocular(Eye.Left), 5, -1),
      RecordingError.UndeclaredPupilUnit(RecordingChannel.LeftEye, 6, 3),
      RecordingError.NegativeSamplingTolerance(Span.micros(-1)),
      RecordingError.FixedRateMismatch(
        7,
        100L,
        250L,
        Span.micros(150),
        SamplingTolerance.Exact,
        Span.micros(50)
      )
    ),
    family[ScanpathError]("ScanpathError")(
      ScanpathError.NoFixations,
      ScanpathError.OutOfOrder(1, "[0,10)", "[5,15)"),
      ScanpathError.WrongClock(2, "a", "b"),
      ScanpathError.InvalidTransitionSpan(1, TimeError.ReversedWindow(2L, 1L)),
      ScanpathError.InvalidExtent(TimeError.ReversedWindow(2L, 1L)),
      ScanpathError.UnmappableFixation(3, fid, deg, 1, 2)
    ),
    family[EventError]("EventError")(
      EventError.EmptySpan("fixation", span),
      EventError.NonFinitePoint("saccade", "start", span, Inf, 0),
      EventError.InvalidDispersion(-1, DispersionMethod.RmsRadius),
      EventError.NonPositiveSampleCount(0),
      EventError.EmptyPursuit(span),
      EventError.NonFinitePursuitPoint(span, 2, Inf, 1)
    ),
    family[DetectionSupportError]("DetectionSupportError")(
      supportError,
      DetectionSupportError.EventSupportCountMismatch(rec, 3, 2),
      DetectionSupportError.EventClockMismatch(rec, 1, clk, clk2),
      DetectionSupportError.SampleRangeOutsideRecording(rec, 0, range, 3),
      DetectionSupportError.OverlappingSampleRanges(rec, 1, range, range2),
      DetectionSupportError.EventSpanOutsideRecording(rec, 0, span, span2),
      DetectionSupportError.EventSpanHasNoSamples(rec, 0, span),
      DetectionSupportError.EventSampleRangeMismatch(rec, 0, span, range, range2),
      DetectionSupportError.InvalidDerivedSampleRange(rec, 0, span, 5, 2),
      DetectionSupportError.InvalidDerivedFixation(rec, 0, range, sampleError),
      DetectionSupportError.NoUsableSourceSamples(rec, 0, range),
      DetectionSupportError.UnmappableSourceSample(rec, 0, 3, fid, deg, 1, 2),
      DetectionSupportError.UnmappableEventPoint(rec, 0, "start", 1, fid, deg, 1.5, 2.5)
    ),
    family[DetectorDefinitionError]("DetectorDefinitionError")(
      DetectorDefinitionError.Configuration(alg, params)
    ),
    family[DetectionResultError]("DetectionResultError")(
      DetectionResultError.DetectorEmissionFailed(
        rec,
        det,
        2,
        DetectionFailure.EventSummary(coreError)
      ),
      DetectionResultError.EventOutsideRecording(rec, det, 1, span, span2),
      DetectionResultError.SourceSupport(rec, det, supportError),
      DetectionResultError
        .GapPolicyViolation(rec, det, 1, range, Span.micros(10), GapPolicy.Break),
      DetectionResultError.InvalidDerivedRange(rec, det, "gap", 5, 2, supportError)
    ),
    family[DetectionFailure]("DetectionFailure")(
      DetectionFailure.EventSummary(CoreError.OfTime(timeError)),
      DetectionFailure.Kinematics(
        KinematicsError.InvalidSampling(ConfigurationError.InsufficientRegularSamples(3))
      )
    ),
    family[KinematicsError]("KinematicsError")(
      KinematicsError.InvalidSampling(ConfigurationError.NonPositiveWindowHalfWidth(0))
    ),
    family[ConfigurationError]("ConfigurationError")(
      ConfigurationError.NonPositiveWindowHalfWidth(0),
      ConfigurationError.InsufficientRegularSamples(2),
      ConfigurationError.NonPositiveSamplingInterval(3, Span.micros(0)),
      ConfigurationError.IrregularSamplingInterval(4, Span.micros(1000), Span.micros(1500)),
      ConfigurationError.NegativeMissingPadding(Span.micros(-1)),
      ConfigurationError.NegativeInterpolationGap(Span.micros(-2)),
      ConfigurationError.NegativeMaximumMergeGap(Span.micros(-3)),
      ConfigurationError.NonPositiveMinimumEventDuration(Span.micros(0)),
      ConfigurationError.NonPositiveIvtThreshold(0),
      ConfigurationError.InvalidEkThresholds(0, 1),
      ConfigurationError.InvalidEkMultiplier(0),
      ConfigurationError.NonPositiveEkMinimumSamples(0)
    ),
    family[AoiError]("AoiError")(
      AoiError.BlankId(" "),
      AoiError.BlankLabel(aoiA, " "),
      AoiError.BlankAttributeKey(aoiA, " "),
      AoiError.NonPositiveResolution(0, 2),
      AoiError.EmptySet,
      AoiError.DuplicateId(aoiA, 0, 2),
      AoiError.FrameConflict(frameError),
      AoiError.ResolutionGridFailure(fid, 0, 1, GeometryError.DegenerateGrid(0, 1)),
      AoiError.ObservedOverlap(fid, 3, 1, 2, Vector(aoiA, aoiB))
    ),
    family[DescriptorError]("DescriptorError")(
      DescriptorError.InvalidField("sigma", 0, " "),
      DescriptorError.InvalidAlternatives("edges", Vector.empty),
      DescriptorError.InvalidDefault("sigma", " "),
      DescriptorError.DuplicateFields(Vector("a", "a")),
      DescriptorError.InvalidComponent("value", " ", MeasureScale.Bounded(1, 0)),
      DescriptorError.ParameterMismatch(params, Vector.empty),
      DescriptorError.ComponentMismatch(Vector("a"), Vector("value")),
      DescriptorError.MissingMethod(DefinitionId.cosine),
      DescriptorError.MethodIdentity(DefinitionId.cosine, cosine2),
      DescriptorError.ExecutionMismatch(
        ExecutionCapability.BoundedComparison,
        ExecutionCapability.SynchronousWholeOperation
      ),
      DescriptorError.UnexplainedFields(Vector("x"))
    ),
    family[StudyFinding[StudyKey, Px]]("StudyFinding")(
      StudyFinding.UndescribedMethod(DefinitionId.cosine),
      StudyFinding.InconsistentDescriptor(
        DefinitionId.cosine,
        DescriptorError.MissingMethod(DefinitionId.cosine)
      ),
      StudyFinding.MissingArtifact(studyRef),
      StudyFinding.ArtifactMismatch(studyRef, studyRef2),
      StudyFinding.OverBudget(BudgetError.CandidateVisits(2, 2, 1, 3L)),
      StudyFinding.Refused(PlanError.EmptyScales(0)),
      StudyFinding.FrameMismatch(k1, frameError),
      StudyFinding.DuplicateTrial(k1, PairingSide.Focal, Vector(0, 2)),
      StudyFinding.UnmatchedFocal(k1),
      StudyFinding.UncontrolledFocal(k1),
      StudyFinding.OffWindowFixations(k1, tally, OffWindowPolicy.FailTrial),
      StudyFinding.NoFixationInWindow(k1, tally),
      StudyFinding.MatchedCardinality(k1, Vector(k2), MatchedReferences.RequireOne),
      StudyFinding.AmbiguousReferences(Vector(k1, k2), MatchedReferences.SameOccurrence),
      StudyFinding.UnmatchedFocalRefused(k1),
      StudyFinding.MatchItemConflict(Vector(k1, k2))
    ),
    family[RecordingFinding]("RecordingFinding")(
      RecordingFinding.UndescribedMethod(DefinitionId.cosine),
      RecordingFinding.InconsistentDescriptor(
        DefinitionId.cosine,
        DescriptorError.UnexplainedFields(Vector("x"))
      ),
      RecordingFinding.MissingArtifact(recRef),
      RecordingFinding.ArtifactMismatch(recRef, recRef2),
      RecordingFinding.FrameMismatch(rec, frameError),
      RecordingFinding.ClockMismatch(rec, timeError),
      RecordingFinding.MissingViewing(rec),
      RecordingFinding.MissingSynchronization(clk, clk2),
      RecordingFinding.Refused(RecordingPlanError.MissingViewing(rec)),
      RecordingFinding.Synchronization(syncError),
      RecordingFinding.AngularFrame(deg, GeometryError.NonPositivePerspective(0, 1, 1)),
      RecordingFinding.AreaWarp("a", AreaCorner.Maximum),
      RecordingFinding.DetectorDefinition(
        DefinitionId.cosine,
        DetectorDefinitionError.Configuration(alg, params)
      )
    ),
    family[TemporalFinding[StudyKey, Px]]("TemporalFinding")(
      TemporalFinding.MissingArtifact(temporalRef),
      TemporalFinding.ArtifactMismatch(temporalRef, temporalRef2),
      TemporalFinding.Refused(TemporalStudyError.WindowNames(Vector.empty)),
      TemporalFinding.Study(StudyFinding.UnmatchedFocal(k1)),
      TemporalFinding.RepetitionPlan("rep", PlanError.EmptyScales(0)),
      TemporalFinding.Repetition("rep", StudyFinding.UncontrolledFocal(k1)),
      TemporalFinding.MissingEpoch(k1),
      TemporalFinding.CoverageClock(k1, timeError),
      TemporalFinding.WindowResolution(
        k1,
        "early",
        TemporalStudyError.AnchorOverflow("early", Long.MaxValue, 0L, 10L)
      ),
      TemporalFinding.NoObservedCoverage(k1, "early")
    ),
    family[BudgetError]("BudgetError")(
      BudgetError.CandidateVisits(2, 3, 1, 5L),
      BudgetError.Schedule(PairScheduleError.InvalidQuantum(0))
    ),
    family[PreflightError]("PreflightError")(
      PreflightError.ChangedPlan(
        RecipeFamily.FixationStudy,
        Vector(PlanChange("grid", Vector.empty, Vector(Provenance.Param.Text("g"))))
      ),
      PreflightError.ChangedInput(RecipeFamily.TemporalStudy, studyRef, studyRef2),
      PreflightError
        .NotReady(RecipeFamily.FixationStudy, Vector(StudyFinding.MissingArtifact(studyRef))),
      PreflightError.Refused(PlanError.EmptyScales(0))
    ),
    family[AdmissionReason]("AdmissionReason")(
      AdmissionReason.Width(9, 8),
      AdmissionReason.Key("Missing phase column 'phase'."),
      AdmissionReason.Number("x_px", "abc", "a finite number"),
      AdmissionReason.Time("0", "-1", "microseconds", "duration must be positive"),
      AdmissionReason.Position(5, 6, fid),
      AdmissionReason.Event("zero sample count"),
      AdmissionReason.Quarantined(
        Vector(2, 3),
        QuarantineCause.Overlap(1, "[0,10)", "[5,15)")
      )
    ),
    family[QuarantineCause]("QuarantineCause")(
      QuarantineCause.RejectedRecords,
      QuarantineCause.DuplicateOrdinals,
      QuarantineCause.NoFixations,
      QuarantineCause.Overlap(1, "[0,10)", "[5,15)"),
      QuarantineCause.WrongClock(1, "a", "b"),
      QuarantineCause.InvalidTransition(1, "reversed"),
      QuarantineCause.InvalidExtent("reversed"),
      QuarantineCause.UnmappableFixation(2, fid, deg, 1.5, 2.5),
      QuarantineCause.CorrectionConflict(0, 1),
      QuarantineCause.ItemConflict(Vector("beach-042", "dog-077"))
    ),
    family[AdmissionError]("AdmissionError")(
      AdmissionError.NonPositiveRecord(0),
      AdmissionError.RecordOrder(1, 3, 2),
      AdmissionError.NegativeOrdinal(2, -1),
      AdmissionError.DuplicateOrdinal(Vector(2, 3), 0),
      AdmissionError.QuarantineScope(2, Vector(3)),
      AdmissionError.QuarantineAdmitted(2, 3),
      AdmissionError.QuarantinedKeyAdmitted(2, 5),
      AdmissionError.OutcomeMismatch(AdmissionOutcome.Complete, 2),
      AdmissionError.AmbiguousTrial(Vector(0, 1)),
      AdmissionError.UnknownTrial(Vector(7)),
      AdmissionError.UnadmittedTrial(3),
      AdmissionError.FixationCount(1, 4, 3),
      AdmissionError.OutsideFrameRecord(4, OffScreenPolicy.QuarantineTrial),
      AdmissionError.CorrectionConflict(4, 0, 1)
    ),
    family[InspectionError[StudyKey]]("InspectionError")(
      InspectionError.UnknownScale(3, 1),
      InspectionError.UnknownCell("rep", "early"),
      InspectionError.UnknownReference(ResultRef.Estimation(0, k1)),
      InspectionError.DuplicateReference(ResultRef.PairRow(0, StudyDesign.Matched, k1, k2)),
      InspectionError.InvalidPageSize(0, PageSize.maximum),
      InspectionError.Sources(
        LedgerRefusal(
          AdmissionError.UnadmittedTrial(0),
          Vector(k1),
          Vector(SourceLink.Missing(MissingSource.UnknownTrial(k1)))
        )
      ),
      InspectionError.InputMismatch(studyRef, studyRef2),
      InspectionError.Components(DescriptorError.MissingMethod(DefinitionId.cosine)),
      InspectionError.PlanMismatch(
        Vector(PlanChange("grid", Vector.empty, Vector(Provenance.Param.Text("g"))))
      ),
      InspectionError.ReductionMembership(
        ResultRef.InCell("rep", "early", ResultRef.Reduction(0, StudyDesign.Control, k1)),
        2,
        1,
        3,
        4
      ),
      InspectionError.Orientation(1, StudyDesign.Matched, ReductionOrientation.EdgesOnce),
      InspectionError.NoContrast(2)
    )
  )

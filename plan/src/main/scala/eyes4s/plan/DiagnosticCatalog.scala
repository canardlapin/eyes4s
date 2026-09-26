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

/** The code table: every diagnostic family, its case labels in declaration
  * order, and so every stable code. docs/DIAGNOSTICS.md is generated from this
  * table and checked against it; a pinned copy of the rendered codes is
  * checked on the JVM and on Scala.js.
  *
  * Families cover the study, temporal and recording plan errors, preflight
  * findings, admission reasons and ledger refusals, result reconstruction and
  * inspection refusals, and every lower-level error they wrap, so no nested
  * cause is reduced to its message.
  */
object DiagnosticCatalog:
  import DiagnosticFamily.error

  // ---------------------------------------------------------------- plan and run
  val plan: DiagnosticFamily = error("plan")(
    "InvalidDefinition",
    "InvalidArtifact",
    "MissingArtifact",
    "ArtifactMismatch",
    "InvalidPhases",
    "EmptyScales",
    "DuplicateScales",
    "Specification",
    "Schedule",
    "StudyWorkBudget",
    "ChangedPreparedPlan",
    "ComparisonWork",
    "UnsupportedExecution",
    "MissingAngularScale",
    "Geometry",
    "InvalidWindowTally",
    "InvalidOccurrence",
    "BlankKeyField",
    "OccurrenceUnavailable",
    "MatchItemConflict",
    "MatchedCardinality",
    "UnmatchedFocalRefused"
  )
  val studyFailure: DiagnosticFamily =
    error("study-failure")(
      "Frame",
      "Occupancy",
      "Temporal",
      "Estimation",
      "Comparison",
      "OffWindow"
    )
  val studyResult: DiagnosticFamily = error("study-result")(
    "Description",
    "InputMismatch",
    "LayoutMismatch",
    "ScaleCount",
    "ScaleEstimate",
    "MassGrid",
    "MassProvenance",
    "FailureKey",
    "PairFailure",
    "OrphanKey",
    "OrphanPair",
    "SourceIdentity",
    "ContrastAnalyses",
    "ProvenanceInputs",
    "MissingSpecification",
    "SpecificationMethod",
    "SpecificationParameters",
    "Policy",
    "Phase",
    "Reconstruction",
    "Scale",
    "SpecificationTime"
  )
  val temporal: DiagnosticFamily = error("temporal")(
    "Input",
    "Time",
    "Occupancy",
    "InvalidWindow",
    "AnchorOverflow",
    "InvalidRepetition",
    "WindowNames",
    "RepetitionNames",
    "DuplicateEpochs",
    "DuplicateTrials",
    "UnknownEpochs",
    "MissingEpoch",
    "Weighting"
  )
  val recordingPlan: DiagnosticFamily = error("recording-plan")(
    "Input",
    "Geometry",
    "Time",
    "Synchronization",
    "Core",
    "Recording",
    "DetectorDefinition",
    "Detection",
    "Areas",
    "MissingViewing",
    "MissingSynchronization",
    "InvalidSource",
    "InvalidArea",
    "AreaNames",
    "AreaWarp",
    "Cardinality",
    "Parameters"
  )
  val recordingInput: DiagnosticFamily = error("recording-input")(
    "EmptySource",
    "SynchronizationTargetIsSource",
    "NoSynchronizationMarks",
    "Synchronization",
    "PlanDisagreement",
    "BinocularChannels",
    "Plan"
  )
  val recordingResult: DiagnosticFamily = error("recording-result")("Plan", "Stage")
  val temporalResult: DiagnosticFamily  = error("temporal-result")(
    "CellCount",
    "CellLayout",
    "Repetition",
    "Plan",
    "Result",
    "OccupancyKeys",
    "Boundary",
    "Width",
    "Epoch",
    "Anchor",
    "Density",
    "Failure",
    "Cell"
  )

  // ---------------------------------------------------------------- reduction and contrast
  val reduction: DiagnosticFamily = error("reduction")(
    "NoSelectedScores",
    "AmbiguousKey",
    "FailedScores",
    "InsufficientSuccessful",
    "MeanFailure"
  )
  val reconstruction: DiagnosticFamily = error("reconstruction")(
    "PairCounts",
    "RowCount",
    "Denominator",
    "ResultKey",
    "ResultCounts",
    "DuplicateEntry",
    "ReportCount",
    "FailedKeys",
    "ProvenanceConflict",
    "ContrastDomain",
    "ContrastRowShape",
    "ContrastOperand",
    "Incompatible",
    "Orientation",
    "KeyDomain",
    "KeyDenominator",
    "OutcomeShape"
  )
  val contrast: DiagnosticFamily =
    error("contrast")("Incompatible", "EmptyDomain", "IndistinguishableOrdering")
  val contrastRow: DiagnosticFamily =
    error("contrast-row")("MissingOperands", "ReductionFailures", "Arithmetic")
  val contrastCompatibility: DiagnosticFamily = error("contrast-compatibility")(
    "Orientation",
    "Policy",
    "Scale",
    "MissingSpecification",
    "Method",
    "Components",
    "SpatialConvention",
    "Frames",
    "Grids",
    "Time",
    "Clocks"
  )
  val difference: DiagnosticFamily =
    error("difference")("NonFiniteOperands", "NonFiniteDifference")
  val scoreMean: DiagnosticFamily = error("score-mean")(
    "EmptyValues",
    "NonFiniteValue",
    "NonFiniteMean",
    "InvalidComparisonValue"
  )
  val evaluationSpec: DiagnosticFamily =
    error("evaluation-spec")("EmptyField", "InvalidComponents", "InvalidParameters")
  val pairSchedule: DiagnosticFamily = error("pair-schedule")(
    "InvalidBudget",
    "InvalidCounts",
    "InvalidQuantum",
    "SourceBudget",
    "CandidateBudget",
    "SelectedBudget"
  )

  // ---------------------------------------------------------------- comparison and estimation
  val comparisonWork: DiagnosticFamily =
    error("comparison-work")("InvalidQuantum", "InvalidBudget", "WorkBudget")
  val compare: DiagnosticFamily = error("compare")(
    "Grids",
    "Frames",
    "Estimation",
    "ConstantInput",
    "EmptyInput",
    "ZeroNorm",
    "RelativeEntropySupport",
    "CostMatrixLimitExceeded",
    "WorkLimitExceeded",
    "InvalidSubstitutionCost",
    "InvalidScore",
    "TooShort"
  )
  val comparisonValue: DiagnosticFamily = error("comparison-value")(
    "NonFiniteMeasureDistance",
    "NegativeMeasureDistance",
    "NonFiniteSimilarity",
    "InvalidUnitSimilarity"
  )
  val estimate: DiagnosticFamily =
    error("estimate")(
      "FrameMismatch",
      "NoMass",
      "DegenerateBandwidth",
      "Surface",
      "DegenerateAxisBandwidth",
      "KernelSupportOverflow"
    )
  val surface: DiagnosticFamily = error("surface")(
    "LengthMismatch",
    "NegativeWeight",
    "NegativeValue",
    "NonFiniteValue",
    "DegenerateTotal",
    "GridMismatch",
    "GridIdentityConflict",
    "EmptyCollection"
  )
  val geometry: DiagnosticFamily = error("geometry")(
    "DegenerateBounds",
    "NonFiniteBounds",
    "FrameMismatch",
    "FrameIdentityConflict",
    "NonFiniteLength",
    "NegativeLength",
    "NonPositivePerspective",
    "NotAffine",
    "NonFiniteSigma",
    "NonPositiveSigma",
    "DegenerateGrid",
    "GridCellCountOverflow",
    "DegenerateEllipse",
    "DegeneratePolygon",
    "NonFiniteRegion",
    "NonFiniteVelocity",
    "NegativeVelocity",
    "NonFiniteDistance",
    "NegativeDistance",
    "BoundsExtentOverflow",
    "SubframeOutsideParent",
    "SubframeIdentity",
    "NonPositiveAngularScale",
    "NonFiniteTranslation"
  )
  val time: DiagnosticFamily = error("time")(
    "ReversedInterval",
    "ReversedWindow",
    "ClockMismatch",
    "WrongSourceClock",
    "NonPositiveRate",
    "NonFiniteDrift",
    "NonPositiveClockScale"
  )
  val windowOccupancy: DiagnosticFamily = error("window-occupancy")(
    "Time",
    "Measure",
    "InvalidWidth",
    "EmptyCoverageInterval",
    "OverlappingCoverage",
    "ObservedTime",
    "Ledger",
    "MeasureSupport"
  )

  // ---------------------------------------------------------------- recordings and detection
  val syncEvidence: DiagnosticFamily = error("sync-evidence")(
    "EmptyMarkId",
    "NegativeResidualLimit",
    "NegativeErrorMagnitude",
    "TooFewCommonMarks",
    "TooFewRetainedMarks",
    "DuplicateMarkId",
    "NonIncreasingSourceMarks",
    "NonIncreasingTargetMarks",
    "DegenerateSourceVariance",
    "NonFiniteFit",
    "InvalidFittedSync"
  )
  val core: DiagnosticFamily = error("core")(
    "OfTime",
    "OfGeometry",
    "OfSurface",
    "OfRecording",
    "OfScanpath",
    "OfEvent",
    "OfDetectionSupport"
  )
  val recording: DiagnosticFamily = error("recording")(
    "NoSamples",
    "NonMonotonic",
    "UnpairedEyes",
    "NonFinitePosition",
    "TrackedOutsideFrame",
    "OffScreenInsideFrame",
    "InvalidPupil",
    "UndeclaredPupilUnit",
    "NegativeSamplingTolerance",
    "FixedRateMismatch"
  )
  val scanpath: DiagnosticFamily = error("scanpath")(
    "NoFixations",
    "OutOfOrder",
    "WrongClock",
    "InvalidTransitionSpan",
    "InvalidExtent",
    "UnmappableFixation"
  )
  val event: DiagnosticFamily = error("event")(
    "EmptySpan",
    "NonFinitePoint",
    "InvalidDispersion",
    "NonPositiveSampleCount",
    "EmptyPursuit",
    "NonFinitePursuitPoint"
  )
  val detectionSupport: DiagnosticFamily = error("detection-support")(
    "InvalidSampleRange",
    "EventSupportCountMismatch",
    "EventClockMismatch",
    "SampleRangeOutsideRecording",
    "OverlappingSampleRanges",
    "EventSpanOutsideRecording",
    "EventSpanHasNoSamples",
    "EventSampleRangeMismatch",
    "InvalidDerivedSampleRange",
    "InvalidDerivedFixation",
    "NoUsableSourceSamples",
    "UnmappableSourceSample",
    "UnmappableEventPoint"
  )
  val detectorDefinition: DiagnosticFamily = error("detector-definition")("Configuration")
  val detectionResult: DiagnosticFamily    = error("detection-result")(
    "DetectorEmissionFailed",
    "EventOutsideRecording",
    "SourceSupport",
    "GapPolicyViolation",
    "InvalidDerivedRange"
  )
  val detectionFailure: DiagnosticFamily =
    error("detection-failure")("EventSummary", "Kinematics")
  val kinematics: DiagnosticFamily    = error("kinematics")("InvalidSampling")
  val configuration: DiagnosticFamily = error("configuration")(
    "NonPositiveWindowHalfWidth",
    "InsufficientRegularSamples",
    "NonPositiveSamplingInterval",
    "IrregularSamplingInterval",
    "NegativeMissingPadding",
    "NegativeInterpolationGap",
    "NegativeMaximumMergeGap",
    "NonPositiveMinimumEventDuration",
    "NonPositiveIvtThreshold",
    "InvalidEkThresholds",
    "InvalidEkMultiplier",
    "NonPositiveEkMinimumSamples"
  )
  val aoi: DiagnosticFamily = error("aoi")(
    "BlankId",
    "BlankLabel",
    "BlankAttributeKey",
    "NonPositiveResolution",
    "EmptySet",
    "DuplicateId",
    "FrameConflict",
    "ResolutionGridFailure",
    "ObservedOverlap"
  )

  // ---------------------------------------------------------------- descriptors and preflight
  val descriptor: DiagnosticFamily = error("descriptor")(
    "InvalidField",
    "InvalidAlternatives",
    "InvalidDefault",
    "DuplicateFields",
    "InvalidComponent",
    "ParameterMismatch",
    "ComponentMismatch",
    "MissingMethod",
    "MethodIdentity",
    "ExecutionMismatch",
    "UnexplainedFields"
  )

  /** Finding severity is the finding's own blocker/warning classification. */
  val studyFinding: DiagnosticFamily = error("study-finding")(
    "UndescribedMethod",
    "InconsistentDescriptor",
    "MissingArtifact",
    "ArtifactMismatch",
    "OverBudget",
    "Refused",
    "FrameMismatch",
    "DuplicateTrial",
    "UnmatchedFocal",
    "UncontrolledFocal",
    "OffWindowFixations",
    "NoFixationInWindow",
    "MatchedCardinality",
    "AmbiguousReferences",
    "UnmatchedFocalRefused",
    "MatchItemConflict"
  )
  val recordingFinding: DiagnosticFamily = error("recording-finding")(
    "UndescribedMethod",
    "InconsistentDescriptor",
    "MissingArtifact",
    "ArtifactMismatch",
    "FrameMismatch",
    "ClockMismatch",
    "MissingViewing",
    "MissingSynchronization",
    "Refused",
    "Synchronization",
    "AngularFrame",
    "AreaWarp",
    "DetectorDefinition"
  )
  val temporalFinding: DiagnosticFamily = error("temporal-finding")(
    "MissingArtifact",
    "ArtifactMismatch",
    "Refused",
    "Study",
    "RepetitionPlan",
    "Repetition",
    "MissingEpoch",
    "CoverageClock",
    "WindowResolution",
    "NoObservedCoverage"
  )
  val budget: DiagnosticFamily    = error("budget")("CandidateVisits", "Schedule")
  val preflight: DiagnosticFamily =
    error("preflight")("ChangedPlan", "ChangedInput", "NotReady", "Refused")

  // ---------------------------------------------------------------- admission and inspection
  val admissionReason: DiagnosticFamily = error("admission-reason")(
    "Width",
    "Key",
    "Number",
    "Time",
    "Position",
    "Event",
    "Quarantined"
  )
  val quarantine: DiagnosticFamily = error("quarantine")(
    "RejectedRecords",
    "DuplicateOrdinals",
    "NoFixations",
    "Overlap",
    "WrongClock",
    "InvalidTransition",
    "InvalidExtent",
    "UnmappableFixation",
    "CorrectionConflict",
    "ItemConflict",
    "OccurrenceConflict",
    "NotInInventory",
    "InventoryItemConflict"
  )
  val inventory: DiagnosticFamily = error("inventory")(
    "Width",
    "Field",
    "Conflict",
    "DuplicateAttribute",
    "DuplicateTrial",
    "RecordOrder",
    "SharedRecord",
    "AbsentMismatch",
    "AttributeRecord",
    "UnknownRecord",
    "ForeignRecord",
    "UnclaimedRecord",
    "DispositionMismatch",
    "ItemMismatch",
    "NoTrialProjection",
    "RecordItems",
    "AttributeNames",
    "AttributeKindMismatch"
  )
  val admission: DiagnosticFamily = error("admission")(
    "NonPositiveRecord",
    "RecordOrder",
    "NegativeOrdinal",
    "DuplicateOrdinal",
    "QuarantineScope",
    "QuarantineAdmitted",
    "QuarantinedKeyAdmitted",
    "OutcomeMismatch",
    "AmbiguousTrial",
    "UnknownTrial",
    "UnadmittedTrial",
    "FixationCount",
    "OutsideFrameRecord",
    "CorrectionConflict",
    "Inventory",
    "UninventoriedCause"
  )
  val inspection: DiagnosticFamily = error("inspection")(
    "UnknownScale",
    "UnknownCell",
    "UnknownReference",
    "DuplicateReference",
    "InvalidPageSize",
    "Sources",
    "InputMismatch",
    "Components",
    "PlanMismatch",
    "ReductionMembership",
    "Orientation",
    "NoContrast"
  )

  // ---------------------------------------------------------------- appended by CR5
  // Every other public error enum of the pure modules. Appended after the
  // families above, whose codes are unchanged.
  val timeline: DiagnosticFamily = error("timeline")(
    "BlankClock"
  )
  val moving: DiagnosticFamily = error("moving")(
    "NoSegments",
    "OverlappingSegments",
    "Clock",
    "Frame"
  )
  val timeQuantity: DiagnosticFamily = error("time-quantity")(
    "NegativeNonNegativeSpan",
    "NonPositiveSpan",
    "NegativeNonNegativeLong",
    "NonNegativeLongOverflow"
  )
  val occupancy: DiagnosticFamily = error("occupancy")(
    "Measure"
  )
  val replication: DiagnosticFamily = error("replication")(
    "Period",
    "Duration",
    "MaximumRows",
    "Cardinality"
  )
  val temporalSupport: DiagnosticFamily = error("temporal-support")(
    "NonPositiveFixedPeriod",
    "NegativeMaximumGap",
    "NegativeEdgeSupport"
  )
  val algorithmMetadata: DiagnosticFamily = error("algorithm-metadata")(
    "EmptyAlgorithmId",
    "InvalidDoi",
    "InvalidVersion",
    "InvalidAuthors",
    "InvalidYear",
    "EmptyCitationTitle",
    "EmptyAlgorithmName",
    "NoCitations",
    "NoAssumptions",
    "NoReferences"
  )
  val ekEstimation: DiagnosticFamily = error("ek-estimation")(
    "InvalidSampling",
    "InsufficientVelocities",
    "DegenerateVelocitySpread"
  )
  val merge: DiagnosticFamily = error("merge")(
    "Interval",
    "Range",
    "Event",
    "SourceSupport"
  )
  val densityLookup: DiagnosticFamily = error("density-lookup")(
    "Arithmetic",
    "Surface"
  )
  val densityPointFailure: DiagnosticFamily = error("density-point-failure")(
    "NonFinitePoint",
    "OutsideGrid",
    "Trajectory"
  )
  val iqrBandwidth: DiagnosticFamily = error("iqr-bandwidth")(
    "TooFewPoints",
    "Sigma"
  )
  val smootherCard: DiagnosticFamily = error("smoother-card")(
    "EmptyText",
    "InvalidParameters"
  )
  val comparisonConfiguration: DiagnosticFamily = error("comparison-configuration")(
    "InvalidProjectionDirections",
    "InvalidRegularisation",
    "InvalidIterationCount",
    "InvalidCellLimit",
    "InvalidProbabilityFloor",
    "InvalidGapCost"
  )
  val crqaParameter: DiagnosticFamily = error("crqa-parameter")(
    "NonPositiveEmbeddingDimension",
    "NonPositiveEmbeddingDelay",
    "NonPositiveLineMinimum",
    "InvalidTargetRecurrenceRate"
  )
  val crqa: DiagnosticFamily = error("crqa")(
    "Geometry",
    "TooShort",
    "MatrixTooLarge",
    "NonFinitePosition",
    "NonFiniteEmbeddedDistance",
    "SelectedRadius"
  )
  val fixationComparison: DiagnosticFamily = error("fixation-comparison")(
    "Parameter",
    "Frames",
    "Clock",
    "EmptyQueries",
    "MissingSupport",
    "MatrixSize",
    "Numerical",
    "NonConvergence",
    "SolverNumerical",
    "WorkLimit",
    "Score"
  )
  val overlapFailure: DiagnosticFamily = error("overlap-failure")(
    "Missing",
    "NonfiniteDistance"
  )
  val mapScaleFailure: DiagnosticFamily = error("map-scale-failure")(
    "MissingLeft",
    "MissingRight",
    "Comparison"
  )
  val mapComparison: DiagnosticFamily = error("map-comparison")(
    "UnsupportedMethod",
    "Scales",
    "Grid",
    "Incomplete",
    "Score"
  )
  val scanpathComponent: DiagnosticFamily = error("scanpath-component")(
    "Comparison",
    "Unavailable"
  )
  val decomposition: DiagnosticFamily = error("decomposition")(
    "PredictorName",
    "PredictorKeys",
    "Geometry",
    "Solve",
    "Numerical"
  )
  val pairing: DiagnosticFamily = error("pairing")(
    "NonPositiveLimit"
  )
  val session: DiagnosticFamily = error("session")(
    "InvalidKey",
    "DuplicateKey",
    "MissingKey",
    "Frame",
    "GridIdentity"
  )
  val reductionPolicy: DiagnosticFamily = error("reduction-policy")(
    "NonPositiveMinimumSuccessful"
  )
  val workQuanta: DiagnosticFamily = error("work-quanta")(
    "InvalidSampleQuantum"
  )
  val evaluationWork: DiagnosticFamily = error("evaluation-work")(
    "Schedule",
    "Comparison"
  )
  val repetitionMean: DiagnosticFamily = error("repetition-mean")(
    "Incomplete",
    "Mean"
  )
  val learnedTemplate: DiagnosticFamily = error("learned-template")(
    "Observation",
    "Definition",
    "DuplicateKey",
    "Split",
    "Geometry",
    "Feature",
    "Fit",
    "Identity",
    "Numerical"
  )
  val leastSquares: DiagnosticFamily = error("least-squares")(
    "Shape",
    "NonFinite",
    "RankTolerance",
    "RankDeficient",
    "ColumnArithmetic",
    "RowArithmetic"
  )
  val templateFit: DiagnosticFamily = error("template-fit")(
    "Fit",
    "Basis",
    "Observation",
    "Width",
    "DuplicateKey",
    "Split",
    "Receipt",
    "Numerical",
    "Evaluation"
  )
  val rng: DiagnosticFamily = error("rng")(
    "NonPositiveBound"
  )
  val epoch: DiagnosticFamily = error("epoch")(
    "Clock",
    "AnchorMatches",
    "AnchorOverflow",
    "DurationOverflow",
    "NonDivisible",
    "BinLimit"
  )
  val pointSampling: DiagnosticFamily = error("point-sampling")(
    "Boundaries",
    "Clock",
    "Frames",
    "MissingTemplate",
    "AmbiguousTemplate",
    "DuplicateSource",
    "Density",
    "Point",
    "Insufficient",
    "NonFinite"
  )
  val recipeParameter: DiagnosticFamily = error("recipe-parameter")(
    "Geometry",
    "Time",
    "Configuration",
    "Temporal",
    "Reduction",
    "Synchronization",
    "Recording"
  )
  val repetitionPlan: DiagnosticFamily = error("repetition-plan")(
    "ProjectionIds",
    "DuplicateLayout",
    "MissingLayout",
    "Rules",
    "OverlappingRelations",
    "Grid",
    "Specification"
  )
  val diagnosticCode: DiagnosticFamily = error("diagnostic-code")(
    "InvalidFamily",
    "InvalidName"
  )

  /** Every family, grouped as documented. */
  val families: Vector[DiagnosticFamily] = Vector(
    plan,
    studyFailure,
    studyResult,
    temporal,
    recordingPlan,
    recordingInput,
    recordingResult,
    temporalResult,
    reduction,
    reconstruction,
    contrast,
    contrastRow,
    contrastCompatibility,
    difference,
    scoreMean,
    evaluationSpec,
    pairSchedule,
    comparisonWork,
    compare,
    comparisonValue,
    estimate,
    surface,
    geometry,
    time,
    windowOccupancy,
    syncEvidence,
    core,
    recording,
    scanpath,
    event,
    detectionSupport,
    detectorDefinition,
    detectionResult,
    detectionFailure,
    kinematics,
    configuration,
    aoi,
    descriptor,
    studyFinding,
    recordingFinding,
    temporalFinding,
    budget,
    preflight,
    admissionReason,
    quarantine,
    inventory,
    admission,
    inspection,
    timeline,
    moving,
    timeQuantity,
    occupancy,
    replication,
    temporalSupport,
    algorithmMetadata,
    ekEstimation,
    merge,
    densityLookup,
    densityPointFailure,
    iqrBandwidth,
    smootherCard,
    comparisonConfiguration,
    crqaParameter,
    crqa,
    fixationComparison,
    overlapFailure,
    mapScaleFailure,
    mapComparison,
    scanpathComponent,
    decomposition,
    pairing,
    session,
    reductionPolicy,
    workQuanta,
    evaluationWork,
    repetitionMean,
    learnedTemplate,
    leastSquares,
    templateFit,
    rng,
    epoch,
    pointSampling,
    recipeParameter,
    repetitionPlan,
    diagnosticCode
  )

  /** Every stable code, in catalog order. */
  val codes: Vector[DiagnosticCode] = families.flatMap(_.codes)

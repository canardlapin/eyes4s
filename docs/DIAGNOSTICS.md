# Diagnostics, source links and result inspection

`eyes4s-plan` gives an application three things it needs to explain a study
without parsing text: a typed `Diagnostic` for every error, finding and
admission reason; source links from trials and fixations back to the records
that supplied them; and keyed, pageable views of a completed result. The
library owns identity, operands, links and scientific meaning. The application
owns wording, localisation, layout, graphics and selection appearance.

Everything here is pure and cross-compiled for the JVM and Scala.js. No
scientific value is recomputed: projections read the result, the input and the
admission ledger. The one derived view, a reduction's member pairs, regroups
the stored pair rows and is checked against the stored denominators.

## Diagnostics

`Diagnostic.of(error)` projects any cataloged error through its `Diagnose`
instance; `Diagnostics.plan`, `Diagnostics.failure`, `Diagnostics.reduction`
and the other `Diagnostics` methods do the same by name, and also accept
errors whose key type is a wildcard, such as the `ReconstructionError[?]`
inside `CodecError.Reconstruction`. A diagnostic carries:

- `code`: the stable identity, rendered `family.case`, for example
  `reduction.failed-scores`. A consumer localises from the code and the
  operands, never from `message`.
- `severity`: `Error` when the operation or the named item produced no value,
  `Warning` for preflight warnings only.
- `subject`: the failing object as a path of `Locus` values from the coarsest to
  the finest: `Repetition` and `Window` for a temporal cell, `Scale`, `Design`,
  then `Trial`, `Pair`, `Trials`, `Record`, `Records` or `Fixation`, and for
  recordings `Recording`, `Event`, `Sample`, `Samples` or `Area`. A wrapper (a
  scale around a reconstruction error, a repetition around a finding, a refusal
  around a plan error, a core error around a recording error) keeps every locus
  of the error it wraps, so the innermost failing object is named. The two
  exceptions are `study-result.failure-key` and `study-result.pair-failure`:
  they report a stored failure that names the wrong trial, and their subject
  is the row it was stored in.
- `operands`: every field of the underlying case, under the case's own field
  name and in declaration order. Values are typed: `Key`, `Integer`, `Real`
  (an `ExactDouble`, compared by bit pattern so a NaN operand equals itself),
  `Micros`, `Token` (library vocabulary such as a policy or an enum case),
  `Name` (an identifier from the plan or its data), `Definition`, `Artifact`,
  `Fields` for structured values such as intervals, frame and grid specs,
  measure scales and evaluation specifications, `Cause` and `Causes` for nested
  errors, and `Absent` for an absent option. `Text` marks free text produced
  outside the catalog, such as a key reader's reason or a rendered span; show
  it verbatim.
- `sources`: source links, filled in by `StudySources.link` (below).
- `category` and `remedy`: the preflight classification, for findings only.
- `message`: a default English rendering. It is not an identity, and numbers
  may render differently on Scala.js.

Nested errors never collapse to their messages. `StudyFailure.Frame(key, e)`
has the subject `Trial(key)` and the operand `underlying`, a `Cause` whose own
code is `geometry.frame-mismatch` and whose operands are the two frame names.

### Stability

`DiagnosticCatalog` is the code table: each family lists its enum's cases in
declaration order, and a case's code is the family name and its kebab-case
label. `DiagnosticCatalogSuite` compares every list with the compiler's own
case list and samples every case of every family through its public
`Diagnose` instance. For each sample it checks that the operands are the
case's fields in order, that each operand carries its field's value (a
structured field must have its structured projection, and a nested error must
carry the code of the family that owns its case), that a wrapper keeps the
wrapped subject, and that same-typed fields of a sample differ, so a swapped
operand is detected. It pins the rendered code list by count and portable
digest on both platforms. A new, removed, renamed or reordered case therefore
fails the tests until the catalog, the pinned digest and this document are
updated. Renaming a case changes its code; treat that as a breaking change.

The catalog covers the plan, study-failure, study-result, temporal,
recording-plan and recording-input errors; reduction, reconstruction, contrast
and contrast-row errors; every preflight finding family, `BudgetError` and
`PreflightError`; admission reasons, quarantine causes and ledger refusals;
inspection refusals; and every lower-level error those wrap, from geometry,
time and surface errors to detection support and AOI errors.

Not cataloged: `CodecError` and the io import errors (`FixationImportError`,
`TidyCsvError` and the EyeLink errors). They remain typed values, and the
`CodecError` cases that wrap a cataloged error (`Definition`, `Admission`,
`Recording`, `Support`, `Synchronization`, `Input`, `Temporal`,
`Reconstruction`, `Result`) project that error directly. `PreflightError` and
`preflightFinding` project to `Diagnostic[Any]` because `PreflightError`
carries its blockers without their key type.

## Source links

`StudySources.of(input, ledger)` indexes the admission ledger of a ledgered
import after `AdmissionLedger.checkAgainst` confirms that it describes exactly
this input; `StudySources.unledgered(input)` indexes an input without one.
Both take the key type's `KeyDigest`.

| Question | Call | Answer |
|---|---|---|
| Which records supplied this trial? | `trial(key)` | `TrialSources`: admitted fixations in scanpath order, records rejected under the key, recording support |
| Which record supplied fixation `i`? | `fixation(key, i)` | `FixationSource(key, i, record, ordinal)` |
| Which samples support fixation `i`? | `samples(key, i)` | `SourceLink.Samples(recording, artifact, from, until)` |
| Every link for fixation `i` | `fixationLinks(key, i)` | the fixation, its record and its samples, or one explicit missing link |
| What is record `n`? | `record(n)`, `trialOf(n)` | the ledger entry and its trial key |
| Where does this diagnostic point? | `link(diagnostic)` | the diagnostic with the links of its innermost object |
| Why were records excluded? | `rejections` | one linked diagnostic per rejected record |

A fixation's position follows ordinal rank, as the ledger defines, never
record order or row position. Record numbers are logical CSV records with the
header as record 1; a quoted field that spans lines makes physical line
numbers differ, so an application decodes the source to jump to a record.
`SourceRef.records` is the artifact identity; its label is for display. A
sample link's `artifact` is the content identity of the recording the
scanpath carries, which the study input encodes with it; a detected event's
link names the recording plan's input artifact.

Where evidence is absent, the answer is an explicit `MissingSource`, never a
guessed link: `NoLedger`, `UnknownTrial`, `NotAdmitted` (a quarantined trial,
with its rejected records), `FixationOutOfRange`, `NotSourceSupported`,
`AmbiguousTrial` (the input repeats the full key), `UnresolvedDigest` and
`UnknownInputTrial`. An error that kept only an input position
(`admission.unadmitted-trial`) or a key digest (`temporal.missing-epoch`)
resolves to its trial when exactly one input trial has it.

## Result inspection

`ResultInspection.study(plan, result, input, ledger)` builds a
`StudyInspection` and refuses a result computed by another plan;
`study(result, sources, schema)` takes the sources and a `ScoreSchema`
directly. Both refuse sources for another input, a schema whose component ids
differ from those the result's evaluation specification stores, and a
reduction whose regrouped members disagree with its stored `selected` or
`contributing` count. `ResultInspection.temporal(result, sources, schema)`
and `ResultInspection.recording(plan, analysis)` cover the other shipped
result families.

Every item has a typed `ResultRef`: `Estimation(scale, key)`,
`PairRow(scale, design, focal, reference)`, `Reduction(scale, design, key)`,
`ContrastRow(scale, key)`, `Occupancy(repetition, window, key)`,
`Event(from, until)`, and `InCell(repetition, window, ref)` for an item of a
temporal cell, so a reference from one cell never resolves in another. A
`Listing` holds one kind of item in the result's own order. `get(ref)`
resolves an item, `first(size)` and `page(from, size)` return a `Page` of at
most `size` entries (`PageSize` is 1 to 4096) with the reference the next page
starts at, and a reference that addresses more than one item is refused when
the inspection is built. Neither a row number, a display label nor a digest
addresses an item.

Per scale, `ScaleInspection` offers:

- `estimation`: each trial's outcomes, a `DensityView` or a located failure.
  More than one outcome means the input repeats the full key; pairing excluded
  every occurrence. `DensityView.cells` returns a fresh copy of the cell masses.
- `pairs(design)`: each directed pair with both keys and its score or failure.
- `reductions(design)`: each focal key's score or failure, the stored
  `selected`, `successful`, `failed` and `contributing` counts, and `members`,
  the selected pairs with their membership: `Contributing`, `FailedPair` (the
  pair's evaluation failed) or `Withheld` (the pair succeeded but the key's
  reduction failed). `contributors` are the contributing observations; the
  aggregate target stays `key`.
- `pairing(design)` and `report(design)`: the stored pairing and reduction
  reports, including unmatched keys and duplicate-key ambiguities.
- `contrast`: `Failed` with the scale's refusal, or `Rows` whose entries refer
  to their two reductions. Asking a failed scale for a row is
  `inspection.no-contrast`.

`ScoreSchema.study(plan)` reads the method descriptor's score components, which
must name the method's contrast components in order. Each score and difference
view then lists its components with units, range, direction and value, read
from the typed value by the descriptor. An undescribed method has no
components: missing, not zero.

`failures` lists every failure the result records, located under its scale and
design and linked to its sources. A drill-down from a contrast row to CSV
records reads:

```scala
val inspection = ResultInspection.study(plan, result, input, Some(ledger))
for
  view      <- inspection
  row       <- view.contrastRow(ResultRef.ContrastRow(0, focal))
  reduction <- view.reduction(row.matched.get)
  pair      <- view.pair(reduction.members.head.pair)
yield view.sources.fixation(pair.reference, 0).map(_.record)
```

`TemporalInspection.cell(repetition, window)` returns each trial's window
occupancy (the fixation indices it retains link to their records through
`fixation`) and a `StudyInspection` of the cell whose items are `InCell`
references; `cellOf(ref)` finds the cell a reference belongs to.
`RecordingInspection.events` addresses detected events by their source sample
range, each with a `Samples` link into the plan's input recording, whose
sample count and order preprocessing keeps.

## Evidence

- `plan/src/test/scala/eyes4s/plan/DiagnosticCatalogSuite.scala`: catalog
  totality against the compiler's case lists, per-case samples, field
  alignment with mutant checks, preserved nested subjects, exact non-finite
  operands, unique and pinned codes on both platforms.
- `plan/src/test/scala/eyes4s/plan/ResultInspectionSuite.scala`: drill-down,
  membership under both failure policies, colliding display text and repeated
  keys, reordered input, a two-component custom score and a mislabelled
  schema, scale, trial and window failures, cell-scoped references, missing
  locators, digest and position resolution, rejected records, buffer copies,
  paging at several sizes, agreement with the typed result, and recording
  events linked to the plan's input.
- `codec/src/test/scala/eyes4s/codec/InspectionArchiveSuite.scala`: the pinned
  study-result-v1 and study-input-v1 drill down to CSV record numbers; the
  pinned admission ledger names its rejected records; a decoded archive
  inspects exactly as its original, reports included; source-supported
  fixations link to their samples; a tampered archive is refused with a coded
  diagnostic.
- `io/src/test/scala/eyes4s/io/InspectionSourceSuite.scala`: an imported CSV
  with multi-line fields drills down to its logical records, rejected rows
  become coded diagnostics, and `ContrastCsv` (one and two components) and
  `TemporalContrastCsv` carry the projection's values, denominators, statuses
  and reasons.
- `tools/study-consumer`: a packaged consumer drills down from a result row to
  a CSV record number on the JVM and Scala.js.

## Code table

Generated from `DiagnosticCatalog` and checked by
`plan/.jvm/src/test/scala/eyes4s/plan/DiagnosticsDocJvmSuite.scala`. Set
`EYES4S_WRITE_DIAGNOSTICS_DOC=1` and run that suite to regenerate it; the run
fails after rewriting, so review the change and run it again.

<!-- BEGIN GENERATED DIAGNOSTIC CODES -->

### `plan` — `PlanError`

| Code | Case | Operands |
|---|---|---|
| `plan.invalid-definition` | `InvalidDefinition` | `name`, `version` |
| `plan.invalid-artifact` | `InvalidArtifact` | `digest` |
| `plan.missing-artifact` | `MissingArtifact` | `digest` |
| `plan.artifact-mismatch` | `ArtifactMismatch` | `expected`, `actual` |
| `plan.invalid-phases` | `InvalidPhases` | `focal`, `reference` |
| `plan.empty-scales` | `EmptyScales` | `count` |
| `plan.duplicate-scales` | `DuplicateScales` | `names` |
| `plan.specification` | `Specification` | `underlying` |
| `plan.schedule` | `Schedule` | `underlying` |
| `plan.study-work-budget` | `StudyWorkBudget` | `focalTrials`, `referenceTrials`, `scales`, `maximumCandidateVisits` |
| `plan.changed-prepared-plan` | `ChangedPreparedPlan` | `method`, `layout` |
| `plan.comparison-work` | `ComparisonWork` | `underlying` |
| `plan.unsupported-execution` | `UnsupportedExecution` | `method`, `capability` |

### `study-failure` — `StudyFailure`

| Code | Case | Operands |
|---|---|---|
| `study-failure.frame` | `Frame` | `key`, `underlying` |
| `study-failure.occupancy` | `Occupancy` | `key`, `underlying` |
| `study-failure.temporal` | `Temporal` | `key`, `underlying` |
| `study-failure.estimation` | `Estimation` | `key`, `underlying` |
| `study-failure.comparison` | `Comparison` | `left`, `right`, `underlying` |

### `study-result` — `StudyResultError`

| Code | Case | Operands |
|---|---|---|
| `study-result.description` | `Description` | `field`, `found` |
| `study-result.input-mismatch` | `InputMismatch` | `reference`, `described` |
| `study-result.layout-mismatch` | `LayoutMismatch` | `expected`, `described` |
| `study-result.scale-count` | `ScaleCount` | `declared`, `found` |
| `study-result.scale-estimate` | `ScaleEstimate` | `declared`, `found` |
| `study-result.mass-grid` | `MassGrid` | `key`, `declared`, `found` |
| `study-result.mass-provenance` | `MassProvenance` | `key`, `expected`, `found` |
| `study-result.failure-key` | `FailureKey` | `key`, `failure` |
| `study-result.pair-failure` | `PairFailure` | `left`, `right`, `failure` |
| `study-result.orphan-key` | `OrphanKey` | `key` |
| `study-result.orphan-pair` | `OrphanPair` | `left`, `right` |
| `study-result.source-identity` | `SourceIdentity` | `design` |
| `study-result.contrast-analyses` | `ContrastAnalyses` | `design` |
| `study-result.provenance-inputs` | `ProvenanceInputs` | `design`, `declared`, `expected` |
| `study-result.missing-specification` | `MissingSpecification` | `design`, `evaluation` |
| `study-result.specification-method` | `SpecificationMethod` | `design`, `expected`, `method`, `revision` |
| `study-result.specification-parameters` | `SpecificationParameters` | `design`, `expected`, `found` |
| `study-result.policy` | `Policy` | `design`, `declared`, `found` |
| `study-result.phase` | `Phase` | `key`, `expected`, `found` |
| `study-result.reconstruction` | `Reconstruction` | `underlying` |
| `study-result.scale` | `Scale` | `index`, `underlying` |

### `temporal` — `TemporalStudyError`

| Code | Case | Operands |
|---|---|---|
| `temporal.input` | `Input` | `underlying` |
| `temporal.time` | `Time` | `underlying` |
| `temporal.occupancy` | `Occupancy` | `underlying` |
| `temporal.invalid-window` | `InvalidWindow` | `name`, `fromMicros`, `untilMicros` |
| `temporal.anchor-overflow` | `AnchorOverflow` | `window`, `anchor`, `from`, `until` |
| `temporal.invalid-repetition` | `InvalidRepetition` | `name`, `focal`, `reference` |
| `temporal.window-names` | `WindowNames` | `names` |
| `temporal.repetition-names` | `RepetitionNames` | `names` |
| `temporal.duplicate-epochs` | `DuplicateEpochs` | `keyDigests` |
| `temporal.duplicate-trials` | `DuplicateTrials` | `keyDigests` |
| `temporal.unknown-epochs` | `UnknownEpochs` | `keyDigests` |
| `temporal.missing-epoch` | `MissingEpoch` | `keyDigest` |
| `temporal.weighting` | `Weighting` | `weight` |

### `recording-plan` — `RecordingPlanError`

| Code | Case | Operands |
|---|---|---|
| `recording-plan.input` | `Input` | `underlying` |
| `recording-plan.geometry` | `Geometry` | `underlying` |
| `recording-plan.time` | `Time` | `underlying` |
| `recording-plan.synchronization` | `Synchronization` | `underlying` |
| `recording-plan.core` | `Core` | `stage`, `underlying` |
| `recording-plan.recording` | `Recording` | `stage`, `underlying` |
| `recording-plan.detector-definition` | `DetectorDefinition` | `underlying` |
| `recording-plan.detection` | `Detection` | `underlying` |
| `recording-plan.areas` | `Areas` | `underlying` |
| `recording-plan.missing-viewing` | `MissingViewing` | `source` |
| `recording-plan.missing-synchronization` | `MissingSynchronization` | `source`, `target` |
| `recording-plan.invalid-source` | `InvalidSource` | `source` |
| `recording-plan.invalid-area` | `InvalidArea` | `id`, `label` |
| `recording-plan.area-names` | `AreaNames` | `ids` |
| `recording-plan.area-warp` | `AreaWarp` | `id`, `corner` |
| `recording-plan.cardinality` | `Cardinality` | `source`, `before`, `after` |
| `recording-plan.parameters` | `Parameters` | `method`, `values` |

### `recording-input` — `RecordingInputError`

| Code | Case | Operands |
|---|---|---|
| `recording-input.empty-source` | `EmptySource` | `source` |
| `recording-input.synchronization-target-is-source` | `SynchronizationTargetIsSource` | `clock` |
| `recording-input.no-synchronization-marks` | `NoSynchronizationMarks` | `source`, `target` |
| `recording-input.synchronization` | `Synchronization` | `underlying` |
| `recording-input.plan-disagreement` | `PlanDisagreement` | `field`, `plan`, `evidence` |
| `recording-input.binocular-channels` | `BinocularChannels` | `source` |
| `recording-input.plan` | `Plan` | `underlying` |

### `reduction` — `ReductionError`

| Code | Case | Operands |
|---|---|---|
| `reduction.no-selected-scores` | `NoSelectedScores` | `key` |
| `reduction.ambiguous-key` | `AmbiguousKey` | `key`, `sourceIndices` |
| `reduction.failed-scores` | `FailedScores` | `key`, `successful`, `failed` |
| `reduction.insufficient-successful` | `InsufficientSuccessful` | `key`, `required`, `successful`, `failed` |
| `reduction.mean-failure` | `MeanFailure` | `key`, `underlying` |

### `reconstruction` — `ReconstructionError`

| Code | Case | Operands |
|---|---|---|
| `reconstruction.pair-counts` | `PairCounts` | `eligible`, `selected` |
| `reconstruction.row-count` | `RowCount` | `expected`, `actual` |
| `reconstruction.denominator` | `Denominator` | `key`, `successful`, `failed`, `contributing` |
| `reconstruction.result-key` | `ResultKey` | `expected`, `found` |
| `reconstruction.result-counts` | `ResultCounts` | `key`, `result`, `successful`, `failed` |
| `reconstruction.duplicate-entry` | `DuplicateEntry` | `key`, `indices` |
| `reconstruction.report-count` | `ReportCount` | `field`, `expected`, `found` |
| `reconstruction.failed-keys` | `FailedKeys` | `expected`, `found` |
| `reconstruction.provenance-conflict` | `ProvenanceConflict` | `stage`, `declared`, `derived` |
| `reconstruction.contrast-domain` | `ContrastDomain` | `expected`, `found` |
| `reconstruction.contrast-row-shape` | `ContrastRowShape` | `key`, `matched`, `control`, `difference` |
| `reconstruction.contrast-operand` | `ContrastOperand` | `key`, `operand` |
| `reconstruction.incompatible` | `Incompatible` | `issues` |
| `reconstruction.orientation` | `Orientation` | `expected`, `found` |
| `reconstruction.key-domain` | `KeyDomain` | `expected`, `found` |
| `reconstruction.key-denominator` | `KeyDenominator` | `key`, `expectedSuccessful`, `expectedFailed`, `successful`, `failed` |
| `reconstruction.outcome-shape` | `OutcomeShape` | `key`, `expected`, `found` |

### `contrast` — `ContrastError`

| Code | Case | Operands |
|---|---|---|
| `contrast.incompatible` | `Incompatible` | `issues` |
| `contrast.empty-domain` | `EmptyDomain` | `matchedKeys`, `controlKeys` |
| `contrast.indistinguishable-ordering` | `IndistinguishableOrdering` | `first`, `second` |

### `contrast-row` — `ContrastRowError`

| Code | Case | Operands |
|---|---|---|
| `contrast-row.missing-operands` | `MissingOperands` | `key`, `missing` |
| `contrast-row.reduction-failures` | `ReductionFailures` | `key`, `matched`, `control` |
| `contrast-row.arithmetic` | `Arithmetic` | `key`, `underlying` |

### `contrast-compatibility` — `ContrastCompatibilityError`

| Code | Case | Operands |
|---|---|---|
| `contrast-compatibility.orientation` | `Orientation` | `matched`, `control` |
| `contrast-compatibility.policy` | `Policy` | `matched`, `control` |
| `contrast-compatibility.scale` | `Scale` | `matched`, `control` |
| `contrast-compatibility.missing-specification` | `MissingSpecification` | `operand`, `evaluation` |
| `contrast-compatibility.method` | `Method` | `matched`, `control` |
| `contrast-compatibility.components` | `Components` | `operand`, `declared`, `required` |
| `contrast-compatibility.spatial-convention` | `SpatialConvention` | `matched`, `control` |
| `contrast-compatibility.frames` | `Frames` | `underlying` |
| `contrast-compatibility.grids` | `Grids` | `underlying` |
| `contrast-compatibility.time` | `Time` | `matched`, `control` |
| `contrast-compatibility.clocks` | `Clocks` | `underlying` |

### `difference` — `DifferenceError`

| Code | Case | Operands |
|---|---|---|
| `difference.non-finite-operands` | `NonFiniteOperands` | `component`, `matched`, `control` |
| `difference.non-finite-difference` | `NonFiniteDifference` | `component`, `matched`, `control` |

### `score-mean` — `ScoreMeanError`

| Code | Case | Operands |
|---|---|---|
| `score-mean.empty-values` | `EmptyValues` | `operand` |
| `score-mean.non-finite-value` | `NonFiniteValue` | `component`, `index`, `value` |
| `score-mean.non-finite-mean` | `NonFiniteMean` | `component`, `value` |
| `score-mean.invalid-comparison-value` | `InvalidComparisonValue` | `component`, `underlying` |

### `evaluation-spec` — `EvaluationSpecError`

| Code | Case | Operands |
|---|---|---|
| `evaluation-spec.empty-field` | `EmptyField` | `field`, `value` |
| `evaluation-spec.invalid-components` | `InvalidComponents` | `values` |
| `evaluation-spec.invalid-parameters` | `InvalidParameters` | `values` |

### `pair-schedule` — `PairScheduleError`

| Code | Case | Operands |
|---|---|---|
| `pair-schedule.invalid-budget` | `InvalidBudget` | `sourceRows`, `candidatePairs`, `selectedPairs` |
| `pair-schedule.invalid-counts` | `InvalidCounts` | `left`, `right` |
| `pair-schedule.invalid-quantum` | `InvalidQuantum` | `value` |
| `pair-schedule.source-budget` | `SourceBudget` | `left`, `right`, `maximum` |
| `pair-schedule.candidate-budget` | `CandidateBudget` | `left`, `right`, `maximum` |
| `pair-schedule.selected-budget` | `SelectedBudget` | `relation`, `attempted`, `maximum` |

### `comparison-work` — `ComparisonWorkError`

| Code | Case | Operands |
|---|---|---|
| `comparison-work.invalid-quantum` | `InvalidQuantum` | `value` |
| `comparison-work.invalid-budget` | `InvalidBudget` | `maxWorkUnits` |
| `comparison-work.work-budget` | `WorkBudget` | `measure`, `workUnits`, `maximum` |

### `compare` — `CompareError`

| Code | Case | Operands |
|---|---|---|
| `compare.grids` | `Grids` | `underlying` |
| `compare.frames` | `Frames` | `underlying` |
| `compare.estimation` | `Estimation` | `underlying` |
| `compare.constant-input` | `ConstantInput` | `measure`, `operand` |
| `compare.empty-input` | `EmptyInput` | `measure`, `operand`, `total` |
| `compare.zero-norm` | `ZeroNorm` | `measure`, `leftNorm`, `rightNorm` |
| `compare.relative-entropy-support` | `RelativeEntropySupport` | `measure`, `cellIndex`, `leftMass`, `rightMass` |
| `compare.cost-matrix-limit-exceeded` | `CostMatrixLimitExceeded` | `measure`, `cells`, `limit` |
| `compare.invalid-substitution-cost` | `InvalidSubstitutionCost` | `measure`, `leftIndex`, `rightIndex`, `value` |
| `compare.invalid-score` | `InvalidScore` | `measure`, `underlying` |
| `compare.too-short` | `TooShort` | `what`, `got`, `needed` |

### `comparison-value` — `ComparisonValueError`

| Code | Case | Operands |
|---|---|---|
| `comparison-value.non-finite-measure-distance` | `NonFiniteMeasureDistance` | `value` |
| `comparison-value.negative-measure-distance` | `NegativeMeasureDistance` | `value` |
| `comparison-value.non-finite-similarity` | `NonFiniteSimilarity` | `value` |
| `comparison-value.invalid-unit-similarity` | `InvalidUnitSimilarity` | `component`, `value` |

### `estimate` — `EstimateError`

| Code | Case | Operands |
|---|---|---|
| `estimate.frame-mismatch` | `FrameMismatch` | `measure`, `grid` |
| `estimate.no-mass` | `NoMass` |  |
| `estimate.degenerate-bandwidth` | `DegenerateBandwidth` | `sigma`, `cellSize` |
| `estimate.surface` | `Surface` | `underlying` |

### `surface` — `SurfaceError`

| Code | Case | Operands |
|---|---|---|
| `surface.length-mismatch` | `LengthMismatch` | `expected`, `actual` |
| `surface.negative-weight` | `NegativeWeight` | `index`, `value` |
| `surface.negative-value` | `NegativeValue` | `index`, `value` |
| `surface.non-finite-value` | `NonFiniteValue` | `index`, `value` |
| `surface.degenerate-total` | `DegenerateTotal` | `total` |
| `surface.grid-mismatch` | `GridMismatch` | `left`, `right` |
| `surface.grid-identity-conflict` | `GridIdentityConflict` | `id`, `left`, `right` |
| `surface.empty-collection` | `EmptyCollection` | `operation` |

### `geometry` — `GeometryError`

| Code | Case | Operands |
|---|---|---|
| `geometry.degenerate-bounds` | `DegenerateBounds` | `xMin`, `yMin`, `xMax`, `yMax` |
| `geometry.non-finite-bounds` | `NonFiniteBounds` | `xMin`, `yMin`, `xMax`, `yMax` |
| `geometry.frame-mismatch` | `FrameMismatch` | `left`, `right` |
| `geometry.frame-identity-conflict` | `FrameIdentityConflict` | `id`, `left`, `right` |
| `geometry.non-finite-length` | `NonFiniteLength` | `value`, `unit` |
| `geometry.negative-length` | `NegativeLength` | `value`, `unit` |
| `geometry.non-positive-perspective` | `NonPositivePerspective` | `distanceMm`, `widthMm`, `heightMm` |
| `geometry.not-affine` | `NotAffine` | `matrix` |
| `geometry.non-finite-sigma` | `NonFiniteSigma` | `value` |
| `geometry.non-positive-sigma` | `NonPositiveSigma` | `value` |
| `geometry.degenerate-grid` | `DegenerateGrid` | `nx`, `ny` |
| `geometry.grid-cell-count-overflow` | `GridCellCountOverflow` | `nx`, `ny`, `cells` |
| `geometry.degenerate-ellipse` | `DegenerateEllipse` | `rx`, `ry` |
| `geometry.degenerate-polygon` | `DegeneratePolygon` | `vertices` |
| `geometry.non-finite-region` | `NonFiniteRegion` | `shape` |
| `geometry.non-finite-velocity` | `NonFiniteVelocity` | `value` |
| `geometry.negative-velocity` | `NegativeVelocity` | `value` |
| `geometry.non-finite-distance` | `NonFiniteDistance` | `value` |
| `geometry.negative-distance` | `NegativeDistance` | `value` |

### `time` — `TimeError`

| Code | Case | Operands |
|---|---|---|
| `time.reversed-interval` | `ReversedInterval` | `clock`, `onsetMicros`, `offsetMicros` |
| `time.reversed-window` | `ReversedWindow` | `fromMicros`, `untilMicros` |
| `time.clock-mismatch` | `ClockMismatch` | `left`, `right` |
| `time.wrong-source-clock` | `WrongSourceClock` | `expected`, `actual` |
| `time.non-positive-rate` | `NonPositiveRate` | `value` |
| `time.non-finite-drift` | `NonFiniteDrift` | `from`, `to`, `drift` |
| `time.non-positive-clock-scale` | `NonPositiveClockScale` | `from`, `to`, `drift` |

### `window-occupancy` — `WindowOccupancyError`

| Code | Case | Operands |
|---|---|---|
| `window-occupancy.time` | `Time` | `underlying` |
| `window-occupancy.measure` | `Measure` | `underlying` |
| `window-occupancy.invalid-width` | `InvalidWidth` | `interval`, `micros` |
| `window-occupancy.empty-coverage-interval` | `EmptyCoverageInterval` | `clock`, `intervals` |
| `window-occupancy.overlapping-coverage` | `OverlappingCoverage` | `clock`, `intervals` |

### `sync-evidence` — `SyncEvidenceError`

| Code | Case | Operands |
|---|---|---|
| `sync-evidence.empty-mark-id` | `EmptyMarkId` | `value` |
| `sync-evidence.negative-residual-limit` | `NegativeResidualLimit` | `limit` |
| `sync-evidence.negative-error-magnitude` | `NegativeErrorMagnitude` | `value` |
| `sync-evidence.too-few-common-marks` | `TooFewCommonMarks` | `source`, `target`, `available`, `required` |
| `sync-evidence.too-few-retained-marks` | `TooFewRetainedMarks` | `source`, `target`, `supplied`, `retained`, `required` |
| `sync-evidence.duplicate-mark-id` | `DuplicateMarkId` | `source`, `target`, `id`, `firstIndex`, `secondIndex` |
| `sync-evidence.non-increasing-source-marks` | `NonIncreasingSourceMarks` | `source`, `target`, `index`, `previous`, `current` |
| `sync-evidence.non-increasing-target-marks` | `NonIncreasingTargetMarks` | `source`, `target`, `index`, `previous`, `current` |
| `sync-evidence.degenerate-source-variance` | `DegenerateSourceVariance` | `source`, `target`, `variance` |
| `sync-evidence.non-finite-fit` | `NonFiniteFit` | `source`, `target`, `scale`, `offsetMicros` |
| `sync-evidence.invalid-fitted-sync` | `InvalidFittedSync` | `source`, `target`, `underlying` |

### `core` — `CoreError`

| Code | Case | Operands |
|---|---|---|
| `core.of-time` | `OfTime` | `underlying` |
| `core.of-geometry` | `OfGeometry` | `underlying` |
| `core.of-surface` | `OfSurface` | `underlying` |
| `core.of-recording` | `OfRecording` | `underlying` |
| `core.of-scanpath` | `OfScanpath` | `underlying` |
| `core.of-event` | `OfEvent` | `underlying` |
| `core.of-detection-support` | `OfDetectionSupport` | `underlying` |

### `recording` — `RecordingError`

| Code | Case | Operands |
|---|---|---|
| `recording.no-samples` | `NoSamples` |  |
| `recording.non-monotonic` | `NonMonotonic` | `index`, `previousMicros`, `currentMicros` |
| `recording.unpaired-eyes` | `UnpairedEyes` | `timestamps`, `left`, `right` |
| `recording.non-finite-position` | `NonFinitePosition` | `channel`, `index`, `state`, `x`, `y` |
| `recording.tracked-outside-frame` | `TrackedOutsideFrame` | `channel`, `index`, `frameId`, `frameSpec`, `x`, `y` |
| `recording.off-screen-inside-frame` | `OffScreenInsideFrame` | `channel`, `index`, `frameId`, `frameSpec`, `x`, `y` |
| `recording.invalid-pupil` | `InvalidPupil` | `channel`, `index`, `value` |
| `recording.undeclared-pupil-unit` | `UndeclaredPupilUnit` | `channel`, `index`, `value` |
| `recording.negative-sampling-tolerance` | `NegativeSamplingTolerance` | `tolerance` |
| `recording.fixed-rate-mismatch` | `FixedRateMismatch` | `index`, `previousMicros`, `currentMicros`, `nominalPeriod`, `tolerance`, `deviation` |

### `scanpath` — `ScanpathError`

| Code | Case | Operands |
|---|---|---|
| `scanpath.no-fixations` | `NoFixations` |  |
| `scanpath.out-of-order` | `OutOfOrder` | `index`, `previous`, `current` |
| `scanpath.wrong-clock` | `WrongClock` | `index`, `expected`, `actual` |
| `scanpath.invalid-transition-span` | `InvalidTransitionSpan` | `index`, `underlying` |
| `scanpath.invalid-extent` | `InvalidExtent` | `underlying` |
| `scanpath.unmappable-fixation` | `UnmappableFixation` | `index`, `from`, `to`, `x`, `y` |

### `event` — `EventError`

| Code | Case | Operands |
|---|---|---|
| `event.empty-span` | `EmptySpan` | `eventType`, `span` |
| `event.non-finite-point` | `NonFinitePoint` | `eventType`, `role`, `span`, `x`, `y` |
| `event.invalid-dispersion` | `InvalidDispersion` | `value`, `method` |
| `event.non-positive-sample-count` | `NonPositiveSampleCount` | `sampleCount` |
| `event.empty-pursuit` | `EmptyPursuit` | `span` |
| `event.non-finite-pursuit-point` | `NonFinitePursuitPoint` | `span`, `index`, `x`, `y` |

### `detection-support` — `DetectionSupportError`

| Code | Case | Operands |
|---|---|---|
| `detection-support.invalid-sample-range` | `InvalidSampleRange` | `from`, `until` |
| `detection-support.event-support-count-mismatch` | `EventSupportCountMismatch` | `source`, `events`, `ranges` |
| `detection-support.event-clock-mismatch` | `EventClockMismatch` | `source`, `eventIndex`, `expected`, `actual` |
| `detection-support.sample-range-outside-recording` | `SampleRangeOutsideRecording` | `source`, `rangeIndex`, `range`, `recordingSamples` |
| `detection-support.overlapping-sample-ranges` | `OverlappingSampleRanges` | `source`, `rangeIndex`, `previous`, `current` |
| `detection-support.event-span-outside-recording` | `EventSpanOutsideRecording` | `source`, `eventIndex`, `eventSpan`, `recordingExtent` |
| `detection-support.event-span-has-no-samples` | `EventSpanHasNoSamples` | `source`, `eventIndex`, `eventSpan` |
| `detection-support.event-sample-range-mismatch` | `EventSampleRangeMismatch` | `source`, `eventIndex`, `eventSpan`, `declared`, `derived` |
| `detection-support.invalid-derived-sample-range` | `InvalidDerivedSampleRange` | `source`, `eventIndex`, `eventSpan`, `from`, `until` |
| `detection-support.invalid-derived-fixation` | `InvalidDerivedFixation` | `source`, `eventIndex`, `range`, `underlying` |
| `detection-support.no-usable-source-samples` | `NoUsableSourceSamples` | `source`, `eventIndex`, `range` |
| `detection-support.unmappable-source-sample` | `UnmappableSourceSample` | `source`, `eventIndex`, `sampleIndex`, `from`, `to`, `x`, `y` |
| `detection-support.unmappable-event-point` | `UnmappableEventPoint` | `source`, `eventIndex`, `role`, `pointIndex`, `from`, `to`, `x`, `y` |

### `detector-definition` — `DetectorDefinitionError`

| Code | Case | Operands |
|---|---|---|
| `detector-definition.configuration` | `Configuration` | `id`, `parameters` |

### `detection-result` — `DetectionResultError`

| Code | Case | Operands |
|---|---|---|
| `detection-result.detector-emission-failed` | `DetectorEmissionFailed` | `recording`, `detector`, `emissionIndex`, `underlying` |
| `detection-result.event-outside-recording` | `EventOutsideRecording` | `recording`, `detector`, `eventIndex`, `eventSpan`, `recordingExtent` |
| `detection-result.source-support` | `SourceSupport` | `recording`, `detector`, `underlying` |
| `detection-result.gap-policy-violation` | `GapPolicyViolation` | `recording`, `detector`, `eventIndex`, `gap`, `duration`, `policy` |
| `detection-result.invalid-derived-range` | `InvalidDerivedRange` | `recording`, `detector`, `role`, `from`, `until`, `underlying` |

### `detection-failure` — `DetectionFailure`

| Code | Case | Operands |
|---|---|---|
| `detection-failure.event-summary` | `EventSummary` | `underlying` |
| `detection-failure.kinematics` | `Kinematics` | `underlying` |

### `kinematics` — `KinematicsError`

| Code | Case | Operands |
|---|---|---|
| `kinematics.invalid-sampling` | `InvalidSampling` | `underlying` |

### `configuration` — `ConfigurationError`

| Code | Case | Operands |
|---|---|---|
| `configuration.non-positive-window-half-width` | `NonPositiveWindowHalfWidth` | `halfWidth` |
| `configuration.insufficient-regular-samples` | `InsufficientRegularSamples` | `sampleCount` |
| `configuration.non-positive-sampling-interval` | `NonPositiveSamplingInterval` | `index`, `interval` |
| `configuration.irregular-sampling-interval` | `IrregularSamplingInterval` | `index`, `expected`, `observed` |
| `configuration.negative-missing-padding` | `NegativeMissingPadding` | `pad` |
| `configuration.negative-interpolation-gap` | `NegativeInterpolationGap` | `maxGap` |
| `configuration.negative-maximum-merge-gap` | `NegativeMaximumMergeGap` | `maxGap` |
| `configuration.non-positive-minimum-event-duration` | `NonPositiveMinimumEventDuration` | `minDuration` |
| `configuration.non-positive-ivt-threshold` | `NonPositiveIvtThreshold` | `thresholdDegPerSecond` |
| `configuration.invalid-ek-thresholds` | `InvalidEkThresholds` | `etaXDegPerSecond`, `etaYDegPerSecond` |
| `configuration.invalid-ek-multiplier` | `InvalidEkMultiplier` | `lambda` |
| `configuration.non-positive-ek-minimum-samples` | `NonPositiveEkMinimumSamples` | `minSamples` |

### `aoi` — `AoiError`

| Code | Case | Operands |
|---|---|---|
| `aoi.blank-id` | `BlankId` | `value` |
| `aoi.blank-label` | `BlankLabel` | `id`, `value` |
| `aoi.blank-attribute-key` | `BlankAttributeKey` | `id`, `key` |
| `aoi.non-positive-resolution` | `NonPositiveResolution` | `nx`, `ny` |
| `aoi.empty-set` | `EmptySet` |  |
| `aoi.duplicate-id` | `DuplicateId` | `id`, `firstIndex`, `secondIndex` |
| `aoi.frame-conflict` | `FrameConflict` | `underlying` |
| `aoi.resolution-grid-failure` | `ResolutionGridFailure` | `frame`, `nx`, `ny`, `underlying` |
| `aoi.observed-overlap` | `ObservedOverlap` | `frame`, `sampleIndex`, `x`, `y`, `areas` |

### `descriptor` — `DescriptorError`

| Code | Case | Operands |
|---|---|---|
| `descriptor.invalid-field` | `InvalidField` | `id`, `version`, `meaning` |
| `descriptor.invalid-alternatives` | `InvalidAlternatives` | `id`, `values` |
| `descriptor.invalid-default` | `InvalidDefault` | `name`, `reason` |
| `descriptor.duplicate-fields` | `DuplicateFields` | `ids` |
| `descriptor.invalid-component` | `InvalidComponent` | `id`, `meaning`, `range` |
| `descriptor.parameter-mismatch` | `ParameterMismatch` | `described`, `declared` |
| `descriptor.component-mismatch` | `ComponentMismatch` | `described`, `declared` |
| `descriptor.missing-method` | `MissingMethod` | `id` |
| `descriptor.method-identity` | `MethodIdentity` | `expected`, `found` |
| `descriptor.execution-mismatch` | `ExecutionMismatch` | `declared`, `actual` |
| `descriptor.unexplained-fields` | `UnexplainedFields` | `fields` |

### `study-finding` — `StudyFinding`

| Code | Case | Operands |
|---|---|---|
| `study-finding.undescribed-method` | `UndescribedMethod` | `method` |
| `study-finding.inconsistent-descriptor` | `InconsistentDescriptor` | `method`, `underlying` |
| `study-finding.missing-artifact` | `MissingArtifact` | `expected` |
| `study-finding.artifact-mismatch` | `ArtifactMismatch` | `expected`, `actual` |
| `study-finding.over-budget` | `OverBudget` | `underlying` |
| `study-finding.refused` | `Refused` | `underlying` |
| `study-finding.frame-mismatch` | `FrameMismatch` | `key`, `underlying` |
| `study-finding.duplicate-trial` | `DuplicateTrial` | `key`, `side`, `positions` |
| `study-finding.unmatched-focal` | `UnmatchedFocal` | `key` |
| `study-finding.uncontrolled-focal` | `UncontrolledFocal` | `key` |

### `recording-finding` — `RecordingFinding`

| Code | Case | Operands |
|---|---|---|
| `recording-finding.undescribed-method` | `UndescribedMethod` | `method` |
| `recording-finding.inconsistent-descriptor` | `InconsistentDescriptor` | `method`, `underlying` |
| `recording-finding.missing-artifact` | `MissingArtifact` | `expected` |
| `recording-finding.artifact-mismatch` | `ArtifactMismatch` | `expected`, `actual` |
| `recording-finding.frame-mismatch` | `FrameMismatch` | `source`, `underlying` |
| `recording-finding.clock-mismatch` | `ClockMismatch` | `source`, `underlying` |
| `recording-finding.missing-viewing` | `MissingViewing` | `source` |
| `recording-finding.missing-synchronization` | `MissingSynchronization` | `source`, `target` |
| `recording-finding.refused` | `Refused` | `underlying` |
| `recording-finding.synchronization` | `Synchronization` | `underlying` |
| `recording-finding.angular-frame` | `AngularFrame` | `frame`, `underlying` |
| `recording-finding.area-warp` | `AreaWarp` | `area`, `corner` |
| `recording-finding.detector-definition` | `DetectorDefinition` | `method`, `underlying` |

### `temporal-finding` — `TemporalFinding`

| Code | Case | Operands |
|---|---|---|
| `temporal-finding.missing-artifact` | `MissingArtifact` | `expected` |
| `temporal-finding.artifact-mismatch` | `ArtifactMismatch` | `expected`, `actual` |
| `temporal-finding.refused` | `Refused` | `underlying` |
| `temporal-finding.study` | `Study` | `underlying` |
| `temporal-finding.repetition-plan` | `RepetitionPlan` | `repetition`, `underlying` |
| `temporal-finding.repetition` | `Repetition` | `repetition`, `underlying` |
| `temporal-finding.missing-epoch` | `MissingEpoch` | `key` |
| `temporal-finding.coverage-clock` | `CoverageClock` | `key`, `underlying` |
| `temporal-finding.window-resolution` | `WindowResolution` | `key`, `window`, `underlying` |
| `temporal-finding.no-observed-coverage` | `NoObservedCoverage` | `key`, `window` |

### `budget` — `BudgetError`

| Code | Case | Operands |
|---|---|---|
| `budget.candidate-visits` | `CandidateVisits` | `focalTrials`, `referenceTrials`, `scales`, `maximumCandidateVisits` |
| `budget.schedule` | `Schedule` | `underlying` |

### `preflight` — `PreflightError`

| Code | Case | Operands |
|---|---|---|
| `preflight.changed-plan` | `ChangedPlan` | `family`, `changes` |
| `preflight.changed-input` | `ChangedInput` | `family`, `reported`, `actual` |
| `preflight.not-ready` | `NotReady` | `family`, `blockers` |
| `preflight.refused` | `Refused` | `underlying` |

### `admission-reason` — `AdmissionReason`

| Code | Case | Operands |
|---|---|---|
| `admission-reason.width` | `Width` | `expected`, `actual` |
| `admission-reason.key` | `Key` | `reason` |
| `admission-reason.number` | `Number` | `column`, `value`, `requirement` |
| `admission-reason.time` | `Time` | `onset`, `duration`, `unit`, `reason` |
| `admission-reason.position` | `Position` | `x`, `y`, `frame` |
| `admission-reason.event` | `Event` | `reason` |
| `admission-reason.quarantined` | `Quarantined` | `records`, `cause` |

### `quarantine` — `QuarantineCause`

| Code | Case | Operands |
|---|---|---|
| `quarantine.rejected-records` | `RejectedRecords` |  |
| `quarantine.duplicate-ordinals` | `DuplicateOrdinals` |  |
| `quarantine.no-fixations` | `NoFixations` |  |
| `quarantine.overlap` | `Overlap` | `index`, `previous`, `current` |
| `quarantine.wrong-clock` | `WrongClock` | `index`, `expected`, `actual` |
| `quarantine.invalid-transition` | `InvalidTransition` | `index`, `reason` |
| `quarantine.invalid-extent` | `InvalidExtent` | `reason` |
| `quarantine.unmappable-fixation` | `UnmappableFixation` | `index`, `from`, `to`, `x`, `y` |

### `admission` — `AdmissionError`

| Code | Case | Operands |
|---|---|---|
| `admission.non-positive-record` | `NonPositiveRecord` | `record` |
| `admission.record-order` | `RecordOrder` | `index`, `previous`, `record` |
| `admission.negative-ordinal` | `NegativeOrdinal` | `record`, `value` |
| `admission.duplicate-ordinal` | `DuplicateOrdinal` | `records`, `value` |
| `admission.quarantine-scope` | `QuarantineScope` | `record`, `records` |
| `admission.quarantine-admitted` | `QuarantineAdmitted` | `record`, `admitted` |
| `admission.quarantined-key-admitted` | `QuarantinedKeyAdmitted` | `quarantined`, `admitted` |
| `admission.outcome-mismatch` | `OutcomeMismatch` | `outcome`, `rejected` |
| `admission.ambiguous-trial` | `AmbiguousTrial` | `indices` |
| `admission.unknown-trial` | `UnknownTrial` | `records` |
| `admission.unadmitted-trial` | `UnadmittedTrial` | `index` |
| `admission.fixation-count` | `FixationCount` | `index`, `fixations`, `records` |

### `inspection` — `InspectionError`

| Code | Case | Operands |
|---|---|---|
| `inspection.unknown-scale` | `UnknownScale` | `index`, `scales` |
| `inspection.unknown-cell` | `UnknownCell` | `repetition`, `window` |
| `inspection.unknown-reference` | `UnknownReference` | `reference` |
| `inspection.duplicate-reference` | `DuplicateReference` | `reference` |
| `inspection.invalid-page-size` | `InvalidPageSize` | `requested`, `maximum` |
| `inspection.sources` | `Sources` | `underlying` |
| `inspection.input-mismatch` | `InputMismatch` | `result`, `sources` |
| `inspection.components` | `Components` | `underlying` |
| `inspection.plan-mismatch` | `PlanMismatch` | `changes` |
| `inspection.reduction-membership` | `ReductionMembership` | `reference`, `selected`, `members`, `contributing`, `contributors` |
| `inspection.orientation` | `Orientation` | `scale`, `design`, `found` |
| `inspection.no-contrast` | `NoContrast` | `scale` |

<!-- END GENERATED DIAGNOSTIC CODES -->

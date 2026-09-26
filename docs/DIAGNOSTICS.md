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

`Diagnostic.of(error)` projects any cataloged error through its
`Diagnose[E, K]` instance, the single projection surface: every public enum of
the shipped modules whose name ends in `Error` or `Failure` has one. `K` is
the key type of the trials the diagnostic names: `Nothing` for a family that
names none, the study key for a keyed family. `Diagnose` is contravariant in
the error, so a case type such as `CodecError.Entry` projects through its
enum's instance. The instances of the plan and every pure module below it are
in `Diagnose`'s companion and need no import. The codec's families
(`CodecError`, `ResolveError`, `SourceFailure`, `RelationMismatch`,
`ManifestError`, `PayloadError`, `ByteDigestError`) project through
`CodecDiagnostics`, the results module's (report findings and refusals,
covariates and result tables) through `eyes4s.results.ResultsDiagnostics`, io's through
`IoDiagnostics`, the laws module's through
`LawsDiagnostics` and the JVM-only Arrow export through `ArrowDiagnostics`;
import their `given`s to use `Diagnostic.of` on them.

Codes are only ever issued. When an error type is removed its family is retired:
`DiagnosticCatalog.retired` keeps the family and its codes in their place in
`DiagnosticCatalog.issued`, so no code is reused with another meaning, while
`DiagnosticCatalog.families` and the table below list only the live ones. The
retired families are `scanpath-component` (the six-slot scanpath result) and
`learned-template` and `template-fit`, which became the `template` family.

The codec cannot state the key type of a study it decodes, so trial keys
inside a codec, resolution or relation error (and an io export error that
wraps one) are `ErasedKey`s. `diagnostic.narrow[K]` returns the diagnostic
with typed keys, or `None` when any key, source links included, is not a `K`.

Families whose projection names no custom subject are derived from the enum:
each field becomes the operand of its type under its own name, and a wrapped
error keeps its subject. An io case that names a text source and a line has
the subject `Line(source, line)`; a tidy CSV error names its logical record
(the header is record 1) as `Record`; a fixation import refused for rejected
rows names them as `Records`. A diagnostic carries:

- `code`: the stable identity, rendered `family.case`, for example
  `reduction.failed-scores`. A consumer localises from the code and the
  operands, never from `message`.
- `severity`: `Error` when the operation or the named item produced no value,
  `Warning` for preflight warnings only.
- `subject`: the failing object as a path of `Locus` values from the coarsest to
  the finest: `Repetition` and `Window` for a temporal cell, `Scale`, `Design`,
  then `Trial`, `Pair`, `Trials`, `Record`, `Records` or `Fixation`; for
  recordings `Recording`, `Event`, `Sample`, `Samples` or `Area`; and for saved
  studies `Entry` (a manifest entry), `Relation` and `Path` (a location in a
  decoded document). A wrapper (a
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
- `source`: who reported it. Every library diagnostic is `EyesCore`; a host
  application reports its own checks as `Host`, with a code from
  `DiagnosticCode.host(family, name)`, so the two can be shown as separately
  labelled sources.
- `affectedTrials`: every trial key the diagnostic names, typed: its subject,
  then its operands and nested causes, each key once in first-seen order. An
  application opens exactly these trials, for example to choose an occurrence.
- `message`: a default English rendering. It is not an identity, and numbers
  may render differently on Scala.js.

Nested errors never collapse to their messages. `StudyFailure.Frame(key, e)`
has the subject `Trial(key)` and the operand `underlying`, a `Cause` whose own
code is `geometry.frame-mismatch` and whose operands are the two frame names.

### Stability

`DiagnosticCatalog` (plan) and `CodecDiagnosticCatalog` (codec) are the code
tables: each family lists its enum's cases in declaration order, and a case's
code is the family name and its kebab-case label. `DiagnosticCatalogSuite`
and `CodecDiagnosticCatalogSuite` compare every list with the compiler's own
case list and sample every case of every family through its public
`Diagnose` instance. For each sample they check that the operands are the
case's fields in order, that each operand carries its field's value (a
structured field must have its structured projection, and a nested error must
carry the code of the family that owns its case), that a wrapper keeps the
wrapped subject, and that same-typed fields of a sample differ, so a swapped
operand is detected. Walking every sample's fields, they also fail when an
error, failure, mismatch, cause, reason or finding reachable from any
cataloged error (`CodecError` and `ResolveError` included) belongs to a family
that is not cataloged, or is carried as a token instead of a cause. They pin
each table's rendered codes by count and portable digest on both platforms. A
new, removed, renamed or reordered case therefore fails the tests until the
catalog, the pinned digest and this document are updated. Renaming a case
changes its code; treat that as a breaking change.

The families covered are exactly those listed in the code table below, which
a test generates from every catalog.

Every public enum of the shipped modules whose name ends in `Error` or
`Failure` is cataloged; `DiagnosticCoverageJvmSuite` reads the reviewed API
inventory (`tools/api-audit/inventory.json`) and fails for one without an
instance. Not cataloged, and so without codes, are the record types that
report warnings or parse diagnostics as data rather than refuse an operation.
A test checks that none of them is in a catalog.

<!-- BEGIN NOT CATALOGED -->
- EyeLink ASC parse and session reports: `EyeLinkAscSessionDiagnostic`,
  `AscSampleDiagnostic`, `AscNativeDiagnostic`, `AscNativePairingDiagnostic`,
  `AscFramingDiagnostic`, `AscLexicalDiagnostic` and `DelimitedDiagnostic`,
  each with its own severity.
- Warnings carried beside a result: `DetectionWarning`,
  `ConversionEvidenceWarning` and `ScientificValidationWarning`.
<!-- END NOT CATALOGED -->

`TemporalStudyError` identifies trials by key digest, so its subject is a
`TrialDigest`; inside a temporal result the inspection adds the typed key.

Reconstructing a recording or temporal result archive refuses with
`RecordingResultError` and `TemporalResultError` (plan table), which the codec
carries as `codec.recording-result` and `codec.temporal-result`, and with
`codec.derived` when an archived member differs from the one the archive's own
evidence derives; the member's path is its `Path` locus. A refused temporal
cell has the subject `Repetition` and `Window`, then the trial it concerns; a
refused recording stage has a `Field` locus naming the stage and field.

Preflight: a report's `diagnostics` are its findings projected to
`Diagnostic[K]`, keys typed, and every finding's `affectedTrials` are exactly
the finding's `keys`. `PreflightError[K]` keeps its blockers' key type, so
`PreflightError.NotReady` projects to `Diagnostic[K]` too; a recording
refusal is `PreflightError[Nothing]`.

## Source links

`StudySources.of(input, ledger)` indexes the admission ledger of a ledgered
import after `AdmissionLedger.checkAgainst` confirms that it describes exactly
this input; `StudySources.unledgered(input)` indexes an input without one.
Both take the key type's `KeyDigest`. A refusal is a `LedgerRefusal`: the
`AdmissionError`, which names input positions and record numbers, resolved
against the input and the ledger to the trials it concerns, by key, and links
to their records. `Diagnostic.of(refusal)` projects it with the admission
code, the trials first in its subject and those links as its sources; an input
position stays in the subject only for a repeated key, one per occurrence.
An input trial the ledger never mentions links to an explicit
`UnknownTrial`.

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
`AmbiguousTrial` (the input repeats the full key), `UnknownDigest` (no input
trial has the digest), `CollidingDigest` (several do, all named) and
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
design and linked to its sources; it is computed once, on first use.

Opening an inspection builds every listing, its diagnostics and their source
links at once, so its cost is proportional to the size of the result and its
input. Paging then only slices the built listings.

A drill-down from a contrast row to CSV records reads:

The complete [inspection example](../io/src/test/scala/eyes4s/io/InspectionSourceSuite.scala), in the test `a result row drills down to the logical CSV record that supplied it`, constructs the plan, input and ledger and follows the references to an asserted CSV record.

`TemporalInspection.cell(repetition, window)` returns each trial's window
occupancy (the fixation indices it retains link to their records through
`fixation`) and a `StudyInspection` of the cell whose items are `InCell`
references; `cellOf(ref)` finds the cell a reference belongs to.
`RecordingInspection.events` addresses detected events by their source sample
range, each with a `Samples` link into the plan's input recording, whose
sample count and order preprocessing keeps.

## Evidence

- `plan/src/test/scala/eyes4s/plan/DiagnosticCatalogSuite.scala`,
  `codec/src/test/scala/eyes4s/codec/CodecDiagnosticCatalogSuite.scala` and
  `io/src/test/scala/eyes4s/io/IoDiagnosticCatalogSuite.scala`:
  catalog totality against the compiler's case lists, per-case samples, field
  alignment with mutant checks, reachable error types, preserved nested
  subjects, exact non-finite operands, unique and pinned codes on both
  platforms, typed affected trials of every finding and preflight refusal.
  A derived family's samples are generated, one per case, from its
  compiler-supplied case list (`DiagnosticExample`).
- `io/.jvm/src/test/scala/eyes4s/io/DiagnosticCoverageJvmSuite.scala`: every
  public `Error` or `Failure` enum of the API inventory has an instance and a
  sampled family, and no public signature projects to `Diagnostic[Any]`.
- `plan/src/test/scala/eyes4s/plan/ResultInspectionSuite.scala`: drill-down,
  membership under both failure policies, colliding display text and repeated
  keys, reordered input, a two-component custom score and a mislabelled
  schema, scale, trial and window failures, cell-scoped references, missing
  locators, digest and position resolution, rejected records, buffer copies,
  paging at several sizes, agreement with the typed result, recording events
  linked to the plan's input, and ledger refusals named by trial key.
- `codec/src/test/scala/eyes4s/codec/InspectionArchiveSuite.scala` and
  `codec/.jvm/src/test/scala/eyes4s/codec/InspectionManifestJvmSuite.scala`:
  the pinned study-result-v1, study-input-v1 and admission-ledger-complete-v1,
  and the same artifacts resolved from manifest-v1, drill down to CSV record
  numbers; the pinned refused admission ledger names its rejected records and,
  against the full input, the trial it did not admit; a decoded archive
  inspects exactly as its original, reports included; source-supported
  fixations link to their samples; a tampered archive is refused with a coded
  diagnostic.
- `io/src/test/scala/eyes4s/io/InspectionSourceSuite.scala`: an imported CSV
  with multi-line fields drills down to its logical records, rejected rows
  become coded diagnostics, and `ContrastCsv` (one and two components) and
  `TemporalContrastCsv` carry the projection's values, denominators, statuses
  and reasons.
- `tools/study-consumer`: a packaged consumer drills down from a result row to
  a CSV record number on the JVM and Scala.js. Its fixation journey
  (`FixationJourneySuite`) also projects a failed run's `PlanError`, preflight
  findings, a bad table row's admission reasons and ledger refusal, the
  failures of a scale whose bandwidth the grid cannot express, and a flipped,
  missing or replaced archive entry's `ResolveError` to coded diagnostics; it
  asserts the subjects and record links of the admission reasons, the ledger
  refusal, the scale failures and the refused entries.

## Code table

Generated from `DiagnosticCatalog`, `CodecDiagnosticCatalog`,
`LawsDiagnosticCatalog`, `IoDiagnosticCatalog` and `ArrowDiagnostics` and
checked by `io/.jvm/src/test/scala/eyes4s/io/DiagnosticsDocJvmSuite.scala`.
Codes are only ever appended: the plan table's first 433 codes are pinned
separately and never change. Set
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
| `plan.missing-angular-scale` | `MissingAngularScale` | `scale` |
| `plan.geometry` | `Geometry` | `underlying` |
| `plan.invalid-window-tally` | `InvalidWindowTally` | `outsideScreen`, `outsideWindow`, `total`, `outsideScreenMicros`, `outsideWindowMicros`, `totalMicros` |
| `plan.invalid-occurrence` | `InvalidOccurrence` | `value` |
| `plan.blank-key-field` | `BlankKeyField` | `field` |
| `plan.occurrence-unavailable` | `OccurrenceUnavailable` | `layout`, `matched` |
| `plan.match-item-conflict` | `MatchItemConflict` | `trialDigests` |
| `plan.matched-cardinality` | `MatchedCardinality` | `matched`, `focalDigests`, `referenceGroups` |
| `plan.unmatched-focal-refused` | `UnmatchedFocalRefused` | `focalDigests` |
| `plan.initial-fixations` | `InitialFixations` | `underlying` |

### `study-failure` — `StudyFailure`

| Code | Case | Operands |
|---|---|---|
| `study-failure.frame` | `Frame` | `key`, `underlying` |
| `study-failure.occupancy` | `Occupancy` | `key`, `underlying` |
| `study-failure.temporal` | `Temporal` | `key`, `underlying` |
| `study-failure.estimation` | `Estimation` | `key`, `underlying` |
| `study-failure.comparison` | `Comparison` | `left`, `right`, `underlying` |
| `study-failure.off-window` | `OffWindow` | `key`, `tally` |
| `study-failure.initial-fixations` | `InitialFixations` | `key`, `underlying` |

### `initial-fixation` — `InitialFixationError`

| Code | Case | Operands |
|---|---|---|
| `initial-fixation.non-positive-radius` | `NonPositiveRadius` | `radiusDegrees` |
| `initial-fixation.non-finite-cross` | `NonFiniteCross` | `x`, `y` |
| `initial-fixation.cross-off-frame` | `CrossOffFrame` | `x`, `y`, `frame` |
| `initial-fixation.missing-angular-scale` | `MissingAngularScale` | `radiusDegrees` |
| `initial-fixation.invalid-tally` | `InvalidTally` | `dropped`, `total`, `droppedMicros`, `totalMicros` |
| `initial-fixation.no-fixation-kept` | `NoFixationKept` | `dropped`, `droppedMicros` |

### `study-revision` — `StudyRevisionError`

| Code | Case | Operands |
|---|---|---|
| `study-revision.duplicate-field` | `DuplicateField` | `field` |
| `study-revision.stale` | `Stale` | `field`, `stated`, `current` |
| `study-revision.incomplete-window` | `IncompleteWindow` | `window`, `offWindow` |
| `study-revision.plan` | `Plan` | `underlying` |

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
| `study-result.specification-time` | `SpecificationTime` | `design`, `expected`, `found` |

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

### `recording-result` — `RecordingResultError`

| Code | Case | Operands |
|---|---|---|
| `recording-result.plan` | `Plan` | `underlying` |
| `recording-result.stage` | `Stage` | `stage`, `field`, `expected`, `found` |

### `temporal-result` — `TemporalResultError`

| Code | Case | Operands |
|---|---|---|
| `temporal-result.cell-count` | `CellCount` | `expected`, `found` |
| `temporal-result.cell-layout` | `CellLayout` | `index`, `repetition`, `window`, `foundRepetition`, `foundWindow` |
| `temporal-result.repetition` | `Repetition` | `repetition`, `underlying` |
| `temporal-result.plan` | `Plan` | `changes` |
| `temporal-result.result` | `Result` | `underlying` |
| `temporal-result.occupancy-keys` | `OccupancyKeys` | `expected`, `found` |
| `temporal-result.boundary` | `Boundary` | `key`, `expected`, `found` |
| `temporal-result.width` | `Width` | `key`, `expectedMicros`, `foundMicros` |
| `temporal-result.epoch` | `Epoch` | `key`, `expected`, `found` |
| `temporal-result.anchor` | `Anchor` | `key`, `expectedClock`, `expectedMicros`, `foundClock`, `foundMicros` |
| `temporal-result.density` | `Density` | `key`, `expected`, `found` |
| `temporal-result.failure` | `Failure` | `key`, `failure` |
| `temporal-result.cell` | `Cell` | `repetition`, `window`, `underlying` |

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
| `compare.work-limit-exceeded` | `WorkLimitExceeded` | `measure`, `cells`, `pairs`, `limit` |
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
| `estimate.degenerate-axis-bandwidth` | `DegenerateAxisBandwidth` | `axis`, `sigma`, `cellSize` |
| `estimate.kernel-support-overflow` | `KernelSupportOverflow` | `axis`, `sigma`, `cellSize` |

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
| `geometry.bounds-extent-overflow` | `BoundsExtentOverflow` | `xMin`, `yMin`, `xMax`, `yMax` |
| `geometry.subframe-outside-parent` | `SubframeOutsideParent` | `window`, `xMin`, `yMin`, `xMax`, `yMax`, `parent`, `parentSpec` |
| `geometry.subframe-identity` | `SubframeIdentity` | `window` |
| `geometry.non-positive-angular-scale` | `NonPositiveAngularScale` | `frame`, `unitsPerDegree` |
| `geometry.non-finite-translation` | `NonFiniteTranslation` | `dx`, `dy` |

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
| `window-occupancy.observed-time` | `ObservedTime` | `interval`, `observedMicros`, `missingMicros` |
| `window-occupancy.ledger` | `Ledger` | `position`, `index`, `originalMicros`, `retainedMicros`, `boundary` |
| `window-occupancy.measure-support` | `MeasureSupport` | `retained`, `positions` |

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
| `study-finding.off-window-fixations` | `OffWindowFixations` | `key`, `tally`, `policy` |
| `study-finding.no-fixation-in-window` | `NoFixationInWindow` | `key`, `tally` |
| `study-finding.matched-cardinality` | `MatchedCardinality` | `key`, `references`, `matched` |
| `study-finding.ambiguous-references` | `AmbiguousReferences` | `references`, `matched` |
| `study-finding.unmatched-focal-refused` | `UnmatchedFocalRefused` | `key` |
| `study-finding.match-item-conflict` | `MatchItemConflict` | `trials` |
| `study-finding.no-fixation-kept` | `NoFixationKept` | `key`, `tally` |

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
| `quarantine.correction-conflict` | `CorrectionConflict` | `first`, `second` |
| `quarantine.item-conflict` | `ItemConflict` | `items` |
| `quarantine.occurrence-conflict` | `OccurrenceConflict` | `occurrences` |
| `quarantine.not-in-inventory` | `NotInInventory` | `participant`, `phase`, `trial`, `occurrence` |
| `quarantine.inventory-item-conflict` | `InventoryItemConflict` | `inventory`, `records` |

### `inventory` — `InventoryError`

| Code | Case | Operands |
|---|---|---|
| `inventory.width` | `Width` | `record`, `expected`, `actual` |
| `inventory.field` | `Field` | `record`, `column`, `value`, `requirement` |
| `inventory.conflict` | `Conflict` | `participant`, `phase`, `trial`, `records`, `columns` |
| `inventory.duplicate-attribute` | `DuplicateAttribute` | `names` |
| `inventory.duplicate-trial` | `DuplicateTrial` | `participant`, `phase`, `trial`, `occurrence` |
| `inventory.record-order` | `RecordOrder` | `trial`, `records` |
| `inventory.shared-record` | `SharedRecord` | `record`, `trials` |
| `inventory.absent-mismatch` | `AbsentMismatch` | `trial`, `disposition`, `records` |
| `inventory.attribute-record` | `AttributeRecord` | `record` |
| `inventory.unknown-record` | `UnknownRecord` | `trial`, `record` |
| `inventory.foreign-record` | `ForeignRecord` | `trial`, `record`, `found` |
| `inventory.unclaimed-record` | `UnclaimedRecord` | `record`, `trial` |
| `inventory.disposition-mismatch` | `DispositionMismatch` | `trial`, `disposition`, `record`, `found` |
| `inventory.item-mismatch` | `ItemMismatch` | `trial`, `record`, `expected`, `actual` |
| `inventory.no-trial-projection` | `NoTrialProjection` | `layout` |
| `inventory.record-items` | `RecordItems` | `trial`, `disposition`, `items` |
| `inventory.attribute-names` | `AttributeNames` | `owner`, `declared`, `found` |
| `inventory.attribute-kind-mismatch` | `AttributeKindMismatch` | `owner`, `name`, `declared`, `found` |

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
| `admission.outside-frame-record` | `OutsideFrameRecord` | `record`, `policy` |
| `admission.correction-conflict` | `CorrectionConflict` | `record`, `first`, `second` |
| `admission.inventory` | `Inventory` | `underlying` |
| `admission.uninventoried-cause` | `UninventoriedCause` | `record`, `cause` |

### `inspection` — `InspectionError`

| Code | Case | Operands |
|---|---|---|
| `inspection.unknown-scale` | `UnknownScale` | `index`, `scales` |
| `inspection.unknown-cell` | `UnknownCell` | `repetition`, `window` |
| `inspection.unknown-reference` | `UnknownReference` | `reference` |
| `inspection.duplicate-reference` | `DuplicateReference` | `reference` |
| `inspection.invalid-page-size` | `InvalidPageSize` | `requested`, `maximum` |
| `inspection.sources` | `Sources` | `refusal` |
| `inspection.input-mismatch` | `InputMismatch` | `result`, `sources` |
| `inspection.components` | `Components` | `underlying` |
| `inspection.plan-mismatch` | `PlanMismatch` | `changes` |
| `inspection.reduction-membership` | `ReductionMembership` | `reference`, `selected`, `members`, `contributing`, `contributors` |
| `inspection.orientation` | `Orientation` | `scale`, `design`, `found` |
| `inspection.no-contrast` | `NoContrast` | `scale` |

### `timeline` — `TimelineError`

| Code | Case | Operands |
|---|---|---|
| `timeline.blank-clock` | `BlankClock` | `value` |

### `moving` — `MovingError`

| Code | Case | Operands |
|---|---|---|
| `moving.no-segments` | `NoSegments` |  |
| `moving.overlapping-segments` | `OverlappingSegments` | `first`, `second` |
| `moving.clock` | `Clock` | `underlying` |
| `moving.frame` | `Frame` | `underlying` |

### `time-quantity` — `TimeQuantityError`

| Code | Case | Operands |
|---|---|---|
| `time-quantity.negative-non-negative-span` | `NegativeNonNegativeSpan` | `value` |
| `time-quantity.non-positive-span` | `NonPositiveSpan` | `value` |
| `time-quantity.negative-non-negative-long` | `NegativeNonNegativeLong` | `value` |
| `time-quantity.non-negative-long-overflow` | `NonNegativeLongOverflow` | `left`, `right` |

### `occupancy` — `OccupancyError`

| Code | Case | Operands |
|---|---|---|
| `occupancy.measure` | `Measure` | `policy`, `underlying` |

### `replication` — `ReplicationError`

| Code | Case | Operands |
|---|---|---|
| `replication.period` | `Period` | `microseconds` |
| `replication.duration` | `Duration` | `index`, `microseconds` |
| `replication.maximum-rows` | `MaximumRows` | `value` |
| `replication.cardinality` | `Cardinality` | `requested`, `maximum` |

### `temporal-support` — `TemporalSupportError`

| Code | Case | Operands |
|---|---|---|
| `temporal-support.non-positive-fixed-period` | `NonPositiveFixedPeriod` | `period` |
| `temporal-support.negative-maximum-gap` | `NegativeMaximumGap` | `maxGap` |
| `temporal-support.negative-edge-support` | `NegativeEdgeSupport` | `edgeSupport` |

### `algorithm-metadata` — `AlgorithmMetadataError`

| Code | Case | Operands |
|---|---|---|
| `algorithm-metadata.empty-algorithm-id` | `EmptyAlgorithmId` | `value` |
| `algorithm-metadata.invalid-doi` | `InvalidDoi` | `value` |
| `algorithm-metadata.invalid-version` | `InvalidVersion` | `major`, `minor`, `patch` |
| `algorithm-metadata.invalid-authors` | `InvalidAuthors` | `authors` |
| `algorithm-metadata.invalid-year` | `InvalidYear` | `year` |
| `algorithm-metadata.empty-citation-title` | `EmptyCitationTitle` | `value` |
| `algorithm-metadata.empty-algorithm-name` | `EmptyAlgorithmName` | `value` |
| `algorithm-metadata.no-citations` | `NoCitations` | `id` |
| `algorithm-metadata.no-assumptions` | `NoAssumptions` | `id` |
| `algorithm-metadata.no-references` | `NoReferences` | `id` |

### `ek-estimation` — `EkEstimationError`

| Code | Case | Operands |
|---|---|---|
| `ek-estimation.invalid-sampling` | `InvalidSampling` | `underlying` |
| `ek-estimation.insufficient-velocities` | `InsufficientVelocities` | `available`, `required` |
| `ek-estimation.degenerate-velocity-spread` | `DegenerateVelocitySpread` | `etaXDegPerSecond`, `etaYDegPerSecond`, `lambda` |

### `merge` — `MergeError`

| Code | Case | Operands |
|---|---|---|
| `merge.interval` | `Interval` | `source`, `leftEventIndex`, `rightEventIndex`, `underlying` |
| `merge.range` | `Range` | `source`, `leftEventIndex`, `rightEventIndex`, `underlying` |
| `merge.event` | `Event` | `source`, `leftEventIndex`, `rightEventIndex`, `underlying` |
| `merge.source-support` | `SourceSupport` | `source`, `underlying` |

### `density-lookup` — `DensityLookupError`

| Code | Case | Operands |
|---|---|---|
| `density-lookup.arithmetic` | `Arithmetic` | `normalization`, `operand`, `value` |
| `density-lookup.surface` | `Surface` | `underlying` |

### `density-point-failure` — `DensityPointFailure`

| Code | Case | Operands |
|---|---|---|
| `density-point-failure.non-finite-point` | `NonFinitePoint` | `x`, `y` |
| `density-point-failure.outside-grid` | `OutsideGrid` | `grid`, `x`, `y` |
| `density-point-failure.trajectory` | `Trajectory` | `reason` |

### `iqr-bandwidth` — `IqrBandwidthError`

| Code | Case | Operands |
|---|---|---|
| `iqr-bandwidth.too-few-points` | `TooFewPoints` | `frame`, `count` |
| `iqr-bandwidth.sigma` | `Sigma` | `frame`, `underlying` |

### `smoother-card` — `SmootherCardError`

| Code | Case | Operands |
|---|---|---|
| `smoother-card.empty-text` | `EmptyText` | `field` |
| `smoother-card.invalid-parameters` | `InvalidParameters` | `names` |

### `comparison-configuration` — `ComparisonConfigurationError`

| Code | Case | Operands |
|---|---|---|
| `comparison-configuration.invalid-projection-directions` | `InvalidProjectionDirections` | `algorithm`, `supplied` |
| `comparison-configuration.invalid-regularisation` | `InvalidRegularisation` | `algorithm`, `supplied` |
| `comparison-configuration.invalid-iteration-count` | `InvalidIterationCount` | `algorithm`, `supplied` |
| `comparison-configuration.invalid-cell-limit` | `InvalidCellLimit` | `algorithm`, `supplied` |
| `comparison-configuration.invalid-probability-floor` | `InvalidProbabilityFloor` | `algorithm`, `supplied` |
| `comparison-configuration.invalid-gap-cost` | `InvalidGapCost` | `algorithm`, `supplied` |

### `crqa-parameter` — `CrqaParameterError`

| Code | Case | Operands |
|---|---|---|
| `crqa-parameter.non-positive-embedding-dimension` | `NonPositiveEmbeddingDimension` | `value` |
| `crqa-parameter.non-positive-embedding-delay` | `NonPositiveEmbeddingDelay` | `value` |
| `crqa-parameter.non-positive-line-minimum` | `NonPositiveLineMinimum` | `value` |
| `crqa-parameter.invalid-target-recurrence-rate` | `InvalidTargetRecurrenceRate` | `value` |

### `crqa` — `CrqaError`

| Code | Case | Operands |
|---|---|---|
| `crqa.geometry` | `Geometry` | `underlying` |
| `crqa.too-short` | `TooShort` | `operand`, `observedFixations`, `requiredFixations`, `embeddingDimension`, `embeddingDelay` |
| `crqa.matrix-too-large` | `MatrixTooLarge` | `leftStates`, `rightStates`, `requiredCells`, `maximumCells` |
| `crqa.non-finite-position` | `NonFinitePosition` | `operand`, `index`, `x`, `y` |
| `crqa.non-finite-embedded-distance` | `NonFiniteEmbeddedDistance` | `leftState`, `rightState`, `value` |
| `crqa.selected-radius` | `SelectedRadius` | `underlying` |

### `fixation-comparison` — `FixationComparisonError`

| Code | Case | Operands |
|---|---|---|
| `fixation-comparison.parameter` | `Parameter` | `name`, `value` |
| `fixation-comparison.frames` | `Frames` | `error` |
| `fixation-comparison.clock` | `Clock` | `error` |
| `fixation-comparison.empty-queries` | `EmptyQueries` |  |
| `fixation-comparison.missing-support` | `MissingSupport` | `requested`, `contributing` |
| `fixation-comparison.matrix-size` | `MatrixSize` | `left`, `right`, `limit` |
| `fixation-comparison.numerical` | `Numerical` | `left`, `right`, `value` |
| `fixation-comparison.non-convergence` | `NonConvergence` | `left`, `right`, `iterations`, `residual`, `tolerance` |
| `fixation-comparison.solver-numerical` | `SolverNumerical` | `left`, `right`, `cost`, `residual` |
| `fixation-comparison.work-limit` | `WorkLimit` | `left`, `right`, `iterations`, `residual`, `limit` |
| `fixation-comparison.score` | `Score` | `error` |

### `overlap-failure` — `OverlapFailure`

| Code | Case | Operands |
|---|---|---|
| `overlap-failure.missing` | `Missing` | `left`, `right` |
| `overlap-failure.nonfinite-distance` | `NonfiniteDistance` | `leftX`, `leftY`, `rightX`, `rightY` |

### `map-scale-failure` — `MapScaleFailure`

| Code | Case | Operands |
|---|---|---|
| `map-scale-failure.missing-left` | `MissingLeft` |  |
| `map-scale-failure.missing-right` | `MissingRight` |  |
| `map-scale-failure.comparison` | `Comparison` | `underlying` |

### `map-comparison` — `MapComparisonError`

| Code | Case | Operands |
|---|---|---|
| `map-comparison.unsupported-method` | `UnsupportedMethod` | `value` |
| `map-comparison.scales` | `Scales` | `values` |
| `map-comparison.grid` | `Grid` | `scale`, `underlying` |
| `map-comparison.incomplete` | `Incomplete` | `requested`, `failures` |
| `map-comparison.score` | `Score` | `underlying` |

### `decomposition` — `DecompositionError`

| Code | Case | Operands |
|---|---|---|
| `decomposition.predictor-name` | `PredictorName` | `value` |
| `decomposition.predictor-keys` | `PredictorKeys` | `keys` |
| `decomposition.geometry` | `Geometry` | `operand`, `underlying` |
| `decomposition.solve` | `Solve` | `keys`, `intercept`, `underlying` |
| `decomposition.numerical` | `Numerical` | `operation`, `value` |

### `pairing` — `PairingError`

| Code | Case | Operands |
|---|---|---|
| `pairing.non-positive-limit` | `NonPositiveLimit` | `value` |

### `session` — `SessionError`

| Code | Case | Operands |
|---|---|---|
| `session.invalid-key` | `InvalidKey` | `value` |
| `session.duplicate-key` | `DuplicateKey` | `role`, `key` |
| `session.missing-key` | `MissingKey` | `role`, `key` |
| `session.frame` | `Frame` | `role`, `key`, `underlying` |
| `session.grid-identity` | `GridIdentity` | `key`, `existingKey`, `underlying` |

### `reduction-policy` — `ReductionPolicyError`

| Code | Case | Operands |
|---|---|---|
| `reduction-policy.non-positive-minimum-successful` | `NonPositiveMinimumSuccessful` | `value` |

### `work-quanta` — `WorkQuantaError`

| Code | Case | Operands |
|---|---|---|
| `work-quanta.invalid-sample-quantum` | `InvalidSampleQuantum` | `value` |

### `evaluation-work` — `EvaluationWorkError`

| Code | Case | Operands |
|---|---|---|
| `evaluation-work.schedule` | `Schedule` | `underlying` |
| `evaluation-work.comparison` | `Comparison` | `underlying` |

### `repetition-mean` — `RepetitionMeanError`

| Code | Case | Operands |
|---|---|---|
| `repetition-mean.incomplete` | `Incomplete` | `level`, `requested`, `successful`, `required` |
| `repetition-mean.mean` | `Mean` | `error` |

### `least-squares` — `LeastSquaresError`

| Code | Case | Operands |
|---|---|---|
| `least-squares.shape` | `Shape` | `rows`, `columns`, `response`, `rowWidths` |
| `least-squares.non-finite` | `NonFinite` | `row`, `column`, `value` |
| `least-squares.rank-tolerance` | `RankTolerance` | `value` |
| `least-squares.rank-deficient` | `RankDeficient` | `column`, `pivot`, `threshold` |
| `least-squares.column-arithmetic` | `ColumnArithmetic` | `operation`, `column` |
| `least-squares.row-arithmetic` | `RowArithmetic` | `operation`, `row` |

### `rng` — `RngError`

| Code | Case | Operands |
|---|---|---|
| `rng.non-positive-bound` | `NonPositiveBound` | `value` |

### `epoch` — `EpochError`

| Code | Case | Operands |
|---|---|---|
| `epoch.clock` | `Clock` | `trial`, `selector`, `underlying` |
| `epoch.anchor-matches` | `AnchorMatches` | `trial`, `selector`, `count` |
| `epoch.anchor-overflow` | `AnchorOverflow` | `trial`, `selector`, `anchor`, `window` |
| `epoch.duration-overflow` | `DurationOverflow` | `trial`, `window`, `durationMicros` |
| `epoch.non-divisible` | `NonDivisible` | `trial`, `window`, `width`, `remainderMicros` |
| `epoch.bin-limit` | `BinLimit` | `trial`, `window`, `width`, `requested`, `maximum` |

### `point-sampling` — `PointSamplingError`

| Code | Case | Operands |
|---|---|---|
| `point-sampling.boundaries` | `Boundaries` | `values` |
| `point-sampling.clock` | `Clock` | `error` |
| `point-sampling.frames` | `Frames` | `error` |
| `point-sampling.missing-template` | `MissingTemplate` | `participant`, `stimulus` |
| `point-sampling.ambiguous-template` | `AmbiguousTemplate` | `participant`, `stimulus`, `indices` |
| `point-sampling.duplicate-source` | `DuplicateSource` | `indices` |
| `point-sampling.density` | `Density` | `templateIndex`, `error` |
| `point-sampling.point` | `Point` | `error` |
| `point-sampling.insufficient` | `Insufficient` | `level`, `requested`, `successful`, `minimum` |
| `point-sampling.non-finite` | `NonFinite` | `level`, `index`, `value` |

### `recipe-parameter` — `RecipeParameterError`

| Code | Case | Operands |
|---|---|---|
| `recipe-parameter.geometry` | `Geometry` | `error` |
| `recipe-parameter.time` | `Time` | `error` |
| `recipe-parameter.configuration` | `Configuration` | `error` |
| `recipe-parameter.temporal` | `Temporal` | `error` |
| `recipe-parameter.reduction` | `Reduction` | `error` |
| `recipe-parameter.synchronization` | `Synchronization` | `error` |
| `recipe-parameter.recording` | `Recording` | `error` |

### `repetition-plan` — `RepetitionPlanError`

| Code | Case | Operands |
|---|---|---|
| `repetition-plan.projection-ids` | `ProjectionIds` | `values` |
| `repetition-plan.duplicate-layout` | `DuplicateLayout` | `id` |
| `repetition-plan.missing-layout` | `MissingLayout` | `id` |
| `repetition-plan.rules` | `Rules` | `role`, `values` |
| `repetition-plan.overlapping-relations` | `OverlappingRelations` | `matched`, `controls` |
| `repetition-plan.grid` | `Grid` | `row`, `underlying` |
| `repetition-plan.specification` | `Specification` | `underlying` |

### `diagnostic-code` — `DiagnosticCodeError`

| Code | Case | Operands |
|---|---|---|
| `diagnostic-code.invalid-family` | `InvalidFamily` | `family` |
| `diagnostic-code.invalid-name` | `InvalidName` | `name` |

### `fixation-entropy` — `FixationEntropyError`

| Code | Case | Operands |
|---|---|---|
| `fixation-entropy.frame-mismatch` | `FrameMismatch` | `path`, `target`, `underlying` |
| `fixation-entropy.degenerate-lattice` | `DegenerateLattice` | `nx`, `ny` |
| `fixation-entropy.invalid-padding` | `InvalidPadding` | `padding` |
| `fixation-entropy.lattice-bounds` | `LatticeBounds` | `underlying` |
| `fixation-entropy.fixation-outside-lattice` | `FixationOutsideLattice` | `fixation`, `x`, `y` |
| `fixation-entropy.no-occupancy` | `NoOccupancy` | `fixations`, `outside` |
| `fixation-entropy.occupancy` | `Occupancy` | `underlying` |
| `fixation-entropy.bandwidth` | `Bandwidth` | `underlying` |
| `fixation-entropy.estimate` | `Estimate` | `sigma`, `underlying` |
| `fixation-entropy.no-scales` | `NoScales` |  |
| `fixation-entropy.duplicate-scale` | `DuplicateScale` | `sigma` |
| `fixation-entropy.scale-grid` | `ScaleGrid` | `sigma`, `underlying` |
| `fixation-entropy.missing-scale-weight` | `MissingScaleWeight` | `sigma` |
| `fixation-entropy.unknown-scale-weight` | `UnknownScaleWeight` | `sigma` |
| `fixation-entropy.invalid-scale-weight` | `InvalidScaleWeight` | `sigma`, `weight` |
| `fixation-entropy.degenerate-scale-weights` | `DegenerateScaleWeights` | `total` |
| `fixation-entropy.non-finite-position` | `NonFinitePosition` | `fixation`, `x`, `y` |

### `template` — `TemplateError`

| Code | Case | Operands |
|---|---|---|
| `template.basis` | `Basis` | `id`, `columns`, `responseUnit` |
| `template.definition` | `Definition` | `splitUnit`, `responseUnit` |
| `template.observation` | `Observation` | `key`, `splitGroup`, `matchGroup`, `response` |
| `template.features` | `Features` | `key`, `features` |
| `template.width` | `Width` | `key`, `expected`, `actual` |
| `template.duplicate-key` | `DuplicateKey` | `key` |
| `template.split` | `Split` | `requested`, `available`, `training`, `heldOut`, `excluded` |
| `template.geometry` | `Geometry` | `key`, `underlying` |
| `template.feature` | `Feature` | `key`, `underlying` |
| `template.fit` | `Fit` | `trainingHash`, `underlying` |
| `template.route` | `Route` | `method`, `route` |
| `template.identity` | `Identity` | `expected`, `actual` |
| `template.receipt` | `Receipt` | `expected`, `actual`, `reason` |
| `template.numerical` | `Numerical` | `key`, `prediction`, `response` |
| `template.evaluation` | `Evaluation` | `failed`, `total` |
| `template.aggregate` | `Aggregate` | `operation`, `value` |

### `codec` — `CodecError`

| Code | Case | Operands |
|---|---|---|
| `codec.invalid-json` | `InvalidJson` | `input`, `reason` |
| `codec.field` | `Field` | `path`, `input`, `reason` |
| `codec.schema` | `Schema` | `expected`, `found` |
| `codec.definition` | `Definition` | `underlying` |
| `codec.duplicate-keys` | `DuplicateKeys` | `schema`, `indices` |
| `codec.missing-identity` | `MissingIdentity` | `kind`, `id` |
| `codec.identity-conflict` | `IdentityConflict` | `kind`, `id`, `existing`, `incoming` |
| `codec.missing-method` | `MissingMethod` | `method` |
| `codec.duplicate-method` | `DuplicateMethod` | `method` |
| `codec.missing-key-schema` | `MissingKeySchema` | `schema` |
| `codec.duplicate-key-schema` | `DuplicateKeySchema` | `schema` |
| `codec.entry` | `Entry` | `path`, `underlying` |
| `codec.unsupported` | `Unsupported` | `path`, `reason` |
| `codec.input-identity` | `InputIdentity` | `declared`, `reconstructed` |
| `codec.admission` | `Admission` | `underlying` |
| `codec.recording` | `Recording` | `path`, `underlying` |
| `codec.support` | `Support` | `path`, `underlying` |
| `codec.synchronization` | `Synchronization` | `path`, `underlying` |
| `codec.input` | `Input` | `underlying` |
| `codec.temporal` | `Temporal` | `underlying` |
| `codec.sample-bound` | `SampleBound` | `path`, `samples`, `maximum` |
| `codec.synchronization-fit` | `SynchronizationFit` | `path`, `declaredOffsetMicros`, `declaredDrift`, `refitOffsetMicros`, `refitDrift` |
| `codec.reconstruction` | `Reconstruction` | `underlying` |
| `codec.result` | `Result` | `underlying` |
| `codec.score-components` | `ScoreComponents` | `expected`, `found` |
| `codec.missing-result-codec` | `MissingResultCodec` | `method` |
| `codec.duplicate-result-codec` | `DuplicateResultCodec` | `method` |
| `codec.payload` | `Payload` | `path`, `underlying` |
| `codec.missing-payload` | `MissingPayload` | `path`, `reference` |
| `codec.manifest` | `Manifest` | `underlying` |
| `codec.text` | `Text` | `offset`, `reason` |
| `codec.unsupported-schema` | `UnsupportedSchema` | `role`, `found`, `supported` |
| `codec.derived` | `Derived` | `path`, `declared`, `derived` |
| `codec.recording-result` | `RecordingResult` | `underlying` |
| `codec.temporal-result` | `TemporalResult` | `underlying` |
| `codec.non-canonical` | `NonCanonical` | `path`, `found`, `canonical`, `rule` |
| `codec.report` | `Report` | `underlying` |
| `codec.report-spec` | `ReportSpec` | `underlying` |
| `codec.covariates` | `Covariates` | `underlying` |

### `resolve` — `ResolveError`

| Code | Case | Operands |
|---|---|---|
| `resolve.missing-manifest` | `MissingManifest` | `address` |
| `resolve.unreadable-manifest` | `UnreadableManifest` | `address`, `reason` |
| `resolve.manifest-digest` | `ManifestDigest` | `address`, `actual` |
| `resolve.manifest-decode` | `ManifestDecode` | `address`, `underlying` |
| `resolve.refused-manifest` | `RefusedManifest` | `address`, `failure` |
| `resolve.missing` | `Missing` | `entry` |
| `resolve.unreadable` | `Unreadable` | `entry`, `reason` |
| `resolve.refused` | `Refused` | `entry`, `failure` |
| `resolve.length` | `Length` | `entry`, `declared`, `actual` |
| `resolve.digest` | `Digest` | `entry`, `declared`, `actual` |
| `resolve.text` | `Text` | `entry`, `offset` |
| `resolve.syntax` | `Syntax` | `entry`, `reason` |
| `resolve.schema` | `Schema` | `entry`, `declared`, `found` |
| `resolve.decode` | `Decode` | `entry`, `underlying` |
| `resolve.identity` | `Identity` | `entry`, `declared`, `reconstructed` |
| `resolve.relation` | `Relation` | `relation`, `mismatch` |

### `source-failure` — `SourceFailure`

| Code | Case | Operands |
|---|---|---|
| `source-failure.missing` | `Missing` |  |
| `source-failure.unreadable` | `Unreadable` | `reason` |
| `source-failure.outside-root` | `OutsideRoot` | `location`, `root` |
| `source-failure.not-regular-file` | `NotRegularFile` | `location` |
| `source-failure.oversize` | `Oversize` | `location`, `size`, `limit` |

### `relation` — `RelationMismatch`

| Code | Case | Operands |
|---|---|---|
| `relation.prerequisites` | `Prerequisites` | `errors` |
| `relation.result-input` | `ResultInput` | `expected`, `found` |
| `relation.description` | `Description` | `changes` |
| `relation.admission` | `Admission` | `error` |
| `relation.refused-admission` | `RefusedAdmission` |  |
| `relation.base-study` | `BaseStudy` | `expected`, `found` |
| `relation.recording-identity` | `RecordingIdentity` | `expected`, `found` |
| `relation.unreferenced-payload` | `UnreferencedPayload` | `reference` |
| `relation.unavailable` | `Unavailable` | `endpoints` |
| `relation.recording-prerequisites` | `RecordingPrerequisites` | `errors` |
| `relation.temporal-prerequisites` | `TemporalPrerequisites` | `errors` |
| `relation.report-spec` | `ReportSpec` | `report`, `stored` |
| `relation.report-binding` | `ReportBinding` | `field`, `bound`, `stored` |
| `relation.report-input` | `ReportInput` | `input`, `computed` |
| `relation.report-ledger` | `ReportLedger` | `ledger`, `input` |
| `relation.report-members` | `ReportMembers` | `scale`, `unknown` |

### `manifest` — `ManifestError`

| Code | Case | Operands |
|---|---|---|
| `manifest.invalid-name` | `InvalidName` | `value` |
| `manifest.duplicate-name` | `DuplicateName` | `name` |
| `manifest.media-mismatch` | `MediaMismatch` | `name`, `role`, `media` |
| `manifest.negative-length` | `NegativeLength` | `name`, `length` |
| `manifest.identity-presence` | `IdentityPresence` | `name`, `role`, `identity` |
| `manifest.payload-declaration` | `PayloadDeclaration` | `name`, `role`, `schema`, `layout` |
| `manifest.layout-length` | `LayoutLength` | `name`, `declared`, `layout` |
| `manifest.unknown-entry` | `UnknownEntry` | `relation`, `name` |
| `manifest.role-mismatch` | `RoleMismatch` | `relation`, `name`, `expected`, `found` |
| `manifest.payload-owner` | `PayloadOwner` | `relation`, `schema` |
| `manifest.duplicate-relation` | `DuplicateRelation` | `relation` |
| `manifest.relation-count` | `RelationCount` | `name`, `kind`, `count`, `expected` |

### `payload` — `PayloadError`

| Code | Case | Operands |
|---|---|---|
| `payload.empty-shape` | `EmptyShape` |  |
| `payload.negative-extent` | `NegativeExtent` | `axis`, `extent` |
| `payload.too-large` | `TooLarge` | `element`, `shape`, `maximumBytes` |
| `payload.count` | `Count` | `layout`, `values` |
| `payload.element` | `Element` | `declared`, `requested` |
| `payload.length` | `Length` | `declared`, `actual` |
| `payload.digest` | `Digest` | `declared`, `actual` |
| `payload.not-a-number` | `NotANumber` | `index` |

### `byte-digest` — `ByteDigestError`

| Code | Case | Operands |
|---|---|---|
| `byte-digest.wrong-length` | `WrongLength` | `value`, `length` |
| `byte-digest.invalid-character` | `InvalidCharacter` | `value`, `index`, `character` |

### `report` — `ReportFinding`

| Code | Case | Operands |
|---|---|---|
| `report.empty-group` | `EmptyGroup` | `group`, `role` |
| `report.unpaired-participant` | `UnpairedParticipant` | `participant`, `stratum`, `role`, `missing` |
| `report.missing-covariate` | `MissingCovariate` | `key`, `term` |
| `report.unknown-predicate` | `UnknownPredicate` | `key`, `term` |
| `report.below-minimum` | `BelowMinimum` | `participant`, `group`, `role`, `queries`, `required` |
| `report.undefined-window-share` | `UndefinedWindowShare` | `key`, `measure` |
| `report.covariate-type` | `CovariateType` | `key`, `covariate`, `raw`, `expected` |

### `report-error` — `ReportError`

| Code | Case | Operands |
|---|---|---|
| `report-error.unknown-scale` | `UnknownScale` | `scale`, `scales` |
| `report-error.scale-mismatch` | `ScaleMismatch` | `spec`, `table` |
| `report-error.unknown-component` | `UnknownComponent` | `component`, `available` |
| `report-error.undescribed-scores` | `UndescribedScores` | `requested` |
| `report-error.unknown-covariate` | `UnknownCovariate` | `covariate`, `declared` |
| `report-error.covariate-mismatch` | `CovariateMismatch` | `covariate`, `report`, `table` |
| `report-error.plan-mismatch` | `PlanMismatch` | `changes` |
| `report-error.input-mismatch` | `InputMismatch` | `result`, `input` |
| `report-error.stale-binding` | `StaleBinding` | `field`, `bound`, `current` |
| `report-error.malformed-digest` | `MalformedDigest` | `field`, `value` |
| `report-error.duplicate-query` | `DuplicateQuery` | `key` |
| `report-error.component-count` | `ComponentCount` | `key`, `components`, `found` |
| `report-error.blank-participant` | `BlankParticipant` | `key` |
| `report-error.invalid-query` | `InvalidQuery` | `key`, `reason` |
| `report-error.inconsistent-accounting` | `InconsistentAccounting` | `role`, `eligible`, `kept`, `filteredOut`, `unknownPredicate`, `failed`, `missingGroupAttribute` |
| `report-error.inconsistent-cell` | `InconsistentCell` | `group`, `role`, `component`, `reason` |
| `report-error.components` | `Components` | `underlying` |
| `report-error.unbound-covariates` | `UnboundCovariates` | `covariates` |

### `report-spec` — `SpecError`

| Code | Case | Operands |
|---|---|---|
| `report-spec.blank-id` | `BlankId` | `value` |
| `report-spec.negative-scale` | `NegativeScale` | `scale` |
| `report-spec.invalid-selection` | `InvalidSelection` | `roles`, `components` |
| `report-spec.invalid-minimum` | `InvalidMinimum` | `value` |
| `report-spec.non-finite-threshold` | `NonFiniteThreshold` | `term`, `threshold` |
| `report-spec.empty-levels` | `EmptyLevels` | `term` |
| `report-spec.undeclared-level` | `UndeclaredLevel` | `term`, `level`, `declared` |
| `report-spec.invalid-bin` | `InvalidBin` | `label`, `from`, `until` |
| `report-spec.invalid-bins` | `InvalidBins` | `labels` |
| `report-spec.duplicate-grouping` | `DuplicateGrouping` | `terms` |
| `report-spec.covariate-declarations` | `CovariateDeclarations` | `covariate`, `declared` |
| `report-spec.contrast-term` | `ContrastTerm` | `term`, `groupings` |
| `report-spec.contrast-levels` | `ContrastLevels` | `term`, `minuend`, `subtrahend`, `declared` |

### `covariate` — `CovariateError`

| Code | Case | Operands |
|---|---|---|
| `covariate.blank-name` | `BlankName` | `value` |
| `covariate.blank-unit` | `BlankUnit` | `symbol` |
| `covariate.invalid-levels` | `InvalidLevels` | `levels`, `repeated` |
| `covariate.duplicate-covariate` | `DuplicateCovariate` | `names` |
| `covariate.duplicate-key` | `DuplicateKey` | `key` |
| `covariate.unknown-attribute` | `UnknownAttribute` | `covariate`, `declared` |
| `covariate.incompatible-kind` | `IncompatibleKind` | `covariate`, `declared`, `attribute` |
| `covariate.no-trial-projection` | `NoTrialProjection` | `layout` |

### `result-table` — `ResultTableError`

| Code | Case | Operands |
|---|---|---|
| `result-table.schema` | `Schema` | `columns`, `reason` |
| `result-table.cell` | `Cell` | `row`, `column`, `value`, `reason` |
| `result-table.width` | `Width` | `row`, `expected`, `actual` |
| `result-table.context` | `Context` | `operand`, `reason` |

### `detector-validation` — `DetectorValidationError`

| Code | Case | Operands |
|---|---|---|
| `detector-validation.label-count-mismatch` | `LabelCountMismatch` | `referenceCount`, `predictedCount` |
| `detector-validation.empty-truth-denominator` | `EmptyTruthDenominator` | `label` |
| `detector-validation.empty-prediction-denominator` | `EmptyPredictionDenominator` | `label` |
| `detector-validation.no-matched-events` | `NoMatchedEvents` | `referenceCount`, `predictedCount` |
| `detector-validation.no-centred-event-pairs` | `NoCentredEventPairs` | `referenceCount`, `predictedCount` |
| `detector-validation.no-peak-velocity-pairs` | `NoPeakVelocityPairs` | `referenceCount`, `predictedCount` |
| `detector-validation.event-clock-mismatch` | `EventClockMismatch` | `label`, `reference`, `predicted` |
| `detector-validation.invalid-numeric-metric` | `InvalidNumericMetric` | `name`, `underlying` |

### `synthetic-generation` — `SyntheticGenerationError`

| Code | Case | Operands |
|---|---|---|
| `synthetic-generation.invalid-clock-scale` | `InvalidClockScale` | `scale` |
| `synthetic-generation.frame` | `Frame` | `underlying` |
| `synthetic-generation.recording` | `Recording` | `underlying` |
| `synthetic-generation.geometry` | `Geometry` | `underlying` |
| `synthetic-generation.support` | `Support` | `underlying` |
| `synthetic-generation.time` | `Time` | `underlying` |

### `validation-artifact` — `ValidationArtifactError`

| Code | Case | Operands |
|---|---|---|
| `validation-artifact.empty-build-field` | `EmptyBuildField` | `name`, `value` |
| `validation-artifact.invalid-build-field` | `InvalidBuildField` | `name`, `value`, `requirement` |
| `validation-artifact.invalid-source-revision` | `InvalidSourceRevision` | `name`, `value` |
| `validation-artifact.empty-oracle-content` | `EmptyOracleContent` | `path`, `contentLength` |
| `validation-artifact.missing-oracle-schema` | `MissingOracleSchema` | `path`, `expectedSchema` |
| `validation-artifact.synthetic` | `Synthetic` | `underlying` |
| `validation-artifact.metrics` | `Metrics` | `underlying` |

### `fixation-import` — `FixationImportError`

| Code | Case | Operands |
|---|---|---|
| `fixation-import.csv` | `Csv` | `underlying` |
| `fixation-import.columns` | `Columns` | `names` |
| `fixation-import.header` | `Header` | `found`, `required` |
| `fixation-import.incomplete` | `Incomplete` | `rejectedRows` |
| `fixation-import.participant-scope` | `ParticipantScope` | `rules` |
| `fixation-import.inventory` | `Inventory` | `errors` |
| `fixation-import.no-item-column` | `NoItemColumn` | `inventory`, `fixations` |

### `fixation-row` — `FixationRowError`

| Code | Case | Operands |
|---|---|---|
| `fixation-row.width` | `Width` | `expected`, `actual` |
| `fixation-row.key` | `Key` | `reason` |
| `fixation-row.number` | `Number` | `column`, `value`, `requirement` |
| `fixation-row.time` | `Time` | `onset`, `duration`, `unit`, `reason` |
| `fixation-row.position` | `Position` | `x`, `y`, `frame` |
| `fixation-row.event` | `Event` | `reason` |
| `fixation-row.trial` | `Trial` | `rows`, `cause` |

### `tidy-csv` — `TidyCsvError`

| Code | Case | Operands |
|---|---|---|
| `tidy-csv.missing-header` | `MissingHeader` |  |
| `tidy-csv.unexpected-header` | `UnexpectedHeader` | `expected`, `actual` |
| `tidy-csv.wrong-column-count` | `WrongColumnCount` | `line`, `expected`, `actual` |
| `tidy-csv.unexpected-schema` | `UnexpectedSchema` | `line`, `expected`, `actual` |
| `tidy-csv.missing-required-context` | `MissingRequiredContext` | `line`, `column` |
| `tidy-csv.invalid-value-cells` | `InvalidValueCells` | `line`, `status`, `value`, `missingReason` |
| `tidy-csv.invalid-scientific-field` | `InvalidScientificField` | `line`, `column`, `value`, `reason` |
| `tidy-csv.malformed-csv` | `MalformedCsv` | `characterIndex`, `character` |
| `tidy-csv.unterminated-quoted-field` | `UnterminatedQuotedField` | `characterIndex` |

### `tidy-result` — `TidyResultError`

| Code | Case | Operands |
|---|---|---|
| `tidy-result.blank-participant` | `BlankParticipant` | `value` |
| `tidy-result.blank-trial` | `BlankTrial` | `participant`, `value` |
| `tidy-result.blank-condition-key` | `BlankConditionKey` | `participant`, `trial`, `index`, `value` |
| `tidy-result.blank-condition-value` | `BlankConditionValue` | `participant`, `trial`, `index`, `key`, `value` |
| `tidy-result.duplicate-condition-key` | `DuplicateConditionKey` | `participant`, `trial`, `key`, `firstIndex`, `secondIndex` |
| `tidy-result.no-validated-recording` | `NoValidatedRecording` | `source` |
| `tidy-result.frame-conflict` | `FrameConflict` | `source`, `underlying` |
| `tidy-result.clock-conflict` | `ClockConflict` | `source`, `underlying` |
| `tidy-result.analysis-recording-mismatch` | `AnalysisRecordingMismatch` | `source`, `assignment`, `detection` |
| `tidy-result.missing-synchronization` | `MissingSynchronization` | `source`, `nativeClock`, `analysisClock` |
| `tidy-result.synchronization-mismatch` | `SynchronizationMismatch` | `source`, `nativeClock`, `analysisClock`, `evidenceSource`, `evidenceTarget` |
| `tidy-result.custom-detector-cannot-produce-scientific-export` | `CustomDetectorCannotProduceScientificExport` | `source`, `detector` |
| `tidy-result.temporal-support-mismatch` | `TemporalSupportMismatch` | `source`, `detection`, `assignment` |
| `tidy-result.missing-detector-provenance` | `MissingDetectorProvenance` | `source`, `detector` |
| `tidy-result.blank-detector-parameter` | `BlankDetectorParameter` | `source`, `index`, `name` |
| `tidy-result.duplicate-detector-parameter` | `DuplicateDetectorParameter` | `source`, `name`, `firstIndex`, `secondIndex` |
| `tidy-result.blank-warning` | `BlankWarning` | `source`, `index`, `warning` |
| `tidy-result.blank-operation` | `BlankOperation` | `source`, `index`, `operation` |
| `tidy-result.invalid-operation-parameters` | `InvalidOperationParameters` | `source`, `operationIndex`, `operation`, `underlying` |

### `contrast-export` — `ContrastExportError`

| Code | Case | Operands |
|---|---|---|
| `contrast-export.codec` | `Codec` | `underlying` |
| `contrast-export.plan-mismatch` | `PlanMismatch` | `expected`, `actual` |
| `contrast-export.components` | `Components` | `expected`, `actual` |
| `contrast-export.values` | `Values` | `operand`, `components`, `values` |

### `result-export` — `ResultExportError`

| Code | Case | Operands |
|---|---|---|
| `result-export.schema` | `Schema` | `columns`, `reason` |
| `result-export.cell` | `Cell` | `row`, `column`, `value`, `reason` |
| `result-export.width` | `Width` | `row`, `expected`, `actual` |
| `result-export.codec` | `Codec` | `underlying` |
| `result-export.score` | `Score` | `underlying` |
| `result-export.context` | `Context` | `operand`, `reason` |

### `template-csv` — `TemplateCsvError`

| Code | Case | Operands |
|---|---|---|
| `template-csv.csv` | `Csv` | `underlying` |
| `template-csv.header` | `Header` | `expected`, `actual` |
| `template-csv.row` | `Row` | `index`, `fields`, `reason` |
| `template-csv.fit` | `Fit` | `underlying` |

### `delimited-schema` — `DelimitedSchemaError`

| Code | Case | Operands |
|---|---|---|
| `delimited-schema.no-tracked-validity-token` | `NoTrackedValidityToken` |  |
| `delimited-schema.blank-validity-token` | `BlankValidityToken` | `meaning` |
| `delimited-schema.ambiguous-validity-token` | `AmbiguousValidityToken` | `token`, `meanings` |
| `delimited-schema.blank-column-name` | `BlankColumnName` | `index` |
| `delimited-schema.duplicate-logical-column` | `DuplicateLogicalColumn` | `name`, `firstIndex`, `secondIndex` |
| `delimited-schema.empty-supplied-header` | `EmptySuppliedHeader` |  |
| `delimited-schema.blank-supplied-header` | `BlankSuppliedHeader` | `index` |
| `delimited-schema.duplicate-supplied-header` | `DuplicateSuppliedHeader` | `name`, `firstIndex`, `secondIndex` |
| `delimited-schema.unknown-missing-token-column` | `UnknownMissingTokenColumn` | `column` |
| `delimited-schema.missing-validity-overlap` | `MissingValidityOverlap` | `token` |

### `psychology-workflow` — `PsychologyWorkflowError`

| Code | Case | Operands |
|---|---|---|
| `psychology-workflow.analysis-failed` | `AnalysisFailed` | `source`, `underlying` |
| `psychology-workflow.blank-source-name` | `BlankSourceName` | `value` |
| `psychology-workflow.invalid-study` | `InvalidStudy` | `underlying` |
| `psychology-workflow.milliseconds-outside-range` | `MillisecondsOutsideRange` | `operand`, `value` |
| `psychology-workflow.blank-clock` | `BlankClock` | `source`, `role`, `value` |
| `psychology-workflow.invalid-display` | `InvalidDisplay` | `source`, `underlying` |
| `psychology-workflow.invalid-viewing` | `InvalidViewing` | `source`, `underlying` |
| `psychology-workflow.invalid-interpolation` | `InvalidInterpolation` | `source`, `underlying` |
| `psychology-workflow.invalid-velocity` | `InvalidVelocity` | `source`, `underlying` |
| `psychology-workflow.invalid-detector-configuration` | `InvalidDetectorConfiguration` | `source`, `underlying` |
| `psychology-workflow.invalid-sync-mark` | `InvalidSyncMark` | `underlying` |
| `psychology-workflow.invalid-synchronization` | `InvalidSynchronization` | `source`, `underlying` |
| `psychology-workflow.blank-aoi-id` | `BlankAoiId` | `value` |
| `psychology-workflow.blank-aoi-label` | `BlankAoiLabel` | `id`, `value` |
| `psychology-workflow.non-finite-aoi-bounds` | `NonFiniteAoiBounds` | `id`, `xMin`, `yMin`, `xMax`, `yMax` |
| `psychology-workflow.degenerate-aoi-bounds` | `DegenerateAoiBounds` | `id`, `xMin`, `yMin`, `xMax`, `yMax` |
| `psychology-workflow.no-areas` | `NoAreas` | `source` |
| `psychology-workflow.duplicate-area` | `DuplicateArea` | `source`, `id`, `firstIndex`, `secondIndex` |
| `psychology-workflow.area-outside-display` | `AreaOutsideDisplay` | `source`, `id`, `x`, `y`, `frame` |
| `psychology-workflow.invalid-source-metadata` | `InvalidSourceMetadata` | `source`, `index`, `key`, `value` |
| `psychology-workflow.reserved-source-metadata` | `ReservedSourceMetadata` | `source`, `index`, `key` |
| `psychology-workflow.duplicate-source-metadata` | `DuplicateSourceMetadata` | `source`, `key`, `firstIndex`, `secondIndex` |
| `psychology-workflow.schema-failed` | `SchemaFailed` | `source`, `underlying` |
| `psychology-workflow.import-failed` | `ImportFailed` | `source`, `diagnostics` |
| `psychology-workflow.synchronized-recording-failed` | `SynchronizedRecordingFailed` | `source`, `underlying` |
| `psychology-workflow.angular-frame-failed` | `AngularFrameFailed` | `source`, `underlying` |
| `psychology-workflow.warp-failed` | `WarpFailed` | `source`, `underlying` |
| `psychology-workflow.preprocessing-cardinality` | `PreprocessingCardinality` | `source`, `expected`, `actual` |
| `psychology-workflow.preprocessed-recording-failed` | `PreprocessedRecordingFailed` | `source`, `underlying` |
| `psychology-workflow.detection-failed` | `DetectionFailed` | `source`, `underlying` |
| `psychology-workflow.area-warp-undefined` | `AreaWarpUndefined` | `source`, `id`, `corner` |
| `psychology-workflow.area-region-failed` | `AreaRegionFailed` | `source`, `id`, `underlying` |
| `psychology-workflow.area-construction-failed` | `AreaConstructionFailed` | `source`, `underlying` |
| `psychology-workflow.assignment-failed` | `AssignmentFailed` | `source`, `underlying` |
| `psychology-workflow.tidy-result-failed` | `TidyResultFailed` | `source`, `underlying` |
| `psychology-workflow.export-failed` | `ExportFailed` | `source`, `underlying` |
| `psychology-workflow.export-round-trip-mismatch` | `ExportRoundTripMismatch` | `source` |

### `sha256` — `Sha256Error`

| Code | Case | Operands |
|---|---|---|
| `sha256.wrong-length` | `WrongLength` | `operand`, `actual` |
| `sha256.invalid-character` | `InvalidCharacter` | `operand`, `index`, `value` |

### `edf2asc-provenance` — `Edf2AscProvenanceError`

| Code | Case | Operands |
|---|---|---|
| `edf2asc-provenance.blank-converter-name` | `BlankConverterName` | `value` |
| `edf2asc-provenance.blank-converter-version` | `BlankConverterVersion` | `converter` |
| `edf2asc-provenance.blank-converter-argument` | `BlankConverterArgument` | `index` |
| `edf2asc-provenance.invalid-converter-argument` | `InvalidConverterArgument` | `index`, `value` |
| `edf2asc-provenance.blank-platform` | `BlankPlatform` | `value` |
| `edf2asc-provenance.unsuccessful-conversion` | `UnsuccessfulConversion` | `exitCode`, `ascDigest` |
| `edf2asc-provenance.missing-edf-digest` | `MissingEdfDigest` | `ascDigest` |
| `edf2asc-provenance.missing-conversion-receipt` | `MissingConversionReceipt` | `ascDigest` |

### `asc-sample-materialization` — `AscSampleMaterializationError`

| Code | Case | Operands |
|---|---|---|
| `asc-sample-materialization.coordinate-mode-is-not-screen-pixels` | `CoordinateModeIsNotScreenPixels` | `eye`, `mode`, `frame` |
| `asc-sample-materialization.pixel-value-outside-double-range` | `PixelValueOutsideDoubleRange` | `eye`, `x`, `y`, `frame` |

### `asc-source-line` — `AscSourceLineError`

| Code | Case | Operands |
|---|---|---|
| `asc-source-line.blank-source` | `BlankSource` | `value` |
| `asc-source-line.non-positive-line-number` | `NonPositiveLineNumber` | `source`, `number` |
| `asc-source-line.negative-byte-offset` | `NegativeByteOffset` | `source`, `offset` |
| `asc-source-line.non-positive-line-limit` | `NonPositiveLineLimit` | `bytes` |
| `asc-source-line.line-too-long` | `LineTooLong` | `source`, `line`, `limit`, `actual` |
| `asc-source-line.embedded-line-terminator` | `EmbeddedLineTerminator` | `source`, `line`, `index`, `value` |

### `asc-stream-configuration` — `AscStreamConfigurationError`

| Code | Case | Operands |
|---|---|---|
| `asc-stream-configuration.blank-source` | `BlankSource` | `value` |
| `asc-stream-configuration.invalid-line-limit` | `InvalidLineLimit` | `bytes`, `cause` |
| `asc-stream-configuration.non-positive-read-chunk` | `NonPositiveReadChunk` | `bytes` |

### `asc-native-timeline` — `AscNativeTimelineError`

| Code | Case | Operands |
|---|---|---|
| `asc-native-timeline.fractional-microsecond` | `FractionalMicrosecond` | `source`, `line`, `field`, `milliseconds` |
| `asc-native-timeline.instant-outside-long-range` | `InstantOutsideLongRange` | `source`, `line`, `field`, `milliseconds` |
| `asc-native-timeline.invalid-timeline` | `InvalidTimeline` | `clock`, `underlying` |

### `asc-performance-validation` — `AscPerformanceValidationError`

| Code | Case | Operands |
|---|---|---|
| `asc-performance-validation.blank` | `Blank` | `operand`, `value` |
| `asc-performance-validation.non-positive` | `NonPositive` | `operand`, `value` |
| `asc-performance-validation.negative` | `Negative` | `operand`, `value` |
| `asc-performance-validation.allocation-measurement-unavailable` | `AllocationMeasurementUnavailable` | `operand`, `reason` |

### `eyelink-session-config` — `EyeLinkAscSessionConfigError`

| Code | Case | Operands |
|---|---|---|
| `eyelink-session-config.blank-frame` | `BlankFrame` | `value` |
| `eyelink-session-config.blank-clock` | `BlankClock` | `value` |

### `eyelink-oracle` — `EyeLinkOracleError`

| Code | Case | Operands |
|---|---|---|
| `eyelink-oracle.invalid-preamble` | `InvalidPreamble` | `source`, `actual` |
| `eyelink-oracle.invalid-header` | `InvalidHeader` | `source`, `line`, `expected`, `actual` |
| `eyelink-oracle.wrong-field-count` | `WrongFieldCount` | `source`, `line`, `expected`, `actual` |
| `eyelink-oracle.invalid-escape` | `InvalidEscape` | `source`, `line`, `field`, `index`, `value` |
| `eyelink-oracle.invalid-value` | `InvalidValue` | `source`, `line`, `field`, `value`, `expected` |
| `eyelink-oracle.invalid-digest` | `InvalidDigest` | `source`, `line`, `field`, `detail` |
| `eyelink-oracle.invalid-descriptor` | `InvalidDescriptor` | `oracleId`, `field`, `value`, `expected` |
| `eyelink-oracle.invalid-fact` | `InvalidFact` | `recordOrdinal`, `fieldOrdinal`, `field`, `value`, `expected` |
| `eyelink-oracle.empty-manifest` | `EmptyManifest` | `oracleId` |
| `eyelink-oracle.non-contiguous-records` | `NonContiguousRecords` | `oracleId`, `expected`, `actual` |
| `eyelink-oracle.non-contiguous-fields` | `NonContiguousFields` | `oracleId`, `recordOrdinal`, `expected`, `actual` |
| `eyelink-oracle.inconsistent-record-metadata` | `InconsistentRecordMetadata` | `oracleId`, `recordOrdinal`, `field`, `values` |
| `eyelink-oracle.duplicate-field-path` | `DuplicateFieldPath` | `oracleId`, `recordOrdinal`, `paths` |
| `eyelink-oracle.ordering-conflict` | `OrderingConflict` | `oracleId`, `expected`, `actual` |
| `eyelink-oracle.missing-ordering-disclosure` | `MissingOrderingDisclosure` | `oracleId`, `recordOrdinals` |

### `eyelink-conformance` — `EyeLinkConformanceError`

| Code | Case | Operands |
|---|---|---|
| `eyelink-conformance.invalid-operand` | `InvalidOperand` | `artifact`, `field`, `actual`, `expected` |
| `eyelink-conformance.invalid-absent-value` | `InvalidAbsentValue` | `presence`, `actual`, `expected` |
| `eyelink-conformance.invalid-field-path` | `InvalidFieldPath` | `actual`, `expected` |
| `eyelink-conformance.duplicate-field` | `DuplicateField` | `operand`, `fieldPath`, `count` |
| `eyelink-conformance.empty-manifest` | `EmptyManifest` | `operand` |
| `eyelink-conformance.invalid-tolerance` | `InvalidTolerance` | `tolerance`, `field`, `actual`, `expected` |
| `eyelink-conformance.invalid-summary-row` | `InvalidSummaryRow` | `fixture`, `field`, `actual`, `expected` |
| `eyelink-conformance.duplicate-summary-fixture` | `DuplicateSummaryFixture` | `fixture`, `count` |
| `eyelink-conformance.empty-summary` | `EmptySummary` |  |

### `eyelink-corpus` — `EyeLinkCorpusError`

| Code | Case | Operands |
|---|---|---|
| `eyelink-corpus.invalid-preamble` | `InvalidPreamble` | `source`, `actual` |
| `eyelink-corpus.invalid-header` | `InvalidHeader` | `source`, `line`, `expected`, `actual` |
| `eyelink-corpus.wrong-field-count` | `WrongFieldCount` | `source`, `line`, `expected`, `actual` |
| `eyelink-corpus.invalid-escape` | `InvalidEscape` | `source`, `line`, `field`, `index`, `value` |
| `eyelink-corpus.invalid-value` | `InvalidValue` | `source`, `line`, `field`, `value`, `expected` |
| `eyelink-corpus.invalid-digest` | `InvalidDigest` | `source`, `line`, `field`, `detail` |
| `eyelink-corpus.partial-converter-evidence` | `PartialConverterEvidence` | `source`, `line` |
| `eyelink-corpus.invalid-converter-evidence` | `InvalidConverterEvidence` | `source`, `line`, `detail` |
| `eyelink-corpus.invalid-fixture` | `InvalidFixture` | `source`, `line`, `fixtureId`, `detail` |
| `eyelink-corpus.unsafe-local-path` | `UnsafeLocalPath` | `source`, `line`, `fixtureId`, `path` |
| `eyelink-corpus.duplicate-fixture-id` | `DuplicateFixtureId` | `source`, `fixtureId`, `lines` |
| `eyelink-corpus.duplicate-local-path` | `DuplicateLocalPath` | `source`, `path`, `fixtureIds` |
| `eyelink-corpus.empty-manifest` | `EmptyManifest` | `source` |

### `arrow-export` — `ArrowExportError`

| Code | Case | Operands |
|---|---|---|
| `arrow-export.limits` | `Limits` | `memoryBytes`, `batchRows` |
| `arrow-export.write` | `Write` | `path`, `reason` |

<!-- END GENERATED DIAGNOSTIC CODES -->

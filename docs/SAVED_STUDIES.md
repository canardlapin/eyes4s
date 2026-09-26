# Save and rerun a study

A saved study records which fixation artifact to analyze, which phases to compare, the spatial
frame and grid, duration or uniform weighting, the estimation scales, the comparison method and
its parameters, and the reduction failure policy. Loading JSON does not run the analysis or read
files. Supply the referenced input explicitly when you run it.

Start with the [fixation study guide](FIXATION_STUDIES.md). Its exact
[Scala source](examples/StudyGuide.scala) compiles and runs on JVM and Scala.js. The ordinary
`StudyPlan.cosine` and `StudyCodecs.cosine` constructors supply the built-in method and key schemas;
the scientific choices remain arguments. These constructors use the same `StudyPlan.of` interpreter
and `StudyCodec` machinery available to method authors.

## Inspect, compare, execute

`plan.description` exposes named fields with typed parameter values. `plan.diff(other)` reports
changed description fields with their before/after values, and `plan.structuralDiff(other)` the
typed change of each declared field, which `plan.revise(changes)` applies (see
[comparing plan revisions](FIXATION_STUDIES.md#compare-plan-revisions)). Plans have structural
equality under those declared fields. A change of weighting, bandwidth, phase, failure policy, input digest or registered method
version is visible without executing either plan.

`plan.prerequisites(None)` reports the missing input artifact. Supplying the wrong input reports
both the expected and actual digest. `plan.run(input)` executes each declared scale independently.
The result retains estimation failures, keys excluded by phase selection, and the full matched and
control analyses, including pair failures and per-key denominators. Input identity uses the existing
portable `ContentHash`; it is not a cryptographic authentication guarantee.

Input artifacts contain the fixation data, typed keys, frames and trial clocks. Their JSON reference
contains only the content digest. The importer or application resolves the artifact; pure plan and
codec modules never load files or access a network. The first workflow supports existing fixation
summaries. Detection plans and a general artifact storage service remain separate work.

## Prepare a study and inspect its pair schedule

`plan.prepare(input, budget)` returns a `PreparedStudy` bound to the input,
layout/method identities, and full plan description. It checks the input artifact
and records each trial's frame compatibility through `Agreement`. It does not
compute density maps or comparison scores. `work.run` executes that same prepared
work; `plan.run(input)` now uses this route internally.

The `matched` and `controls` schedules retain typed keys and positions in the
focal/reference operands. Start with `schedule.start`, then call
`cursor.advance(quantum)`. `PairPage.More` returns a bounded vector and the next
immutable cursor; `PairPage.Done` also returns the complete pairing diagnostics.
Duplicate keys are excluded in full and retained as ambiguities. Missing matches,
excluded phases, and failure-policy denominators keep their existing semantics.
Traversal is focal-major/reference-minor in input order; the final contrast still
uses the layout's canonical key ordering.

`work.preview` returns `Either[PlanError, StudyPreview[K, U]]`: a thin inspection
facade over those same schedules. It exposes `focalKeys`, `referenceKeys`,
`excludedPhases`, `failurePolicy`, `reductionOrientation` (`ByLeft`, meaning
the focal trial), `windowTallies`, each trial's fixations outside the analysis
window and the screen, with their `windowSummary`, the plan's `pairing` and its
`matchedCardinality`, the same value that refuses execution when a
one-reference rule meets several matched references. Repeated stimulus occurrences with distinct full keys remain
separate trials. Schedule indices and duplicate indices address the two key
vectors. No maps or scores are computed by preview creation or paging.

The preview carries `inputReference`, `layoutId`, `methodId`, and the full
`description`. `preview.checkCurrent(plan, input)` rejects changed input or
declared choices; obtaining a fresh preview also rejects a prepared plan whose
declared method parameters have changed. These are scientific identities, not
cryptographic verification of registered code. Registered behavior and key
projections must remain pure and stable for their declared identities.

Before paging, `candidatePairCount` is only the size of the usable Cartesian
candidate space, before relation filtering. The exact `eligiblePairCount`,
selected count, unmatched keys, and ambiguities are in the completed
`PairPage.Done` report. These design counts do not predict numerical success or
the number of scores contributing to a reduction; those remain execution results.
Consumers may discard each inspected page instead of retaining the entire table.

`PairScheduleBudget.of(sourceRows, candidatePairs, selectedPairs)` and
`PairQuantum.of(visits)` are checked constructors. The study candidate budget
conservatively counts both designs at every scale before duplicate exclusion.
The default retains the count limits representable by the existing result API;
applications should supply smaller budgets suited to their workload. Refusals
name operand sizes and limits instead of returning a truncated result.

Preparation stores source metadata rather than a Cartesian table of source pairs.
Duplicate grouping and frame checks are bounded by the source-row budget; pages
bound candidate visits and final diagnostic visits. This is not a wall-clock bound
for arbitrary custom key/projection functions. Custom projections, parameters,
and registered behavior must remain pure and stable; an observed change in the
captured plan description invalidates prepared execution.

## Run a study in bounded steps

`work.work(budget)` returns `Either[PlanError, StudyCursor]`; `work.run` drives
the same cursor to completion, so pure and resumable execution are one
scientific path. Each `cursor.advance(quanta)` performs one bounded step and
returns `StudyStep.More` with the stage it worked on (`Estimating(scale, trial)`,
`Comparing(scale, design)`, `Reducing(scale, design)` or `Contrasting(scale)`),
the units it visited and the next immutable cursor, or `StudyStep.Done` with the
complete `StudyResult`. `WorkQuanta(pairs, comparison)` bounds one step:
`PairQuantum` limits schedule visits and reduction/contrast keys,
`ComparisonQuantum` limits cells inside one comparison. Cursors are immutable,
so re-advancing one is deterministic, and the result does not depend on where
steps were cut: pair outcomes accumulate in schedule order and keys reduce in
order of first appearance. Grouping by key relies on `hashCode` being consistent
with `equals`, as the contrast's key domain already does.

Units are a step's own count, not a promise that `workUnits <= quantum`: a
reduction step charges `max(1, scores.size)` for the key it reduces, so its
units can exceed the pair quantum, and a pair whose comparison is already
decided (for example incompatible grids) records its outcome as a zero-unit
`More`. A driver must treat both as progress rather than a stall.

What is bounded is declared, not assumed. `Distribution.cosine` is a
`BoundedCompare`: its dot product and norms accumulate one cell per unit through
a `ComparisonCursor`, and `compare` is the same cursor run to completion.
`StudyMethod.cosine` therefore carries `MethodExecution.Bounded`, and
`work.capability`, `plan.inspect.execution` and the method descriptor all report
`ExecutionCapability.BoundedComparison`. A method built from an ordinary
`Compare` closure carries `MethodExecution.Synchronous`: its pairs still run one
per step, but each comparison is a whole operation, and `work.boundedWork`
refuses it with `PlanError.UnsupportedExecution` before any work. A descriptor
whose execution claim disagrees with the method's typed evidence fails
inspection with `DescriptorError.ExecutionMismatch`. One trial's estimation is
always a whole step.

Extensions declare bounded work by implementing `BoundedCompare`, or by deriving
one from a supported instance with `mapScore`, which applies a constant-time
transformation of the finished score and performs exactly the source's work; a
scaled cosine is the intended example. There is no lifting from a closure.
`ComparisonBudget.of(maxWorkUnits)` bounds one comparison's declared work; for a
bounded method every comparison is checked against it when a scale begins,
before any trial is estimated, and a refusal names the measure, its cells and
the limit. `ComparisonBudget.default` is effectively unbounded.

## Run a study under Cats Effect

`eyes4s-fs2` interprets the same cursor with cooperative yields. Fix the effect
type once, `StudyExecution[IO]`, then either pull the deterministic sequence or
start a run handle:

The executed [StudyExecutionSuite](../fs2/src/test/scala/eyes4s/fs2/StudyExecutionSuite.scala) demonstrates both routes: `events at every quantum replay the pure cursor's stages and plan.run's result` pulls the event stream; `start commits the pure result once and cancelling afterwards changes nothing` acquires the run resource and checks its outcome. Both use complete checked plans and explicit budgets.

Every `StudyProgress` carries the `StudyRunId` (input digest, layout, method,
description and both quanta, so the same submission yields the same id and the
same event sequence), the step number, the cursor's `StudyStage` and its own
units, the `StudySegment` the step counts toward (trials of one scale share one
`Estimating(scale)` segment), the segment's cumulative units and typed
`SegmentTotal`, and the run's cumulative units. A segment's total is stated once,
as the segment begins, and every step of the segment reports that same total.
Preparation can state `Exact(trials)` for estimation;
`AtMost(candidates * (2 + cells) + focal + reference)` for a bounded comparison
segment and `AtMost(2 * candidates + focal + reference)` for a synchronous
method, where `candidates` is the schedule's `candidatePairCount`, the whole
focal-by-reference space. Paging visits at most that space: for each focal key
it visits the reference keys in its block of the design's first declared
equality (the participant, for both shipped designs; every reference key when a
design declares none), or charges one unit when that block is empty, so one per
focal key when there are no reference trials. It then visits every reference
key once for the unmatched report, and each selected pair costs one unit to
begin plus its cells. The isolated consumer's journey pins
this: 36 candidates, 18 visits, 6 reference keys and 6 matched pairs of 1 + 4
units make 54 of an `AtMost(228)`. A contrast states `AtMost(focal keys)`. A
reduction's units depend on the realized scores, so
`StudyExecution.total(work, segment)` answers `Unknown` before the run, and the
runner reports `Exact(units)` from the reduction's first step:
`StudyCursor.reductionUnits` counts one unit per contribution, unmatched key and
ambiguity visited plus `max(1, scores)` per distinct key, a function of the
realized scores alone and therefore the same at any quanta. The comparison budget
is not part of the id: a refusal is a `Failed` outcome, not a different run.

The outcome is one value, `StudyOutcome.Completed(run, last, result)`,
`Cancelled(run, last)` or `Failed(run, error, last)`, and a `StudyResult` exists
only inside `Completed`. Each step (one `advance` plus its bookkeeping) is an
uncancelable region and the fiber cedes before every step, so cancellation lands
between steps; one trial's estimation is a whole step. `start` commits at a
single point, inside the deciding step; `cancel` or releasing the resource
before that commit settles `Cancelled` with the last completed step, and the
first commit wins. `run.progress` is telemetry with one coalescing slot: it
never slows the run, an observer that keeps up sees every step, a slow or late
observer sees the latest step, and every observer ends once the run settles,
having received the last completed step; a cancelled run's observers see no
step for the work that was cut off, since none completed. `run.outcome` is the
authority. `events` has no commit point: interrupting it ends the stream
between steps with no terminal element.

## Run recording and temporal plans through the same runner

The three shipped plan families share one execution contract. In `eyes4s-plan`,
`Stepwise[C, Stage, E, R]` is the step shape a runner interprets: a pure,
immutable cursor that names the stage its next `advance(quanta)` works on and
returns `WorkStep.More(stage, units, next)`, `WorkStep.Done(units, result)` or a
typed error. `StudyCursor` satisfies it as it is; `RecordingCursor` and
`TemporalCursor` are written to it, and `Stepwise.complete` drives any of them.
The counted vocabulary is also pure: `SegmentTotal`, `StudySegment`,
`RecordingSegment` and `TemporalSegment` live in `eyes4s-plan` with the totals
each family states (`StudySegment.total` and so on), and `eyes4s-fs2` keeps the
same names as aliases, so existing imports compile and importing both packages
is not ambiguous.
In `eyes4s-fs2`, `Execution[F]` is the one runner: it interprets a `Submission`
(id, quanta, how to begin the cursor, the segment of each stage, and the total of
each segment given the cursor about to start it, plus the `Stepwise` evidence)
into `RunEvent`, `RunOutcome`, `RunProgress` and `Run`. A hand-built
`Submission` passes `total` as `(segment, cursor) => SegmentTotal`; a total
that needs no cursor ignores it. `StudyExecution`, `RecordingExecution` and
`TemporalExecution` are thin wrappers that build the submission; the study
names (`StudyProgress`, `StudyOutcome`, `StudyEvent`, `StudyRun`) are aliases of
the shared types, so `StudyOutcome.Completed(_, last, result)` and
`StudyEvent.Advanced(progress)` construct and match as before. Two things a
consumer can notice: `StudyProgress(...)` still constructs, but the alias has no
`unapply`, so a progress value is matched as `RunProgress(...)` or read by
field; and a wildcard type test must name the shared case,
`RunEvent.Advanced[?, ?, ?, ?, ?]`, because an alias with four parameters cannot
be applied to wildcards. The commit, cancellation, observer and defect contracts
of the previous section hold unchanged for every family.

**Recording plans.** `plan.work(recording)` checks the prerequisites and returns
a `RecordingCursor`; `plan.run(recording)` drives it to completion, so the
streamed analysis is the pure one bit for bit, chunk cuts included. Its stages
are `Synchronizing` (one whole step), `Warping` (one whole step),
`Interpolating(sample)` and `Detecting(sample)` in chunks of
`WorkQuanta.samples` (a `SampleQuantum`, default 4096; the new third field of
`WorkQuanta`, which no study step reads), and `Assigning` (one whole step). The
chunked stages step the interpolation and detection machines through the
kernel's `MachineCursor`, the pure chunked driver beside `runAll` and the
streaming pipe: an event that spans a cut is emitted where the machine emits
it, and `flush` runs exactly once, inside the step that feeds the last sample.
A run cancelled earlier has flushed nothing and manufactured no event; a
cancelled or failed outcome never carries a `RecordingAnalysis`. In
`eyes4s-detect`, `Detection.stepped` returns the `DetectionCursor` behind this,
and `Detection.run` is that cursor driven to completion, so identity, gap
policy, support ledgers and provenance are assembled by the same code either
way. The bounded-step requirement for an independently implemented detector is
therefore stated, not assumed: its machine's per-sample `step` and its `flush`
are the units the cursor cuts between, and a `step` that does unbounded work on
one sample is not made interruptible by chunking. Detectors register once,
through `EventDetector.of`; there is no second bounded registration.
`RecordingRunId` is the plan's input digest, detector identity, description and
sample quantum. Segment totals are all `Exact`: one unit for each whole step
and the recording's sample count for each chunked machine.

**Temporal plans.** `plan.prepare(available, budget)` checks the prerequisites
and prepares every repetition's `StudyPlan` and `PreparedStudy` once, in plan
order, without estimating anything; `PreparedTemporalStudy.work(budget)` returns
a `TemporalCursor` and `plan.run(available)` is `prepare` then completion. Cells
run in plan order, repetitions outer and windows inner. Per cell, one
`Preparing(repetition, window, trial)` step resolves one trial's measured
anchor, window and observed occupancy, exactly as before (explicit anchors and
coverage, clipped straddlers, missing epochs and 64-bit anchor overflow stay
per-trial typed data inside the result); then the cell's `StudyCursor`, created
from the repetition's prepared study with that occupancy and the window's
provenance context, advances step for step as `Studying(repetition, window,
stage)`. The inner cursor's `Done` is the temporal cursor's `More` into the next
cell, so a temporal run's step sequence is its cells' study sequences
concatenated, each stamped with its cell, and its segments are
`TemporalSegment.Preparing(repetition, window)` with total `Exact(trials)` and
`TemporalSegment.Studying(repetition, window, studySegment)` with the
repetition's study totals. Cancellation lands between trials of a preparation,
between study steps and between cells; no partial cell is returned as a result.
`TemporalRunId` is the temporal input digest (fixation input plus every anchor
and coverage ledger), the base layout and method identities, the full plan
description and the pair and comparison quanta. One ordering note: because
preparation now precedes execution for every repetition, a preparation failure
of a later repetition is reported before an execution failure of an earlier one.
Execution failures can occur under default budgets; what cannot is a later
repetition failing preparation after an earlier one succeeded, since every
repetition is prepared against the same input and budget before any cell runs.

## Test the execution contract and its response envelope

The contract above is published, so a downstream family is tested the same way
as the shipped ones. `eyes4s-laws` provides `ExecutionLaws`, pure laws over
`Stepwise` alone. A family supplies an `ExecutionLaws.Family`: its stage-to-segment
map, its totals, the reference run of the plan a cursor came from, result and error
equality, and a step budget. Pass it to `ExecutionLaws.conformance(family, cursors,
quanta)` with a cursor generator and `ExecutionLaws.quanta(...)`, which always
includes `WorkQuanta.default` and `ExecutionLaws.finest` (every quantum at 1). The
rule set checks eight laws:

- the same cursor at the same quanta yields the same steps and the same end;
- a run ends in exactly one terminal step within the budget, a result or a typed
  failure (a run whose first advance fails ends with no step, lawfully);
- completion at any quanta, and at the finest cut, is the reference run;
- every sequence of quanta, changing from step to step, yields the reference run;
- units are non-negative and each segment is visited in one contiguous block;
- a segment's total is position-independent: every step of it states the same one;
- an `Exact` total is met and an `AtMost` total is never exceeded (a block a
  failure cut short is held to the bound only);
- the work each completed segment charges does not depend on how the steps were
  cut, the finest cut always included.

For the shipped families the reference run is the plan's own `run`, which drives
the same cursor at the default quanta, so these laws establish cut invariance, not
scientific correctness. A ninth law adds that when the family passes an
independent oracle, an expectation computed without the cursor:
`conformance(family, cursors, quanta, Some(expect))` requires every run's end,
under any sequence of quanta, to satisfy it.

`ExecutionLawsSuite` runs them over the study, recording and temporal fixtures on
the JVM and Scala.js. It also runs two temporal fixtures that fail lawfully (a
refused comparison budget, once after four preparation steps and once on the first
advance) and checks the R-pinned matched/control fixture against its pinned
matched means, control means and differences as the independent oracle. Twelve
deliberate mutants are each falsified by exactly the laws their receipts name:
dropped units, a skipped trial, drifting units, premature completion, a
quanta-dependent result, a run that never ends, a cut that charges extra work, a
revisited segment, an overstated `Exact`, an understated `AtMost`, a wavering
total, and a self-vouching instance whose reference run is itself, which only the
independent oracle catches. The effectful half lives in `eyes4s-fs2`'s
`ExecutionConformanceSuite`. It states five laws as functions of a runner, over a
synthetic cursor under `TestControl`: `events` ends in exactly one terminal event,
`Cancelled` never carries a result, cancellation is observed only between steps,
the first commit wins, and a defect surfaces from both entry points. The shipped
`Execution` and a fault-free re-implementation satisfy all five, and each of five
mutant runners fails the laws its receipt names.

How long a step can hold the runner is measured, not assumed.
`fs2ModuleJVM/Test/runMain eyes4s.fs2.ExecutionResponsivenessMain` reports the
longest step, the longest runner gap and the cancellation latency for each route,
on a pinned JVM. It never fails on timing. [Execution responsiveness](EXECUTION_RESPONSIVENESS.md)
records the evidence. Against the proposed 100 ms target, on the measured runtime
(an Apple M3 Max, JDK 25, four visible processors, a 2 GiB heap), every fixture
meets it at default and smallest quanta. A 100-trial 256×256 study meets it with
Gaussian bandwidths up to 32 cells: one trial's estimation takes at most 19.6 ms,
and cancellation settles within 17.2 ms. More trials lengthen the run, not its
steps, and cost 512 KiB of retained heap per trial per scale at that grid. A
1024×1024 grid misses it at 647 ms per
trial, so intra-trial estimation (UI-X2) stays deferred until a consumer needs
grids that large. Recordings of 60,000 samples or more also miss it, because
detection assembly scans every sample once per event. That defect is outside
estimation.

## Input payloads and admission ledgers

`StudyInputCodecs.study[U]` supplies two versioned codecs for the ordinary participant/stimulus/phase
route, and `StudyInputCodecs.trial[U]` (with `StudyCodecs.trialCosine`) the trial-keyed route of
`eyes4s.trial-key@1` keys under the `eyes4s.participant-phase-trial-occurrence@1` layout; `new StudyInputCodec(schema, ledgerSchema, layout, keyCodec)` builds them for a custom key
layout, so repeated presentations are kept apart by an explicit occurrence or session field in `K`
rather than by a label or digest. `input` encodes a `StudyInput[K, U]` (`eyes4s.study-input@1`):
the layout and key schema identities, the spatial unit, the declared input digest, a document
identity table of frames and trial clocks, and the trials as a row array (`eyes4s.trials@1`) whose
rows carry the typed key, the unit metadata and a scanpath (`eyes4s.scanpath@1`) that references its
frame and clock by nominal ID. Fixations are half-open microsecond intervals as decimal strings,
finite centre coordinates, the sample count and, when declared, the dispersion value and method.
Decoding rebuilds the input through `StudyInput` and compares the reconstructed digest with the
declared one. That digest covers what the study computes on: the typed keys, trial order, each
trial's spatial unit, frame identity/geometry/axis and clock, and every fixation's interval, centre
and sample count. Reordering, dropping or altering any of those fails with
`CodecError.InputIdentity`. Declared dispersion is carried through the payload but is not
identity-bearing: it does not enter the digest, so a changed dispersion decodes as a different
value with the same input reference. Errors inside a trial are located, for example
`CodecError.Entry("trials.rows[2].fixations[1]", ...)`.
Scanpaths and fixation summaries backed by source samples are refused with `CodecError.Unsupported`
rather than silently detached; their support belongs to the recording payload.

`ledger` encodes an `AdmissionLedger[K]` (`eyes4s.admission-ledger@1`, `@2` when it records an
admission policy other than the version-1 one, `@3` when it records a trial inventory, or `@4`
when it records a declared source interpretation; each
ledger is written under the earliest version that expresses it): the source reference (a label
and the portable digest of the decoded header and records), the header, the recorded outcome and one
entry per source record in record order. An admitted record links its logical record number to the
typed trial key and the ordinal it supplied; a rejected record keeps its raw fields, its key when one
could be read, and a typed `AdmissionReason`. A quarantined trial names the affected records and a
`QuarantineCause`. `AdmissionLedger.of` refuses unordered records, duplicate ordinals within a trial,
quarantine scopes that omit their own record or name a record the ledger lacks, and an outcome
inconsistent with the rejected count; `ledger.checkAgainst(input)` verifies that admitted records
address every input trial exactly once per fixation. Version 2 adds the `AdmissionPolicy` (the
off-screen policy and the correction rules, each with its scope) and the admitted records outside
the frame; `AdmissionLedger.of` refuses such a record unless it is admitted, listed once in record
order and admitted under `ExcludeRecord`, and decoding refuses a ledger in which two rules cover an
admitted record's trial (`AdmissionError.CorrectionConflict`). A version-1 ledger decodes with the
version-1 policy. Version 3 adds the `InventoryLedger` of an inventory admission
([fixation studies](FIXATION_STUDIES.md#join-a-trial-inventory)): the inventory's own source
reference and header, the declared attribute columns, every inventory trial (identity, inventory
records, declared item, typed attributes, the items its records named, its fixation records and
its `TrialDisposition`), the trials only the fixation table names, the declared record attribute
columns with the typed attributes of admitted records, and the `SampleCountRule` the records were
admitted under (a count column, or counts derived from duration at a recorded rate). Integer
attributes are decimal strings, so 64-bit values survive Scala.js. Decoding rebuilds the inventory
through the smart constructors of every part (`InventoryTrial.of`, `UnlistedTrial.of`,
`RecordAttributes.of`, `InventoryLedger.of`) and joins it with `AdmissionLedger.inventoried`, so a
saved inventory is refused (`AdmissionError.Inventory(InventoryError…)`) unless every attribute
is of its declared column's kind, every trial's record items agree with its records and
disposition, every keyed record is listed under its own trial and carries its trial's item, and
every disposition agrees with its records. A version-3 document states its `inventory` member:
the inventory, or `null` in the lift of a ledger without one (read with version 2's vocabulary and
written back under version 1 or 2; see the [version policy](DOMAIN_CODECS.md#version-policy)), and
only a ledger with an inventory may name `NotInInventory` or `InventoryItemConflict`
(`AdmissionError.UninventoriedCause`); a key layout without a trial label
cannot carry one (`InventoryError.NoTrialProjection`). A version-3 ledger is pinned by
[admission-ledger-v3.json](../codec/src/test/resources/eyes4s/admission-ledger-v3.json). `StudyInputRegistry` registers codecs by key
schema and refuses missing or duplicate registrations. `VersionedCodec.trials` is the generic
row-array codec these payloads use.

### Declared source identity

`ImportSpec[K, U]` is the pure, JVM/Scala.js description of fixation admission: typed key
columns, fixation columns, frame, timestamp units and rounding, sample-count rule, attributes,
correction policy and admission decision. Build it with `ImportSpec.of` and checked
`SourceFixationColumns.of`; `SourceKeyColumns.Study` and `.Trial` determine the key type of its
policy. `InventoryImportSpec.of` describes a separate trials file; `SourceInventory` binds that
description to the inventory's semantic identity. `ImportSpecCodec.study[U]`, `.trial[U]` and
`.inventory` persist these descriptions. Custom reader/clock identities can round-trip, but
the Phase 1 `SourceAdmission.read` interpreter refuses them with `UnsupportedReplay`.

`SourceAdmission.read(label, contents, spec, inventory)` interprets the description in `io`.
The optional inventory is its display label and text. The result carries a declared ledger and
all accepted trial groups; `admitted` is absent when the decision refuses the import. Rejected
records remain in the ledger. `SourceAdmission.source` and `.inventorySource` derive the reference
from decoded records without admitting trials, for inexpensive asset repair.

`SourceRef.records` remains the digest of the decoded header and every record, including rejected
records. `SourceRef.identity` is present only for a declared interpretation. The versioned
`eyes4s.source-identity/1` digest combines format, parser definition/version, decoded records and
admission options. It excludes display labels, paths and the separate byte SHA-256. Earlier ledgers
and the original `SourceRef.of` constructor carry `LegacyUnspecified`; loading them does not invent
parser or option evidence. The v4 ladder preserves v1–v3 meanings and their earliest encoding.

`SourceComparison.of(sameBytes, expectedRef, actualRef)` compares the identity components first.
`ChangedIdentity` reports a non-empty set of `Format`, `Parser`, `Options`, `Records` or `Undeclared`
causes, even if the byte checksums match. Only equal declared components produce `SameBytes`
(equal checksums) or `SameIdentity` (different checksums). A declared source is evidence of an
interpretation, not proof that its saved ledger has been replay-verified.

What a decoded ledger does **not** prove is that its exclusions are the source's. The source digest
is carried, not recomputed: admitted records keep no raw fields, so the ledger alone cannot
reproduce the digest of the header and records it names. Nothing binds a rejected row to the
source, so a ledger whose rejected rows were edited is refused only where the edit breaks one of
the invariants above. `LedgerForgerySuite` pins this. Dropping the time rejection that a quarantine
scope names is refused (`AdmissionError.QuarantineScope(3, Vector(2, 3, 4, 5))`), but dropping it
and rewriting the scope decodes cleanly, and so does dropping one of two standalone rejections from
a reviewed ledger that has a `LedgerOf` relation to its input. Exclusions can be verified only by
re-importing the source file and comparing the ledger the importer produces.

`LedgerReverification.verify(label, contents, spec, ledger, input, inventory)` performs that
check in `io`. It compares fresh admission with the saved source interpretation, header, every
record disposition, policy, outcome, outside-frame evidence, complete inventory evidence and
input digest. It returns privately constructed `VerifiedAdmission` only after all comparisons
pass. Its `admitted` input is absent for a refused admission. `LedgerVerificationError` names
the source and differing component or input identities, and `IoDiagnostics.given` projects every
case through `Diagnose`.

This entry point is synchronous; it does not promise cooperative cancellation. Legacy sources
with unspecified interpretation remain unverified, and custom readers without a supported
interpreter are refused. The pure resolver retains its structural guarantee and existing forgery
fixtures; it cannot perform this source check. Manifest-to-source wiring remains tracked under
`bd-01M2SC6N15J2N7PHD4DXBE43VD`. Replay proves consistency of the supplied archive, not who
created it: replacing the source, options, ledger and input together is not an authenticity check.

Both payloads are artifacts of their own; plan JSON references the input by digest only. The pinned
[study-input-v1.json](../codec/src/test/resources/eyes4s/study-input-v1.json) and
[admission-ledger-v1.json](../codec/src/test/resources/eyes4s/admission-ledger-v1.json) fixtures are
checked for decoded meaning and JSON value identity on JVM and Scala.js; the JVM suite additionally
checks byte-identical re-encoding of the pretty-printed files, which Scala.js does not promise because
it renders integral doubles without a fraction.

## Recording and temporal input payloads

`RecordingInputCodecs.input[U]` encodes a `RecordingInput[U]` (`eyes4s.recording-input@1`): the
nominal source name, the declared input digest, a document identity table of the frame and clocks, the
gaze channels, the optional viewing geometry in millimetres, and the optional observed synchronization.
Channels are either a monocular `Recording[U]` or a paired `BinocularRecording[U]`; the standalone
`recording[U]` (`eyes4s.recording@1`) and `binocular[U]` (`eyes4s.binocular-recording@1`) codecs carry
the same inner shape with their own identity table. A recording is its frame and clock by nominal ID,
the eye, the declared pupil unit, the sampling rate (`fixed` with a finite `hz`, or `irregular`), the
fixed-rate tolerance in microseconds, its `Recording.contentHash`, and its samples as parallel columns
of one declared `length`: `tMicros` as decimal strings, `state` as one of `tracked`, `blink`, `lost`
and `offScreen`, `x`/`y` and `pupil` as finite numbers where the category has them and `null` where it
does not, and `lineage` as the ordered derivation of each sample (`measured` or `interpolated`,
followed by `smoothed` and `projected` steps). A binocular recording carries `left` and `right`
column groups over one `tMicros` column. Decoding rebuilds the value through `Recording.of` or
`BinocularRecording.of`, so monotonic time, in-frame positions, declared pupil units and the
fixed-rate tolerance are re-proven and reported as `CodecError.Recording`, and then compares the
declared digest with `Recording.contentHash` (for a paired recording, `BinocularRecording.contentHash`,
the ordered combination of its two eye projections); a dropped or altered sample, a changed support
category or lineage, or a swapped clock fails with `CodecError.InputIdentity`, so
`RecordingPlan.prerequisites` accepts the decoded recording by the same `ArtifactRef`. Two operands
are carried and re-proven but not identity-bearing: the spatial unit, which the payload's `unit` field
and the typed frame lookup guard instead, and the sampling tolerance of an irregular recording, which
its hash omits because irregular sampling never applies it. Mis-shaped columns, a `tracked` sample
without a position, an unknown category, or a lineage that does not begin with a basis are located
errors such as `CodecError.Entry("channels.recording.samples[3]", ...)`. Payloads are in-memory
JSON; a recording with more than `RecordingInputCodecs.maximumSamples` (2^22, about seventy minutes
at 1 kHz) is refused with `CodecError.SampleBound` on both sides, naming the count and the bound, and
must be split or carried as a separately referenced typed payload.

The synchronization entry is input evidence: the target clock, the fit mode, the observed common
marks and the optional residual limit, from which `RecordingInput.synchronize` refits `SyncEvidence`
deterministically. The `fitted` offset and drift are written alongside and cross-checked on decode
(`CodecError.SynchronizationFit` names the declared and refit coefficients); the fitted diagnostics
themselves belong to the completed-result archive. Only offset-only synchronization is fixture-pinned;
affine fits with non-zero drift are covered by the generated laws on both the JVM and Scala.js. `RecordingInput.of` refuses
an empty source, a synchronization whose target is the recording's own clock, or marks that do not
fit. Viewing geometry and every mark enter the input digest. A recording plan still runs on the bare
`Recording[Px]` with its own declared provenance; `RecordingInput.disagreements(input, plan)` names
every field where that provenance departs from the input's evidence (source, clocks, viewing, fit
mode, marks, residual limit), followed by the plan's own prerequisites, so a plan whose marks differ
from the observed ones is refused before it records the wrong provenance.

A source-supported scanpath inside a study input (`eyes4s.scanpath@1`) now carries a `source` entry:
the `RecordingRef`, the exact recording in the inner shape above, and the half-open sample ranges of
its fixations. Decoding rebuilds it through `EventSeries.of` and `Scanpath.fromEvents`, so centres
and sample counts are re-derived from the samples and must equal the declared summaries; a detached
summary, an overlapping or truncated range, or a support category flipped inside the source
recording is refused. Its fixations declare a dispersion `method` only: the value is re-derived from
the samples rather than compared. This is a design assumption rather than a measured difference:
the spread statistics use `hypot` and `pow`, which neither platform promises to round identically,
so a value written on one platform is not promised bit-identical on the other and the wire does not
depend on it. The source name, the source recording's digest and every sample
range enter the study digest of a source-supported trial, so a different source recording or
segmentation with the same summaries is a different input; detached trials keep their S2 digests.
Dispersion recomputed under a warp (`SummaryEvidence.Recomputed`) is a transformation result, not an
input, and stays `CodecError.Unsupported`.

`TemporalInputCodecs.study[U](embedding, resolve)` encodes a `TemporalStudyInput[K, U]`
(`eyes4s.temporal-study-input@1`): the layout and key schema identities, the spatial unit, the
declared temporal digest, the base study either inline (`StudyEmbedding.Inline`, the complete
study-input payload) or by reference (`StudyEmbedding.ByReference`, the study digest resolved through
a caller-supplied lookup that never loads files and fails with `PlanError.MissingArtifact`), a clock
identity table, and one epoch per trial that has one, in key order: the typed key, the measured
`anchorMicros` as a decimal string, and the observed coverage as a clock and its intervals. Trials
without an epoch stay absent, so a decoded input reports the same `MissingEpoch` at run time.
Decoding rebuilds the value through `TemporalStudyInput.of`, so duplicate or foreign epochs are the
constructor's `CodecError.Temporal` refusals; a coverage interval on another clock, a coverage clock
that is not the trial scanpath's clock, or an unknown clock is located at its epoch, and a moved
anchor or changed coverage fails with `CodecError.InputIdentity`. `TimelineCodecs.timeline(schema, values)` is the conditional codec for
`Timeline[A]` (`planned` and `observed` wrap the two timing kinds with a `timing` field, and the
neutral codec refuses a payload that carries one): a clock and ordered marks with exact microsecond
instants, where equal instants keep input order.

The pinned [recording-input-v1.json](../codec/src/test/resources/eyes4s/recording-input-v1.json),
[binocular-recording-input-v1.json](../codec/src/test/resources/eyes4s/binocular-recording-input-v1.json),
[study-input-source-supported-v1.json](../codec/src/test/resources/eyes4s/study-input-source-supported-v1.json)
and [temporal-study-input-v1.json](../codec/src/test/resources/eyes4s/temporal-study-input-v1.json)
fixtures are seeded from the core recording constructions, the synthetic EyeLink binocular corpus
file and the temporal contrast fixtures (with one trial anchored beyond JavaScript's exact integer
range and one trial left without an epoch). They are checked for decoded meaning and JSON value
identity on JVM and Scala.js; the JVM suite additionally checks byte-identical re-encoding of the
pretty-printed files.

## Completed results

`StudyResultCodecs.cosine[U]` supplies the versioned codec for a completed `StudyResult` of the
ordinary cosine route (`eyes4s.study-result@1`); `persistence.results(scoreCodec, differenceCodec)`
builds one from any `StudyCodec`, so an extension method archives its own score and difference
types through the codecs it registers, never through `Any`, an unnamed numeric vector or a rendered
string. Only a `StudyResult` can be encoded, and a `StudyResult` exists only for completed execution:
a cancelled or failed run has no value to archive.

The archive keeps the result's identity and every piece of evidence the run produced:

- the layout, key, method, score and difference schema identities, the spatial unit, the input
  reference digest and the complete plan description with its typed parameters;
- per scale, the estimator, every trial's estimation outcome (a density as its cell values on the
  plan grid with its provenance, or a typed `StudyFailure` naming the trial), and the keys excluded
  by phase selection;
- both directed pair analyses: every pair row with both source keys and its score or typed failure
  (`StudyFailure.Comparison` names both trials; an estimation failure names the trial that failed),
  the pairing report (pair space, eligible and selected counts, unmatched keys, duplicate-key
  ambiguities with their operand indices), the evaluation metadata (name, scale and the full
  `EvaluationSpec`) and the evaluation provenance;
- both by-focal reductions: every `ReductionRow` with its typed `ReductionError` or score and its
  `successful`/`failed`/`contributing` denominators, the `ReductionReport` and the reduction provenance;
- the contrast rows in the layout's key order, each with its operands and its difference or typed
  `ContrastRowError`, or the typed `ContrastError` when no contrast could be formed.

Provenance is written as its input digest and every step's parameters in order; 64-bit values are
decimal strings. Densities are cell values on a grid declared once in the document identity table.
Each scale carries `analyses`, a `StudyAnalyses` with both typed directed pair analyses
(`DirectedPairwiseAnalysis[K, K, StudyFailure[K], S]`) and their reductions, and the `contrast` is
over those same reductions; the archive stores the analyses once and the contrast rows beside them.

Decoding rebuilds the result through checked reconstruction: `DirectedPairwiseAnalysis.reconstruct`,
`Analysis.reconstructByLeft` (and `reconstructByRight`, `reconstructEdges`, `reconstructByEndpoint`
for the other orientations), `ReductionRow.reconstruct`, `ReductionReport.reconstruct`,
`ContrastRow.reconstruct`, `Contrast.reconstruct` (design) and `StudyAnalyses.of`,
`StudyScaleResult.reconstruct`, `StudyResult.reconstruct` (plan). What is re-derived and checked:

- `DirectedPairwiseAnalysis.reconstruct` requires one row per selected pair and re-derives the
  evaluation provenance from the rows, the pairing report and the evaluation metadata; the stored
  provenance must equal it (`ReconstructionError.RowCount`, `ProvenanceConflict`).
- `Analysis.reconstructByLeft` regroups the pair rows by focal key exactly as the reduction cursor
  does (contributions, then unmatched keys, then ambiguities, in order of first appearance) and
  requires the stored rows to cover those keys in that order (`KeyDomain`), each row's
  `successful`/`failed` to be the group's (`KeyDenominator`), each row's outcome to be the one the
  group and the policy force, an ambiguity, no scores, a rejected failure count, or otherwise a score
  or a mean failure (`OutcomeShape`; the mean itself is the one thing not recomputed), every report
  count to follow from the rows and the source, including `contributionCount == rows.size`
  (`ReportCount`, `FailedKeys`), and the reduction provenance to equal the derivation
  (`ProvenanceConflict`). `ReductionRow.reconstruct` additionally requires `contributing` to follow
  from the outcome (`Denominator`) and a failed outcome's own counts to agree with the row's.
- `Contrast.reconstruct` re-runs the contrast compatibility check, requires the rows to cover the
  sorted key union in the layout's ordering (`ContrastDomain`) and each row's operands to be the
  reductions' own rows (`ContrastOperand`); `ContrastRow.reconstruct` requires the difference's shape
  to follow from its operands (`ContrastRowShape`).
- `StudyAnalyses.of` requires each reduction to have been reduced from the very pair analysis
  supplied beside it (`StudyResultError.SourceIdentity`), which is how a reduction over an
  undirected or foreign source is refused, and `StudyScaleResult.reconstruct` requires the contrast
  to be over the scale's own reductions (`ContrastAnalyses`).
- `StudyScaleResult.reconstruct` requires every density's provenance steps to be the ones the scale's
  estimator derives, `StudyEstimate.provenanceSteps`, so the smoothing bandwidth and edge policy on
  record are the declared ones (`MassProvenance`); every stored failure to name the row it sits in
  (`FailureKey`, `PairFailure`); and every pair, unmatched, reduced, contrasted or excluded key to
  be an estimated trial (`OrphanPair`, `OrphanKey`).
- `StudyResult.reconstruct` takes the layout and requires the description to name it
  (`LayoutMismatch`), the input reference (`InputMismatch`) and exactly the stored scales with the
  same estimator parameters (`ScaleCount`, which is how a partial accumulator tagged as complete is
  refused, and `ScaleEstimate`); every density to lie on the described grid (`MassGrid`); and, for
  both pair analyses of every scale: the evaluation provenance's inputs digest to be the study input
  (`ProvenanceInputs`), the evaluation specification to name the described method and version
  (`MissingSpecification`, `SpecificationMethod`) with parameters equal to the described weight,
  estimator and `method.*` parameters (`SpecificationParameters`), the reduction to use the
  described failure policy (`Policy`), and every pair to join a focal-phase trial to a
  reference-phase trial under the layout, with excluded keys outside both phases (`Phase`).
- The codec itself requires the archived score components to be the method's
  (`CodecError.ScoreComponents`).

Not re-derived: the reduced means and contrast differences (arithmetic), and the digest inside each
density's provenance, which is the digest of the trial's occupancy and needs the input. Errors inside
a stage are located, for example `CodecError.Entry("scales[0].analyses.matched.source.rows[1]", ...)`.

`StudyResultRegistry` registers result codecs by method identity and refuses unknown
(`CodecError.MissingResultCodec`) or duplicate (`CodecError.DuplicateResultCodec`) registrations; a
payload declaring another score or difference schema than the registered codec's is refused before
any row is read. A temporal failure (`StudyFailure.Temporal`) is written as its trial and the typed
`TemporalStudyError` its occupancy produced, with every case of that family and of the plan, window
occupancy, evaluation-specification, pair-schedule and comparison-work errors it wraps as a `kind`
tag and its operands; it arises only in the cells of a temporal result, below.

The pinned [study-result-v1.json](../codec/src/test/resources/eyes4s/study-result-v1.json) is the
pinned study-v1 plan run on the pinned study-input-v1 input. The portable suite checks its decoded
meaning and JSON value identity on JVM and Scala.js, and that re-executing the pinned plan on the
pinned input reproduces the archive bit for bit; the JVM suite additionally checks byte-identical
re-encoding of the pretty-printed file. `eyes4s.laws.StudyResultEquivalence` is the published
structural identity of two results, for round-trip laws over extension score types that keep
reference equality.

## Recording and temporal result archives

A recording analysis and a temporal study have archives of their own, built from their plan
family's codec: `RecordingPlanCodec.results` (`eyes4s.recording-result@1`, a `RecordingResultCodec[P]`)
and `TemporalStudyCodec.results(scoreCodec, differenceCodec)` (`eyes4s.temporal-result@1`, a
`TemporalResultCodec[K, U, P, S, D]`; `TemporalResultCodecs.cosine[U](planSchema)` for the ordinary
route). Unlike a study result, both embed the plan document they ran, through the plan family's
versioned codec: a recording analysis's detector identity and a temporal cell's study plan are
typed values only the typed plan can rebuild, so the plan is the evidence the rest is checked
against. A result's description and input reference are its plan's.

**`eyes4s.recording-result@1`** carries the plan, a document identity table (the angular frame and
the analysis clock), the synchronization evidence (source and target clocks, fit mode, offset and
drift, the marks used, every residual, every rejected mark with its residual and limit, and the
root-mean-square, maximum and uncertainty magnitudes), the angular and the prepared recordings in
the inline recording shape, the detection (the detector's name and version, the gap policy and
temporal support, every event with its half-open sample support, the label of every sample, the
report with its class durations, unclassified ranges, bridged gaps and warnings, and the
provenance) and the assignment (every area with its region and attributes, the membership policy
and temporal support, every sample's membership and the time ledger). Events are written by kind:
a fixation's span, centre, sample count and dispersion method (its value is re-derived from the
supporting samples, as a source-supported scanpath's is, and a declared value is refused where it
would be ignored: `CodecError.Field("value", …)` located at the event), a saccade's endpoints and
peak velocity
(or `null` where it was not measured), a pursuit's path, a blink's span. Decoding rebuilds the
analysis through `RecordingAnalysis.reconstruct(plan, angular, prepared, events, support, areas)`,
which re-derives the synchronization evidence by refitting the plan's marks exactly as the run
fits them, the detector's identity and configuration from the plan's method and parameters, the gap
policy and the temporal support; requires the angular recording to be on the plan's angular frame
and analysis clock; re-derives the prepared recording by re-running the plan's gap interpolation
over the angular samples (a linear fill in basic IEEE arithmetic, exact and identical on every
platform) and requires the archived one to equal it sample for sample, refusing otherwise with
`RecordingResultError.Stage(stage, field, expected, found)`; rebuilds the detection through
`Detection.reconstruct`, the assembly a detector's emissions go through, so every event must lie in
the recording, every declared support range must be the range its span covers, invalid samples
inside an event must be allowed by the gap policy, and the labels, report and provenance are
re-derived (the dispersion uses `hypot` and `pow`, which neither platform promises to round
identically, so a fixation's re-derived dispersion may differ in its last bit between the JVM and
Scala.js; decoding cannot fail because of it, since the archive carries no dispersion value to
compare); requires the areas to be the plan's `(id, label)` pairs, in order and trimmed as an
AOI trims them, with the attributes the run records (`RecordingArea.attributes`: the native frame
and the pixel bounds rendered canonically, so the same area archives to the same bytes on the JVM
and Scala.js); and re-derives the assignment by assigning the prepared recording to them under exclusive
priority, an exact point-in-region test. Every archived member the reconstruction derives
(synchronization, detection, assignment) must then equal it, member by member, or decoding fails
with `CodecError.Derived(path, declared, derived)` at the first difference, for example
`detection.labels[5]`; numbers compare as the doubles they denote, never through a decimal
rendering. Not re-derived: the angular samples and frame (a trigonometric warp of the input) and
each area's warped region.

**`eyes4s.temporal-result@1`** carries the plan, the score and difference schemas, a document
identity table of the occupancy measures' frames, and one cell per repetition and window in plan
order (repetitions outer, windows inner): the names of its repetition and window, every trial's
occupancy in input order, and the cell's completed study result in the `study-result@1` wire,
envelope included. An occupancy is its resolved window interval, boundary, observed and missing
microseconds, the complete ledger of every fixation's original and retained microseconds and the
positions of the retained fixations; or the typed `TemporalStudyError` it failed with (a missing
epoch, a window that cannot be anchored without overflow). Decoding rebuilds each occupancy through
`WindowOccupancy.reconstruct`, which requires the observed and missing time to partition the window
(`WindowOccupancyError.ObservedTime`), every ledger row to sit at its own fixation with
`0 <= retained <= original`, no more retained than the window observed and, under
`FullyContained`, retained all or nothing (`Ledger`), and
one position per retained fixation (`MeasureSupport`), and re-derives the measure's weights and
its digest from the ledger; each cell's study result through the checked study reconstruction in
the cell's provenance context (`TemporalStudyPlan.provenanceContext`), so every evaluation
specification must carry the cell's temporal input, window, bounds, boundary and repetition and the
time order that context implies (`StudyResultError.SpecificationTime`); and the whole through
`TemporalStudyResult.reconstruct(plan, cells)`, which re-derives the cell layout, each
repetition's study plan (`TemporalStudyPlan.repetitionPlan`) and each cell's context from the plan
and refuses, as a `TemporalResultError` wrapped in `Cell(repetition, window, …)` where it concerns
one cell: a missing or extra cell (`CellCount`), a cell out of place (`CellLayout`), a result
describing another plan than its repetition's (`Plan`) or failing its context (`Result`), a cell
whose ledger lists other trials than the first cell's, or a scale estimating other trials than its
cell's ledger lists (`OccupancyKeys`), an occupancy under another boundary (`Boundary`) or over
another width (`Width`), a missing epoch naming another trial's digest or where another cell has
the trial's epoch (`Epoch`), an occupancy anchored at another instant or clock than the same trial
in another cell (`Anchor`; each cell's outcome is compared as a typed anchor, missing epoch or
failure, never by a rendered name, so any clock name is safe), a density not estimated
from its trial's occupancy measure, which ties every density to the archived ledger
(`Density`), and a temporal failure the occupancy does not produce (`Failure`). Not re-derived:
densities, scores, means and differences, and the retained fixations' positions, which only the
input can confirm.

`RecordingResultRegistry` and `TemporalResultRegistry` register result codecs by method (the
recording method, the base study's method) and refuse missing or duplicate registrations with
`CodecError.MissingResultCodec` and `DuplicateResultCodec`; `TemporalRegistry` does the same for
temporal plans by their base study's method (`MissingMethod`, `DuplicateMethod`), beside the
existing `RecordingRegistry` for recording plans.

The pinned [recording-result-v1.json](../codec/src/test/resources/eyes4s/recording-result-v1.json)
is an I-DT plan (one degree square, 2 ms minimum, a 6 ms interpolation gap) whose provenance is
exactly the pinned recording-input-v1's evidence, run on its monocular channels: the fourth mark
is rejected by the residual limit, the gap interpolation fills the blink and the signal loss so the
prepared recording differs from the angular one, and two source-supported fixations (the first
over interpolated samples) leave one off-screen sample excluded from the one area. The pinned
[temporal-result-v1.json](../codec/src/test/resources/eyes4s/temporal-result-v1.json) is the pinned
temporal-study-v1 plan run on the pinned temporal-study-input-v1: eight cells, each with the trial
without an epoch as a typed `MissingEpoch` in its ledger and a `StudyFailure.Temporal` at every
scale, the anchor beyond 2^53 intact in every window and the coverage gap visible as missing time.
`GenerateResultArchivesV1` writes both, and their compact portable mirrors; the portable
`ResultArchivesV1Suite` checks their decoded meaning and JSON value identity on the JVM and
Scala.js, and `ResultArchivesV1JvmSuite` that the files are the writer's output with independently
computed `shasum -a 256` digests, re-encode byte for byte, and are reproduced byte for byte by
re-executing the pinned plans on the pinned inputs. The temporal archive is large (2.3 MB
pretty-printed): its sixteen scale results each carry the complete `study-result@1` evidence.
`eyes4s.laws.RecordingResultEquivalence` and `TemporalResultEquivalence` are the published
structural identities of two results.

## Artifact manifests and verified resolution

A saved study is several artifacts. `ScientificManifest` (`eyes4s.manifest@1`) lists them and the
typed relations between them, and `ArtifactResolver` rebuilds the scientific values from bytes the
application supplies, admitting nothing it has not verified. eyes4s owns the manifest and the
verification; the application owns where the bytes live, autosave, crash recovery and missing-file
repair.

Each `ManifestEntry` records a manifest-local `ArtifactName`, an `ArtifactRole` (`study-plan`,
`study-input`, `admission-ledger`, `study-result`, `recording`, `recording-input`,
`temporal-study-input`, `payload`, `recording-plan`, `recording-result`, `temporal-plan` or
`temporal-result`), the schema identity of the artifact's envelope, its media kind
(`application/json` or `application/octet-stream`), its exact byte length as a decimal string and
the SHA-256 of its exact bytes (`ByteDigest`, 64 lowercase hexadecimal digits). The identity-bearing
roles (study input, recording, recording input, temporal input) also declare the semantic identity
that plans and results reference: the 16-hex `ContentHash` of an `ArtifactRef`, whose meaning is
unchanged. The two are separate on purpose. Re-serializing a JSON artifact (compact instead of
pretty-printed, say) gives new bytes and therefore a new length and digest, but the same identity;
the byte digest never stands in for the semantic one. A `payload` entry carries the
`eyes4s.packed-array@1` schema and its `PayloadLayout`, whose byte length must equal the entry's.

Relations are typed edges, checked against decoded values rather than names:

| Relation | Endpoints | Multiplicity | Resolution checks |
|---|---|---|---|
| `PlanInput` | plan, input | exactly one per plan | `plan.prerequisites(input)` is empty |
| `ResultOf` | result, plan, input | exactly one per result | the result's input reference is the input's, and its plan description is the plan's (`PlanChange` names differing fields) |
| `LedgerOf` | ledger, input | at most one per ledger | the ledger is not a refused import and `ledger.checkAgainst(input)` holds: consistency of keys and fixation counts, not identity, since a ledger carries its source's digest rather than the input's |
| `TemporalBase` | temporal input, input | at most one per temporal input; required for a base embedded by reference | the temporal input's base study is the input |
| `RecordingOf` | recording input, recording | at most one per recording input | the recording input's channels are the recording (`contentHash`) |
| `PayloadOf` | packed recording, payload | at least one per payload | the owner references the payload's digest and layout |
| `RecordingPlanInput` | recording plan, recording input | exactly one per recording plan | `RecordingInput.disagreements(input, plan)` is empty: the plan's declared provenance is the input's evidence and its prerequisites hold on the input's channels |
| `RecordingResultOf` | recording result, recording plan, recording input | exactly one per recording result | the result's input reference is the input's channels (`contentHash`), its description is the plan's, and the plan agrees with this input's evidence (`RecordingInput.disagreements`), since the channels do not cover the source, viewing geometry or marks |
| `TemporalPlanInput` | temporal plan, temporal input | exactly one per temporal plan | `plan.prerequisites(input)` is empty |
| `TemporalResultOf` | temporal result, temporal plan, temporal input | exactly one per temporal result | the result's input reference is the temporal input's, and its description is the plan's |

`ScientificManifest.of` checks structure only: unique names, relations naming existing entries of
the required roles, a packed-recording owner for every `PayloadOf`, no repeated relation and the
multiplicities above, each refusal a `ManifestError` naming the entry or relation.
`ManifestEntry.of` refuses a media kind other than the role's, a missing or superfluous identity and
a payload without its schema or with a layout of another length. A graph's own consistency is proven
only by resolution.

**Writing.** `StoredArtifact.plan`, `input`, `ledger`, `result`, `recording`, `binocular`,
`recordingInput`, `temporalInput`, `recordingPlan`, `recordingResult`, `temporalPlan`,
`temporalResult` and `packedRecording` encode a typed value through its registered codec and store the UTF-8 of the pretty-printed document; `StoredArtifact.bytes` stores existing
JSON bytes verbatim (strict UTF-8 with a schema envelope, as the pinned fixture below does), and
`StoredArtifact.payload` stores a verified payload. Every artifact holds a private copy of its bytes.
`SavedManifest.of(artifacts, relations)` builds the manifest, its canonical bytes
(`ScientificManifest.bytes`, the UTF-8 of the pretty-printed envelope) and its `address`, the SHA-256
of those bytes: the manifest is itself digest-addressed. It contains no floating-point number, so
its canonical bytes and address are the same on the JVM and Scala.js. A store may keep the manifest
in another byte form; its address is always the digest of the bytes stored.

**Resolving.** Storage is injected as a `ByteSource`, a total function from a `ByteRequest` (the
manifest under an address, or one entry) to bytes or a `SourceFailure` (`Missing`, or `Unreadable`
with a reason). `ByteSource.inMemory` serves manifests by address and entries by name;
`ByteSource.contentAddressed` serves every blob by its digest. `ArtifactDecoders` bundles the
registries the values are decoded through: `ArtifactDecoders.of(studyRegistry, inputRegistry,
resultRegistry)` for any key layout, or `ArtifactDecoders.study[U]` for the ordinary cosine route;
recordings use the built-in `recording@1`, `binocular-recording@1` and `packed-recording@1` codecs.
Recording and temporal plans and results decode only through registries added explicitly:
`decoders.withRecordings(recordingPlans, recordingResults)` (a `RecordingRegistry` and a
`RecordingResultRegistry`) and `decoders.withTemporal(temporalPlans, temporalResults)` (a
`TemporalRegistry` and a `TemporalResultRegistry`); without them such an entry is refused as
`CodecError.UnsupportedSchema(role, schema, Vector())`, and with them an entry of a schema no
registration declares is refused as `UnsupportedSchema(role, schema, registered)`, naming the
registered schemas. A decorator (counting calls, logging) extends `ArtifactDecoders.Delegating`,
which forwards every decoder, so wrapping registered decoders never turns one into a refusal.
A recording plan runs on display pixels
while the resolver is generic in the unit `U`, so `withRecordings` takes a `PixelUnit[U]`: a sealed
witness whose one instance is for `Px`, where its conversion is the identity. A manifest in any
other unit cannot register recording plans (a type error, not a cast), and a decoded
`LoadedRecordingPlan[U]` checks and runs against a recording input of the manifest's own unit
through it (`disagreements(input)`, `run(input)`). A `LoadedStudy[K, U]` and a
`LoadedTemporal[K, U]` keep their parameter, score and difference types abstract but fixed, and
expose the typed plan as `plan`: preflight, prepare, execute and inspect it directly
(`loaded.preflight(available, budget).prepare(loaded.plan, input, budget)`, then
`StudyExecution`, or `ResultInspection.study(loaded.plan, result, input, ledger)` on the result it
runs), with no encode and re-decode.
`ArtifactResolver.resolve(address, source, decoders)` reads the manifest, checks its digest against
the address and decodes it, then resolves the graph in three phases, reporting every error of a
phase as `NonEmptyVector[ResolveError]`:

1. integrity: every entry is read once, copied, and checked for its declared length
   (`ResolveError.Length`) and SHA-256 (`ResolveError.Digest`); missing and unreadable entries are
   `Missing` and `Unreadable`, an entry the storage refused to read is `Refused` with its typed
   `SourceFailure` (`OutsideRoot`, `NotRegularFile`, `Oversize`), and a source that throws is
   reported as `Unreadable` rather than propagated. Any failure stops resolution here, so a changed
   artifact is never parsed or decoded;
2. decoding: strict UTF-8 (`Text`), JSON (`Syntax`), the envelope's schema against the declared one
   (`Schema`), the registered decoder (`Decode`, carrying the located `CodecError`, for example an
   unsupported schema version or a missing result registration), and the reconstructed semantic
   identity against the declared one (`Identity`);
3. relations: every relation against the decoded values (`Relation` with a typed
   `RelationMismatch`).

A successful resolution is a `ResolvedManifest`: plans, inputs, ledgers, results, recordings,
recording inputs, temporal inputs, verified payloads, recording plans and results and temporal
plans and results, each by name in manifest order. Failure
produces no values, so an unverified input can never reach a runner. Ownership: the resolver copies
every buffer a source returns before verifying it and decodes only the copy, so a caller that
mutates its array afterwards, or even during resolution, cannot change what was verified or admitted.
Decoding a manifest document performs no reads, the resolver reads each entry exactly once and
caches nothing, and `ArtifactResolver.manifest` and `resolveManifest` expose the two halves.

Storage that is itself effectful uses `eyes4s.io.ArtifactLoading.resolve(address, read,
decoders)`, on the JVM and Scala.js, with `read: ByteRequest => F[Either[SourceFailure,
IArray[Byte]]]` for any `MonadThrow` `F`: it reads the manifest, then each entry, one `F` at a time,
and only then hands the bytes to the pure resolver. Cancelling `F` between or during reads leaves
nothing decoded, and the source releases whatever it opened in its own finalizers; a read that fails
in `F` is reported as `Unreadable`. On the JVM, `ArtifactFiles.resolve(address, root, manifestFile,
decoders)` is a directory source built on it (`ArtifactFiles.source` takes any naming of requests).
It compares real paths, so a name that leaves the root through `..`, an absolute path or a symbolic
link is refused as `OutsideRoot` without being read, as is anything but a regular file
(`NotRegularFile`) and a file larger than the entry's declared length or the manifest bound
(`Oversize`); reads are bounded as well, and each file is opened as a `Resource` and read under
`Sync.interruptible`.

**Typed payloads.** Large numeric blobs are stored as packed payloads rather than JSON. A
`PayloadLayout` declares the element kind (`uint8`, `int32`, `int64` or `float64`), the shape, the
storage order (`row-major` or `column-major`) and the byte order, which is pinned to
`little-endian`; any other declaration is refused. A layout's size is bounded only by
`PayloadLayout.maximumBytes`, the largest array a JVM allocates. A `PayloadRef` is the SHA-256 of the
payload's bytes plus its layout, so a payload is content-addressed. `VerifiedPayload.verify` checks
the length before copying anything, then the digest of its private copy; `PackedArrays.pack` and `unpack` convert typed values.
Every value is assembled from its bytes with integer shifts, so 64-bit integers keep all 64 bits and
doubles come from their exact IEEE bits on both platforms, including signed zeros, subnormals and
infinities, with no platform number formatting. NaN is refused on both sides
(`PayloadError.NotANumber`): its bit pattern is not portable.

`PackedRecordingCodecs.recording[U]` (`eyes4s.packed-recording@1`) is the escape hatch for
recordings above the inline bound `RecordingInputCodecs.maximumSamples`. Its document carries the
inline recording's metadata (frame, clock, eye, pupil unit, rate, sampling tolerance and declared
`contentHash`) and four payload references: `tMicros` (`int64[n]`), `support` (`uint8[n]`: the
category in bits 0-1, `tracked`, `blink`, `lost`, `offScreen`, and a measured pupil in bit 2),
`lineage` (`uint8[n]` indices into a `lineages` dictionary listed in order of first appearance) and
`values` (`float64[n, 3]` column-major: the `x`, `y` and `pupil` columns, with `+0.0` exactly where a
value is absent). Decoding reads payloads only through a lookup of verified payloads, requires the
declared layouts, refuses unknown support codes, non-canonical fillers and out-of-order lineage
indices at the sample they occur, rebuilds the value through `Recording.of` and compares its
`contentHash`, so a packed recording has exactly one encoding. `StoredArtifact.packedRecording`
stores it with its payloads and `PayloadOf` relations. Binocular recordings and recording inputs
whose channels are carried by reference are not yet packed: a binocular layout needs its own
schema (two eyes' value columns over one `tMicros` column) and fixture, which no shipped route
requires yet, so it is deferred rather than squeezed into `packed-recording@1`.

The pinned [manifest-v1.json](../codec/src/test/resources/eyes4s/manifest-v1.json) lists the pinned
study-v1 plan, the study-input-v1 input, its complete admission ledger
([admission-ledger-complete-v1.json](../codec/src/test/resources/eyes4s/admission-ledger-complete-v1.json),
every matched-control record admitted), the refused admission-ledger-v1 import and the
study-result-v1 archive by the SHA-256 of their resource files, with `PlanInput`, `LedgerOf` and
`ResultOf` relations. Its entry digests equal independently computed `shasum -a 256` values. The JVM
suite checks that the writer reproduces the manifest and the complete ledger byte for byte and
resolves all five artifacts end to end from memory; the portable suite checks the manifest's decoded
meaning and its byte-identical re-encoding and address on JVM and Scala.js, and that both platforms
re-encode the complete ledger to its frozen bytes (neither contains a floating-point number). The
refused ledger is carried as evidence without a relation: it records the refused import of a
variant of the source whose first record has a negative duration, so its admitted records do not
cover the pinned input (`AdmissionError.UnadmittedTrial(0)`), and declaring `LedgerOf` for it is
refused with `RelationMismatch.RefusedAdmission`.

The pinned [manifest-inputs-v1.json](../codec/src/test/resources/eyes4s/manifest-inputs-v1.json)
does the same for the recording and temporal payloads. Its twelve entries are named by their
resource files, so a directory source serving each entry under its name resolves it from the
resource directory itself: the pinned recording-input-v1 and binocular-recording-input-v1 inputs,
each with a `RecordingOf` relation to the recording its channels are
([recording-standalone-v1.json](../codec/src/test/resources/eyes4s/recording-standalone-v1.json),
`eyes4s.recording@1`, and
[binocular-recording-v1.json](../codec/src/test/resources/eyes4s/binocular-recording-v1.json),
`eyes4s.binocular-recording@1`); the same monocular recording as
[packed-recording-v1.json](../codec/src/test/resources/eyes4s/packed-recording-v1.json)
(`eyes4s.packed-recording@1`) with its four `packed-array@1` payloads
`packed-recording-v1.{tMicros,support,lineage,values}.bin` and their `PayloadOf` relations; the
source-supported study input; and the pinned temporal input with a `TemporalBase` relation to its
base study, [temporal-base-v1.json](../codec/src/test/resources/eyes4s/temporal-base-v1.json).
`GenerateInputsManifestV1` writes all of them from the typed fixture sources, together with
[temporal-study-v1.json](../codec/src/test/resources/eyes4s/temporal-study-v1.json), the temporal
fixture's plan under the conventional `eyes4s.temporal-study@1` schema, and
`InputsManifestV1JvmSuite` checks that the resource files are exactly its output, that every file
has its independently computed `shasum -a 256` digest, that every new JSON fixture re-encodes byte
for byte, that the packed recording decodes from its four payload files to the pinned recording
and re-packs to the same bytes, and that all twelve entries resolve end to end. The portable
`InputsManifestV1Suite` checks the manifest's decoded meaning, its byte-identical re-encoding and
address on the JVM and Scala.js, that both platforms pack the pinned recording to the frozen payload
digests (exact IEEE bits, no platform number formatting), the typed meaning of every new fixture,
and that the same graph over the portable fixture strings resolves on both platforms.

`eyes4s.laws.ManifestLaws.verifiedResolution(graphs, write, decoders, reproduces)` is the published
conformance for an application's own graphs and registrations, over a writer producing a
`StoredGraph` (the manifest, the exact bytes the writer stored it as, and every entry's bytes): the
stored manifest bytes decode to the manifest and are its canonical form; a written graph resolves by address,
reading each artifact once, and reproduces the values written; and corrupting any single byte of the
manifest or of any artifact is refused by exactly that digest, with the laws' instrumented decoders
never called. Writer mutants that drop an entry, alter a digest, point a relation at the wrong
artifact or swap a declared identity are each killed by a falsified property. The manifest codec
itself has a published round-trip law over generated manifests of every role and relation kind,
and `eyes4s.laws.PayloadLaws` gives the packed recording and packed arrays theirs; see
[the codec evidence](DOMAIN_CODECS.md#evidence).

## Fresh-process reconstruction

A saved study is only as good as a reader that starts from nothing but its files. The JVM suite
`FreshProcessReconstructionJvmSuite` (in `eyes4s-io`) proves this with two processes it launches,
neither of which is the test's JVM. Both run from the build's class directories: the test
classpath, which the build writes to a resource, holds the library, the test code (including the
harness) and the pinned fixtures. The reader shares no memory, registry or cache with the writer,
and it reads the saved studies only from their files and reads none of the pinned fixtures; it is
not isolated from the repository's classes. The isolated consumer runs the fixation, recording and
temporal journeys from published artifacts instead (see
[below](#the-consumer-journeys-from-published-artifacts)).

1. A **writer** process (`FreshProcessHarness write <root>`) decodes the pinned v1 fixtures, runs
   each plan, and saves three studies under `root`, each a directory holding the manifest
   (`manifest.json`), every entry under its manifest name, the application's pointer to the
   manifest (`manifest.sha256`) and the writer's exact fingerprint of the result it computed.
   Every study is its plan, its input and its result archive under one manifest:
   - `fixation`: the study-v1 plan, the study-input-v1 input, its complete and its refused
     admission ledgers and the result of running the plan (`study-result@1`), with `PlanInput`,
     `LedgerOf` and `ResultOf` relations;
   - `recording`: the recording-input-v1 input and its monocular channels as a packed recording
     with four payloads (`RecordingOf`, `PayloadOf`), the pinned recording-result-v1 I-DT plan
     (`eyes4s.recording-plan@1`), whose declared provenance is exactly the input's evidence, and
     its analysis (`recording-result@1`), with `RecordingPlanInput` and `RecordingResultOf`
     relations;
   - `temporal`: the pinned temporal input, with its missing epoch and its anchor beyond 2^53,
     stored with its base study by reference (`TemporalBase`), the pinned
     [temporal-study-v1.json](../codec/src/test/resources/eyes4s/temporal-study-v1.json) plan
     (`eyes4s.temporal-study@1`) over both repetitions, all four windows and the binned and
     Gaussian scales, and its result (`temporal-result@1`), with `TemporalPlanInput` and
     `TemporalResultOf` relations.
2. A **reader** process (`FreshProcessHarness read <root>`) registers explicitly the cosine study
   plan, input and result codecs, the I-DT recording plan and result codecs (through
   `withRecordings`, with the pixel unit witness) and the cosine temporal plan and result codecs
   (through `withTemporal`), resolves each manifest through the shipped `ArtifactFiles` directory
   source (which verifies every length, SHA-256, schema, semantic identity and relation before
   admitting anything), re-executes each plan on its verified input and compares, for every
   study: the re-executed result encoded under the canonical contract (the UTF-8 of the
   pretty-printed archive document) must have the archived entry's exact length and SHA-256, and
   the result of the typed plan, the result of the plan as registered (`LoadedStudy.run`,
   `LoadedRecordingPlan.run`, `LoadedTemporal.run`) and the decoded archive must all equal the
   writer's fingerprint. The comparison with the archive needs nothing outside the saved files;
   the writer's fingerprint is a second, independent record.

A fingerprint (`ScientificFingerprint`, test code) renders a result field by field without any
codec: every double as the sixteen hexadecimal digits of its raw IEEE 754 bits, every 64-bit value
as a decimal string. Equal fingerprints therefore mean every field is equal and every double has
the same bits (the three fingerprints carry 308, 136 and 6,178 doubles, and the suite asserts those
counts); `-0.0` and `+0.0` differ and nothing is rounded or compared with a tolerance. Result
classes without structural equality are rendered from an explicit list of their fields; a value of
any other class that is not a case class, an enum case or a collection is refused rather than
skipped. The reader reports the digest of its own rendering, so a match is two independently
computed fingerprints agreeing, and changing one bit of one double in the writer's record is
caught.

The suite requires the writer to reproduce the pinned `study-input-v1`, both ledgers,
`study-result-v1`, `recording-result-v1`, `temporal-study-v1` and `temporal-result-v1` byte for
byte (the plan differs from the pinned `study-v1.json` in whitespace only and is compared as a JSON
value), the reader to report every study reconstructed, its rerun encoding to the archived bytes
and its fingerprint the writer's, and three distinct processes.

**What verification establishes.** A manifest's byte digests protect against corruption. They do
not protect against a forger who edits an artifact and re-declares its digests, and the documents
that reference it, consistently. Such a forgery is refused only where the forged content
contradicts something the reader re-derives: a semantic identity, a relation, a ledger invariant, a
plan's recorded input, a result's own evidence or the rerun of its plan. The suite perturbs private
copies of the saved studies, each read by a fresh reader, and pins the typed outcome of exactly the
perturbed study while the other two still reconstruct. The reader reports each refusal with its
entry, its case, its located path and its innermost cause as the typed value:

| Perturbation | What the reader reports |
|---|---|
| One byte of the archived result flipped | `ResolveError.Digest(result, declared, actual)`, before anything is decoded |
| The result codecs left unregistered | `ResolveError.Decode` of each archive by its method: `MissingResultCodec(eyes4s.cosine@1)` for the study and temporal results, `MissingResultCodec(eyes4s.recording.idt@1)` for the recording result |
| One packed double moved by one unit in the last place, with the payload digest in the packed document and the manifest re-declared | `ResolveError.Decode` at `recording`: `CodecError.InputIdentity(2c826dc41ae25e67, …)`, the rebuilt recording's `contentHash` |
| The same recording re-packed with its new `contentHash` declared, and the manifest's identity re-declared | `ResolveError.Relation` on `recording-of`: `RelationMismatch.RecordingIdentity`, since the recording input's channels are still the original recording |
| The recording input forged over the same channels as well, with its identity re-declared | `ResolveError.Relation` on `recording-plan-input`: `RecordingPrerequisites(Plan(Input(ArtifactMismatch(2c826dc41ae25e67, …))))`, and on `recording-result-of`: `ResultInput(…, 2c826dc41ae25e67)`: the saved plan and archive still record the original recording |
| All of the above and the plan's input digest rewritten | `ResolveError.Relation` on `recording-result-of`: `ResultInput`, since the archive records the plan it ran |
| All of the above and the archive's embedded plan rewritten the same way | resolves, but the plan re-executed on the verified input does not encode to the archived bytes ("the re-executed result encodes to …, not the archived …") |
| All of the above and the archive replaced by the forged study's own result | **passes every check** and re-executes to the archive; only the writer's fingerprint, kept outside the saved files, differs (pinned) |
| One retained time in a temporal occupancy ledger changed (190 ms to 180 ms), manifest re-declared | `ResolveError.Decode` at `temporal-result`: `CodecError.TemporalResult(Cell(recall-encode, early, Density(s1/a/encode, …)))`: the ledger no longer weights the measure its densities were estimated from |
| One archived pair score in a temporal cell changed, manifest re-declared | resolves, but the re-executed temporal study does not encode to the archived bytes |
| Record 2 dropped from the refused ledger, manifest re-declared | `ResolveError.Decode` at `refused-ledger`: `CodecError.Admission(AdmissionError.QuarantineScope(3, Vector(2, 3, 4, 5)))`, because records 3 to 5 are quarantined with record 2 |
| Record 2 dropped and the quarantine scope rewritten to records 3 to 5, manifest re-declared | **resolves and reconstructs** (pinned): nothing binds a ledger's rejected rows |
| One bit of one double in the writer's temporal fingerprint flipped | the temporal study fails its comparison ("fingerprints differ from the writer's for rerun, loaded-rerun, archive") |

So a consistent forger can replace a whole study with a different, self-consistent one, results
included, and can drop exclusions from a ledger wherever the drop keeps its invariants; a forger
who changes an archived number that the archive does not re-derive (a score, a density, a mean) is
caught only by re-executing the plan, which the reader does. `LedgerForgerySuite` additionally pins
that dropping a standalone rejection from a reviewed ledger related to its input resolves.
Establishing authenticity (who wrote the study) needs a signature over the manifest address, which
is the application's concern. Establishing that a ledger's exclusions are the source's needs a
re-import of the source file, the deferral described under
[input payloads](#input-payloads-and-admission-ledgers).

The harness runs on the JVM, where the application process lives. On Scala.js, the pinned
archives are compared with pinned bytes by decoding and re-encoding them to the same JSON values:
`ResultV1Suite` also re-executes the pinned study plan on the pinned input and matches
study-result-v1, `ResultArchivesV1Suite` decodes recording-result-v1 and temporal-result-v1
(re-deriving every member it re-derives on the JVM), and `InputsManifestV1Suite` reproduces the
pinned manifests and packed payload digests. The recording and temporal re-executions are compared
with their archives bit for bit on the JVM only: the angular warp and Gaussian smoothing use
transcendental functions neither platform promises to round identically. A realistic-size input
for throughput is not archived: no permitted realistic-size dataset is in the repository, and G1
records this as a limit rather than inventing one.

## The consumer journeys from published artifacts

The isolated consumer under [`tools/study-consumer`](../tools/study-consumer/README.md) runs the
fixation-only route an application needs, through packaged artifacts alone, on the JVM and
Scala.js (UI-G0): a fixation table imported with its admission ledger, recipe discovery through
typed descriptors, preflight with explicit budgets, the pair-schedule preview, a run through
`StudyExecution` with progress, a cancellation that settles `Cancelled` with no result and a
completed run, a save under one manifest, a reload through a fresh resolver, a rerun compared bit
for bit, and inspection down to CSV record numbers, with a bad table and a corrupted archive turned
into coded diagnostics. It runs once for the shipped cosine and once for the consumer's own method,
key and score types. On the JVM the run is also stored in a directory, resolved through
`ArtifactFiles`, and reloaded and rerun by a separate JVM launched over the consumer's own classpath,
which `verify.py` checks holds only the consumer's own `example` classes, the packaged eyes4s jars
(each compared by SHA-256 with the locally published artifact) and third-party jars. A reader
preflights and runs a resolved plan through `LoadedStudy.plan`; to compare an archived
`LoadedResult` with the application's own score types it re-reads the result through the typed
codec it registered, because the two registrations do not share their abstract types.

UI-G1 extends the consumer to the recording and temporal routes, each for a shipped method (I-VT,
cosine) and for one of the consumer's own (a laboratory detector with its own typed parameter
descriptors; the scaled cosine over its own key and score types). A recording run is saved as its
recording input, its channels packed with four payloads, its plan and its `recording-result@1`
archive, with `RecordingOf`, `PayloadOf`, `RecordingPlanInput` and `RecordingResultOf` relations; a
temporal run as its base input, admission ledger, temporal input (base by reference), plan and
`temporal-result@1` archive, with `LedgerOf`, `TemporalBase`, `TemporalPlanInput` and
`TemporalResultOf`. Each resolves through a fresh resolver with only the route's own registrations
(`withRecordings` with the pixel witness, `withTemporal`), is re-typed through the route's codecs and
reruns through `RecordingExecution` or `TemporalExecution` to the same fingerprint and the archived
entry's SHA-256; on the JVM a separate reader process does the same from a directory. The detected
events match the pinned pymovements I-VT fixture, and every temporal ledger and contrast matches the
independent `temporal.json` oracle. The failure paths the consumer pins for all three routes, the
published laws it runs over its own types, its response-envelope smoke run and its limits are in
[its README](../tools/study-consumer/README.md#the-recording-and-temporal-journeys-ui-g1).

## Inspect results and explain failures

`ResultInspection.study(plan, result, input, ledger)` opens a completed or decoded result for
navigation. Every estimation, pair row, reduction and contrast row has a typed `ResultRef` built
from the scale index, design and trial keys, never a row position; listings page from a reference.
A contrast row refers to its two reductions, a reduction lists its member pairs as contributing,
failed or withheld, and `StudySources` maps each trial and fixation back to the admission-ledger
record and CSV record number that supplied it, or names why it cannot. The plan, run, preflight,
admission, result-reconstruction, inspection and artifact-resolution errors, and every lower-level
error they wrap, project to a `Diagnostic` with a stable code, the failing object and every
operand, so an application localises and navigates without parsing messages; the errors that are
not cataloged are listed in [diagnostics](DIAGNOSTICS.md). An inspection of a decoded archive
equals the inspection of the result it was written from. See [diagnostics](DIAGNOSTICS.md) for the
model, the source-link rules and the generated code table.

## Versions and extensions

The JSON envelope has a schema identifier and version. Its payload separately records the method
identifier/version, key schema, key layout, and parameter schema. Missing or unsupported versions
are explicit failures. The pinned [version-one project](../codec/src/test/resources/eyes4s/study-v1.json)
is exercised by the portable codec suite, so changing defaults cannot silently reinterpret it.
The study plan and the admission ledger have a second version (`eyes4s.study@2` records the
geometry, declared scales and units per degree, and the pairing; `eyes4s.admission-ledger@2` the
admission policy) and a third (`eyes4s.study@3` records the
[initial-fixation policy](FIXATION_STUDIES.md#initial-fixations); `eyes4s.admission-ledger@3` the
trial inventory).
Each is a `SchemaLadder`: its codec reads every version with that version's own meaning, writes
each value under the earliest version that expresses it, so a version-1 document re-encodes to its
own bytes, and `ladder.lift` rewrites a stored document as the latest version through each
version's upcast. The other schemas
have one version and no historical migration;
[the schema compatibility policy](DOMAIN_CODECS.md#schema-compatibility) states what a new version
means, which decoders stay readable, how unknown versions are refused and how unknown members are
treated, and `SchemaCompatibilitySuite` enforces it on every pinned v1 document.

`VersionedCodec[A]` encodes and decodes with `Either`, including checked encoding for values belonging
to a different registration. Generic maps use arrays of typed key/value entries and reject duplicate
keys, retaining all duplicate occurrence indices. They never convert user keys to JSON field names.

`StudyMethod[P, U, S, D]` belongs to `plan`; `StudyCodec` pairs it with versioned key and parameter
codecs in `codec`. This keeps the dependency direction acyclic. Registry lookup selects a closure
that already contains those types. A loaded method retains abstract score and difference types;
there is no cast or universal untyped parameter object. Use the typed `StudyCodec` directly when your
program needs the concrete custom score type. See [extending studies](EXTENDING_STUDIES.md).

Method and layout authors must change their registered versions when behavior changes. Every
score-affecting parameter must appear in the typed description. The library cannot infer the behavior
of arbitrary user-supplied comparison functions or key projections.

## Reproducibility contract for this workflow

The isolated consumer compares actual JVM and Scala.js output. Input digests, decoded JSON values
and binned contrast bit patterns must match exactly. Gaussian contrasts are checked against the
independent oracle and across runtimes with absolute tolerance `1e-12`. JSON text can differ in
numeric spelling (`1.0` versus `1`); byte-identical JSON serialization is not the contract.
Within either runtime, saving/reloading the same plan preserves its numerical results exactly.
On the JVM the fresh-process harness shows the stronger statement across processes: a study saved
by one JVM and re-executed by another reproduces every double bit for bit, and the fixation
result re-encodes to the archived bytes exactly. The consumer's journeys show the same from
packaged artifacts for all three routes, and their JVM and Scala.js runs must also agree exactly on
the input digests, the 64-bit onsets, the binned contrast bits, every progress segment with its
stated total and units, and the step at which a cancelled run stopped; for recordings, on the
event kinds, microsecond spans, sample support, labels and area memberships (event places within
`1e-9` degrees); for temporal studies, on every occupancy ledger and the binned bits (Gaussian
values within `1e-12`).

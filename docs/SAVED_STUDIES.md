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
changed fields with their before/after values. Plans have structural equality under those declared
fields. A change of weighting, bandwidth, phase, failure policy, input digest or registered method
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
`excludedPhases`, `failurePolicy`, and `reductionOrientation` (`ByLeft`, meaning
the focal trial). Repeated stimulus occurrences with distinct full keys remain
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

```scala
val runner = StudyExecution[IO]
runner.events(work, budget, quanta)   // Stream[IO, StudyEvent]: Advanced(progress)* then Finished(outcome)
runner.start(work, budget, quanta)    // Resource[IO, StudyRun]: id, progress, outcome, cancel
```

Every `StudyProgress` carries the `StudyRunId` (input digest, layout, method,
description and both quanta, so the same submission yields the same id and the
same event sequence), the step number, the cursor's `StudyStage` and its own
units, the `StudySegment` the step counts toward (trials of one scale share one
`Estimating(scale)` segment), the segment's cumulative units and typed
`SegmentTotal`, and the run's cumulative units. Totals are what preparation can
state: `Exact(trials)` for estimation;
`AtMost(candidates * (2 + cells) + focal + reference)` for a bounded comparison
segment and `AtMost(2 * candidates + focal + reference)` for a synchronous
method, where the schedule's paging visits every candidate pair and then every
reference key (or every focal key when there are no reference trials) and each
selected pair costs one unit to begin plus its cells; `AtMost(focal keys)` for
a contrast; and `Unknown` for a reduction, whose units depend on the realized
scores. The comparison budget is not part of the id: a refusal is a `Failed`
outcome, not a different run.

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
In `eyes4s-fs2`, `Execution[F]` is the one runner: it interprets a `Submission`
(id, quanta, how to begin the cursor, the segment of each stage and the total of
each segment, plus the `Stepwise` evidence) into `RunEvent`, `RunOutcome`,
`RunProgress` and `Run`. `StudyExecution`, `RecordingExecution` and
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

## Input payloads and admission ledgers

`StudyInputCodecs.study[U]` supplies two versioned codecs for the ordinary participant/stimulus/phase
route; `new StudyInputCodec(schema, ledgerSchema, layout, keyCodec)` builds them for a custom key
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

`ledger` encodes an `AdmissionLedger[K]` (`eyes4s.admission-ledger@1`): the source reference (a label
and the portable digest of the decoded header and records), the header, the recorded outcome and one
entry per source record in record order. An admitted record links its logical record number to the
typed trial key and the ordinal it supplied; a rejected record keeps its raw fields, its key when one
could be read, and a typed `AdmissionReason`. A quarantined trial names the affected records and a
`QuarantineCause`. `AdmissionLedger.of` refuses unordered records, duplicate ordinals within a trial,
quarantine scopes that omit their own record, and an outcome inconsistent with the rejected count;
`ledger.checkAgainst(input)` verifies that admitted records address every input trial exactly once
per fixation. `StudyInputRegistry` registers codecs by key schema and refuses missing or duplicate
registrations. `VersionedCodec.trials` is the generic row-array codec these payloads use.

Both payloads are artifacts of their own; plan JSON references the input by digest only. The pinned
[study-input-v1.json](../codec/src/test/resources/eyes4s/study-input-v1.json) and
[admission-ledger-v1.json](../codec/src/test/resources/eyes4s/admission-ledger-v1.json) fixtures are
checked for decoded meaning and JSON value identity on JVM and Scala.js; the JVM suite additionally
checks byte-identical re-encoding of the pretty-printed files, which Scala.js does not promise because
it renders integral doubles without a fraction.

## Versions and extensions

The JSON envelope has a schema identifier and version. Its payload separately records the method
identifier/version, key schema, key layout, and parameter schema. Missing or unsupported versions
are explicit failures. The pinned [version-one project](../codec/src/test/resources/eyes4s/study-v1.json)
is exercised by the portable codec suite, so changing defaults cannot silently reinterpret it.
This first schema has no historical migration; unsupported schemas are rejected precisely.

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

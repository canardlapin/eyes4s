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
`SegmentTotal`, and the run's cumulative units. A segment's total is stated once,
as the segment begins, and every step of the segment reports that same total.
Preparation can state `Exact(trials)` for estimation;
`AtMost(candidates * (2 + cells) + focal + reference)` for a bounded comparison
segment and `AtMost(2 * candidates + focal + reference)` for a synchronous
method, where the schedule's paging visits every candidate pair and then every
reference key (or every focal key when there are no reference trials) and each
selected pair costs one unit to begin plus its cells; and `AtMost(focal keys)`
for a contrast. A reduction's units depend on the realized scores, so
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
any row is read. Temporal failures (`StudyFailure.Temporal`) are refused with
`CodecError.Unsupported`: they name windows and epochs of the temporal route and belong to the
temporal result archive that follows the recording and temporal input payloads.

The pinned [study-result-v1.json](../codec/src/test/resources/eyes4s/study-result-v1.json) is the
pinned study-v1 plan run on the pinned study-input-v1 input. The portable suite checks its decoded
meaning and JSON value identity on JVM and Scala.js, and that re-executing the pinned plan on the
pinned input reproduces the archive bit for bit; the JVM suite additionally checks byte-identical
re-encoding of the pretty-printed file. `eyes4s.laws.StudyResultEquivalence` is the published
structural identity of two results, for round-trip laws over extension score types that keep
reference equality.

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

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
Decoding rebuilds the result through checked reconstruction: `DirectedPairwiseAnalysis.reconstruct`,
`Analysis.reconstruct`, `ReductionRow.reconstruct`, `ReductionReport.reconstruct`,
`ContrastRow.reconstruct`, `Contrast.reconstruct` (design) and `StudyScaleResult.reconstruct`,
`StudyResult.reconstruct` (plan). Each recomputes what it can from the stored parts and refuses
disagreement with the operands that disagree: a pair analysis whose row count differs from its
selected count, a stored provenance that differs from the derivation of the same rows
(`ReconstructionError.ProvenanceConflict`), a row whose `contributing` does not follow from its
outcome (`ReconstructionError.Denominator`), a report whose counts do not follow from its rows, a
contrast whose rows do not cover the sorted key union or whose operands are not the analyses' own
rows, a scale count that differs from the described estimators (`StudyResultError.ScaleCount`, which
is how a partial accumulator tagged as complete is refused), a pair or key that no estimated trial
carries (`StudyResultError.OrphanPair`, `OrphanKey`), a failure naming another trial than its row, a
density on a grid other than the plan grid, and archived score components that differ from the
method's (`CodecError.ScoreComponents`). Errors inside a stage are located, for example
`CodecError.Entry("scales[0].contrast.matched.source.rows[1]", ...)`.

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

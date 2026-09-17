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

`work.work(budget)` returns a `StudyCursor`; `work.run` drives the same cursor to
completion, so pure and resumable execution are one scientific path. Each
`cursor.advance(quanta)` performs one bounded step and returns `StudyStep.More`
with the stage it worked on (`Estimating(scale, trial)`, `Comparing`,
`Reducing(scale, design)` or `Contrasting(scale)`), the units it visited and the
next immutable cursor, or `StudyStep.Done` with the complete `StudyResult`.
`WorkQuanta(pairs, comparison)` bounds one step: `PairQuantum` limits schedule
visits and reduction/contrast keys, `ComparisonQuantum` limits cells inside one
comparison. Cursors are immutable, so re-advancing one is deterministic, and
the result does not depend on where steps were cut: pair outcomes accumulate in
schedule order and keys reduce in order of first appearance.

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

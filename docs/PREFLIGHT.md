# Recipe availability and prerequisite reports

`Preflight` in `eyes4s-plan` tells a consumer, before anything runs, whether a
shipped recipe is ready against the scientific values it actually holds and,
when it is not, exactly which object blocks it. The library owns the rules;
the application owns wording, layout and draft state.

## Ask

`plan.preflight(available)` on a `StudyPlan`, `RecordingPlan` or
`TemporalStudyPlan` returns a `StudyReport`, `RecordingReport` or
`TemporalReport`. `Preflight.families` lists the three shipped families.
`available` is the input the application can supply now, or `None`. Study and
temporal preflight accept a `PairScheduleBudget`; the default is effectively
unbounded, so pass one suited to the workload.

Every report carries the plan description and the input reference it observed,
a `Vector` of typed findings, and `notChecked`: the aspects that only execution
decides. `availability` is `Ready` when no finding is a blocker. Readiness means
the recipe will not be refused; it does not certify that estimation, comparison
or detection succeeds on the data.

## Read

A finding is a typed ADT value, never a string. Each names its object and
carries three classifications:

- `severity`: `Blocker` (the plan's constructors or prerequisites refuse the
  recipe as a whole, so `prepare`/`run` cannot proceed) or `Warning` (it runs,
  but the named trial has a deterministic failure, or the method's descriptor is
  missing or disagrees with the method; execution never consults descriptors).
- `category`: `UnavailableInput`, `IncompatibleInput`, `InvalidSetting` or
  `DataDependent`.
- `remedy`: a scientific next step such as `SupplyReferencedArtifact`,
  `AlignFrame`, `SupplyCommonMarks`, `SupplyEpoch` or `ResolveDuplicateTrials`.

`StudyFinding` names the missing or mismatched artifact, an undescribed or
inconsistent method descriptor, an exceeded budget (`BudgetError`: candidate
visits at preparation, or the selected-pair budget while paging), a trial
whose frame disagrees with the plan's nominal frame, a duplicated key with its
operand positions, and focal trials without a matched or control reference.
`RecordingFinding` names the artifact, the display frame and tracker clock, the
absent viewing geometry, absent or insufficient synchronization marks, an
angular frame or area corner the viewing geometry cannot map, and a detector
definition its factory refuses. `TemporalFinding` adds trials without an epoch,
coverage on another clock, windows that overflow an anchor, and windows with no
observed coverage; repetition-level design findings name their repetition.
A windowed or whole-frame study also reports, as warnings in input order, each
trial with fixations outside its analysis window or the screen
(`OffWindowFixations`, carrying the trial's `WindowTally` and the off-window
policy) and each trial with none inside (`NoFixationInWindow`); both suggest
`ReviewAnalysisWindow`.

`affectedTrials` lists distinct keys in the layout's canonical order. Finding
order is deterministic: plan-level checks, then trials in source order, then the
schedule's duplicate, unmatched and uncontrolled keys in schedule order. The
same input yields the same report on JVM and Scala.js.

## What preflight reuses and never does

Artifact, frame, clock, viewing and common-mark findings are derived from each
plan's own `prerequisites`, so preflight and execution refuse on one rule; any
other constructor refusal surfaces as a `Refused` blocker carrying the plan
error rather than being dropped. Preflight also reuses `inspect`, the
synchronization-evidence constructor, the detector factory, and the prepared
pair schedule from `plan.prepare`. It never estimates a density, compares two
maps, warps samples or feeds a detector. Its cost is bounded by the supplied
pair budget and by the input's row count.

Outcomes such as per-trial occupancy normalisation, comparison scores,
failure-policy reductions, per-sample synchronization, gap interpolation, event
detection and AOI assignment are reported under `notChecked`. Preflight does not
guess them.

## Act

`report.prepare(plan, input, budget)` returns the `PreparedStudy` for the same
plan and input; `RecordingReport.confirm` and `TemporalReport.confirm` return
`Unit`. Each refuses with a typed `PreflightError` when the plan description
changed (`ChangedPlan` with the field diff), the input identity changed
(`ChangedInput` with both references), or blockers remain (`NotReady`).
Preparation and execution revalidate the artifact identity again; a stale report
never bypasses them.

The isolated headless consumer and the fixation route remain the scientific
oracle. See [METHOD_DESCRIPTORS.md](METHOD_DESCRIPTORS.md) for the descriptors
preflight consults and [SAVED_STUDIES.md](SAVED_STUDIES.md) for preparation.

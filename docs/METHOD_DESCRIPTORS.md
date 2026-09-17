# Scientific configuration descriptors

The `eyes4s-plan` module provides headless inspection and typed construction for
the shipped fixation-study, I-VT recording and temporal recipes. A separate UI
can use these contracts without introducing JavaFX or Intaglio into eyes4s.

## Discover and inspect

`StudyPlan.inspect`, `RecordingPlan.inspect` and `TemporalStudyPlan.inspect`
return `Either[DescriptorError, RecipeInspection]`. Every canonical description
field has a stable ID, descriptor version, scientific meaning, units, allowed
values or constructor contract, and the current value. Gaussian estimates also
expose named sigma and edge-policy children with their own units and constraints.
`inspection.description` reproduces the plan description used for structural
diffs. The values are display projections of typed domain values, not a parameter
bag accepted by execution.

`StudyMethod.descriptor` provides the existing `MeasureInfo`, typed score and
difference component accessors, per-component ranges/units/directions, and declared
comparison properties. Cosine is symmetric, nonnegative and bounded; this API
does not classify it as a metric. Component construction may fail with a typed
descriptor error, including when a range depends on parameters.

`RecordingMethod.descriptor` retains the canonical `AlgorithmCard`. Its citations,
implementation version, assumptions, deviations and source references remain the
authority for the detector. The detector's streaming capability does not imply
that an entire recording recipe supports bounded cancellation.

[PREFLIGHT.md](PREFLIGHT.md) describes the typed availability reports that
consult these descriptors before a recipe runs.

All current recipe descriptors report `SynchronousWholeOperation`. Bounded
execution is subsequent X2–X6 work; attaching metadata to an arbitrary custom
closure cannot confer cancellation guarantees.

## Construct typed parameters

`RecipeParameters` exposes typed descriptor constructors for sigma, Gaussian
estimation, bounds, frames, grids, physical perspective, I-VT thresholds and
durations, interpolation gaps, synchronization marks/residual limits, AOIs,
relative/named windows, repetition contrasts, weighting, edge treatment and
failure policies. Compose the resulting values with the existing `StudyPlan.of`,
`RecordingPlan.of` and `TemporalStudyPlan.of` factories. Those factories remain
responsible for whole-recipe invariants and cross-field checks.

For example, `RecipeParameters.sigma[Px].parse(12.0)` constructs a checked pixel
standard deviation. `RecipeParameters.ivtThreshold.parse(velocity)` accepts a
`Velocity[Deg]`; a pixel velocity fails compilation. The threshold constructor
does not convert units. `RecipeParameters.minimumDuration.parse(Span.micros(n))`
preserves integer time and delegates to `MinimumEventDuration.of`.

`ParameterDescriptor[Raw, Value, Error]` retains all three types. `construct`
returns the domain error; `parse` wraps that error with the field identity and
original input. Range/choice metadata explains the smart constructor's contract;
it does not implement another validator. Invalid scientific values fail through
the same domain constructors used by ordinary library callers.

No universal sigma or detection-threshold default is scientifically justified,
so shipped descriptors supply none. A caller can provide a checked
`NamedParameterDefault` with an explicit name and reason. Interactive draft
state, field layouts, localization and widgets belong in the application.

## Describe an extension

Build `ParameterInfo.of(...)`, a typed `ParameterDescriptor`, and bind its typed
getter/encoding into `ParameterSet.of(...)`. Attach that set and typed score
components to `MethodDescriptor.of(...)`, then supply it to the optional
`StudyMethod` descriptor argument. Recording extensions use
`RecordingMethodDescriptor` with their actual algorithm card.

The original method constructors remain source-compatible. An extension without
a descriptor can still run and persist; `inspect` returns `MissingMethod`.
Inspection checks method identity, exact parameter names and values, and ordered
score-component names against the executable method's declarations. A missing,
extra or misencoded parameter fails. Field versions survive qualification such
as `method.multiplier`; schema and method versions retain their existing meaning.

The isolated [consumer](../tools/study-consumer/src/main/scala/example/CustomMethod.scala)
demonstrates a validated multiplier, a parameter-dependent score range and typed
custom score/difference accessors using packaged artifacts only. No runtime cast
or `Any` parameter registry is involved. These checks detect inconsistent
declarations; independent numerical tests remain necessary to establish that a
method actually implements its declared science.

## Verification

The descriptor suites check smart-constructor boundaries, static unit rejection,
structured score meanings, hidden/misencoded parameter mutants and individual
saved-field changes across all three recipe families. Existing numerical workflow
tests remain the scientific oracle. The external consumer checks custom typed
construction and save/reload/inspection on JVM and Scala.js.

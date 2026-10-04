# Scientific configuration descriptors

The `eyes4s-plan` module provides headless inspection and typed construction for
the shipped fixation-study, I-VT recording and temporal recipes. A separate UI
can use these contracts without introducing JavaFX or Intaglio into eyes4s.

## Discover and inspect

`StudyPlan.inspect`, `RecordingPlan.inspect` and `TemporalStudyPlan.inspect`
return `Either[DescriptorError, RecipeInspection]`. Every canonical description
field has a stable ID, descriptor version, scientific meaning, a `FieldView`
(quantity, bounds, allowed values, parts and rules; see
[form-grade fields](#form-grade-fields)), and the current value. Gaussian
estimates also expose named sigma and edge-policy children with their own views.
`inspection.description` reproduces the plan description used for structural
diffs. The values are display projections of typed domain values, not a parameter
bag accepted by execution.

`StudyMethod.descriptor` provides the existing `MeasureInfo`, typed score and
difference component accessors, per-component ranges/units/directions, and declared
comparison properties. Component construction may fail with a typed
descriptor error, including when a range depends on parameters.

Every registered map method has one: `ComparisonMethods` holds an entry per
`MapSimilarityMethod` with its built-in identity and descriptor, and `entry.study`
is the study method that carries it ([map comparison](MAP_COMPARISON.md#the-registry-and-study-methods)).
The component range is the measure's declared scale; the properties are symmetric
always, bounded for a bounded or correlation scale, and nonnegative when the lower
bound is. Cosine is therefore symmetric, nonnegative and bounded, Pearson symmetric
and bounded, Fisher z symmetric only; no descriptor classifies a method as a metric.
The published `ComparisonMethodLaws.registered` checks each entry's descriptor against
its measure.

`RecordingMethod.descriptor` retains the canonical `AlgorithmCard`. Its citations,
implementation version, assumptions, deviations and source references remain the
authority for the detector. The detector's streaming capability does not imply
that an entire recording recipe supports bounded cancellation.

[PREFLIGHT.md](PREFLIGHT.md) describes the typed availability reports that
consult these descriptors before a recipe runs.

A study plan's inspection reports its method's execution capability:
`BoundedComparison` for cosine (the registered kernel) and for an extension built on
a bounded comparison cursor, `SynchronousWholeOperation` for every other registered
method and for an arbitrary closure. A
temporal plan reports its base study's capability, since every cell runs the
base study's cursor (UI-G1 found it reporting the whole-operation default).
Recording recipes report `SynchronousWholeOperation`: the runner feeds the
detector's machine in sample chunks, but the flush and support assembly remain
one step. Attaching metadata to an arbitrary custom closure cannot confer
cancellation guarantees.

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
original input. Invalid scientific values fail through the same domain
constructors used by ordinary library callers.

No universal sigma or detection-threshold default is scientifically justified,
so shipped numeric descriptors supply none. A caller can provide a checked
`NamedParameterDefault` with an explicit name and reason. A recipe field carries
a `DefaultValue` only where the library itself has one (`StudyPairing.default`,
keeping every fixation). Interactive draft state, field layouts, localization
and widgets belong in the application.

## Form-grade fields

Every described field has a `FieldView`: plain data with no type members or
functions, which a host renders a control from and a codec can store.

- `FieldView.kind` is a `FieldKind`: `Numeric(quantity, shape, bounds)`,
  `Choice`, `Toggle`, `Text`, `Reference` (an identity bound elsewhere),
  `Group(parts, rule)`, `Optional`, `Repeated` or `Variant(cases)`. A group's
  `GroupRule` states what holds across its parts: `Ordered` pairs such as
  `xMin < xMax`, or `Distinct` parts such as the focal and reference phases.
- A `Quantity` names what a number measures in a kernel unit: `Planar(PlanarUnit)`,
  `Rate`, `UnitsPerDegree`, `Length(LengthUnit)`, `Duration` (integer
  microseconds), `Count(Counted)`, `Dimensionless`. `PlanarUnit` is the runtime
  value of a `Unit2D`, and every `UnitLabel` carries its own, so
  `RecipeParameters.sigma[Deg]` is `Planar(Deg)` by construction.
- `NumericBounds` has an optional lower and upper `Endpoint`, each `Open` or
  `Closed`, in the field's own quantity. Integral shapes (`Int32`, `Int64`)
  take integral endpoints only.
- A form value is a `RawValue`. A number is the text the user typed, so an empty
  or malformed entry is representable; `Numeral.write` gives the canonical text,
  identical on the JVM and Scala.js, and reading it back gives the same number.

A `NumericField[E, N, A]` parses a raw value on its own, without a whole
recipe, in three stages: the shape (text to a number of shape `N`), the declared
bounds, then the domain constructor, which stays the authority. A refusal is a
`FieldError` naming the field and the value; `OutOfBounds` also names the side,
the violated endpoint and the quantity ("sigma: 0 deg is out of bounds; it must
be greater than 0 deg."). A part of a group, case, option or list is named by its
path (`window.xMin`, `scales.1`), and 64-bit integers are compared exactly.
`ParameterSet.of` refuses a parameter whose `form` presents a different view
(`FormViewMismatch`). `RecipeParameters.forms` holds the typed fields of
every one-number parameter, and each such `ParameterDescriptor` carries its
field as `form`. `ParameterSet.validate(id, raw)` checks one method parameter,
and `MethodDescriptor.formView` and `RecipeInspection.views` give the host its
views. Recipe-level fields (frames, windows, pairing, initial fixations,
estimates) parse through the recipe forms below.

Every shipped one-number field states its bounds explicitly. Intervals are
open `(`, `)` or closed `[`, `]`; `∞` is a side with no endpoint, bounded only by
the shape. The table is generated from `RecipeParameters.forms`; the
unit-generic sigma fields are shown in pixels and degrees and take the same
bounds in every unit.

<!-- BEGIN GENERATED FIELD BOUNDS -->

| Field | Unit | Shape | Bounds |
|---|---|---|---|
| `etaXDegPerSecond` | `deg/s` | `Real` | `(0, ∞)` |
| `etaYDegPerSecond` | `deg/s` | `Real` | `(0, ∞)` |
| `extentHeightDeg` | `deg` | `Real` | `(0, ∞)` |
| `extentWidthDeg` | `deg` | `Real` | `(0, ∞)` |
| `interpolationGapMicros` | `µs` | `Int64` | `[0, ∞)` |
| `minimumDurationMicros` | `µs` | `Int64` | `(0, ∞)` |
| `minimumSamples` | `samples` | `Int32` | `[1, ∞)` |
| `residualLimitMicros` | `µs` | `Int64` | `[0, ∞)` |
| `sigma` | `deg` | `Real` | `(0, ∞)` |
| `sigma` | `px` | `Real` | `(0, ∞)` |
| `sigmaX` | `deg` | `Real` | `(0, ∞)` |
| `sigmaX` | `px` | `Real` | `(0, ∞)` |
| `sigmaY` | `deg` | `Real` | `(0, ∞)` |
| `sigmaY` | `px` | `Real` | `(0, ∞)` |
| `thresholdDegPerSecond` | `deg/s` | `Real` | `(0, ∞)` |

<!-- END GENERATED FIELD BOUNDS -->

The bounds restate the constructor, so `eyes4s.laws.FormLaws.numeric` holds them
to it: at each endpoint, one representable step inside and one outside, the
bounds admit a number exactly when the constructor accepts it, and a side with no
endpoint must accept the shape's extreme number. `FormLaws.inspection` checks
that every inspected field has a well-formed view. `FieldError` projects to the
`form-field` diagnostic family.

## Recipe forms, advisories and methods text

`StudyForm`, `TemporalForm` and `RecordingForm` present the editable fields of
the three plans. A study form takes a `StudyFormContext` (the admission frame and
the identities of the window frame and the grid) and has one typed field per
recipe field: phases, weight, failure policy, grid, window, off-window policy,
scales, units per degree, pairing and initial fixations. Every field is a
`FormField` (`StructuredField` for composite values): it parses its own raw value
through the view's shape, bounds and rules and then the domain constructors, so a
host validates one field without a whole valid recipe (`validate`). `parse`
checks every field and reports all refusals together; `values(plan)` gives the
raw values of an existing plan. Choices show English labels (`Labelled`), keyed
by stable tokens. The only defaults are the library's: `StudyPairing.default`
and keeping every fixation.

`StudyRecipe.plan(input, layout, method, parameters)` runs the whole-recipe
checks on demand and refuses with a `StudyRecipeError` whose `field` is the
`StudyField` it concerns (a window without its off-window policy, a degree scale
without units per degree, duplicate scales, an initial-fixation cross off the
frame). `TemporalRecipe.plan` and `RecordingRecipe.plan` rebuild those plans and
return their own typed refusals.

`StudyAdvice.facts(plan)` derives the grid cell in frame units and degrees and
each bandwidth in frame units, degrees, cells and as a fraction of the mapped
region. `StudyAdvice.advisories(plan)` warns, keyed by `StudyField.Scales`, when a
bandwidth spans fewer than `MinimumSigmaCells` (2) grid cells or at least
`NearUniformFraction` (0.25) of the mapped region. The first is the Studio
design's rule; the second is chosen so that the design's example (8 degrees,
0.36 of the image) warns and 4 degrees (0.18) does not. On the Studio fixture,
sigma 0.5 and 8 degrees warn, 1, 2 and 4 do not. Advisories project to the
`study-advisory` diagnostic family with warning severity.

`StudyText.sentence(plan)` and `StudyText.methods(plan)` give the recipe sentence
and the methods text as `Phrase`s of `Token`s: fixed words, or values with the
form field they come from and a `TokenRole` (query, reference, match, control,
scale, metric, policy), so a host styles and localises them without parsing
English. Run counts are not part of a plan and are not in the text.

A host holding an erased `FieldError[Any]` from `ParameterSet.validate` projects
it with `Diagnose.reportedFormField`.

## Stored forms

A host stores a form's fields and the values a user typed with two codecs in
`eyes4s-codec`, cross-built and pinned on the JVM and Scala.js:

- `FormCodecs.view` (`eyes4s.form-view@1`) writes a `FormView`: its definition
  and every `FieldView` with its kind, quantity (units are their symbols),
  shape, bounds, choices, parts, rules and default. Decoding rebuilds each field
  through `FieldView.of`, `NumericBounds.of` and `DefaultValue.of`, so a stored
  view cannot hold a field the plan would refuse, and field ids are distinct.
  A numeric field must measure a number: `FieldView.of` refuses the `nominal`
  and `composite` quantities there (`NonNumericQuantity`).
- `FormCodecs.values` (`eyes4s.form-values@1`) writes `FormValues` in ascending
  field order. A number stays the text the user typed (an empty entry, a
  malformed entry and a 64-bit integer survive exactly); an absent value is left
  out, and a repeated field is refused.

`eyes4s.laws.FormLaws.stored` holds a stored form to its original: the view and
the values read back as written and re-encode to the same documents, and every
field of the restored view checks each stored value, and each probe, as the
original does. `FormLawSuite` runs it over the shipped study, temporal and
recording forms and the pinned fixtures (`form-view-v1.json`,
`form-values-v1.json`).

`ParameterUnits` and `ParameterDomain` are deprecated. `ParameterInfo.units` and
`allowed` remain as projections of `quantity` and `kind`, and the deprecated
`ParameterInfo.of(id, version, meaning, units, allowed)` translates what it can
and refuses the rest with `DescriptorError.UntranslatableLegacy`.

## Describe an extension

Build a `NumericField.of(...)` and `ParameterDescriptor.numeric(field)` (whose
metadata and constructor are the field's own), or a `FieldView`,
`ParameterInfo.of(view)` and a typed `ParameterDescriptor`, and bind its typed
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

# Matched/control analysis and contrast contract

Implemented contrast slice, 2026-09-08. This document defines the scientific acceptance case for
`x-contrast` and the first two slices in [the development plan](DEVELOPMENT_PLAN.md).
The production operation now reproduces the independently pinned scalar and structured targets.
General fixation-table admission, R-facing exports, and the remaining scientific workflows are
separate baseline work; this fixture starts from typed fixation summaries.

## The study and estimand

[The synthetic fixation table](../tools/r-parity/fixtures/matched-control.csv) has two participants,
three images, and encoding/recall phases: 12 trials and 48 fixation summaries. Each trial has four
fixations at the centres of a 2-by-2 pixel grid. Durations are integer multiples of 100 ms and total
900 ms per trial. The input declares support counts; no raw recordings, sample indices, device
measurements, or fitted detectors are invented by the example. Trial clocks are distinct.

Duration-weighted occupancy is binned and normalized to cell mass. There is no KDE in this case.
In x-fastest cell order each duration vector is a permutation of `(1, 2, 2, 4)`. Its sum is 9 and
squared Euclidean norm is 25, so the cosine of two normalized maps is exactly their integer dot
product divided by 25. [The independent oracle](../tools/r-parity/generate_reference.py) uses
rational arithmetic and explicit Cartesian-product enumeration.

The focal unit is a recall trial. Its target is the encoding trial with the same participant and
image. Its controls are the two encoding trials from the same participant with different images.
All eligible controls are used; this fixture does not certify finite-cap random sampling or a
permutation significance test. The result of interest is matched cosine minus mean control cosine.
A positive result means greater similarity to the matched template under this declared design;
it does not by itself establish statistical significance.

| Recall trial | Matched cosine | Mean control cosine | Difference | Matched / control count |
|---|---:|---:|---:|---:|
| s1/a | 1 | 18/25 | 7/25 | 1 / 2 |
| s1/b | 4/5 | 9/10 | -1/10 | 1 / 2 |
| s1/c | 4/5 | 41/50 | -1/50 | 1 / 2 |
| s2/a | 21/25 | 41/50 | 1/50 | 1 / 2 |
| s2/b | 1 | 17/25 | 8/25 | 1 / 2 |
| s2/c | 21/25 | 43/50 | -1/50 | 1 / 2 |

The generator independently verifies the exported `eyesim::template_similarity` results against
these fractions, using a composite participant/image key and exhaustive within-participant
controls. [PARITY.md](../PARITY.md) records the measured agreement and adversarial differences.

## What can run now

[MatchedControlExample](../laws/src/test/scala/eyes4s/examples/MatchedControlExample.scala) is
compiled in a consumer package outside the implementation packages. It uses public constructors
for intervals, fixations, scanpaths, grids, intensity and mass; public pairing descriptions;
`evaluatePairs`; explicit `meanByLeft(RequireAll)` reductions; and `contrast`. It preserves pair
scores and reports failures. The example consumes generated fixation values, not a general CSV
admission API. `MatchedControlExample.analyse(study)` is the tested convenience composition.

[MatchedControlSuite](../laws/src/test/scala/eyes4s/examples/MatchedControlSuite.scala) checks exact
pair membership and counts, normalized duration weights, scalar reference values, row-order
invariance, unmatched trials, duplicate focal/reference keys, incompatible geometry, and explicit
successful-only reduction. It also verifies undefined constant Pearson behavior and structured
score means. Numerical comparisons use `Tolerance(absolute = 1e-12, relative = 0)`; key and count
comparisons are exact.

The core composition is:

Run the complete [MatchedControlExample](../laws/src/test/scala/eyes4s/examples/MatchedControlExample.scala) with its [asserted results](../laws/src/test/scala/eyes4s/examples/MatchedControlSuite.scala). It constructs both designs, evaluates their edges, reduces by focal key, and contrasts the two reductions.

The executable example defines a tuple-based `Ordering[StudyKey]`. `contrast` aligns by key equality
and sorts the union using that explicit ordering. It rejects observed ordering ties between distinct
keys. Each result row exposes `key`, `matched`, `control`, and `difference`; no successful-row filter
is applied. A signed scalar is read with `.value`. A structured result exposes `shape`, `direction`,
`length`, `position`, and `duration`, each a `SignedDifference`.

`Analysis.entries` exposes each key's reduced result and selected, successful, failed, and contributing
counts. Failed reductions have zero contributing scores, while successful-only reductions use their
actual successful count. `Analysis.source` retains the original pairwise result, including typed
failure values and the full pairing report. A `Contrast` retains both analyses under their matched
and control roles, including both input digests and provenance histories.

## Production contrast semantics

A contrast combines two compatible reductions by focal key. It must preserve both source analyses
and identify their roles as matched and control. Matching keys by row position, intersecting the
key sets silently, averaging participant summaries with unequal implicit weights, or replacing
failed scores with zero is unacceptable.

`Contrastable[S, D]` has separate input and output score types: a bounded score is generally not closed
under subtraction. A scalar cosine contrast is a finite signed difference. A `MultiMatchScore`
contrast has five named signed components; it is not a `MultiMatchScore` and does not implicitly
collapse to a mean. Do not require a group instance for a bounded score type.

A second, analytic score-algebra fixture supplies these values independently of the MultiMatch
algorithm. It tests the downstream contract, not scanpath alignment:

| Component | Matched | Two control inputs | Control mean | Required difference |
|---|---:|---|---:|---:|
| shape | 0.9 | 0.2, 0.4 | 0.3 | 0.6 |
| direction | 0.8 | 0.4, 0.6 | 0.5 | 0.3 |
| length | 0.7 | 0.6, 0.8 | 0.7 | 0 |
| position | 0.6 | 0.8, 1.0 | 0.9 | -0.3 |
| duration | 0.5 | 1.0, 0.8 | 0.9 | -0.4 |

### Compatibility and failure cases

| Case | Required behavior |
|---|---|
| Same keys, different row order | Align by key and return identical keyed results. |
| Focal key appears on only one side | Retain the union of keys; that row names the missing matched/control operand. |
| Duplicate focal identity | Reject ambiguity before subtraction; retain the key and its occurrence indices. |
| One or both reductions fail | Preserve both operands' diagnostic evidence and return a per-key failure. |
| Different reduction orientation | Reject before subtraction; name both orientations. |
| Different failure policy or minimum-success rule | Reject implicit mixing; name the policies. A later explicit conversion may define a different estimand. |
| Different measure, scale, method parameters, or score components | Reject with both method specifications; a shared Scala score type is insufficient. |
| Incompatible geometry or temporal convention for the chosen measure | Reject through existing compatibility contracts. Distinct trial clocks alone do not prohibit an order-free spatial comparison. |
| Different input digests | Preserve both. Distinct datasets are expected and are not, on their own, an incompatibility. |
| Finite operands whose subtraction overflows | Return a typed numerical failure naming the component and operands. |
| Empty focal domain | Return an explicit empty-domain failure rather than a zero-valued study result. |

A successful row should expose both reduced operands, their effective contributing-pair counts,
and its signed difference. Result-level diagnostics should retain eligible, selected, successful,
failed, unmatched, and ambiguous cases for each side. Per-key denominators must be available even
when `SuccessfulOnly` changes them; global counts alone cannot support a row-level interpretation.
A canonical output order must be specified without constraining user keys to accidental string
ordering. Relabeling or reordering inputs must not alter keyed numerical results.

### Declaring an evaluator

`EvaluationInfo` carries an optional `EvaluationSpec`. Existing name-and-scale evaluations remain
valid for pair scoring and reduction; contrast rejects either side if its specification is absent.
A declaration includes method identifier, revision, named typed parameters, ordered score components,
spatial domain, and temporal convention. Its smart constructor rejects empty or duplicate names and
non-finite numeric parameters, and canonicalizes parameter order.

Method authors must declare all score-affecting settings, including preprocessing. The fixture
names duration weighting, binned occupancy, unit-mass normalization, and its grid. It declares
`EvaluationTime.OrderFree`, so distinct trial clocks are appropriate. A temporal method can instead
require ordering, relative microseconds, or a shared clock. This declaration is an author contract;
the library cannot infer the behavior of an arbitrary evaluator function.

`EvaluationGeometry.inFrame` and `.onGrid` capture nominal and structural geometry plus the existing
`UnitLabel` witness. Frame, grid, and shared-clock checks delegate to `Agreement`. The specification
also enters structured provenance, so changing a method parameter changes the provenance digest.
Different dataset digests are retained and do not themselves prevent a contrast.

`ContrastLaws.subtraction` is published in `eyes4s-laws`. Downstream authors supply a generator,
component projections, an independent nonzero oracle, and a named tolerance. Built-in instances
cover `Double`, `Similarity`, `MeasureDistance`, and `MultiMatchScore`; the last produces a
`MultiMatchDifference`. No group instance or implicit scalar aggregation is required.

## Acceptance boundary

Production scalar and structured contrasts are checked against the pinned fixture. Adversarial tests
cover key union and reordering, missing and ambiguous operands, both reductions failing, strict and
successful-only denominators, method/parameter/scale/component mismatches, spatial units and
identities, clock disagreement, empty domains, and finite subtraction overflow. Compile-time tests
reject substituting signed outputs for bounded scores. The 12 fixture tests and 29 contrast
conformance tests pass on both JVM and Scala.js; [PARITY.md](../PARITY.md) records full validation
and the two rejected mutations.

This completes `x-contrast`. The complete M1 inventory, broader eyesim parity, general CSV admission,
and comparison exports remain open. The next core slice describes and persists this concrete
workflow through the smallest useful plan/registry/codec boundary.

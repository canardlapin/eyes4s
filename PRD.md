# eyes4s PRD: A Typed Core for Eye-Movement Analysis

## Status

Draft product requirements for `eyes4s`, a Scala 3 library for eye-movement analysis, event
detection, attention mapping, and gaze-contingent tracking.

`eyes4s` is not a port of `eyesim`, not a Scala binding to a Python eye-tracking stack, and not a
statistics package. It is a standalone typed core for gaze data, informed by a full read of `eyesim`
and by the design decisions recorded in [`eyes4s.md`](eyes4s.md), which is the architecture
specification this document turns into a release contract.

Four scope decisions are settled and govern everything below:

| Decision | Value |
|---|---|
| v1.0 feature boundary | **Thesis core**: `kernel`, `core`, `detect`, `surface`, `compare`, `design`, `laws`, `fs2`, `io` (ASC/CSV). Raw samples through to a contrast. |
| Parity with `eyesim` | **Advisory fixtures**, reported in CI, documented in `PARITY.md`. Never a release gate. |
| Platforms for v1.0 | **JVM + Scala.js.** Scala Native deferred to post-1.0, with dependencies kept Native-eligible throughout. |
| Primary audience | **Published open-source library** for the eye-tracking research community. |

The audience decision and the feature boundary pull in different directions and the resolution is
deliberate: the audience governs *how* v1.0 ships — documentation site, binary-compatibility policy,
Maven Central publication, contribution process, reproducible verification — while the feature
boundary stays the thesis core. BIDS ingest and additional vendor formats are therefore a **firm
v1.1 commitment** (§Release Roadmap), not an open-ended aspiration.

---

## Evidence Base

The design is grounded in the following state as of July 2026. Items marked *(measured)* were
verified directly in this repository or in `~/code/eyesim`.

### Scala platform

- Scala publishes two lines: an LTS line at 3.3.8 and a Scala Next line, with LTS recommended for
  published libraries.
- The sibling `4s` libraries (`linop4s`, `graph4s`) pin `val Scala3 = "3.3.8"`, sbt 1.11.7,
  sbt-typelevel 0.8.7, `crossProject` with `CrossType.Pure`, and `tlCrossRootProject`. *(measured)*
- Dependency versions in current use across the sibling repositories: cats-core 2.13.0,
  cats-effect 3.7.0, fs2 3.13.0, munit 1.3.4, munit-scalacheck 1.3.0, discipline-munit 2.0.0,
  scalacheck 1.19.0, sbt-scalajs 1.22.0, sbt-scala-native 0.5.12, portable-scala crossproject
  1.3.2, scalafmt 3.11.4, circe 0.14.16. *(measured)*
- `linop4s` enforces module boundaries with a `checkModuleBoundaries` sbt task that inspects the
  resolved dependency graph and fails if a pure module transitively acquires `cats-effect` or
  `co.fs2`. *(measured)*
- Spire's most recent release predates Scala Native 0.5 and its array instances define a
  length-zero `zero` with length-padding `plus`, which is incompatible with a per-space carrier.
  `linop4s` rejects it in core for these reasons and defines its own vector-space layer. *(measured)*
- `algebra` provides scalar structures but no vector-space abstraction, so there is nothing for
  `eyes4s` to inherit at that layer.

### Sibling libraries

- `gale` — cross-platform dense/sparse linear algebra with dense factorizations, an Either-first
  public API, and a published `gale-laws` module. *(measured)*
- `linop4s` — matrix-free linear operators, capability types with provenance, Krylov solvers where
  non-convergence is a value rather than an exception. *(measured)*
- `graph4s` — lawful graph kernel separating `GraphExpr` / `Graph` / `IndexedGraph`. *(measured)*
- `frame4s` — typed local dataframe over Scala 3.7 named tuples; requires Scala 3.7.4. *(measured)*
- `intaglio` — grammar of graphics, cross-compiled to JVM and Scala.js. *(measured)*
- `fmrihrf` — HRF bases, convolution, and an R-parity fixture harness under `tools/r-parity/`.
  *(measured)*
- The convention for a new library's first artifact is a single long design specification named
  `<lib>.md` at the repository root, followed by `PRD.md` where one exists. *(measured)*

### The `eyesim` reference implementation

A complete read of `eyesim` (5,143 lines of R across 18 files) established the following, each
verified against source. These are the requirements' primary motivation.

- The package's atom is a fixation. There is no representation of raw gaze samples anywhere, and
  therefore no velocity, pupil, microsaccade, pursuit, drift, or data-quality capability. *(measured)*
- `eye_table` computes the complete coordinate frame — clip bounds, x-direction, y-direction, and
  whether coordinates were relativized — and persists only the centroid in
  `attr(res, "origin")`, which `[.eye_table` then drops. `eye_frame.R:33`. *(measured)*
- The same `sigma` yields a 4× different kernel bandwidth depending on whether `ks` is installed:
  the `ks` path uses `H = diag(sigma^2, 2)` (`similarity.R:1337`) while the fallbacks pass `h = sigma`
  into functions that internally compute `h <- h/4` (`similarity.R:1364`, `:1729`). The switch is
  announced by a `message()` mid-computation. *(measured)*
- `Ops.eye_density` defines `+` as `(z1 + z2)/2`, `/` as `log(z1/z2)`, and returns all three results
  tagged `eye_density` while dropping `sigma`. A signed difference map is therefore accepted by
  `fixation_entropy.eye_density` as a probability mass. `similarity.R:1411`. *(measured)*
- `eye_density(..., weights = )` is documented, is one of seven positional parameters on the
  generic, and is never read by the method. `similarity.R:1185`. *(measured)*
- `estimate_scale` fits its scaling on columns `1:2` of a `fixation_group`, which are `index` and
  `x`, not `x` and `y`. `expansion.R:30`. *(measured)*
- The permutation baseline is implemented three times with divergent semantics — first-occurrence
  versus all-occurrence self-removal, and subsample-then-remove versus remove-then-subsample — so
  the realized count is nondeterministic and must be returned as an `n_perm` column.
  `similarity.R:196`, `:268`, `:976`. *(measured)*
- The `match()` join preamble is copy-pasted verbatim in four places, with the same warning string.
  `similarity.R:121`, `regression.R:99`, `expansion.R:72`, `similarity.R:911`. *(measured)*
- The flagship vignette calls `template_similarity(enc_dens, ret_dens, match_on = "image")` on a
  60-row table of 3 participants × 20 images. Because `match()` takes the first hit, every
  participant's retrieval map is compared against participant s1's encoding map. The vignette
  demonstrates the `paste(participant, image)` workaround two sections later without noting that the
  earlier call required it. `vignettes/eyesim.Rmd`. *(measured)*
- The repetitive-similarity vignette says it compares the same image across conditions, but calls
  `repetitive_similarity(..., condition_var = "phase")`. The implementation defines `repsim` using
  phase equality alone and `othersim` using phase inequality alone; it never reads image or
  participant. Under the vignette design, the intended same-participant, same-image, different-phase
  pair is placed in `othersim`, while `repsim` contains same-phase, cross-image and cross-participant
  pairs. A real reinstatement effect therefore biases the documented `repsim - othersim` contrast
  downward. `repetitive_similarity.R:147-150`, `vignettes/RepetitiveSimilarity.Rmd`. *(measured)*
- `template_regression(method = "rank")` returns Spearman partial correlations from `ppcor::pcor`
  but labels them beta weights, while `template_multireg(method = "logistic")` applies a binomial
  GLM to continuous sum-normalised grid values with no observation model. `regression.R:45-46`,
  `:124-129`. *(measured)*
- `density_matrix` is exported, documented, carries a `\dontrun` example, and has zero methods; it
  always errors. The operation it names is separately reimplemented three times as private helpers
  with divergent NA and ragged-length behaviour. `all_generic.R`. *(measured)*
- Nine distinct parameter names denote "the name of the list-column holding my objects": `fixvar`,
  `refvar`, `sourcevar`, `template_var`, `source_var`, `density_var`, `result_name`, `outcol`,
  `outvar`. *(measured)*
- MultiMatch alignment constructs an `igraph` object and runs Dijkstra over an (n−1)×(m−1) lattice.
  The lattice is a DAG topologically ordered by `i + j`, so an O(nm) dynamic program is exact and
  the graph dependency is unnecessary. `multimatch.R:78`. *(measured)*
- Scanpath simplification — the `grouping` / `TDir` / `TDur` / `TAmp` step of canonical MultiMatch —
  is not implemented in R at all; it exists only in the `reticulate` bridge to the Python
  `multimatch_gaze` package. `multimatch.R:296`. *(measured)*
- `crqa` is 16 lines, unexported, not in `NAMESPACE`, with four declared parameters (`delay`,
  `embed`, `rescale`, `metric`) that are never forwarded. `crqa.R`. *(measured)*
- `template_multireg` tests `if (method == "lm")` without `match.arg`, so calling it with the
  documented default — a length-4 character vector — is an error on R ≥ 4.2. The file has zero test
  coverage. `regression.R`. *(measured)*
- Sum-normalization to a probability vector occurs at approximately ten sites across four files,
  with four different epsilon guards and no record of whether a given map has already been
  normalized. *(measured)*

### Domain references

Cited by name and identifier rather than URL so that every pointer is checkable without relying on a
transcribed link.

- **Event detection.** Salvucci & Goldberg (2000) for the I-VT and I-DT taxonomy; Engbert & Kliegl
  (2003) for microsaccade detection; Nyström & Holmqvist (2010) for adaptive velocity thresholds;
  Hessels et al. for I2MC noise-robust fixation detection; Dar, Wagner & Hanke for REMoDNaV.
- **Scanpath comparison.** Jarodzka, Holmqvist & Nyström (2010) and Dewhurst et al. (2012) for
  MultiMatch; the `multimatch_gaze` Python package as the reference implementation; Cristino et al.
  (2010) for ScanMatch; Anderson et al. (2015) for a comparative review of scanpath measures.
- **Saliency evaluation.** Bylinskii et al., *What do different evaluation metrics tell us about
  saliency models?*, for the AUC-Judd / AUC-Borji / shuffled-AUC / NSS / CC / SIM / KL / IG family;
  the MIT/Tübingen saliency benchmark as the fixture source.
- **Fixation-map statistics.** Caldara & Miellet's `iMap` for pixel-wise statistical mapping of
  fixation maps with multiple-comparison control.
- **Data quality.** Holmqvist et al. for the accuracy / precision / data-loss battery, including
  RMS sample-to-sample precision.
- **Reading measures.** The standard first-pass / second-pass battery: first-fixation duration,
  single-fixation duration, gaze duration, go-past time, regression-path duration, total time,
  regressions in and out.
- **Ambient/focal dynamics.** Krejtz et al. for the K coefficient.
- **AOI-sequence models.** Chuk, Chan & Hsiao for EMHMM.
- **Visual-world and relational-event paradigms.** Hafri & Trueswell (2025),
  *Apprehending relational events: The visual world paradigm and the interplay of event perception
  and language*, Brain Research 150000, DOI 10.1016/j.brainres.2025.150000, for preview-time
  manipulations, role/filler independence, alternative event construals, and the cascading relation
  between event apprehension and linguistic encoding.
- **Standards and formats.** BIDS includes an eye-tracking modality specification with TSV data and
  JSON sidecars. EyeLink `.edf` is a proprietary binary format readable only through SR Research's
  `edfapi`; the `edf2asc` utility produces a text `.asc` that is freely parseable.

---

## Product Thesis

A gaze record is a timed trajectory through a known geometry, and it has a shadow: the measure that
trajectory induces on the stimulus. Eye-movement statistics live on one side or the other of that
duality, and knowing which side is half the design.

`eyes4s` exists because every library in this space represents a fixation as a row of floats. None
carries the screen, the viewing distance, the y-axis direction, the clock domain, or the
normalization state in the value. The result is a literature-wide tax paid in silent unit errors,
y-flips, and comparisons between incommensurable maps — a tax this document's Evidence Base
quantifies in one well-written package.

The product claim is therefore narrow and checkable:

> **`eyes4s` makes the conventions that eye-movement analysis leaves in the analyst's head into
> types the compiler checks, and in doing so makes several subfields reachable that a
> fixation-table library cannot express at all.**

The second clause is the commercial argument. Starting at raw samples rather than fixations, and
making comparison heterogeneous rather than same-type, is what puts microsaccades, data quality,
pupillometry, reading measures, saliency benchmarking, and statistical mapping inside the same
library instead of five different ones.

---

## Design Doctrine

These are binding on every requirement below. They restate the pillars of `eyes4s.md` in
requirement form.

- **DOC-1. Geometry is a member, not an argument.** No public function takes a screen size, viewing
  distance, or bounds that a carried value could have supplied.
- **DOC-2. Units are static; frame identity is nominal-runtime.** `Px` versus `Deg` is a compile-time
  distinction. *Which* screen or stimulus is a runtime value with nominal identity, because the
  library cannot know at compile time which of 200 images a trial belongs to.
- **DOC-3. The unit lives on the data, not only on the parameter.** A threshold typed `Velocity[Deg]`
  proves nothing if the samples it consumes are untyped. `Sample`, `Event`, and `Scanpath` are
  unit-parameterized or the guarantee is theatre.
- **DOC-4. Parse, don't validate.** Every domain type has a private constructor and an
  `Either`-returning smart constructor. Invariants are properties of the type.
- **DOC-5. Trajectory and occupancy are distinct types with an explicit lossy map between them.**
  `occupancy: Scanpath[U] => PointMeasure[U]` discards order and says so.
- **DOC-6. A measure declares its own result type, its own scale, and the laws it actually
  satisfies.** No named vectors; no claiming metric status for a median-of-distances aggregate.
- **DOC-7. One detector definition, two runtimes.** Pure state machines; batch in core, streaming in
  the effect module.
- **DOC-8. Designs, pairings, and baselines are data.** A permutation baseline is not a code path.
- **DOC-9. Failure is a value; diagnostics are data.** No function silently changes its caller's row
  count, and no function reports a problem only through a warning.
- **DOC-10. The trajectory-and-measure layer contains no ocular vocabulary**, and this is enforced by
  a module boundary rather than a convention.

---

## Goals

1. Represent gaze data from raw samples through events, scanpaths, occupancy measures, and
   comparisons, with geometry and units carried in types throughout.
2. Provide event detection that runs identically offline and online, on the JVM and in a browser.
3. Provide a lawful comparison hierarchy in which each measure ships under the interface it actually
   satisfies, with law suites as published library code.
4. Provide a design layer in which matched and permuted analyses are the same function applied to
   different pairings, with unmatched and ambiguous keys returned as data.
5. Reproduce `eyesim`'s analyses where `eyesim` is correct, and document every divergence with its
   cause.
6. Ship as a credible open-source library: semantic versioning, binary-compatibility policy,
   documentation site, reproducible verification, published law modules.
7. Preserve observed stimulus-side timing and its synchronization evidence so later experiment
   modules can align gaze to language or other events without treating a planned schedule as an
   observation.

## Non-Goals

`eyes4s` is not, and v1.0 will not become:

1. A general time-series or signal-processing library.
2. A statistics package. No mixed models, no GLMM, no GAMM. Results are exported for analysis
   elsewhere. Permutation and bootstrap inference over the library's own result types is in scope;
   general inferential modelling is not.
3. A plotting library. `eyes4s-viz` emits specifications; `intaglio` renders them.
4. A vendor SDK binding or acquisition driver. No `edfapi`, no Tobii Pro SDK, no proprietary
   binary formats in v1.0.
5. A saliency-model *training* framework. Evaluation is in scope; training is not.
6. A tensor library.
7. A package that encodes grid dimensions at the type level.
8. A package that claims metric status for measures that do not satisfy the metric axioms.
9. A forced-alignment, natural-language parsing, or computer-vision role-inference system. Those
   systems may supply typed annotations; eyes4s does not infer them.

## Target Users

**Primary — eye-tracking researchers publishing methods and analyses.** They need correct units,
reproducible verification, documented divergence from familiar tools, and formats they already
have. This is the audience the release process is built for.

**Secondary — method developers.** They need `Compare`, `Pairing`, `Detector` and `Smoother` to be
open for extension with law suites they can run against their own instances. `eyes4s-laws` is
published as main-scope library code precisely for them.

**Tertiary — experiment builders.** They need detection in the browser and gaze-contingent
primitives. The Scala.js target exists for them, and is why Native is deferred rather than JS.

**Quaternary, and strategically decisive — a planned desktop/web application.** A separate product,
not part of this repository, for which `eyes4s` is the analysis engine and whose users are
psychologists rather than Scala programmers. It is not built in v1.0 and its absence gates nothing,
but several architectural decisions are cheap now and expensive to retrofit. Those are specified in
§Application-Layer Requirements and are binding on v1.0.

**Explicitly not targeted in v1.0** — users needing a turnkey GUI *today*, users needing
vendor-native binary ingest, and users wanting a drop-in `eyesim` replacement with bit-identical
output.

---

## Package Shape

```
eyes4s/
  kernel/        eyes4s-kernel    JVM+JS   cats-core
  core/          eyes4s-core      JVM+JS   kernel
  detect/        eyes4s-detect    JVM+JS   core
  surface/       eyes4s-surface   JVM+JS   core
  aoi/           eyes4s-aoi       JVM+JS   core
  compare/       eyes4s-compare   JVM+JS   core, surface, aoi
  design/        eyes4s-design    JVM+JS   core, compare
  plan/          eyes4s-plan      JVM+JS   design          -- analyses as descriptions (APP-1..4)
  codec/         eyes4s-codec     JVM+JS   plan, circe     -- JSON round-trip (APP-5..8)
  laws/          eyes4s-laws      JVM+JS   all pure modules; munit, scalacheck, discipline (MAIN)
  fs2/           eyes4s-fs2       JVM+JS   core, detect, plan, fs2, cats-effect
  io/            eyes4s-io        JVM+JS   fs2 module
```

Deferred beyond v1.0: `eyes4s-gale`, `eyes4s-graph4s`, `eyes4s-viz`, `eyes4s-frame4s`,
`eyes4s-vwp`.

`eyes4s-plan` and `eyes4s-codec` exist because of §Application-Layer Requirements. They are the
only structural additions the planned application imposes, and both are pure modules subject to
PKG-3.

**PKG-1.** `eyes4s-kernel` contains the trajectory-and-measure layer and **must not depend on
`eyes4s-core`**. It contains no type, method, or identifier naming an ocular concept.

**PKG-2.** `eyes4s-core` contains the eye-specific layer: `Gaze`, `Sample`, `Recording`, `Eye`,
`Rate`, `Fixation`, `Saccade`, `Blink`, `Pursuit`, `Scanpath`, `Viewing`.

**PKG-3.** No module outside `eyes4s-fs2` and `eyes4s-io` may depend on `cats-effect` or `co.fs2`.

**PKG-4.** PKG-1 and PKG-3 are enforced by a `checkModuleBoundaries` sbt task that inspects the
resolved dependency graph and fails the build on violation. Documentation of these rules is not
sufficient.

**PKG-5.** `eyes4s-kernel`'s only external dependency is `cats-core`. Every other pure module's
external dependency set is empty.

**PKG-6.** `eyes4s-laws` declares munit, munit-scalacheck, discipline-munit and scalacheck as
**main-scope** dependencies, so downstream authors can run the suites against their own instances.

---

## Core Domain Model

The authoritative signatures are in [`eyes4s.md`](eyes4s.md). This section states the requirements
those signatures must satisfy.

### Quantities and time

**T-1.** `Instant` and `Span` are opaque over `Long` microseconds. Floating-point time is not
permitted in the domain model.

**T-2.** `Instant` provides `Order`; `Span` provides `Order` and `CommutativeGroup`; `Instant - Instant`
yields `Span` and `Instant + Span` yields `Instant`.

**T-3.** `Interval` is **absolute**: it carries a `ClockId` and stores both `onset` and `offset`.
Duration is derived. No type in the library represents a temporal extent as onset-plus-duration.
Intervals are produced by events from a `Recording`; hand-construction is rare and requires naming
the clock.

**T-3a.** `Window` is **relative**: two `Span`s from a named anchor, carrying no clock. An analysis
window — "0 to 3000 ms after trial onset" — is a `Window`, not an `Interval`. *Per decision OD-1
(`bead q-interval-clock`); `eyesim` conflates these, expressing every window as absolute onsets.*

**T-4.** Window selection takes a `Window`, an anchor, and an explicit `Overlap` policy
(`OnsetInside`, `FullyContained`, `AnyIntersection`). There is exactly one implementation of window
filtering in the library.

**T-5.** Clock identity is a runtime value (`ClockId`) carried by `Recording`, `Scanpath` and
`Interval`. Conversion between clocks requires a `Sync` value, and every operation over two intervals
checks clock identity.

**T-6.** `Instant` and `Span` are each defined in their **own compilation unit**, so neither is
ever transparent where the other is in scope. They are therefore not mutually assignable anywhere,
including inside the library. *This is stronger than the limitation originally anticipated here,
which assumed both would share a scope and accepted convention in its place. Implemented and tested:
`TimeSuite` asserts both directions with `typeCheckErrors`.*

**T-7.** `Instant` exposes no overloaded `-`. Subtracting a `Span` yields an `Instant`; the span
between two instants is `a.until(b)`. Both operands erase to `Long`, so the overloads would collide
after erasure — and the named form reads better at call sites regardless.

**T-8.** `Timeline[A]` is the first stimulus-side temporal carrier. It is a neutral kernel value,
not an ocular `Event`: it carries one `ClockId` and an ordered collection of instantaneous
`Mark[A]` values. Equal timestamps are allowed because distinct marks may be simultaneous; input
order breaks ties deterministically. `PlannedTimeline[A]` and `ObservedTimeline[A]` are distinct
types. An analysis that claims realized timing requires the observed form and may not silently use
the planned form.

**T-9.** Synchronization evidence is data. `SyncEvidence` contains the fitted `Sync`, fit mode,
number of common-event pairs, residuals, RMS error and maximum absolute error.
`SyncEvidence.fromCommonEvents` fits either an offset-only or affine relation from trigger pairs,
centres timestamps before fitting, and returns a sealed `SyncFitError` for insufficient or
degenerate evidence. Callers do not assemble residual summaries by hand. A one-pair offset fit is
representable but distinguishable from a fitted multi-trigger relation.

**T-10.** `Span` remains signed because subtraction and relative windows require a group.
Quantities that cannot be negative use smart-constructed `NonNegativeSpan`; bin widths use
`PositiveSpan`, and event counts use `NonNegativeLong`. Exposure, dwell, residual magnitudes and
accounting buckets never expose a signed `Span` or `Long` under a non-negativity convention.

### Geometry

**G-1.** `Unit2D` has subtypes `Px`, `Deg`, `Norm`, `Mm`. `Frame[U <: Unit2D]` carries `FrameId`,
`Extent`, and `YAxis`.

**G-2.** Frame *identity* is carried by collections, never by individual points. The *unit*,
however, is a phantom parameter on `Pt[U]`, `Vec2[U]`, `Bounds[U]` and `Extent[U]`. *Strengthened
during implementation: the architecture note left points untagged on the grounds that per-point
frames are slow, which is true of storing a `Frame` reference and false of an erased type parameter.
Since tagging is free at runtime it closes a real hole -- `Bounds[Deg].contains` would otherwise
accept a pixel position. Asserted with `typeCheckErrors` in `GeometrySuite`.*

**G-2a.** `Pt[U]` and `Vec2[U]` form an affine space: the difference of two positions is a
displacement, a position plus a displacement is a position, and two positions cannot be added.
Summing two gaze positions is meaningless and does not compile. Saccade amplitude and direction are
properties of the `Vec2` between two fixations.

**G-2b.** `Bounds[U]` stores an explicit rectangle, not a width and height, because frames in this
domain are not all anchored at zero. A y-axis flip is recorded in `YAxis`, never encoded in the sign
of a bounds argument.

**G-3.** `Warp[A, B]` is a sealed ADT retaining structure, with `render: String`. Every case has a
private constructor; `Then` is reachable only through the checked `andThen`.

**G-4.** `andThen` returns `Either[FrameMismatch, Warp[A, C]]`, checking `f.to.id == g.from.id`.

**G-5.** `Warp` cases include `Id`, `Affine`, `Tangent` (px↔deg, carrying `Viewing` and a `Sense`
so the pairing is genuinely invertible), `Homography`, and `Then`.

**G-6.** `inverse: Option[Warp[B, A]]` returns `Some` for every case that is mathematically
invertible, including `Tangent`.

**G-7.** Conversion from `Px` to `Deg` is available only via a `Viewing` value carrying viewing
distance and physical screen dimensions.

**G-8.** `Moving[A, B]` represents a time-varying warp as an inspectable piecewise structure with an
explicit interpolation mode, supporting dynamic stimuli and surface-mapped mobile recordings.

**G-9.** Every binary operation between two frame-carrying values checks frame identity and returns
`Either[FrameMismatch, _]`. The unit parameter proves both sides are in degrees; it does not prove
they are in the *same* degrees.

**G-10.** Identity checks are performed by **one shared implementation**, `Agreement`, never
re-inlined per call site. `Agreement.clocks` and `Agreement.frames` are the only places two
coordinate systems are compared, with n-ary forms that anchor their error on the first element so a
mismatch in a collection reports stably. *The seam this closes had already formed after a single
file: `Interval` carried a private `sameClock` while `Overlap.selects` re-inlined the same
comparison with the operands reversed. Both were correct; they would not have stayed so. This is
`eyesim`'s copy-pasted-predicate failure in miniature.*

**G-11.** Ergonomic relief from G-9 comes from a **checked container, not a capability**. `Session`
validates frame and clock membership on insertion — returning `Either` there — and its accessors are
total, because membership was proven on the way in. There is no `Scope[U]`, no ambient given, and no
total variant reachable without having passed the insertion check. *Per decision OD-2 (`bead
q-scope`): a capability value is forgeable, since one obtained for frame A can be applied to objects
from frame B.*

**G-11a.** `Session` lives in **`eyes4s-design`**, not in the kernel and not in `eyes4s-core`. *Forced
by the dependency graph rather than by preference: a container worth having holds recordings, AOI
sets and grids together, and `aoi` depends on `core`, so `core` cannot see `AoiSet`. The earliest
module that sees all three is `design`. The kernel keeps only the mechanism (G-9, G-10). Tracked as
bead `x-session`, and it is the project object APP-13's prerequisite query interrogates.*

### Trajectory

**TR-1.** `Gaze[U]` is an ADT with `Tracked(p, pupil)`, `Blink`, `Lost`, `OffScreen(p)`. Missing data
is never a sentinel or a null.

**TR-2.** `Sample[U <: Unit2D]`, `Event[U <: Unit2D]`, `Fixation[U]`, `Saccade[U]`, `Blink[U]`,
`Pursuit[U]`, and `Scanpath[U]` are all unit-parameterized (DOC-3).

**TR-3.** `Recording.of` rejects non-monotone timestamps and empty sample sets, returning
`Either[RecordingError, Recording[U]]`.

**TR-4.** `Rate` is `Fixed(hz)` or `Irregular`. Variable-rate devices are representable.

**TR-4a.** `Recording` is monocular — one `Gaze` per `Sample`. `BinocularRecording[U]` is a separate
type holding paired samples, with `left`, `right` and `cyclopean(Fusion)` projections and a vergence
signal. Detectors consume `Recording`, so the monocular path carries no binocular tax; vergence,
disparity and binocular-coordination analyses demand the paired type explicitly. *Per decision OD-3
(`bead q-binocular`): two independently filtered and resampled recordings no longer share a sample
index, so disparity would be unrecoverable rather than merely absent. The type ships at v0.3 —
before ASC ingest at v0.6 — specifically so that a binocular file cannot silently lose an eye.
Binocular* analyses *are deferred past v1.0.*

**TR-5.** `Scanpath.saccades` has length exactly `n - 1`. No padded sentinel row exists anywhere in
the library.

**TR-6.** `Saccade` carries `peakVelocity: Velocity[U]`; amplitude and direction are derived.

**TR-7.** `Recording.warp` and `Scanpath.warp` map a whole recording or scanpath between frames,
returning `Either[FrameMismatch, _]`.

### Occupancy

**O-1.** `PointMeasure[U]` carries a frame, points, and masses, with `Weight` ∈ {`Uniform`,
`Duration`, `Custom`}. It is constructible from a `Scanpath` or from a `Recording`.

**O-2.** `integrate` is the single primitive of this layer; `massIn(region)` is defined in terms of
it, and the law suite asserts that integrating an indicator equals `massIn`.

**O-3.** `Grid[U]` has nominal identity (`GridId`). Two grids of equal dimensions over different
frames are not interchangeable.

**O-4.** `Surface[U]` has exactly three inhabitants: `Mass` (non-negative, unit sum), `Intensity`
(non-negative, arbitrary scale), `Signed` (any real).

**O-5.** `Intensity.normalised: Either[DegenerateSurface, Mass[U]]` is the **only** route from an
unnormalized estimate to a probability mass. Sum-normalization appears in exactly one place in the
library.

**O-6.** `entropy` is available on `Mass` only. Calling it on `Signed` is a compile error.

**O-7.** `Mass.mean` and `Mass.weightedMean` accept `NonEmptyVector` and return `Mass`. There is no
operator whose name misrepresents its operation; `mean` is named `mean` and log-ratio is named
`logRatio`.

**O-8.** Every `Surface` carries `Provenance` recording smoother, bandwidth, weighting, and
normalization history. No operation may drop it.

**O-9.** `Sigma[U]` is opaque over `Double` and is **always a standard deviation in frame units**.
Any backend with a different convention converts at its own boundary. I-DT dispersion takes an
`Extent`, not a `Sigma`.

**O-10.** `Pyramid[U]` carries its scales in the value. No multi-scale representation depends on an
attribute or a parallel array.

**O-11.** `Region[U]` is a sealed ADT forming a Boolean algebra, with `rasterise` as a lowering to
`Field[U, Boolean]`. `contains` is exact and resolution-independent; `area` takes a `Grid[U]` and is
computed by rasterisation at a stated resolution. Boolean-algebra laws are tested observationally via
`contains` on sampled points. *Per decision OD-5 (`bead q-region-exact`): exact area over arbitrary
Boolean combinations of polygons requires polygon clipping, which is delicate work for a value most
analyses never read, whereas `contains` is cheap, exact, and is what dwell time actually needs.*

**O-12.** The module structure on `Signed[U]` is reached through a grid *value*
(`grid.signedModule`), never summoned as a given, because `zero` depends on the grid.

### AOI assignment and relational attention

**RA-1.** `AoiSet[U, E]` is a greenfield, smart-constructed value carrying a frame and entity-keyed
regions. Frame checking, duplicate entity keys and spatial-overlap behaviour are part of its public
contract; no existing temporal `Overlap` value is reused for spatial membership.

**RA-2.** The primitive time-course result is `EntityTrace[E, Q]`. Roles are never the native
currency of binning. A `Construal[C, E, R]` is a validated relation between entity and role values,
and role traces are pure projections of an entity trace. Reinterpreting one scene under another
construal therefore cannot rerun gaze assignment or alter the entity trace.

**RA-3.** A construal may be many-to-many: one entity may fill several roles and one role may have
several fillers. Projection states an explicit `RoleAttribution` policy (`Replicate`,
`SplitEqually`, or `RequireFunctional`) and returns mass for visible entities assigned to no role as
data. A role trace that duplicates mass does not claim partition accounting.

**RA-4.** Trace source determines the result type. Sample-time and clipped fixation-dwell analyses
return non-negative durations; fixation-onset analyses return non-negative counts. A single
`Map[E, Span]` result may not represent both. Onset-count results carry coverage diagnostics but do
not claim that counts sum to temporal exposure.

**RA-5.** Temporal event selection and contribution are separate decisions. Existing `Overlap`
states whether a fixation is selected. `EventAllocation` states whether its contribution is clipped
to the bin intersection or assigned whole at onset. Only clipped-intersection dwell participates in
an exposure identity; whole-at-onset is an event-attribution estimand.

**RA-6.** Spatial membership is an explicit policy with policy-indexed output. Under exclusive or
priority assignment, entity mass plus background, off-screen, blink, lost and ambiguous mass
partitions the represented exposure. Under multiple assignment, `sum(byEntity)` is not an
accounting identity: the result instead carries visible-union mass, pairwise overlap mass and mass
by overlap multiplicity. Triple and higher-order overlap cannot be inferred from pairwise values
alone.

**RA-7.** Fixation-dwell coverage distinguishes fixation, saccade, pursuit, blink and unsegmented
time. It may not imply that fixation dwell alone exhausts a window. Sample-time coverage states its
sample-support convention, including irregular-rate weighting and any uncovered gaps.

**RA-8.** `EpochPlan` stores a serializable `MarkSelector`, not a captured predicate or a mark
value. Selection names a typed mark key and an occurrence policy (`RequireUnique`, `First`, `Last`,
or `Nth`). Missing and multiply matched anchors are values naming the trial, selector and observed
match count. A `FinalBin` policy states whether a non-divisible window is rejected, includes a
short final bin, or excludes and returns the tail as data.

**RA-9.** `eyes4s-vwp` is a deferred pure consumer of `Timeline`, `AoiSet`, `Trials` and the plan
registry. Its first analyses are realized-preview traces, role/filler dissociation, alternative
construal projection, role-resolution paths and production/onset alignment. It exports descriptive
estimands with complete support and provenance; it does not implement GAMMs, GLMMs, forced
alignment or automatic visual-role inference.

### Comparison

**C-1.** `Compare[-A, -B, +S]` is heterogeneous. `compare` returns `Either[CompareError, S]`.
`CompareError` subsumes `EstimateError`, `GridMismatch`, and `FrameMismatch`.

**C-2.** Every measure declares a `MeasureScale` (`Correlation`, `FisherZ`, `Probability`,
`Bounded`, `DistanceLike`).

**C-3.** `Metric`, `Semimetric`, `Divergence` and `Kernel` are separate interfaces. A measure ships
under the one it satisfies. Specifically: cosine similarity is not a `Metric`; `1/(1 + W₁)` is not a
`Metric`; entropic Sinkhorn does not satisfy `d(x, x) = 0`; MultiMatch's median aggregate does not
satisfy the triangle inequality. Each of these is stated in Scaladoc.

**C-4.** Multi-valued results are product types. `MultiMatchScore` has five named fields.
`ScaleProfile` carries a `NonEmptyMap` keyed by scale. There is no code path that reshapes a named
vector into columns based on a string comparison against a measure name.

**C-5.** `Alignment` is factored out as a shared abstraction with `monotoneLattice`,
`needlemanWunsch`, `dtw`, and `frechet` instances. MultiMatch and ScanMatch are consumers of it.

**C-6.** `monotoneLattice` is an O(nm) dynamic program. The library has no graph-library dependency
for alignment.

**C-7.** `viaSmoothing` lifts `Compare[Mass[U], Mass[U], S]` to `Compare[Scanpath[U], Scanpath[U], S]`,
given a `Smoother` and a `Grid`.

**C-8.** The map-versus-points slot (`Compare[Mass[U], PointMeasure[U], Score]`) exists in v1.0 as an
interface with at least one instance, even though the saliency metric family ships in v1.1.

**C-9.** `SymmetricCompare[A, S]` is the interface for a comparison that is symmetric but need not
satisfy the stronger laws of `Metric`, `Semimetric`, or `Kernel`. Canonical-undirected pair
evaluation requires this interface; it cannot silently choose an orientation for an asymmetric
`Compare`. Every `SymmetricCompare` instance has a symmetry law suite.

### Design and inference

**X-1.** `Trials[K, M, A]` is keyed by a user-supplied type `K` and carries user-supplied metadata
`M` that the library never inspects. Fields used to establish identity, matching, strata, occasions,
or participant scope belong in `K`; outcomes and annotations belong in `M`. No public API takes a
`String` naming a column.

**X-2.** `Relation[L, R]` is a sealed, inspectable ADT with at least `All`, `SameOn`,
`DifferentOn`, and `And`. Its projections are named, typed values. A Boolean `accepts` function is a
derived interpreter, not the representation, and there is no arbitrary-predicate core case. The
structure lets execution use a hash join for `SameOn`, lets diagnostics explain exclusions, and lets
plans persist projection identifiers through the typed extension registry.

**X-3.** `PairDesign` is a sum type whose inhabitants encode only meaningful combinations:
`BetweenDirected(relation, selection)`, `WithinDirected(relation, self, selection)`, and
`WithinUndirected(relation, self)`. `SelfPolicy` therefore exists only for a within-collection
design. Per-focal sampling exists only for a directed design. A canonical-undirected design stores
each edge once and requires `SymmetricCompare`.

**X-4.** `Pairing.matched`, `matchedOn`, `mismatchedWithin`, and `sampled` remain convenient public
constructors, but are façades that compile to `Relation` and `PairDesign`; they do not own separate
execution paths.

**X-5.** `Paired[KL, KR, A, B]` returns eligible pairs plus `unmatchedLeft`, `unmatchedRight`, and
`ambiguous` as data. Ambiguity — a duplicate key where uniqueness is required — is reported, never
silently resolved to the first match.

**X-6.** Pair evaluation first returns `PairwiseAnalysis[KL, KR, E, S]`, whose `PairScore` retains
both keys and an `Either[E, S]`. A generic `evaluatePairs` function maps a total evaluator over
`Paired`; `Compare.compare`, surface decomposition, and temporal sampling can reuse it without a
vacuous universal operation typeclass. `Analysis[K, S]` is a reduced, derived result.

**X-7.** Reduction states its orientation and failure policy. Directed results offer `meanByLeft`
and `meanByRight`. Canonical-undirected results offer `meanEdges`, where each stored edge contributes
once, and `meanByEndpoint`, an explicit mirrored view in which each edge contributes to both
endpoints. Failed scores are never silently discarded: the caller chooses `RequireAll` or
`SuccessfulOnly(minSuccessful)`, and the summary returns eligible, selected, successful, and failed
counts.

**X-8.** Matched and control analyses evaluate different `PairDesign` values through the same
function. There is exactly one baseline sampler and one reduction implementation. `contrast`
combines compatible reduced analyses and requires `Contrastable[S]`, because typed scores do not all
form a group.

**X-9.** `Selection.BottomK(cap, seed, sampleId)` assigns each eligible directed candidate a stable
priority derived from `(seed, sampleId, focalKeyDigest, candidateKeyDigest)`. Eligibility and
selection parameters, including `cap`, do not enter the priority. Increasing the cap therefore
produces a superset; changing row order or an unrelated stratum changes nothing. A distinct
`SampleId` requests an independent priority field. Sampling excludes the true match before taking
the bottom `k`.

**X-10.** `KeyDigest[K]` is the only route from a key to sampling identity. It derives through
`Mirror` for products of supported primitives and opaque identifiers, with domain and position
separators. Neither `hashCode` nor `toString` is used. Its law is
`Eq[K].eqv(a, b) => digest(a) == digest(b)`, and composite golden vectors agree bit-for-bit on JVM
and Scala.js.

**X-11.** Randomness is threaded explicitly through `Seed`; no library function mutates a global
RNG. The library-internal splittable generator and `Seed.derive` produce identical streams on JVM and
Scala.js.

**X-12.** Every pairwise and reduced analysis carries `Provenance`: evaluator, measure scale,
relation, pair space, self/orientation/selection policy, seed and sample identifier where present,
realized counts, grid/smoother where present, and a `ContentHash` of its inputs. *Per decision OD-9
(`bead q-provenance-cache`): a parameter record alone cannot distinguish two datasets analysed
identically, so provenance without input identity was never a valid cache key. `ContentHash` is a
fast non-cryptographic digest over the numeric payload plus geometry, computed once at ingest,
memoized, propagated through derived objects, and identical on JVM and Scala.js per DET-2. It is
cache invalidation, not a security boundary.*

**X-13.** Surface decomposition aligns a response `Mass[U]` with a non-empty, nominally keyed
`PredictorSet[Mass[U]]`. Solver output states the representation it proves: OLS produces a
`Signed[U]` fit, intercept-free NNLS produces an `Intensity[U]` fit, and a simplex-constrained solver
produces a `Mass[U]` fit whose non-negative coefficients sum to one and may therefore be called
mixture weights. Every result carries a `Signed[U]` residual.

**X-14.** Intercept policy is explicit and documented as scientifically consequential for
simplex-constrained grid values. `FitDiagnostics` contains descriptive fit quality — rank,
conditioning, convergence, residual norms, and R² where defined — but no per-cell coefficient
standard errors or p-values, because treating autocorrelated grid cells as independent would make
them invalid. Population inference remains outside this package.

**X-15.** Partial Pearson or Spearman association is a separate `PartialAssociation` operation and
result, never labelled as regression coefficients. A binomial GLM over continuous normalised grid
mass is not an inhabitant.

**X-16.** The convenient API uses distinct scientific verbs — matched similarity, repetition
similarity, surface decomposition, and temporal reinstatement — implemented as thin façades over
pair design, evaluation, reduction, and contrast. There is no `Workflow` class hierarchy and no
umbrella `TemplateAnalysis` engine.

**X-17.** Every within-collection repetition design states participant scope explicitly. Within-
participant repetition and cross-participant consistency are different `PairDesign` values; neither
is the other's default.

---

## Type System Requirements

**TY-1.** Opaque types are used for `Instant`, `Span`, `Hz`, `Sigma[U]`, `Score`, `Distance`,
`Similarity`, and identifier types. Each has a companion providing the instances its use requires.

**TY-2.** No public API returns `null`, uses a sentinel numeric value, or relies on `NaN` propagation
to signal absence.

**TY-3.** No public API is partial. Where a total function cannot be given, the signature returns
`Either` or `Option`.

**TY-4.** Domain ADTs are `sealed` and exhaustive. Adding a case is a binary-compatibility event and
is treated as such.

**TY-5.** Every declared abstraction has at least one instance and at least one law suite or
conformance test in v1.0. A trait with no inhabitant does not ship.

**TY-6.** Where compile-time rejection is claimed in documentation, a negative test using
`scala.compiletime.testing.typeCheckErrors` asserts it. This applies at minimum to: `entropy` on
`Signed`, a `Deg`-parameterized detector applied to `Px` samples, and a `Sigma[Deg]` supplied to a
`Px` grid.

---

## Detection Requirements

**D-1.** `Detector[S, -I, +O]` is a pure state machine with `init`, `step`, and `flush`. It performs
no I/O and allocates no resources.

**D-2.** `Machine[-I, +O]` is an existential wrapper hiding the state type, and is the composable
public form. `Category[Machine]` is provided.

**D-3.** The `Category[Machine]` laws are stated and tested as **observational equality on output
sequences**, not structural equality of state, because composite states are isomorphic rather than
equal.

**D-4.** `Machine.runAll` is available in a pure module with no effect dependency, and always calls
`flush`.

**D-5.** `Machine.toPipe[F[_]]` is available in `eyes4s-fs2`. `runAll` and `toPipe` produce identical
output on finite input; on unbounded input `flush` does not run, and this is documented at the
`toPipe` call site.

**D-6.** v1.0 ships: `Filter.deblink`, `Filter.median`, `Filter.savitzkyGolay`, `Detector.ivt`,
`Detector.idt`, `Detector.engbertKliegl`, `Merge.adjacentFixations`.

**D-7.** `Detector.ivt` consumes `Sample[Deg]` specifically. A pixel recording cannot be supplied to
it without an explicit warp, which cannot be constructed without a `Viewing`.

**D-8.** Deferred to v1.1: `Detector.nystromHolmqvist`, `Detector.i2mc`, HMM-based detection,
smooth-pursuit classification.

**D-9.** Each detector's Scaladoc names the publication it implements and states every deviation
from it.

---

## Surface and Estimation Requirements

**S-1.** `Smoother[U]` declares `bandwidth: Sigma[U]` and returns `Either[EstimateError, Intensity[U]]`.

**S-2.** v1.0 ships `Smoother.gaussian` and `Smoother.anisotropic`. `Smoother.foveal` is deferred to
v1.1.

**S-3.** The Gaussian smoother is implemented as a separable convolution — two one-dimensional
passes — and requires no linear-algebra dependency.

**S-4.** `Bandwidth.silverman2D` and `Bandwidth.scott` are provided as named, explicit choices. No
bandwidth is selected implicitly, and no estimator is selected by probing for an available backend
at runtime.

**S-5.** Estimation failure modes are distinguished in the error ADT: empty input after windowing,
fewer points than a stated minimum, degenerate mass. A single undifferentiated failure value is not
acceptable.

---

## IO Requirements

**IO-1.** v1.0 ingests EyeLink `.asc` (as produced by `edf2asc`) and CSV. Parsing is streaming, via
fs2, and does not require the file to fit in memory.

**IO-2.** ASC ingest recovers: samples with timestamps and gaze position, pupil size where present,
blink and saccade messages where present, recording start/stop, and experimenter messages carrying
stimulus events. Experimenter messages land in an `ObservedTimeline`, preserving their source line
and clock; they are not folded into ocular `Event` and are not attached to `Recording` as an
untyped side channel.

**IO-2a.** Presentation logs may provide a `PlannedTimeline`, an `ObservedTimeline`, or both.
Importers never promote planned timestamps to observed ones. Common triggers fit `SyncEvidence`
through the shared kernel constructor, and the importer reports when evidence is insufficient for
the requested fit mode.

**IO-3.** Ingest is total with respect to malformed input: unparseable lines are reported through a
diagnostics channel with line numbers, never silently dropped and never thrown.

**IO-4.** Metadata decoding from CSV uses `Mirror`-based derivation against a user-supplied case
class. No external dataframe or codec dependency is required for this.

**IO-5.** Results export to CSV and, on the JVM, to Arrow IPC, so that R can read them without a
bespoke bridge.

**IO-6.** Binary `.edf` is out of scope for v1.0 and the limitation is stated in the README, with
`edf2asc` named as the required preprocessing step.

**IO-7.** *v1.1 commitment.* BIDS eye-tracking ingest (TSV plus JSON sidecar), Tobii TSV, and SMI.
This is a commitment, not an aspiration, and is scheduled in the roadmap.

---

## Application-Layer Requirements

A graphical application is a *different kind of consumer* from a library user, and the difference is
not cosmetic. A programmer writes an analysis as a Scala expression; an application must **construct
one from user actions, show it, edit it, save it, re-run it, and explain why it cannot run yet**.
Those five verbs impose requirements that a library designed only for programmers will not satisfy,
and retrofitting them means rewriting the execution layer.

The requirements below are binding on v1.0 even though the application is not. Each is cheap now.

### Analyses are descriptions, not expressions

**APP-1.** Detection pipelines and analyses are representable as an inspectable, sealed description
ADT — a *plan* — that is separate from its execution. `Plan` is a value; running it is an
interpretation of that value.

**APP-2.** `Machine` remains the executable form, but is *derived* from a `DetectPlan` by an
interpreter. A hand-composed `Machine` stays available to programmers; the plan is what the
application manipulates.

**APP-3.** Every plan node carries enough information to render itself for display: a name, its
parameters with their units, and a one-line description.

**APP-4.** Plans support structural equality and diffing, so an application can show what changed
between two runs and support undo.

**APP-4a.** The plan vocabulary is a fixed core — detection, estimation, AOI definition, comparison,
pairing — plus a **typed extension registry**. `NodeDef[P]` carries a `NodeId` and interpreter
`P => Op`; persistence registration associates the conditional `VersionedCodec[P]` required by
APP-8a without introducing a plan/codec dependency cycle. Registration is compile-time typed; only
*lookup* is by identifier, and a miss returns an explicit error naming the node and its version.
*Per decision OD-8 (`bead
q-plan-coverage`): covering the whole API means maintaining two parallel libraries by hand, while a
core-only vocabulary leaves a method developer's new measure invisible to the application until the
library changes. The registry gets both — but only while its entries stay typed. OD-13 refines the
original unqualified `Codec[P]` carrier into a conditional, versioned generic contract.*

**APP-4b.** Plan node parameters are typed domain values. `Map[String, String]` parameter bags are
prohibited. An operation the plan cannot express is a missing node definition, not grounds for a
dynamic one.

This is the highest-leverage decision in this document and it has direct house precedent: `frame4s`
makes a query a pure logical plan with execution as a separate effectful step, and `linop4s` makes
operator composition an inspectable expression tree rather than opaque closure composition. `eyes4s`
applies the same discipline one level up, to the analysis itself.

### Everything round-trips

**APP-5.** Every domain value an application must persist has a codec: `Frame`, `Warp`, `Viewing`,
`Grid`, `Region`, `AoiSet`, `Timeline`, `SyncEvidence`, `Sigma`, `Smoother`, all plan types,
`Provenance`, and `Analysis`.

**APP-6.** Codecs are JSON, in a dedicated module, and are versioned with an explicit schema version
so that a project file saved by one release opens in the next.

**APP-7.** Round-tripping is a law: `decode(encode(x)) == x` for every codec, tested via ScalaCheck
generators already required by V-5.

**APP-8.** Surfaces and recordings — the large numeric payloads — are excluded from JSON and
persisted separately, referenced by content hash.

**APP-8a.** Generic domain values use conditional, versioned codecs. A codec for
`Timeline[A]`, `Trials[K, M, A]`, `Construal[C, E, R]`, or a generic plan parameter exists only when
the corresponding user types provide `VersionedCodec` instances. Registration of a typed plan
extension captures those instances; runtime loading reports a missing schema identifier or version
as a value. The generic mechanism and its module dependency direction are settled before v0.65
implementation; it is a blocking prerequisite, not an assumption hidden in `NodeDef[P]`.

**APP-8b.** Generic maps encode as arrays of key/value entries rather than JSON object fields, so
keys do not acquire an accidental string representation. Decoding rejects duplicate keys. Large
generic payloads are represented by typed artifact references carrying content hashes rather than
being inlined into project JSON.

### Execution is observable and interruptible

**APP-9.** No long-running operation is a single opaque call. Estimation over many trials, detection
over long recordings, and pairwise comparison over many pairs are expressed as streams or as
chunked, resumable steps.

**APP-10.** Progress is reportable: an execution surface in `eyes4s-fs2` emits progress events
carrying completed and total counts.

**APP-11.** Cancellation is supported through Cats Effect at that boundary, and leaves no partial
state in any returned value.

**APP-12.** `Provenance` is complete enough to serve as a cache key: two runs with equal provenance
must produce equal results. This is what lets an application recompute only what changed when a
parameter is tweaked, and is the natural join point with the `repro4s` design.

### The type system is a user-interface affordance

**APP-13.** Prerequisites are queryable. Given the current state of a project, an application can ask
which analyses are available and, for each unavailable one, *what is missing*. Concretely: without a
`Viewing`, degree-based measures are unavailable; without an `AoiSet`, reading measures are
unavailable; and without an observed anchor or adequate `SyncEvidence`, an event-aligned analysis is
unavailable. Missing and ambiguous mark selections name the trial and selector.

**APP-14.** Errors name the object that failed, not only the reason. Every error carries the trial
key, and where applicable the source file and line, so an application can navigate to it.

**APP-15.** Every measure carries a `MeasureInfo`: display name, one-line description, scale, metric
properties, parameter list with units, and a literature citation. An application must be able to
populate a method-selection UI and a methods section without hardcoding a table.

**APP-16.** Every detector and smoother carries the equivalent metadata, including the publication
it implements and its documented deviations (D-9).

APP-13 is the product argument for the type discipline, restated for a non-programmer audience.
Parse-don't-validate means the set of currently-valid analyses is computable at any moment — so the
application can present a live capability list with actionable reasons rather than failing at run
time with a stack trace. The types stop being an implementation detail and become the feature that
tells a psychologist what to do next.

### Rendering

**APP-17.** `eyes4s-viz` plot specifications are themselves serializable descriptions, consistent
with APP-1.

**APP-18.** The core exposes render-ready projections without requiring a plotting dependency:
surfaces as row-major numeric buffers with their extent, scanpaths as polylines with per-vertex
timing, regions as path geometry.

**APP-19.** Because pure modules cross-compile to Scala.js, an application may run detection,
estimation and rendering client-side against the identical law-tested code. This is the operative
reason JS is a v1.0 platform and Native is deferred.

### What this does not require

The application is a separate repository and a separate product. `eyes4s` gains **no** UI framework
dependency, no window toolkit, no server, and no persistence engine. Non-Goal 3 stands: the library
emits specifications and projections; it does not draw. The requirements above are all internal
shape, not surface area.

---

## Error Model

Mirrors the discipline established in `frame4s` and `linop4s`.

**E-1.** Recoverable and validation failures return `Either[E, A]` with a sealed `enum` error type.
Every error enum provides `def message: String`.

**E-2.** Accumulating validation — ingest diagnostics, multi-row failures — uses `ValidatedNec`.

**E-3.** Exceptions are reserved for defects and are raised only at construction, never inside an
estimation, comparison, or detection loop. `FrameMismatch` and `GridMismatch` raised as defects
carry a multi-line explanatory message naming both operands.

**E-4.** No pure public API throws.

**E-5.** No function reports a problem solely through a logging or warning side channel. Anything a
caller might need to act on is in the return value.

**E-6.** No function silently changes the cardinality of its input. Dropped rows are returned.

---

## Determinism Requirements

**DET-1.** Every result is a pure function of its inputs and an explicit `Seed`.

**DET-2.** The RNG produces identical streams on JVM and Scala.js. `scala.util.Random` is not used
in the domain layer.

**DET-3.** Fold and stratum assignment is a deterministic function of the key, not of a shuffled
global draw.

**DET-4.** Directed bottom-k sampling is a deterministic function of `Seed`, `SampleId`, focal key,
and candidate key. Priorities are independent of row order, eligibility relation, and cap; increasing
the cap preserves the smaller sample as a subset.

**DET-5.** Reductions over floating-point values sum in a defined key order so that results are
bit-reproducible across platforms, following `gale`'s precedent.

**DET-6.** Parallelism, where introduced, does not alter results. v1.0 ships no implicit parallelism;
any parallel execution is an explicit caller choice.

---

## Cross-Platform Build Requirements

**B-1.** Scala 3.3.8 LTS, pinned as `val Scala3` at the top of `build.sbt`. `crossScalaVersions :=
Seq(Scala3)`.

**B-2.** sbt 1.11.7, sbt-typelevel 0.8.7, `tlCrossRootProject`, `tlBaseVersion := "0.1"`,
`tlJdkRelease := Some(11)`.

**B-3.** `crossProject(JVMPlatform, JSPlatform)` with `CrossType.Pure` for all pure modules.

**B-4.** Scala Native is deferred, but every dependency must remain Native-eligible so that adding
the axis post-1.0 is a build change and not a redesign. This is the operative reason the library
defines its own module structure rather than depending on Spire.

**B-5.** Apache-2.0, `startYear := Some(2026)`, `tlGitHubDev`, organization `io.github.canardlapin`.

**B-6.** `.scalafmt.conf` copied from `linop4s`: scalafmt 3.11.4, `runner.dialect = scala3`,
`maxColumn = 96`, `align.preset = more`, `rewrite.scala3.convertToNewSyntax = false`,
`removeOptionalBraces = false`.

**B-7.** `addCommandAlias("compileAll", …)` and `("testAll", …)` generated by flat-mapping module ×
platform, following house convention.

**B-8.** CI is sbt-typelevel-generated and not hand-edited. Java 17 and 21 on temurin.

**B-9.** `checkModuleBoundaries` runs in CI and fails the build on violation of PKG-1 or PKG-3.

---

## Documentation Requirements

Governed by the "published OSS library" audience decision.

**DOC-R1.** `README.md` states in its first screen: what the library is, the one-sentence thesis, a
runnable example, the v1.0 scope boundary, and the `edf2asc` limitation.

**DOC-R2.** `eyes4s.md` is maintained as the architecture specification and kept in sync with the
code. Divergence between the two is a bug.

**DOC-R3.** A documentation site built with mdoc, so every example in the documentation is compiled
and executed by CI. Note that no sibling library has yet built one; `eyes4s` establishes it.

**DOC-R4.** `PARITY.md` lists every measure compared against `eyesim`, the agreement achieved, and
for each divergence its cause and which implementation is correct.

**DOC-R5.** Every public type's Scaladoc states its invariants. Every measure's Scaladoc states its
scale, its metric properties, and its parameters' units.

**DOC-R6.** Scaladoc records *rationale* for non-obvious design decisions, following house style,
including why an alternative was rejected.

**DOC-R7.** `AGENTS.md` modelled on `frame4s`'s: layout, build and test, design contract.
`CONTRIBUTING.md` with pre-PR rules and DCO sign-off. `CODE_OF_CONDUCT.md`, `SECURITY.md`, `LICENSE`.

**DOC-R8.** A migration guide for `eyesim` users mapping each `eyesim` entry point to its `eyes4s`
equivalent and naming the behavioural differences.

---

## Verification Requirements

**V-1. Law suites.** `eyes4s-laws` publishes Discipline rule sets for, at minimum:
- `Warp` composition: associativity and identity on the matching-frame subcategory, and refusal
  off it;
- `Warp` round-trip: `px → deg → px` within a stated tolerance;
- `Region`: Boolean-algebra laws;
- `Signed[U]`: module laws per grid;
- `Mass`: non-negativity and unit sum preserved by every operation that returns a `Mass`;
- `Metric` / `Semimetric` / `Divergence` / `Kernel`: the axioms each interface claims, per instance;
- `SymmetricCompare`: symmetry per instance;
- `KeyDigest`: `Eq`-consistent digests for derived and hand-written instances;
- `Machine`: `Category` laws as observational equality;
- `integrate`: integrating an indicator equals `massIn`.

**V-2. Tolerances are explicit.** Every numerical law states its tolerance as a named value. No
hidden epsilon.

**V-3. Reference conformance.** Where a published reference implementation exists, agreement with it
is a test:
- MultiMatch against the Python `multimatch_gaze` package with `grouping = FALSE`;
- `monotoneLattice` against a brute-force shortest path on small lattices;
- detection against published fixtures where available.

**V-4. R-parity, advisory.** A fixture harness modelled on `fmrihrf`'s `tools/r-parity/` generates
inputs and `eyesim` outputs from R, and CI reports per-measure agreement on `wynn_study` /
`wynn_test`. **Parity failures do not block release.** Each divergence is recorded in `PARITY.md`
with its cause and a statement of which implementation is correct. The known divergences to expect,
from the Evidence Base, are: kernel bandwidth (`ks` versus `MASS` semantics), join behaviour
(first-match versus keyed), `Ops` semantics (`+` as mean, `/` as log-ratio), permutation baseline
construction, the treatment of signed maps as probability masses, and repetitive similarity. The
repetitive fixture is a **divergence fixture**: it proves that the eyesim vignette places the intended
same-participant, same-image, different-phase pair in `othersim`, and that eyes4s selects it only
under the explicit target relation.

**V-5. Property tests.** ScalaCheck generators for `Frame`, `Warp`, `Scanpath`, `PointMeasure`,
`Grid`, `Region` and `Mass` are published in `eyes4s-laws` for downstream use.

**V-6. Negative type tests.** As specified in TY-6.

**V-7. Cross-platform equivalence.** A test asserts bit-identical output for a representative
pipeline on JVM and Scala.js, including `KeyDigest` golden vectors and directed bottom-k samples.

**V-8. Coverage.** Every public entry point is exercised. The `eyesim` evidence includes a file with
zero test coverage containing a live type error; this is the specific failure mode being guarded
against.

**V-9. Relationship conformance.** Truth-table tests cover every `Relation` constructor and legal
`PairDesign` inhabitant. They distinguish canonical edge storage, mirrored endpoint reduction,
directed left/right reduction, self exclusion, uniqueness diagnostics, and participant scope. A
sampling mutant that includes the true match, derives priority from cap, shares a stream across
focals, or depends on row order must fail.

**V-10. Timeline and relational-attention conformance.** Timeline fits recover synthetic offset and
drift with reported residuals; shifting both clocks together leaves relative epochs unchanged.
Exclusive spatial assignment satisfies its partition identity. Multiple assignment satisfies the
visible-union identity and reports overlap multiplicity. Reprojecting one entity trace through a
different construal leaves the entity trace bit-identical. Mutation tests kill denominator changes,
role-first binning, boundary double-counting, blink/loss conflation and silent final-bin truncation.
These rules publish with the first module that declares the corresponding abstraction.

---

## Performance Requirements

Performance is a v1.0 concern only to the extent that it must not preclude realistic use.

**P-1.** Ingest and detection are streaming and constant-memory in the number of samples. A
one-hour monocular recording at 1000 Hz must process without loading the file into memory.

**P-2.** Gaussian smoothing onto an n×n grid is O(n²k) via separable convolution, not O(n²m) against
m points.

**P-3.** Alignment is O(nm) in time. Quadratic memory is acceptable at v1.0; a banded variant is
deferred.

**P-4.** No benchmark suite is required for v1.0. A JMH module is deferred to post-1.0, following
`gale`'s staging. Complexity claims above are asserted by construction and reviewed, not measured.

**P-5.** Where an allocation-conscious inner loop is written with `while` and mutable arrays, the
mutation is unobservable through the public API.

**P-6.** `SameOn` enumeration uses a keyed join rather than testing the full cross product.
Conjunctions use equality nodes to restrict candidate groups before residual relations are tested.
Canonical-undirected evaluation computes each symmetric edge once; mirroring is a result view, not a
second evaluator call.

---

## Release Roadmap

### v0.1 — Kernel geometry

`eyes4s-kernel` geometry and time. `Unit2D`, `Frame`, `Warp`, `Moving`, `Instant`, `Span`,
`Interval` (absolute, clock-carrying), `Window` (relative), `Overlap`, `ClockId`, `Sync`. `eyes4s-laws` with warp category and round-trip suites.
Build, CI, boundary enforcement, scalafmt, `AGENTS.md`.

*Exit:* V-1 warp suites green on JVM and JS; `checkModuleBoundaries` green; `eyes4s-kernel` compiles
with `eyes4s-core` absent from the classpath.

### v0.2 — Occupancy

`PointMeasure`, `Grid`, `Surface` (`Mass` / `Intensity` / `Signed`), `Field`, `Region`, `Provenance`,
`ContentHash`, the module structure. Law suites for regions, surfaces, and `integrate`.

*Exit:* V-1 occupancy suites green; TY-6 negative test for `entropy` on `Signed`.

### v0.3 — Trajectory and detection

`eyes4s-core` and `eyes4s-detect`. `Gaze`, `Sample`, `Recording`, `BinocularRecording`, `Event`, `Scanpath`, `Viewing`;
`Detector`, `Machine`, and the D-6 detector set. `eyes4s-fs2` with `toPipe`.

*Exit:* D-5 equivalence property test green; V-3 detection conformance; TY-6 negative test for a
`Deg` detector against `Px` samples.

### v0.4 — Surface estimation and comparison

`eyes4s-surface` and `eyes4s-compare`. Smoothers, bandwidth selection, `Pyramid`; the `Compare`
hierarchy, `Alignment`, MultiMatch, ScanMatch, the distribution-measure family, Sinkhorn and sliced
OT. CRQA is **not** in v1.0 (OD-6).

*Exit:* V-1 metric-axiom suites green per instance; V-3 MultiMatch agreement with `multimatch_gaze`.

### v0.5 — Design and inference

`eyes4s-design`. `Trials[K, M, A]`; sealed `Relation`; legal `PairDesign` inhabitants; `Pairing`
façades; `Paired`; primary `PairwiseAnalysis`; explicit reductions and contrasts; `KeyDigest`,
`SampleId`, and the seeded RNG; matched/repetition/temporal conveniences; surface decomposition and
partial association. The arity and persistence responsibilities of every public generic type are
finalized here; the conditional `VersionedCodec` mechanism is recorded as a prerequisite for v0.65,
not retrofitted after plan parameters exist.

*Exit:* relation truth tables and sampling mutation tests are green; cap-monotone bottom-k samples
and derived-key digests agree on JVM and Scala.js; V-4 parity and repetitive-divergence fixtures run
in CI; `PARITY.md` names every expected disagreement; the generic-codec decision is closed.

### v0.6 — IO and AOI

`eyes4s-io` with ASC and CSV; `eyes4s-aoi` with regions, dwell, entry, run counts, and transition
matrices. The neutral kernel timeline, planned/observed distinction and fitted `SyncEvidence` land
before ASC experimenter-message recovery. `AoiSet` defines spatial membership and overlap outputs
rather than inheriting an unstated policy.

*Exit:* IO-3 diagnostics on a corrupted fixture; a real `edf2asc` output parsed end to end with
experimenter messages preserved in an observed timeline; a synthetic multi-trigger fit recovers
known offset and drift; exclusive and multiple AOI accounting laws are green.

### v0.65 — Plans and codecs

`eyes4s-plan` and `eyes4s-codec`. Detection and analysis plans as description ADTs with
interpreters, plus the typed extension registry; conditional `VersionedCodec` instances for generic
user types; JSON codecs with a versioned schema; serializable mark selectors and epoch/final-bin
policies; `MeasureInfo` and detector/smoother metadata; prerequisite queries; progress and
cancellation on the fs2 execution surface.

*Exit:* APP-7 round-trip law green for every codec; APP-13 prerequisite query returns actionable
reasons for a project lacking a `Viewing`, observed anchor, or adequate synchronization evidence; a
plan containing user-typed keys and markers is constructed programmatically, serialised, reloaded
and produces output identical to the unserialised run.

### v0.7 — Documentation and hardening

mdoc site, migration guide, `PARITY.md` completed, Scaladoc coverage, `CONTRIBUTING.md`, negative
tests, cross-platform equivalence test.

*Exit:* every documented example compiles and runs in CI.

### v1.0 — Stability

No new features. Binary-compatibility policy published; MiMa configured; artifacts published to
Maven Central for JVM and Scala.js.

*Exit:* Acceptance Criteria below, in full.

### v1.1 — The first expansion beyond `eyesim`

Reading measures; the saliency metric family; BIDS eye-tracking ingest, Tobii TSV, SMI;
`Detector.nystromHolmqvist` and `Detector.i2mc`; `Smoother.foveal`; **CRQA implemented properly**
(recurrence matrix, embedding, delay, radius selection, RR/DET/LAM/ENTR/TT/L_max — per OD-6), which
also unlocks dual eye tracking and joint-attention work. The first `eyes4s-vwp` release consumes the
v1.0 timeline/AOI/plan foundations to provide entity-primary exposure and onset traces, pure
construal projection, realized-preview analyses and export-ready support diagnostics. **This is the
release that delivers the second clause of the product thesis** and is committed, not aspirational.

### v1.2 and beyond

Statistical mapping with cluster-based permutation inference; `eyes4s-gale` adapters (PCA, CORAL,
CCA); `eyes4s-viz`; `eyes4s-graph4s`; `eyes4s-frame4s`; pupillometry with `fmrihrf` bases; Scala
Native axis; exact network-simplex EMD; RSA for gaze; HMM scanpath models; dynamic visual-world
entity geometry and richer production annotations; JMH benchmarks.

---

## Acceptance Criteria

v1.0 ships when all of the following hold.

1. **A-1.** `compileAll` and `testAll` green on JVM and Scala.js, Java 17 and 21.
2. **A-2.** `checkModuleBoundaries` green: `eyes4s-kernel` does not depend on `eyes4s-core`, and no
   pure module depends on `cats-effect` or `co.fs2`.
3. **A-3.** `eyes4s-kernel` contains no identifier naming an ocular concept, verified by review
   against a documented word list.
4. **A-4.** Every law suite in V-1 green, with explicit named tolerances.
5. **A-5.** Every claim of compile-time rejection in the documentation has a corresponding
   `typeCheckErrors` test (TY-6).
6. **A-6.** MultiMatch agrees with `multimatch_gaze` at `grouping = FALSE` within a stated
   tolerance on the fixture set.
7. **A-7.** `runAll` and `toPipe` produce identical output on finite input, as a property test.
8. **A-8.** The RNG produces identical streams on JVM and Scala.js, as a test.
9. **A-9.** A representative end-to-end pipeline produces bit-identical output on JVM and Scala.js.
10. **A-10.** The R-parity harness runs in CI and `PARITY.md` documents every divergence with its
    cause and a statement of which implementation is correct. *No parity threshold gates the
    release.*
11. **A-11.** A real `edf2asc` output is ingested end to end and produces a `Scanpath`.
12. **A-12.** Every documented example compiles and executes in CI via mdoc.
13. **A-13.** Every public type documents its invariants; every measure documents its scale, its
    metric properties, and its parameters' units.
14. **A-14.** No public API returns `null`, throws from a pure path, or silently changes input
    cardinality — verified by review against E-4, E-5, E-6, TY-2.
15. **A-15.** Every declared abstraction has at least one instance and at least one law suite or
    conformance test (TY-5).
16. **A-16.** MiMa configured; binary-compatibility policy published; artifacts on Maven Central for
    both platforms.
17. **A-17.** `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `CODE_OF_CONDUCT.md`, `SECURITY.md`,
    `LICENSE`, `PARITY.md` and the migration guide all present and current.
18. **A-18.** APP-7 round-trip law green for every codec, with a versioned schema.
19. **A-19.** A plan built programmatically, serialised to JSON, reloaded, and executed produces
    output identical to the unserialised run.
20. **A-20.** APP-13 prerequisite query implemented: for a project lacking a `Viewing`, the
    degree-based measures are reported unavailable with the missing prerequisite named.
21. **A-21.** Every measure, detector and smoother carries complete `MeasureInfo`-equivalent
    metadata including citation (APP-15, APP-16).
22. **A-22.** ASC experimenter messages are preserved in an `ObservedTimeline`; planned timestamps
    cannot satisfy an observed-timing prerequisite.
23. **A-23.** Conditional codecs round-trip a representative `Trials[K, M, A]` and timeline with
    user-supplied key, metadata and marker types; duplicate generic keys and missing schema versions
    are rejected explicitly.

---

## Resolved Decisions

The original ten open decisions were resolved on 2026-07-24. Three further decisions governing
stimulus timelines and relational attention were resolved on 2026-07-25. Each is recorded as a
closed bead in the `mote` store at `.mote/`, carrying a `decision`-kind note with its full rationale.
Three of the original ten were resolved to an option **not offered in the original framing**, which
is noted below.

| # | Bead | Resolution |
|---|---|---|
| OD-1 | `q-interval-clock` | **Split the type rather than tag it** *(third option)*. `Interval` is absolute, carries a `ClockId`, and is produced by events. `Window` is relative — two `Span`s from a named anchor — and carries no clock because it semantically has none. An analysis window was never an absolute interval; conflating the two is its own bug class. |
| OD-2 | `q-scope` | **No `Scope` capability** *(third option)*. Kernel binary operations stay `Either`-only. Ergonomics come from a checked *container*: `Session` validates frame membership on insertion and its accessors are total, because membership was proven on the way in. A capability is forgeable; a container's contents were validated on entry. Doubles as the application's project object (APP-13). |
| OD-3 | `q-binocular` | **Separate `BinocularRecording`** *(third option)*. `Recording` stays monocular so the common path is simple; `BinocularRecording[U]` holds paired samples with `left` / `right` / `cyclopean` projections and a vergence signal. Two independently filtered recordings no longer share a sample index, so disparity would be unrecoverable. Type lands v0.3 so v0.6 ingest cannot silently drop an eye; binocular *analyses* deferred past v1.0. |
| OD-4 | `q-sigma-units` | **Frame units**, with `Sigma.deg` as the documented default. Forcing degrees would require a `Viewing` for every KDE, making the library unusable on data without viewing geometry. |
| OD-5 | `q-region-exact` | **Exact ADT**; `contains` exact and resolution-independent, `area(g: Grid[U])` computed by rasterisation at a stated resolution. Exact area over arbitrary Boolean combinations of polygons needs polygon clipping — real, delicate work for a value most analyses never read. Laws tested observationally via `contains`. |
| OD-6 | `q-crqa` | **Implement properly, in v1.1.** Not on the thesis path, and larger than it appears. Moved off the v0.4 critical path; v1.0 README states the absence. |
| OD-7 | `q-laws-publication` | **One `eyes4s-laws` module** for v1.0; revisit post-1.0 only on real demand. If ever split, the seam is `kernel-laws` versus the rest, aligning with the `trace4s` extraction boundary. |
| OD-8 | `q-plan-coverage` | **Core vocabulary plus a typed extension registry.** `NodeDef[P]` carries a `NodeId` and typed interpreter; registration is compile-time typed and only *lookup* is by identifier. Holds the Product Warning rule: parameters are typed domain values, never `Map[String, String]`. The original unqualified codec carrier is refined by OD-13. |
| OD-9 | `q-provenance-cache` | **`Provenance` carries a `ContentHash` of its inputs.** Settled by inspection: parameters alone cannot distinguish two datasets analysed identically, so provenance was never a valid cache key. Fast non-cryptographic digest, computed once at ingest, identical on JVM and JS. The join point with `repro4s`. |
| OD-10 | `q-app-target` | **Local JVM process serving a browser UI** — the RStudio Server / Jupyter pattern. Decided by data governance more than technology: gaze files are large and are human-subjects data, so a hosted app means uploading participant data off-site. **Consequence: Scala Native remains unnecessary post-1.0, and the Scala.js investment is confirmed load-bearing.** |
| OD-11 | `bd-01KYDZ53W05NZRFXH38VMMP44B` | **Neutral planned/observed timelines with fitted synchronization evidence.** `Timeline[A]` belongs in the kernel; planned and observed forms are distinct; stimulus messages never become ocular events. `SyncEvidence.fromCommonEvents` owns fitting and its residual diagnostics. |
| OD-12 | `bd-01KYDZ5F929YKS1TMPCK2NVYYV` | **Entity-primary, estimand-indexed relational-attention traces.** Roles are pure construal projections. Duration and onset-count results are distinct. Temporal selection, event contribution and spatial membership are separately named, with accounting laws specific to each policy. |
| OD-13 | `bd-01KYDZ5VDVS0E6SSKQJ8QH5EWJ` | **Conditional versioned codecs for generic domain values.** User-supplied type parameters require registered schemas; generic keys are encoded as entries rather than JSON field names; duplicate keys and missing schema versions are explicit failures. The acyclic plan/codec seam is a blocking v0.5 design task. |

Requirements affected by these resolutions have been updated in place. Their implementation is
dependency-ordered under the v0.6/v0.65 milestones and the deferred visual-world epic.

---

## Product Warning

Three risks are worth stating plainly.

**The unit guarantee is easy to get wrong, and was got wrong once already.** An earlier draft of
`eyes4s.md` typed the I-VT threshold as `Velocity[Deg]` while leaving `Sample` carrying a bare `Pt`,
so the compiler checked the literal and not the data. The guarantee only holds if the unit parameter
is carried on `Sample`, `Event` and `Scanpath` (DOC-3, TR-2), and TY-6 exists to keep it honest.

**Frame identity is checked at runtime, not compile time, by deliberate choice.** `U` proves both
operands are in degrees; it does not prove they are in the *same* degrees. G-9 requires the runtime
check on every binary operation. OD-2 resolved this toward a checked container (`Session`) rather
than a forgeable capability, so the check is not opt-out — but it remains a runtime check, and
`Session`'s insertion path is now the load-bearing place where correctness is established. Review it
accordingly.

**Advisory parity is a real trade.** Choosing not to gate on `eyesim` agreement is correct — several
of its behaviours are defects and gating would encode them as requirements — but it means `eyes4s`
cannot promise that a migrated analysis reproduces a previously published number. `PARITY.md` and
the migration guide are the mitigation, and they must be honest about which differences will change
results.

**The plan layer is the one place where the application can corrupt the library.** A description ADT
that must express everything the GUI can do will, under pressure, grow stringly-typed parameter bags
and escape hatches — the exact failure mode this document's Evidence Base catalogues in `eyesim`.
OD-8 exists to bound it. The rule to hold: a plan node's parameters are typed domain values, never a
`Map[String, String]`, and an operation the plan cannot express is a missing plan node rather than a
reason to add a dynamic one.

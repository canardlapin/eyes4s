# Scanpath and fixation comparison

The R fixtures in [scanpath.json](../tools/r-parity/fixtures/scanpath.json) measure exported
`scanpath`, `multi_match`, `similarity`, `scanpath_similarity`, `fixation_overlap` and
`fixation_similarity` at the frozen revision. The existing Python `multimatch_gaze` conformance
suite remains separate evidence. Only `igraph` 2.3.1 was added to the R dependency lock; every
existing package record and the selected T4transport backend are unchanged.

## Five native scores; the sixth has no slot

`ScanpathComparison.baseline` is `MultiMatch` over two `BaselineScanpath`s and returns its
five-field `MultiMatchScore`: vector shape, direction, length, position and duration. eyesim's
baseline requires three fixations per operand; `BaselineScanpath.of(path, operand)` states that
cardinality once and refuses a shorter path as `CompareError.TooShort` naming the operand, its
fixation count and the minimum (`BaselineScanpath.MinimumFixations`), so a short path never
reaches the comparison. `MultiMatch` itself still admits two-fixation paths. eyesim's sixth,
transport-backed position component has no native exact-EMD solver (the decision on
`eyesim-compare`); rather than a public slot that is always unavailable, there is no slot, and it
is never replaced by an unrelated approximation.

On the non-square 800×400 frame, translated, duration-changed, geometry-changed, straight and
stationary paths agree with the pinned five R components at `AnalyticTolerance = 1e-12`.
Translation by (30,40) changes position to 1−50/√800000 and leaves the other four components
unchanged. Paired duration ratios 25/50 and 50/100 give duration similarity 1/2. Shape normalizes
by twice the screen diagonal; position and length by the diagonal; direction by π. Tied
straight-path self-alignment is measured separately. The fixture retains the sixth component,
including a duration-reweighted case whose pinned backend returns one; that is a measured
backend outcome, not an exact transport oracle, and it has no native counterpart.

`Scanpath.within(window, anchor, Overlap.OnsetInside)` expresses the reference half-open onset
window. A final onset at 200 is excluded by [0,200), admitted by [0,201), and an empty selection
is a typed failure. R returns six missing components for short paths, a scalar missing result
for an empty filtered path, and errors for duplicate onsets or zero-duration transport. Native
checked paths/fixations refuse the invalid construction, and a short path is refused at
`BaselineScanpath` admission. Frame geometry supplies native normalization; R requires a separate screensize.

## Fixation overlap

`FixationOverlap.compare` composes the shared `FixationTrajectory` and takes an explicit query
clock, time vector, typed threshold, Euclidean/Manhattan distance, endpoint policy and missing
policy. It returns every requested query with distance or a located failure and a strict
`distance < threshold` hit. Exact equality is a non-hit. Queries remain in caller order,
including repeats and missing support; empty queries are an error.

`MissingOverlapPolicy.CountAsNonOverlap` matches the reference denominator: all requested
queries, including unsupported ones. `RequireComplete` retains the same rows but refuses the
summary when support is incomplete. `requested`, `contributing` and `overlaps` are distinct.
Nonfinite distance arithmetic is also a located failure, not an infinity score.

The pinned direct R entry point defaults to threshold 60 and
`seq(0, max(c(x$onset, y$onset)), by = 20)`, spanning both paths' onsets, and its
fixation-group similarity facade now shares the threshold 60 and requires `time_samples`.
Both hold each path's final fixation after its onset. For the translated fixture, the direct
default is one, the facade with explicit queries is 5/6, and the longer-path grid example is
11/21 whichever path comes first. The native API requires explicit parameters; it does not infer
a grid or an untyped time unit. With `TrajectoryEndpoint.HoldLastOnset` the six explicit
millisecond queries contain five supported times; when the threshold exceeds Euclidean distance
50 (or Manhattan distance 70), the result is 5/6. At equality it is zero. `OnsetRange` instead
leaves the query after the last onset unsupported.

## Spatiotemporal entropic transport

`FixationTransportConfig.of[U]` checks positive x/y scales in U, positive exact `Span` time scale,
nonnegative time weight, positive lambda, iteration budget, marginal tolerance, cost-matrix
limit and total work limit. The cost-matrix limit bounds one iteration; `maximumWork` bounds
leftCount × rightCount × iterations for the whole call (default
`FixationTransportConfig.DefaultMaximumWork` = 5e7, roughly half a second on a laptop JVM), because
the call is synchronous and cannot be cancelled. Exhausting it before convergence is a
`WorkLimit` error naming both counts, the completed iterations, the residual and the limit, not
a `NonConvergence`. `FixationTransport.compare` checks frames and clocks before forming a
rectangular cost matrix. Duration-derived weights use exact endpoint differences before conversion and scaling.
Onset differences are subtracted as integers before floating conversion, preserving a one
microsecond separation even beyond double's exact absolute-integer range.

For fixation i,j the dimensionless squared ground cost is

```
Cij = ((xi-xj)/xScale)^2 + ((yi-yj)/yScale)^2
    + (timeWeight * (onset_i-onset_j)/timeScale)^2
```

The log-domain solver alternates marginal scaling of `exp(-C/lambda)` and admits a result only
when both marginal residuals meet the configured tolerance. `squaredCost` is the expected C
under the fitted plan; `rootCost` is its square root, corresponding to T4transport's default
p=2; `similarity` is 1/(1+rootCost). The result carries iterations, residual and both input
counts. It is not a debiased Sinkhorn divergence, a metric, or a proof of finite-budget symmetry.
Self-cost for two equally weighted positions one unit apart at lambda 1/2 is
1/(1+exp(2)), so self-similarity is less than one. A singleton scaled offset (.3,.4,1) has squared
cost 1.25, providing a solver-independent oracle. Unequal duration masses have a separate
analytic two-by-two marginal equation.

The default native marginal tolerance is 1e-10; conformance uses 1e-13 and compares against an
independent primal solver at 1e-12. The reference comparison gate is fixed at 1e-7. At lambda
0.01, pinned T4transport's default 496 iterations give similarity 0.948732031806375 versus the
independent converged 0.9487302525140882, **failing that gate**. Its marginal residual is about
8.94e-6. Increasing only the reference iteration budget reduces the residual to about 7.40e-9;
both outputs and plans remain in the fixture. Reference defaults at lambda 0.1 and 1 meet the
fixed comparison gate. This is a measured solver divergence; no tolerance was widened and no
failed case was removed. Native nonconvergence, invalid scales, incompatible identities,
overflow and allocation limits are explicit errors.

These costs differ from the spatial map `Transport.sinkhorn` API, which returns squared spatial
cost and has its own finite-iteration contract. Neither route supplies an exact-EMD solver.

## Executable evidence

`ScanpathReferenceSuite` exercises the public consumer API on JVM and Scala.js. Independent
translation/duration/threshold/two-point expectations supplement the exported R values. Mutation
checks must reject inclusive overlap thresholds and removal of onset weighting. Offline fixture
regeneration is:

```sh
python3 tools/r-parity/generate_scanpath.py --eyesim /path/to/eyesim --check
sbt 'compareJVM/testOnly *ScanpathReferenceSuite' 'compareJS/testOnly *ScanpathReferenceSuite'
```

## Typed saved method parameters

`ScanpathMethod[U]` registers MultiMatch, latest-onset fixation overlap and duration-weighted
fixation transport. `kind`, `id`, `info`, `description`, `inspect`, `parameters` and
`componentNames` let applications explain the exact comparison. The declared execution
capability is `SynchronousWholeOperation`: cancellation can separate pair evaluations but
cannot interrupt alignment or a transport iteration inside a pair.

MultiMatch selects `NativeTwoFixations` or `BaselineThreeFixations` explicitly. Its checked
`AlignmentCellLimit` admits the transition lattice before alignment allocation; the named
canonical exact-cost alignment convention is persisted. Overlap retains the ordered query
ledger, including repeated times, its nominal clock, strict threshold, distance, endpoint
and missing-support policy. Under `RequireComplete`, the ledger remains available even when
its scalar similarity is refused. Finite-iteration transport does not advertise symmetry;
its scales, duration scale, regularization, tolerance and allocation/work limits are saved.

`ScanpathMethodCodec[U].codec` writes `eyes4s.scanpath-method@1`. All signed 64-bit query
times and work limits use decimal strings, so values outside JavaScript's exact-number range
survive. Decoding checks the unit, method/version, parameter field set and alignment
convention before constructing a method. Pinned documents cover all three methods on JVM
and Scala.js. Regeneration is `python3 tools/codec-fixtures/scanpath-method.py --write`;
without `--write` the command checks byte identity.

The method document is a reusable component. `ScanpathStudyPlan` supplies the complete study
family over `StudyLayout`, `StudyPairing` and `StudyInput`, including the two phases, keyed
control selection, failure policy, expected frame and preparation budget. The native two-fixation
and baseline three-fixation MultiMatch admissions remain separate choices.

## Saved scanpath studies

`ScanpathStudyCodec(layout, keys)` writes `eyes4s.scanpath-study-plan@1` and
`eyes4s.scanpath-study-result@1`. Plans embed the complete typed input, registered method and
all settings. A canonical `RunStamp` binds that complete plan before result replay. Completed
archives and result-table projections reuse the existing result without recomputing comparisons.
Decoding recomputes and requires exact structured equality with every stored result field;
a different result is a typed refusal, with no tolerance-based acceptance or silent replacement.
The portable fixtures demonstrate all three methods. This is an exact replay-or-refusal contract,
not a claim that every native elementary operation produces identical bits for every possible
input on all runtimes. Transport and MultiMatch retain native operations outside the named shared
norm. The original numeric reference tolerances remain unchanged.

Euclidean overlap uses the shared `FdlibmHypotV1` operation order; Manhattan distance uses
`AbsoluteAxisSumV1`. The distance's `normConvention` is saved and checked. The observed native
`hypot(24,4)` discrepancy is an exact public regression, alongside 120,000 JVM reference pairs
and the 1,024 independent portable kernel cases. Regeneration is
`python3 tools/numerical/hypot.py` and `python3 tools/codec-fixtures/scanpath-study.py --write`.

Preparation uses `ConservativeRawPairsV1`: the raw focal/reference product must fit all pair
ceilings before either pairing is allocated. Filtering or bottom-k selection does not relax that
reservation. Preparation and final reduction are whole operations. `WorkQuanta.pairs` bounds
whole pair comparisons; it does not split alignment, overlap queries or transport iterations.
`ScanpathStudyExecution` reports exact per-stage totals and supports cancellation between those
operations. A cancelled execution returns no partial result.

Results retain both trial keys for every selected pair, original pairing reports, excluded-phase
keys, complete overlap query ledgers, transport diagnostics and per-component matched/control
means with selected, successful and contributing counts. Typed diagnostics retain pair operands
and reduction roles. Ambiguous keys and unmatched focal trials are reported, never replaced by an
arbitrary first match. Controls are evaluated only for focal keys that have a matched reference.

`ResultExports.scanpaths` produces six typed tables: pair scores, overlap queries, transport
diagnostics, reductions, matched-minus-control contrasts and exclusions. Empty method-specific
tables retain their schema. Exact query times use Int64, missing distances use nullable fields,
and repeated query times keep their query indices. CSV uses explicit validity columns; Arrow
retains nulls, booleans and signed Int64 values. Every table shares one converted complete archive
context so the embedded input graph is not copied for each projection.

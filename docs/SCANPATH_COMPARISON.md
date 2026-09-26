# Scanpath and fixation comparison

The R fixtures in [scanpath.json](../tools/r-parity/fixtures/scanpath.json) measure exported
`scanpath`, `multi_match`, `similarity`, `scanpath_similarity`, `fixation_overlap` and
`fixation_similarity` at the frozen revision. The existing Python `multimatch_gaze` conformance
suite remains separate evidence. Only `igraph` 2.3.1 was added to the R dependency lock; every
existing package record and the selected T4transport backend are unchanged.

## Six component slots, five native scores

`ScanpathComparison.baseline` returns all six named component slots in reference order:
VectorShape, Direction, Length, Position, Duration, PositionEmd. The first five delegate to
`MultiMatch` with a minimum of three fixations per operand. Short input produces five located
comparison failures. PositionEmd always carries an explicit unavailable value, consistent with
the decision on `eyesim-compare`; it is never replaced by an unrelated approximation. The
existing `MultiMatch` API still supports two-fixation paths and returns its five-field product.

On the non-square 800×400 frame, translated, duration-changed, geometry-changed, straight and
stationary paths agree with the pinned five R components at `AnalyticTolerance = 1e-12`.
Translation by (30,40) changes position to 1−50/√800000 and leaves the other four components
unchanged. Paired duration ratios 25/50 and 50/100 give duration similarity 1/2. Shape normalizes
by twice the screen diagonal; position and length by the diagonal; direction by π. Tied
straight-path self-alignment is measured separately. The fixture retains the sixth component,
including a duration-reweighted case whose pinned backend returns one; that is a measured
backend outcome, not an exact transport oracle.

`Scanpath.within(window, anchor, Overlap.OnsetInside)` expresses the reference half-open onset
window. A final onset at 200 is excluded by [0,200), admitted by [0,201), and an empty selection
is a typed failure. R returns six missing components for short paths, a scalar missing result
for an empty filtered path, and errors for duplicate onsets or zero-duration transport. Native
checked paths/fixations refuse the invalid construction, and the baseline result keeps its
component shape. Frame geometry supplies native normalization; R requires a separate screensize.

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

The pinned direct R entry point defaults to threshold 60 and `seq(0,max(x$onset),by=20)`, ending
at the **left** final onset. Its fixation-group similarity facade defaults to threshold 40 and
requires `time_samples`. For the translated fixture, the direct default is one, the facade
with explicit queries is zero, and the asymmetric left-grid example is 10/11. The native API
requires explicit parameters; it does not infer a left-dependent grid or an untyped time unit.
The six explicit millisecond queries contain four supported times; when the threshold exceeds
Euclidean distance 50 (or Manhattan distance 70), the result is 4/6. At equality it is zero.

## Spatiotemporal entropic transport

`FixationTransportConfig.of[U]` checks positive x/y scales in U, positive exact `Span` time scale,
nonnegative time weight, positive lambda, iteration budget, marginal tolerance and cost-matrix
limit. `FixationTransport.compare` checks frames and clocks before forming a rectangular cost
matrix. Duration-derived weights use exact endpoint differences before conversion and scaling.
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

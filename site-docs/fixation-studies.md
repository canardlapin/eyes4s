# Compare fixation maps

Use [the executable first study](getting-started.md) when you already have fixation summaries.
Keep participant, stimulus and phase in the trial key. An image-only relation compares across
participants; it does not mean within-participant reinstatement.

## Choose the estimand

A study built with `StudyPlan.configure` (or the version-1 `StudyPlan.cosine`) estimates a map per
trial, compares each focal trial with its matched reference
and explicit within-participant other-image controls, then subtracts the control mean. Binned and
Gaussian estimates remain separate labelled results. It does not pool bandwidths implicitly.

`Weight.Duration` weights occupied positions by fixation duration. `Weight.Uniform` weights
fixations equally. A Gaussian bandwidth is a standard deviation in the frame's units. Edge
truncation and source-wise edge renormalization are different estimators: choose explicitly.

### Smooth differently along x and y

For a row-structured display, you may want a wider horizontal kernel without merging adjacent
rows. Supply both standard deviations explicitly; this does not estimate measurement uncertainty
or choose bandwidths from the observed result.

```scala mdoc
import eyes4s.kernel.Sigma
import eyes4s.plan.StudyEstimate
import eyes4s.surface.EdgePolicy

val directionalEstimates = for {
  horizontal <- Sigma.deg(2.0)
  vertical <- Sigma.deg(1.0)
} yield Vector(StudyEstimate.Anisotropic(horizontal, vertical, EdgePolicy.Renormalise))
```

Pass these estimates, as `StudyScale.Native`, to `StudyPlan.configure` with a grid and input in
degrees; for pixel data, declare them as `StudyScale.Angular` with a `LinearAngularScale`. For direct surface
estimation, use `Smoother.anisotropic(sigmaX, sigmaY, edges).density(measure, grid)`.
The axes follow the frame, not the scanpath direction: this is an axis-aligned Gaussian, not a
rotated, adaptive or foveal kernel. Each width must be at least one fifth of its own grid cell
dimension; failures identify the axis. Both edge policies and duration/uniform weighting remain
available. Equal supported widths give the same numerical surface as `Smoother.gaussian`, while
provenance retains the selected method and both widths.

## Inspect before accepting

`FixationCsv.admit` returns accepted trials, every rejected row and original fields. The default
`requireComplete` refuses incomplete input. Reviewing `accepted` is an explicit analytical choice,
not permission to forget the rejection ledger. One bad keyed row quarantines its entire trial; a
finite position off the screen is admitted and reported under the default admission policy
(`FixationCsv.read` is the version-1 route that quarantines it).

Pair scores retain both keys and a success or failure. A mean has a failure policy and reports
eligible, selected, successful and failed counts. `RequireAll` refuses a mean containing failures;
successful-only analysis requires an explicit minimum. Contrasts retain both source reductions.

For expert control use `Trials`, `Relation`, `PairDesign`, `pair`, `evaluatePairs`, oriented
reductions and `contrast`. The ordinary plan interprets these same operations. Equality relations
use keyed candidate lookup; additional clauses still decide eligibility.

## Persist and export

`StudyCodecs.cosine` encodes versioned parameters, geometry and input identity. `plan.diff` reports
changed choices. `plan.preflight` reports missing or incompatible inputs without estimation.
`ContrastCsv.document` exports keys, scale, method, counts, failures and the saved plan with scores.
Contrast CSV schema 2 adds `sigma_x` and `sigma_y`. Isotropic rows retain `sigma` and repeat that
width in both axis columns; anisotropic rows leave `sigma` empty and supply both axis widths.
Binned rows leave all three empty. Decimal cells with identical input bits have identical spelling
on JVM and JS; numerically close results are not necessarily byte-identical exports.

For smoother implementors, `Smoother.bandwidth` now returns `KernelBandwidth[U]` rather than
`Sigma[U]`: declare `Isotropic(sigma)` or `AxisAligned(sigmaX, sigmaY)`. Existing Gaussian call
sites and saved Gaussian plans remain valid. Scalar `Pyramid` scales remain isotropic.

Next: [repetition designs](repetition.md) or [migration differences](migration.md).

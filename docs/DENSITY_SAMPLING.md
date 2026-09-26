# Density estimation and deterministic lookup

Estimation and lookup have separate contracts. `Smoother.gaussian` bins a weighted
point measure, then applies a separable discrete Gaussian truncated at three
standard deviations, on cell centres. `Sigma` always denotes standard deviation.
`EdgePolicy.Truncate` loses off-grid kernel mass; `Renormalise` corrects each source
kernel before accumulation. `smooth` returns `Intensity`; `density` explicitly
normalizes it to `Mass`. `Smoother.anisotropic` has independent per-axis sigmas;
a list of sigmas is a list of scales, not an anisotropic kernel.

The non-square 5-by-3 fixture has independent two-dimensional discrete-kernel
expectations for uniform and duration weights under both edge policies. These
checks do not duplicate the production separable convolution. Reference `ks`
and `MASS` outputs, actual axes, raw and normalized values, sigma vectors, defaults,
window/minimum-count behavior and failures are stored in
`tools/r-parity/fixtures/kde.json`.

The pinned backends do not share one bandwidth convention: `ks` receives sigma
squared as the covariance, whereas MASS and the custom fallback interpret sigma
through a division by four. An independent continuous Gaussian oracle confirms
the unweighted, duration-weighted and explicitly weighted MASS outputs within the
fixed 1e-7 reference rounding tolerance.
The stored ks approximation differs from direct Gaussian evaluation by up to
about 6.73e-6 in this fixture; the difference is recorded, not called exact parity.
Native discrete smoothing has a different grid/kernel/edge contract and is
checked against its own independent oracle at 1e-12.

At the pinned revision the explicit `weights` argument is honoured and takes
precedence over duration weighting, and weighted MASS works on this non-square
grid; earlier revisions ignored the weights and failed with `invalid 'times'
argument`. `density_by` still drops groups whose estimate fails: both the ks and
MASS group calls retain the full group and drop the one-fixation group with a
warning, which the fixture keeps. The native
weighted estimate works under either edge policy; use `Trials.traverseV` when
processing groups so failures retain their source operands. Native estimation
does not impose R's arbitrary minimum of two fixations. Window and straddling
selection are explicit Scanpath operations before estimation.

## IQR bandwidth

`IqrBandwidth.suggest` provides the pinned type-7-quantile rule, distinct from
`Bandwidth.silverman` and `scott`:

sigma = sqrt((IQRx² + IQRy²)/2) / 1.349 * n^(-1/6).

The rule uses positions without weighting. `Unclamped` leaves it unchanged;
`DisplayOneToFifteenPercent` clamps it to 1–15% of the mean frame width/height.
For the fixture, raw sigma is approximately 0.9759730951 and the upper-clamped
value is 0.6. A constant cloud clamps to 0.04; without the clamp its zero sigma
is rejected by the native positive `Sigma` constructor. Fewer than two points
return a typed error instead of R's NA.

## Lookup and normalization

`DensityLookup.prepare(surface, normalization)` returns a checked `Signed` field
and normalization identity. Supported modes are `None`, `Maximum`, `Sum` and
`SampleZScore`. Normalization precedes lookup. Sample z-scoring divides by the
sample SD with denominator n−1. Exactly constant fields retain their original
values, as do zero maxima/sums. Constant detection is exact and has no hidden
epsilon. A singleton z-score retains the value; the pinned R route errors.
Nonfinite normalization arithmetic is a typed failure, never a fabricated mass.

The prepared field offers two explicit lookup policies:

- `ContainingCell` preserves `Surface.sampleAt`: half-open cells and a located
  failure outside the frame.
- `NearestClampedRIndex` interpolates coordinates to a one-based centre index,
  rounds half ties to even and clamps finite outside coordinates. It reads one
  cell; it never bilinearly interpolates density values.

For centres 0.5,1.5,2.5,3.5,4.5, midpoint coordinates 1,2,3,4 select one-based
indices 2,2,4,4. The source mutant using zero-based ties failed two tests. NaN
and infinity are located query failures; frame compatibility uses `Agreement`.
Every query keeps its original point and order, including empty query vectors.
The lookup fixture uses the actual native cell-centre coordinates in
`eyesim::gen_density`; it does not pretend those centres are every KDE backend's
endpoint-inclusive output lattice.

`PreparedDensityLookup.along` consumes `TrajectorySamples`, preserving each time
and missing trajectory reason. Sampled under `TrajectoryEndpoint.HoldLastOnset`,
which like the reference holds the final fixation after its onset, the fixture
matches both exported `sample_density(times, normalize="sum")` and
`template_sample` with raw values.
This same composition is available to temporal binning. It requires no R at runtime.

Offline regeneration:

```sh
python3 tools/r-parity/generate_kde.py --eyesim /path/to/eyesim --check
```

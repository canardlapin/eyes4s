# Map comparison contracts

`MapSimilarityMethod` is the finite native similarity vocabulary. Each instance accepts checked
`Mass[U]` on agreeing nominal grids and returns a `Similarity` or an operand-bearing error.
It implements `SymmetricCompare`, without claiming a metric. `fromReference` accepts the seven
non-transport names below and rejects unsupported names explicitly. Raw signed or unnormalized
vectors must not enter through a silent conversion: normalization changes some estimands.

| Reference name | Native instance | Quantity and direction | Vector / density / multiscale at pin |
|---|---|---|---|
| pearson | `Distribution.pearson` | centered correlation, [-1,1], larger closer | supported / supported / supported |
| spearman | `Distribution.spearman` | Pearson of average tie ranks, [-1,1], larger closer | supported / supported / supported |
| fisherz | `Distribution.fisherZMachineEpsilon` | atanh(Pearson), r within 64 eps of ±1 snapped to ±1, endpoints clamped to ±(1−2^-52), larger closer | supported / supported / supported |
| cosine | `Distribution.cosine` | normalized dot product, [0,1] on Mass, larger closer | supported / supported / supported |
| l1 | `Distribution.l1Similarity` | 1−total variation, [0,1], larger closer | supported / supported / supported |
| jaccard | `Distribution.extendedJaccard` | dot/(squared norms−dot), [0,1], larger closer | supported / supported / supported |
| dcov | `Distribution.distanceCorrelation` | biased distance **correlation** of cell values, [0,1], larger stronger dependence | supported / supported / supported |
| emd | no similarity alias | pinned density result 1/(1+transport cost), larger closer | error / backend dependent / error |

The old `Distribution.fisherZ` retains its historical ±0.999999999999 clamp. Callers must choose
the endpoint policy explicitly. Distance correlation is not spatial transport and does not mean
that two maps are equal: reverse-ordered cell values can have distance correlation one. This
baseline implementation takes quadratic time and linear auxiliary storage in the number of cells.

`Distribution.cosine` also exposes the `Kernel` contract: its scores are inner
products of maps normalized to unit Euclidean length. The independent Gram-matrix
conformance test checks signed quadratic forms against squared feature-vector
norms. This stronger interface does not change the cosine scores or make them a
distance metric.

## Measured input boundaries

The per-ticket [raw fixture](../tools/r-parity/fixtures/compare.json) retains every exported
vector and density call, warning, error and missing value. Five admitted nonconstant Mass pairs
agree with an independent rational-arithmetic oracle and the reference at absolute tolerance
`MapTolerance = 1e-12`. Cases include exact ties, orthogonal support, identical inputs and a
near-identical pair whose Pearson r is 1 − 32.4 eps: both implementations snap it to one, so its
Fisher z equals the identical pair's atanh(1 − 2^-52), about 18.3684.

Native correlation methods reject constant operands, including identical uniform maps; pinned R
special-cases identical constants to Pearson one and Fisher z atanh(1 − 2^-52), and scores two
all-zero vectors the same way. R returns missing for all one-cell
vector methods; native cosine, Jaccard and L1 similarity are defined there, while correlations
remain errors. Zero total mass, empty, signed, nonfinite and wrong-length values cannot construct
Mass. R may remove common missing cells, recycle unequal vectors or return missing/error results;
none of these operations authorizes a native cardinality change. Raw-amplitude and signed R calls
are recorded separately from the normalized native domain. In the shifted coordinate case the
pinned R density comparison now refuses maps on different lattices, as native `Agreement.grids`
rejects foreign geometry/identity; earlier revisions compared the cells as if aligned.

## Scales and smoothing

`MapScales.of` requires nonempty, unique positive sigmas and agreeing grids, and sorts scales
ascending. `MapComparison.scales` returns a row for every sigma in the union of requested scale
sets. Missing-left, missing-right and comparison failures remain rows. `requested` and
`contributing` are separate counts; `mean` refuses an incomplete result. It does not remove failed
rows or silently choose a common-scale estimand.

Pinned R preserves left-hand order of common sigma names, collapses duplicate matching labels,
intersects unmatched sets and averages with missing removal. The fixture retains those outputs,
including no-common/empty errors and a partially failed correlation whose R mean is one. Native
matching-scale values and complete means agree at 1e-12; differing admission and aggregation are
intentional. A public consumer test runs every method directly and through `Lift.viaSmoothingSymmetric`.
A non-square 5×3 Gaussian pyramid at sigmas 1 and 2 is checked against an independent direct 2-D
kernel oracle before its cosine scale scores are checked. The discrete-versus-continuous KDE
boundary remains as specified in [density conformance](DENSITY_SAMPLING.md).

## Transport qualification

The frozen backend lock selects `T4transport::sinkhornD` for eyesim's `emd` label. Source dispatch
prefers optional `emdist` if installed, then T4transport, then another fallback. We do not change
that lock or infer a general exact solver from the name. Two small cases have independently exact
transport plans: opposite-corner Dirac masses on a 2×2 unit grid have distance √2, while the split
case has all positive source-target distances equal to one. Pinned similarities match 1/(1+√2)
and 1/2 at 1e-12. For the Dirac case native four-direction sliced W1 is (2+√2)/4, and native
squared-ground-cost Sinkhorn is 2. These are distinct estimands, not exact-EMD aliases. No native
LP/network-simplex solver or general exact-backend equivalence is claimed.

## Reproduction

`MapComparisonSuite` executes the public route on JVM and Scala.js. `BaselineMapLawSuite` applies
published symmetry and description laws to every vocabulary member. Numerical fixtures must
reject incorrect tie ranks and an omitted distance-correlation square root. Reference regeneration
is offline; ordinary builds need no R:

```sh
python3 tools/r-parity/generate_compare.py --eyesim /path/to/eyesim --check
sbt 'compareJVM/testOnly *MapComparisonSuite' 'compareJS/testOnly *MapComparisonSuite' \
    'lawsJVM/testOnly *BaselineMapLawSuite' 'lawsJS/testOnly *BaselineMapLawSuite'
```

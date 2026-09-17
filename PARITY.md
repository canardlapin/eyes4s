# Reference agreement and intentional differences

This is a bounded evidence report, not a declaration of complete eyesim coverage. The source
revision is `ecb9c496257bce51acd5330af6a5e7a8d5b84e05`.
[The environment lock](tools/r-parity/fixtures/reference-lock.json) records R/package versions,
input digest, and conventions. [Regeneration instructions](tools/r-parity/README.md) reproduce the
public R calls from an isolated installation of that revision.

The executable [capability inventory](tools/r-parity/baseline.json) currently classifies 21 cases
across all 13 required rows:

| Status | Cases | Meaning |
|---|---:|---|
| Verified equivalent | 2 | Exhaustive matched/control cosine on the pinned fixed-grid study; supplied coordinate transforms (center, rescale, normalize) with an exact affine oracle. |
| Verified intentional divergence | 3 | Matched/control failure semantics, windowed duration-mass analysis, and typed current result exports. |
| Implementation gap | 16 | Each case names a live task and selected falsification input; no parity is claimed. |

## Verified R reference and analytic results

The [synthetic fixture](tools/r-parity/fixtures/matched-control.csv) contains 48 fixation summaries
in 12 trials. Duration-weighted mass on a four-cell grid is compared with cosine similarity.
Each recall trial has one matched encoding trial and two exhaustive within-participant controls.
The [exact rational oracle](tools/r-parity/fixtures/exact.json) independently enumerates every pair;
it does not call eyesim or eyes4s. All six matched scores, control means, differences, and counts
agree with `eyesim::template_similarity` under an absolute tolerance of `1e-12` (counts exact).

| Recall trial | Matched | Control mean | Matched minus control |
|---|---:|---:|---:|
| s1/a | 1.00 | 0.72 | 0.28 |
| s1/b | 0.80 | 0.90 | -0.10 |
| s1/c | 0.80 | 0.82 | -0.02 |
| s2/a | 0.84 | 0.82 | 0.02 |
| s2/b | 1.00 | 0.68 | 0.32 |
| s2/c | 0.84 | 0.86 | -0.02 |

The R call uses `match_on = "key"`, where `key` combines participant and image,
`permute_on = "participant"`, `method = "cosine"`, and `permutations = 100`. There are three
encoding candidates per stratum, so this takes all candidates and excludes the match. It makes
no claim about R versus Scala finite-cap sampling or RNG equivalence.

### Supplied coordinate transforms

[transforms.json](tools/r-parity/fixtures/transforms.json) pins the public `center`, `rescale` and
`normalize` fixation-group methods on the non-square 100-by-50 transform case from
[baseline-cases.json](tools/r-parity/fixtures/baseline-cases.json): three points, a supplied origin,
eyesim's default mean origin, per-axis factors 2 and 3, and the bounds themselves. An exact rational
oracle computed without either implementation agrees with every eyesim output to `1e-12`, and
`Warp.affine`, `Warp.rescale` and `Scanpath.warp` reproduce them with explicitly declared frames and
y-axes in `core/src/test/scala/eyes4s/core/TransformConformanceSuite.scala`. The homogeneous affine,
its solve from the three fitting pairs alone, the excluded held-out target and the axis-swap and
no-translation mutants are oracle values only: eyesim has no coordinate affine entry point. Its
`affine_transform` and `contract_transform` fit density-space maps from matched density moments and
resample densities; the fixture records their response to coordinate tables, and eyes4s does not
implement them (gap `eyesim-transform`).

### Entropy and map arithmetic

[entropy.json](tools/r-parity/fixtures/entropy.json) pins the public `fixation_entropy`
(`entropy_from_mass` through both the `eye_density` and `density` methods) and `Ops.eye_density`
on hand-built `eye_density` objects over the two-by-two lattice in
[baseline-cases.json](tools/r-parity/fixtures/baseline-cases.json): `mass_p` `[1/2, 1/4, 1/4, 0]`
with a zero cell, the uniform `mass_q`, the all-positive `mass_r`, the count map `[2, 1, 1, 0]`
and the signed vector `[0.5, -0.25, 0.75, 0]`.
`kernel/src/test/scala/eyes4s/kernel/EntropyConformanceSuite.scala` consumes the generated
`EntropyFixtures` on JVM and Scala.js.

| Case at the pinned revision | Observed eyesim behavior | eyes4s contract and evidence |
|---|---|---|
| Entropy of a positive map, both bases, `normalize` true and false | `mass_p` gives 1.5 bits (0.75 relative), `mass_q` 2 bits, `mass_r` 1.84644 bits; the count map gives `mass_p`'s values; the `density` class gives the same numbers. | `Mass.entropy` and `relativeEntropy` agree with eyesim and with a 60-digit decimal oracle to `1e-12` ("entropy and relative entropy on positive maps equal eyesim and the oracle in both bases", "an unnormalised count map normalises to the entropy eyesim computes from the counts"). **Verified equivalent.** |
| `+` and `-` on `mass_p`, `mass_q` | `+` is the two-map mean `[0.375, 0.25, 0.25, 0.125]` classed `eye_density_add`; `-` is `[0.25, 0, 0, -0.25]` classed `eye_density_delta`; neither carries a `sigma`. | `Mass.mean` and `Mass.difference` coincide cell by cell and with the exact rationals; the difference is typed `Signed` ("eyesim + is the two-map mean ...", "eyesim - is the cell-wise difference ..."). **Verified equivalent.** |
| `/` on `mass_r`, `mass_q` and on `mass_p`, `mass_q` | `log(e1/e2)` with no floor: `r / q` is finite everywhere; at `mass_p`'s zero cell `p / q` is `-Inf`, `q / p` is `Inf`, `p / p` is `NaN`. | `Mass.logRatio` agrees to `1e-12` wherever eyesim is finite; at the zero cell it floors both cells at `1e-12` (`log(1e-12 / 0.25) = -26.2447`), and `floor = 0.0` is refused as `NonFiniteValue` ("at a zero cell eyesim / is -Inf, Inf or NaN; eyes4s floors both cells at 1e-12 and stays finite"). **Verified intentional divergence.** |
| `*` and a shifted lattice | `*` stops with `undefined operation`; a map whose `x` differs stops in `stopifnot` with `all(e1$x == e2$x) is not TRUE`. | No product typechecks; a second grid returns `GridMismatch` ("eyesim * stops ...", "a shifted lattice ..."). **Verified intentional divergence.** |
| `fixation_entropy` on `mass_p - mass_q` | `NA`: the total is exactly zero, within `.Machine$double.eps`. | `difference(...).entropy()` does not typecheck; `Surface.mass` and `Surface.intensity` return `NegativeValue(3, -0.25)` ("an exact difference map: ..."). **Verified intentional divergence.** |
| `fixation_entropy` on the signed vector | 0.5623 nats, 0.8113 bits, 0.4056 relative: the positive cells `0.5` and `0.75` over the signed total `1.0`, so the "probabilities" sum to 1.25. The fixture's labelled reading of the formula reproduces the number. | `Surface.mass` and `Surface.intensity` return `NegativeValue(1, -0.25)`; `Signed` has no entropy ("a signed map with positive total: ..."). **Verified intentional divergence.** |
| `fixation_entropy` on `p / q` and `p / p` | `p / q` gives exactly 0 in every base: the `-Inf` cell is dropped and the single positive finite cell holds all the mass. `p / p` gives `NA`: its finite cells sum to zero. | The log ratio is `Signed` and has no entropy ("the log-ratio map: ..."). **Verified intentional divergence.** |

The eyesim `fixation_entropy.fixation_group` (density and grid methods) and
`fixation_entropy.eye_density_multiscale` entry points are not pinned here; the density method
depends on the KDE row, and both remain gap `eyesim-entropy`.

## Cases that must remain distinguishable

| Case at the pinned revision | Observed eyesim behavior | eyes4s contract and evidence |
|---|---|---|
| Composite participant/image match | Produces the table above, with `n_perm = 2`. | Explicit same-participant/same-image relation; independent pair list and scalar reduction targets. |
| Image-only match with shared image names | s2/a, s2/b, s2/c matched scores become 0.72, 0.72, 0.64. The first reference image match belongs to s1. | The intended scientific relation includes participant. An image-only eyes4s relation admits cross-participant pairs; types cannot infer omitted scientific intent. This is a query-design difference, not proof that every image-only call is wrong. |
| Extra s3/a recall trial with no template | Warns and drops the row, returning six rows. | Preserve the seventh focal key and return `NoSelectedScores` for its reduction. |
| Duplicate s1/a encoding key | Selects the first occurrence; this fixture's output is unchanged. | Exclude ambiguous reference occurrences and retain their indices in diagnostics; the affected matched focal key has no selected score. |
| Identical constant maps with Pearson | Returns 1. | Return `ConstantInput`: both centered vectors have zero norm, so correlation is undefined. This is independently justified by the correlation formula. |

The full R observations, including warnings, are in [eyesim.json](tools/r-parity/fixtures/eyesim.json).
The Scala suite also exercises duplicate focal keys, incompatible reference geometry, row-order
invariance, and explicit successful-only reduction. These are contract checks, not claims about
additional eyesim algorithms.

## Residual divergences without a reference fixture

The rows below are read from the pinned eyesim source and from current eyes4s code and tests.
None has pinned R output, so none is a verified case, and each names the fixture that would settle
it. Every one of them is an implementation gap in [baseline.json](tools/r-parity/baseline.json);
the owning task is in the last column.

| Case | eyesim at the pinned revision (source reading) | eyes4s contract and evidence | Status |
|---|---|---|---|
| Kernel bandwidth and lattice | `eye_density.fixation_group` passes `sigma` as `H = diag(sigma^2)` to `ks::kde` when `kde_pkg = "ks"`, so `sigma` is a standard deviation there. The `MASS::kde2d` and `kde2d_weighted` fallbacks receive the same `sigma` as `h`, and both divide `h` by four before use, so the effective standard deviation is `sigma/4`; `MASS::kde2d` also ignores weights. Both engines evaluate the continuous kernel on an inclusive-endpoint lattice of `outdim` points (bounds 0 to 100 with `outdim` 5 evaluate at 0, 25, 50, 75, 100), then sum-normalise and `zapsmall`. Off-grid mass is simply never evaluated; there is no edge option. `suggest_sigma` is the root mean square of the two `IQR/1.349` spreads times `n^(-1/6)`, clamped to 1% to 15% of the mean display extent, and `NA` below two points. | `Smoother.gaussian` takes `Sigma` as a standard deviation in frame units, with one meaning everywhere (SmootherCardSuite "the Gaussian card states its bandwidth convention and every edge policy"). Fixations are binned to cell centres, then convolved with a separable discrete kernel truncated at `ceil(3 sigma)` cells and normalised per axis. `EdgePolicy` is a required argument, and `Truncate` and `Renormalise` are pinned to disagree at the edge (SmootherSuite "at the edge the two policies genuinely disagree, which is why it is required"). A `sigma` below a fifth of the cell size is refused as `DegenerateBandwidth` (SmootherSuite "a kernel narrower than the grid can express is refused, with a reason"). `Bandwidth.silverman` and `Bandwidth.scott` take the narrower axis of `min(sd, IQR/1.349)` times `n^(-1/6)` (factor 1.0 or 1.06), drop a zero-spread axis and never clamp (SmootherSuite "a cloud degenerate in one axis still yields a bandwidth", "Scott's rule is wider than Silverman's on the same data"). | **Implementation gap: `eyesim-kde`.** Divergent by construction, unverified: the two eyesim engines disagree with each other by a factor of four on the same `sigma`, both use a point lattice where eyes4s uses cell centres and a truncated discrete kernel, and the two rules both called Silverman compute different numbers. The two-by-two multiscale fixture cannot detect any of this. Settling fixture: the 5-by-3 density case in [baseline-cases.json](tools/r-parity/fixtures/baseline-cases.json) run under both `kde_pkg` values with the locked `ks` and `MASS` versions, plus pinned `suggest_sigma` output. |
| Permutation baseline construction | `run_similarity_analysis` and its fast cosine path take as candidates the matched reference indices of the source rows in the `permute_on` stratum (all source rows without `permute_on`), so a reference matched by several source rows is counted several times. When `permutations` is below the candidate count, `sample(candidates, permutations)` runs before the true match is removed, so the realised `n_perm` is `permutations` or `permutations - 1`, and a cap of one can leave no control (`perm_sim = NA`, `n_perm = 0`). The baseline is the arithmetic mean of the remaining similarities with `na.rm = TRUE`, and `eye_sim_diff = eye_sim - perm_sim`. The general path draws under `furrr_options(seed = TRUE)`, which derives streams from the session RNG rather than a fixed seed; the fast cosine path calls `sample` directly. | Controls are an explicit `Relation`, so the match is never a candidate. `Selection.All` enumerates every eligible pair; `Selection.BottomK(cap, seed, sampleId)` ranks eligible candidates by a keyed hash of seed, sample id, focal key and candidate key and takes the lowest `cap`, so the realised count is `min(cap, eligible)`, independent of row order, and a larger cap is a superset. Seed and sample id are written into provenance. The reduction is `ScoreMean` under an explicit `FailurePolicy`; an empty selection is `NoSelectedScores`. `PairScheduleBudget.default` is unbounded. Evidence: MatchedControlSuite "selected pairs and reductions agree with rational enumeration and pinned eyesim"; PairDesignSuite "raising the cap yields a SUPERSET, never a different sample", "the realised count is min(cap, eligible), and knowable in advance", "a distinct seed gives an independent field too". | **Verified equivalent for the exhaustive baseline only:** `permutations = 100` over two eligible controls reproduces `n_perm = 2` and every `perm_sim` in the table above. **Implementation gap: `eyesim-sampling`** for any finite cap. eyesim caps before excluding the match and eyes4s excludes before capping, the priorities are unrelated, and no cross-language RNG identity is claimed. Settling fixture: the cap-of-one case in `baseline.json` run in R under a recorded `set.seed`, pinning the realised `n_perm` (0 or 1) against the eyes4s constant 1. |

The eyesim readings come from `R/similarity.R` and `R/density.R` at the pinned revision and from
the `MASS` version in the environment lock. The eyes4s readings come from `surface/Smoother.scala`,
`design/Relation.scala` and `design/Analysis.scala` at the current revision. Map arithmetic and
signed maps treated as mass left this table on 2026-09-17 when
[entropy.json](tools/r-parity/fixtures/entropy.json) pinned them; see
[Entropy and map arithmetic](#entropy-and-map-arithmetic).

## Scope of Scala conformance

[MatchedControlSuite](laws/src/test/scala/eyes4s/examples/MatchedControlSuite.scala) consumes generated
values without starting R, fetching dependencies, or reading filesystem resources at test time.
It checks public eyes4s execution through fixation construction, occupancy, binning, normalization,
pair selection, cosine evaluation, scalar reduction, and matched-minus-control contrasts. The pair list comes from the rational
oracle; scalar reduction targets come from the independently checked R output.

The production `contrast` operation reproduces all six stored scalar differences and the five
signed components of the analytic score fixture. The five-component fixture checks component-wise
means and subtraction; it is not a new MultiMatch algorithm conformance claim. See [the contrast contract](docs/CONTRAST_CONTRACT.md).

General KDE bandwidth/edge equivalence, finite control sampling (the residual table above),
fixation-group and multiscale entropy inputs, eyesim temporal template-density sampling,
template-model fitting, real study-data coverage, and the remaining
[capability baseline](docs/EYESIM_CAPABILITIES.md) still require their own reference fixtures.
Historical eyesim bug descriptions outside this report have not been revalidated by this slice.
Earlier Python MultiMatch fixtures remain a separate conformance programme.

## Local validation receipt

The first slice passed all 10 original `MatchedControlSuite` tests on JVM and Scala.js on 2026-09-08.
Removing the participant constraint from the matched design caused four test failures; restoring
the original source restored the passing runs. This is sensitivity evidence for this fixture,
not a mutation-coverage claim for every algorithm. The mutation was not retained.

Reference regeneration with `--check` reproduced all four generated artifacts, including the
runtime lock. `headerCheckAll`, `scalafmtCheckAll`, `scalafmtSbtCheck`, `githubWorkflowCheck`, and
`checkBoundaries` passed. The local sbt JVM was Java 25.0.1. The full test suite, Java 17/21 CI matrix,
and hosted checks were not rerun for that first test/fixture/documentation slice.

The production-contrast slice passed all 12 fixture tests and all 29 `ContrastSuite` tests on both
JVM and Scala.js. The full module matrix passed 1,978 test executions; header, formatting,
generated-workflow, and boundary checks passed. The initial 1 GB sbt process exhausted its heap
while linking the Scala.js laws; the remaining modules and boundaries passed in a 3 GB process.
No source changes were needed for that resource failure. This is local Java 25.0.1 evidence;
the Java 17/21 hosted matrix was not run.

Reversing subtraction caused 12 assertion failures. Ignoring method parameters caused one
compatibility-test failure. Both mutants were restored byte-for-byte, and the passing matrix
used the restored source. Reference `--check` again reproduced all four pinned artifacts.
The completion and design rationale are recorded under `x-contrast` in Mote.

## Saved, tabular and multiscale studies

The [compiled fixation guide](docs/examples/StudyGuide.scala) admits the 48-row CSV without
fabricating raw recordings, runs the matched/control study, saves/reloads its typed plan, and
exports six scalar contrast rows. Its portable tests also cover whole-trial quarantine, retained
rejections, exact 64-bit microsecond timing, quoted identifiers, five-component score exports,
and failed/excluded rows. A changed plan cannot be attached to an unrelated executed result.

[StudyCodecSuite](codec/src/test/scala/eyes4s/codec/StudyCodecSuite.scala) checks the original
version-one schema fixture, meaningful plan diffs, missing/wrong input artifacts, conditional
key codecs, duplicate-key rejection, unsupported schema/method versions and malformed geometry.
Grid dimensions that overflow the indexed cell count now fail at the shared smart constructor.
The registry captures typed keys, parameters and results; no cast is needed for runtime lookup.

The [multiscale oracle](tools/r-parity/generate_multiscale.py) computes 36 normalized maps and
18 contrasts at sigma 0.5, 1 and 2 pixels using 60-digit decimal arithmetic and a closed-form
cell transfer. Both edge policies agree for this two-by-two all-corner fixture because their
source correction is constant and cancels under normalization. This is not evidence that the
policies agree on general grids, or that these estimates reproduce an eyesim KDE.

## Temporal duration-mass evidence

The [temporal oracle](tools/r-parity/generate_temporal.py) fixes 72 fixation summaries in 18 trials,
four named half-open windows, two repetition relations, two spatial scales, exact observation
coverage and 96 matched-minus-control targets. Integer interval intersections independently produce
every fixation's clipped retained duration and every trial/window missing-duration ledger before
60-digit cosine calculation. The late window includes deliberately fragmented support, and the
outside window has no fixation mass.

The saved public workflow and its CSV exporters reproduce all 96 targets and coverage ledgers on
JVM and Scala.js. This is a verified intentional divergence from eyesim's
`sample_density_time`: eyes4s compares duration-weighted masses estimated inside each window,
whereas that eyesim function samples one static template density along an interpolated fixation
trajectory. The two estimands remain separate. Pinned reference evidence for the latter is the
`eyesim-temporal` gap.

The [separate consumer](tools/study-consumer) uses locally packaged artifacts to add its own
validated parameter, score and integer-valued trial key. It runs published codec/contrast laws,
registers and saves/reloads the method, and checks its three-scale results against the independent
oracle. Duplicate/missing extensions and invalid schemas fail explicitly. The interpreter is
unchanged. A raw output comparison establishes equal input digests, equal decoded JSON plan values,
and bit-identical binned contrasts on JVM and Scala.js; Gaussian contrasts agree with absolute
tolerance `1e-12`. Numeric spellings in JSON text can differ between runtimes.

The same consumer now registers extension-owned detector parameters and a versioned codec, saves
and reloads a real typed recording analysis, then runs the restored detector on JVM and Scala.js.
The semantic receipt records equal plan values, recording hashes, detector identity, event count and
sample classifications. The registered wrapper delegates to canonical IVT behavior and therefore
retains the truthful algorithm identity `eyes4s.detect.ivt@1.0.0`; it does not invent a new detector
claim.

### Local acceptance receipt, 2026-09-08

- The complete `testAll` matrix passed **2,034 test executions** on JVM and Scala.js.
  `headerCheckAll`, `scalafmtCheckAll`, `scalafmtSbtCheck`, `githubWorkflowCheck`, and
  `checkBoundaries` passed. The exact public guide is part of the portable I/O tests.
- The isolated artifact consumer passed **14 checks on each runtime**, plus its formatting
  checks. Its verifier checked packaged source entries against current source and recorded
  SHA-256 hashes for all 24 JVM/Scala.js module JARs and the consumer sources.
- Reference regeneration reproduced all four pinned eyesim artifacts. Multiscale regeneration
  reproduced the JSON, portable Scala targets and consumer fixture. The version-one JSON resource
  and the embedded portable fixture contain the same source text.
- The documented launcher wrote the actual six-row CSV and saved plan. Base R read the CSV and
  confirmed keys, units, denominators, matched/control/difference values, and embedded JSON against
  both the pinned eyesim output and rational targets. The initial relative-path launcher command
  failed under sbt's module working directory; the corrected documented absolute-path command passed.
- Replacing the string encoder with a constant failed two codec-law checks. Replacing duration
  weighting with uniform weighting failed three study checks. Both mutants were restored
  byte-for-byte before the passing matrix; neither mutation is retained.

Run the [consumer verifier](tools/study-consumer/verify.py), the commands in the
[fixation guide](docs/FIXATION_STUDIES.md), and the reference generator's `--check` to reproduce
these bounded checks. Evidence is local on Java 25.0.1, Scala 3.3.8 and sbt 1.11.7. The hosted
Java 17/21 matrix was not run. Local artifact publication is not a remote release.

### Current local acceptance receipt, 2026-09-15

- The complete `testAll` matrix passed **2,060 test executions** across JVM and Scala.js. Kernel
  purity, module boundaries, headers, formatting and generated-workflow checks also passed.
- The temporal generator reproduced 72 fixations, 18 trials, 72 coverage ledgers, 36 pair records
  and 96 contrasts, including empty windows. The compiled guide and both runtime suites consumed
  those targets.
- The isolated packaged consumer passed on JVM and Scala.js. Its verifier checked the hashes of all
  24 locally published module artifacts and compared raw semantic evidence across runtimes,
  including the custom detector run.
- `check_baseline.py --eyesim ... --mote` validated all 13 rows, 20 classified cases, fixture
  digests, the exact eyesim Git object, live gap tasks and their downstream blocking edges.

### Local acceptance addendum, 2026-09-17

- The root `test` matrix passed **2,276 test executions** across 24 JVM and Scala.js module runs
  after the surface-totality, smoother-card, scanpath-generator and coordinate-transform changes.
  `scalafmtCheckAll`, `headerCheckAll` and `checkBoundaries` also passed.
- `generate_transforms.py --eyesim ... --check` reinstalled the pinned eyesim archive and reproduced
  `fixtures/transforms.json` and `TransformFixtures.scala` byte for byte; the exact oracle agreed
  with every public `center`, `rescale` and `normalize` output.
- `check_baseline.py --eyesim ... --mote` validated all 13 rows and 21 classified cases (two
  verified equivalent, three intentional divergences, sixteen gaps).

These are local results. Hosted Java 17/21 CI and remote publication were not run.

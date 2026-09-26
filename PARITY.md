# Reference agreement and intentional differences

This is a bounded evidence report, not a declaration of complete eyesim coverage. The source is
the eyesim revision pinned in [baseline.json](tools/r-parity/baseline.json) (`eyesim.revision`),
the only place the pin is written. [r-lock.json](tools/r-parity/r-lock.json) fixes the R version
and every package a generator may load, and each reference fixture records the runtime it
consumed; [the matched/control lock](tools/r-parity/fixtures/reference-lock.json) also records the
input digest and conventions. [Regeneration instructions](tools/r-parity/README.md) reproduce the
public R calls from an isolated installation of the pinned revision.

The executable [capability inventory](tools/r-parity/baseline.json) classifies every case of the
13 required rows under one of the statuses below. `python3 tools/r-parity/check_baseline.py` prints
the current number of cases in each status, and `--list-cases` names them; this report does not
repeat those counts. The sections below give the measured findings case by case.

| Status | Meaning |
|---|---|
| Verified equivalent | A public eyesim call and a public eyes4s route agree on a pinned input within an explicit tolerance, and an independent oracle could detect a mistake they share. |
| Verified intentional divergence | The behaviors differ, the eyes4s behavior is independently justified, and portable tests retain the difference. |
| Implementation gap | The case names a live task and a selected falsification input; no parity is claimed. |
| Out of baseline | Removed from the baseline by an owner decision recorded with its reason and date. The record and its evidence are kept and validated, but reported separately: it is neither a gap nor progress towards closing the baseline. |

## Fixation admission: measured differences

The [pinned admission fixture](tools/r-parity/fixtures/admission.json) calls `eye_table`,
`fixation_group`, `coords` and `as_eye_table`. Valid coordinates and explicitly declared
millisecond times agree exactly. On the four selected rows, eyesim clips `outside-left`, retains
`zero-duration`, and returns the remaining three fixations. eyes4s rejects both defects and
quarantines the other row in their trial: `valid-b` is accepted and all three trial-a source rows
remain in the rejection ledger. No partial trial is analyzed by default.

Reference probes also retain unsorted/duplicate fixations and closed upper boundaries; missing
coordinates disappear. `eye_table` rejects negative duration but `fixation_group` accepts it.
Both empty constructors error, and `as_eye_table` reclasses an unrelated table without validation.
eyes4s instead enforces positive intervals, half-open frames and explicit ordinal ordering, accepts
an empty import as empty data, and rejects malformed headers. Shifted/reversed reference bounds
are measured separately; eyes4s requires an explicit coordinate warp rather than an import default.
The independent oracle checks retained groups, coordinates and times; portable
`AdmissionConformanceSuite` checks source identity, exact row accounting and these rejection rules.

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

### Repetition cosine: phase grouping is not participant-scoped reinstatement

[repetition.json](tools/r-parity/fixtures/repetition.json) records public
`repetitive_similarity(condition_var = "phase", method = "cosine")` pairwise and reduced
outputs at the pinned revision. On the same twelve-trial input, R groups across participants
and stimuli: five other same-phase rows and six different-phase rows per focal trial. The
explicit `RepetitionDesign.withinParticipant` estimand instead compares across occasions within
participant, with one same-stimulus target and two different-stimulus controls. Neither grouping
can substitute for the other without changing the scientific question.

An independent rational dot-product oracle verifies both sets of means and enumerates the
reinstatement edges. Duplicated rows remain independent observations in R; ambiguous full keys
are excluded with diagnostics in eyes4s. Singleton and empty R results are retained, not removed
from the fixture. Portable conformance tests retain failures, unmatched keys and denominators,
exercise keyed finite-cap controls, and compare the saved exhaustive phase route with the direct
all-occasion facade at the same focal keys. The [compiled guide](docs/REPETITION_STUDIES.md)
connects admission, persistence, execution and CSV export. Named absolute cosine tolerance is
`1e-12`; identities and counts are exact.

Local verification on 2026-09-17: the full CI-style command with `sbt -J-Xmx4g` passed 2,492
tests across JVM and Scala.js, headers, formatting, workflow-generation and boundaries on
Homebrew Java 25.0.1. Pinned repetition regeneration and the baseline/Mote checker also passed.
A deliberate `sameOn(participant)` to `differentOn(participant)` source mutant caused four
of six repetition tests to fail; the production source was restored byte-for-byte.
These are local checks, not the supported-JDK hosted release matrix. Other repetition methods,
multiscale aggregation and arbitrary sampled-design persistence remain explicit gaps.

### Historical training-only fixed-feature fitting (2026-09-17)

The [template fitting workflow](docs/TEMPLATE_FITTING.md) now executes the fixed four-row
training case through actual Scala CSV export, R `stats::lm.fit` with no intercept, coefficient
reimport against the saved recipe and held-out evaluation. Exact rational normal equations give
coefficients `(1,2)` and prediction `7`. The observed prediction is `7.000000000000002`, with
MSE `3.1554436208840472e-30`. This is numerical agreement within `1e-12`, not exact floating-point
identity. Both portable runtimes consume the R receipt; numeric CSV spelling can differ while
the parsed values and metadata agree.

Changing only the held-out response from 7 to 700 does not change the training identity, export,
coefficients or prediction; the residual becomes 693. Deliberately contaminating fitting with
that row produces coefficients `(701/8,709/8)`, so the analytic target detects leakage. The R
adapter rejects rank deficiency, underdetermination and non-finite data. A shifted-response case
distinguishes no-intercept fitting from an accidental intercept.

[template.json](tools/r-parity/fixtures/template.json) also pins public eyesim
`template_multireg(method="lm", intercept=FALSE)` on the same vectors as normalized maps.
It returns `(0.4,0.6)`: each predictor and response is separately normalized before regression.
That is a per-source fit over cells, not a held-out trial predictor. The fixture preserves this
different basis and statistical unit rather than claiming universal template-model parity.
At that checkpoint, learned feature construction, cross-fitted transform workflows,
`template_regression`, robust fits, NNLS and typed surface decomposition were separate gaps.
The 2026-09-19 evidence below supersedes the bounded native-template and LM gaps. No coefficient standard errors,
p-values or binomial-on-mass analysis is exposed by the new route.

Local acceptance on 2026-09-17: actual JVM export → R 4.5.1 fit → saved-recipe reimport ran
successfully. The full `sbt -J-Xmx4g headerCheckAll scalafmtCheckAll scalafmtSbtCheck
githubWorkflowCheck testAll checkBoundaries` command passed 2,516 tests across JVM and Scala.js
on Homebrew Java 25.0.1. Pinned template regeneration and the baseline/Mote checker pass.
A production split mutant that included held-out rows in training failed four of six domain
tests; source was restored before the full gate. These are local checks, not hosted supported-JDK
or release evidence.

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
implement them. They are out of the baseline by owner decision on 2026-09-18: fitted density-space
maps belong with PCA, CORAL and CCA in a later adapter module. `baseline.json` keeps the
`fitted-density-transforms` record, this fixture and its eyesim evidence under `out_of_baseline`.

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
| `fixation_entropy` on `mass_p - mass_q` | An error: `fixation_entropy() requires non-negative mass`. Earlier revisions returned `NA` because the total is exactly zero. | `difference(...).entropy()` does not typecheck; `Surface.mass` and `Surface.intensity` return `NegativeValue(3, -0.25)` ("an exact difference map: ..."). **Verified intentional divergence.** |
| `fixation_entropy` on the signed vector | The same non-negative-mass error. Earlier revisions returned 0.5623 nats from the positive cells over the signed total, which is not the entropy of anything. | `Surface.mass` and `Surface.intensity` return `NegativeValue(1, -0.25)`; `Signed` has no entropy ("a signed map with positive total: ..."). Both now refuse, by error or by type. **Verified intentional divergence** as a case, because of the log-ratio row below. |
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
| Signed maps in `similarity.density` | Any `eye_density`-classed object, including `-` and `/` results, is accepted by `similarity.density` without a sign check, so a difference or log-ratio map can be correlated, cosined or transported as if it were mass. | Every `Distribution`, `Transport` and `Lift` comparison takes `Mass`; `Signed` has no comparator (OccupancySuite "entropy on a signed surface does not compile"; the compare suites take `Mass` only). | **Implementation gap: `eyesim-compare`.** The entropy fixture pins `fixation_entropy` on signed maps but does not call `similarity`. Settling fixture: R `similarity` output on `mass_p - mass_q` and on the pinned `signed` vector for every method, recorded as reference behaviour that eyes4s refuses by type. |
| Permutation baseline construction | `run_similarity_analysis` and its fast cosine path take as candidates the matched reference indices of the source rows in the `permute_on` stratum (all source rows without `permute_on`), so a reference matched by several source rows is counted several times. When `permutations` is below the candidate count, `sample(candidates, permutations)` runs before the true match is removed, so the realised `n_perm` is `permutations` or `permutations - 1`, and a cap of one can leave no control (`perm_sim = NA`, `n_perm = 0`). The baseline is the arithmetic mean of the remaining similarities with `na.rm = TRUE`, and `eye_sim_diff = eye_sim - perm_sim`. The general path draws under `furrr_options(seed = TRUE)`, which derives streams from the session RNG rather than a fixed seed; the fast cosine path calls `sample` directly. | Controls are an explicit `Relation`, so the match is never a candidate. `Selection.All` enumerates every eligible pair; `Selection.BottomK(cap, seed, sampleId)` ranks eligible candidates by a keyed hash of seed, sample id, focal key and candidate key and takes the lowest `cap`, so the realised count is `min(cap, eligible)`, independent of row order, and a larger cap is a superset. Seed and sample id are written into provenance. The reduction is `ScoreMean` under an explicit `FailurePolicy`; an empty selection is `NoSelectedScores`. `PairScheduleBudget.default` is unbounded. Evidence: MatchedControlSuite "selected pairs and reductions agree with rational enumeration and pinned eyesim"; PairDesignSuite "raising the cap yields a SUPERSET, never a different sample", "the realised count is min(cap, eligible), and knowable in advance", "a distinct seed gives an independent field too". | **Verified equivalent for the exhaustive baseline only:** `permutations = 100` over two eligible controls reproduces `n_perm = 2` and every `perm_sim` in the table above. **Implementation gap: `eyesim-sampling`** for any finite cap. eyesim caps before excluding the match and eyes4s excludes before capping, the priorities are unrelated, and no cross-language RNG identity is claimed. Settling fixture: the cap-of-one case in `baseline.json` run in R under a recorded `set.seed`, pinning the realised `n_perm` (0 or 1) against the eyes4s constant 1. |

The eyesim readings come from `R/similarity.R` and `R/density.R` at the pinned revision and from
the `MASS` version in [r-lock.json](tools/r-parity/r-lock.json). The eyes4s readings come from `surface/Smoother.scala`,
`design/Relation.scala`, `design/Analysis.scala` and the compare suites at the current revision.
Map arithmetic and signed maps in `fixation_entropy` left this table on 2026-09-17 when
[entropy.json](tools/r-parity/fixtures/entropy.json) pinned them; see
[Entropy and map arithmetic](#entropy-and-map-arithmetic). The `similarity` half of the signed-map
reading stays above as its own row.

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
fitted density-transform breadth, real study-data coverage, and the remaining
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
- After the entropy and map-arithmetic fixture: `generate_entropy.py --eyesim ... --check`
  reinstalled the pinned eyesim archive and reproduced `fixtures/entropy.json` and
  `EntropyFixtures.scala` byte for byte, with the rational and decimal oracle agreeing with every
  finite eyesim value; `generate_transforms.py --check` reproduced the regenerated transform
  artifacts for the new `baseline-cases.json` digest. `EntropyConformanceSuite` (15 tests) passed on
  `kernelJVM` and `kernelJS`; the root `clean test` matrix passed **2,414 test executions** across
  24 module runs; `scalafmtCheckAll`, `headerCheckAll` and `checkBoundaries` passed;
  `check_baseline.py --eyesim ... --mote` validated 13 rows and 24 classified cases (four verified
  equivalent, five intentional divergences, fifteen gaps).

These are local results. Hosted Java 17/21 CI and remote publication were not run.

## Native surface OLS (2026-09-19)

`surface-decomposition-ols` adds a bounded equivalent map-regression case under
`bd-01KYD6SYK02ZRV99MG7FX939ZS`. Its per-ticket input and manifest avoid the frozen shared
case list. Pinned public `template_multireg(method="lm")` and `template_regression(method="lm")`
agree with exact rational coefficients and with native Householder QR for the declared
normalized maps. Intercept/no-intercept, nonzero residual and signed coefficient cases pin
the statistical unit: cells within a map. No cell-wise inference is claimed. See
[SURFACE_DECOMPOSITION](docs/SURFACE_DECOMPOSITION.md) for rank and R-squared conventions.
The bounded native template workflow is qualified below. `check_baseline.py` reports
the current evidence counts.


### Native template fitting and learned mean-map recipe (2026-09-19)

The native fixed-feature guide now executes save/reopen/refit/evaluate without R, with a
separate method and codec from historical R-labelled recipes. The actual JVM CLI produced
prediction 7 and MSE 0; portable tests retain the exact slopes (1,2), response-700, rank,
shape and no-implicit-intercept controls. Historical import remains optional interop.

[LearnedTemplate](docs/TEMPLATE_CV.md) learns an equal-trial mean of normalized training maps,
uses cosine as one feature and fits a through-origin response slope. The independent analytic
case gives mean (2/3,1/3), slope sqrt(5), prediction 3/sqrt(2). Changing held-out map coordinates
and response leaves learned state unchanged while changing prediction to 1 and residual to 699.
An actual admission mutant that included held-out maps failed three tests. Restored source,
saved learned recipes, exclusions and external capability probes pass on JVM and Scala.js.

The pinned exported `template_similarity_cv` fixture separately measures explicit participant
folds, matching, training exclusions and cosine scores. Each fold has a repeated match key in
the other fold; native exclusions and scores agree within 1e-12. Full duplicate/missing outputs
and warnings remain in the fixture. R's first-duplicate/drop-unmatched behavior and its measured
RNG side effect (`rng_restored: false`) are intentional native divergences. Native fold membership
is explicit. This is not learned-transform parity for PCA/CORAL/CCA. Normalized cellwise LM
remains separately qualified by the rational surface-decomposition fixture; robust/NNLS variants
remain deferred and logistic regression on continuous mass is excluded.


### Shared trajectory, finite controls and density lookup (2026-09-19)

[Fixation sampling](docs/FIXATION_SAMPLING.md) now has a shared exact-time trajectory route
with named onset-range/hold-last policies and bounded exact duration replication. Ordered and
singleton fast/slow paths agree with pinned exported calls. Duplicate/empty admission and
0.29-second floating truncation remain explicit divergences. A boundary mutant failed three
tests. Optimized density and generic fixation finite-control calls are both retained under
recorded seed/RNGkind, including different cap-one counts. Native eligibility excludes every
true-match copy before BottomK and keeps occurrence multiplicity and both denominators.

[Density lookup](docs/DENSITY_SAMPLING.md) composes that same trajectory and matches pinned
sample_density(times) and template_sample. Its named one-based-even nearest/clamped policy,
normalization and query retention pass both runtimes; a zero-based-rounding mutant failed two
tests. Exactly constant z-scores retain their field, with no epsilon. Signed output cannot be
mistaken for Mass. The type-7 IQR suggestion and display clamp are separately measured.

The non-square KDE fixture retains all backend outputs, failures and coordinate axes. Native
weighted discrete smoothing has an independent direct 2-D kernel oracle under both edge policies.
MASS unweighted agrees with a continuous Gaussian at fixed 1e-7 rounding tolerance; ks differs
from direct evaluation by up to about 6.73e-6 in this fixture. Pinned explicit weights are ignored,
weighted MASS fails, and density_by removes failures. None of those outcomes is hidden or claimed
as successful backend parity. The native discrete estimator and explicit error retention are
intentional scientific/API differences.


### Map methods and supplied scales (2026-09-19)

[Map comparison](docs/MAP_COMPARISON.md) now measures the complete exported vector, density and
multiscale dispatch matrix. Spearman average ties, extended Jaccard, distance correlation and
1-minus-TV have explicit native instances; the machine-epsilon Fisher endpoint policy is separate
from the unchanged legacy instance. Rational numerical oracles, published symmetry laws,
direct-versus-lift execution and non-square discrete Gaussian scale checks run on both runtimes.
Constant/invalid admission, grid identity and strict failure-preserving scale means intentionally
differ from the pinned R special cases and missing removal. All raw outcomes remain in the fixture.

The frozen reference lock selects T4transport for the method labelled emd. Small analytically exact
transport plans qualify those pinned outputs beside native sliced W1 and squared-cost Sinkhorn;
the label does not establish general exact-backend parity. Multiscale emd remains unsupported.


### Exported scanpath and fixation conformance (2026-09-19)

[Scanpath comparison](docs/SCANPATH_COMPARISON.md) now measures actual R scanpath construction,
direct MultiMatch and table facades. Five components agree on selected non-square cases; the
sixth remains a named unavailable native result with every R backend outcome retained. Native
short/invalid/window behavior is explicit, and Python conformance remains separate evidence.
Shared-trajectory overlap pins both distance norms, strict equality, explicit times and the
all-requested denominator. Direct/facade R defaults differ and remain documented.

The new fixation transport route names duration weights, scaled position/onset, lambda, squared
cost, root cost and similarity separately. It has independent primal/analytic oracles and a
marginal-convergence gate. Pinned T4transport's default lambda .01 run fails the unchanged 1e-7
comparison tolerance; its residual and extended-iteration plan remain evidence of an intentional
solver divergence. No exact solver, metric, debiased-divergence or unconditional backend parity
claim is made. The only new locked dependency is igraph 2.3.1; preexisting records are unchanged.


### Repetition breadth and all-occasion persistence (2026-09-19)

[Repetition studies](docs/REPETITION_STUDIES.md) now measure every baseline map dispatch through
repetitive_similarity, including pairwise/reduced and multiscale mean/none outputs. Independent
Cartesian and rational oracles qualify seven native methods. Heterogeneous scale failures expose
the reference's nested comparison means; typed aggregation keeps source rows and explicit failure
policies at both levels. Transport unavailability and the reference grouping/duplicate behavior
remain declared differences.

A separate supplied-map RepetitionPlan persists a finite relation vocabulary and typed projection
registrations, with exact BottomK seed/cap and input/plan identities. The pinned three-occasion
custom-key example reopens through a fresh registry on both runtimes and reproduces directed
endpoints, counts, scores and provenance; the old two-phase study v1 fixture remains unchanged.
The single-map recipe does not silently infer multiscale aggregation or KDE.


## Static temporal point sampling (2026-09-19)

[PointSamplingPlan](docs/POINT_SAMPLING.md) is now separate from windowed duration occupancy.
The locked `sample_density_time` fixture and independent onset/grid/arithmetic oracle verify
all four normalizations, repeated/unsorted queries, final-endpoint inclusion, source-occurrence
control multiplicity and per-time then per-bin means at absolute `OracleTolerance = 1e-12`.
A heterogeneous-control case gives nested 11/3 versus pooled 4. The reference drops unmatched
sources and chooses the first duplicate template; native retains typed failures and every query.
Native keyed finite selection preserves occurrence identities without claiming R PRNG equality.
Reference missing field cells remain evidence; checked native fields refuse their admission.
The complete plan and replay-verified result are saved under separate versioned schemas, with
exact integer time/seed text and equal JVM/Scala.js JSON values. Earlier temporal-gap statements
above describe their dated checkpoints; this evidence closes the static point-sampling case.


## Finite baseline result exports (2026-09-19)

The [result export matrix](docs/RESULT_EXPORTS.md) covers scalar/structured pairs, endpoint
reductions and contrasts, repetition edges, point samples/bins, fixed and learned predictions,
OLS coefficients/diagnostics/cells, and the existing study/duration-window outputs. Checked
canonical rows drive both portable CSV and optional JVM-only Arrow IPC. Sidecars retain schemas,
units, identities, method parameters, selection evidence, exclusions and provenance, including
empty output. Exact Int64 values do not pass through Double; null bits/CSV validity columns
distinguish missing values from empty strings.

The compiled public example emits actual artifacts. Base R independently checks identities,
counts and analytic scalar values; PyArrow checks every cell, schema, nullability and metadata
and recomputes the content SHA-256. Both readers reject a changed score. Arrow write-failure
tests prove explicit stream/vector cleanup. JVM/JS learned coefficients differ by one rounding
step within the fixed numerical tolerance, so their derived table identities remain different.
The learned input-hash formatted-geometry defect discovered here is fixed and independently
pinned across runtimes; OLS rank tolerance now has numeric provenance. These are export and
interop guarantees; the numerical qualification of each method stays in its own case evidence.


## eyesim pin moved to the audit-fixed master (2026-09-25)

The pin now names eyesim master after its eyes4s parity-audit correctness fixes (owner decision
2026-09-25). Every registered fixture was regenerated from the isolated archive of that
revision; `DESCRIPTION` imports and the R lock are unchanged. The following measured behaviours
changed and supersede the dated statements above; no case changed status.

- **Trajectories.** Both `sample_fixations` paths hold the final fixation after its onset and take
  the later of tied onsets, so `sample_density(times)`, `template_sample`, `fixation_overlap` and
  `sample_density_time` score times after the last onset. Parity tests now use
  `TrajectoryEndpoint.HoldLastOnset`; `OnsetRange` remains a named native alternative.
  `rep_fixations` gives 29 replicas at 0.29 seconds and 100 Hz, as native replication does.
- **Controls.** `template_similarity`, `fixation_similarity`, `scanpath_similarity`,
  `template_similarity_cv` and `sample_density_time` remove every true-match copy and count each
  template once before drawing. A cap of one therefore always yields a control when one is
  eligible. `PointSamplingPlan` counted the source-occurrence multiset; it now counts each matched
  template once, as the reference does, and the regenerated point-sampling fixture agrees.
  Native pairing against a one-row-per-template table reproduces the exhaustive finite-control
  reference row for row. RNG draws are still not claimed equal.
- **Bins and overlap.** Every `sample_density_time` bin is half-open, including the last, as
  `PointBinEndpoint.HalfOpen` is. The overlap facade defaults to the direct threshold 60, and the
  direct default grid spans both paths' onsets, so the score no longer depends on argument order.
  The heterogeneous partial-control fixture now uses one bin `[0, 30)` so that it still separates
  nested from pooled control means.
- **Maps.** Density `similarity` refuses maps on different lattices, as native grid agreement does.
  Fisher z snaps r within 64 machine epsilons of plus or minus one to it before the clamp, so
  every perfect correlation gives atanh(1 - 2^-52); `Distribution.fisherZMachineEpsilon` now
  does the same, and a near-identical fixture pair (r = 1 - 32.4 eps) pins it. Identical constant
  maps give that value in eyesim and remain `ConstantInput` natively.
- **Entropy and KDE.** `fixation_entropy` refuses maps with a negative cell, so the exact
  difference and the signed vector are now errors where eyesim returned `NA` and a positive-cell
  number; the log-ratio rows are unchanged. `eye_density` honours explicit weights and weighted
  MASS works; both agree with a direct weighted Gaussian at the fixed 1e-7 rounding tolerance.
- **Other.** `mm_position_emd` uses all fixations, which changes the pinned sixth component that
  eyes4s leaves unavailable. `template_similarity_cv` restores the caller's RNG stream.

The new pin also exports GazeWeave replay and transport entry points. They are outside the
thirteen baseline rows and are noted for a later epic (owner 2026-09-25); eyes4s does not
implement them.

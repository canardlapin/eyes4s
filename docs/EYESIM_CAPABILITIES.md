# eyesim capability baseline

This document fixes the scientific baseline for subsuming eyesim's basic analysis workflows. The
machine-readable contract is [baseline.json](../tools/r-parity/baseline.json); it is authoritative
for case ids, entry points, estimands, conventions, backend dependencies, fixtures, evidence and
gap ownership. Run its structural, source-pin and tracker checks from the repository root:

```sh
python3 tools/r-parity/check_baseline.py \
  --eyesim /path/to/eyesim \
  --mote
```

The contract currently contains 13 required rows and 26 classified cases: four verified equivalent,
seven verified intentional divergences and fifteen implementation gaps. This is a finite work list,
not a parity percentage. A row remains open while any required case is a gap.

## Reference and classification

The source is eyesim 0.1.0.9000 at
`ecb9c496257bce51acd5330af6a5e7a8d5b84e05`. The inventory is taken from the pinned `DESCRIPTION`,
`NAMESPACE` and the R source files named in `baseline.json`. The reference is available at the
[exact Git revision](https://github.com/bbuchsbaum/eyesim/tree/ecb9c496257bce51acd5330af6a5e7a8d5b84e05).
The checker reads those files from the Git object, so uncommitted checkout changes and the current
branch cannot change the inventory.

Every case has exactly one of these statuses:

- **Verified equivalent** means a public eyesim call and public eyes4s route agree on a pinned input
  and explicit tolerance, with an independent oracle capable of detecting a shared mistake.
- **Verified intentional divergence** means the observed behaviors differ, the eyes4s behavior is
  independently justified, and portable tests retain that difference.
- **Implementation gap** means the required reference measurement, public composition or scientific
  behavior is still missing. Each gap names one live Mote task and a concrete evidence target.

Symbol presence, a source review, adjacent-project output and an eyes4s-only oracle do not establish
eyesim equivalence. Advanced exported models listed below remain outside this baseline; optional
placement never excuses an omitted required case.

## Dependency boundary

The pinned package imports Rcpp, proxy, assertthat, dplyr, ggplot2, tibble, broom, furrr, purrr,
MASS, RColorBrewer, magrittr, pracma, ks, rlang and scales, links RcppArmadillo, and requires C++14.
Baseline reference cases use only the narrower method-specific subset recorded in the contract.

| Capability | Required reference backends | Optional backend triggered by a method |
|---|---|---|
| Construction, grouping and transforms | base R, dplyr, tibble, assertthat, stats | none |
| KDE and bandwidth | `ks`, `MASS`, stats, dplyr | none; the two KDE engines are separate cases |
| Map comparisons | proxy | energy for `dcov`; transport or emdist for EMD |
| Fixation transport | stats interpolation | T4transport or transport for Sinkhorn routes |
| Matched/control and repetition | dplyr, purrr, future, furrr | comparison-specific backends above |
| Template models | stats `lm`/`glm`, MASS `rlm` | nnls for nonnegative least squares |

Reference fixture locks record versions actually consumed. Statistical fitting backends stay outside
eyes4s pure modules; the baseline template route may use a typed optional adapter or executable
export/reimport boundary.

## Required baseline

| Scientific task and case ids | eyesim entry points, methods and estimand | eyes4s contract and current evidence | Status and owner |
|---|---|---|---|
| Construct and group fixation data (`fixation-table-admission`) | `fixation_group`, `eye_table`, `as_eye_table`, `coords`; one ordered fixation-summary group per trial. Measure bounds clipping, relative coordinates, grouping, onset/duration units and invalid rows. | `FixationCsv`, `Event.Fixation`, `Scanpath`, `Trials`; the guide already proves typed keys and accepted/rejected row accounting without fabricating raw samples. | **Gap — `eyesim-admission`.** Pinned R clipping, ordering and invalid-value output is missing. |
| Transform spatial coordinates (`basic-coordinate-transforms`, `fitted-density-transforms`) | `center`, `rescale`, `normalize`; supplied coordinate maps on a fixation group. `affine_transform`, `contract_transform`; density-space maps fitted from matched density moments and applied by bilinear resampling, not coordinate maps. Coordinate normalization must remain distinct from mass normalization. | `Frame`, `Warp`, `Perspective`, `Scanpath.warp` reproduce the pinned center, mean-center, rescale and normalize outputs on the non-square 100-by-50 bounds with explicit frames and y-axes. The homogeneous affine, a training-only three-pair solve and the excluded held-out point are exact-oracle targets with axis-swap and no-translation mutants; eyesim has no coordinate affine entry point. No fitting backend exists in pure modules. | **Equivalent** for the supplied maps ([transforms.json](../tools/r-parity/fixtures/transforms.json), `core/.../TransformConformanceSuite.scala`); **gap — `eyesim-transform`** for the fitted density-moment `affine_transform`/`contract_transform`, whose response to coordinate tables is pinned and which eyes4s does not implement. |
| Describe and compare scanpaths (`scanpath-multimatch`, `fixation-overlap-and-transport`) | `scanpath`, `scanpath_similarity`, `multi_match`, `fixation_similarity`, `fixation_overlap`; MultiMatch vector/direction/length/position/duration plus eyesim's position EMD, and overlap/Sinkhorn with explicit thresholds, time step and metric. | `Scanpath`, `Alignment`, `MultiMatch`, `Transport`, `Lift`; existing Python MultiMatch conformance is kept separate from eyesim evidence. | **Gaps — `eyesim-scanpath`.** The pinned R MultiMatch, overlap and transport cases remain unmeasured. |
| Estimate and sample density (`kde-estimation-and-bandwidth`, `density-point-evaluation`) | `eye_density`, `density_by`, `get_density`, `gen_density`, `suggest_sigma`, `sample_density`; weighted KDE or normalized mass on an explicit grid, then deterministic point evaluation. Pin `ks` versus `MASS`, sigma, bounds, outdim, window, minimum fixations, edges, interpolation and none/max/sum/zscore normalization. | `PointMeasure`, Gaussian `Smoother`, `Bandwidth`, `Grid`, `Mass`, `Intensity`, `Surface.sampleAt`; a 5-by-3 adversarial input is selected. | **Gaps — `eyesim-kde`**; public surface totality (`Surface.at`, `Grid.indexAt`/`cellCentre` returning `Option`) is delivered. |
| Sample fixation patterns and distributions (`fixation-and-density-random-sampling`) | `sample_fixations`, `rep_fixations` and density draws; a requested temporal resolution or draw count from explicit weights. Pin replacement, endpoints, seed, RNG and realized cardinality. | Design RNG and explicit selection primitives exist, but there is no complete public distribution-sampling route or R fixture. | **Gap — `eyesim-sampling`**; relation sampler mutation coverage remains `bd-01KYD6T48ZREC657WRGSAHM1R3`. |
| Compute entropy and combine maps (`fixation-entropy`, `fixation-entropy-derived-inputs`, `density-map-mean-and-difference`, `density-map-log-ratio`, `signed-maps-as-mass`) | `fixation_entropy` for density/grid/fixation/multiscale inputs and `Ops.eye_density`; Shannon entropy or an explicitly named compatible-grid operation. Pin base, normalization, duration weights, scale reduction and `+ - * /` meanings. | `Mass.entropy` and `relativeEntropy` reproduce the pinned `fixation_entropy` output on supplied positive maps in both bases; `Mass.mean` and `Mass.difference` coincide with eyesim `+` and `-` cell by cell and with an exact rational oracle; `Mass.logRatio` coincides with `/` away from zero cells. At a zero cell eyesim gives `-Inf`, `Inf` or `NaN` and eyes4s floors at `1e-12`. On signed maps eyesim returns `NA` (exact difference) or a finite positive-cell number (signed vector), and eyes4s refuses by type or with `NegativeValue`; the eyesim values are pinned. | **Equivalent** for entropy on supplied maps and for `+`/`-` ([entropy.json](../tools/r-parity/fixtures/entropy.json), `kernel/.../EntropyConformanceSuite.scala`); **intentional divergence** for `/` at zero cells, the absent product and signed maps treated as mass; **gap — `eyesim-entropy`** for fixation-group density/grid and multiscale entropy inputs. |
| Compare maps and fixation patterns (`distribution-method-matrix`) | `similarity` for default, density, multiscale and fixation groups; Pearson, Spearman, Fisher z, cosine, L1, Jaccard, distance covariance, EMD, Sinkhorn and overlap. Each result's scale/direction, geometry support and constant/empty behavior are part of the estimand. | `Distribution`, `Lift`, `Similarity`, `MeasureDistance`, `Transport`; current unit tests establish internal contracts only. | **Gap — `eyesim-compare`**, with scalable advanced implementations tracked separately by `bd-01M02N4E54KR43Q5JSSMCV4G2E`. |
| Analyze matched templates and controls (`matched-control-exhaustive-cosine`, `matched-control-failure-semantics`, `matched-control-finite-sampling`) | `template_similarity`, `fixation_similarity`, `template_sample`; matched-minus-mean-control under explicit identity/strata, then finite control or template point sampling. | Public `StudyPlan`, relations, pair design, cosine mass, reductions, signed contrasts, saved execution and tidy CSV reproduce all exhaustive targets. Unmatched, ambiguous and failed comparisons remain values. | **Equivalent** for exhaustive cosine; **intentional divergence** for first-duplicate, dropped-unmatched and constant-Pearson behavior; **gap — `eyesim-sampling`** for finite sampling. |
| Analyze repetition and reinstatement (`repetition-phase-cosine`, `repetition-and-reinstatement`) | `repetitive_similarity`; same- versus different-condition comparisons with pairwise/reduced modes and multiscale mean/none. Participant strata, direction, self edges and realized denominators must be explicit. | `RepetitionDesign` exposes same-participant, across-occasion matched/control edges and typed contrasts; the compiled [guide](REPETITION_STUDIES.md) proves admission, saved exhaustive phase execution and CSV. Pinned phase-only R cosine groups across participants/stimuli (five/six references), versus the explicit reinstatement design (one/two). Duplicate, singleton and empty R cases and an independent rational oracle are retained. | **Intentional divergence** for supplied-map phase cosine; **gap — `eyesim-repetition`** for remaining methods, multiscale aggregation and arbitrary sampled repetition persistence. |
| Compare across spatial scales (`multiscale-density-and-comparison`) | Vector-sigma `eye_density`, multiscale `similarity` and `fixation_entropy`; one labelled result per bandwidth followed by explicit mean/none reduction. | The extension guide and 60-digit oracle prove three eyes4s Gaussian scales and contrasts. The symmetric 2-by-2 fixture cannot establish general edge behavior or eyesim KDE parity. | **Gap — `eyesim-compare`**, related to `eyesim-kde` and surface totality. |
| Analyze change over time (`windowed-duration-repetition`, `temporal-template-density-sampling`) | `sample_density_time` evaluates one static template density along an interpolated focal path; temporal `eye_density` filters events by a window. Pin time units, interpolation, normalization, bin closure, final bin, aggregation, matching, permutations and missing support. | The temporal guide now exports 96 windowed repetition contrasts with exact clipped-duration ledgers, explicit missing support and empty windows on JVM and Scala.js. This duration-mass estimand differs from template point sampling. | **Intentional divergence** for the verified windowed duration workflow; **gap — `eyesim-temporal`** for `sample_density_time`; serializable anchored epochs remain `bd-01KYDZ80ANFH3HW11946E4QTR8`. |
| Fit and evaluate templates without leakage (`training-only-fixed-feature-lm`, `leakage-free-basic-template-model`) | `template_similarity_cv`, `template_regression`, `template_multireg`; distinguish cross-fitted learned transforms, per-map cell regression and held-out trial prediction. | The [compiled fixed-feature route](TEMPLATE_FITTING.md) exports training only, fits intercept-free R QR, saves/restores a typed recipe and imports training-bound coefficients for keyed held-out evaluation. Exact coefficients `(1,2)` and prediction `7` survive held-out response changes; the contaminated-fit mutant does not. Pinned eyesim normalized-map regression gives `(0.4,0.6)` on the corresponding vectors, a different basis and statistical unit. | **Intentional estimand divergence** for the fixed-feature basic route; **gap — `eyesim-template`** for learned template/feature construction, cross-fitted similarity and the surface-regression bridge. Robust/NNLS variants remain deferred; binomial regression on continuous mass is excluded. |
| Reuse, inspect and export (`typed-current-result-exports`, `complete-baseline-result-exports`) | eyesim returns data frames/list columns from grouped workflows. The scientific contract is reusable keyed rows with method identity, units, denominators, failures and provenance. | Current scalar/structured/temporal CSV and versioned JSON preserve quoted keys, 64-bit times, exclusions, failures and hashes; base R consumes the emitted study CSV. | **Intentional language/API divergence** for current workflows; **gap — `eyesim-export`** for every remaining baseline result family, related to `io-export`. |

## Pinned inputs and regeneration

[baseline-cases.json](../tools/r-parity/fixtures/baseline-cases.json) fixes the small adversarial inputs
for every unresolved case: clipping, non-square transforms, scanpaths, KDE queries, random sampling,
entropy and arithmetic, comparison edge cases, temporal point sampling and a leakage-sensitive linear
fit. These are selected inputs, not manufactured passing outputs. Their future reference artifacts
must retain the same input digest or explicitly revise this contract.

Implemented evidence is reproducible with:

```sh
python3 tools/r-parity/generate_reference.py --eyesim /path/to/eyesim --check
python3 tools/r-parity/generate_transforms.py --eyesim /path/to/eyesim --check
python3 tools/r-parity/generate_entropy.py --eyesim /path/to/eyesim --check
python3 tools/r-parity/generate_repetition.py --eyesim /path/to/eyesim --check
python3 tools/r-parity/generate_template.py --eyesim /path/to/eyesim --check
python3 tools/r-parity/generate_multiscale.py --check
python3 tools/r-parity/generate_temporal.py --check
```

The first five commands archive the pinned Git object, install that source into a temporary R
library, and invoke exported eyesim functions; each records the observed R and package versions.
The other commands are independent rational, integer-overlap or high-precision decimal oracles and
do not call eyesim. Use `check_baseline.py --run-regeneration --eyesim /path/to/eyesim` to run all
seven.
Portable JVM and Scala.js tests consume generated values without starting R or reading runtime
fixtures.

The matched/control input contains 48 authored fixation summaries in 12 trials. The temporal input
extends it to 72 summaries in 18 trials and adds explicit observation coverage. The selected
template-model case uses the exact intercept-free relation `y = x1 + 2*x2`; changing only the
held-out response from 7 to 700 must leave fitted coefficients unchanged.

## Beyond the baseline

The pinned export set also contains GazeWeave replay and transport models, Gaussian mixtures,
elastic consensus alignment, CRQA helpers and PCA/CORAL/CCA or other learned transformations. These
belong to optional modeling or later scientific-breadth work. They still require typed geometry,
fit/apply separation, training/test isolation, diagnostics and reusable results, but they do not
become baseline requirements merely because eyesim exports them.

Plotting, animation, installation helpers and R-specific S3 mechanisms map to consumer integrations
or language idioms. eyes4s must expose the scientific values needed by those consumers; it need not
reproduce those mechanisms.

Measured agreements and divergences live in [PARITY.md](../PARITY.md). Migration documentation must
point to these bounded cases and cannot convert an open gap into a supported claim.

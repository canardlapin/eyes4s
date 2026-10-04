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

The contract has 13 required rows. The checker prints how many classified cases each status
currently holds, and `--list-cases` names them; this document does not repeat those counts, which
change as cases close. This is a finite work list, not a parity percentage. A row remains open
while any required case is a gap.

## Reference and classification

The source is the [eyesim](https://github.com/bbuchsbaum/eyesim) revision and version pinned in
`baseline.json` (`eyesim.revision`, `eyesim.version`). That file is the only place the pin is
written, so moving it is a one-line change followed by regeneration (see
[the parity README](../tools/r-parity/README.md#moving-the-eyesim-pin)). The inventory is taken
from the pinned `DESCRIPTION`, `NAMESPACE` and the R source files named in `baseline.json`. The
checker reads those files from the Git object, so uncommitted checkout changes and the current
branch cannot change the inventory.

Every case has exactly one of these statuses:

- **Verified equivalent** means a public eyesim call and public eyes4s route agree on a pinned input
  and explicit tolerance, with an independent oracle capable of detecting a shared mistake.
- **Verified intentional divergence** means the observed behaviors differ, the eyes4s behavior is
  independently justified, and portable tests retain that difference.
- **Implementation gap** means the required reference measurement, public composition or scientific
  behavior is still missing. Each gap names one live Mote task and a concrete evidence target.

A case the owner removes from the baseline leaves the counted cases. Its record moves to
`out_of_baseline` in `baseline.json` with the owner's reason and decision date, keeps its fixtures
and evidence, and is still validated; the checker reports it on a separate line, so it is neither
a gap nor progress towards closing the baseline.

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

[r-lock.json](../tools/r-parity/r-lock.json) fixes the R version and every package a generator may
load, and each reference fixture records the versions it consumed. R and its statistical backends are offline reference tools only. Native baseline fitting uses
the shared Householder QR in `design`; the template guide now fits, saves and reopens natively. External coefficient import may remain optional interop, never a required
workflow step (owner decision 2026-09-18).

## Required baseline

| Scientific task and case ids | eyesim entry points, methods and estimand | eyes4s contract and current evidence | Status and owner |
|---|---|---|---|
| Construct and group fixation data (`fixation-table-admission`) | `fixation_group`, `eye_table`, `as_eye_table`, `coords`; one ordered fixation-summary group per trial. Bounds clipping, relative coordinates, grouping, onset/duration units and invalid rows are pinned. | `FixationCsv`, `Event.Fixation`, `Scanpath`, `Trials`; valid coordinates/times agree. Rejected rows and whole-trial quarantine remain explicit rather than clipping or accepting invalid intervals. Shifted/flipped reference bounds and empty/duplicate/negative/nonfinite cases are measured. | **Intentional divergence** ([admission.json](../tools/r-parity/fixtures/admission.json), `io/.../AdmissionConformanceSuite.scala`). |
| Transform spatial coordinates (`basic-coordinate-transforms`, `fitted-density-transforms`) | `center`, `rescale`, `normalize`; supplied coordinate maps on a fixation group. `affine_transform`, `contract_transform`; density-space maps fitted from matched density moments and applied by bilinear resampling, not coordinate maps. Coordinate normalization must remain distinct from mass normalization. | `Frame`, `Warp`, `Perspective`, `Scanpath.warp` reproduce the pinned center, mean-center, rescale and normalize outputs on the non-square 100-by-50 bounds with explicit frames and y-axes. The homogeneous affine, a training-only three-pair solve and the excluded held-out point are exact-oracle targets with axis-swap and no-translation mutants; eyesim has no coordinate affine entry point. No fitting backend exists in pure modules. | **Equivalent** for the supplied maps ([transforms.json](../tools/r-parity/fixtures/transforms.json), `core/.../TransformConformanceSuite.scala`); **out of baseline** (owner decision 2026-09-18) for the fitted density-moment `affine_transform`/`contract_transform`: fitted density-space maps belong with PCA, CORAL and CCA in a later adapter module. Their response to coordinate tables stays pinned, and eyes4s does not implement them. |
| Compare scanpaths (`scanpath-multimatch`) | Exported R construction, six-component direct and table calls. | [Scanpath components](SCANPATH_COMPARISON.md): five measured scores and explicit sixth-component unavailability, windows and typed admission. | **Intentional divergence** for sixth backend, cardinality and errors; five measured components agree. Python evidence remains separate. |
| Estimate and sample density (`kde-estimation-and-bandwidth`, `density-point-evaluation`) | `eye_density`, `density_by`, `get_density`, `gen_density`, `suggest_sigma`, `sample_density`; weighted grids and deterministic lookup. | [Density conformance](DENSITY_SAMPLING.md) pins both backends, defaults, weighting failures, a non-square Gaussian oracle, type-7 `IqrBandwidth`, all normalization modes and shared trajectory composition. | **Intentional divergence** for native discrete KDE versus reference continuous backends, failure retention and invalid bandwidths; named nearest/clamped lookup matches at identical coordinates. The pinned reference honours explicit weights and its weighted MASS path agrees with a direct weighted Gaussian; `density_by` still drops failed groups. |
| Sample fixation patterns and distributions (`fixation-trajectory-evaluation`) | Fast/slow `sample_fixations`, `rep_fixations`, `sample_density(times)`; deterministic trajectories and replication. | [FixationTrajectory](FIXATION_SAMPLING.md) preserves time/order/cardinality and names endpoint policies; exact bounded replication and density-time composition pass both runtimes. | **Intentional divergence** for duplicate/empty admission; ordered/singleton trajectories (both reference paths hold the last fixation, as `HoldLastOnset` does), the 29-row duration replication and shared density-time values agree. |
| Compute entropy and combine maps (`fixation-entropy`, `fixation-entropy-derived-inputs`, `fixation-group-density-entropy`, `multiscale-entropy-reduction-policy`, `density-map-mean-and-difference`, `density-map-log-ratio`, `signed-maps-as-mass`) | `fixation_entropy` for density/grid/fixation/multiscale inputs and `Ops.eye_density`; Shannon entropy or an explicitly named compatible-grid operation. Pin base, normalization, duration weights, scale reduction and `+ - * /` meanings. | `Mass.entropy` and `relativeEntropy` reproduce the pinned `fixation_entropy` output on supplied positive maps in both bases; `Mass.mean` and `Mass.difference` coincide with eyesim `+` and `-` cell by cell and with an exact rational oracle; `Mass.logRatio` coincides with `/` away from zero cells. At a zero cell eyesim gives `-Inf`, `Inf` or `NaN` and eyes4s floors at `1e-12`. On the exact difference and the signed vector eyesim now refuses with an error, as eyes4s refuses by type or with `NegativeValue`; on the log-ratio map `p / q` it still drops the `-Inf` cell and returns 0. `FixationEntropy` (surface) goes from a scanpath to an entropy: `occupancy` on an `OccupancyLattice` reproduces the grid method on explicit and padded bounds once its closed upper edge, edge-cell clamp and uniform weight are named; `MultiscaleEntropy` reproduces `aggregate = "none"` and `"mean"` by sigma label, on the multiscale object and on a fixation group with a sigma vector. `density` and `multiscale` smooth with the native estimator, so they differ from eyesim's `ks` maps although the entropy of eyesim's own map agrees. eyesim drops a massless scale from its mean and returns `NA` for one fixation; eyes4s refuses the massless scale, returns every scale and takes explicit scale weights. The eyesim outcomes are pinned. | **Equivalent** for entropy on supplied maps, for `+`/`-`, for the grid method and for multiscale none/mean ([entropy.json](../tools/r-parity/fixtures/entropy.json), `kernel/.../EntropyConformanceSuite.scala`, [derived-entropy.json](../tools/r-parity/fixtures/derived-entropy.json), `surface/.../FixationEntropyConformanceSuite.scala`); **intentional divergence** for `/` at zero cells, the absent product, signed maps treated as mass, the density method and multiscale reduction policy. |
| Compare maps (`distribution-method-matrix`) | Seven scalar similarities and density-only backend-dependent emd. | [Map method matrix](MAP_COMPARISON.md), independent rational oracles, symmetry laws and public smoothing lift on both runtimes. | **Intentional divergence** for malformed/constant input and transport estimands; normalized nonconstant methods, including the Fisher z endpoint snap near one, agree, and both implementations refuse maps on different lattices. |
| Analyze matched templates and controls (`matched-control-exhaustive-cosine`, `matched-control-failure-semantics`, `matched-control-finite-sampling`) | `template_similarity`, `fixation_similarity`, `template_sample`; matched-minus-mean-control under explicit identity/strata, then finite control or template point sampling. | Public `StudyPlan`, relations, pair design, cosine mass, reductions, signed contrasts, saved execution and tidy CSV reproduce all exhaustive targets. Unmatched, ambiguous and failed comparisons remain values. | **Equivalent** for exhaustive cosine; **intentional divergence** for first-duplicate, dropped-unmatched and constant-Pearson behavior; **intentional divergence** for [finite sampling](FIXATION_SAMPLING.md): both exclude every true-match copy and count each template once before selection, with realized count `min(cap, eligible)`; the draws differ (keyed selection versus R's RNG) and a zero cap is a native typed error. Pinned R draws and both facade outputs are retained. |
| Repetition and reinstatement (`repetition-and-reinstatement`) | Full map dispatch, pairwise/reduced and multiscale nested means. | [Repetition workflows](REPETITION_STUDIES.md): typed per-level failure policies, all-occasion custom-key plans and fresh-registry persistence with finite controls. | **Intentional divergence** for explicit participant/identity relations, failure retention and EMD availability; measured native method means agree. Historical two-phase v1 preserved. |
| Compare scales (`multiscale-density-and-comparison`) | Named sigma comparisons and mean/none aggregation. | [Checked map scales](MAP_COMPARISON.md), non-square Gaussian oracle and complete-scale means. | **Intentional divergence**: native preserves every requested/missing/failed scale and refuses incomplete means; pinned R intersects and removes missing values. |
| Analyze change over time (`windowed-duration-repetition`, `temporal-template-density-sampling`) | `sample_density_time` evaluates one static template density by nearest-grid lookup along the fast constant-step focal path; temporal `eye_density` filters events by a window. Pin time units, interpolation, normalization, bin closure, final bin, aggregation, matching, permutations and missing support. | The temporal guide now exports 96 windowed repetition contrasts with exact clipped-duration ledgers, explicit missing support and empty windows on JVM and Scala.js. This duration-mass estimand differs from template point sampling. | **Intentional divergence** for the verified windowed duration workflow; **intentional divergence** for [static point sampling](POINT_SAMPLING.md): all normalizations, the half-open final bin, the held final fixation, distinct-template controls and nested control means agree on admitted cases; keyed finite selection, retained matching failures and checked fields are explicit differences. The complete point recipe/result has separate replay-verified persistence. |
| Fit and evaluate templates without leakage (`training-only-fixed-feature-lm`, `surface-decomposition-ols`, `leakage-free-basic-template-model`) | `template_similarity_cv`, `template_regression`, `template_multireg`; separate trial prediction, matched-map CV and cellwise regression. | [Native fixed-feature fitting](TEMPLATE_FITTING.md) and the [training-only learned mean/cosine recipe](TEMPLATE_CV.md) save/reopen without R. Analytic map/response perturbations and a killed feature-leakage mutant qualify the learned route. Pinned explicit-fold cosine scores/exclusions and [surface OLS](SURFACE_DECOMPOSITION.md) are separately measured. | **Verified equivalence** for cellwise LM and explicit-fold cosine scores/exclusions; **intentional divergence** for the learned recipe estimand, explicit fold assignment and duplicate/missing handling; the pinned CV now restores the caller's RNG stream. Native [NNLS, simplex mixtures and partial association](SURFACE_DECOMPOSITION.md) are checked against exact oracles; NNLS is **verified equivalent** to pinned `nnls`, and partial Spearman matches pinned `rank` with an **intentional divergence** in labelling (an association, not a beta). Robust variants and fitted density transforms remain deferred; binomial regression on continuous mass is excluded. |
| Reuse, inspect and export (`typed-current-result-exports`, `complete-baseline-result-exports`) | eyesim returns data frames/list columns from grouped workflows. The scientific contract is reusable keyed rows with method identity, units, denominators, failures and provenance. | Current scalar/structured/temporal CSV and versioned JSON preserve quoted keys, 64-bit times, exclusions, failures and hashes; base R consumes the emitted study CSV. | **Intentional language/API divergence** for current workflows; **intentional language/API divergence** for the complete finite [result export matrix](RESULT_EXPORTS.md), including shared checked CSV and optional JVM Arrow IPC. Actual artifacts have independent base-R/PyArrow readback, exact time/count/identity checks and resource-failure tests. |

## Pinned inputs and regeneration

[baseline-cases.json](../tools/r-parity/fixtures/baseline-cases.json) fixed the first small
adversarial inputs: clipping, non-square transforms, scanpaths, KDE queries, sampling weights,
entropy and arithmetic, comparison edge cases, temporal point sampling and a leakage-sensitive linear
fit. These are selected inputs, not manufactured passing outputs. The file is frozen: four
generators hash all of it, so any edit would force each of them to regenerate, and the checker
refuses a changed digest. A case that needs new or revised inputs takes its own
`tools/r-parity/fixtures/cases/<ticket>.json`, registered in
`tools/r-parity/manifest.d/<ticket>.json`; the
[parity README](../tools/r-parity/README.md#adding-a-case) gives the steps.

Every generator is registered in `baseline.json` or a `manifest.d/` fragment and runs standalone
with `--check`. Generators that call eyesim archive the pinned Git object, install that source into
a temporary R library and invoke exported eyesim functions in a fixed R session against the locked
package library; each records the R and package versions it consumed. The others are independent
rational, integer-overlap or high-precision decimal oracles and do not call eyesim. Run every
registered check with:

```sh
python3 tools/r-parity/check_baseline.py --eyesim /path/to/eyesim --run-regeneration
```

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

The fitted density-space transforms `affine_transform` and `contract_transform` left the baseline by
owner decision on 2026-09-18: they belong with PCA, CORAL and CCA in that later adapter module.
`baseline.json` keeps their `fitted-density-transforms` record under `out_of_baseline`, with its
fixture and pinned eyesim evidence.

Plotting, animation, installation helpers and R-specific S3 mechanisms map to consumer integrations
or language idioms. eyes4s must expose the scientific values needed by those consumers; it need not
reproduce those mechanisms.

Measured agreements and divergences live in [PARITY.md](../PARITY.md). Migration documentation must
point to these bounded cases and cannot convert an open gap into a supported claim.

## Reference semantics that constrain parity

The pinned reference has behaviours that a native method must reproduce deliberately or record
as a divergence: nearest-cell density lookup with clamped outside queries, the fast and slow
fixation-sampling policies, L1 similarity as 1 − total variation, distance correlation and
extended Jaccard, strict overlap thresholds and their denominators, time-bin endpoints, and the
averaging of available scales before comparisons. Each is recorded with its measured agreement or
divergence in [PARITY.md](../PARITY.md), [map comparison](MAP_COMPARISON.md) and the per-ticket
manifests under `tools/r-parity/manifest.d/`. These descriptions are source inspection; none
promotes a baseline gap to a supported claim.

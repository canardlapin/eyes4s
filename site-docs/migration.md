# Migrating from eyesim

Migrate the scientific question, not the R calling convention. The reference is the eyesim
revision pinned in the project's parity manifest, `tools/r-parity/baseline.json`; agreement claims
below refer only to the pinned cases.

| eyesim task | eyes4s route | Important difference or limit |
|---|---|---|
| `fixation_group`, `eye_table`, `as_eye_table`, `coords` | `FixationCsv`, `Scanpath`, `Trials` | Explicit units, ordering and row rejection; no silent clipping or unchecked reclassification. |
| `center`, `rescale`, `normalize` | `Warp`, `Scanpath.warp` | Named source/target frames; coordinate normalization is not mass normalization. Supplied maps are verified. |
| `eye_density`, `density_by`, `get_density`, `gen_density`, `suggest_sigma` | `occupancy`, `Smoother`, `Bandwidth`, `Pyramid` | Sigma is an SD and edges are explicit. Analytic and pinned backend checks bound the evidence; the continuous/binned KDE discrepancy remains recorded. |
| `sample_density`, `sample_fixations`, `rep_fixations` | `DensityLookup`, `FixationTrajectory`, `DurationReplication` | Explicit normalization, grid indexing and endpoints. Exact integer duration replication differs from R floating-point truncation. |
| `fixation_entropy` | `Mass.entropy`, `relativeEntropy`; `FixationEntropy.occupancy`, `density`, `multiscale`; `MultiscaleEntropy` | Supplied-map, grid-method and multiscale none/mean entropy verified. The density method smooths with the native estimator, so it differs from `ks` although the entropy step agrees. Signed surfaces cannot be entropy inputs; a massless scale is refused, not dropped from a mean. |
| `Ops.eye_density` | `Mass.mean`, `difference`, `logRatio` | Named operations; subtraction is Signed. Log ratios floor zero cells explicitly. No implicit product-as-mass. |
| `similarity` | `Distribution`, `Transport`, `Lift` | Result scale and mathematical interface are explicit. The eight map methods have pinned conformance; approximate transport is not a claim of general exact EMD. |
| `scanpath`, `multi_match`, `scanpath_similarity` | `Scanpath`, `MultiMatch`, `Alignment` | Five named components and a separate EMD slot; eyesim minimum-length and unavailable-method behavior are explicit. |
| `fixation_similarity`, `fixation_overlap` | `FixationComparison`, `FixationTransportConfig` | Shared query trajectory and strict overlap threshold; entropic position/time transport retains its convergence limits. |
| `template_similarity`, `template_sample` | `StudyPlan` and typed pair designs | Keys include participant. Duplicate/unmatched keys remain data; exclusion order, multiplicity and finite caps are pinned. Native keyed RNG differs from R. |
| `repetitive_similarity` | `RepetitionPlan`, `RepetitionDesign`, `RepetitionAggregation` | Arbitrary occasions and checked persisted relations. Scale means precede comparison means; participant grouping differs from eyesim. |
| `sample_density_time` | `PointSamplingPlan` for static templates on focal trajectories | Exact query times and final-bin policy; per-query control means precede bin means. `TemporalStudyPlan` remains the separate windowed-map estimand. |
| `template_regression`, `template_multireg`, `template_similarity_cv` | Native fixed-feature QR and training-only learned mean-map recipes | Held-out changes cannot alter training. Cellwise OLS is a separate estimand; legacy R fit import is optional. |
| `affine_transform`, `contract_transform` | No fitted density-space adapter; outside the baseline, planned for a later adapter module with PCA, CORAL and CCA | Supplied coordinate `Warp`s are not substitutes for learned density transforms. |
| Result tables | `BaselineExports`, `ResultTable`, optional JVM `ArrowResultExport` | CSV plus schema/identity sidecars and Arrow preserve keys, failures and denominators. Export views are distinct from replay archives. |

## Differences to review in a migrated analysis

`eye_table` clips outside/NA positions, accepts zero durations and closed upper bounds. eyes4s
uses half-open frames and positive intervals; an invalid keyed row quarantines its trial, with
all rows retained in diagnostics. Onset/duration units must be stated. Negative duration fails
`eye_table` but is accepted by the pinned `fixation_group`; neither behavior is a reason to admit
an invalid eyes4s interval.

An image-only match can mix participants. Write both participant and stimulus projections when
that is your question. Never interpret a changed denominator or dropped match as numerical noise.
Cosine, correlation, distance and signed contrasts have different scales. A constant-map Pearson
failure is data, not a substitute zero correlation.

Start with [the four-trial example](getting-started.md), inspect admission and pair diagnostics,
then reproduce your own estimand with explicit conventions. The repository's
[measured parity report](https://github.com/canardlapin/eyes4s/blob/main/PARITY.md) and
[capability inventory](https://github.com/canardlapin/eyes4s/blob/main/docs/EYESIM_CAPABILITIES.md)
separate measured agreement, justified differences and remaining work.

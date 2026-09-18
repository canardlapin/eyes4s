# Migrating from eyesim

Migrate the scientific question, not the R calling convention. The reference is the eyesim
revision pinned in the project's parity manifest, `tools/r-parity/baseline.json`; agreement claims
below refer only to the pinned cases.

| eyesim task | eyes4s route | Important difference or limit |
|---|---|---|
| `fixation_group`, `eye_table`, `as_eye_table`, `coords` | `FixationCsv`, `Scanpath`, `Trials` | Explicit units, ordering and row rejection; no silent clipping or unchecked reclassification. |
| `center`, `rescale`, `normalize` | `Warp`, `Scanpath.warp` | Named source/target frames; coordinate normalization is not mass normalization. Supplied maps are verified. |
| `eye_density`, `density_by`, `get_density`, `gen_density`, `suggest_sigma` | `occupancy`, `Smoother`, `Bandwidth`, `Pyramid` | Sigma is an SD and edges are explicit. General eyesim KDE/backend parity remains unverified. |
| `sample_density`, `sample_fixations`, `rep_fixations` | `Surface.sampleAt` for deterministic map lookup | Random and temporal fixation/density sampling are not a complete equivalent route yet. |
| `fixation_entropy` | `Mass.entropy`, `relativeEntropy` | Supplied-map entropy verified. Signed surfaces cannot be entropy inputs; derived/multiscale inputs remain gaps. |
| `Ops.eye_density` | `Mass.mean`, `difference`, `logRatio` | Named operations; subtraction is Signed. Log ratios floor zero cells explicitly. No implicit product-as-mass. |
| `similarity` | `Distribution`, `Transport`, `Lift` | Result scale and mathematical interface are explicit. Full method-matrix conformance is incomplete. |
| `scanpath`, `multi_match`, `scanpath_similarity` | `Scanpath`, `MultiMatch`, `Alignment` | Five named components, no padded transition. Python reference evidence is not eyesim-reference evidence. |
| `fixation_similarity`, `fixation_overlap` | Study composition and comparison primitives | Exhaustive cosine study verified; full overlap/transport parity remains a gap. |
| `template_similarity`, `template_sample` | `StudyPlan` and typed pair designs | Keys include participant. Duplicate/unmatched keys remain data; finite reference sampling is not yet covered. |
| `repetitive_similarity` | `RepetitionDesign`, explicit pair/reduce operations | Within-participant reinstatement deliberately differs from eyesim phase-only grouping. |
| `sample_density_time` | `TemporalStudyPlan` for windowed duration maps | A different estimand; static-template trajectory sampling remains a gap. |
| `template_regression`, `template_multireg`, `template_similarity_cv` | Training-only fixed-feature export/fit/import | Trial-level QR route works; learned features/CV and normalized-map regression bridge remain gaps. |
| `affine_transform`, `contract_transform` | No fitted density-space adapter; outside the baseline, planned for a later adapter module with PCA, CORAL and CCA | Supplied coordinate `Warp`s are not substitutes for learned density transforms. |
| Result tables | `ContrastCsv`, temporal CSV, typed results and versioned codecs | Keep keys, method identity, failures, exclusions and denominators. Not every baseline result family is implemented. |

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

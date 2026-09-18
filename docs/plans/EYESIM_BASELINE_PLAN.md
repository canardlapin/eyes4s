# Closing the eyesim baseline epic

Plan for `bd-01M214CX2CTADNR6Q9D032Y1FB` ("Complete the eyesim baseline through coherent
public workflows"), written 2026-09-18 at main `c1d151f`. It records the planning review, the
owner's scope decisions and the execution order. Ticket notes in Mote carry the detail and the
delivery evidence; this file is the map.

## Owner decisions (2026-09-18)

- **eyesim is ours.** When a parity fixture exposes an eyesim bug, eyesim is fixed too. Fixes land
  on a local eyesim branch, the eyes4s pin moves once, and every fixture is regenerated against the
  corrected revision. Pushing eyesim upstream is a separate owner step.
- **R is never a workflow dependency.** R may generate checked-in parity fixtures offline under
  `tools/r-parity`. No eyes4s test or user workflow may require R. The current template route,
  which asks the user to run `Rscript tools/template-fit/fit.R`, is replaced by native fitting.
- **Native least squares is a small hand-rolled Householder QR** in `design`, shared by template
  fitting and the decomposition slice. `gale` enters later, for SVD-based adapters.
- **Surface decomposition is split.** The baseline keeps OLS over a predictor set (intercept,
  coefficients, signed residual, descriptive R²). NNLS, simplex weights and partial association
  move to `bd-01M2T3ZCY7HJNFQ5G0Z6MQND0A`, outside the epic.
- **Fitted density transforms leave the baseline.** `affine_transform` and `contract_transform`
  join PCA, CORAL and CCA in a later adapter module (`eyesim-transform`, outside the epic).
- **Exact EMD is an intentional divergence.** A fixture shows eyesim's exact values beside
  eyes4s's sliced W1 and Sinkhorn; no exact solver is in scope.

## Findings from the planning review

- eyesim has **no random fixation or density sampling**: `sample_fixations` and `rep_fixations`
  are deterministic trajectory evaluation (`fixations.R:52-91`). The random-sampling case is
  reworded to what it actually pins.
- The review confirmed 22 eyesim bugs in R against the pinned revision, with two more from
  reading the source. The eyesim bug-fix branch is scoped to 21 of them, three of which are
  documentation corrections; the remaining three are to be documented as intended behaviour
  (sigma meaning under MASS, point-lookup rounding and clamping, off-bounds entropy binning).
  The branch's own report records what was actually fixed.
- Four generators hard-coded the pin and hashed the whole shared case file, so any edit forced
  unrelated fixtures to regenerate. The harness work fixes both before parallel tickets start.

## Waves

| Wave | Work | Runs |
|---|---|---|
| 0 | `v-parity` harness: one archive-and-install helper reading the pin, pinned R options, frozen shared case file with per-ticket input files, per-ticket manifest fragments, count-free prose, a project-local R library with a lock file, out-of-baseline case support. In parallel: the eyesim bug-fix branch. | 2 agents |
| 0.5 | Move the pin to the fixed eyesim revision; regenerate every fixture; turn divergences caused by fixed bugs into equivalences. | serial |
| 1 | `eyesim-kde` (point evaluation first), `eyesim-sampling` (trajectory evaluator, finite matched/control sampling), `eyesim-compare` method matrix (Spearman, extended Jaccard, distance correlation, exact-EMD divergence), decomposition OLS slice with the shared QR, `bd-01KYDZ80ANFH3HW11946E4QTR8` FinalBin, `eyesim-scanpath` MultiMatch | parallel, disjoint files |
| 2 | Multiscale density and comparison (needs kde, compare); `eyesim-temporal` (needs the trajectory evaluator, point lookup, FinalBin); overlap and fixation-cloud Sinkhorn (needs the trajectory evaluator); entropy on derived inputs (needs kde); `eyesim-template` with native fitting (needs the decomposition QR) | parallel |
| 3 | `eyesim-repetition` method breadth and persistence, then `eyesim-export` | serial |
| 4 | `v-parity` closing gate: zero gaps under `check_baseline.py --eyesim --mote --run-regeneration` | serial |

Each ticket edits only its own case objects, `PARITY.md` section, capability row and migration row,
declares its identifiers in its own files, rebases before merge, runs its generator with `--check`,
then `check_baseline` and the full test suite. Every delivery gets a fresh-context review before it
merges into main.

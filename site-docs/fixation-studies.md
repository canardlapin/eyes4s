# Compare fixation maps

Use [the executable first study](getting-started.md) when you already have fixation summaries.
Keep participant, stimulus and phase in the trial key. An image-only relation compares across
participants; it does not mean within-participant reinstatement.

## Choose the estimand

`StudyPlan.cosine` estimates a map per trial, compares each focal trial with its matched reference
and explicit within-participant other-image controls, then subtracts the control mean. Binned and
Gaussian estimates remain separate labelled results. It does not pool bandwidths implicitly.

`Weight.Duration` weights occupied positions by fixation duration. `Weight.Uniform` weights
fixations equally. A Gaussian bandwidth is a standard deviation in the frame's units. Edge
truncation and source-wise edge renormalization are different estimators: choose explicitly.

## Inspect before accepting

`FixationCsv.read` returns accepted trials, every rejected row and original fields. The default
`requireComplete` refuses incomplete input. Reviewing `accepted` is an explicit analytical choice,
not permission to forget the rejection ledger. One bad keyed row quarantines its entire trial.

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
Floating-point text spelling can differ between JVM and JS; compare the declared numerical values,
not an assumed byte-identical CSV.

Next: [repetition designs](repetition.md) or [migration differences](migration.md).

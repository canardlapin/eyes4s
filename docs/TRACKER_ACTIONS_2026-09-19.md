# Actionable tracker tranche, 2026-09-19

Base: local main `131970d`. This tranche is local and uncommitted; no hosted CI,
publication or repository setting change is implied.

## Implemented work

- Detection support uses one binary timestamp-bound search in Recording, consumed
  by Detection and EventSeries. Recording extent/content hash are memoized. The
  quadratic-oracle property and half-open/large-timestamp cases pass on JVM/JS.
  [Full timing evidence](EXECUTION_RESPONSIVENESS.md) retains the initial missed
  run and the final support-search run: 60k samples meet 100 ms; that initial
  tranche left 600k outside the envelope. The follow-up below resolves it.
- Serializable epoch selectors, occurrence/final-bin policies and a conditional
  versioned codec are implemented. Exact partition, selector errors, clock/overflow,
  allocation and pinned wire/execution tests cover JVM/JS. See [epoch plans](EPOCH_PLANS.md).
- Native Householder QR and keyed OLS surface decomposition have rational and pinned
  public eyesim `lm` conformance. Fitted/residual maps are Signed. See
  [surface decomposition](SURFACE_DECOMPOSITION.md).
- The annotated benchmark has a reproducible [corpus admission gate](../tools/detector-benchmark/README.md).
  It does not yet have detector scoring or reading coverage.

## Tracker reconciliation

The roadmap, app-progress, OLS, epoch, X2, detection performance, annotated benchmark
and Security issue bodies were reconciled with source and owner decisions. X2 is
blocked/deferred pending the previously agreed consumer trigger. The standalone
Security setting is blocked by the product-browser runtime failure (sandbox-exec
TIOCSTI); authentication is valid, and no unrelated security option was enabled.
The browser ownership audits found no automated top-level browser processes.

Dependencies now connect app-progress to detection performance, eyesim-template to
the shared OLS/QR slice, and eyesim-temporal to epoch/final-bin support. The measured
long-recording residual was assigned to `bd-01M2WTSXKJWP6AS2QVHX3CW9RB`;
the follow-up below removes that blocker. Broad foundation/scientific milestones
remain open.

## Mutation sensitivity

Each mutant failed test assertions, not compilation; its original source bytes were
restored before normal checks. Receipts identify the source bytes at mutation time:

```json
[
  {
    "mutant": "support-boundary",
    "outcome": "killed-by-assertion",
    "restored_sha256": "11e085f799121d32577957ece141a1cae6046dcf00cf55c0e114524100013a4f"
  },
  {
    "mutant": "epoch-tail",
    "outcome": "killed-by-assertion",
    "restored_sha256": "ccc3935125918f08bc29a9e0bd63643b4f85b2f02067901382332b5759d2106f"
  },
  {
    "mutant": "ols-backsolve",
    "task": "lawsJVM/testOnly *SurfaceDecompositionSuite",
    "outcome": "killed-by-assertion",
    "restored_sha256": "c3e76606332c326f62bd5c34e51ba9390e84558d34495f5ebdea43a24456934c"
  }
]
```

The first full check attempt ran out of the default JVM heap during Scala.js linking.
The same CI-equivalent checks passed with `-J-Xmx4g`: 3,614 tests across JVM/JS,
headers, formatting, generated workflows and module boundaries. Final review added
an epoch-duration overflow guard; the affected JVM/JS epoch and codec tests and
static/boundary gates passed again. The mutation hash above identifies the earlier
epoch source at mutation time; the guarded version retains that tested tail logic.
Pinned OLS regeneration with `--check`, baseline/Mote verification, corpus re-admission,
corrupt-cache refusal, documentation checks and `git diff --check` pass.
No threshold or failed input was removed.

## Long-recording assembly follow-up

`bd-01M2WTSXKJWP6AS2QVHX3CW9RB` now uses immutable resumable assembly
for support validation, fixation summaries, hash/lineage, gaps, labels and reports.
Direct execution drains the same work; sample counts remain exact and named
assembly phases expose operation counts with unknown totals. Independent legacy
report/hash comparisons and dispersion formulas preserve results and error order
across independent sample/assembly quanta. Barrier tests cancel in every phase
without a partial artifact and with exactly one detector flush. The class-duration
plus-one mutant was killed by assertions; the original source bytes were restored.

The unchanged 600k recording meets the 100 ms gate at default and smallest quanta
on M3 Max / OpenJDK 25.0.1. Maximum step/gap/cancellation values are
63.237/48.047/15.311 ms and 46.100/31.641/15.710 ms, respectively.
The [complete report](evidence/detection-assembly-final.md) records measured and
final source digests, unchanged JVM flags, and retained stress-case failures.
Only the observation cap grew, so all 5,235,966 steps at quantum one are observed.
The wider Gaussian envelope and supported-JDK hosted qualification remain outside
this claim. Changes remain local and uncommitted.

Final local verification passed: 3,622 tests across 24 JVM/JavaScript module
totals, plus `headerCheckAll`, `scalafmtCheckAll`, `scalafmtSbtCheck`,
`githubWorkflowCheck` and `checkBoundaries`, using `sbt -J-Xmx4g`.
The deliberate duration mutant failed assertions and was restored before this run.

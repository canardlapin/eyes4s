# Mutation receipts, 2026-09-19

Receipts of the deliberate mutants run for the detection-support search, epoch
selectors and native OLS slices (base: local main `131970d`). Moved here from the
retired tracker log of that date; the long-recording follow-up is recorded in
[detection-assembly-final.md](detection-assembly-final.md).

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

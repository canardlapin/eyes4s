# Reliability and interaction performance tranche

The owner authorized five Motes: saved-part retention, test pointer isolation,
deterministic renderer-failure capture, default fork-handshake reconciliation,
and named-machine interaction performance. The implementation uses a separate
checkout; the shared primary branch and files were not moved or cleaned.

## Changes and safeguards

- Saved-part collection runs only after the new manifest is committed and its
  journal reset succeeds, under the session mutex and writer lock. It considers
  only parts named by the retired previous manifest and verifies the current,
  previous and retired bundles before deletion. Current/previous generations,
  unknown files, inputs and result archives are preserved. Archived journals pin
  every generation. Any uncertain read defers collection. Deletion failures are
  warnings on a successful save; recovery remains available.
- The FX harness consumes pointer events that did not originate inside its owned
  robot dispatch. The filter belongs to the scene, survives root replacement and
  records consumed events. Suites can explicitly opt out. This is scene event
  isolation, not an OS pointer grab; separate popup scenes remain outside it.
- A controlled exception on the actual QuantumRenderer queue proves that the
  renderer's printed failure reaches the harness verdict with its original cause
  and stack. A second queue fence makes the check deterministic.
- Default fork-handshake evidence is reconciled from contemporaneous receipts
  and full gate logs. Pre-fix passes, interrupted runs and focused probes do not
  count. Historical environment-attribution limits remain explicit in
  `fork-handshake-review.json`.
- Performance uses the production stored-project/backend path and all public
  fixture rows at normal and tenfold size. The final 900 measured interactions
  pass the original p95 budgets. Numerical results, uncertainty and excluded rows
  were not simplified to make the UI fast.

The performance work uncovered and corrected quadratic duplicate-key checks,
repeated unchanged projections, unbounded native row construction, a blocking
save-clock call, preview invalidation during draft promotion, and report calls
blocking effect workers on a shared lazy digest. The final rendering bottleneck
was repeated clipped painting after temporary dock detachment. The host now
retains its painted frame through detachment and rebuilds for real size, scale
or source changes. Transparent sampling guards and plot-internal clipping are
preserved. Fault mutants and negative timing runs are retained beside the fixes.

## Evidence

[PERFORMANCE.md](../../PERFORMANCE.md) records the named machine, exact measured
source, each interaction budget and memory. `performance/` contains all raw
observations, process activity and twenty screenshots; `performance/visual-review.json`
records the ten perspective inspections. Earlier failed measurements and
profiles are preserved in `baseline/`, `corrected-*` and diagnostic folders.

`reviewed-scrollbar-baselines/` records the separately reviewed light/dark
Compare image changes caused by replacing the participant ScrollPane with a
virtual ListView. No pixel tolerance changed. Source-bound [qualification](qualification/qualification.json) records the complete
local release pipeline (5,315 Studio case passes), hosted Linux pixel
goldens, native GTK at scale 2, native macOS, and the JDK 17/21 library checks.
The qualified candidate is `a74f86b758653de67235ea0f0d9e4ca0652910fd` (tree
`40e578eccc1a5aa1953bcb7c698734d039c527d7`). The performance-measured production code, benchmark and fixture are unchanged;
the later test-only native-scale, focus-audit and Figures/Compare theme corrections are recorded separately.

## Limits

The tenfold workload completes analysis but explicitly exceeds the unchanged
20-million-density-cell archive budget. Its interactive in-memory results are
qualified; reopening that oversized archive is not. The normal fixture stores
and verifies its archive. G2 acquisition provenance and production-study
acceptance, physical Retina/VoiceOver, packaging, publication and researcher
usability remain separate obligations. Passing this tranche does not close G5.

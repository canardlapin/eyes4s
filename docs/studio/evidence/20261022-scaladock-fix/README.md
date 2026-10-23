# Scaladock restore fix and Studio integration — 2026-10-22

The provider fix is merged in [Scaladock PR 4](https://github.com/canardlapin/scaladock/pull/4). The merge, source pin and hosted merge-ref runtime probe share tree `0422bfa4b501ea106d76e7ed81de716af2921560`. Mainline source commit is `085740fc03d935e6945f8f3a595abc291bcdcb13`, an ancestor of upstream main.

Binding reuse now checks nominal pane type identity for both attached and retained panes. Unresolved views also require their original raw type/state record. Live-state refresh uses the same compatibility check, so an inactive layout cannot lose its saved payload to another type with the same pane ID.

The expanded provider suite was run against the old implementation: 10 failures in 24 tests. With the fix, all 24 pass. The full provider gate passes strict compilation, formatting, 70 core tests and 117 JavaFX tests. Hosted Linux/macOS, JDK 22 with JavaFX 24, and coherent named-module JavaFX 27 all pass. Logs, lossless XML reports and the hosted runtime manifest are attached with hashes.

Studio removes the detach-before-restore and independent-capture workarounds. Its 14 focused provider/layout tests pass, including capture of an inactive unknown pane and restore back to a known pane. Full application/library qualification is recorded separately after it completes; the focused run alone does not qualify the dependency upgrade.

No study geometry/timing provenance or scientific comparison acceptance is claimed by this software fix.

## Narrow Studio backport

The full current-upstream pin candidate `03f1162f` was rejected by seven macOS accessibility assertions: dock-header tabs/buttons entered traversal beyond Studio's derived order, and Compare actions had ambiguous names. The raw log is preserved. Its API audit passed with zero gaps; the remaining local gate was stopped when the candidate was superseded. No complete qualification or landing is claimed for that attempt.

Studio therefore pins `b83a7ecb90529184bd75eb1ad0b322af73ec065f`, a clean backport of the same restore patch and thirteen regressions onto `628c46f`. Later theme, focus, motion and keyboard changes are excluded. The broader adoption remains open as mote `bd-01M5K8EKNNXPCZJ2PZ198CTDVV`; assertions and the existing keyboard contract were not weakened. The backport passes full local qualification (66 core and 65 JavaFX tests), hosted Linux/macOS/JDK22+JavaFX24 checks, and 87 focused Studio checks with the existing accessibility assertions unchanged. It is merged via upstream PR5 on the Studio compatibility branch. Final application qualification is recorded separately.

## Final qualification

The scoped source candidate `58ee62cd08ae062c1e3ab1edbd9f7951dd5e3133` (tree `fe3dd5e20740895bba07fc78b9e2f1ddd6c5acd9`) passed the full canonical landing gate, strict Studio/boundary/style checks, all 24 E2E02–25 scenarios, checker tests, site and documentation/isolated-consumer verification. Hosted Linux headless, native GTK2, macOS functional and clean-clone Studio jobs and all hosted library checks passed for that same source. Named report hashes and actual successful cases were checked from the downloaded artifact. See [qualification.json](qualification.json) and the local/hosted E2E receipts. The broader provider-adoption mote remains open.

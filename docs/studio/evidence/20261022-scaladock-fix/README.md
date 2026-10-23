# Scaladock restore fix and Studio integration — 2026-10-22

The provider fix is merged in [Scaladock PR 4](https://github.com/canardlapin/scaladock/pull/4). The merge, source pin and hosted merge-ref runtime probe share tree `0422bfa4b501ea106d76e7ed81de716af2921560`. Mainline source commit is `085740fc03d935e6945f8f3a595abc291bcdcb13`, an ancestor of upstream main.

Binding reuse now checks nominal pane type identity for both attached and retained panes. Unresolved views also require their original raw type/state record. Live-state refresh uses the same compatibility check, so an inactive layout cannot lose its saved payload to another type with the same pane ID.

The expanded provider suite was run against the old implementation: 10 failures in 24 tests. With the fix, all 24 pass. The full provider gate passes strict compilation, formatting, 70 core tests and 117 JavaFX tests. Hosted Linux/macOS, JDK 22 with JavaFX 24, and coherent named-module JavaFX 27 all pass. Logs, lossless XML reports and the hosted runtime manifest are attached with hashes.

Studio removes the detach-before-restore and independent-capture workarounds. Its 14 focused provider/layout tests pass, including capture of an inactive unknown pane and restore back to a known pane. Full application/library qualification is recorded separately after it completes; the focused run alone does not qualify the dependency upgrade.

No study geometry/timing provenance or scientific comparison acceptance is claimed by this software fix.

## Narrow Studio backport

The full current-upstream pin candidate `03f1162f` was rejected by seven macOS accessibility assertions: dock-header tabs/buttons entered traversal beyond Studio's derived order, and Compare actions had ambiguous names. The raw log is preserved. Its API audit passed with zero gaps; the remaining local gate was stopped when the candidate was superseded. No complete qualification or landing is claimed for that attempt.

Studio therefore pins `b83a7ecb90529184bd75eb1ad0b322af73ec065f`, a clean backport of the same restore patch and thirteen regressions onto `628c46f`. Later theme, focus, motion and keyboard changes are excluded. The broader adoption remains open as mote `bd-01M5K8EKNNXPCZJ2PZ198CTDVV`; assertions and the existing keyboard contract were not weakened. The backport passes full local qualification (66 core and65 JavaFX tests), hosted Linux/macOS/JDK22+JavaFX24 checks, and87 focused Studio checks with the existing accessibility assertions unchanged. It is merged via upstream PR5 on the Studio compatibility branch. Final application qualification is recorded separately.

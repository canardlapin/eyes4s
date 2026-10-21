# Scaladock upstream restore check — 2026-10-21

Upstream main `4a7a61bb397a68d5048f809a6ea1c30ce8b45223` still reproduces both defects tracked by mote `bd-01M5FGA2BND8FVYZ9ZM8NDREG1`.

- An already attached pane whose id is unchanged but type becomes unknown retains the old live view instead of creating an unresolved placeholder.
- `Perspectives.save()` refreshes an inactive layout from a live binding with the same id even when their types differ, losing the unknown type and payload.

The two regression assertions fail; two positive controls for Studio's detach-before-restore and per-layout capture workaround pass. All eleven existing `PerspectivesSuite` tests pass. Expected total: 15 tests, 13 passed, 2 failed, no errors. Failure is the finding; this is not a passing upgrade qualification.

The relevant `Dock.reconcile` and `Dock.withLiveState` logic is unchanged from Studio's current pin `628c46fce92c4d31b0b08459ac0f18777b5ba527`. `Perspectives.scala` is unchanged in full. New upstream changes cover presentation, accessibility, motion and focus recovery.

## Reproduce

Clone Scaladock at the revision above. Copy `probe.sbt.txt` to `probe.sbt`; copy both `*.scala.txt` probes to `modules/fx/src/test/scala/scaladock/fx/` without the `.txt` suffix. On JDK 25, run:

```sh
sbt -batch 'fx/testOnly scaladock.fx.RestoreTypeRegressionSuite scaladock.fx.PerspectivesSuite'
```

The temporary settings add Monocle headless rendering. The probe takes Studio's machine-wide FX lock before starting tests. No real desktop windows open. No upstream production code was modified and no artifacts were published. This probe uses JavaFX on the classpath; named-module, native-output-scale and whole-application upgrade qualification were not performed.

The current Eyes Studio provider pin and workaround are retained. The follow-up mote stays open. Raw output, losslessly gzipped named XML results and hashes are included here. XML is compressed to retain the exact raw report bytes, including generator whitespace.

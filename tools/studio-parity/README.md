# Whole-board review receipts

`BoardParityCaptureFxSuite` opens each application board's named story moment,
waits for served content, reaches the reference's keyboard stop, and captures
the actual JavaFX window. `SystemSpecimenFxSuite` captures a test-only native
component sheet; System is not an application perspective. Existing component
tests and the application moments cover its remaining states and behaviours.

The review lives under `docs/studio/evidence/20261023-parity/`: a side-by-side
index, a per-board checklist disposition, exact source/reference/capture hashes,
native shell measurements and named successful test cases. These are semantic
and measured comparisons, not browser-versus-JavaFX pixel equality claims.
Local 2x files are snapshot transforms; they are not native HiDPI evidence.

After the full Studio gate, verify the reviews against this checkout's XML:

```sh
python3.13 tools/studio-parity/check.py docs/studio/evidence/20261023-parity \
  --output target/studio-parity/receipt.json
```

Verification refuses missing boards/checklist items, changed inputs or captures,
wrong shell measurements, unresolved dispositions, and failed or skipped named
tests. A changed source or capture needs another visual review before updating
its hash. Hash collection is not visual review. Qualification and landing are
recorded separately; these reviews make no private-data G2, physical Retina or
VoiceOver acceptance claim.

`python3.13 tools/studio-parity/test_check.py` exercises refusal of changed images,
incomplete checklists, skipped/failed/duplicate named tests, rehashed wrong shell
metrics, unreviewed records and unjustified deviations.

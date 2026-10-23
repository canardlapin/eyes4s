# Whole-board parity review, 2026-10-23

The [side-by-side](index.html) compares eight remaining reference boards with actual
JavaFX captures. Each board's `review.json` records all of its checklist items,
source/reference/capture hashes, exact native cases and individual deviations.
There are 51 dispositions. A checked checklist item means reviewed, including
explicit deviations; it is not a claim that every illustrated control is present.

The concrete repairs are selected-result scale following, served Compare maps and
independent overlay controls, captions that wrap at ordinary pane widths, physical
figure columns and tiles, caption-free figure gaze viewports, explicit Analysis
`n/a`/`no value` states, content-aware raster-cache reuse and visible/retryable
Compare raster failures. Map reading, identity checks, display limits and overlay
choices live in portable studio-app; the JavaFX adapter performs reads and binds
view-models. No density or comparison is computed by Studio.

The native capture port verifies actual public fixture bytes against their recorded
digests. Data is t1, Results t3, the other application screens t2. System is a
component/documentation board, so its native sheet uses real tokens, controls and
Intaglio trial scenes, with real application captures/component tests for remaining
states. It is not an extra application perspective.

Native captures use 1440×900 logical pixels and output scale1; `2x` files are snapshot
transforms. Metrics measure 44/32/24 px app/context/status rows and 28 px dock tabs.
This is no physical Retina, VoiceOver or private-study G2 acceptance. Only public
synthetic fixtures are included. Research plots and private dataset outputs were
not changed.

Main/Figures `before-*` captures are intermediate regression evidence, taken after
the scale/map repair but before the narrow-caption and figure-column repairs. They
are not untouched main baselines. Red regressions are retained under `regressions/`;
the final named cases pass. The export bounds regression also caught a transitional
error during this work: the old caption-band height compensation must be removed
when the gaze scene has no caption band. The final SVG golden changes only the two
viewport clip rectangles; drawings, values, page dimensions and PDF text stay the
same. Native font and export embedding are checked by their own tests.

The first strict Studio attempt found stale keyboard-stop expectations. The second
passed all portable tests but failed the SVG golden and two duplicate accessible-name
checks. Query/reference names now differ, the viewport correction was inspected,
and those focused regressions pass. A browser reference verification had one
transient repeated-frame mismatch at Results2x; a fresh process reproduced all18
committed PNGs and the manifest exactly. Failed attempts are not qualification.

One functional gap is explicit: the Results Participants navigator is still a generic
empty placeholder. The participant plot and tables contain the actual24participants.
Follow-up `bd-01M5N6CCJ42414CZSEX6B2CMTR` covers the dedicated navigator, exact served
cell identities, keyboard activation and native accessibility. Other deviations
include the documented generic form/scroll layout, actual fixture art, undeclared
trial extents and historical fake-backend data unavailable rather than invented.

Run `python3.13 tools/studio-parity/test_check.py` for evidence refusal checks, and
`python3.13 tools/studio-parity/check.py docs/studio/evidence/20261023-parity
--output target/studio-parity/receipt.json` after the full native gate. The verifier
refuses changed captures, missing checklist items, invalid shell bounds and failed,
skipped or ambiguous named tests. It does not substitute for visual inspection.

[qualification.json](qualification.json) records source/tree identity and full-gate
results. Motes close only after the qualifying source/evidence lands.

Candidate `24ad6d55` was deliberately stopped during its API audit when review
found a pre-existing cached-raster correctness gap. Pending requests checked grid
content; cache hits did not. A failing regression requested different cells under
one nominal key and received the old cached image. The corrected store checks the
same content fingerprint on held rasters and renders changed contents anew, pruning
fingerprints with evictions/invalidation. A native refusal/Retry regression checks
visible reasons and unchanged source cells. This stopped attempt is not qualification.

The hosted native-GTK2 run for `9d93ff26` failed the new narrow-caption test’s
readiness wait: it required a1× frame despite the actual2× Stage. The artifact
showed a drawn map and the independent scale receipt recorded2×2, no override,
a one-device-pixel line and zero raster mismatch. The test now follows the actual
native scale and also checks explicit1×/2× render contexts, retaining every text,
bounds and non-overlap assertion. The corrected test passed the fresh complete canonical/strict Studio gate and hosted nativeGTK2 run. No production code changed.

The S10.4 parent remains open for its additional complete light/dark JavaFX
regression-golden matrix; these per-board semantic reviews do not claim that
unimplemented acceptance criterion.

Final qualification covers source `2753627d887cac61191b82af28e6b26f1a51fc3d`,
tree `064afa2dc657e510c132239cf77424e9e086f030`. The full canonical gate, strict
Studio, all nine supplemental checks and final strict compilation/boundaries/API
consistency passed. Studio passed 4,363 portable and 812 desktop cases; the private
acceptance case remained opt-in. All 24 E2E catalogue scenarios have executed
passing bindings. The API audit found zero uncovered entries.

Hosted run [40024842869](https://github.com/canardlapin/eyes4s/actions/runs/40024842869)
passed macOS, clean-clone pins, native GTK2 and headless checks. The first headless
attempt skipped Figures after transient window-focus loss; its receipt is retained
and it is not counted as a pass. The same-source second headless attempt was
downloaded by its exact artifact ID and every required board case was verified as
executed and passed. The original artifact is retained separately.

The [hosted review](hosted-review.json) records artifact IDs and XML hashes, actual
Stage scale 2×2 without a host override, a one-device-pixel feedback stripe, and zero
analytic raster mismatch. [Main at actual GTK2](Main/hosted-native2.png) and the
[independent stripe](System/hosted-native2-stripe.png) were visually inspected. These
are distinct from the local 2× snapshot transforms.

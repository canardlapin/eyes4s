# Whole-board parity review, 2026-10-23

The [side-by-side](index.html) compares eight remaining reference boards with actual
JavaFX captures. Each board's `review.json` records all of its checklist items,
source/reference/capture hashes, exact native cases and individual deviations.
There are 51 dispositions. A checked checklist item means reviewed, including
explicit deviations; it is not a claim that every illustrated control is present.

The concrete repairs are selected-result scale following, served Compare maps and
independent overlay controls, captions that wrap at ordinary pane widths, physical
figure columns and tiles, caption-free figure gaze viewports, and explicit Analysis
`n/a`/`no value` states. Map reading, identity checks, display limits and overlay
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

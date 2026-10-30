# Eyes Studio overhaul: the frozen v4h golden

The 2026-10-29 redesign of Eyes Studio, reviewed in a scored hill climb by six critics (usability, aesthetic,
JavaFX UI engineering, product design, scientific fidelity, and a Tufte lens): native app v0 5.11 → v4e 8.11 (mean of
6 critics × 9 dimensions). Progress, captures and every critique: https://claude.ai/artifact/5Kq5evzPYgEjnjSwmCMHC5 (private).
Tracker: epic bd-01M653SAZ8MZQKSXER2W0RD1NN (slices for each port step).

- `mockups/`: the frozen v4h pages (run `mockups/assets.sh` once, then open any page in a browser). They use only layouts,
  CSS and drawing that map to JavaFX 24, scaladock and Intaglio, and the fixture data in `docs/studio/fixture`.
- `golden/`: Chromium renders (1440×900 @1x) and region maps (panes, text runs) of the ten specimens. The port matches these.
- `parity/`: `golden.js` (renders and maps a build) and `parity.js` (judges a native capture against the golden);
  thresholds and positive/negative controls in `parity/README.md`.
- `spike/`: the JavaFX vs Chromium fidelity measurement the thresholds come from (`RESULT.md`).
- `DECISIONS.md`: the owner's decisions (map ramp, study-wide state button, cancel confirm, navigator, inspector,
  docking, strip, v4) and the decision-3 amendment.
- `PORTING_MAP.md`: the UI-engineering critic's final map (element → JavaFX/scaladock/Intaglio construct → risk) and
  per-region parity thresholds.
- `LIBRARY_BUGS.md`: defects isolated on the way (Intaglio #30, #31, #32 filed; scaladock header buttons mouse-only in the pin).
- `FROZEN.md`: hashes of the frozen build and golden.

Structure: four perspectives (Data · Analysis · Results · Report). Explore is dissolved: Results drills pair → trial →
fixation → record in place, and Data › Trials browses without a result. Figures became Report (figures, methods, bundle).

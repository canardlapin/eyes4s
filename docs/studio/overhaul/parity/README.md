# Parity harness: the JavaFX port against the frozen v4h golden

1. `golden.js <build> <out> page…` renders each mockup page in Chromium at 1440×900 @1x and writes `<page>.png` and
   `<page>.regions.json`: every pane (toolbar, trail, status bar, `.pane`) and every text run (from DOM text nodes).
   The golden lives in `golden-v4h/` (hashes in ../FROZEN.md).
2. The port captures the same specimen states natively (extend `BoardParityCaptureFxSuite`: same story moments, 1440×900 @1x,
   headless Monocle), writing `<page>.png` with the golden's names.
3. `parity.js <golden> <native> page…` judges each pane and text run; exit 1 on any failure.

## Thresholds (from spike/RESULT.md: JavaFX 24 vs Chromium on identical geometry)

| what | rule |
|---|---|
| non-text pixels, per pane | mean Δ ≤ 2.0 and pixels with Δ > 48 ≤ 0.5% |
| flat colour (golden pixel equal to both neighbours along a row or column: fills, borders, hairlines) | native within Δ 3 on ≥ 99.8% |
| text runs (pixels inside text boxes are excluded above) | ink-box left edge exact; top/right/bottom ±1 px; mean ink darkness ±8% |

Δ is the max channel difference. Text is judged by geometry because glyph anti-aliasing differs between the renderers
(13% of text pixels Δ > 16 in the spike) while layout matches (ink boxes within 1 px over 300+ px lines).
Not covered yet (measure when the port reaches them): Intaglio's own smooth-raster path, 2× output scale, dark-theme stages.

## Controls (all must hold before the harness is trusted)

| control | expected | result |
|---|---|---|
| golden vs itself | PASS | PASS (Δ 0) |
| real JavaFX render of spike geometry vs its Chromium golden | PASS | PASS (mean Δ 0.33, flat-miss 0.1%, 14/14 text runs) |
| hairline and surface-3 tokens shifted (#DEDAD2→#CFCAC0, #E9E6E0→#E2DED6) | FAIL | FAIL (flat-miss 9.85% toolbar, 3.33% trail, 0.21% queries) |
| pane titles shifted 1 px | FAIL | FAIL (left edges, 4 runs) |
| pane titles 600 → 500 | FAIL | FAIL (darkness 13%, 8 runs) |

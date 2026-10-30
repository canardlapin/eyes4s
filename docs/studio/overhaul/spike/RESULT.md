# Fidelity spike result (2026-10-29)

Same geometry drawn by Chromium (spike/spike.html) and headless JavaFX 24 / Monocle software pipeline
(clone: studio/desktop/src/test/.../harness/FidelitySpikeFxSuite.scala, throwaway), 800×460 at 1x, diffed per region
(tools/diff.js; ink boxes tools/inkbox.js). Max channel difference per pixel.

| region | mean Δ | pixels Δ>16 | pixels Δ>48 | verdict |
|---|---|---|---|---|
| density raster 64×48 → 360×270, smooth (ImageView smooth=true vs SVG image-rendering:auto) | 0.93 | 0.35% | 0 | match |
| 1 px hairline at y+0.5 | 0.01 | 0.06% | 0 | match |
| cased stitched polyline (inside raster region) | (in raster) | | | match |
| 6 px rounded button, 1 px border | 3.51 | 5.3% | 2.3% | border/corners match; difference is glyph AA |
| Plex 11–15 px, 400/500/600 | 11.6 | 13.2% | 8.6% | layout matches; AA differs |
| Plex 34 px 500 | 5.0 | 7.0% | 4.2% | layout matches; AA differs |

Text geometry (ink bounding boxes, 14 rows): left edges identical, right edges within 0–1 px over 300–385 px lines,
top/bottom within 1 px; mean ink darkness JavaFX 0–6% lighter (gray AA, different gamma).

Conclusions for the parity harness:
- Raster, strokes, fills, borders, radii, hairlines: pixel parity is realistic. Threshold: mean Δ ≤ 2, pixels Δ>48 ≤ 0.5% per region.
- Text: judge geometry, not pixels. Threshold: ink box edges ±1 px per text run, ink darkness within ±8%; pixel Δ excluded inside text masks.
- Not yet measured: Intaglio Raster(Smooth) path (spike used ImageView); 2x output scale; LCD smoothing (unavailable in snapshot);
  the macOS glass (only Monocle software pipeline measured).

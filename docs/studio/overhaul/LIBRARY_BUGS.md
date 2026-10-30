# Library defects isolated during the visual overhaul

| # | library | defect | evidence | status |
|---|---|---|---|---|
| 1 | Intaglio | Cased Grob.Segments render dashed (casing per segment overpaints previous ink) | audit/IsolineCasingReproSuite.scala; audit/isoline-repro.svg; widths alternate 2 1 2 1 (39 runs / 41) | filed canardlapin/intaglio#30 |
| 1b | Intaglio | #30 follow-up: translucent casings compound alpha at shared endpoints (beads), so paint order alone does not fix it; join connected runs | v2-uiexpert; verified by mechanism | comment on #30 |
| 2 | Intaglio | RasterImage.equals scans every pixel on cache hits (no `eq` shortcut); hit path in JavaFxCanvasContext image cache | Raster.scala:111-122, JavaFxCanvasContext.scala:26/212 (read by builder) | filed canardlapin/intaglio#31 |
| 3 | Intaglio | JavaFX backend resolves Font.font(family, weight) to Regular for every weight with static faces; Medium/SemiBold labels draw Regular on screen, correct in SVG | measured in headless JavaFX 24 (FidelitySpikeFxSuite, throwaway); JavaFxCanvasContext.scala:171-177, 256-265 | filed canardlapin/intaglio#32 |
| 4 | scaladock | pane header buttons (pop-out, minimise, maximise, close) and the minimised rail are StackPanes with setOnMouseClicked only: no focus, no Enter/Space, no FIRE action | scaladock-fx views.scala:76-118 (pinned b83a7ec) | not filed: may be fixed upstream; tracked by mote bd-01M5K8EKNNXPCZJ2PZ198CTDVV (adopt current scaladock keyboard/a11y contracts) — check the current scaladock first |
| - | Intaglio | v0 blocky map cells: app chooses RasterInterpolation.Nearest (TrialScene.scala:846); related open issues #26, #22, #14 | v0-uiexpert | app-side choice; no new issue |
| - | scaladock | none found in v0 (all docking chrome renders per dock.css) | v0-uiexpert, v0-usability | — |

| - | Intaglio (feature gap) | no image colour-effect hook; desaturated stimulus must be computed app-side (RasterImage.tabulate) | v3-uiexpert | not filed (feature) |

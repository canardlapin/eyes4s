# Design review record (2026-10-11)

The boards in this folder were revised in a scored review before landing: baseline v0 6.34 → v1b 7.53 →
v2 7.94 → v3 8.09 (blocked: invisible table focus) → v3b 8.28 of 10, accepted. Four independent reviewers
scored each round: visual design, function and accessibility, science fidelity, and JavaFX/Intaglio
buildability. A browser gate checked every round (render errors, unresolved values, keyboard focus visible on
every tab stop, occluded or clipped text, wraps, off-scale type and weights, accent use, literal colours, and a
JavaFX portability lint). The owner's decisions are in `../DESIGN_SPEC.md` §15; the boards' open differences
from the desktop are listed at the end of `../PARITY_CHECKLIST.md`.

- `MAP_RECIPE.js` is the map recipe every board pastes verbatim: the desktop's ramp, the 10% alpha floor,
  marching-squares isolines at 50% and 90% of mass, the legend, and the cross and mark casing.
- The ticket list below carries file:line anchors at commit 9431e120.

## Landing tickets (ordered)

Status key: **[approved]** owner-approved · **[bug]** a pure bug, landable without sign-off · **[board]** board-only edit (done before landing). Every ticket below is owner-approved (2026-10-11) as amended in DESIGN_SPEC §15; items marked *Decision* record how an open option was settled.

**A. Board edits before landing (no code)**
1. [board] Figures panel C: control caption as "controls ·" / "street-112" / "0.61" (three rows), or record F2's explicit split. Data: caption "Missing asset · forest-044.png".
2. [board] Explore: drop the chip text "Map: preview · σ 2° · not a result".
3. [board] Main "Edit in Analysis ⌘3" weight 400. Data thumbnail dash `82 55`, width 14 (6/4 px at 0.073×).

**B. Pure bugs (code; land first, small PRs)**
4. [bug] Mono SmBld → "IBM Plex Mono Medm" at `studio-navigator.css:131` and `studio-ledger.css:99`. Test: a TypeCss/font-family lint that every family named in CSS is loaded. **Fixed.**
5. [bug] `.design-chip:focused` (`studio-design.css:63`) replaces the `:selected` fill (`:53`). Write the compound `.design-chip:selected:focused`. Test: `ResolvedDesignFxSuite` snapshot. **Fixed.**
6. [bug] Greyscale proof: `ColorAdjust` (`FiguresHost.scala:396`) gives max(R,G,B), so match and control print the same grey. Replace it with a Rec. 709 luma render. Test: a new luma check that the match and control greys differ. **Fixed.**
7. [bug] Plural: "1 records inside" (`GeometryText.scala:105`; `GeometryPanelSuite:461`). Add a singular form. **Fixed.**

**C. Owner-approved**
8. [approved] Map alpha floor (SPEC-AMENDMENTS #1, #10). **Fixed** (uncovered cells are one compound shape, so row seams are opaque at every size and scale, bd-01M4TCR2EWPSX1NG4R686MY3M7; figure tiles fade in but do not hatch).
   - `MapColours.argb` (`MapPalette.scala:164`): alpha = min(1, t/0.10), for mass maps only.
   - Missing cells draw the missing-asset hatch (never transparent, never a ramp colour), with a "{n} cells without coverage" TrialText id in the caption.
   - Tests: `PaletteSuite` (~:36-59, :150), `TrialMapSceneSuite:118, :295-320`, `MapTileSceneSuite:81-96`, `TrialViewMapFxSuite`.

**D. Tokens and type (app) **
9. `ThemedToken.SelFill` (light #E4E0D6, dark #2E3035) in `Tokens.scala` (ThemedToken :36-72, values ~:269/:300 blocks). Tests: `TokenContrastSuite` (text pairs, plus hover surface-2 vs sel-fill), TokenCss golden. **Fixed.**
10. TypeCss `.t11-strong`, `.t12-strong`, `.t12-label`, `.mono-500`. Test: a TypeCss unit test; `checkStudioTypeScale` is unaffected. **Fixed.**
11. NeutralMark: *Decision* — token alpha .35 (the Explore board is aligned); was opaque ink-3 vs alpha 35 (`Tokens.scala:269, :300`); `HaloPx` 2.0 vs spec 1.5 (`TrialScene.scala:411`); DESIGN_SPEC.md:78. Tests: `TokenContrastSuite`, `TrialSceneSuite` neutral marks, `GeometrySceneSuite:41`. **Fixed** (the 1.5 px halo applies to neutral marks only; role marks keep 2 px, DESIGN_SPEC §13).

**E. Desktop CSS sweep (desktop; one PR) **
12. **D1, composite cursor.** **Fixed** (row-level accent frame at `:focused` until ticket 14; the design table's container ring also removed; the pane-placeholder ring is unchanged).
    - Remove the container rings: `.nav-rows:focused` (`studio-navigator.css:78-81`), `.table-twin:focused` (`studio-plot.css:59-61`), and modena `.list-view:focused`/`.tree-view:focused` (override in `studio-shell.css`).
    - Cursor = row-level layered background `-es-accent, <fill>` at insets 0, 2:
      - `.table-twin:focus-visible .table-twin-row:cursor` replaces `studio-plot.css:124-127`;
      - `.records-row:cursor` (`:247-250`) is scoped to the container's focus;
      - `.design-row:cursor` (`studio-design.css:151`);
      - `.list-cell:focused` for FocusModel lists;
      - compound `:cursor:selected` → `-es-accent, -es-sel-fill`.
    - Not per-cell (no `:first-child` in JavaFX).
    - Tests: re-baseline `AppShellFxSuite`/`ShellMomentsFxSuite`; `TableTwinViewFxSuite`, `SourceRecordsFxSuite` (cursor state unchanged); `A11yTreeSuite`.
13. **D2, selection and hover.** **Fixed.**
    - `-fx-selection-bar[-non-focused]: -es-sel-fill`, `-fx-cell-hover-color: -es-surface-2` (`studio-shell.css:41-44`), and `.list-cell:filled:hover`.
    - Fold the accent-soft fills (`studio-navigator.css:89`, `studio-ledger.css:66,162`, `studio-explore.css:49,58`, `studio-timeline.css:44`, `studio-design.css:53`, `studio-figures.css:43-45`) and `.table-twin-row:selected` ink rules (`studio-plot.css:108-111`) into sel-fill + SmBld.
    - Tests: FX snapshot re-baseline.
14. **D3, focus-visible.** **Fixed** (slice b: canvas focus rings show only after Tab or a roving key since focus arrived, `MarkInputAdapter.showFocus`; slice a: `:focus-visible` on every control rule and composite cursor; primary ring accent / on-accent / accent at 0, 2, 4; `:armed` → hairline; a composite pane draws no ring while its rows carry the cursor; a focused table puts its cursor on its home row, the first selected, else the first; only a composite with no rows rings its container (`CompositeFocus`); a canvas pane rings only for keyboard focus, `:keyboard-within` set by `MarkInputAdapter.showFocus`).
    - `:focused` → `:focus-visible` on ~32 control rules.
    - Canvas rings gated on focusVisible OR keyboard-since-focus (Issue 1): `MarkInputAdapter.scala:137` listens on `focusedProperty`, and `TrialInputAdapter.focusChanged` is at `:75`.
    - Primary focus `-es-accent, -es-on-accent, -es-accent` at insets 0, 2, 4 (`studio-plot.css:316`, `studio-geometry.css:154`, `studio-ledger.css:216`, `studio-import.css:192`).
    - `:armed` → `-es-hairline`.
    - Tests: FX focus tests use `requestFocusTraversal(TraversalDirection.NEXT)` or a robot Tab, not `requestFocus()` (verified: focusVisible = false). This touches `InputAdapterFxSuite:402` and any ring assertion.
15. **D4, disabled.** *Decision:* keep the desktop rule — pending and blocked actions are disabled but fully readable (`PreflightFxSuite:55-100`, the readable overrides stay); the boards were amended to match. Remaining code: attach each visible reason to its control for assistive technology (accessible help text), with an `A11yTreeSuite` check. Only controls with no workflow meaning fade to 0.4. **Fixed** (`DisabledReason.attach`; `A11yTreeSuite`).
16. **D5, controls** (split into slices of at most half a day: the controls CSS, then the `SegmentedBar`). `studio-controls.css` for radio, checkbox and slider, and a `SegmentedBar` replacing the RadioButtons at `CompareInspectorHost.scala:114-116`. Test: a new FX snapshot. **Fixed** (`studio-controls.css` for radio, check box and slider; the `SegmentedBar` replaces the Compare inspector radios).

**F. Scenes (viz) **
17. **E1, TrialScene.** **Fixed** (S10: cross above the map, cased; inner casing only where 3:1 needs it, derived by `StageCasing`; round isoline caps. S11: `MapLegendScene` under the trial, Explore's keys in its legend row; difference maps have no legend).
    - Cross after the map, 2 px halo with a 4 px casing (`TrialScene.scala:828-841`, layer order ~:1129-1141). *Decision:* `--halo` stays stage-keyed; where a focus or role mark is below 3:1, it gets its own inner casing (`OverlayPalette` already separates focus paint from casing, `MarkInput.scala:218`). Test both themes × three stages over map and image grounds (`TokenContrastSuite:64` rule).
    - Isoline `LineCap.Round`.
    - A `MapLegendScene` with the legend text "fades below 10% of max" as a TrialText/ExploreText id.
    - Tests: `TrialMapSceneSuite` (cross after map); `TrialViewFxSuite` goldens.
18. **E2, timeline.** **Fixed.**
    - `OverlayRing` rectangle geometry (`MarkInput.scala:207, 253-325, 474-487`) and an outside-window bar style (Custom dash, no fill) in `TimelinePlot` (`:187-191`).
    - Fill and height already match.
    - Tests: `TimelinePlotSuite:62` (unchanged), `OverlaySelectionsSuite`, `RovingCursorSuite`.
    - Bars outside the brush (no half-open overlap, the brush rule) take `--neutral-mark`; outside-window outlines dim too; rings stay at full strength. A dimmed bar is de-emphasis by design and below 3:1 on purpose; its values are in the table twin.
19. **E3, Explore mark halo.** The board draws the halo outside r = `radiusPx` (`:438`), so no code change, only a board tweak. **Closed as code.**

**G. Figures (viz/app; regenerate the golden figure once) **
20. **F1, y ticks and signed values**: `ParticipantPlot` (`:416-421`, value labels `:368-371`) and `ScaleProfilePlot` (`:379-384`). Tests: `ParticipantPlotSuite:231`, `ScaleProfilePlotSuite:216`, `PlotTableParitySuite`, `FigurePageMapSuite`. **Fixed.**
21. **F2, tile rows.** **Fixed** (`MapTileVM.subtitle`; fixed title row, map and two caption rows, elided, not wrapped; role glyphs drawn as shapes because Plex lacks U+25CF/25C6/25CB).
    - `MapTileVM` gains `subtitle` (`FigurePanels.scala:114, 255-276`).
    - `FigurePage` uses a fixed title row, a map, and a fixed two-row caption drawn without `wrap` (`:161-166, :183, :348, :398`).
    - A Points role glyph (Plex coverage of ● ◆ ○ is unverified).
    - Tests: `FigurePageMapSuite:117`, plus a new "every map shares one top".
22. **F3, figure cross arm.** *Decision:* 20 px half-arm in native screen coordinates, as `TrialScene` (`:423`, `:834`). Test: a `FigureGaze` cross-length test. **Fixed** (already drawn by `TrialScene`'s cross via `FigureGaze`; pinned by `FigureGazeSuite`).

**H. Data (app/viz) **
23. **G1, DensityPicture** (SPEC-AMENDMENTS #15, extended). **Fixed** (S17 rule, S18 code).
    - Floor → every non-zero cell (`GeometryPictures.scala:95`).
    - Log levels (`:97-103`): *Decision:* fixed per dataset revision, never rescaled per view; write the level rule (threshold placement, rounding, empty and single-cell studies, overflow level, labelled count ranges, singletons visible) before coding this part.
    - Scaladoc `:80-87` and the semantics text `GeometryScene.scala:123`.
    - Tests: `GeometryPanelSuite:473` (still 5 cells), `GeometrySceneSuite:91-94`, a new level-threshold test.
24. **G2, text ids.** **Fixed** (`ThumbMissing` is wired with ticket 25).
    - The density key ("Records per cell, log steps · {c} × {r} screen cells" with swatch labels).
    - The thumbnail caption split (`ThumbTrial` + count ids), the singular form, and `ThumbMissing`.
    - At `GeometryText.scala:40, 105-107`.
    - Tests: `GeometryPanelSuite:451-462`.
25. **G3, missing-asset thumbnail.** *Decision:* thumbnails stay the grey placement frame plus marks (`GeometryScene.scala:63`), except a missing-asset trial, which is hatched with an X and never blank (DESIGN_SPEC §9), as the Data board draws it. Image frame art in thumbnails would be a separate feature. **Fixed** (hatch and X from the served asset registry; mounted in the Data Geometry pane by S26, with the import draft's geometry form inside the panel and the panel's own field editor hidden).
    - `ThumbnailPicture` (`GeometryPictures.scala:72-78`) carries the trial's frame state (Present / Missing(file) / Unreadable).
    - `GeometryScene.thumbnail`/`layers` (`:84, :142`) draws `TrialScene.hatched` as a shared helper (`TrialScene.scala:793-818`), with a thumbnail-scale hatch and X.
    - Tests: a `GeometrySceneSuite` test that a missing frame draws a hatch + X; a `GeometryPanelSuite` caption test.

**I. Plots and Results **
26. **H1, ParticipantPlot annotations** (split into slices of at most half a day): an `Annotations` API and `ParticipantTextId`s. Tests: a new suite, plus a Results FX snapshot. **Fixed** (slice a: the `Annotations` API; slice b: text ids, the app builder, Results wiring, accessible summary).
27. **H2, Results paired descriptive**: an app VM plus text. Test: a fixture value test of **0.15567 → "+0.16"**, range +0.05 (P21) to +0.26 (P15), 24 of 24, computed before rounding participant means. The fixture field is added in this landing (`docs/studio/fixture`). **Fixed** (the t3 story spec declares Remembered − Forgotten).

**J. Parity harness **
28. The board's resting depicted focus (`.kf`) gets a desktop counterpart only in tests. Moments put visible focus on the depicted node via `requestFocusTraversal`/Tab (`ShellMomentsFxSuite`, `AppShellFxSuite`), or `PARITY_CHECKLIST.md` exempts resting focus rings. No runtime change. **Fixed** (`RestingFocus` puts real keyboard focus on each board's `.kf` node in AppShellFxSuite and ShellMomentsFxSuite; Data's mapping row and DataEmpty's preset have no desktop stop; MainDark depicts none).

**Settled 2026-10-11:** methods denominator (DESIGN_SPEC §15); dark-stage casing (ticket 17); D4 (ticket 15); NeutralMark (11); density levels (23, rule to write); figure cross arm (22); Data thumbnails stay the grey placement frame plus marks (`GeometryScene.scala:63`), missing assets hatched (25); image frame art would be a separate feature. The mock fixture's science scores are reconciled to the golden fixture in their own ticket, since every board and many studio tests read them. Stage selection keeps the two-band ring with the focus ring outside it; add parity tests for selection only, focus only, both on one mark, and both on different marks (fallback: focus as corner brackets).

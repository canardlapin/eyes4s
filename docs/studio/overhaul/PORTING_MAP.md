<!-- Final porting map and parity thresholds from the v4e UI-engineering review; see README.md. -->
## Scores

| dimension | score | justification |
|---|---|---|
| hierarchy | 8 | Each pane keeps one focal object. The switcher is now fixed at the window centre, so the toolbar reads the same in every specimen. Methods is a 600 px paper with one quiet selected fact. |
| clarity | 8 | The state control always has three slots (Run, Data, Analysis or job), and its menu sections follow the same order. The confirm names what is lost and what stays shown. Data › Trials says "Map is a preview, not a result". |
| density | 8 | Nothing is restated. Analysis (269 words) and Data (261) are still over the 250-word budget (gate log). |
| typography | 8 | The HTML now uses only bundled weights (400, 500, 600), except four `<b>` (700) on Analysis line 28. Plot text is 9–11 px. Methods is Source Serif 14/21. Intaglio Medium and SemiBold still need face families in the port (#32). |
| color | 8 | Report panel A now greys its photo, so the blue diamond no longer sits on a blue ellipse. One ground (#808080) is used for every stage. Amber still marks warnings and "no match" as well as the control role (carried). |
| dataviz | 8 | The strip, stages, slopegraphs and across-scales plot are clean, and the photo is greyed under every map. The dot-strip axis labels are still anchored inconsistently (`results.html`: "0" middle at x 4, "+0.4" end at x 120). |
| interaction | 8 | Every list is one tab stop, Methods is one stop with arrow keys, and the strip is one listbox stop. The confirm has the safe default on the right. Weak spot: the running state is still three stops on 98×28, 22×22 and 16×16 targets, and "Data r3" and "Run 8 · comparing …" are inert text. |
| fidelity | 9 | Methods carries the caveat paragraph, "evaluated at σ 2°", "paired n = 24", the P17/P21 note, the failure cause and "Analysis rev 4, run 7; eyes4s 0.1", all checked by the new gate rule. Role encodings are correct, and the units caveat is on every scaled view. |
| portability | 8 | Five of my six v4b portability blockers are fixed in the golden: the switcher, the shadow, the border shift, the draft mark and the Methods focus model. What remains is small but concrete: the Pair header is 9 px over budget, so JavaFX would ellipsize the units caveat; the running state's structure; a dashed CSS border on the Analysis "not run" tag; `<b>` asking for 700; and the scaladock rail. |

## Previous round

Against my v4b issues, in order:

1. **State width and the switcher: resolved.** The toolbar is `persp-wrap` (absolute, full width) over the brand and state row. The switcher sits at x 575.8–864.2 in every specimen, while the state control is 370 px (idle), 401 px (Data) and 522 px (running, Results), all with no overlap. The perspective buttons are the same width on and off (`Data` 57.9 px both ways; off state `1px rgba(0,0,0,0)`). New consequence: the running control is 522 px, so the window minimum for no overlap is now about **1,380 px** (W/2 + 144 + 12 ≤ W − 12 − 522), up from my v4b estimate of 1,190. See Issue 3.
2. **Collapsed inspector vs the scaladock rail: design resolved, port open.** Every open inspector has "Close the inspector (⌘I)", 28×28 at 1404,86. The closed state is a 28 px pane with one 28×28 open button at y 90, and it is a tab stop on Results, Report and Methods. The scaladock rail is still mouse-only in the pin (LIBRARY-BUGS #4, not filed, pending a check of current scaladock). The port path is in the map below.
3. **Cancel confirm placement and order: resolved.** Decision 3 is amended. `state-menu.html:44` draws [Cancel run] (fail-coloured outline) on the left and [Keep running] (primary, `focusring`) on the right in a plain row, with "Enter or Esc keeps it running." The notice pauses on hover or focus according to `decisions.txt`, but the specimen's caption ("stays for 8 s … then the button returns") does not say so. The focus ring on Keep running is not visible at 1×: in `audit/uiexpert/v4e/sm-confirm.png` the button looks the same as an unfocused primary. That is minor, but the focused default is the safety claim, so the golden should show the ring.
4. **Methods TextFlow: resolved as designed.** There is one stop (`DIV.paper methods`, `role=document`) and 50 facts with no `tabindex`. Underlines appear only on focus or with "Show facts". Two facts wrap across a line ("64 × 48" and "every admitted encoding trial of another item from the same participant"), so `Text` runs are still mandatory. A `Hyperlink` port would move them and reflow paragraphs 1 and 2.
5. **Font weights: mostly resolved.** HTML text is 400, 500 or 600 on every page except Analysis, whose four `<b>` request 700 (Chromium falls back to the 600 face, so it looks right but specifies an unbundled weight). SVG labels still request 500 (Results) and 600 (Figures). The rendering is the intended one; the port must reach it through face families until intaglio#32 lands.
6. **Targets, shadow, draft mark, Report A:**
   - Running-state targets: **open.** `results.html` still builds `part` (97.7×28), ✕ (22×22) and chevron (16×16) as three stops. "Data r3" and the job segment are `span`s, so roughly 290 px of the control does nothing when clicked. The `part` now fills the height, which fixes the ring overpaint.
   - Shadow: **resolved.** Only `.popup` has `box-shadow: 0 8px 24px rgba(0,0,0,.18)`, and the lint passes.
   - Draft mark: **resolved** in the state control (an `svg.draftmark` 9×9 at 1255.5,19). However, a new dashed CSS border appeared on the Analysis navigator's "not run" tag (`analysis.html:15`, 52×20). This is the same defect class.
   - Report A: **resolved** (greyed photo, `filter` on the image).

Also from v4b: the strip is one listbox stop ("Matched and 19 controls; Enter opens a pair") and the queries list is one stop. Both are resolved. Pop-out now appears on the Pair, Across-scales, Results plot, Explore stage, Data › Trials stage and Report page. That is resolved.

## Claims

- *"Switch centred on the window … x 574–866 … no overlap"*: true (575.8–864.2 measured; state left edges 905.9, 1027.2 and 1057.8).
- *"Transparent 1 px border when off"*: true.
- *"Draft mark is a dashed SVG square"*: true in the state control; false as a general claim, because Analysis still has a dashed CSS border.
- *"State-button parts fill the button height"*: true for the idle button and for "Showing run 7". It is not true for ✕ (22 px) or the chevron (16 px).
- *"State menu is a popup with a drop shadow"*: true.
- *"Query list is one tab stop"*: true. So is the strip.
- *"Pop-out on every stage and plot header"*: true. On the Pair header, though, the pop-out button is squeezed to **19.0 px** wide instead of 28 (Issue 1).
- *"Methods is the native golden sentence for sentence … caveat … rev/run stamp"*: the required phrases are present (screenshot and gate). I did not diff against `native-golden-methods.md` line by line.
- *"Menus ordered Run, Dataset, Analysis, Jobs"*: true in all three popups.
- *"Run rev 5 again is also offered in the Jobs section"*: not shown. No specimen has the Jobs section after a cancel; the idle Jobs section reads "run 6 was cancelled at Comparing" with no action.
- *"GATE PASS"*: verified (`logs/v4e-gate.log`, EXIT 0, 9 specimens plus extras).

## Issues

Ordered by their impact on freezing v4e as the porting golden.

1. **The Pair header is 9 px over its width at 1440, and JavaFX would resolve that by ellipsizing the units caveat (must-fix).** On compare-query and compare-query-dark, the pop-out `ibtn` measures 19.0×28 instead of 28×28, because Chromium flex-shrinks the button. In a JavaFX `HBox`, the overflow is taken from children whose min width is below their pref width. A graphic-only `Button` cannot shrink, but the subtitle `Label` can, so the port would render "σ 2°, declared 35 px/°, not c…". That text is frozen content (units must stay visible). **Fix (golden):** make room for at least 12 px, for example by renaming "Underlay on query" to "Underlay" or by tightening the toggles' gap. Add `flex-shrink:0` to `.ibtn`, `.btn`, `.seg` and `.tog`, so that any future overflow shows up as clipping (which the gate catches) rather than as a squeezed control. **Fix (port):** give the header's subtitle `Label` `minWidth = USE_PREF_SIZE` wherever it carries units, so that overflow fails layout tests instead of truncating silently.
2. **The running state should be two stops and one hit area (must-fix in the golden, because the port should not invent a structure that differs from the golden).** Build it as I proposed in v4b and v3. The group is a bordered `HBox` containing a `Button` (dot, "Showing run 7", sep, "Data r3", sep, "Run 8 · comparing", count, 64×4 bar, chevron; `maxHeight ∞`, so all of it opens the menu), then a `Separator`, then a 28×28 ✕ `Button`. The chevron stops being a separate stop. "Data r3" and the job text become clickable. ✕ grows from 22 to 28 px.
3. **Window-width policy for the state control (must-fix, as a spec line).** The running control is 522 px, so below about 1,380 px window width it collides with the centred switcher. 1,366 px laptops are common in labs. **Fix:** state a collapse order in the golden's notes and implement it with a custom `HBox.layoutChildren` or by binding `visible`/`managed` to the toolbar width: first drop the "21,400 / 44,845" count (the bar remains), then "Data r3", then shorten "Showing run 7" to "Run 7". That removes about 220 px, so the running control (about 300 px) fits wherever the idle control (370 px, which needs W ≥ about 1,080) fits. Set the window minimum width to 1,080 px, and add FX layout tests at 1,080, 1,280 and 1,366 px.
4. **Dashed CSS border on the Analysis "not run" tag (must-fix, small).** `analysis.html:15` uses `border:1px dashed`. JavaFX dashes start at the top-left and run continuously, while Chromium fits them per side, so this region can never pass a pixel diff. **Fix:** use a solid hairline tag with the same `draftmark` SVG glyph that the state control uses. That is also more consistent, because "draft" then has one mark everywhere.
5. **`<b>` in the Analysis sentence requests 700 (must-fix, trivial).** Replace it with `.strong` (600). The golden then names only bundled faces, so a CSS-to-FX token extraction cannot produce `-fx-font-weight: bold`, which on Prism resolves to a synthetic or Regular face (the same mechanism as #32).
6. **Intaglio weights (carried, port-side).** Results slopegraph labels (500) and Figures labels (600) must go through studio-viz face-family mapping until canardlapin/intaglio#32 is fixed. Add the conformance check from #32 to the port's golden harness: the advance width of "Remembered" at 500 must exceed its width at 400. This catches a silent Regular fallback.
7. **Collapsed inspector (carried, port-side).** Do not rely on scaladock's rail for the 28 px strip in this pin. Either the app owns it (the inspector group's content swaps to a 28 px `VBox` holding one app `Button`, and the split is fixed through scaladock's fractions), or scaladock is restyled (`headerPx 28`, title hidden) and keyboard access goes through ⌘I. The first option matches the golden's geometry exactly and is keyboard-complete today, so I recommend it until bd-01M5K8EKNNXPCZJ2PZ198CTDVV confirms upstream keyboard support.
8. **Confirm focus not visible; notice-pause not in the specimen; Jobs after cancel not drawn (minor).** Draw the 2 px ring on Keep running. Add "pauses while you point at it or it has focus" to the caption. Show the Jobs row "Run 8 cancelled at Comparing · Run rev 5 again" in the after-cancel section.
9. **Methods underline and selection geometry (port note).** The golden draws the fact underline as a span `border-bottom`, at the bottom of the inline content box, and the selection as the inline content-area background (about 19 px tall). `TextFlow.underlineShape` sits at baseline + underline offset, about 2–3 px higher, and `rangeShape` covers the full 21 px line. Either accept ±3 px vertically for these two shapes (see thresholds), or draw both from the line metrics: underline at baseline + 3, and selection from ascent − 1 to descent + 1. Paper colours are still hex literals (`#FFFFFF`, `#ECE9E2`, `#0E6E68`, `#CBC6BC`); name them as `-paper-*` tokens.
10. **Dot-strip axis label anchoring (carried, trivial).** Use `text-anchor="start"` at x = 4 for "0" and `"end"` at x = 116 for "+0.4", or centre both on their ticks.

## Regressions

Against v0 (the native app):

- **None functional.** Cancel (now confirmed), freshness, the trail, table twins, both themes and keyboard paths all survive. Keyboard cost is now lower than v0 (one stop per list, strip and Methods).
- **Fidelity risk, not yet a regression:** the Pair header overflow (Issue 1) would truncate the units caveat in a straight port.
- **scaladock rail keyboard gap** (Issue 7). The design avoids it only if the app owns the strip.

Against v4b: none. Interaction rises from 7 to 8, portability from 7 to 8, and fidelity from 8 to 9.

## Proposals

### Final porting map (v4e golden)

| design element | JavaFX / scaladock / Intaglio construct | risk |
|---|---|---|
| Window shell (toolbar 48, trail 34, status 26) | `BorderPane`: top `VBox(toolbar, trail)`, centre the `Dock` view, bottom status `HBox` | low |
| Toolbar with a centred switcher | `StackPane(HBox(brand, Region hgrow, state), persp)`; `persp` `StackPane.alignment CENTER`; HBox `pickOnBounds=false` | low |
| Perspective switch, segmented controls | `ToggleButton`s in a `ToggleGroup`, 1 px transparent border when off and `-hairline-strong` when on; ⌘1–4 | low |
| State control, idle | One `Button`, height 30, radius 8; graphic `HBox(Circle r3.5, Label, Region sep 1×14, Label muted, sep, SVGPath draftmark 9, Label, SVGPath chevron)` | low |
| State control, running | Bordered `HBox`: a `Button` (all three slots, the 64×4 `ProgressBar`, the chevron; `maxHeight ∞`), a `Separator`, then a 28×28 ✕ `Button`; collapse order from Issue 3 in `layoutChildren` | low once the golden matches (Issue 2) |
| State menu (Run, Dataset, Analysis, Jobs) | `Popup`, `autoHide`, anchored to the control's bottom-right (`AnchorLocation.CONTENT_TOP_RIGHT`); root `VBox` of sections (padding 14, hairline top), `GridPane` kv 104 + fill; `DropShadow(GAUSSIAN, rgba(0,0,0,.18), radius 36, spread 0, offsetY 8)`, which approximates CSS blur 24 (σ 12); root insets ≥ 32 so the shadow is not clipped; focus the first action and return focus to the owner on hide | medium (shadow match, focus return) |
| Cancel confirm | Same `Popup`, content swapped; `HBox(Region hgrow, Button "Cancel run" .danger-outline, Button "Keep running" defaultButton)`; not a `ButtonBar`; `requestFocus` on Keep running; Esc keeps the run; ⌘. reaches the same path and is inert while `Window.getWindows()` contains a modal | low |
| "Run 8 cancelled at Comparing · Run rev 5 again" notice | Replaces the state graphic; `PauseTransition(8 s)`, paused while `hover` or `focusWithin`; same action in the Jobs section | low |
| Open inspector close (⌘I) | App `Button.ibtn` 28×28 in the inspector header; accelerator on the scene | low |
| Collapsed inspector (28 px) | Recommended: app-owned 28 px `VBox(Button.ibtn)` swapped in as the group's content with a fixed split fraction. Alternative: scaladock `minimize` with `headerPx 28`, `.dock-strip-title` hidden and the strip restyled | low (app-owned); medium (scaladock rail is mouse-only, #4) |
| Pop-out | App ibtn → `Dock.popOut(groupId)`; floating `Stage` with the OS frame on JavaFX 24; `dockBack` restores fractions; tooltip "Open Pair in its own window" | medium |
| Fixed layouts per perspective | One `Dock` + `Perspectives`, one pane per group, `Header.Hidden` | low |
| Pane header (36) | App `HBox.ph`; subtitle `Label` with units `minWidth USE_PREF_SIZE`; buttons `minWidth USE_PREF_SIZE` | low (after Issue 1) |
| Queries and trials trees, group rows | `TreeView`, `fixedCellSize 26`, non-selectable group `TreeItem`s via a selection model that skips them; one stop | medium |
| Summary Δ navigator | `ListView`, cell `HBox(Label, Pane 120×12 of Lines + Circle, Region, tag)`; `Line.strokeLineCap BUTT` | low |
| Pair stages, Data › Trials stage, Explore stage | One Intaglio scene per stage on a Canvas: greyed photo computed in pixels (`RasterImage.tabulate`: grayscale, contrast 0.32, brightness 1.6), never a `ColorAdjust`; map raster pre-upsampled in studio-viz and drawn 1:1; stitched contours as cased `Grob.lines` (not `Grob.segments`, #30); butt-capped scanpath; role points; selection ring | low with stitched contours and pixel-side grey |
| Pair strip (11 + 10 + B) | `GridPane` 11 × 2, 48 px columns, hgap 3; cells `VBox(ImageView 48×36, Label)`; B cell `VBox(Rectangle 2.5×36, Label)`; the grid is one focus-traversable stop with a roving index, arrows, Enter; tiles cached per (key, σ, limit); Intaglio #31 makes the cache scan pixels | low–medium |
| Across scales | One Intaglio `PlotComposition`, 66 px rows, app beeswarm, `TableView` twin | medium |
| Slopegraphs + Δ by σ | One Intaglio composition; Medium labels through face families (#32) | low (with the face mapping) |
| Participant and record tables | `TableView`, `fixedCellSize 26`, right-aligned tabular numerals, the σ 2° column in a SemiBold face family | low |
| Timeline with brush | Intaglio scene + `intaglio-interaction` brush; step, play and speed `ToggleGroup` | medium |
| Methods page | `ScrollPane` → centred paper `StackPane` 600 wide, padding 32/48 → `VBox` of `TextFlow`s (Source Serif 14, `lineSpacing = 21 − natural line height`, top padding + half-leading); facts as `Text` runs; underline and selection `Path`s from line metrics (Issue 9); one focus-traversable flow, roving fact, `accessibleText` = current fact; footer `HBox` bound to the fact | medium (line breaks) |
| Methods toolbar | "Show facts" `CheckBox` styled as `.tog`; Regenerate `disable` bound to staleness; Copy; Export `.btn.primary` | low |
| Report page and panels A–D | One Intaglio page composition; panel A uses the greyed photo; C's B slot as an annotation; panel focus as a 2 px `Rectangle` overlay; zoom `Spinner`-like `HBox` | medium |
| Draft mark, icons | `SVGPath` on a 16 grid, explicit dash segments for the draft mark, round caps and joins | low |
| Analysis "not run" tag | Solid tag with the draftmark glyph (after Issue 4) | low |

### Per-region parity thresholds for the harness

These derive from `spike/RESULT.md`, measured at 1×, headless Monocle SW pipeline vs Chromium. Mask text runs out of every pixel comparison. Diff each region separately, so that a large clean area cannot dilute a local failure.

| region class | geometry | pixels (text masked) | notes |
|---|---|---|---|
| Flat chrome: toolbar, trail, status, pane backgrounds, gaps, hairlines | region bounds ±1 px (the golden has 3–10 fractional boxes per page) | mean Δ ≤ 2; Δ>48 ≤ 0.5% | Hairline spike: 0.01. Fail on any 1 px gap or missing rule. |
| Buttons, segmented controls, tags, focus rings, radii 4–8 | bounds ±1 px | mean Δ ≤ 4; Δ>48 ≤ 1% | Spike button: 3.51 mean with text unmasked; masked, the corners alone are well inside. |
| UI text (Plex 9–15, all weights) | ink-box left ±1, right ±1 px + 0.4% of run width, top/bottom ±1; baseline ±1 | excluded; ink darkness within ±8% | **Hard fail on any ellipsis or clip the golden lacks.** Weight check: an advance at 500 or 600 must exceed the advance at 400 (catches #32). |
| Methods serif paragraphs | line-break positions identical (hard); first baseline ±1; line pitch exactly 21 | excluded | Underline and selection shapes ±3 px vertically unless drawn from line metrics, then ±1. |
| Map rasters (stages, strip tiles, Report C) | placement ±0.5 px | mean Δ ≤ 2; Δ>48 ≤ 0.5% once both sides draw studio-viz pre-upsampled pixels 1:1; provisionally mean Δ ≤ 3, Δ>48 ≤ 1% until Intaglio's `Raster(Smooth)` path is measured | The spike measured `ImageView` (0.93), not Intaglio's raster path. |
| Greyed photo | — | mean Δ ≤ 2, Δ>48 = 0 | After the golden is regenerated from the port's pixel function. Do not compare a CSS `filter` against the pixel recipe without measuring them once. |
| Intaglio strokes: contours, scanpaths, ticks, axes | coordinates from the scene model exactly | mean Δ ≤ 2; Δ>48 ≤ 0.5% | Stitched contours only. A dashed, alternating-width pattern means #30. |
| Role marks (circles, diamonds, B bar) | centres ±0.5 px from the data | mean Δ ≤ 3; Δ>48 ≤ 1% | Not in the spike; measure once and then tighten. |
| Popup content | ±1 px relative to the anchor | as chrome | |
| Popup shadow, floating window frame | — | shadow band excluded (or mean Δ ≤ 8, unmeasured); OS frame excluded | |
| Dark theme | same as light | same as light | Run every region in both themes. |
| Output scale 2× | geometry only | not compared | The spike did not measure 2×. Measure it before setting pixel thresholds. |

### Verdict

**DO NOT FREEZE v4e byte for byte. FREEZE v4f = v4e + must-fixes 1–5.** All five are mechanical golden edits:

1. Give the Pair header at least 12 px of slack. Add `flex-shrink:0` on controls.
2. Make the running state one `Button` plus a 28 px ✕ (two stops).
3. Write the state control's collapse order and the 1,080 px minimum into the golden notes.
4. Replace the Analysis "not run" dashed border with a solid tag and the SVG draft mark.
5. Change `<b>` to `.strong` (600).

None of these changes a number, an encoding or a caveat. Qualify v4f with the gate plus a rerun of `audit/uiexpert/v4e/probe.cjs` (the pop-out is 28 px, the running state has two stops, no dashed borders, no 700 weight). A further critique round is not needed. Items 6–10 are port-side notes or minor polish and do not block the freeze.

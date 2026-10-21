# Board parity checklist

Every studio PR that renders a screen checks its board here and records deviations. Each parity ticket
(S10.4-<Board>) signs off one board: it seeds the board's story moment and compares against the board's
reference PNG. The PNGs are rendered from `design/` (S10.4-render).

Shell metrics are checked from JavaFX node bounds to ±1 px. Texts are checked verbatim. Colours are checked
by token name, never by hex value.

Reference rendering (2026-10-20): `tools/studio-boards` pins Chromium 131.0.6778.33 /
Playwright Core 1.49.1, uses the seven bundled font faces and the vendored local board
resolver, and refuses external requests. All nine boards at 1440 × 900 logical pixels
have committed 1×/2× references under `design/reference/`, with input/environment hashes.
A second process reproduced all eighteen PNGs byte-for-byte. These are design references;
application parity is qualified separately below.

## Shared shell (every board except System)

- [ ] Title bar: native and system-drawn. The app does not tint it.
- [ ] App bar is 44 px tall with a bottom hairline.
  - [ ] Eye mark plus "Eyes Studio" in 13/600.
  - [ ] Project chip with a menu.
  - [ ] Perspective switcher: Data ⌘1 · Explore ⌘2 · Analysis ⌘3 · Compare ⌘4 · Figures ⌘5. The active item has a surface fill and a hairline ring.
  - [ ] Jobs chip, idle state: "No jobs".
  - [ ] Jobs chip, running state: "Run 8 · Comparing · 21,400 / 44,845 pairs" with a progress bar and Cancel.
  - [ ] Jobs chip, failed state: "Run N failed · k diagnostics".
- [ ] Context strip is 32 px tall.
  - [ ] Back and Forward buttons.
  - [ ] Live trail crumbs, with the current crumb in ink 600.
  - [ ] Freshness badge: "Analysis rev N · run M · data rK · current", with a teal dot.
  - [ ] Dashed draft chip "Draft rev N · k change · ready", shown only while a draft exists.
- [ ] Draft banner, 30 px, dashed. It appears only in Compare and Figures, and only while a draft differs from what is shown or a run is in progress.
- [ ] Dock: 1 px hairline splitters, 28 px tab headers. The focused group's selected tab carries a 2 px accent top rule. Every perspective has a navigator group.
- [ ] Status bar, 24 px, four slots: `● Selected: path` · hint · job · `Saved hh:mm`.
- [ ] Type uses only the sizes 11, 12, 13/600, 16 and 28. Numerals are Plex Mono.
- [ ] Colours come only from tokens. Accent is used only for focus, primary action and the "current" dot. Selected rows and items use `--sel-fill` plus weight 600 (Mono 500); marks on stages use the neutral ring (sel-ring) (DESIGN_SPEC §15).
- [ ] Each list, tree, grid and plot is one tab stop; while focused, only its cursor item draws the accent ring.
- [ ] Pending or blocked actions are disabled but fully readable, with an attached visible reason.

## System
- [ ] Token swatches in light and dark, including match-text, control / control-fill and on-stage.
- [ ] Role marks on light, dark and stage backgrounds: query as a filled circle, matched as a diamond, control as a hollow circle.
- [ ] Map ramp with cased isolines, and the diverging ramp.
- [ ] Type scale, freshness states (including Blocked on P11 ret_05) and the Compare "No run yet" state.
- [ ] The four kinds of change, the display-kind glyphs, the shell anatomy, the keyboard model, and the module map keyed to UI-A…UI-H.

## DataEmpty (moment t0)
- [ ] "Untitled project"; the trail reads "New project"; freshness reads "No dataset · no analysis"; status reads "Not saved".
- [ ] Drop targets for fixations.csv (required) and trials.csv (recommended, with the stated reason). The whole window accepts drops.
- [ ] Stimuli folder, preset radios, the bundled example, recent projects, and the local-only statement.
- [ ] "What Eyes Studio will ask you" checklist.

## Data (moment t1)
- [ ] "Dataset r3 · draft · no run on r3 yet", plus "Run 5 (rev 3) used r2 · becomes stale when r3 is admitted".
- [ ] Column mapping includes Ordinal and Sample count, with time units declared in ms. Mapping and geometry are tagged "Dataset · re-admit".
- [ ] Trial identity builder with the duplicate-key warning.
- [ ] Ledger: 960 = 937 + 17 (by cause) + 6 absent. The inventory origin of absent trials is noted. Every count is a button.
- [ ] Geometry:
  - [ ] Frame and placement.
  - [ ] "35 px/° declared · not calibrated".
  - [ ] Viewing distance and screen size.
  - [ ] Degrees measured from the image centre, x right, y up.
  - [ ] Off-screen policy (ExcludeRecord by default).
  - [ ] "543 of 11,520 source records outside the window in 409 trials".
  - [ ] Thumbnails and the all-trials overlay.
  - [ ] "Mark trial as wrong orientation…".
- [ ] 257 of 259 images; forest-044 and kitchen-081 shown missing (hatched), never blank.

## Explore (moment t2)
- [ ] Trail runs from Summary through retrieval response, Remembered, P17, ret_07 and the pair to enc_03 · fixation 6 · fixations.csv record 7,214.
- [ ] Neutral fixation marks with halos. The preview map is labelled "preview · σ 2° · not a result" in a dashed frame. Isolines are cased.
- [ ] Timeline brush and playhead, with the note that the brush does not crop the analysis.
- [ ] Source records table follows the selection. The table is a single focus stop.
- [ ] Inspector for fixation 6: onset 2160 ms, duration 412 ms, screen (1148, 456), image (700, 300), (+5.4°, +2.4°). "Used by" lists ret_07 (matched) and 18 other queries.

## Analysis (moment t2)
- [x] Three presets and an explicit inventory column/value selection for expected-unmatched trials. Library tests prove that item collisions, including a dropped studied item, cannot create pairs.
- [ ] Native by-design projection is implemented and passes focused JVM/JS checks; full qualification and landing remain pending (`bd-01M5E637J24JYT6FHQW336E01P`). `RealPreviewCursorSuite` now checks service receipts, rows, acceptance and completed native results, including an item-ID collision and a mixed four-status cohort. See [the bounded evidence receipt](evidence/20261021-by-design/review.json).
- [x] Recipe sentence with role-coloured tokens. Window, grid and initial-fixation controls use served descriptors; their board tags now read "served UI-A / UI-F". `DescriptorFormSuite` adds a synthetic field without changing the Studio renderer.
- [x] Scales 0.5 / 1 / 2 / 4 (+8° in the draft), cell 0.46°, and the 0.5° and 8° warnings. Values and warnings are served by eyes4s; `ScalesEditorSuite` checks the drawn warnings, one Tab stop and keyboard removal.
- [x] The served pairing control persists “Report as no match”; the generic field label is “Focal trials without a matched reference”.
- [x] Draft settings apply as one undoable command; Diff uses the library plan diff and Description is saved with the revision. `DraftLifecycleSuite` checks invalid-input blocking, immutable revisions, run attachments and discard.
- Presentation differences recorded for this tranche: the generic form scrolls and shows the library's field metadata; Apply commits a valid group of inputs to the draft. Presets and scale selection use the shared segmented control. The cell label shows both axes, and the large-scale warning displays the served extent ratio. No protocol/exploratory scale classification is added.
- [ ] Resolved design:
  - [ ] 219,486 Cartesian candidates before paging (466 admitted retrieval × 471 admitted encoding), then 8,969 after.
  - [ ] 480 = 454 without a known focal preparation failure + 3 known focal failures + 9 no match + 14 not admitted. Successful contributing contrasts are known only after execution.
  - [ ] Input digest plus plan revision.
- [ ] Preflight keeps eyes4s findings separate from Studio checks ("0 queries with >1 matched reference · checked"). "Save & run rev 5 · 44,845 pairs" is enabled.
- [ ] Preflight budget row: a recorded deviation. The board's "Budget: pairs ≤ 50,000 · cells ≤ 20M" is omitted because the real backend prepares with eyes4s's default `PairScheduleBudget`, which sets no limit, and eyes4s has no cell budget. The row returns, from the served value, when an analysis applies a budget (decision on `bd-01M437FHJSCPPAZ8PMZ3WD7EGH`).

Analysis tranche evidence (2026-10-13): `DescriptorFormSuite`, `ScalesEditorSuite`,
`DraftLifecycleSuite`, `PresetSuite`, `RecipeFormServiceSuite`, and the Analysis/Explore
walks in `A11yTreeSuite`. Light and dark native snapshots were reviewed. The Analysis board's
window/grid/initial-fixation “pending UI-A / UI-F” tags are replaced by served descriptors;
other boards' pending tags remain unchanged. Full qualification is recorded with the landing.

## Main and MainDark (moment t2)
- [ ] Queries navigator with the count strip, including "by design".
- [ ] Query panel: filled circles, "Displayed: blank + fixation cross".
- [ ] Reference panel: diamonds for matched, hollow circles for control. Clicking a control dot switches the panel. "Back to matched reference" is present.
- [ ] Scale ladder: 19 control dots in a beeswarm, a 3 px B tick, the M diamond and the ink D bar.
- [ ] Readout: D +0.62 in 28/500, M 0.94, B 0.32 (19 controls), and the confound sentence verbatim.
- [ ] "Why this reference?" panel, including the outside-window counts. Reporting reads "By retrieval response" with "Min queries per group: Off · n 2–17". Appearance offers the stage, colour-limit and opacity (0.6) controls.
- [ ] Dark theme: the query / matched L* spread is at least 20, and every role pill reaches 4.5:1.
- Deviation (S10.5 F3, accessibility): the contrast group also has the scale profile's Table tab after "Scale profile", which the board does not show. Every plot has its Table twin (DESIGN_SPEC §10).

## Results (moment t3)
- [ ] Jobs chip is running. The banner says the results will not replace this view until Show.
- [ ] Participant plot shows 24 participants with grand means +0.43 and +0.24, each group labelled "n = 24", paired n = 24, n range 2–17. P05 has participant means (D +0.35); its 3 failed queries show as "no value".
- [ ] Paired descriptive "Δ Remembered − Forgotten +0.18" (range +0.04 P15 to +0.34 P11; higher in 24 of 24), computed before rounding participant means, with the rounding note.
- [ ] Scale profile on a log-spaced x axis. Participant table matches FIXTURE.md exactly.
- [ ] Reporting inspector:
  - [ ] Minimum queries per group is off, and it states what turning it on would drop.
  - [ ] "used by Figure 1".
  - [ ] The confound sentence.

## Figures (moment t2)
- [ ] Figure-level binding, locked: run 7 plus "By retrieval response". Panels choose only scale and selection.
- [ ] Panel C reads "Matched 0.94 · highest of 19 controls harbor-231 0.72 · control mean B 0.32 · D +0.62". Panel D uses a Δ-cosine axis with a zero rule. Panel E uses a log x axis. Figure text is Plex Sans at 7 pt.
- [ ] Stale Figure 2 reads "r3 changed the admission status of 4 trials", with Rebind… / Keep.
- [ ] Binding note: the board's "Rebinding makes a new figure version; the old one stays reproducible" is a recorded deviation. Rebinding replaces the binding in place and Undo restores it; 1.0 persists no figure versions (decision on `bd-01M42HMHQ1PQMZCJFS4AHQ8QE5`).
- [ ] methods.md states:
  - [ ] 457 eligible queries.
  - [ ] 543 of the 11,311 fixation records of admitted trials (4.8% of their fixation duration).
  - [ ] The off-screen policy and the occurrence rule.
  - [ ] The confound sentence.
- [ ] Export bundle list.

## Open deviations from the 2026-10-11 boards

Every ticket of the design review's landing list has landed (`design/REVIEW.md`, tickets 4–28 Fixed or Closed;
the board edits 1–3 are done). The differences below remain between the boards and the desktop; a parity check
records each one instead of failing. Each names its ticket and why it stands.

- Shared: the boards' resting focus (`.kf`) is shown by real keyboard focus in the parity moments (`RestingFocus`),
  not drawn at rest (ticket 28's exemption). Data's mapping row and DataEmpty's preset radio have no desktop Tab
  stop to carry it, and stay non-stops (owner decision 2026-10-15).
- Shared: the slider's track is drawn whole; the board's range fills it to the thumb. The board's fill is the
  browser's native `accent-color` range, and JavaFX's `Slider` has no filled-range part (16). Accepted as a
  platform difference (owner decision 2026-10-15).
- Data: the off-screen policy sits after the corrections (board: in the facts list). At 1440 × 900 the density and thumbnails are below the fold, because
  the import draft's editable geometry form, the one geometry editor, replaces the board's facts list (S26).


## Bounded review evidence, 2026-10-20

The Main tabs (S8.5) and Data column-mapping host reviews have a side-by-side
record at `evidence/20261020-parity/index.html`, with capture/source hashes in
`evidence/20261020-parity/review.json`. The record reviews those bounded features;
it does not sign off either entire board.

The mounted Scale profile capture exposed a real rendering refusal: the builder
accepted report series but rejected the query's `QueryContrast` result refs. The
query series now draws selectable points and its line, preserves those refs, and
breaks the line at missing values. `ScaleProfilePlotSuite` reproduced the original
refusal before the fix, then passed on JVM and Scala.js. `PairsTableFxSuite` now
requires the full mounted profile to reach `Drawn`, alongside the table-value checks.
The additional Table tab remains the recorded accessibility deviation above.

The Data host uses the reusable import form: declared units have an explicit global
control, rows scroll, and preset/apply controls remain available. The fixture input
has ten source columns; the board's illustrative eleven-column export includes an
optional Sample count. The role is available when that source column is present.
The actual served form, not a fabricated extra column, is reviewed. The geometry
editor replaces the board's compact facts list as already recorded under S26.

Current captures still show the separately tracked trial-caption overlap at normal
pane sizes (`bd-01M4ZQJ91DP721J9XEM9TV9862`). That issue remains open; the fixed
profile is not evidence that every Main/Explore difference is resolved. Final
qualification and landing are recorded on the corresponding motes.


## Bounded count/provenance review, 2026-10-20

The current-draft placement counts, matching trial drill-down, eligible/failing split,
and served unmatched-reference declarations are captured at
`evidence/20261020-counts/index.html`, with source and capture hashes in `review.json`.
Focused JVM/JS and native checks and Studio style checks passed; full qualification
and landing are pending. This does not sign off the whole Data or Analysis board.

Placement counts include unadmitted trials and explicitly report unplaced source rows;
they are distinct from admission-only counts. A changed geometry under the same draft
id hides previous counts, and a late response for an older key cannot replace them.
Count clicks open the matching served per-trial tallies, including occurrence.
The shared TableTwin renderer reserves actual cell insets before assigning text width;
a native regression reproduced clipped trial identities before the fix.

The resolved design shows 457 pair-eligible queries: 454 without a known focal
preparation failure and 3 that will fail. It names the library's source causes,
keeps missing matches separate from reference declarations, and never claims that
uncomputed scores will contribute. Complete reasons are available through the row's
accessible name and tooltip when a narrow status column elides them. The new source
reference metadata also activates the formerly pending Compare-inspector test.
Native by-design classification remains the separate G3 blocker cited above.

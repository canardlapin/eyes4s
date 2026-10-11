# Board parity checklist

Every studio PR that renders a screen checks its board here and records deviations. Each parity ticket
(S10.4-<Board>) signs off one board: it seeds the board's story moment and compares against the board's
reference PNG. The PNGs are rendered from `design/` (S10.4-render).

Shell metrics are checked from JavaFX node bounds to ±1 px. Texts are checked verbatim. Colours are checked
by token name, never by hex value.

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
- [ ] Three presets. Recognition shows its "by design" category.
- [ ] Recipe sentence with role-coloured tokens. Descriptor fields show "pending UI-A / UI-F" tags.
- [ ] Scales 0.5 / 1 / 2 / 4 (+8° in the draft), cell 0.46°, and the 0.5° and 8° warnings.
- [ ] "Queries without a matched reference: Report as no match".
- [ ] Resolved design:
  - [ ] 219,486 Cartesian candidates before paging (466 admitted retrieval × 471 admitted encoding), then 8,969 after.
  - [ ] 480 = 454 + 3 + 9 + 14.
  - [ ] Input digest plus plan revision.
- [ ] Preflight keeps eyes4s findings separate from Studio checks ("0 queries with >1 matched reference · checked"). "Save & run rev 5 · 44,845 pairs" is enabled.
- [ ] Preflight budget row: a recorded deviation. The board's "Budget: pairs ≤ 50,000 · cells ≤ 20M" is omitted because the real backend prepares with eyes4s's default `PairScheduleBudget`, which sets no limit, and eyes4s has no cell budget. The row returns, from the served value, when an analysis applies a budget (decision on `bd-01M437FHJSCPPAZ8PMZ3WD7EGH`).

## Main and MainDark (moment t2)
- [ ] Queries navigator with the count strip, including "by design".
- [ ] Query panel: filled circles, "Displayed: blank + fixation cross".
- [ ] Reference panel: diamonds for matched, hollow circles for control. Clicking a control dot switches the panel. "Back to matched reference" is present.
- [ ] Scale ladder: 19 control dots in a beeswarm, a 3 px B tick, the M diamond and the ink D bar.
- [ ] Readout: D +0.38 in 28/500, M 0.73, B 0.35 (19 controls), and the confound sentence verbatim.
- [ ] "Why this reference?" panel, including the outside-window counts. Reporting reads "By retrieval response" with "Min queries per group: Off · n 2–17". Appearance offers the stage, colour-limit and opacity (0.6) controls.
- [ ] Dark theme: the query / matched L* spread is at least 20, and every role pill reaches 4.5:1.
- Deviation (S10.5 F3, accessibility): the contrast group also has the scale profile's Table tab after "Scale profile", which the board does not show. Every plot has its Table twin (DESIGN_SPEC §10).

## Results (moment t3)
- [ ] Jobs chip is running. The banner says the results will not replace this view until Show.
- [ ] Participant plot shows 24 participants with grand means +0.30 and +0.15, paired n = 24, n range 2–17. P05 has participant means (D −0.08); its 3 failed queries show as "no value".
- [ ] Paired descriptive "Δ Remembered − Forgotten +0.16" (range +0.05 P21 to +0.26 P15; higher in 24 of 24), computed before rounding participant means, with the rounding note.
- [ ] Scale profile on a log-spaced x axis. Participant table matches FIXTURE.md exactly.
- [ ] Reporting inspector:
  - [ ] Minimum queries per group is off, and it states what turning it on would drop.
  - [ ] "used by Figure 1".
  - [ ] The confound sentence.

## Figures (moment t2)
- [ ] Figure-level binding, locked: run 7 plus "By retrieval response". Panels choose only scale and selection.
- [ ] Panel C reads "Matched 0.73 · highest of 19 controls street-112 0.61 · control mean B 0.35 · D +0.38". Panel D uses a Δ-cosine axis with a zero rule. Panel E uses a log x axis. Figure text is Plex Sans at 7 pt.
- [ ] Stale Figure 2 reads "r3 changed the admission status of 4 trials", with Rebind… / Keep.
- [ ] Binding note: the board's "Rebinding makes a new figure version; the old one stays reproducible" is a recorded deviation. Rebinding replaces the binding in place and Undo restores it; 1.0 persists no figure versions (decision on `bd-01M42HMHQ1PQMZCJFS4AHQ8QE5`).
- [ ] methods.md states:
  - [ ] 457 eligible queries.
  - [ ] 543 of the 11,311 fixation records of admitted trials (4.8% of their fixation duration).
  - [ ] The off-screen policy and the occurrence rule.
  - [ ] The confound sentence.
- [ ] Export bundle list.

## Open deviations from the 2026-10-11 boards

The boards were revised ahead of the code (DESIGN_SPEC §15). Until each ticket lands, a parity check records
the deviation below instead of failing. Tickets are numbered as in the design review's landing list.

- Shared: selection fill `--sel-fill` and weight 600 not yet in the desktop (tickets 9, 13); composite cursor
  and `:focus-visible` gating (12, 14); type helpers `.t11-strong`, `.t12-strong`, `.t12-label`, `.mono-500` (10);
  radio, checkbox and slider styling and the segmented bar (16).
- Shared: the board's one resting focus ring (`.kf`) is shown in tests by putting real keyboard focus on that
  node, not drawn at rest (28).
- Bugs found by the review: Mono "SmBld" family named in `studio-navigator.css:131` and `studio-ledger.css:99`
  (4); `.design-chip` focus replaces its selected fill (5); greyscale proof by desaturation (6); "1 records" (7).
- Maps: alpha floor and hatched missing cells (8); fixation cross above the map and cased, round isoline caps,
  map legend (17).
- Explore: NeutralMark alpha .35 and halo (11); timeline, including bars outside the brush dimmed (18).
- Results: y ticks and signed values (20); annotation column (26); paired descriptive (27).
- Figures: tile rows and wrapping (21); figure cross arm (22).
- Data: density levels, pending the level rule (23); thumbnail caption text ids (24), including "Missing asset · forest-044.png"; the missing-asset thumbnail hatch and X (25).

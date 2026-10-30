Owner decisions, 2026-10-29 (after the v3 review):
1. Map ramp: reopened. The MapPalette ramp starts at #F9D9EA, lighter than the grey ground, so every density map shows a pale halo. v4 proposes a ramp that never rises above the ground's lightness, with transparency (not near-white) carrying low mass.
2. State button scope: study-wide (designer's recommendation): run, dataset draft and analysis draft, and the running job; the menu has Run, Dataset, Analysis and Jobs sections.
3. Job cancel safety: inline confirm. ✕ or ⌘. turns the control into "Cancel run 8? [Cancel run] [Keep running]"; ⌘. is inert while a dialog is open; a "Run 8 cancelled · Restart" notice follows.
4. Summary navigator: keep the Δ dot strip as the pick list, remove its Δ number column (the table holds the exact values at every σ).
5. Inspector: closed by default on Results › Summary and Report; open on Results › pair and the trial drill. Summary's reporting choices move to a control in the plot header.
6. Docking: fixed splitter layouts per perspective; stages and plots may float to their own window; no default tab stacks.
7. Pair control strip: two balanced rows of 48 px maps (11 + 10), as in Report panel C.
8. One more mockup round (v4) for Report › Methods and Data › Trials, applying 1–7; v4 is then frozen as the porting golden.
Amendment 2026-10-29 (v4b UI-engineering review): decision 3 is drawn as follows. ✕ or ⌘. opens the state menu (a Popup anchored to the state button) at its confirm section: "Cancel run 8? … Run 7 stays shown." with [Cancel run] on the left and [Keep running] on the right as the default (focus, Enter and Esc keep the run). Use a plain HBox, not ButtonBar (which reorders per platform). The "Run 8 cancelled at Comparing · Run rev 5 again" notice pauses on hover or focus; Run rev 5 again is also offered in the Jobs section.

# Eyes Studio accessibility audit

Ticket S10.5 (bead bd-01M3DPG1XBKAVXA1CDT1T1RPMA). This report has three parts:

- **Slice a, automated audit:** done.
- **Slice b, keyboard-only journey:** pending.
- **Slice c, VoiceOver pass on macOS:** needs a person at a Mac.

Requirements: DESIGN_SPEC §10 and WCAG 2.2 AA (1.4.3 text contrast, 1.4.11
non-text contrast, 2.1.1 keyboard, 2.4.3 focus order, 2.4.6 labels, 2.4.7 focus
visible, 3.2.4 consistent identification, 4.1.2 name and role).

## Status

| Part | State | Evidence |
|---|---|---|
| Every plot has a Table twin | Checked, two named exceptions | `TableTwinSuite` (studio-app) |
| Rendered text contrast, light and dark | Checked, 4 findings fixed | `A11yRenderSuite`, `ImportWizardFxSuite` |
| Focus drawn at every Tab stop | Checked, 2 findings fixed | `A11yRenderSuite`, `ImportWizardFxSuite` |
| Names and roles of every Tab stop | Checked (S1.11), extended to the wizard and a popout | `A11yTreeSuite`, `A11yRenderSuite`, `ImportWizardFxSuite` |
| Unambiguous names | Checked, 1 finding fixed, 1 deferred | `A11yRenderSuite` |
| Focus order | Reviewed against the boards, 1 finding fixed | `docs/studio/a11y/tab-order-*.txt` |
| Keyboard-only E2E-01 | Pending: slice b | `KeyboardJourneySuite` (to come) |
| VoiceOver on macOS | Pending: slice c, the script is below | a person at a Mac |

## Method

The automated checks run headless on Monocle, as every Studio FX suite does
(AGENTS.md). They open no window and cannot hear anything. They read the
scene graph that JavaFX would draw and announce, which leaves two gaps:

- a defect in how macOS's accessibility bridge turns that graph into speech;
- anything that depends on hearing it.

The VoiceOver pass covers those gaps.

The checks live in `A11yChecks` (studio-desktop test scope) and apply to any
drawn root: the window, the import wizard, or a popped-out dock window. Each
check returns its failures with their text and place, so a test asserts the
list is empty and a failure says exactly what failed.

- **Twins.** For every Plot pane of every layout in `StudioLayouts.spec`,
  there is a mutual Table twin in its own tab group (`StudioLayouts.twin`).
  The twin is `<id>.table`, or a named pair. The tab menu offers "Show
  table" on the plot and "Show plot" on the twin.
- **Contrast.** Every shown, enabled text must reach 4.5:1, or 3:1 for large
  text (24 px, or 18.66 px bold). The ratio is measured over the worst colour
  the text can be drawn on.
  - The backdrop is composited from the scene's fill down the chain to the
    text. Each region paints its fills, and each node's opacity mixes
    everything it draws over what lies below it.
  - A gradient fill contributes every stop.
  - The compositor has its own test: a half-opaque box, and a gradient's
    worst stop.
  - The luminance is checked against `Wcag.contrast` on a token pair.
- **Focus.** At every Tab stop, the drawing must change when the stop is
  focused: its own fills and borders, its first descendants', or an enclosing
  node's. The check sets `:focused` on the node and `:focus-within` on its
  ancestors, as JavaFX does for the focus owner. These states are set
  directly because a headless window cannot be sure of holding OS focus;
  A11yTreeSuite does the same.
- **Names.** Every Tab stop has a role and an accessible name. A labelled
  control's name contains its drawn words. No two Tab stops share a role and
  a name, except one action mirrored by design.
- **Order.** The committed tab-order files are read against each board's
  reading order. `A11yTreeSuite` keeps them true: it walks the Tab cycle and
  compares the walk with the stops that studio-app derives.

Every new check was verified by mutation. Each fix below was reverted in turn
and the check failed. The compositor's opacity mixing and its per-stop
gradients were each removed in turn, and the synthetic test failed.

## Coverage

The checks below ran in light and in dark, except that names and roles are the
same in both themes and were checked once.

| State | Contrast | Focus drawn | Names, roles | Unambiguous | Tab order file |
|---|---|---|---|---|---|
| Data, t1 (admission) | ✓ | ✓ | ✓ | ✓ | `tab-order-data.txt` |
| Data, new project (empty) | ✓ | ✓ | ✓ | ✓ | — |
| Explore, t2 (P17 enc_03) | ✓ | ✓ | ✓ | ✓ | `tab-order-explore.txt` |
| Analysis, t2 (design checked) | ✓ | ✓ | ✓ | ✓ | `tab-order-analysis.txt` |
| Compare, t2 (query) | ✓ | ✓ | ✓ | ✓ | `tab-order-compare.txt` |
| Compare, t3 (summary, run running) | ✓ | ✓ | ✓ | ✓ (one exception) | — |
| Figures, t2 (inspector, methods, bundle) | ✓ | ✓ | ✓ | ✓ | `tab-order-figures.txt` |
| Import wizard, each of its 4 pages | ✓ | ✓ | ✓ | — | — |
| A popped-out dock window | ✓ | ✓ | ✓ | — | — |

Not covered by the automated checks:
- The operating system's own file chooser (import, Repair…, Export bundle…).
  It is native, outside the scene graph, and macOS answers for it.
- Plot marks against their neighbours (1.4.11). The mark colours are tokens,
  and TokenContrastSuite holds the pairs the spec names. This audit checks
  text and focus rings, not the drawn marks of a plot.
- Pointer-only affordances. Slice b finds them by driving E2E-01 from the
  keyboard alone.

## Findings

| Id | Severity | Where | Finding | Resolution |
|---|---|---|---|---|
| F1 | Medium | Explore | The small-multiples plot pane has no Table twin. | Exception: S6.7, which builds the pane, owns its twin. `TableTwinSuite` names it and fails when the twin exists without the exception being dropped. |
| F2 | Medium | Compare, query | The scale ladder (`compare.contrast`) and its Pairs table (`compare.pairs`) are twins, but the tab menu did not know it, so neither offered the other. | Fixed: `StudioLayouts.twin` names the pair, and the tab menu reads it. |
| F3 | Low | Compare, query | The scale profile has no Table twin in the query layout. Its twin is in the Summary layout only, as on the board (Main.dc.html). | Open, needs the lead: add the twin to the query layout (a board change), or keep the named exception in `TableTwinSuite`. |
| F4 | High | Compare, query | The queries navigator's filter prompt used modena's prompt colour: 1.72:1 in light, 1.04:1 in dark. | Fixed: the field is Explore's `nav-filter`, with its prompt in ink-3. |
| F5 | High | Figures, dark | The caption and stamp under the panels took the theme's ink: 1.20:1 on the white paper. | Fixed: paper ink and paper ink-2 in either theme. |
| F6 | Medium | Figures | The figure list's rows drew no focus. | Fixed: an accent ring inside the row's edge. |
| F7 | Medium | Explore | The trial view drew no focus. It lies outside any `plot-pane`, whose ring is drawn on `:focus-within`. | Fixed: the explore stage draws the same 2 px ring. |
| F8 | Low | Explore | Two Tab stops were both named "Show raw record" (source records and inspector). | Fixed: their accessible names say which; the drawn labels are unchanged. |
| F9 | Low | Compare, Figures | The scale profile announced "Scale profile of Scale profile of run 7". | Fixed: the summary leads with the caption alone. |
| F10 | Low | Shell, while a run runs | The jobs chip's Cancel and its mirror in the status bar are both named "Cancel". This is consistent, one function under one name (3.2.4), but the name does not say what it cancels (2.4.6). | Deferred to a bead: name the run ("Cancel run 8") in the shell view-model and in `A11y.tabOrder`. |

Focus order: each perspective's Tab cycle follows its board's reading order:
- the app bar;
- the context strip and its draft actions;
- each dock group's shown pane, then that pane's controls, left to right and
  top to bottom;
- the status bar's action.

The review found no ordering defect. F8 and F9, both naming defects, came out
of it.

## Slice b: keyboard-only E2E-01 (pending)

`KeyboardJourneySuite` will drive E2E-01 in the window on Monocle using only
key events:
- ⌘1–5, F6/⇧F6, ⌃⇥, Tab/⇧Tab;
- arrows, Home, End, Enter, Space and Esc on plots, tables and navigators;
- menu accelerators.

It ends where `GoldenJourneyFxSuite` ends, and is held to `GoldenRoute`: the same
document science, the same bundle bytes, and the same saved folder. A step with no
keyboard path is a finding. Small fixes land in the slice; larger ones become
beads, and the step is marked pending under a named stub.

## Slice c: VoiceOver script (for a person at a Mac)

This pass needs a visible build and a person listening. Run it on a Mac
that nobody else is working at.

1. Build the app.
2. Turn VoiceOver on (⌘F5).
3. Start a new project and import `fixtures/studio-golden` (`fixations.csv`,
   `trials.csv`, `stimuli/`), as E2E-01 does.
4. Work through each step below, using only the keyboard and the VoiceOver
   keys.
5. Mark each step pass or fail, and write down what was heard when it
   differs.

VoiceOver reads a control's name, then its role. The expected names are the
ones the committed tab-order files record. A fail is any of:
- a missing name;
- a role that does not match;
- a name that does not match the file;
- focus landing somewhere you cannot hear.

| # | Step | Keys | Expect to hear | Pass |
|---|---|---|---|---|
| 1 | Data: the switcher | ⌘1, then Tab from the project chip | "Data (⌘1)", toggle button, selected | |
| 2 | Data: the panes in order | F6 repeatedly | "Sources", list; "Column mapping"; "Admission"; "Geometry" (each a group) | |
| 3 | Admission counts | Tab inside Admission | "Inventory: 960 trials", button … "Absent: 6 trials", button | |
| 4 | Admission choice | Tab to the radio | "Require complete: Refuses r3 while 17 trials are quarantined …", radio button | |
| 5 | Repair a missing image | Tab to "Repair…", then Space | "Repair…", button; the OS file chooser opens and reads its own controls | |
| 6 | Explore: the trail | ⌘2, then Tab through the context strip | crumbs ending "fixations.csv record 7,214, current location" | |
| 7 | Explore: the trial view | F6 to "Trial view", then Tab to the plot | "Fixations of P17 · enc_03. Arrow keys move to the nearest fixation …" | |
| 8 | Roving cursor | Right arrow, then Enter | the mark under the cursor is announced, then selected; the trail follows the fixation | |
| 9 | Next fixation | Tab to "Next fixation", then Space | "Next fixation", button; the selection moves to fixation 7 | |
| 10 | Source records | F6 to the table, then Down | "Source records from fixations.csv …", table; the row cursor's record is read | |
| 11 | Show raw record (twice) | Tab to each toggle | "Show raw record in the source records", then "… in the inspector" | |
| 12 | Analysis | ⌘3, then F6 to Preflight, then Tab | "Save & run rev 5 · 44,845 pairs", button | |
| 13 | A running run | after Save & run: Tab to the jobs chip | its state and count; "Cancel", button (see F10) | |
| 14 | Compare: the queries | ⌘4, F6 to "Queries", then Tab | "Filter participant or item", text field | |
| 15 | Compare: the scale ladder | F6 to the contrast plot, then Tab | "Scale ladder of P17 · ret_07 contrast …"; arrows move between scales | |
| 16 | Show table | ⌃⇥ in the plot's group | the Pairs table is announced, its row cursor read | |
| 17 | Summary | ⌘4, then Tab to the crumb "Summary · by retrieval response", then Space | "Participant plot of Participant D at 2° …"; "Scale profile of run 7: …" | |
| 18 | Figures | ⌘5, then Tab through the figure list | "Figure 1, current, run 7 · rev 4 · data r3", button | |
| 19 | Figure panels | Tab through the panels | "Panel A, Encoding gaze" … "Panel E, Scale profile by response" | |
| 20 | Export | Tab to "Export bundle…", then Space | "Export bundle…", button; the OS folder chooser opens | |
| 21 | Dark | View › Appearance › Dark (via VoiceOver's menu bar, VO-M) | the same names; nothing new goes silent | |

Record the build's commit, the macOS and VoiceOver versions, and the date
with the results.

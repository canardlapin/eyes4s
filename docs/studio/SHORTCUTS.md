# Eyes Studio keyboard shortcuts

Generated from `CommandRegistry` (studio-app, ticket S1.9); do not edit by hand.
`KeymapFxSuite` fails when this file and the registry disagree; regenerate
it with `EYES4S_UPDATE_GOLDENS=1`.

⌘ is Command on macOS and Control on Linux and Windows. Every command is
also an item of the menu named in the first column. Inside a plot, the
arrow keys move a roving cursor, Enter selects and Esc clears; Tab leaves
the plot (DESIGN_SPEC section 10).

On macOS the native menu bar's accelerators are the only path for these
chords (the window's key handler skips them, `CommandRegistry.windowKeymap`),
so a key press cannot fire a command twice. A disabled command's item is
greyed out, and its chord stays the window's, which answers with the
Unavailable notice ("Undo: There is no edit to undo.").

Verified by hand on macOS: pending. The hand check covers:

- A press fires its command once, never twice.
- With nothing to undo, ⌘Z shows the Unavailable notice or beeps, and never undoes twice.
- While a text field has focus, the native menu sees ⌘Z (and ⌘⇧Z) before the
  field. Expected: the field's own undo wins while it has focus; the
  document's Undo applies only outside text fields. Pending: confirm which
  one wins today.

| Menu | Command | Shortcut | Id |
|---|---|---|---|
| File | Import sources… | — | `data.import` |
| File | Rename… | — | `project.rename` |
| File | Reveal in Finder | — | `project.reveal` |
| File | Project info | — | `project.info` |
| Edit | Undo | `⌘Z` | `edit.undo` |
| Edit | Redo | `⌘⇧Z` | `edit.redo` |
| Edit | Undo view change | — | `view.undo` |
| Edit | Redo view change | — | `view.redo` |
| View | Data | `⌘1` | `perspective.data` |
| View | Explore | `⌘2` | `perspective.explore` |
| View | Analysis | `⌘3` | `perspective.analysis` |
| View | Compare | `⌘4` | `perspective.compare` |
| View | Figures | `⌘5` | `perspective.figures` |
| View | Reset perspective | — | `view.reset-perspective` |
| View | Appearance › Light | — | `view.appearance-light` |
| View | Appearance › Dark | — | `view.appearance-dark` |
| View | Appearance › System | — | `view.appearance-system` |
| Go | Back | `⌘[` | `navigate.back` |
| Go | Forward | `⌘]` | `navigate.forward` |
| Run | Cancel run | — | `run.cancel` |
| Run | Show the finished run | — | `run.show` |
| Run | Review draft in Analysis | — | `draft.review` |
| Run | Discard draft | — | `draft.discard` |
| Window | Next pane | `F6` | `pane.next` |
| Window | Previous pane | `⇧F6` | `pane.previous` |
| Window | Next tab | `⌃⇥` | `tab.next` |
| Window | Previous tab | `⌃⇧⇥` | `tab.previous` |
| Window | Maximize the focused group | `⌘⇧↩` | `pane.maximize` |

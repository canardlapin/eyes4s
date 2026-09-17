# Eyes Studio: a desktop workbench for eye-movement research

Proposal, 2026-09-15. Working name only. The proposed application lives in a separate
repository and uses JavaFX for its interface and Intaglio for scientific graphics.
This document records product direction; it does not declare the application or
every library prerequisite implemented.

## The promise

**Open an eye-tracking study, see how people looked, ask a precise comparison
question, and leave with an understandable result and a beautiful, reproducible figure.**

The first audience is a researcher with fixation tables from a perception or memory
experiment. They know participants, stimuli, phases, and conditions; they should not
need to write Scala or know the library's module structure. A method developer should
recognize the same eyes4s analyses underneath every control and exported result.

The first complete journey is encoding-to-retrieval comparison: inspect the two
gaze patterns, compare the matching stimulus against eligible other-stimulus controls,
understand the per-trial contrast, inspect its variation across participants, and
export the result. This gives the app a distinctive purpose beyond displaying heatmaps.

eyesim supplies the scientific questions: fixation patterns, spatial distributions,
scanpath similarity, matched templates, repetition, and changes across time and scale.
The interface organizes those questions into a study workflow. It does not mirror R
function names or expose a menu entry for every available Scala primitive.

## The experience

One persistent workspace has three destinations: **Data**, **Explore**, and **Compare**.
Saved analyses appear in the project navigator. **Export** opens a figure and table
export sheet from the current result. Import setup is guided; ordinary exploration
does not require repeatedly stepping through a wizard.

The main workspace gives most of its width to the stimulus and scientific views.
A compact left navigator selects participants, stimuli, trials, and saved analyses.
A contextual right inspector explains and adjusts the current selection. A shallow
bottom timeline links temporal exploration to the spatial view. Panels resize and
collapse; the canvas remains useful on a laptop.

The central comparison view contains two synchronized stimulus panels, a compact
result strip, and a participant/trial result plot. Selecting a result brings its
focal trial, matched reference, and contributing controls into view. The user can
follow a number back to the observations that produced it.

### Data: establish what the study contains

Drop a fixation CSV and an optional folder of stimulus images onto the window.
The importer proposes column mappings, then asks for the scientific facts it cannot
infer reliably: time units, spatial units, frame bounds, axis direction, and trial
identity. Remember mappings as import presets. Stimuli without images use a labelled
coordinate plane; an absent image does not prevent an analysis.

Show a data preview and an interactive geometry preview together. A flipped axis or
incorrect image placement should be visible before a participant starts interpreting
a heatmap. Image placement is an explicit transform into the declared gaze frame;
window resizing changes the view, not the data coordinates.

Summarize participants, stimuli, phases, trials, fixations, and rejected records.
Each issue opens the relevant row or trial and explains the remedy. The existing
importer quarantines a whole keyed trial when one of its fixation rows is invalid;
the UI must retain that rule and make any reviewed exclusion explicit. Preserve the
source and decisions so that corrected imports can be compared with earlier ones.

Trial identity needs particular care: repeated presentations of the same stimulus in
the same phase require an occurrence/session key. The default three-field `StudyKey`
does not prove that distinction; admit it through the existing typed custom-key/layout
extension or add a supported library layout before advertising this import route.

With fixation summaries alone, report fixation coverage and admission diagnostics.
Do not infer raw-sample quality, blink rates, or observed recording support from gaps
between fixation events.

### Explore: make the data legible

Offer four coordinated views: fixation points, ordered scanpaths, duration-weighted
spatial maps, and small multiples across trials or participants. Each starts from a
carefully composed preset with visible units and a useful legend. Stimulus images
remain readable beneath adjustable overlays.

Click a fixation to highlight its row and timeline interval. Brush time to inspect a
portion of a trial. Scrub a playhead to reveal fixation order. Provide play/pause and
an explicit playback-speed control; animation is an explanatory aid, with precise
static selection always available. Dragging over space selects observations without
silently changing the cohort used by a saved analysis.

The inspector contains meaningful controls: duration versus uniform weighting,
spatial scale in the current units, grid resolution, edge handling, opacity, and
shared versus per-panel colour limits. Put scientific parameters together and
presentation controls together. A palette change needs no scientific rerun; a
bandwidth change does.

Fixation marker **area** represents duration. Sequence lines show order between
fixations and are labelled accordingly; they are not reconstructed measured saccade
trajectories. Maps identify whether values are mass per cell or density. Comparing
panels use a common spatial extent and, by default, common colour limits.

### Compare: ask a question, inspect its construction

The first analysis recipe is **Compare retrieval with encoding**. The form reads like
an analysis specification:

- Compare these two phases within each participant.
- Match trials on stimulus identity.
- Use all eligible other-stimulus references as controls.
- Build duration-weighted maps on this frame and grid.
- Compare using cosine similarity at these named spatial scales.
- Retain failures and apply the selected, explicit reduction policy.

Before running, show an example matched pair, its actual control list, the number of
eligible comparisons, unmatched trials, and any ambiguous matches. Use the verified
exhaustive control design first; arbitrary control rules and finite random sampling
are subsequent capabilities with their own evidence.

Results place matched similarity, mean control similarity, and their difference
beside one another. A positive difference means the matching template scored higher
under that method. It does not establish statistical significance. Show selected,
successful, failed, and contributing counts where they affect interpretation.

At study level, show participant points and descriptive distributions. Any reduction
across trials or participants names its unit and weighting. Do not display inferential
intervals or treat pair counts as independent sample sizes without an explicit supported
statistical procedure. A row selection reveals its pair scores and source trials.

Multiple spatial scales become aligned small multiples or a scale profile with units.
Keep each scale visible; a scale slider must not silently turn into a best-score search
or an implicit average. Saved variants make sensitivity checks easy to revisit.

### Export: finish the research task

Offer three deliverables: a polished figure, keyed result tables, and a portable
project containing the analysis needed to reproduce them. A figure export sheet
controls physical size, text size, panel labels, stimulus visibility, and caption.
Preview the export layout at its intended aspect ratio.

Use Intaglio SVG/PDF for figures and PNG for raster consumers. Stimulus photographs
and density rasters remain embedded raster layers; paths, labels, and compatible
marks remain vector. Rebuild layout for the selected export target using the same
scientific values, scales, and visual specification. Do not screenshot the application.

Generate an editable methods summary from the actual run: input identity, inclusion
decisions, frame and units, weighting, grid, bandwidth, edge policy, matching,
controls, metric, reduction, and method versions. The app owns the caption template;
scientific method descriptions should ultimately come from eyes4s metadata.

## Visual direction

The visual character is a quiet scientific studio: warm off-white canvas, ink-coloured
type, hairline divisions, generous plot margins, and a restrained teal accent for
selection and primary actions. Give stimuli and measured data the visual emphasis.
Use a small, coherent set of icons and native-feeling menus, shortcuts, focus rings,
file dialogs, and text editing.

Typography and alignment carry the design. Use a readable interface face, tabular
numerals for measurements, consistent spacing, and clearly differentiated titles,
axis labels, and annotations. Bundle a suitable licensed font for reproducible figure
output and verify actual JavaFX and export metrics. Avoid walls of boxed controls;
related settings sit in short labelled groups in the inspector.

Use perceptually ordered sequential colours for nonnegative maps and a diverging
palette centred at zero for signed differences. Distinguish missing values from zero
with an additional visual treatment. Colour is never the only signal for selection,
failure, phase, or sign. Keep screen contrast and print legibility as acceptance criteria.

Interaction should feel immediate: hover reveals useful details, selection links
views, and keyboard navigation offers the same route as clicking. Animation is brief
and purposeful. Empty states explain the next concrete action. Failed states identify
the affected file or trial. Loading states retain context instead of blanking the view.

## A bounded MVP

| First release | Extension after the first release |
|---|---|
| Fixation CSV admission, saved mapping, geometry preview, optional static images | General raw-recording import, synchronization, preprocessing, detector editors |
| Trial navigator, fixation/scanpath/map views, linked selection, timeline inspection | Video stimuli, advanced playback/export, AOI drawing and editing |
| Saved matched/control cosine studies with explicit weighting, grid and scales | Additional verified metrics, structured MultiMatch workspaces, flexible repetition designs |
| Pair/trial results, descriptive participant plots, diagnostics and exclusions | Inferential modeling, training-only template fitting, advanced GazeWeave methods |
| Save/reopen/rerun, figure and CSV export, methods summary | Collaboration, plugin discovery, acquisition and live device connections |

Temporal analysis is the first candidate expansion: named windows, explicit anchors,
observed support, boundary treatment, and repetition identity feeding the existing
`TemporalStudyPlan`. Include it in the initial release only if the entire project can
save, reopen, and rerun those inputs and plans. Brushing a displayed timeline is not
already a persistent temporal analysis. The current duration-window workflow and
eyesim's static-template `sample_density_time` answer different questions.

The core MVP is complete when a researcher can perform the fixation-to-contrast journey
without a terminal, explain an exclusion and the control set, reopen the project, and
export a useful figure plus the underlying keyed data. Method count is secondary.

## JavaFX, Intaglio, and eyes4s responsibilities

**JavaFX owns the desktop application:** windows, controls, tables, navigation,
keyboard access, pointer events, dialogs, and lifecycle. Keep ordinary interface text
and controls as accessible JavaFX nodes. Canvas marks require a semantic selection
model and keyboard-accessible details/table; drawing them does not make them accessible.

**Intaglio owns scientific graphics:** plot and scene descriptions, axes, scales,
legends, typography, composition, and renderer lowering. A thin eyes-specific adapter
maps eyes4s values to Intaglio scenes, retaining typed trial/fixation identities in a
sidecar for selection. It must not reimplement density estimation or similarity.

Use one spatial transform for the image, gaze marks, and spatial picking, and one
time mapping shared by the timeline, brush, and playhead. Use Intaglio's shared picking/state contracts for
plot selection; supply the missing JavaFX input adapter and accessibility bridge as
bounded integration work. Selection targets and underlying trial/observation keys
remain distinct, especially for aggregate maps.

**eyes4s owns the science:** admission rules, coordinates and clocks, scanpaths,
occupancy and density, matching, comparisons, windows, failures, plan execution, and
method/schema identities. Library improvements remain in eyes4s; no JavaFX dependency
enters its pure modules. Generic renderer improvements belong in Intaglio.

The app owns an immutable document model with three distinct parts:

1. Scientific inputs, explicit inclusion decisions, and versioned analysis plans.
2. Immutable completed runs tied to those exact inputs and plans.
3. Presentation state: selected trials, viewport, colours, panel layout, and captions.

Draft edits need not form a valid eyes4s plan yet. Parse them into domain values as
the user completes the form, show local remedies, and execute only a valid plan.
Undo/redo works on document edits. Changing science marks previous results as belonging
to the earlier revision; it never silently relabels them as current.

Run numerical work and scene compilation away from the JavaFX application thread;
draw and update controls on that thread. Use one app execution service with versioned
requests, bounded work, progress, cancellation, and rejection of stale completions.
Cancellation must propagate into the actual computation. Wrapping the current
synchronous `StudyPlan.run` in a JavaFX task alone cannot establish responsive
cancellation; cooperative checkpoints or an effectful interpreter are library work.
OpenJFX's [Task contract](https://openjfx.io/javadoc/17/javafx.graphics/javafx/concurrent/Task.html)
documents the host threading and cooperative cancellation requirements.

Save a versioned project manifest, portable asset references, original inputs or
explicit external references, admission decisions, plans, and figure specifications.
Reuse eyes4s plan codecs. Add app schemas for workspace state, image registration,
and artifact storage; `StudyCodec` alone is not a whole project format. Missing assets,
changed content, and unsupported schemas get actionable recovery screens. Autosave
atomically and keep the last valid project recoverable. Derived caches are optional
and never substitute for original inputs and plans.

Propose macOS as the first fully qualified desktop target, with a bundled runtime so
researchers do not install Java themselves. Choose exact JDK/OpenJFX/Intaglio versions
in the integration prototype and pin them. The OpenJFX [runtime-image guidance](https://openjfx.io/openjfx-docs/)
supports a self-contained distribution; Windows and Linux need their own packaging
and native interaction checks before being advertised.

## Build in five demonstrable steps

1. **Prove the visual and interaction foundation.** Package a JavaFX window containing
   two stimulus views, an Intaglio scanpath/map, a timeline, and a linked result table.
   Use the same selection identities across views. Prove resize, HiDPI coordinates,
   fonts, keyboard selection, disposal, and SVG/PDF export. Inspect native rendered
   output; headless drawing-command tests alone do not establish appearance.
2. **Complete one polished vertical slice.** Open the existing matched/control fixture,
   run the public saved study, show the source pairs and contrast, save/reopen, and
   export a figure and table. UI-driven and direct-library results must agree under
   the existing equality/tolerance contract. This is the first product demonstration.
3. **Make real imports usable.** Add arbitrary column mapping, reusable presets,
   geometry and stimulus registration, explicit exclusion review, missing-asset repair,
   and a permitted real study fixture. Test a researcher's first-use journey.
4. **Add study breadth and dependable operation.** Add multiple scales, cohort
   navigation, saved variants, result inspection, undo/autosave, bounded execution,
   progress/cancellation, and a declared supported data-size envelope.
5. **Qualify the MVP.** Test the packaged app on the target OS. Verify recovery after
   interruption, stale-result handling, keyboard access, large-study behaviour, and
   export typography. Have researchers complete import-to-figure unaided. Freeze
   datasets and target hardware for measured performance acceptance before optimizing.

Suggested responsiveness targets for the qualification fixture are under 100 ms for
selection feedback and under 250 ms for cached trial switching. These are proposed
product targets, not current measurements. Long computations report actual completed
work where available and preserve navigation and cancellation responsiveness.

## What the source review establishes

This proposal was grounded in eyes4s `6218627` and the local Intaglio checkout whose
HEAD was `5b217a6`. Source inspection is not a native UI or release qualification run.

- [The library vision](VISION.md) and [application requirements](../PRD.md#application-layer-requirements)
  already establish a separate app consuming typed, inspectable scientific plans.
- [The fixation study guide](FIXATION_STUDIES.md), [saved studies](SAVED_STUDIES.md),
  and [extensions](EXTENDING_STUDIES.md) supply a concrete first consumer route.
- [The eyesim capability baseline](EYESIM_CAPABILITIES.md) records 20 cases: one
  verified equivalent, three intentional divergences, and sixteen gaps. Some gaps
  concern external-reference evidence; others concern missing workflows. Neither
  symbol presence nor completion of the baseline inventory establishes full parity.
- Intaglio's [JavaFX backend](../../intaglio/modules/javafx/README.md) already compiles
  scenes to Canvas commands, with pure compilation and FX-thread drawing. Its headless
  conformance tests do not themselves qualify native font rendering.
- Intaglio's [interaction module](../../intaglio/modules/interaction/README.md) has
  portable state and indexed picking; native adapters remain in development.
  [Composition](../../intaglio/docs/composition.md) and [export backends](../../intaglio/docs/backends.md)
  provide useful foundations for aligned comparison panels and figures.

The immediate next deliverable is the first two steps together: a beautifully finished
JavaFX comparison workspace around the already verified fixation-study example, with
linked views, save/reopen, and a figure export that looks as considered as the screen.

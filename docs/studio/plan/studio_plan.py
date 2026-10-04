"""Eyes Studio implementation plan: the single source for IMPLEMENTATION_PLAN.md and the Mote beads.

    python3 docs/studio/plan/studio_plan.py render            # rewrite docs/studio/IMPLEMENTATION_PLAN.md
    python3 docs/studio/plan/studio_plan.py mote --dry-run    # print the mote commands
    python3 docs/studio/plan/studio_plan.py mote              # create missing beads, record ids.json
    python3 docs/studio/plan/studio_plan.py sync              # update text of existing beads that changed

Point A: eyes4s core (StudyPlan, preflight, preview, fs2 execution, codecs, diagnostics) plus the
UI-A..UI-H core tickets; no studio code. Point B: the approved design in docs/studio/design/
(canvas https://claude.ai/artifact/5sk2BaePcrMCYohRT6Vg2v), running as a packaged macOS app.
"""
import json, os, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
IDS = os.path.join(HERE, "ids.json")
ACTOR = "claude-studio-design"

# Existing core tickets the studio depends on (eyes4s core-readiness epic).
EXT = {
    "UI-A": "bd-01M3DH2F9QK95DYA2VBHJ5FA9Y",  # analysis window, px/deg, frame corrections
    "UI-B": "bd-01M3DH2FRNZG7PFZ57SJ632Y18",  # matched cardinality, occurrence-aware layout
    "UI-C": "bd-01M3DH2GCC15HC73EGC0G8GBS3",  # ReportSpec
    "UI-D": "bd-01M3DH2GZ458TTHRH2GBE0C49M",  # progress stages, stale rejection, preview counts
    "UI-E": "bd-01M3DH2HGXSF41GET6C3WAHWTX",  # density grids from archives
    "UI-F": "bd-01M3DH2J1G3Z52RY0ESABW87WX",  # initial-fixation policy, plan.diff
    "UI-G": "bd-01M3DH2JHHC5HKFD41DZS0W2GE",  # provenance, paging, ResultRef
    "UI-H": "bd-01M3DH2K0K354FKKPX0VKNPC0Y",  # FixationCsv extras, trials inventory
    "CR2": "bd-01M3DH0RGYVC6TQXYD995PREY4",  # pure result-table layer
    "CR3": "bd-01M3DH0RQX581MB2X1J64C05B0",  # schema upcasting
    "CR6": "bd-01M3DH0S5ZKFCN47MHDM0XWW10",  # form-grade descriptors
}

SPEC = "docs/studio/DESIGN_SPEC.md"
D = "docs/studio/design/"

EPICS = [
    (
        "S0",
        "Studio S0: module, toolchain, headless FX harness, CI",
        0,
        "Create the `studio` module family inside eyes4s with pinned JavaFX/scaladock/Intaglio, boundary rules, a headless JavaFX test harness and CI. Nothing user-visible.",
    ),
    (
        "S1",
        "Studio S1: design system and application shell",
        0,
        "Tokens, fonts, icons, themes, and the shell every perspective shares: app bar, perspectives (one scaladock Dock each), context strip (trail + freshness), draft banner, status bar, menus and keymap. Reference: "
        + D
        + "System.dc.html and the shell of every board.",
    ),
    (
        "S2",
        "Studio S2: project document, revisions and persistence",
        0,
        "StudioDocument, commands with undo, dataset/analysis revisions and drafts, the .eyes project bundle, atomic save and recovery, RunStore, freshness derivation. Pure and headless-testable (no JavaFX).",
    ),
    (
        "S3",
        "Studio S3: execution, selection and navigation services",
        0,
        "ExecutionService over eyes4s-fs2 (jobs, progress, cancel, stale rejection), preview paging, SelectionBus with typed StudioRef, navigation history and the provenance trail, diagnostics presentation and StudioChecks.",
    ),
    (
        "S4",
        "Studio S4: graphics adapter (studio-viz)",
        0,
        "Intaglio scenes on a JavaFX canvas with HiDPI, picking and a roving keyboard cursor; TrialView; map rasters with cased isolines; the plot kit (scale ladder, participant plot, scale profile, timeline), each with a Table twin. The adapter never recomputes science.",
    ),
    (
        "S5",
        "Studio S5: Data perspective",
        1,
        "First run, import wizard, trial identity, inventory join, geometry, admission ledger, assets. Boards: "
        + D
        + "DataEmpty.dc.html, "
        + D
        + "Data.dc.html.",
    ),
    (
        "S6",
        "Studio S6: Explore perspective",
        1,
        "Trials navigator, neutral TrialView, timeline with brush and playhead, source records, fixation inspector, linked selection. Board: "
        + D
        + "Explore.dc.html.",
    ),
    (
        "S7",
        "Studio S7: Analysis perspective",
        1,
        "Presets, descriptor-driven recipe, drafts and plan.diff, scales, resolved design, preflight and run card. Board: "
        + D
        + "Analysis.dc.html.",
    ),
    (
        "S8",
        "Studio S8: Compare perspective",
        1,
        "Query layout (queries, trial panels, scale ladder, readout, why-this-reference) and summary layout (participants, scale profile, table, reporting). Boards: "
        + D
        + "Main.dc.html, "
        + D
        + "MainDark.dc.html, "
        + D
        + "Results.dc.html.",
    ),
    (
        "S9",
        "Studio S9: Figures and export",
        1,
        "Figure model bound to one run + one reporting spec, composer, SVG/PDF/PNG export via Intaglio, methods generator, export bundle. Board: "
        + D
        + "Figures.dc.html.",
    ),
    (
        "S10",
        "Studio S10: qualification and release",
        1,
        "End-to-end journeys, adversarial science tests, visual parity with the design boards, accessibility, performance, packaging, researcher usability. The release gate.",
    ),
]

# Common acceptance clauses referenced by tickets.
COMMON = (
    "Definition of done for every studio ticket: tests named in the ticket pass in CI (headless); no literal colours outside the token "
    "files; no science computed in studio (values come from eyes4s results); every number shown traces to a StudioRef; board parity "
    "checked against the named board and noted in the PR; `sbt checkBoundaries` passes."
)

T = []


def t(key, epic, title, prio, board, scope, ac, tests, deps=(), tags=()):
    T.append(
        dict(
            key=key,
            epic=epic,
            title=title,
            prio=prio,
            board=board,
            scope=scope,
            ac=list(ac),
            tests=list(tests),
            deps=list(deps),
            tags=list(tags),
        )
    )


# ---------------- S0 ----------------
t(
    "S0.1",
    "S0",
    "Decision: studio module layout and toolchain",
    0,
    "—",
    "Record the owner's decision (2026-09-25): the app lives in eyes4s under `studio/` as three JVM-only sbt projects: studio-core (document, commands, services; no JavaFX), studio-viz (eyes4s values -> Intaglio scenes, semantic bindings; no JavaFX controls), studio-desktop (JavaFX UI, scaladock, packaging). Choose JDK (21 LTS unless OpenJFX pin requires newer), OpenJFX version (align with scaladock), Scala 3.7.4, sbt 1.12.14.",
    [
        "Decision note (kind decision) on this bead with alternatives considered and the chosen versions.",
        "Versions recorded in the root workspace catalog (packages.toml) or a bounded exception with exit criterion.",
        "States that eyes4s pure modules never depend on studio-*.",
    ],
    ["n/a (decision)"],
    tags=["decision"],
)
t(
    "S0.2",
    "S0",
    "Add studio-core, studio-viz, studio-desktop sbt projects with boundary rules",
    0,
    "—",
    "JVM-only projects excluded from JS aggregates; studio-core depends on eyes4s plan/codec/fs2; studio-viz on studio-core + Intaglio; studio-desktop on both + JavaFX (provided/classifier per OS) + scaladock-fx. Extend checkModuleBoundaries: no eyes4s module may depend on studio-*; studio-core and studio-viz may not depend on javafx.",
    [
        "`sbt studioCore/test studioViz/test studioDesktop/test` run with a trivial test each.",
        "`sbt checkBoundaries` fails if any eyes4s module depends on studio-* or studio-core/studio-viz resolve javafx (negative fixture test).",
        "compileAll/testAll keep working; JS builds unaffected.",
    ],
    ["BoundarySuite negative cases for both new rules."],
    deps=["S0.1"],
)
t(
    "S0.3",
    "S0",
    "Pin scaladock and Intaglio by exact artifact or full Git SHA",
    0,
    "—",
    "Default resolution is an exact version or full SHA (workspace policy); a local checkout override only via an explicit system property (e.g. -Dstudio.scaladock.local=../scaladock). No SNAPSHOT or implicit sibling in the default build.",
    [
        "Fresh clone with no sibling checkouts builds studio-*.",
        "The override property is documented in studio/README.md and proven in CI once.",
        "Any unavoidable SNAPSHOT is a recorded, bounded exception in packages.toml.",
    ],
    ["CI job 'studio-clean-clone'."],
    deps=["S0.2"],
)
t(
    "S0.4",
    "S0",
    "Headless JavaFX test harness with 1440x900 screenshot capture",
    0,
    "—",
    "Adopt scaladock's headless FX approach (Monocle or the OpenJFX headless platform, whichever the pinned version supports). Provide StudioFxSuite base (start app on a stage of given size, run on FX thread, await layout, snapshot PNG at 1x and 2x), and a robot/driver for clicks and keys.",
    [
        "A sample test opens a 1440x900 stage, clicks a button, asserts a label, writes PNG snapshots.",
        "Runs on macOS and Linux CI without a display.",
        "Snapshot helper names files <suite>/<test>/<theme>-<scale>.png and uploads them as CI artifacts.",
    ],
    ["HarnessSmokeSuite."],
    deps=["S0.2"],
)
t(
    "S0.5",
    "S0",
    "CI jobs for studio via sbt-typelevel workflow generation",
    1,
    "—",
    "Add studio test and snapshot jobs through build.sbt (githubWorkflowGenerate); never hand-edit .github/workflows.",
    [
        "githubWorkflowCheck passes; studio jobs run on PRs; screenshots uploaded.",
        "Studio jobs do not slow the core matrix (separate job).",
    ],
    ["CI green on a PR touching only studio."],
    deps=["S0.4"],
)
t(
    "S0.6",
    "S0",
    "Design reference and fixture in the repo",
    0,
    "all boards",
    "docs/studio/DESIGN_SPEC.md, docs/studio/design/*.dc.html (the approved boards), docs/studio/fixture/make_fixture.py + FIXTURE.md, this plan generator. Add docs/studio/README.md explaining how to view boards and regenerate the fixture and plan.",
    [
        "Files present; README explains each; regenerating FIXTURE.md is byte-identical.",
        "Board-parity checklist template (docs/studio/PARITY_CHECKLIST.md) lists, per board, the elements a PR must match.",
    ],
    [
        "fixture regeneration check in CI (python3 make_fixture.py; git diff --exit-code)."
    ],
)
t(
    "S0.7",
    "S0",
    "Studio acceptance fixture as a real eyes4s study (fixtures/studio-golden)",
    0,
    "—",
    "Produce fixations.csv + trials.csv + stimulus PNGs that realise FIXTURE.md exactly (960 inventory, 937/17/6, 11,520 records, 543 outside window, 259 items with 2 missing images, 480 queries -> 454/3/9/14, 19 controls, focus P17 ret_07 x enc_03 beach-042), coordinated with the core session's acceptance fixture (core-readiness note). Deterministic seed 20260925.",
    [
        "Running the eyes4s headless study consumer on the fixture reproduces every FIXTURE.md count and the focus query's M/B/D at all scales within a named Tolerance.",
        "Fixture lives once (shared with core tests), not duplicated.",
        "Stimulus images are synthetic and redistributable.",
    ],
    [
        "StudioFixtureSuite (studio-core) asserting all FIXTURE.md numbers via library calls."
    ],
    deps=["S0.6", "UI-A", "UI-B", "UI-H"],
)

# ---------------- S1 ----------------
t(
    "S1.1",
    "S1",
    "Tokens as JavaFX looked-up colours, light and dark, with stage variants",
    0,
    "System.dc.html",
    "Translate the token block (DESIGN_SPEC §4, §12) to studio.css / studio-dark.css looked-up colours on .root, plus stage variants (dark/mid/light) with --on-stage. Provide a Scala Tokens object for Intaglio scenes reading the same values (single source file generates both).",
    [
        "One source of truth generates CSS and Scala tokens; a lint fails on any hex literal in studio-desktop CSS/Scala outside the token source (stimulus art and ramps whitelisted by file).",
        "WCAG contrast test: every text token pair in the spec >= 4.5:1 (3:1 for >=24px), every graphical mark >= 3:1, in both themes, computed in a unit test.",
        "Dark theme role lightness spread >= 20 L* between query and matched.",
    ],
    ["TokenContrastSuite", "NoLiteralColourLint"],
    deps=["S0.2"],
)
t(
    "S1.2",
    "S1",
    "Bundle IBM Plex Sans/Mono and Source Serif 4 (static) with the five-size type scale",
    1,
    "System.dc.html",
    "Static font files (OFL) in resources with licences; CSS classes t11/t12/t13/t16/t28; numerals always Plex Mono (JavaFX has no tnum).",
    [
        "Fonts load at startup (Font.loadFont returns non-null for each face).",
        "Only the five sizes appear in CSS (lint).",
        "OFL licence text shipped in the app bundle.",
    ],
    ["FontLoadSuite", "TypeScaleLint"],
    deps=["S1.1"],
)
t(
    "S1.3",
    "S1",
    "Icon set and eye-aperture mark as SVGPath resources",
    2,
    "System.dc.html",
    "Stroke icons used by the boards (chevrons, minimize, maximize, pop-out, sort, jobs, back/forward, display-kind glyphs, role glyphs) as SVGPath with currentColor semantics.",
    [
        "Every icon on the boards has a named resource; icon-only buttons carry accessible text."
    ],
    ["IconCatalogSuite (all icons parse, render in both themes)."],
    deps=["S1.1"],
)
t(
    "S1.4",
    "S1",
    "Application window, app bar and Jobs chip",
    0,
    "all boards (app bar)",
    "Native decorated stage; app bar with mark + wordmark 13/600, project chip menu, perspective switcher (5 toggle buttons, shortcut hints), Jobs chip with idle / running (stage, done/total, progress bar, Cancel) / failed (n diagnostics) states bound to ExecutionService.",
    [
        "Matches board app bars at 1440x900 (parity checklist).",
        "Jobs chip reflects a fake job's stage changes within one pulse; Cancel calls cancel on the job.",
        "Perspective buttons expose selected state to accessibility.",
    ],
    ["AppBarFxSuite (snapshot light/dark; job states)."],
    deps=["S1.1", "S1.2", "S1.3", "S0.4"],
)
t(
    "S1.5",
    "S1",
    "Perspective host: one scaladock Dock per perspective, PaneRegistry, layout persistence",
    0,
    "System.dc.html (shell anatomy)",
    "StackPane of five Docks (Data, Explore, Analysis, Compare, Figures); a PaneRegistry owns singleton panes and pane lifetime; floating windows of inactive perspectives hidden; per-perspective default LayoutState; save/load per project and user; View > Reset layout; ⌘1-⌘5.",
    [
        "Switching perspectives never disposes a pane (identity test) and preserves scroll, zoom and brush state.",
        "Layouts round-trip through save/load; unknown pane types load as placeholders.",
        "Popped-out windows hide/show with their perspective.",
        "Compare hosts two layouts (summary, query) selected by trail depth.",
    ],
    ["PerspectiveHostFxSuite", "PaneRegistrySuite (pure)."],
    deps=["S1.4", "S0.3"],
)
t(
    "S1.6",
    "S1",
    "Context strip: back/forward, trail and freshness",
    0,
    "all boards (context strip)",
    "Back/forward (⌘[ ⌘]) over NavigationHistory; Trail renders the StudioRef path as live crumbs (current in ink 600), crossing perspectives; freshness badge 'Analysis rev N · run M · data rK · state'; dashed draft chip 'Draft rev N · k change(s) · ready|k blockers' opening Analysis.",
    [
        "Every crumb navigates; the fixation and record crumbs switch to Explore and back.",
        "Trail carries the reporting spec and group (Summary · by retrieval response › Remembered › P17 › …).",
        "Freshness shows current / draft / stale / running / failed per S2.7.",
    ],
    ["ContextStripFxSuite", "TrailModelSuite (pure)."],
    deps=["S1.5", "S3.4", "S2.7"],
)
t(
    "S1.7",
    "S1",
    "Draft banner (Compare and Figures only)",
    1,
    "Main.dc.html, Figures.dc.html, Results.dc.html",
    "Dock-wide 30px banner shown only while a draft differs from the displayed revision or a run is in progress: 'Showing run 7 (analysis rev 4). Draft rev 5 adds σ 8° and has not been run.' with Review in Analysis / Discard draft; during a run: '… Run 8 is running — results will not replace this view until you choose Show.'",
    [
        "Banner text is generated from plan.diff (UI-F) and job state, never hard-coded.",
        "Hidden in Data, Explore and Analysis.",
    ],
    ["DraftBannerFxSuite"],
    deps=["S1.6", "S3.1", "UI-F"],
)
t(
    "S1.8",
    "S1",
    "Status bar with fixed four-slot grammar",
    1,
    "all boards (status bar)",
    "[● Selected: path] · [context hint] · [job + Cancel] · [Saved hh:mm]; always four slots.",
    [
        "Selection path updates from SelectionBus within 100 ms.",
        "Job slot mirrors the Jobs chip.",
        "Saved time reflects the last atomic save.",
    ],
    ["StatusBarFxSuite"],
    deps=["S1.5", "S3.3", "S2.4"],
)
t(
    "S1.9",
    "S1",
    "Menu bar, command registry and keymap",
    1,
    "System.dc.html (keyboard model)",
    "macOS system menu bar; every user action is a registered command with id, label, shortcut, enabled predicate; F6 cycles panes, ⌘⇧↩ maximizes focused group, ⌘1-5, ⌘[ ⌘], ⌘Z/⌘⇧Z; tab context menu (upstream hook if scaladock lacks it).",
    [
        "Keyboard-only user can reach every pane and command (scripted test).",
        "Shortcut table generated from the registry into docs.",
    ],
    ["KeymapFxSuite", "CommandRegistrySuite."],
    deps=["S1.5", "S2.2", "UP-scaladock"],
)
t(
    "S1.10",
    "S1",
    "Theme switching light/dark incl. scaladock DockTheme mapping",
    1,
    "MainDark.dc.html",
    "View > Appearance: Light / Dark / System; applies studio tokens and a DockTheme.Custom generated from them to every window including popouts.",
    [
        "Dark snapshots of every perspective match MainDark parity items.",
        "Switching theme changes no scientific value (value hash identical).",
    ],
    ["ThemeFxSuite"],
    deps=["S1.1", "S1.5"],
)
t(
    "S1.11",
    "S1",
    "Accessibility baseline for controls and single-stop plots",
    1,
    "all boards",
    "Accessible role/text on all controls; focus ring in accent; each plot/trial view is one focus stop (roving cursor from S4.2); lists and tables use row cursors; screen-reader summaries from semantic ids.",
    [
        "Accessibility tree audit: no unlabeled focusable node.",
        "Tab order per perspective recorded and reviewed.",
    ],
    ["A11yTreeSuite"],
    deps=["S1.5", "S4.2"],
)

# ---------------- S2 ----------------
t(
    "S2.1",
    "S2",
    "StudioDocument model and codecs",
    0,
    "—",
    "Pure model: Sources (path, byte digest sha256, eyes4s semantic identity), DatasetRevision (mapping, geometry, corrections, admission decision, ledger ref), AnalysisRevision (StudyPlan + studio fields), Draft, RunRef, ReportingSpec (UI-C), FigureSpec, PresentationState (kept separate). Versioned JSON codecs (circe) with fixtures.",
    [
        "Round-trip property tests for every type.",
        "Presentation state changes never change the scientific document hash.",
        "Codec fixtures pinned; upcasting path uses CR3.",
    ],
    ["StudioDocumentCodecSuite", "DocumentHashSuite."],
    deps=["S0.2", "CR3", "UI-C"],
)
t(
    "S2.2",
    "S2",
    "Commands, reducer and undo/redo",
    0,
    "—",
    "Document + Command -> Document; commands for import, mapping, geometry, admission, draft edits, save & run, discard, reporting edits, figure edits; undo/redo stack; hover/selection never commands.",
    [
        "Property: undo(do(c)) == identity for every reversible command; redo restores.",
        "Commands serialise (for autosave journal and scripting).",
    ],
    ["CommandLawsSuite"],
    deps=["S2.1"],
)
t(
    "S2.3",
    "S2",
    "The .eyes project bundle format",
    0,
    "—",
    "Directory bundle: project.json manifest, inputs/ (copied sources), mappings/, datasets/, analyses/, runs/<id>/, reporting/, figures/, cache/ (disposable). Schema version + upcasting. Sharing options (include participant metadata / stimulus images) explicit.",
    [
        "Open → save → open is lossless (golden bundle fixture).",
        "Deleting cache/ loses nothing scientific.",
        "An old-version bundle upcasts (CR3).",
    ],
    ["ProjectBundleSuite"],
    deps=["S2.1", "CR3"],
)
t(
    "S2.4",
    "S2",
    "Atomic save, single-writer lock, autosave and recovery",
    0,
    "—",
    "Write immutable artifacts first, then swap the manifest atomically; keep the last valid manifest; file lock per project; autosave journal of commands; recovery screen after crash.",
    [
        "Fault-injection test: kill during save at every step leaves a valid project.",
        "Second writer is refused with a clear message.",
        "Recovery replays the journal to the pre-crash document.",
    ],
    ["AtomicSaveFaultSuite", "RecoverySuite"],
    deps=["S2.3", "S2.2"],
)
t(
    "S2.5",
    "S2",
    "Source and asset repair",
    1,
    "Data.dc.html (missing images)",
    "Detect missing or changed sources/assets by byte digest; distinguish missing asset from blank display; Repair… relinks or re-copies and records the change.",
    [
        "Changed source at same path is detected and blocks runs until re-admitted.",
        "Missing image renders the hatched missing state, never blank.",
    ],
    ["AssetRepairSuite"],
    deps=["S2.3"],
)
t(
    "S2.6",
    "S2",
    "RunStore with retention for figure rebinding",
    1,
    "Figures.dc.html (stale Figure 2)",
    "Stores run archives under runs/<id>; lazy loading; retention keeps runs bound by figures; others prunable with confirmation; size shown.",
    [
        "A figure bound to run 5 still renders after runs 6-8 exist.",
        "Pruning never removes a figure-bound run.",
    ],
    ["RunStoreSuite"],
    deps=["S2.3", "UI-E"],
)
t(
    "S2.7",
    "S2",
    "Freshness derivation (current/draft/stale/running/failed)",
    0,
    "System.dc.html (freshness states)",
    "Pure function from (dataset revisions, analysis revisions, draft, runs, jobs) to the badge/chip/banner states; stale when data or analysis revision moved.",
    [
        "Truth-table tests cover every row of the System board states, including 'run 5 on r2 becomes stale when r3 is admitted'."
    ],
    ["FreshnessTruthTableSuite"],
    deps=["S2.1"],
)

# ---------------- S3 ----------------
t(
    "S3.1",
    "S3",
    "ExecutionService: jobs, progress, cancel, stale rejection, Show",
    0,
    "Results.dc.html (run 8 running)",
    "Wrap eyes4s-fs2 StudyExecution: Job(id, revision, stage Estimating/Comparing/Reducing/Contrasting, done/total Exact|AtMost|Unknown, state Queued/Running/Cancelling/Succeeded/Failed/Cancelled/Superseded). Off-FX-thread; results never swap under the user: 'Run N ready — Show'.",
    [
        "Cancelling stops computation within one step (measured in test).",
        "A completion for an older revision can never become the current result (property test).",
        "Failed run exposes diagnostics with stable codes.",
    ],
    ["ExecutionServiceSuite (fake + real small study)."],
    deps=["S2.1", "UI-D"],
)
t(
    "S3.2",
    "S3",
    "Preview paging service for the resolved design",
    0,
    "Analysis.dc.html (resolved design)",
    "Before paging: Cartesian candidatePairCount over admitted trials (219,486 per scale). During: 'counting eligible pairs… k of 24 participants'. After PairPage.Done: exact eligible (8,969 per scale), unmatched, ambiguous. Explicit budgets (counts only).",
    [
        "Counts equal FIXTURE.md after paging.",
        "The preview used for execution is the same prepared design (identity check via input digest + plan revision; stale preview rejected by checkCurrent).",
    ],
    ["PreviewPagingSuite"],
    deps=["S3.1", "UI-D"],
)
t(
    "S3.3",
    "S3",
    "SelectionBus and typed StudioRef",
    0,
    "Explore.dc.html, Main.dc.html",
    "StudioRef ADT: Participant, Trial, Fixation, SourceRecord, Pair, QueryContrast, ParticipantSummary, FigurePanel, plus ResultRef (UI-G). Global selection, local hover; revision-stamped inputs; projected into Intaglio InteractionControllers.",
    [
        "Selecting in any view selects in all views within 100 ms (FX test).",
        "Stale/duplicate inputs rejected (property test).",
        "Selecting an aggregate never selects its source observations implicitly.",
    ],
    ["SelectionBusSuite", "LinkedSelectionFxSuite."],
    deps=["S2.1", "UI-G"],
)
t(
    "S3.4",
    "S3",
    "Navigation history and provenance trail model",
    0,
    "Main.dc.html, Explore.dc.html, Results.dc.html",
    "Trail = path of StudioRefs with the active reporting spec and group; Explain from any number walks down (summary → group → participant → query → pair → map → fixation → record); history supports back/forward across perspectives.",
    [
        "Explain P17 from the summary lands on the P17 group with trail 'Summary · by retrieval response › Remembered › P17'.",
        "Record crumb resolves through StudySources to a fixations.csv record number or an explicit MissingSource.",
    ],
    ["TrailModelSuite", "ExplainNavigationSuite."],
    deps=["S3.3", "UI-G"],
)
t(
    "S3.5",
    "S3",
    "Diagnostics presenter and StudioChecks",
    1,
    "Analysis.dc.html (preflight)",
    "Render eyes4s Diagnostics by stable code (localised text from code, not message), severity, affected trials, remedy commands; StudioChecks labelled as studio rules: matched cardinality ≥2 blocks (until UI-B lands, then delete), empty map in window (pending UI-A).",
    [
        "eyes4s findings and Studio checks are visually and structurally separate.",
        "Each remedy opens the exact affected trials.",
        "Blocked state matches the System board example (P11 ret_05, 2 matched references).",
    ],
    ["DiagnosticsPresenterSuite", "StudioChecksSuite."],
    deps=["S3.1", "UI-B"],
)

# ---------------- S4 ----------------
t(
    "S4.1",
    "S4",
    "CanvasPlotHost: Intaglio scene on a JavaFX canvas",
    0,
    "—",
    "Pure Intaglio compile off the FX thread; draw on Canvas; HiDPI scale; resize; disposal; one shared transform for scene, marks and picking.",
    [
        "Pixel-exact snapshot at 1x and 2x for a reference scene.",
        "Resize does not change data geometry (transform test).",
        "No leaks after 1,000 open/close cycles.",
    ],
    ["CanvasPlotHostFxSuite"],
    deps=["S0.4", "S0.3"],
)
t(
    "S4.2",
    "S4",
    "JavaFX input adapter, roving cursor and selection overlay",
    0,
    "System.dc.html (keyboard model)",
    "Pointer and keyboard events → Intaglio InteractionAction; picking plan hits/nearest/lasso; one focus stop per plot with arrow keys to nearest mark, Enter selects, Esc clears; selection/hover overlay redrawn without recompiling the scene. Upstream in Intaglio where generic (UP-intaglio-input).",
    [
        "Selection feedback < 100 ms on the named machine for 11,520 marks.",
        "Keyboard-only selection of any mark in every plot.",
        "Accessible text for the focused mark from its semantic id.",
    ],
    ["InputAdapterFxSuite", "RovingCursorSuite."],
    deps=["S4.1", "UP-intaglio-input"],
)
t(
    "S4.3",
    "S4",
    "TrialView component",
    0,
    "Main.dc.html, Explore.dc.html, Data.dc.html",
    "Stage surround (dark/mid/light), image placement in screen frame, display kinds (image, blank, blank + fixation cross, cue, unknown, missing hatched), fixation marks (neutral in Explore; role shapes in Compare: query filled circle, matched diamond, control hollow circle) with 2px halos, order lines labelled as order not saccades, map layer from result grids, cased isolines 50/90%, remembered-image underlay with persistent disclosure, outside-window marks flagged.",
    [
        "Blank-display query never shows the image unless underlay is on, and then shows 'Reference image — not displayed during this trial' (also in export).",
        "Missing asset renders the hatched state distinct from blank.",
        "Result-linked maps use the run's grid values (hash equality with the archive).",
    ],
    ["TrialViewFxSuite (all display kinds × themes × stages)."],
    deps=["S4.1", "S4.4", "S2.5", "UI-A", "UI-E"],
)
t(
    "S4.4",
    "S4",
    "MapRasterCache and palettes",
    0,
    "System.dc.html (ramps)",
    "ARGB rasters keyed by (trial, σ, palette, limits), LRU bounded, rendered off the FX thread; magenta sequential ramp opaque per cell with one global opacity (default 0.6); BlueRust diverging with zero pinned; missing ≠ zero; shared vs per-panel colour limits.",
    [
        "Palette or opacity change alters no stored value (hash).",
        "Cache bound respected under 10x fixture load.",
        "Isoline levels at 50% and 90% of mass match a reference computation.",
    ],
    ["MapRasterCacheSuite", "PaletteSuite."],
    deps=["S4.1", "UI-E"],
)
t(
    "S4.5",
    "S4",
    "Plot kit with Table twins",
    0,
    "Main.dc.html (ladder), Results.dc.html, Explore.dc.html (timeline)",
    "Scale ladder (19 control dots beeswarm ±7px, B tick 3px, M diamond, ink D bar; histogram above 50 controls), participant slope/dot plot (solid vs dashed groups, missing = dashed empty marker), scale profile (log x), timeline (duration-height bars, brush, playhead), each with a synced Table tab.",
    [
        "Each plot and its table show identical values from one source.",
        "Histogram fallback above 50 controls (test with 60).",
        "Log-spaced x positions verified.",
    ],
    ["PlotKitFxSuite", "PlotTableParitySuite."],
    deps=["S4.2", "S3.3"],
)
t(
    "S4.6",
    "S4",
    "Semantic bindings and scene summaries",
    1,
    "—",
    "Every mark carries a StudioRef; SceneSemantics alt text and textSummary for each plot; picking returns scientific identity, never canvas coordinates.",
    [
        "Every mark in every plot resolves to a StudioRef (property over generated scenes)."
    ],
    ["SemanticBindingSuite"],
    deps=["S4.1", "S3.3"],
)

# ---------------- S5 ----------------
t(
    "S5.1",
    "S5",
    "Data first-run screen",
    1,
    "DataEmpty.dc.html",
    "Empty Sources navigator; fixations.csv (required) and trials.csv (recommended, with reason) drop targets; stimuli folder; preset choice (shapes import suggestions only); bundled example study; recent projects; local-only statement; 'What Eyes Studio will ask you' checklist.",
    [
        "Board parity (DataEmpty).",
        "Opening the bundled example loads fixtures/studio-golden.",
    ],
    ["DataFirstRunFxSuite"],
    deps=["S1.5", "S0.7"],
)
t(
    "S5.2",
    "S5",
    "Import wizard: column roles, declared units, presets",
    0,
    "Data.dc.html (column mapping)",
    "CSV sniffing; propose roles (participant, phase, trial, occurrence, item, x, y, onset, duration, ordinal, sample count, attributes); time units declared (never inferred); save import preset; mapping edits are 'Dataset · re-admit'.",
    [
        "Required roles enforced with typed errors naming the column.",
        "Unknown columns pass through as attributes (UI-H).",
        "Preset re-applies to a second file.",
    ],
    ["ImportWizardFxSuite", "ColumnMappingSuite."],
    deps=["S2.2", "UI-H"],
)
t(
    "S5.3",
    "S5",
    "Trial identity key builder",
    0,
    "Data.dc.html (key builder)",
    "Compose Participant + Phase + Trial (+ Occurrence) with live duplicate detection ('38 keys repeat without Occurrence'); occurrence-aware layout (UI-B).",
    ["Duplicate keys are reported with the trials, never first-match resolved."],
    ["TrialKeySuite"],
    deps=["S5.2", "UI-B"],
)
t(
    "S5.4",
    "S5",
    "trials.csv inventory join and metadata",
    0,
    "Data.dc.html (sources, ledger)",
    "Join inventory to fixation records; absent trials counted; duplicate metadata rows never multiply fixations; conflicting trial-level values are errors; without inventory, say absent trials cannot be counted.",
    [
        "Fixture: 960 inventory, 6 absent.",
        "Conflicting metadata produces a typed error naming trial and column.",
    ],
    ["InventoryJoinSuite"],
    deps=["S5.2", "UI-H"],
)
t(
    "S5.5",
    "S5",
    "Geometry panel with recorded corrections",
    0,
    "Data.dc.html (geometry)",
    "Screen frame, image placement, origin/y direction, units, px/° declared (not calibrated) with viewing distance and screen size, analysis window = image frame; representative thumbnails + all-trials density overlay; 'Mark trial as wrong orientation…' recorded as a ledger correction (UI-A); degrees from image centre, x right, y up.",
    [
        "Every change redraws thumbnails within 250 ms and creates a dataset draft.",
        "Corrections are recorded, never baked into source coordinates.",
        "Outside-window count shown (543 of 11,520 records in 409 trials for the fixture).",
    ],
    ["GeometryPanelFxSuite", "CorrectionLedgerSuite."],
    deps=["S4.3", "UI-A"],
)
t(
    "S5.6",
    "S5",
    "Admission ledger and dataset revisions",
    0,
    "Data.dc.html (admission)",
    "Counts by cause as buttons opening the rows/trials; Require complete (library, 17 quarantined) vs Review exclusions; absent trials from inventory (UI-H) shown separately; 'Admit as rN' creates a dataset revision and marks runs on older data stale.",
    [
        "Fixture: 960 = 937 + 17 (by cause) + 6.",
        "Every count opens exactly its trials.",
        "Admitting r3 marks run 5 (r2) stale.",
    ],
    ["AdmissionLedgerFxSuite"],
    deps=["S5.4", "S2.7"],
)
t(
    "S5.7",
    "S5",
    "AssetRegistry: display kinds per trial and missing assets",
    1,
    "Data.dc.html (display kinds)",
    "Per-trial display kind (image / blank / blank + fixation cross / cue / unknown) and asset reference; missing asset state + Repair; stored in the studio schema.",
    [
        "Fixture: 257 of 259 images found; forest-044 and kitchen-081 missing (P01, P24).",
        "Imagery trials sharing one blank display never collapse onto one match item.",
    ],
    ["AssetRegistrySuite"],
    deps=["S2.5", "S5.4"],
)

# ---------------- S6 ----------------
t(
    "S6.1",
    "S6",
    "Trials navigator",
    1,
    "Explore.dc.html (left)",
    "Participant → phase → trial tree with display-kind glyphs and statuses (quarantined, absent, image missing).",
    ["Fixture statuses match (P17 ret_09 absent)."],
    ["TrialsNavigatorFxSuite"],
    deps=["S1.5", "S5.6", "S5.7"],
)
t(
    "S6.2",
    "S6",
    "Explore trial view (neutral marks, preview map)",
    0,
    "Explore.dc.html (centre)",
    "TrialView with neutral marks, Points/Order/Map toggles; map preview labelled 'preview · σ 2° · not a result' (dashed); outside-window fixations flagged.",
    [
        "Preview map is visibly distinct from result maps and never shown beside a result score."
    ],
    ["ExploreTrialViewFxSuite"],
    deps=["S4.3", "S6.1"],
)
t(
    "S6.3",
    "S6",
    "Timeline with brush, playhead and playback",
    1,
    "Explore.dc.html (timeline)",
    "Fixation intervals as bars (height ~ duration), brush range, playhead, play/pause/step/speed; brushing never crops the analysis (stated).",
    [
        "Brush selects fixations in all views; the analysis document hash is unchanged by brushing."
    ],
    ["TimelineFxSuite"],
    deps=["S4.5"],
)
t(
    "S6.4",
    "S6",
    "Source records table (virtualised)",
    1,
    "Explore.dc.html (bottom)",
    "fixations.csv records with record numbers, raw screen px, image px, degrees; follows selection; one focus stop with row cursor.",
    ["Scrolls to the selected record; 11,520 records scroll smoothly."],
    ["SourceRecordsFxSuite"],
    deps=["S3.3", "UI-G"],
)
t(
    "S6.5",
    "S6",
    "Fixation inspector with used-by links",
    1,
    "Explore.dc.html (right)",
    "Index, onset, duration, raw/image/deg coordinates, source file + record + digest, trial metadata, 'Used by: ret_07 (matched) · 18 other P17 queries (as a control)' as links.",
    [
        "Fixture fixation 6 = record 7,214, onset 2160 ms, 412 ms, screen (1148, 456), image (700, 300), (+5.4°, +2.4°)."
    ],
    ["FixationInspectorFxSuite"],
    deps=["S6.2", "S3.4"],
)
t(
    "S6.6",
    "S6",
    "Linked selection in Explore",
    0,
    "Explore.dc.html (interactive)",
    "Clicking a mark, a timeline bar, a record or Prev/Next selects the same fixation everywhere; trail fixation/record crumbs update.",
    ["E2E-03 passes."],
    ["ExploreLinkedSelectionFxSuite"],
    deps=["S6.2", "S6.3", "S6.4", "S6.5"],
)

# ---------------- S7 ----------------
t(
    "S7.1",
    "S7",
    "Recipe presets (Enc→Ret, Perception→Imagery, Study→Recognition)",
    0,
    "Analysis.dc.html (recipe)",
    "Three presets over one engine; preset defines query/reference phases, match key and display conventions; Recognition shows 'No corresponding study trial (by design)' as its own category.",
    [
        "Switching preset changes only declared fields (plan.diff).",
        "Lures/novel probes never receive invented matches.",
    ],
    ["PresetSuite"],
    deps=["S2.2", "UI-B"],
)
t(
    "S7.2",
    "S7",
    "Descriptor-driven recipe form and sentence",
    0,
    "Analysis.dc.html (recipe fields)",
    "Render RecipeInspection fields (id, version, units, bounds, defaults, allowed values, meaning) as controls; recipe sentence with role-coloured tokens; fields not yet in eyes4s show 'pending UI-A/UI-F'.",
    [
        "No hard-coded field table: adding a descriptor field in eyes4s shows up without studio changes (test with a synthetic descriptor)."
    ],
    ["DescriptorFormSuite"],
    deps=["S7.1", "CR6", "UI-F"],
)
t(
    "S7.3",
    "S7",
    "Draft editing, plan.diff and Save & run",
    0,
    "Analysis.dc.html, Main.dc.html (draft chip/banner)",
    "Edits create a draft; draft diff from plan.diff ('scales +8°'); Save & run creates an immutable revision and a job; Discard; the persisted field 'Queries without a matched reference: Report as no match'.",
    [
        "Moving a slider does not create revisions (only Save & run does).",
        "Runs stay attached to their revisions.",
    ],
    ["DraftLifecycleSuite"],
    deps=["S7.2", "S3.1", "S2.7"],
)
t(
    "S7.4",
    "S7",
    "Scales editor with cell size and warnings",
    1,
    "Analysis.dc.html (scales)",
    "Chips for σ in degrees; grid 64×48 cell 0.46°; warnings σ < 2 cells and near-uniform σ; protocol scales vs exploratory labelled.",
    ["Warnings fire for 0.5° and 8° on the fixture geometry."],
    ["ScalesEditorSuite"],
    deps=["S7.2", "UI-A"],
)
t(
    "S7.5",
    "S7",
    "Resolved design table",
    0,
    "Analysis.dc.html (resolved design)",
    "Query → matched → #controls → status with filter chips (480 = 454 + 3 + 9 + 14 and by-design n/a); input digest + plan revision; counting state; one focus stop with row cursor.",
    [
        "Preview and execution consume the same prepared design (E2E-05).",
        "Every status row opens its trial.",
    ],
    ["ResolvedDesignFxSuite"],
    deps=["S3.2"],
)
t(
    "S7.6",
    "S7",
    "Preflight pane and run card",
    0,
    "Analysis.dc.html (preflight)",
    "eyes4s findings (Blocker/Warning, category, remedy, affected) and Studio checks separated; Not checked list; run card with pair count (44,845 for rev 5) and enabled/disabled with reason.",
    [
        "Run is disabled with a reason whenever any blocker exists; enabled on the fixture draft rev 5."
    ],
    ["PreflightFxSuite"],
    deps=["S3.5", "S7.5"],
)
t(
    "S7.7",
    "S7",
    "Analyses navigator (revisions and runs)",
    2,
    "Analysis.dc.html (left)",
    "Saved analyses with revisions and runs: current, stale, cancelled; draft entry.",
    [
        "Fixture history: rev 3 · run 5 · r2 · stale; rev 4 · run 6 · cancelled; rev 4 · run 7 · current; draft rev 5."
    ],
    ["AnalysesNavigatorFxSuite"],
    deps=["S2.6", "S2.7"],
)

# ---------------- S8 ----------------
t(
    "S8.1",
    "S8",
    "Compare query layout: Queries navigator and count strip",
    0,
    "Main.dc.html (left)",
    "Participants → queries with D bars (ink, zero-centred), statuses, count strip requested / contributing / failed / no match / not admitted / by design.",
    ["Fixture strip 480 = 454 + 3 + 9 + 14; by design n/a."],
    ["QueriesNavigatorFxSuite"],
    deps=["S1.5", "S3.1"],
)
t(
    "S8.2",
    "S8",
    "Query and reference trial panels with control switching",
    0,
    "Main.dc.html (panels)",
    "Pinned query panel (role Query) and reference panel (Matched or Control, role pill + shape); Back to matched reference; Table tabs; displayed-vs-remembered captions.",
    [
        "Clicking any control switches the reference panel and its header to 'Control · item'; matched identity remains reachable.",
        "Inspected pair score is never confused with the query contrast (separate readouts).",
    ],
    ["TrialPanelsFxSuite"],
    deps=["S4.3", "S8.1"],
)
t(
    "S8.3",
    "S8",
    "Scale ladder and contrast readout",
    0,
    "Main.dc.html (contrast)",
    "Per scale: M, all control dots, B, D bar; readout with hero D (28/500), M, B of 19 controls, inspected pair cosine, Prev/Next through controls ranked by cosine, confound sentence verbatim.",
    [
        "Fixture focus: 2° M 0.73, B 0.35 (19 controls), D +0.38; all four scales match FIXTURE.md.",
        "Controls averaged pairwise, never cosine to an averaged map.",
    ],
    ["ScaleLadderFxSuite"],
    deps=["S4.5", "S8.2"],
)
t(
    "S8.4",
    "S8",
    "Why-this-reference inspector",
    1,
    "Main.dc.html (inspector)",
    "Eligibility explanation for matched and control references, excluded candidates count, outside-window stats for query and reference, read-only analysis summary with 'Edit in Analysis ⌘3', reporting and appearance sections.",
    ["Explanation text generated from the prepared design, not hand-written per case."],
    ["WhyReferenceSuite"],
    deps=["S8.2", "S3.2"],
)
t(
    "S8.5",
    "S8",
    "Pairs table and scale profile tabs",
    1,
    "Main.dc.html (tabs)",
    "Pairs table (query × reference × scale × cosine × role × outside-window) and per-query scale profile.",
    ["Table values equal ladder values."],
    ["PairsTableFxSuite"],
    deps=["S8.3"],
)
t(
    "S8.6",
    "S8",
    "Compare summary layout",
    0,
    "Results.dc.html",
    "Participant plot (24 participants, Remembered vs Forgotten, linked pairs, equal-weight grand means +0.30 / +0.15, paired n 24, per-group n range 2–17, P05 failed shown as no value), scale profile (log x, protocol scales), participant table (exact FIXTURE table), Explain P17 →.",
    [
        "Every number equals FIXTURE.md.",
        "Missing is drawn differently from zero.",
        "Explain carries spec and group into the query layout.",
    ],
    ["CompareSummaryFxSuite"],
    deps=["S4.5", "S8.7", "S3.4"],
)
t(
    "S8.7",
    "S8",
    "Reporting editor (no rerun)",
    0,
    "Results.dc.html (reporting)",
    "Group by trial covariate (retrieval response), filters (exclude queries with >25% duration outside window), minimum queries per group (default OFF, shows what turning it on would drop), weighting, saved specs, 'used by Figure N', estimand/confound text.",
    [
        "Changing reporting reuses run pair scores (no job started).",
        "Hit-only style filters never change the control pool (E2E-09).",
    ],
    ["ReportingEditorSuite"],
    deps=["S2.1", "UI-C"],
)
t(
    "S8.8",
    "S8",
    "Run-in-progress behaviour",
    0,
    "Results.dc.html (banner, Jobs chip)",
    "While run N+1 runs, views stay on run N with the banner; on completion 'Run N+1 ready — Show'; selection preserved by stable ids after Show.",
    ["Results never swap without the user choosing Show (E2E-08)."],
    ["RunInProgressFxSuite"],
    deps=["S3.1", "S1.7"],
)
t(
    "S8.9",
    "S8",
    "Compare empty state",
    2,
    "System.dc.html (no run yet)",
    "'No run yet — Open Analysis ⌘3'.",
    ["Shown for a project with no runs."],
    ["CompareEmptyFxSuite"],
    deps=["S1.5"],
)

# ---------------- S9 ----------------
t(
    "S9.1",
    "S9",
    "Figure model: one run + one reporting spec; stale and rebind",
    0,
    "Figures.dc.html (left, inspector)",
    "FigureSpec binds exactly one run and one reporting spec; panels choose scale and trial selection only; stale detection (run 5 · rev 3 · data r2) with Rebind… / Keep as rev 3.",
    [
        "A figure always renders from its bound run, never the current one.",
        "Rebind shows the plan/data diff before applying.",
    ],
    ["FigureModelSuite"],
    deps=["S2.6", "S2.7"],
)
t(
    "S9.2",
    "S9",
    "Figure composer",
    1,
    "Figures.dc.html (page)",
    "Journal widths (89/183 mm); panel templates A–E (encoding gaze, retrieval gaze with display disclosure, matched/controls maps with 'highest of 19 · B 0.35' labelling, participant D on Δ cosine axis with zero rule, scale profile log x); Plex Sans 7pt, panel letters 8pt 600; serif only for methods.",
    [
        "Panel C never places a single control score where it could be read as B.",
        "Panel D draws all 24 participants and the per-group n range.",
    ],
    ["FigureComposerFxSuite"],
    deps=["S9.1", "S4.5", "S4.3"],
)
t(
    "S9.3",
    "S9",
    "Export SVG/PDF/PNG via Intaglio",
    0,
    "Figures.dc.html (export)",
    "Layout rebuilt for the target (not a screenshot); rasters embedded for maps; greyscale check toggle; fonts embedded.",
    [
        "Exported SVG/PDF snapshots match goldens.",
        "Imagery underlay disclosure survives export (E2E-10).",
    ],
    ["FigureExportSuite"],
    deps=["S9.2", "UP-intaglio-raster"],
)
t(
    "S9.4",
    "S9",
    "Methods generator",
    0,
    "Figures.dc.html (methods.md)",
    "Generate methods.md from the run, plan descriptors and reporting spec: dataset revision, ledger, eligible 457 of 480, controls 19/18 rule, window + outside share (543 of 11,520), initial fixation policy, declared px/°, cell size, scales, cosine, require-all, participant means equal weight, group n range, spatial-not-replay, confound sentence; editable with Regenerate / Show diff.",
    [
        "Every number in the text is generated (golden text for the fixture).",
        "Editing then regenerating shows a diff and never silently discards edits.",
    ],
    ["MethodsGeneratorSuite"],
    deps=["S9.1", "CR6"],
)
t(
    "S9.5",
    "S9",
    "Export bundle and result tables",
    0,
    "Figures.dc.html (bundle)",
    "figure.svg/pdf/png, results.csv, comparisons.csv, participants.csv, methods.md, project snapshot; keyed rows; empty cells mean missing, never zero; status and reason columns (CR2 result tables).",
    ["CSV values equal on-screen values and direct library output for the fixture."],
    ["ExportBundleSuite"],
    deps=["S9.3", "S9.4", "CR2"],
)

# ---------------- S10 ----------------
t(
    "S10.1",
    "S10",
    "E2E golden journey (headless driver and UI)",
    0,
    "all boards",
    "E2E-01: new project → import fixture → map columns → admit r3 → explore P17 enc_03 → Analysis rev 4 → run → Compare query P17 ret_07 → summary → figure → export → close → reopen → identical. Two routes: StudioDriver (commands, headless) and TestFX UI script; both compare with a direct eyes4s run.",
    [
        "All FIXTURE.md numbers asserted along the way.",
        "UI route, headless route and direct library agree exactly.",
        "Reopened project is byte-identical in scientific content.",
    ],
    ["E2E-01 GoldenJourneySuite (headless)", "E2E-01-ui GoldenJourneyFxSuite."],
    deps=["S5.6", "S6.6", "S7.6", "S8.6", "S9.5", "S2.4", "S0.7"],
)
t(
    "S10.2",
    "S10",
    "E2E scenario catalogue (E2E-02 … E2E-12)",
    0,
    "all boards",
    "E2E-02 first run to bundled example; E2E-03 linked selection in Explore; E2E-04 draft → Save & run → stale marks; E2E-05 preview = executed design; E2E-06 cancel mid-run leaves run 7 current; E2E-07 stale completion rejected; E2E-08 run in progress never swaps (Show); E2E-09 reporting change reuses pair scores and keeps control pool; E2E-10 imagery underlay disclosure survives export; E2E-11 missing asset repair; E2E-12 crash during save recovers.",
    ["Each scenario is an automated test in CI with screenshots at key steps."],
    ["E2E-02 … E2E-12 suites."],
    deps=["S10.1"],
)
t(
    "S10.3",
    "S10",
    "Adversarial science tests through the app",
    0,
    "—",
    "All-identical maps → D = 0; empty map fails (not zero); reordering fixed weighted positions leaves spatial scores unchanged; duplicate matched references block (StudioCheck/UI-B); adding an unrelated participant leaves others unchanged; blank-display imagery trials never collapse onto one match; palette/opacity change alters no value; cancelled job cannot overwrite a newer run.",
    [
        "Each property runs through the studio command route and matches the direct library."
    ],
    ["AdversarialScienceSuite"],
    deps=["S10.1", "S3.5"],
)
t(
    "S10.4",
    "S10",
    "Visual parity with the approved boards",
    0,
    "all boards",
    "Screenshot goldens per perspective at 1440×900 (light and dark, 1x/2x) seeded with the fixture at the story moments t1/t2/t3; human sign-off against docs/studio/design with the parity checklist; perceptual-diff gate in CI.",
    ["Every board has a signed parity checklist; deviations recorded with reasons."],
    ["VisualParitySuite"],
    deps=["S10.1", "S1.10"],
)
t(
    "S10.5",
    "S10",
    "Accessibility audit",
    1,
    "all boards",
    "Keyboard-only E2E-01; VoiceOver pass on macOS; contrast suite; every plot has a Table twin; focus order reviewed.",
    [
        "Keyboard-only golden journey passes.",
        "Audit report in docs/studio/A11Y_AUDIT.md.",
    ],
    ["KeyboardJourneySuite"],
    deps=["S10.1", "S1.11"],
)
t(
    "S10.6",
    "S10",
    "Performance qualification",
    1,
    "—",
    "Freeze a named machine and workloads (fixture and 10× fixture); targets: selection feedback < 100 ms, cached trial switch < 250 ms, perspective switch < 150 ms, memory bound documented.",
    ["Measurements recorded in docs/studio/PERFORMANCE.md with machine and commit."],
    ["PerfHarness (JMH or FX timing)."],
    deps=["S10.1", "S4.4"],
)
t(
    "S10.7",
    "S10",
    "macOS packaging",
    1,
    "—",
    "jpackage app with bundled runtime, icons, file association for .eyes, signing/notarization decision.",
    [
        "Fresh Mac opens the app, the bundled example and a saved project with no JDK installed."
    ],
    ["PackagedSmokeTest (manual script + CI build)."],
    deps=["S10.1"],
)
t(
    "S10.8",
    "S10",
    "Researcher usability qualification",
    2,
    "—",
    "Protocol: researchers who did not build the app import a study, explain one data issue, explain one query's control set, compare two scales, inspect an unexpected score, reopen, export a figure and table; success = correct explanation of what was compared, excluded, what the number means and does not establish.",
    ["Protocol and results in docs/studio/USABILITY.md; issues filed."],
    ["n/a (study)"],
    deps=["S10.4", "S10.7"],
)

# Upstream coordination (other repositories; mirror in their trackers).
UP = [
    (
        "UP-intaglio-input",
        "Upstream Intaglio: JavaFX input adapter and selection overlay",
        0,
        "Pointer/keyboard → InteractionAction, HiDPI viewport mapping, selection overlay without scene recompile. Generic parts belong in intaglio's javafx module; mirror as an issue in the intaglio tracker and link it here.",
    ),
    (
        "UP-intaglio-raster",
        "Upstream Intaglio: scalar-field raster layer with legend, raster embedding in SVG/PDF",
        1,
        "Needed for result maps in figures. Mirror in the intaglio tracker.",
    ),
    (
        "UP-scaladock",
        "Upstream scaladock: keyboard navigation, tab context-menu hook, pane retention across load",
        1,
        "Nice-to-have upstream; studio works around via PaneRegistry and one Dock per perspective. Mirror in the scaladock tracker.",
    ),
]

GATES = [
    (
        "G0",
        "Studio gate G0: integration contract",
        0,
        "Pinned deps; headless harness; shell with perspectives; a TrialView on the fixture with picking, keyboard selection, HiDPI, resize, disposal; real cancellation of a running job.",
        ["S0.3", "S0.5", "S1.5", "S4.2", "S4.3", "S3.1"],
    ),
    (
        "G1",
        "Studio gate G1: finished golden slice",
        0,
        "Fixture → saved plan → run → Compare query view → source inspection → save/reopen → export, with UI, headless and direct-library agreement.",
        ["S2.4", "S8.3", "S6.6", "S9.5", "S10.1"],
    ),
    (
        "G2",
        "Studio gate G2: real-study admission",
        1,
        "CSV mapping, trial identity, inventory, geometry, admission, display semantics: a permitted real memory study imports without studio code changes.",
        ["S5.2", "S5.3", "S5.4", "S5.5", "S5.6", "S5.7"],
    ),
    (
        "G3",
        "Studio gate G3: qualified analysis workflow",
        1,
        "Presets, descriptor form, drafts, multiscale runs, resolved design, preflight, summary and reporting; adversarial science tests pass.",
        ["S7.6", "S7.3", "S8.6", "S8.7", "S10.3"],
    ),
    (
        "G4",
        "Studio gate G4: durable daily use",
        1,
        "Autosave/recovery, asset repair, run store, figures with rebinding, complete export, E2E catalogue green.",
        ["S2.5", "S2.6", "S9.1", "S10.2"],
    ),
    (
        "G5",
        "Studio gate G5: release — the approved design, shipped",
        1,
        "Visual parity signed for every board, accessibility audit, performance qualification, packaged macOS app, researcher usability. This is point B.",
        ["S10.4", "S10.5", "S10.6", "S10.7", "S10.8"],
    ),
]


# ---------------------------------------------------------------------------------------------
# Review amendments (2026-09-26, fresh-context plan review: READY AFTER FIXES). Applied as edits
# to the ticket list so the history of the plan stays legible.
# ---------------------------------------------------------------------------------------------
PREAMBLE = (
    "Studio work proceeds behind a `StudyBackend` interface (S3.0). Core tickets (UI-A…UI-H, CR2, CR3, CR6) are "
    "*swap dependencies*: a studio ticket merges when its acceptance criteria pass on `FakeStudyBackend` (which serves "
    "docs/studio/fixture/fixture.json), and S3.7 re-runs those criteria on the real backend once the core tickets land. "
    "Only S0.7b and S3.7 block on core tickets. Module placement: studio-core (document, commands, services, backend; no "
    "JavaFX), studio-viz (pure Intaglio scene builders; no JavaFX), studio-desktop (all JavaFX: canvas host, input, "
    "controls, docking, packaging). JDK 25 LTS for studio-* (JavaFX 24 needs JDK ≥ 22); core modules unchanged. Counts "
    "on the boards equal FIXTURE.md; scores equal fixture.json on the fake backend and fixtures/studio-golden/SCORES.json "
    "(generated from real eyes4s output, never hand-edited) on the real backend."
)


def _find(k):
    for x in T:
        if x["key"] == k:
            return x
    raise KeyError(k)


def _drop(k):
    T.remove(_find(k))


def _retarget(old, new):
    for x in T:
        x["deps"] = [
            d
            for d in sum(([n for n in new] if d == old else [d] for d in x["deps"]), [])
        ]
        x["deps"] = list(dict.fromkeys(x["deps"]))


def _deps(k, add=(), remove=()):
    x = _find(k)
    x["deps"] = [d for d in x["deps"] if d not in remove] + [
        d for d in add if d not in x["deps"]
    ]


def _insert_after(k, *new):
    i = T.index(_find(k)) + 1
    for j, n in enumerate(new):
        T.insert(i + j, n)


def _mk(key, epic, title, prio, board, scope, ac, tests, deps=(), tags=()):
    return dict(
        key=key,
        epic=epic,
        title=title,
        prio=prio,
        board=board,
        scope=scope,
        ac=list(ac),
        tests=list(tests),
        deps=list(deps),
        tags=list(tags),
    )


# 1. Fixture: counts realised exactly; scores frozen from real output.
_x = _find("S0.7")
_x.update(
    key="S0.7a",
    title="Generate the studio acceptance fixture (fixtures/studio-golden) from FIXTURE.md",
    deps=["S0.6"],
    ac=[
        "fixations.csv, trials.csv and synthetic stimulus PNGs realise every FIXTURE.md count exactly: 960 / 937 / 17 by cause / 6; 11,520 records; 543 outside the window in 409 trials; 257 of 259 images; 480 → 454 / 3 / 9 / 14; 19 or 18 controls; record 7,214.",
        "Deterministic generator (seed 20260925) shared with the core acceptance fixture; stimulus images redistributable.",
        "Scores are not asserted here (the mock scores are random draws).",
    ],
    tests=["StudioFixtureCountsSuite (pure, reads the CSVs)."],
)
_insert_after(
    "S0.7a",
    _mk(
        "S0.7b",
        "S0",
        "Verify the acceptance fixture through real eyes4s and freeze SCORES.json",
        0,
        "—",
        "Run the real study pipeline on fixtures/studio-golden; write fixtures/studio-golden/SCORES.json (M, B, D per query and scale; participant summaries) with a generator; never hand-edited.",
        [
            "Every FIXTURE.md count reproduces through the library.",
            "SCORES.json regeneration is byte-identical in CI.",
            "Focus query P17 ret_07 × enc_03 present at all four scales.",
        ],
        ["StudioFixtureRealSuite"],
        deps=["S0.7a", "UI-A", "UI-B", "UI-H", "UI-C"],
    ),
)
_retarget("S0.7", ["S0.7a"])
_find("S0.6")["ac"].append(
    "docs/studio/fixture/fixture.json (generated) is committed so the fake backend and tests read it."
)

# 2. Fake backend, driver, real-backend swap.
_insert_after(
    "S2.7",
    _mk(
        "S3.0",
        "S3",
        "StudyBackend interface and FakeStudyBackend",
        0,
        "—",
        "One interface for everything studio asks of eyes4s (admission, preview paging, execution with progress, results, inspection, provenance, descriptors, reporting). FakeStudyBackend serves fixture.json exactly and lets tests hold a job at any step.",
        [
            "The fake reproduces every FIXTURE.md count and fixture.json score.",
            "A job can be held at 'Comparing 21,400 / 44,845' deterministically.",
            "BackendConformanceSuite runs against the fake now and the real backend in S3.7.",
        ],
        ["BackendConformanceSuite (fake)"],
        deps=["S0.2", "S0.6"],
    ),
    _mk(
        "S3.6",
        "S3",
        "StudioDriver: headless scripting of every command",
        0,
        "—",
        "A headless driver over commands and services so E2E scenarios compose without JavaFX; the UI E2E variants reuse its steps.",
        [
            "Every registered command is callable from the driver.",
            "E2E-01 headless runs on the fake backend in < 60 s.",
        ],
        ["StudioDriverSuite"],
        deps=["S2.2", "S3.0"],
    ),
    _mk(
        "S3.7",
        "S3",
        "Swap to the real eyes4s backend",
        0,
        "—",
        "Implement StudyBackend over eyes4s (StudyPlan, preflight, preview, fs2 execution, ResultInspection, StudySources, ReportSpec, descriptors) and re-run every fake-only acceptance criterion.",
        [
            "BackendConformanceSuite green on the real backend.",
            "E2E-01 and the adversarial suite green on the real backend with SCORES.json.",
            "Every ticket closed on the fake has its real-backend AC re-checked (list in the PR).",
        ],
        ["BackendConformanceSuite (real)", "E2E-01 (real)"],
        deps=[
            "S3.0",
            "S0.7b",
            "UI-A",
            "UI-B",
            "UI-C",
            "UI-D",
            "UI-E",
            "UI-F",
            "UI-G",
            "UI-H",
            "CR2",
            "CR3",
            "CR6",
        ],
    ),
)

# Core deps become swap deps (text only) except in S0.7b and S3.7.
CORE = set(EXT)
for _x in T:
    if _x["key"] in ("S0.7b", "S3.7"):
        continue
    sw = [d for d in _x["deps"] if d in CORE]
    if sw:
        _x["deps"] = [d for d in _x["deps"] if d not in CORE]
        _x.setdefault("swap", []).extend(sw)
for _k in (
    "S2.1",
    "S3.1",
    "S3.2",
    "S3.3",
    "S3.4",
    "S3.5",
    "S5.2",
    "S5.4",
    "S7.1",
    "S8.7",
):
    _deps(_k, add=["S3.0"])
_deps("S1.9", remove=["UP-scaladock"])
_find("S1.9")["scope"] += " Related upstream ask: UP-scaladock (non-blocking)."
_deps("S8.6", remove=["S8.7"])
_deps("S8.1", add=["S2.6"])
_deps("S6.5", add=["S2.6"])
_find("S8.3")["scope"] += " Result tables per CR2 (swap dep)."

# 4. Architecture corrections.
_find("S0.1")["scope"] = (
    "Record the decision: the app lives in eyes4s under `studio/` as three JVM-only sbt projects: studio-core (document, commands, services, "
    "StudyBackend; no JavaFX), studio-viz (pure Intaglio scene builders and semantic bindings; pureModuleSettings; no JavaFX), studio-desktop (all JavaFX: "
    "canvas host, input adapter, controls, scaladock, packaging). studio-* use JDK 25 LTS via per-project tlJdkRelease (scaladock pins JavaFX 24.0.1, which "
    "needs JDK ≥ 22); eyes4s core modules keep their current JDK targets. OpenJFX aligned with scaladock. Scala 3.7.4, sbt 1.12.14. Define the named "
    "performance machine (model, CPU, RAM, macOS version) used by S4.2 and S10.6."
)
_find("S0.2")["ac"].append(
    "AGENTS.md layout and effect rules name studio-core and studio-desktop as effect-permitted modules and studio-viz as pure."
)
_find("S0.3")["scope"] = (
    "scaladock and Intaglio are source-only pre-release: pin each by a full Git SHA (source dependency or CI publishLocal of that SHA "
    "with a fixed version); local checkout override only via an explicit system property. No SNAPSHOT or implicit sibling in the default build."
)
_find("S2.1")[
    "scope"
] += " Document JSON uses circe; scaladock layout JSON stays uPickle (owned by scaladock) and is stored as an opaque blob."
for _k in ("S4.1", "S4.2"):
    _find(_k)["scope"] = "(studio-desktop) " + _find(_k)["scope"]
_find("S4.4")["scope"] = "(studio-desktop) " + _find("S4.4")["scope"].replace(
    "isolines 50/90%", "isolines at the levels supplied with the grid (UI-E)"
)
_find("S4.4")["ac"] = [
    a.replace(
        "Isoline levels at 50% and 90% of mass match a reference computation.",
        "Isoline levels come from the backend (UI-E); studio only contours them.",
    )
    for a in _find("S4.4")["ac"]
]

# 5. Headless and parity realism.
_x = _find("S0.4")
_x["title"] = "Headless JavaFX harness, snapshots and studio CI jobs"
_x["scope"] = (
    "Follow scaladock: Linux CI under xvfb-run with -Dprism.order=sw and bundled fonts; snapshots via SnapshotParameters (scale 1 and 2); "
    "goldens are Linux-only; the macOS job runs functional FX tests without goldens. StudioFxSuite base (stage of given size, FX-thread "
    "helpers, await layout, snapshot) and a robot for clicks/keys. Studio jobs added through build.sbt (githubWorkflowGenerate), separate "
    "from the core matrix."
)
_x["ac"] = [
    "A sample test opens a 1440×900 stage, clicks, asserts, writes PNGs at 1x and 2x.",
    "Linux (xvfb, sw pipeline) and macOS jobs green; snapshots uploaded as artifacts.",
    "githubWorkflowCheck passes; no hand edits to .github/workflows.",
]
_drop("S0.5")
_retarget("S0.5", ["S0.4"])
_find("S4.1")["ac"] = [
    "Intaglio recording-context op log for a reference scene equals its golden.",
    "Linux perceptual diff ≤ 0.5% at 1x and 2x.",
    "Resize does not change data geometry (transform test).",
    "No leaks after 1,000 open/close cycles.",
]

# 6. Splits.
_x = _find("S1.5")
_x["key"] = "S1.5a"
_x["title"] = "Perspective host: one Dock per perspective and the PaneRegistry"
_x[
    "scope"
] = "StackPane of five Docks; PaneRegistry owns singleton panes and lifetimes; ⌘1–⌘5; Compare hosts the summary and query layouts selected by trail depth."
_x["ac"] = [
    "Switching perspectives never disposes a pane (identity test) and preserves scroll, zoom and brush state.",
    "Compare shows the summary layout at the Summary crumb and the query layout below it.",
]
_insert_after(
    "S1.5a",
    _mk(
        "S1.5b",
        "S1",
        "Layout persistence, Reset layout and pop-out windows",
        1,
        "System.dc.html (shell anatomy)",
        "Per-perspective default LayoutState; save/load per project and per user; View › Reset layout; pop-out windows of inactive perspectives hidden and restored.",
        [
            "Layouts round-trip; unknown pane types load as placeholders.",
            "Pop-outs hide/show with their perspective (E2E-18).",
        ],
        ["LayoutPersistenceFxSuite"],
        deps=["S1.5a", "S2.8"],
    ),
)
_retarget("S1.5", ["S1.5a"])

_x = _find("S2.4")
_x["key"] = "S2.4a"
_x["title"] = "Atomic save and single-writer lock"
_x[
    "scope"
] = "Immutable artifacts first, atomic manifest swap, last valid manifest retained, per-project file lock."
_x["ac"] = [
    "Fault injection at every save step leaves a valid project.",
    "A second writer is refused with a clear message (E2E-19).",
]
_insert_after(
    "S2.4a",
    _mk(
        "S2.4b",
        "S2",
        "Autosave journal and crash recovery",
        1,
        "—",
        "Command journal autosave; recovery screen after a crash replays to the pre-crash document.",
        [
            "Recovery replays the journal exactly (E2E-12).",
            "Recovery screen lists what will be restored.",
        ],
        ["RecoverySuite"],
        deps=["S2.4a"],
    ),
)
_retarget("S2.4", ["S2.4a"])
_deps("S10.1", add=["S2.4b"])

_insert_after(
    "S2.7",
    _mk(
        "S2.10",
        "S2",
        "AssetRegistry model: display kinds and image placement",
        0,
        "Data.dc.html (display kinds)",
        "Pure model of per-trial display kind (image, blank, blank + fixation cross, cue, unknown) and asset reference with placement; missing asset is a state, not blank.",
        [
            "Codec round-trip; missing never collapses to blank.",
            "Imagery trials sharing one blank display never collapse onto one match item.",
        ],
        ["AssetRegistrySuite"],
        deps=["S2.1"],
    ),
)
_find("S5.7")["title"] = "Assets in Data: display kinds, missing assets and Repair"
_deps("S5.7", add=["S2.10"])

_x = _find("S4.3")
_x["key"] = "S4.3a"
_x["title"] = "TrialView: stage, placement, display kinds, marks, order lines"
_x["scope"] = (
    "(studio-desktop) Stage surround (dark/mid/light), image placement in the screen frame, display kinds incl. hatched missing, fixation marks "
    "(neutral in Explore; role shapes in Compare) with 2px halos, order lines labelled as order not saccades, outside-window marks flagged."
)
_x["ac"] = [
    "Missing asset renders hatched, distinct from blank.",
    "Blank-display query never shows an image by default.",
    "Shared transform for image, marks and picking (property test).",
]
_x["deps"] = ["S4.1", "S2.10"]
_x.setdefault("swap", []).append("UI-A")
_insert_after(
    "S4.3a",
    _mk(
        "S4.3b",
        "S4",
        "TrialView: result map layer, cased isolines, remembered-image underlay",
        0,
        "Main.dc.html, Explore.dc.html",
        "(studio-desktop) Map layer from run grids via MapRasterCache; cased isolines (4-unit white @0.7 under 2-unit ink @0.8) at backend-supplied levels; underlay toggle with persistent disclosure 'Reference image — not displayed during this trial' (also in export).",
        [
            "Result-linked maps equal the run's grid values (hash).",
            "Isolines legible over sky, sand and blank-screen backgrounds (contrast check).",
            "Disclosure present whenever the underlay is on.",
        ],
        ["TrialViewMapFxSuite"],
        deps=["S4.3a", "S4.4"],
    ),
)
_retarget("S4.3", ["S4.3a", "S4.3b"])
_deps("S5.5", remove=["S4.3b"])

_x = _find("S4.5")
_x["key"] = "S4.5a"
_x["title"] = "Plot host and TableTwin (studio-viz builders, studio-desktop host)"
_x[
    "scope"
] = "Common plot host, keyboard roving cursor wiring, and TableTwin: every plot has a synced Table tab reading the same value source."
_x["ac"] = [
    "A plot and its TableTwin render identical values from one source (property).",
    "Selecting a row selects the mark and vice versa.",
]
_plots = [
    (
        "S4.5b",
        "Scale ladder plot",
        "Main.dc.html (contrast)",
        "19 control dots with beeswarm ±7px, B tick 3px, M diamond, ink D bar; histogram above 50 controls.",
        [
            "Histogram fallback with 60 controls.",
            "Fixture focus row values equal fixture.json.",
        ],
    ),
    (
        "S4.5c",
        "Participant plot",
        "Results.dc.html, Figures.dc.html (D)",
        "24 participants, Remembered vs Forgotten linked pairs, solid vs dashed groups, grand-mean ticks, missing as dashed empty marker, Δ cosine axis with zero rule.",
        ["Missing drawn differently from zero.", "Per-group n on hover/selection."],
    ),
    (
        "S4.5d",
        "Scale profile plot",
        "Results.dc.html, Figures.dc.html (E)",
        "Log-spaced σ axis, faint participant lines, bold group means.",
        ["x positions log-spaced (test)."],
    ),
    (
        "S4.5e",
        "Timeline plot",
        "Explore.dc.html (timeline)",
        "Duration-height bars, brush, playhead.",
        ["Brush range maps to fixation selection exactly."],
    ),
]
_insert_after(
    "S4.5a",
    *[
        _mk(
            k,
            "S4",
            t_,
            0,
            b,
            s,
            a,
            [k.replace(".", "") + "PlotFxSuite"],
            deps=["S4.5a"],
        )
        for k, t_, b, s, a in _plots
    ],
)
_retarget("S4.5", ["S4.5a"])
_deps("S6.3", remove=["S4.5a"], add=["S4.5e"])
_deps("S8.3", remove=["S4.5a"], add=["S4.5b"])
_deps("S8.6", remove=["S4.5a"], add=["S4.5c", "S4.5d"])
_deps("S9.2", remove=["S4.5a"], add=["S4.5c", "S4.5d"])

_x = _find("S9.2")
_x["key"] = "S9.2a"
_x["title"] = "Figure composer: page, layout and panel templates"
_x["scope"] = (
    "Figures navigator with New figure; journal widths (89/183 mm); panel templates A–E (encoding gaze, retrieval gaze with display disclosure, "
    "matched/controls maps labelled 'highest of 19 · B 0.35', participant D on Δ cosine axis, scale profile); zoom; Table tab for the selected panel; Open in Compare."
)
_insert_after(
    "S9.2a",
    _mk(
        "S9.2b",
        "S9",
        "Figure appearance, captions and provenance stamp",
        1,
        "Figures.dc.html (inspector, page)",
        "Figure typography (Plex Sans 7pt, letters 8pt 600), text size, participant lines, panel width controls; generated caption with per-group n range; eyes4s version + run stamp; 'includes images' export option.",
        [
            "Caption numbers generated, not typed.",
            "Stamp shows eyes4s version, run and reporting spec.",
        ],
        ["FigureAppearanceFxSuite"],
        deps=["S9.2a"],
    ),
)
_retarget("S9.2", ["S9.2a", "S9.2b"])

# Parity: one ticket per board plus sign-off.
BOARDS = [
    "System",
    "DataEmpty",
    "Data",
    "Explore",
    "Analysis",
    "Main",
    "MainDark",
    "Results",
    "Figures",
]
_x = _find("S10.4")
_x["title"] = "Visual parity sign-off across all boards"
_x["scope"] = (
    "Board PNGs rendered from docs/studio/design by a pinned Chromium script (support.js vendored, local fonts) are committed as references. "
    "Parity = checklist assertions (texts, tokens, shell metrics ±1 px from node bounds) plus a signed side-by-side review. CI pixel diffs are "
    "JavaFX-vs-JavaFX regression only."
)
_x["ac"] = [
    "Every per-board parity ticket closed with a signed checklist; deviations recorded with reasons.",
    "JavaFX regression goldens for every perspective at t1/t2/t3, light and dark.",
]
_x["deps"] = ["S10.1", "S1.10"] + [f"S10.4-{b}" for b in BOARDS]
_insert_after(
    "S10.3",
    _mk(
        "S10.4-render",
        "S10",
        "Render the design boards to reference PNGs",
        1,
        "all boards",
        "Pinned Chromium script renders docs/studio/design/*.dc.html at 1440×900 (1x, 2x) with vendored support.js and local fonts; PNGs committed.",
        ["Script reproducible; PNGs regenerate identically on the pinned browser."],
        ["n/a (tooling)"],
        deps=["S0.6"],
    ),
)
for b in BOARDS:
    _insert_after(
        "S10.4-render",
        _mk(
            f"S10.4-{b}",
            "S10",
            f"Parity: {b} board",
            1,
            f"{b}.dc.html",
            f"Seed the story moment for {b}, capture the JavaFX perspective, check the parity checklist items for {b}, sign the side-by-side.",
            [
                f"Checklist for {b} fully satisfied or deviations recorded.",
                "Shell metrics within ±1 px of the board.",
            ],
            ["ParityChecklistSuite"],
            deps=["S10.4-render", "S0.8"],
        ),
    )

# E2E: scenarios land with features; S10.2 aggregates.
_x = _find("S10.2")
_x["scope"] = (
    "Aggregates the scenario catalogue; each scenario lands with the feature ticket named in brackets. E2E-02 first run → bundled example [S5.1]; "
    "E2E-03 linked selection [S6.6]; E2E-04 draft → Save & run → stale marks [S7.3]; E2E-05 preview = executed design [S7.5]; E2E-06 cancel mid-run [S3.1]; "
    "E2E-07 stale completion rejected [S3.1]; E2E-08 run in progress never swaps [S8.8]; E2E-09 reporting reuses pair scores and keeps the control pool [S8.7]; "
    "E2E-10 underlay disclosure survives export [S9.3]; E2E-11 missing asset repair [S5.7]; E2E-12 crash recovery [S2.4b]; E2E-13 failed run → diagnostics [S3.5]; "
    "E2E-14 Studio check blocked → Choose occurrence [S3.5]; E2E-15 discard draft [S7.3]; E2E-16 Figure 2 rebind and keep [S9.1]; E2E-17 crumb round-trip into Explore "
    "and back [S3.4]; E2E-18 pane retention across perspectives incl. pop-outs [S1.5b]; E2E-19 second-writer refusal [S2.4a]; E2E-20 changed source digest blocks run "
    "[S2.5]; E2E-21 Recognition 'by design' (recognition fixture) [S7.1]; E2E-22 Perception→Imagery blank display (imagery fixture) [S7.1]; E2E-23 min-queries toggle "
    "shows what it drops [S8.7]; E2E-24 dark theme journey [S1.10]; E2E-25 keyboard-only journey [S10.5]."
)
_x["ac"] = [
    "All scenarios green in CI on the fake backend; real-backend re-run in S3.7.",
    "Recognition and imagery mini-fixtures committed.",
]
_x["deps"] = ["S10.1", "S7.1", "S9.1", "S2.4b", "S2.5", "S3.5"]

# Merges.
_find("S8.1")[
    "scope"
] += " Items tab with filter. Empty state for a project with no run: 'No run yet — Open Analysis ⌘3'."
_drop("S8.9")
_retarget("S8.9", ["S8.1"])

# 7. AC rewrites.
_find("S1.4")["ac"] = [
    "App bar 44 ± 0.5 px; wordmark Plex Sans 13/600; five ToggleButtons in one group bound to ⌘1–5.",
    "Jobs chip text equals 'No jobs' / 'Run 8 · Comparing · 21,400 / 44,845 pairs' / 'Run 8 failed · 2 diagnostics' for the fake states; the failed chip opens the Diagnostics pane.",
    "Project chip menu (rename, reveal in Finder, project info); window title shows project name and a dirty marker.",
]
_find("S3.1")["ac"] = [
    "After cancel(), no pair starts after the in-flight chunk; Cancelled within 500 ms on the fixture; run 7 stays current (E2E-06).",
    "A completion for an older revision never becomes current (property; E2E-07).",
    "Failed runs expose diagnostics with stable codes (E2E-13).",
]
_find("S6.2")["ac"] = [
    "Label 'Map: preview · σ 2° · not a result' with a dashed frame; no score node exists in Explore's scene graph.",
    "Legend for marks and display kinds; outside-window fixations flagged.",
]
_find("S6.4")["ac"] = [
    "At most 60 row cells instantiated; no frame > 32 ms while scrolling 11,520 records.",
    "'Show raw record' reveals the verbatim CSV line.",
]
_find("S1.11")["ac"] = [
    "No unlabeled focusable node in any perspective (tree audit).",
    "Tab order equals committed docs/studio/a11y/tab-order-<perspective>.txt.",
]
_find("S1.3")["scope"] = (
    "SVGPath icons: chevron-down, chevron-right, back, forward, minimize, maximize, pop-out, sort, jobs, search, lock, warning, "
    "display-image, display-blank, display-cross, display-cue, display-unknown, display-missing, role-query (filled circle), role-matched (diamond), "
    "role-control (hollow circle), eye-aperture mark. currentColor semantics."
)
_find("S10.6")["ac"] = [
    "p95 of each target met on the named machine (selection < 100 ms, cached trial switch < 250 ms, perspective switch < 150 ms); a failure blocks G5.",
    "Results in docs/studio/PERFORMANCE.md with machine and commit.",
]
_find("S10.7")["ac"] = [
    "A macOS 14 VM with no JDK opens the app, the bundled example and a saved project.",
    "Signing/notarization decision recorded on the bead.",
]
_find("S10.8")["ac"] = [
    "At least 4 of 5 participants correctly explain what was compared, what was excluded and what the number means and does not establish.",
    "Protocol, results and filed issues in docs/studio/USABILITY.md.",
]

# 9. Board coverage gaps folded into tickets.
_find("S5.1")[
    "scope"
] += " Untitled project / 'Not saved' state; the whole window is a drop target."
_find("S5.2")[
    "scope"
] += " Tabs: Fixation mapping, Trial metadata mapping (trials.csv columns), Geometry, Data issues."
_find("S5.5")[
    "scope"
] += " Worked example raw → image → degrees for the selected record."
_find("S5.6")[
    "scope"
] += " Quarantine-cause breakdown with definitions (no-fixations = records but none admissible; absent = inventory without records)."
_find("S6.1")["scope"] += " Items tab with filter."
_insert_after(
    "S6.6",
    _mk(
        "S6.7",
        "S6",
        "Explore small multiples",
        2,
        "Explore.dc.html (tab)",
        "Small multiples of trials for a participant or item with shared extent and colour limits.",
        ["Shared limits by default; each tile keeps its display-kind semantics."],
        ["SmallMultiplesFxSuite"],
        deps=["S6.2"],
    ),
)
_find("S7.2")["scope"] += " Analysis description field."
_find("S7.3")["scope"] += " 'Diff vs rev 4' toggle."
_find("S7.6")["scope"] += " Budget row (pairs ≤ 50,000 · cells ≤ 20M) as counts."
_find("S7.7")["scope"] += " New analysis…; other analyses listed."
_find("S8.2")["scope"] += " 'Underlay remembered image' toggle."
_find("S8.4")[
    "scope"
] += " Appearance controls: stage Dark/Mid/Light, colour limits Shared/Per panel, map opacity (default 0.6)."
_find("S8.6")[
    "scope"
] += " σ selector; Query table tab; per-participant profile lines; freshness 'running · 48%' during a run."
_deps("S8.6", add=["S8.8"])

# New tickets.
_insert_after(
    "S0.7b",
    _mk(
        "S0.8",
        "S0",
        "Bundled example project and story-moment seeds",
        1,
        "all boards",
        "example.eyes built from fixtures/studio-golden; seeds reproduce t1/t2/t3: dataset r2→r3, runs 5–7, draft rev 5, run 8 held mid-run, Figures 1 and 2.",
        [
            "Opening the example copies it as 'memory-study (copy)'.",
            "Each seed reproduces its board's state for parity (S10.4-*).",
        ],
        ["StorySeedSuite"],
        deps=["S0.7a", "S2.3", "S3.0"],
    ),
)
_insert_after(
    "S1.11",
    _mk(
        "S1.12",
        "S1",
        "Logging and error reporting",
        1,
        "—",
        "SLF4J to ~/Library/Logs/Eyes Studio; uncaught FX-thread or job exceptions open a dialog with a copyable bundle (versions, SHA, stack, project digest, no participant data).",
        [
            "An injected failure produces the dialog and a log entry; the bundle contains no data values."
        ],
        ["ErrorReportingSuite"],
        deps=["S0.2"],
    ),
    _mk(
        "S1.13",
        "S1",
        "Internationalisation readiness",
        2,
        "—",
        "User strings from ResourceBundle keys; one NumberFormat for grouping and U+2212 minus; plurals via MessageFormat; the confound sentence is one constant shared with the methods generator.",
        [
            "Lint: no user-visible string literals in studio-desktop.",
            "Number formatting test for grouping and minus sign.",
        ],
        ["I18nLint", "NumberFormatSuite"],
        deps=["S1.4"],
    ),
    _mk(
        "S1.14",
        "S1",
        "Licences, notices and About box",
        1,
        "—",
        "Font files pinned by SHA-256 from upstream releases; OFL texts; OpenJFX (GPLv2 + Classpath Exception), PDFBox, scaladock and Intaglio notices in the bundle and an About box; confirm export font embedding under OFL.",
        [
            "About box lists every bundled component and version.",
            "Notices present in the packaged app.",
        ],
        ["NoticesSuite"],
        deps=["S1.2"],
    ),
)
_insert_after(
    "S2.10",
    _mk(
        "S2.8",
        "S2",
        "User preferences",
        1,
        "—",
        "Versioned preferences in Application Support: appearance, recent projects, stage default, user layouts, export directory. Corrupt file → defaults plus a log entry.",
        ["Round-trip test; corrupt-file test."],
        ["PreferencesSuite"],
        deps=["S2.1"],
    ),
    _mk(
        "S2.9",
        "S2",
        "Project lifecycle UI",
        1,
        "DataEmpty.dc.html, all boards (project chip)",
        "File › New / Open / Save As / Close / Open Recent; untitled → named; dirty marker in title.",
        [
            "DataEmpty parity for the untitled state.",
            "Close with unsaved changes asks first (scaladock close admission).",
        ],
        ["ProjectLifecycleFxSuite"],
        deps=["S2.4a", "S2.8", "S1.9"],
    ),
)
_insert_after(
    "S5.7",
    _mk(
        "S5.8",
        "S5",
        "Dataset revision diff",
        1,
        "Data.dc.html (history), Figures.dc.html (stale reason)",
        "Diff between dataset revisions: mapping, units, geometry, corrections and trials whose admission status changed.",
        [
            "For the fixture, r2 → r3 reads 'onset declared ms; Block → occurrence; 4 trials change status' and feeds Figure 2's stale text."
        ],
        ["DatasetDiffSuite"],
        deps=["S5.6"],
    ),
)
_deps("S9.1", add=["S5.8"])
_deps("S10.7", add=["S1.14"])
_deps("S5.1", add=["S0.8", "S2.9"])

# 3. Gates: closure and no serial chain.
GATES_EXTRA = {
    "G3": ["S7.4", "S7.7", "S8.4", "S8.5", "S8.8"],
    "G5": [
        "S1.6",
        "S1.7",
        "S1.8",
        "S1.9",
        "S4.6",
        "S5.1",
        "S1.12",
        "S1.13",
        "S2.9",
        "S3.7",
        "S6.7",
        "S10.2",
    ],
}
GATES = [
    (k, t_, p, b, [d for d in dict.fromkeys(ds + GATES_EXTRA.get(k, []))])
    for k, t_, p, b, ds in GATES
]
RENAMES = {"S0.5": ["S0.4"], "S0.7": ["S0.7a"], "S1.5": ["S1.5a"], "S2.4": ["S2.4a"], "S4.3": ["S4.3a", "S4.3b"], "S4.5": ["S4.5a"], "S9.2": ["S9.2a", "S9.2b"], "S8.9": ["S8.1"]}
GATES = [(k, t_, p, b, list(dict.fromkeys(sum((RENAMES.get(d, [d]) for d in ds), [])))) for k, t_, p, b, ds in GATES]
_deps("S10.1", add=["S3.6"])
GATES = [(k, t_, p, b, ds + (["S1.5b"] if k == "G4" else [])) for k, t_, p, b, ds in GATES]
SERIAL_GATES = False
PREAMBLE_SHORT = "Studio merges behind StudyBackend with FakeStudyBackend; see the plan preamble."


# ---------------------------------------------------------------------------------------------
# Owner decisions 2026-09-26 (DESIGN_SPEC §13): portable-by-construction architecture;
# off-screen fixations excluded and reported by default; control pool follows occurrence selection.
# ---------------------------------------------------------------------------------------------
PREAMBLE = PREAMBLE + (" Portability (DESIGN_SPEC §13): studio-core, studio-app (UI-neutral presentation: model, intents, update, "
    "view-models, LayoutSpec, commands, strings) and studio-viz are cross-built JVM + Scala.js and linked for JS in CI; studio-desktop "
    "is a thin JavaFX shell that renders view-models and dispatches intents. Every behaviour is tested headlessly in studio-app.")
COMMON = COMMON + (" Behaviour lives in studio-app (or studio-core) with headless tests; the JavaFX layer only binds view-models to "
    "nodes and dispatches intents; no javafx, scaladock.fx or java.io/java.nio outside studio-desktop.")

_x = _find("S0.1")
_x["title"] = "Decision: studio module layout, toolchain and portability"
_x["scope"] = ("Owner decision 2026-09-26: JVM + JavaFX now, portable by construction (DESIGN_SPEC §13). Projects under studio/: "
    "studio-core (crossProject JVM+JS: document, commands, revisions, services, StudyBackend protocol, fake backend), studio-app "
    "(crossProject: Elm-style presentation layer — app model, intents, update, view-models, LayoutSpec, command registry, strings, "
    "formatting), studio-viz (crossProject: Intaglio scene builders over intaglio core/interaction/svg), studio-desktop (JVM: JavaFX "
    "shell, scaladock, Intaglio javafx backend, platform services, packaging). JDK 25 LTS for studio-desktop (JavaFX 24 needs JDK ≥ 22); "
    "cross projects keep the core JDK target. Scala 3.7.4, sbt 1.12.14. eyes4s kernel…codec and fs2 already cross-build for JS; only "
    "io's ArrowResultExport and ArtifactFiles are JVM-only.")
_x["ac"] = ["Decision note on the bead (portability rationale, alternatives: JavaFX-only monolith; web-first).",
            "Performance reference machine named when S10.6 starts (model, CPU, RAM, macOS)."]

_x = _find("S0.2")
_x["title"] = "Add studio-core, studio-app, studio-viz (JVM+JS) and studio-desktop (JVM) with boundary rules"
_x["scope"] = ("Create the four projects per S0.1. Extend checkModuleBoundaries: no eyes4s module depends on studio-*; studio-core, "
    "studio-app and studio-viz resolve no javafx/scaladock-fx and compile for Scala.js; a source lint rejects javafx.*, scaladock.fx, "
    "java.io and java.nio imports outside studio-desktop.")
_x["ac"] = ["`sbt studioCoreJVM/test studioCoreJS/test studioAppJVM/test studioAppJS/test studioVizJVM/test studioVizJS/test studioDesktop/test` run.",
            "CI links studio-app for JS (fastLinkJS) on every PR.",
            "Boundary check and import lint fail on planted violations (negative fixtures).",
            "AGENTS.md names studio-core and studio-desktop as effect-permitted and studio-app/studio-viz as pure."]

_insert_after("S0.8",
    _mk("S0.9", "S0", "Portability contract: platform interfaces, backend transports, PORTING.md", 0, "—",
        "Define platform service interfaces in studio-core (FileSystem, ProjectStore, Dialogs, Clipboard, Fonts, Scheduler/Clock, ExternalOpen) "
        "with JVM implementations in studio-desktop; BackendTransport with InProcess (JVM now; Scala.js later) and an IPC sidecar protocol spec "
        "(framed JSON over stdio/WebSocket) using the cross-built StudyBackend codecs; docs/studio/PORTING.md explains what a new shell must "
        "implement (render view-models, dispatch intents, map LayoutSpec, provide platform services, pass the UI-neutral acceptance suite and visual parity).",
        ["Interfaces compile on JVM and JS; no JVM type leaks into studio-core/studio-app signatures.",
         "BackendConformanceSuite runs over InProcess and a loopback IPC transport (JVM) with identical results.",
         "PORTING.md reviewed; it lists the exact acceptance a port must pass."],
        ["PlatformInterfacesSuite (JVM+JS)", "TransportConformanceSuite"], deps=["S0.2", "S3.0"]))

_insert_after("S1.0" if any(x["key"] == "S1.0" for x in T) else "S1.1",
    _mk("S1.0", "S1", "studio-app presentation framework: model, intents, update, view-models", 0, "all boards",
        "Elm-style core cross-built JVM+JS: AppModel, Intent ADT, pure update returning effects-as-data, view-model projections per pane; "
        "LayoutSpec for perspectives and pane layouts; command registry and keymap as data; string bundles and formatting. The JavaFX shell "
        "subscribes to view-model streams and dispatches intents.",
        ["update is pure and total (property tests over generated intent sequences).",
         "A headless harness renders every view-model to text snapshots (golden) for the fixture moments t1/t2/t3.",
         "Links for JS in CI."],
        ["AppUpdateLawsSuite (JVM+JS)", "ViewModelSnapshotSuite"], deps=["S0.2", "S2.2"]))
for _k in ("S1.4", "S1.5a", "S1.6", "S1.7", "S1.8", "S1.9", "S1.10", "S1.11", "S1.13", "S3.6"):
    _deps(_k, add=["S1.0"])
for _k in [x["key"] for x in T if x["epic"] in ("S5", "S6", "S7", "S8", "S9")]:
    _deps(_k, add=["S1.0"])
_find("S1.1")["scope"] += " The same token source also generates web CSS custom properties for a future web shell."
_find("S1.1")["ac"].append("Web CSS variables generated from the same source (checked in CI, not shipped).")
_find("S1.5a")["scope"] += " Perspectives and default layouts are declared as a UI-neutral LayoutSpec in studio-app; studio-desktop maps LayoutSpec to scaladock."
_find("S1.9")["scope"] += " The registry and keymap are data in studio-app; the menu bar is a JavaFX rendering of them."
_find("S1.13")["scope"] += " Bundles live in studio-app (cross-built)."
_find("S3.0")["scope"] += " Requests, responses and progress events have cross-built codecs so the same protocol serves in-process and IPC transports."
_find("S3.6")["scope"] += " The driver dispatches studio-app intents and reads view-models, so the same acceptance suite can validate any future UI shell."
for _k in ("S4.5b", "S4.5c", "S4.5d", "S4.5e", "S4.6"):
    _find(_k)["scope"] = "(studio-viz builder, cross-built; hosted by studio-desktop) " + _find(_k)["scope"]
_find("S5.5")["scope"] += (" OffScreenPolicy setting (Dataset · re-admit): ExcludeRecord (default) or QuarantineTrial; 'outside screen' "
    "tallied and shown separately from 'outside window'.")
_find("S5.5")["ac"].append("Switching OffScreenPolicy creates a dataset draft; counts for 'outside screen' and 'outside window' are shown separately.")
_find("S5.6")["scope"] += " Ledger shows 'outside screen' exclusions as a reported count, not a quarantine cause, under ExcludeRecord."
_find("S9.4")["scope"] += " Methods cite the OffScreenPolicy and the occurrence-selection rule for matched references and controls."
_insert_after("S10.8",
    _mk("S10.9", "S10", "Portability proof: minimal Scala.js shell", 2, "Explore.dc.html",
        "Spike (not a release gate): a browser page that links studio-app + studio-viz for JS, renders the Explore perspective from view-models with "
        "Intaglio's canvas/SVG backend, dispatches intents, and runs the fake backend in-process.",
        ["The Explore linked-selection scenario (E2E-03) passes against the web shell via the StudioDriver.",
         "Findings recorded in PORTING.md (gaps, bundle size)."],
        ["E2E-03 on the web shell"], deps=["S1.0", "S0.9", "S6.6"]))

GATES = [(k, t_, p, b, ds + (["S0.9", "S1.0"] if k == "G0" else [])) for k, t_, p, b, ds in GATES]  # S10.9 is a spike, deliberately ungated

# Triage 2026-09-26 (lead request): p0 = tickets in the G0/G1 closure only; post-MVP items to p3.
for _k in ['S0.7b', 'S3.7', 'S5.3', 'S5.5', 'S7.1', 'S7.2', 'S7.3', 'S8.7', 'S10.2', 'S10.3', 'S10.4']:
    _find(_k)["prio"] = 1
for _k in ["S10.9", "S6.7", "S1.13"]:
    _find(_k)["prio"] = 3

def body_of(x):
    lines = [
        f"Plan key {x['key']} (docs/studio/IMPLEMENTATION_PLAN.md). Board: {x['board']}.",
        "",
        "Scope: " + x["scope"],
        "",
        "Acceptance criteria:",
    ]
    lines += ["- " + a for a in x["ac"]]
    lines += ["", "Tests:"] + ["- " + s for s in x["tests"]]
    if x["deps"]:
        lines += ["", "Depends on: " + ", ".join(x["deps"])]
    if x.get("swap"):
        lines += ["", "Swap dependencies (merge on FakeStudyBackend; real-backend AC re-checked in S3.7): "
                  + ", ".join(f"{d} {EXT[d]}" for d in x["swap"])]
    lines += ["", PREAMBLE_SHORT, "", COMMON]
    return "\n".join(lines)


def render():
    out = [
        "# Eyes Studio implementation plan",
        "",
        "Generated by `docs/studio/plan/studio_plan.py` — edit the script, not this file. Mote beads carry the same text "
        "(tag `studio`); `ids.json` maps plan keys to bead ids.",
        "",
        "**Point A:** eyes4s core with the study family, preflight, preview, fs2 execution and codecs; core tickets UI-A…UI-H, CR2, CR3, CR6 in progress; no studio code.  ",
        "**Point B:** the approved design (`docs/studio/design/`, spec `docs/studio/DESIGN_SPEC.md`) running as a packaged macOS app that passes gate G5.",
        "",
        PREAMBLE,
        "",
        "## Gates",
        "",
    ]
    for k, title, p, body, deps in GATES:
        out += [
            f"### {k} — {title.split(': ',1)[1]}",
            "",
            body,
            "",
            "Requires: " + ", ".join(deps),
            "",
        ]
    out += ["## Upstream asks", ""]
    for k, title, p, body in UP:
        out += [f"- **{k}** — {title}. {body}"]
    ext = ", ".join(f"{k} ({v})" for k, v in EXT.items())
    out += [
        "",
        f"Core tickets depended on: {ext}.",
        "",
        "## Epics and tickets",
        "",
        COMMON,
        "",
    ]
    for ek, etitle, ep, ebody in EPICS:
        out += [
            f"### {etitle}",
            "",
            ebody,
            "",
            "| Key | Ticket | P | Depends on | Swap deps (core) |",
            "|---|---|---|---|---|",
        ]
        for x in [x for x in T if x["epic"] == ek]:
            out.append(
                f"| {x['key']} | {x['title']} | {x['prio']} | {', '.join(x['deps']) or '—'} | {', '.join(x.get('swap', [])) or '—'} |"
            )
        out.append("")
        for x in [x for x in T if x["epic"] == ek]:
            out += [
                f"#### {x['key']} · {x['title']}",
                "",
                f"*Board:* {x['board']}",
                "",
                x["scope"],
                "",
                "**Acceptance criteria**",
                "",
            ]
            out += ["- " + a for a in x["ac"]] + [
                "",
                "**Tests:** " + "; ".join(x["tests"]),
                "",
            ]
    open(os.path.join(ROOT, "docs/studio/IMPLEMENTATION_PLAN.md"), "w").write(
        "\n".join(out) + "\n"
    )


def mote(args, dry):
    cmd = ["mote", "--actor", ACTOR, "--json"] + args
    if dry:
        print(" ".join(repr(c) if " " in c else c for c in cmd[:8]), "…")
        return {"id": "DRY-" + args[1][:20] if args[0] == "new" else ""}
    r = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True)
    if r.returncode != 0:
        sys.exit(f"mote failed: {' '.join(cmd[:6])}: {r.stderr}")
    try:
        return json.loads(r.stdout)
    except json.JSONDecodeError:
        return {"raw": r.stdout}


def create(dry):
    ids = json.load(open(IDS)) if os.path.exists(IDS) else {}

    def new(key, title, prio, body, tags):
        if key in ids:
            return
        args = ["new", title, "-p", str(prio), "--body", body] + sum(
            (["--tag", tg] for tg in tags), []
        )
        res = mote(args, dry)
        bid = (
            res.get("id")
            or res.get("bead", {}).get("id")
            or res.get("raw", "").strip().split()[-1]
        )
        ids[key] = bid
        if not dry:
            json.dump(ids, open(IDS, "w"), indent=1, sort_keys=True)

    new(
        "EPIC",
        "Eyes Studio: from core to the approved design (docs/studio/IMPLEMENTATION_PLAN.md)",
        0,
        "Umbrella for the JavaFX desktop app in the studio module. Point B = docs/studio/design (canvas https://claude.ai/artifact/5sk2BaePcrMCYohRT6Vg2v) shipped and passing gate G5. Gates G0-G5; epics S0-S10.",
        ["studio", "epic"],
    )
    for k, title, p, body in EPICS:
        new(k, title, p, body, ["studio", "epic"])
    for k, title, p, body in UP:
        new(k, title, p, body, ["studio", "upstream"])
    for x in T:
        new(
            x["key"],
            f"{x['key']} {x['title']}",
            x["prio"],
            body_of(x),
            ["studio", x["epic"].lower()] + x["tags"],
        )
    for k, title, p, body, deps in GATES:
        new(k, title, p, body + "\n\nRequires: " + ", ".join(deps), ["studio", "gate"])
    resolve = lambda k: EXT.get(k) or ids[k]
    if dry:
        print(f"{len(ids)} beads would be created")
        return
    edges = os.path.join(HERE, "edges.json")
    done = set(json.load(open(edges))) if os.path.exists(edges) else set()

    def edge(kind, child, parent):
        tag = f"{kind}:{child}>{parent}"
        if tag in done:
            return
        mote([kind, "add", resolve(child), resolve(parent)], False)
        done.add(tag)
        json.dump(sorted(done), open(edges, "w"), indent=0)

    for k, *_ in EPICS:
        edge("rel", k, "EPIC")
    for k, *_ in UP:
        edge("rel", k, "EPIC")
    for k, *_ in GATES:
        edge("rel", k, "EPIC")
    for x in T:
        edge("rel", x["key"], x["epic"])
        for d in x["deps"]:
            edge("dep", x["key"], d)
    for k, t_, p, b, deps in GATES:
        for d in deps:
            edge("dep", k, d)
    for g in GATES[:-1]:
        edge("dep", GATES[-1][0], g[0])
    print(f"{len(ids)} beads, {len(done)} edges")


def sync(dry):
    """Update title/body/priority of existing beads whose generated text changed."""
    ids = json.load(open(IDS))
    want = {x["key"]: (f"{x['key']} {x['title']}", body_of(x), x["prio"]) for x in T}
    want.update({k: (t_, b + "\n\nRequires: " + ", ".join(ds), p) for k, t_, p, b, ds in GATES})
    n = 0
    for key, (title, body, prio) in want.items():
        if key not in ids:
            continue
        cur = json.loads(subprocess.run(["mote", "--json", "show", ids[key]], cwd=ROOT, capture_output=True, text=True).stdout)
        if cur["body"] != body or cur["title"] != title or cur["priority"] != prio:
            n += 1
            if not dry:
                mote(["set", ids[key], f"title={title}", f"body={body}", f"priority={prio}"], False)
    print(f"{n} beads {'would be ' if dry else ''}updated")

if __name__ == "__main__":
    cmd = sys.argv[1] if len(sys.argv) > 1 else "render"
    if cmd == "render":
        render()
        print("rendered")
    elif cmd == "mote":
        create("--dry-run" in sys.argv)
    elif cmd == "sync":
        sync("--dry-run" in sys.argv)
    else:
        sys.exit(__doc__)

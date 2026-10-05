# Eyes Studio — design and plan

Eyes Studio is the JavaFX desktop workbench built on eyes4s. It lives in the `studio` module family
(`studio-core`, `studio-app`, `studio-viz` cross-built JVM + Scala.js; `studio-desktop` JavaFX). Nothing in
eyes4s may depend on it. Architecture: DESIGN_SPEC §13.
This folder is the reference that the implementation has to reach.

An exported project snapshot without images withholds stimulus image bytes. The trial inventory
still travels, including its `image_file` column, so stimulus file names remain visible. The bundle
control and its README state this distinction before and after export.

| Path | What it is |
|---|---|
| `DESIGN_SPEC.md` | The design contract. §12 lists the round-3 amendments and overrides anything earlier in the file. |
| `design/*.dc.html` | The approved boards, one file per screen and moment (see below). |
| `fixture/make_fixture.py`, `fixture/FIXTURE.md`, `fixture/fixture.json` | The deterministic mock study behind every number on the boards. The fake backend serves `fixture.json`. The real CSV acceptance fixture is generated separately, into `fixtures/studio-golden/` (S0.7a). |
| `PORTING.md` | The port contract: the seams a new shell (Electron, Tauri, a browser) implements, the sidecar wire protocol, and the acceptance a port must pass (S0.9). |
| `PARITY_CHECKLIST.md` | The per-board items a screen must match. The S10.4-* tickets sign it off. |
| `plan/studio_plan.py` | The single source for `IMPLEMENTATION_PLAN.md` and for the Mote beads tagged `studio`. |
| `IMPLEMENTATION_PLAN.md` | Generated. Contains the gates G0–G5, the epics S0–S10, and every ticket with its acceptance criteria and tests. |

## The boards

The approved canvas is at https://claude.ai/artifact/5sk2BaePcrMCYohRT6Vg2v. It is private to the
owner, so the board sources here are the copy the implementation builds from.

- Each board is a 1440×900 Design Component file.
- `{{name}}` holes in a board are filled from the small `renderVals()` script at its end.
- `MainDark.dc.html` renders `Main` in the dark theme.

| Board | Screen | Story moment |
|---|---|---|
| `System` | Tokens, roles, states, shell anatomy, module map | — |
| `DataEmpty` | Data, first run | t0 |
| `Data` | Data, verifying the dataset r3 draft | t1 |
| `Explore` | Explore, linked trial view | t2 |
| `Analysis` | Analysis, draft rev 5 ready | t2 |
| `Main` / `MainDark` | Compare, query layout | t2 |
| `Results` | Compare, summary layout, run 8 running | t3 |
| `Figures` | Figures | t2 |

The story moments (t0–t3) are defined in `fixture/FIXTURE.md`.

The design was reached through three rounds of critic review: science, interaction, visual craft and
buildability. All four approved round 3.

## Regenerating

```sh
# fixture: prints FIXTURE.md and writes fixture.json to the working directory, so run it in a scratch dir
(cd "$(mktemp -d)" && python3 "$OLDPWD/docs/studio/fixture/make_fixture.py" >/dev/null && diff FIXTURE.md "$OLDPWD/docs/studio/fixture/FIXTURE.md")

# plan document
python3 docs/studio/plan/studio_plan.py render

# Mote beads: idempotent; plan/ids.json records the bead ids that were created
python3 docs/studio/plan/studio_plan.py mote --dry-run
python3 docs/studio/plan/studio_plan.py mote
```

To change the plan, edit `studio_plan.py`, rerun `render`, and update the matching bead with
`mote set`. Beads that already exist are never recreated.

## Backend protocol versions

Protocol 1.12 (bd-01M44FKT4NFZ0F5KQTKT96DC8E; its number is fixed at landing) adds
each pair row's window tallies, additive on 1.11: `PairRowEntry` carries
`queryWindow` and `referenceWindow`, each a `TrialTally` (the trial's eyes4s
`WindowTally`, written as `TrialFailed` writes it, with the trial's `StudioRef`), or
none when the backend holds no admitted scanpath for the trial. The pairs table
shows the reference's (and the query's) fixations and duration outside the window.

Protocol 1.11 (bd-01M43VP5YV6WB4VVQEW2CJTB9V) evaluates a reporting spec over a run,
additive on 1.10: `report(run, spec, scale)` answers `ReportView`, the spec as sent (an
edit, whatever the document has saved) reduced by eyes4s-results (`Report.evaluate`,
UI-C) over the run's stored rows at one scale: each group's M, B and D cells (missing
with a typed `ReportAbsence`, never zero) with their participants, queries and failures;
each participant's D; the participants a minimum per group drops; and the accounting
(eligible, kept, filtered out, undecided, no stored value, below the minimum). A spec
with an outside-window filter is also evaluated by eyes4s with that filter alone, and its
own accounting gives the queries the filter leaves out (failed ones included) and those
it cannot decide (an undefined share); the studio subtracts nothing. Each value has its
`StudioRef`: the new `ReportCell` (a group, or the whole ungrouped report, and a role),
`ParticipantSummary` and the new `ReportTally`. A spec eyes4s cannot evaluate, or stored
rows it cannot read, is refused with the new `ReportRefused`. The fake serves it the same
way, over the fixture's stored rows bound by `ReportSources.tables` (eyes4s-codec).

Protocol 1.10 (UI-G G3) adds the `TrialFailed` map placement, additive on 1.9: a
fixation in the window of a trial the study fails (its off-window policy is
`FailTrial`) carries the trial's eyes4s `WindowTally` (`outsideScreen`,
`outsideWindow`, `total` and their durations in microseconds, as protocol 1.4 writes
large counts), re-validated on decoding. `InWindow` keeps its wire name `InMap`.
The trial-fixations pin gains a `TrialFailed` fixation; the envelope pins are at
1.10.

Protocol 1.9 (S9.5) adds a run's pair rows, additive on 1.8: `pairRows(run, scale,
page)` answers `PairRowPage`, every directed pair of the run at one scale index (eyes4s
`PairScores`), in focal order with each query's matched pair before its controls, each
with its reference's item and a typed `PairScoreState` (`Scored`, `Failed` with its
diagnostic, or `NotServed`). A scale the run does not compute is refused with the new
`UnknownScale`. The export bundle's comparisons.csv reads it. The 1.8 pins are unchanged
apart from the envelope version, now 1.9.

Protocol 1.8 (S3.5) widens `StudioDiagnostic`, additive on 1.7: `affected` lists every
trial the diagnostic names, eyes4s's `affectedTrials` (its subject, then its operands
and causes, each once), so a remedy opens exactly those trials; `category` and
`remedy` carry a preflight finding's eyes4s `FindingClass` and `Remedy` by case
name. The studio words a diagnostic by its code (`DiagnosticsPresenter`), never by
its message. On the wire the three fields are required, since client and backend
speak the same minor; their empty defaults serve diagnostics built in-process (a
studio check, a fake). The pins that hold a diagnostic were re-recorded; the other 1.7 pins are
unchanged; the envelope version is now 1.8.

Protocol 1.7 (S6.4) adds the source records view for Explore, additive on 1.6:
`sourceRecords(revision, from, count)` answers `SourceRecordPage`, records `from`
to `from + count - 1` (at most 500) of the fixation file of the revision's dataset,
in file order and numbered from 1, with the source (its import name and SHA-256),
its total, the count asked for, and the pixels per degree its degrees are at with
where that comes from (`ScaleSource.Recipe`, the plan's declared units, else
`Dataset`). A page holds every record asked for that the file has: only the last
is short and none is empty; an empty file answers `PastEnd(from, 0)`. Each `SourceRecordRow` carries its `StudioRef.SourceRecord` (naming
its fixation when an admitted scanpath holds it), its cells as numbers (`None`
where a cell is not one), its position in image pixels and in degrees from the
image's centre (y up, at the page's linear pixels per degree), the eyes4s
`MapPlacement` the revision's study gives an admitted record, and its verbatim
text (which may span several lines). Screen, image and degrees are all present
or all absent. A range outside the file is `SourceRecordsRefused`. The fake serves
fixtures/studio-golden's fixations.csv, embedded verbatim, with the image frame
and degrees from `DisplayFrames` (eyes4s-kernel's `Subframe` and
`LinearAngularScale`); its generator refuses a CR or a quote in the file. The other
1.6 pins are unchanged; the envelope version is now 1.7.

Protocol 1.6 (S6.2) adds two trial views for Explore, additive on 1.5:
`trialFixations(revision, trial)` answers `TrialFixations`, the trial's admitted
fixations in scanpath order, each with its `StudioRef.Fixation`, fixations.csv
record, screen-pixel centre, onset and duration in milliseconds, and the eyes4s
`MapPlacement` the revision's study gives it; `trialPreview(revision, trial)`
answers `TrialPreview`, eyes4s's σ 2° density of the trial's in-map fixations over
the recipe's grid, with the screen region the grid covers (the analysis
window, not the image unless they coincide), its `RowOrder` stated and the
backend's isoline levels.
Both are keyed by the analysis revision because placement and the grid belong to
the study, not the dataset. Every value is validated (finite positions, a
non-negative onset, a positive duration, positions 1, 2, … in order, finite
non-negative cells); a trial outside the revision's dataset is refused with
`BackendError.UnknownTrial(dataset, trial)`, one in it without an admitted
scanpath is `Unavailable`, and every other refusal is
`BackendError.TrialViewRefused(TrialViewError)`, which names the trial and what
failed (an invalid value, a trial the study fails under `FailTrial`, or an
eyes4s step). The
fake serves both over fixtures/studio-golden under the recipe its story moment
saved: placement through the kernel's screen frame and half-open window (held
to eyes4s's `CoordinateProvenance` by `FakePlacementJvmSuite`), the preview through eyes4s-surface's Gaussian
smoother and `MassLevels` (coverages 0.5 and 0.8). The other 1.5 pins are
unchanged; the envelope version is now 1.6.

Protocol 1.5 (S7.5) adds the resolved-design counts: `PreviewCandidates` carries
`requestedQueries`, `queriesNotAdmitted` and `byDesignQueries` (absent when the
recipe has no by-design category: not applicable, not zero), and `PreviewCounts`
carries `eligibleQueries`, each a non-negative `QueryCount`. A 1.4 preview does not
decode as 1.5. `StudioRef.DesignTally` names each of these counts.

Protocol 1.4 preserves every `Long` in backend messages across JVM and Scala.js
JSON text transport. Values in the inclusive range −9,007,199,254,740,991 to
9,007,199,254,740,991 remain JSON numbers; larger magnitudes use canonical decimal
strings. Decoders accept canonical signed decimal strings in the Long range and
safe integer numbers, and refuse unsafe numeric input before it can be accepted
as a rounded count. `WireFormat` uses Circe's Jawn parser on both platforms so
fractional numeric text cannot round into an integer before validation. Domain
constructors still reject negative progress.

This policy covers progress meters, totals and step counts, preview/result pair
counts, window durations, source-line numbers and request correlation IDs. Existing
small-number body pins are unchanged; the envelope version is now 1.4.
`ProtocolLongSuite` exercises actual JSON text at the safe-integer boundary and
Long extremes on both platforms.

Protocol 1.3 (S5.4) replaces the admission summary's `inventoryTrials` and `absent`
numbers with `inventory: InventoryJoin` (`Joined(trials, absent)`, or `Undeclared`
when the dataset declares no trial inventory, so absent trials are not counted) and
adds `BackendError.InventoryRefused`, whose issues name the inventory records, trial
and columns eyes4s refused. A 1.2 summary does not decode as 1.3.

**Deploy the Studio client and backend together.** They speak exactly one protocol
version: every minor has changed what an older body decoder can read, so a minor is no
promise of compatibility, and nothing negotiates capabilities. Both ends read a frame's
`version` before its body and refuse any other version, major or minor, naming both:
the sidecar answers a request of another version with `UnsupportedVersion(requested,
supported)` under the request's id, and the client fails a frame of another version
with `TransportError.Incompatible(found, supported)`. Neither is ever reported as
malformed, even when the other version's body would not decode
(bd-01M3JH3492J21SKMYYNZM93118). Protocol
1.2 added `ProgressTotal.Counting`, which a 1.0/1.1 decoder cannot read; 1.3 adds
the inventory join; 1.4 adds the exact large-count policy; 1.5 adds the
resolved-design counts; 1.6 adds the trial fixations and preview views; 1.7 adds the source records view; 1.8 adds the diagnostic's affected trials, class and remedy; 1.9 adds the pair rows view and the exact-version refusal; 1.10 adds the `TrialFailed` placement; 1.11 adds reports; 1.12 adds the pair rows' window tallies. `ProtocolCodecSuite`
retains the frozen legacy-total probe, while `ProtocolLongSuite` verifies that
safe-number 1.2 envelopes remain readable. This does not establish mixed-version peer compatibility.

## Bounded preview paging

`StudyBackend.previewCounting(revision, budget)` creates a retained preview and
returns a stream of refusals or preview events. `Initial` supplies the backend's
handle, stamp and candidate counts; `Counting` reports completed participant
pages; `Ready` supplies exact counts and diagnostics. `continuePreview` resumes
that handle and returns its receipt again if counting has already finished.
`PreviewBudget` permits 1–4096 participant pages per request. Stopping a stream
closes its exchange; completed work remains counted, including transport read-ahead.

`ExecutionService.submitPreview` forwards the ready receipt to the backend.
The fake refuses unknown, unfinished, changed or stale receipts and retains the
same prepared snapshot for execution. Its job consumes the captured script.
Tests inject a different script at the fixture boundary to distinguish this
from reconstructing a job by revision.

The fake replays `FIXTURE.md` counts and explicitly uses `CoreBinding.Unbound`.
Its participant pages are fixture steps. They do not qualify a real
`CountCursor` budget, an input digest, or scientific pair counts. S3.7 must retain
the actual `PreparedStudy` and its owned counts, check current input and plan
identity, and execute that prepared study; S0.7b qualifies fixture counts through
real eyes4s.

`PreviewCandidates` also carries the query counts known before paging (requested,
not admitted, and the recipe's by-design category, or none), and `PreviewCounts`
the eligible queries after it; a `PreviewReady` refuses counts that do not partition
the requested queries (protocol 1.5). The fake serves the `FIXTURE.md`
counts; the real backend (S3.7) must source them from eyes4s's own preview of the
prepared study (`StudyPreview`/`PreparedStudy`), not compute them in Studio.

## Resolved design table

S7.5's table (`eyes4s.studio.app.analysis.ResolvedDesign`) presents these events
for the Analysis trail's revision, else the draft. Its chip counts and pair counts
are the backend's, each traced by a `StudioRef.DesignTally`; rows come from
`previewRows`; every row opens its trial. A counted receipt goes to the app with
the recipe it was prepared from (`Intent.DesignPrepared`). A Save & run of the same
stamp and recipe emits `ExecutionEffect.SubmitPreview`, so execution consumes that
prepared design (E2E-05); any other run submits its stamp. The receipt is
submitted once, withdrawn when the pane retargets, and a refused receipt (an evicted
or stale preview) falls back to submitting the run's stamp, so the recorded run is not
orphaned. The recipe comparison is the client's own check: S3.7 must make the backend
refuse a receipt whose recipe or plan identity changed (bead
bd-01M3DPFHY9YW5BPSJQ8VBBXHVN). The board's split of the
457 eligible queries into 454 contributing and 3 failing in the window, and the
per-row reasons ("1 control fewer", "0 of 11 fixations inside window"), need fields
`PreviewRow` does not carry yet.

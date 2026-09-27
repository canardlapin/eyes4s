# Eyes Studio — design and plan

Eyes Studio is the JavaFX desktop workbench built on eyes4s. It lives in the `studio` module family
(`studio-core`, `studio-app`, `studio-viz` cross-built JVM + Scala.js; `studio-desktop` JavaFX). Nothing in
eyes4s may depend on it. Architecture: DESIGN_SPEC §13.
This folder is the reference that the implementation has to reach.

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

Protocol 1.3 preserves every `Long` in backend messages across JVM and Scala.js
JSON text transport. Values in the inclusive range −9,007,199,254,740,991 to
9,007,199,254,740,991 remain JSON numbers; larger magnitudes use canonical decimal
strings. Decoders accept canonical signed decimal strings in the Long range and
safe integer numbers, and refuse unsafe numeric input before it can be accepted
as a rounded count. `WireFormat` uses Circe's Jawn parser on both platforms so
fractional numeric text cannot round into an integer before validation. Domain
constructors still reject negative progress.

This policy covers progress meters, totals and step counts, preview/result pair
counts, window durations, source-line numbers and request correlation IDs. Existing
small-number body pins are unchanged; the envelope version is now 1.3.
`ProtocolLongSuite` exercises actual JSON text at the safe-integer boundary and
Long extremes on both platforms.

Deploy the Studio client and backend together. The transport checks major versions
only and decodes the typed envelope body before checking the version; it does not
negotiate minor capabilities. Mixed-minor deployments are unsupported. Protocol
1.2 added `ProgressTotal.Counting`, which a 1.0/1.1 decoder cannot read; 1.3 adds
the exact large-count policy. `ProtocolCodecSuite` retains the frozen legacy-total
probe, while `ProtocolLongSuite` verifies that safe-number 1.2 envelopes remain
readable. This does not establish mixed-version peer compatibility.

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
real eyes4s. S7.5 owns the resolved-design table that presents these events.

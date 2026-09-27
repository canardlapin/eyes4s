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

Protocol 1.2 adds `ProgressTotal.Counting` to progress events. Deploy the Studio
client and backend together. The transport checks major versions only and decodes
the typed envelope body before checking the version; it does not negotiate minor
version capabilities. A protocol 1.0 or 1.1 decoder cannot read the new `Counting` case,
even if the envelope is labelled 1.1. Mixed-minor deployments are unsupported.

`ProtocolCodecSuite` pins the 1.2 envelopes, verifies a current Counting event,
and exercises the frozen 1.0/1.1 total decoder at the event's meter boundary. It
retains a readable legacy `Exact` control and rejects `Counting` under either
version label. This records the coordinated-upgrade requirement; it does not
claim old-client decoding compatibility or negotiated refusal.

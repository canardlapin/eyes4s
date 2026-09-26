# Eyes Studio — design and plan

Eyes Studio is the JavaFX desktop workbench built on eyes4s. It lives in the `studio` module family
(`studio-core`, `studio-viz`, `studio-desktop`). Nothing in eyes4s's pure modules may depend on it.
This folder is the reference that the implementation has to reach.

| Path | What it is |
|---|---|
| `DESIGN_SPEC.md` | The design contract. §12 lists the round-3 amendments and overrides anything earlier in the file. |
| `design/*.dc.html` | The approved boards, one file per screen and moment (see below). |
| `fixture/make_fixture.py`, `fixture/FIXTURE.md` | The deterministic mock study. Every number on the boards comes from it. |
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

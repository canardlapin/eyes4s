# AGENTS.md

Working instructions for this repository. Read `eyes4s.md` for the architecture
and `PRD.md` for the requirements it has to satisfy.

## Layout

```
kernel/    eyes4s-kernel   geometry, time, trajectories, measures. NO ocular vocabulary.
core/      eyes4s-core     Gaze, Sample, Recording, Event, Scanpath, Viewing
detect/    eyes4s-detect   Detector instances, filters, the Machine runner
surface/   eyes4s-surface  smoothers, bandwidth, pyramids, entropy
aoi/       eyes4s-aoi      AoiSet, dwell, transitions
compare/   eyes4s-compare  Compare hierarchy, alignment, MultiMatch, OT
design/    eyes4s-design   Trials, Pairing, Session, contrasts, RNG
plan/      eyes4s-plan     analyses as descriptions, the typed registry
codec/     eyes4s-codec    JSON codecs, versioned schema
laws/      eyes4s-laws     Discipline rule sets and generators (MAIN-scope deps)
fs2/       eyes4s-fs2      streaming execution                  } the only modules
io/        eyes4s-io       ASC, CSV, export                     } allowed effects
```

## Build and test

```sh
sbt compileAll          # all modules, JVM + JS
sbt testAll
sbt checkBoundaries     # both invariants below
sbt scalafmtAll scalafmtSbt
```

Before opening a PR, run what CI runs:

```sh
sbt headerCheckAll scalafmtCheckAll scalafmtSbtCheck githubWorkflowCheck
sbt testAll checkBoundaries
```

`.github/workflows/` is **generated** by sbt-typelevel. Do not hand-edit it; change
`build.sbt` and run `sbt githubWorkflowGenerate`.

## Design contract

These are the rules that survive review. Most exist because the reference R
implementation, `eyesim`, violates them somewhere and the resulting defect is
recorded in `PRD.md`'s Evidence Base.

1. **Parse, don't validate.** Domain types have private constructors and
   `Either`-returning smart constructors. Invariants belong to the type, not to
   the caller's discipline.

2. **No public API returns `null`, throws from a pure path, uses a sentinel, or
   silently changes its input's cardinality.** Dropped rows are returned as data.

3. **Failure is a value.** Recoverable failures are `Either` with a sealed error
   `enum` whose every case has a `def message: String` and **names its operands**,
   not just the reason. An application has to be able to point at what failed.

4. **Units are static, identity is nominal-runtime.** `Pt[Deg]` proves a position
   is in degrees. Only `FrameId` proves it is in the *same* degrees, and only
   `ClockId` proves two timestamps share a timeline.

5. **One identity check.** Comparing two coordinate systems goes through
   `Agreement`. Never re-inline the comparison; that seam had already opened once
   in this codebase after a single file.

6. **`eyes4s-kernel` contains no ocular vocabulary**, enforced by
   `checkKernelPurity` against a word list in `build.sbt`. If the check rejects a
   name, consider that it may be pointing at a real layering mistake before you
   reach for a synonym — that is what happened with `Viewing`, which became the
   neutral `Perspective` and belonged in the kernel after all.

7. **No pure module depends on `cats-effect` or `fs2`**, enforced by
   `checkModuleBoundaries` against the resolved dependency graph.

8. **State the convention in a type or a name, never in a comment.** Half-open
   intervals, y-axis direction, sigma-is-a-standard-deviation, and the
   straddling-boundary policy are all named types.

9. **A measure ships under the interface it actually satisfies.** Do not call
   something a `Metric` because it is distance-shaped.

10. **Every declared abstraction has an instance and a law suite or conformance
    test.** A trait with no inhabitant does not ship.

## Tests

- munit `FunSuite`; law suites via `munit.DisciplineSuite` and `checkAll`.
- Law suites are **published library code** in `eyes4s-laws`, not test-scope, so
  downstream authors can run them against their own instances.
- Every numerical law states its tolerance as a named `Tolerance`. No hidden
  epsilon — widening one silently is how a suite stops testing anything.
- **Verify a new law suite by mutation.** Break the implementation deliberately
  and confirm the suite fails. Two generator decisions in `WarpLaws` determine
  whether it tests anything at all, and neither is visible from a green run.
  If a mutant survives, establish whether it is *equivalent* before assuming a
  gap.
- Claims of compile-time rejection need a `typeCheckErrors` test.

## Decisions

The plan and its decision record live in `.mote/` (see `mote board`, `mote ready`).
Resolved design decisions are closed beads carrying a `decision`-kind note with
the full rationale. Cite them from source comments as `bead q-<name>`, following
the house convention in `linop4s`.

```sh
mote ready              # what is actionable now
mote show k-warp
mote ls --tag decision
```

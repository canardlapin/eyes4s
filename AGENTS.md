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
results/   eyes4s-results  report specifications and reports, the one result-table layer
codec/     eyes4s-codec    JSON codecs, versioned schema
laws/      eyes4s-laws     Discipline rule sets and generators (MAIN-scope deps)
fs2/       eyes4s-fs2      streaming execution                  } the only modules
io/        eyes4s-io       ASC, CSV, export                     } allowed effects

studio/core/     eyes4s-studio-core     document, commands, StudyBackend (JVM+JS; effect-permitted)
studio/app/      eyes4s-studio-app      Elm-style model, intents, update, view-models (JVM+JS; pure)
studio/viz/      eyes4s-studio-viz      Intaglio scene builders (JVM+JS; pure)
studio/desktop/  eyes4s-studio-desktop  JavaFX shell, platform services (JVM only; effect-permitted)
```

The studio projects are the Eyes Studio application (`docs/studio/DESIGN_SPEC.md` §13).
They are not published, not in the root aggregate, and not in `compileAll`/`testAll`;
build them with `sbt studioAll` and check their style with `sbt studioStyleCheck`.
studio-desktop needs a minimum JDK 22 (JavaFX 24); JDK 25 LTS is recommended and used
in CI. studio-app and studio-viz are pure: they resolve Cats Effect transitively
through studio-core, so purity is enforced on their sources rather than their
dependency graph. `checkBoundaries` enforces that no library module depends on a
studio project, that studio-core/app/viz resolve no JavaFX artifact and their sources
name no `javafx`, `scaladock.fx`, `java.io`, `java.nio.file` or `java.nio.channels`
package (`java.nio` buffers and charsets are allowed), and that studio-app/viz
sources name no `cats.effect` or `fs2` package. Imports are read structurally
(selectors, renames, wildcards) and interpolated expressions are scanned
(`project/StudioLint.scala`).

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

What runs where (all generated from `build.sbt`):

- `checks.yml`, every push and PR: format/header/workflow checks, compile and test per
  matrix project (`rootJVM`, `rootJS`), MiMa, docs, boundaries, the site build, and
  `python3 tools/check-docs.py --platform jvm|js --skip-consumer` in the matching project.
  sbt-typelevel compiles with `-Werror` there; reproduce locally with
  `GITHUB_ACTIONS=true sbt 'project rootJVM' Test/compile 'project rootJS' Test/compile`.
- `evidence.yml`, weekly and on `workflow_dispatch`: the public API audit
  (`python3 tools/api-audit/run.py`, which runs `check.py`) and the published-artifact
  consumer (`python3 tools/check-docs.py --platform none --run-consumer`, built under
  `target/study-consumer`).
- `performance.yml`, weekly and on demand: the two-hour EyeLink performance court.

Locally, `python3 tools/check-docs.py` (default `--platform all`) needs both platforms'
test reports, a current `docs/tlSite`, and a consumer receipt from `--run-consumer`.

`.github/workflows/` is **generated** by sbt-typelevel. Do not hand-edit it; change
`build.sbt` and run `sbt githubWorkflowGenerate`.

### Landing a branch

The full gate takes 60–90 minutes, so run it once per branch.

- **While iterating**, run the affected suites with `testOnly` and the `-Werror` compile
  (`GITHUB_ACTIONS=true sbt 'project rootJVM' Test/compile 'project rootJS' Test/compile`).
- **Merge `main` once**, after review, then run the full gate on the merged tree:
  everything under "Before opening a PR", plus `python3 tools/api-audit/run.py --record`
  and `python3 tools/study-consumer/verify.py`. Record the audit **first** and commit the
  inventory: `DiagnosticCoverageJvmSuite` reads the committed inventory, so a `testAll`
  that runs before the re-record passes against a stale inventory and the branch lands red.
  Report the SHA and `HEAD^{tree}` that the gate covers.
- **Land by tree identity.** The merge into `main` must have the tree that was gated. When
  `main` has moved by a change that cannot affect the branch (another package's sources, the
  tracker, docs), the `-Werror` compile and `checkBoundaries` on the new merge suffice.
- **Batch landings**: land several reviewed branches in one window and gate the combined tree
  once before pushing.
- **Shared files.** `tools/api-audit/inventory.json` and `evidence.json` are re-recorded, never
  hand-merged. Diagnostic catalog codes are append-only in landing order, and the stable table
  never changes. `verify.py` publishes to the shared local ivy repository, so run one at a time.
- **Split a ticket** larger than about half a day into slice beads that each land and close on
  their own.

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

## Shared registries

Parallel changes append their own files instead of editing shared lists:

- **Declare a new built-in `DefinitionId` in the file that introduces it**, as a
  `val` of an object whose name ends in `Definitions`, built with
  `DefinitionId.builtIn`. Do not append to the `DefinitionId` companion in
  `plan/StudyPlan.scala`; its existing identities stay where they are.
  `SchemaRegistryJvmSuite` finds every `*Definitions` object and still requires a
  registry entry, law and pinned fixture for each identity
  (`docs/DOMAIN_CODECS.md`).
- **eyesim parity evidence is per ticket.** Inputs go in
  `tools/r-parity/fixtures/cases/<ticket>.json`, fixtures and generators are
  registered in `tools/r-parity/manifest.d/<ticket>.json`, and
  `fixtures/baseline-cases.json` is frozen (`tools/r-parity/README.md`).
- **Do not write baseline case counts into prose.**
  `tools/r-parity/check_baseline.py` prints them.

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

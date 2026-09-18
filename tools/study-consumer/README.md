# A method author outside the eyes4s build

This small project imports published eyes4s APIs and law suites. It adds a comparison with a
validated multiplier, a distinct score type and an integer-valued custom trial key. It also
registers an extension-owned detector parameter type and codec around the canonical I-VT
implementation, preserving the algorithm card carried by the resulting detection artifact. Its
tests save/reload both study and recording plans, compare independently generated numerical
targets, export results, and exercise missing/duplicate registration and malformed schemas.
They also save the custom plan, input and result archive under a manifest and resolve it from an
in-memory source through the consumer's own registrations, under the published `ManifestLaws`.

From the eyes4s repository root:

```sh
python3 tools/study-consumer/verify.py
```

The script publishes JVM and Scala.js artifacts to the local Ivy cache under
`0.0.0-workflow-slices`, copies only this consumer's build and sources to a fresh temporary directory,
and runs both suites there. It verifies that the consumer classpath uses the packaged artifacts and
contains no path to the library checkout, and checks every entry of the classpath the fresh reader
below is launched with: the consumer's own output directories, holding only its `example` package;
the packaged eyes4s jars, each identical (by SHA-256) to the locally published artifact; and
third-party jars. Nothing else is accepted. The printed directory retains logs and a receipt. No artifact is
published remotely. `--skip-publish` is for rerunning against the already-built local version; after
changing library sources, run the full command again.

The custom multiplier predicts a proportional change in every contrast at each scale. Targets
come from the independent closed-form calculation in `tools/r-parity/generate_multiscale.py`, not
from an eyes4s run. This example demonstrates the comparison extension boundary; custom detector
registration is also exercised through packaged artifacts on both runtimes. A new detector
algorithm would additionally supply its own machine and truthful algorithm card. Smoother
registration remains later application-foundation work; progress and cancellation are exercised by
the journey below.

## The fixation journey: a reference integration for a UI

`FixationJourneySuite` is the first journey of the [app vision](../../docs/UI_APP_VISION.md), run
headless as an application would run it: a fixation table in, a matched/control cosine study out,
then save, reload, rerun, and inspection back to the table. It runs on the JVM and Scala.js, once
for the shipped cosine and once for this consumer's scaled cosine with its own key, parameter and
score types (`AnalysisRoute.cosine` and `AnalysisRoute.scaled`). Every step asserts typed values;
diagnostics are compared by their stable codes, never by message text. The steps, in order:

1. **Import.** `FixationCsv.read` with the route's `FixationKeyReader`, then
   `FixationEvidence.ledger(label, imported, AdmissionDecision.RequireComplete)`: one disposition
   per CSV record, and `requireComplete` for the input. One trial's onsets lie beyond 2^53
   microseconds and survive every later step exactly.
2. **Discover.** `Preflight.families`, the method's descriptor (components, ranges, execution
   capability), scales and policies built through `RecipeParameters` descriptors, and
   `plan.inspect` for every field's identity, version, units and value.
3. **Preflight.** `plan.preflight(available, budget)` with an explicit `PairScheduleBudget` (the
   default is effectively unbounded). Blockers for a missing input, a candidate budget and a
   selected-pair budget; a warning for a focal trial that lost its match; `report.prepare`
   refusing a plan or input changed since the report.
4. **Preview.** `work.preview` pages both designs with a `PairQuantum` against an edge list
   computed from the keys alone; `checkCurrent` refuses a preview kept across an edit.
5. **Run.** `StudyExecution[IO].events` gives the deterministic step sequence: segments in order,
   each with one stated `SegmentTotal`. `start(work, budget, quanta, between)` gives the run
   handle, whose progress stream a new observer reads while the run is held between two steps; a
   cancellation parked between two steps settles `Cancelled` with the last committed step and no
   result, and a completed run equals the pure `work.run`. A comparison budget below one
   comparison's cells is a `Failed` outcome before any step; a selected-pair budget the control
   design exceeds is a `Failed` outcome that keeps the last committed step. Both errors project
   through `Diagnostic.of`.
6. **Save.** `FixationJourney.save`: plan, input, admission ledger and result through the route's
   codecs, under one `SavedManifest` with `PlanInput`, `LedgerOf` and `ResultOf` relations, stored
   as `manifest.json`, one file per entry and the address in `manifest.sha256`.
7. **Reload.** A fresh resolver over a private copy of the stored bytes and fresh registrations
   (`FixationJourney.resolve`) on both runtimes. On the JVM, `FreshProcessJourneyJvmSuite` also
   stores the run in a directory, resolves it through `ArtifactFiles.directory`, and launches
   `JourneyReader` in a separate JVM over this consumer's classpath (its own classes and packaged
   jars), which knows only the route name and the directory, and reports the address it resolved,
   the run id, its rerun's fingerprint and archive digest, and its located failures.
8. **Rerun.** `FixationJourney.rerun` preflights and reruns the reloaded plan through the FS2
   runner. Every number of the result (masses, pair scores, reduced scores, contrasts) matches the
   original by its raw bits, every failure is in the same place, and the rerun's canonical archive
   has the stored entry's SHA-256. This holds for the complete run, for a run whose Gaussian scale
   fails in every pair, and for a reviewed import whose denominators are no longer uniform.
   Binned contrasts match the rational oracle of `tools/r-parity/fixtures/exact.json` and
   Gaussian contrasts the decimal oracle, within the named 1e-12 tolerance; the scaled route's
   binned values are the cosine's doubled, bit for bit.
9. **Inspect.** `ResultInspection.study` drills from a contrast row to its reductions, pairs and
   the CSV record numbers of every fixation. A bandwidth finer than the grid can express fails
   every pair of its scale; the failures are located, linked to their records and survive saving.
   A bad table row becomes `StudySources.rejections` and a ledger refusal naming its trial; a
   flipped byte, a missing file and a replaced input are refused by `ResolveError` with codes
   `resolve.digest`, `resolve.missing` and `resolve.identity`, naming the entry.

`FixationJourney` holds the steps an application repeats after the first run (save, file layout,
resolve, reload, rerun, fingerprint); `Journey` in the test sources holds the rest of the script.
The application owns everything else: file locations, autosave, scheduling and wording. A resolved
plan and result keep their parameter and score types abstract (`LoadedStudy`, `LoadedResult`), so
the application re-reads them through the typed codecs it registered before preparing, running or
inspecting them. The journey covers fixation inputs only: recording and temporal routes, the
execution laws and the response budget belong to G1.

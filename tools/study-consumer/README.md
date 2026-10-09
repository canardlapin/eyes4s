# A method author outside the eyes4s build

This small project imports published eyes4s APIs and law suites. It adds a comparison with a
validated multiplier, a distinct score type and an integer-valued custom trial key. It also
registers an extension-owned detector parameter type and codec around the canonical I-VT
implementation, preserving the algorithm card carried by the resulting detection artifact. Its
tests save/reload both study and recording plans, compare independently generated numerical
targets, export results, and exercise missing/duplicate registration and malformed schemas.
They also save the custom plan, input and result archive under a manifest and resolve it from an
in-memory source through the consumer's own registrations, under the published `ManifestLaws`.
Beyond that, it is the headless reference application of the UI foundation plan: three journeys
(fixation, recording and temporal) run from import to a saved, reloaded and rerun result through
published eyes4s APIs only, each for a shipped method and for one of the consumer's own.

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
inspecting them. `SavedRun` holds the file layout and `JourneyError` the typed refusals that every
route shares.

## The recording and temporal journeys (UI-G1)

`RecordingJourneySuite` and `TemporalJourneySuite` run the same journey for the other two shipped
plan families, on the JVM and Scala.js, each once for a shipped method and once for one of this
consumer's own:

| Route | Shipped | The consumer's own |
|---|---|---|
| Recording (`RecordingRoute`, `RecordingJourney`) | I-VT through `RecordingCodecs.ivt` | `CustomDetector`: a laboratory I-VT with its own parameter type, typed field descriptors (its own error type), `RecordingMethodDescriptor`, codec and schemas |
| Temporal (`TemporalRoute`, `TemporalJourney`) | cosine over `StudyKey` | the scaled cosine over `SubjectItemKey` with `Multiplier` and `ScaledScore` |

The recording journey starts from a normalized `RecordingInput` (channels, viewing geometry and
observed synchronization marks): the pinned I-VT conformance fixture
`ivt-symmetric-central-boundaries` of [`tools/detector-conformance/reference.json`](../detector-conformance/reference.json)
(its fixations are the pymovements 0.26.2 oracle's, mapped to half-open support, and verify.py
checks that mapping; the saccade between them is the fixture's stated eyes4s expectation),
written as display pixels (`x = 500 + 600 tan(theta)` on a 1000 x 1000 mm display at 600 mm, as decimal
literals so the recording's digest is the same on both runtimes). Its steps:

1. **Discover.** `Preflight.families`, the detector's `RecordingMethodDescriptor` (typed fields,
   algorithm card, execution capability) and `plan.inspect` for every field of the plan.
2. **Preflight.** `plan.preflight(Some(recording))` names a missing recording, a missing viewing
   geometry, a display that disagrees with the plan and marks the residual limit rejects;
   `report.confirm` refuses a plan or a recording changed since the report.
3. **Run.** `RecordingExecution[IO]` with two samples per step: one exact total per segment
   (synchronizing 1, warping 1, interpolating 10, detecting 10, assigning 1), a cancellation
   parked between two detection chunks settles `Cancelled` with the last committed step and no
   analysis, and a completed run is the pure `plan.run`, bit for bit.
4. **Inspect.** `ResultInspection.recording` lists every event by its sample range, linked to the
   samples of the input recording; the events match the oracle's kinds and microsecond spans
   exactly and its places within 1e-9 degrees.
5. **Save and reload.** `RecordingJourney.save`: the recording input, its channels as a packed
   recording with four `packed-array@1` payloads, the plan (`recording-plan`) and the analysis
   (`recording-result@1`), with `RecordingOf`, `PayloadOf`, `RecordingPlanInput` and
   `RecordingResultOf` relations. A fresh resolver over a private copy of the bytes, with fresh
   registrations (`route.decoders`: `withRecordings` and the pixel witness), reloads it;
   `RecordingJourney.rerun` checks `RecordingInput.disagreements`, preflights and reruns it. The
   rerun's fingerprint (every sample and event double as raw IEEE bits) and canonical archive
   SHA-256 equal the original's.

The temporal journey imports the pinned temporal table (`TemporalConsumerFixtures`, which
`generate_temporal.py` writes together with `tools/r-parity/fixtures/temporal-study.csv` and
`temporal.json`; verify.py checks the table against that file and runs the generator's `--check`) through the fixation
route's reader and ledger, gives every trial a measured epoch (anchor 0 and the fixture's observed
coverage on the trial's own clock), and runs four windows by two repetitions at the binned and
sigma 1 scales. Each repetition's prepared pair design, paged without scoring, is exactly the oracle's
pair table (36 pairs, the third phase's trials named as excluded). Preflight warns (and only warns)
that no trial is observed in the `outside` window;
the run's cells follow one another with exact preparation totals; every occupancy ledger equals the
independent integer-overlap ledger and all 96 contrasts the 60-digit targets of
[`temporal.json`](../r-parity/fixtures/temporal.json) within 1e-12 (twice the target for the
scaled cosine, bit for bit in the binned scale). `ResultInspection.temporal` locates every failure of
the unobserved window by repetition, window, scale and trial, linked to its CSV records.
`TemporalJourney.save` stores the base input, its ledger, the temporal input (base by reference),
the plan and the result with `LedgerOf`, `TemporalBase`, `TemporalPlanInput` and
`TemporalResultOf`; a fresh resolver reloads it and `TemporalJourney.rerun` reproduces it bit for bit.

On the JVM, `FreshProcessRoutesJvmSuite` stores each of the four runs in a directory, resolves it
through `ArtifactFiles`, and launches `RouteReader` in a separate JVM over this consumer's classpath.
The reader makes its registrations from nothing, knows only a route name and a directory, and
reports the archived and rerun fingerprints and the rerun's canonical archive digest, which must
equal the writer's. On Scala.js the same journeys run in one process: every reload, value identity
and rerun is checked there, and verify.py compares the two runtimes' portable evidence.

### Failure paths

Every path asserts the exact typed outcome it names and what it retains. Recording has no pairs,
so its failed-pair row is the analogous failed stage:

| Path | Fixation (G0) | Recording | Temporal |
|---|---|---|---|
| Missing or changed artifact | `resolve.digest`, `resolve.missing`, `resolve.identity` naming the entry | a flipped archive byte (`Digest`), a missing payload file (`Missing(recording.values)`), another session's input re-declared consistently (`Identity`); also in a separate JVM | a flipped archive byte, a missing ledger, a temporal input without one epoch re-declared consistently (`Identity`) |
| Unknown extension | an archive of the scaled cosine without its result codec (`MissingResultCodec`, `ConsumerSuite`) | no recording registrations: `UnsupportedSchema(recording-plan, schema, [])`; the other detector's registrations: `UnsupportedSchema(…, [registered])` and `MissingResultCodec(method)`; the same refusal from a separate JVM | fixation registrations only: `UnsupportedSchema` for plan and result; empty temporal registries: `MissingMethod` and `MissingResultCodec` by the base method |
| Invalid geometry | a degenerate display is `geometry.degenerate-bounds`; the table read on a display named like the plan's with other bounds is a `FrameMismatch` warning per trial (`AlignFrame`), and every estimation fails as `study-failure.frame`, 48 located failures | a zero viewing distance is refused by the typed descriptor (`NonPositivePerspective`) and, edited into a saved plan, by decoding (`Decode(recording-plan.json, Field(perspective))`); a recording naming the plan's display with other bounds is a `FrameMismatch` blocker, `NotReady`, and `Failed(Geometry(FrameIdentityConflict), None)` before any step | the same table on the other display: a `FrameMismatch` finding per trial, and every trial's estimation `study-failure.frame` in all eight cells |
| Cancelled run | inside a scale's matched comparison, and before step 1 | between two detection chunks, and before step 1 | inside the second cell's matched comparison, and before step 1 |
| Stale revision | `ChangedPlan`, `ChangedInput`, a stale preview | `ChangedPlan(EventRecording, [interpolationGapMicros])`, `ChangedInput` | `ChangedPlan(TemporalStudy, [temporal.boundary])`, `ChangedInput` |
| Failed pair | a bandwidth finer than the grid fails every pair of its scale; `MixedFailureJourneySuite` also empties one admitted reference with `dropFirst`, retaining 5/6 matched and 10/12 control successes at one scale under RequireAll and SuccessfulOnly (minimum 1 and 2), through save/reload/rerun | no pairs; the analogue is marks the plan's residual limit rejects: a `Synchronization` blocker and `Failed(Synchronization(…), None)` | the unobserved window fails every pair of both scales (108 located failures per cell: occupancy `surface.degenerate-total` binned, `estimate.no-mass` Gaussian); a trial without an epoch is `temporal.missing-epoch` in every cell |

### The published laws over the consumer's routes

`ConsumerLawsSuite` runs `ExecutionLaws.conformance` over the cursor of every route (the fixation
journey at the binned and sigma 1 scales, both recording routes, and both temporal routes over one
repetition of an observed and an unobserved window), each with an independent oracle: the rational
and decimal cosine targets, the I-VT conformance events, and the integer-overlap ledgers with the
decimal temporal targets. It runs `ManifestLaws.verifiedResolution` over the consumer's own writers
and registrations for all six routes. One deliberate mutant per law set shows the laws have teeth
here: a detection total overclaimed by one sample, and a writer that drops a packed payload.

### The JVM response envelope

`ResponseEnvelopeJvmSuite` times every step of each route's cursor and the latency from `cancel` to a
settled outcome while a step is in flight (at the slowest step and three spread steps), and adds the
widest study [the declared envelope](../../docs/EXECUTION_RESPONSIVENESS.md) supports (256 x 256
cells, Gaussian sigma 32) and a 10 s recording at 1 kHz. It asserts no time, only the runner's outcome
contract; verify.py copies every workload and the JVM into its receipt. Test suites run one at a time
on the JVM so that nothing competes with the measurement. The run for UI-G1 (Apple M3 Max, JDK 25.0.1,
six visible processors, 3 GiB heap) is recorded in
[the plan's G1 outcome](../../docs/UI_FOUNDATION_PLAN.md#g1-outcome).

### The receipt

verify.py's `receipt.json` names the artifacts (version, each jar's SHA-256, the library revision
they were published from), the fixtures (SHA-256 of every pinned oracle input), the runtime (Python,
platform, sbt, Scala and the measuring JVM), the gates (the commands, both runtimes' test totals and
every check it made), the runtime evidence of every journey, the fresh-process receipts, the response
envelope and the SHA-256 of every tested source file.

### Limits

What the consumer proves stops here; each limit is an open follow-up or a stated boundary:

- Codec refusals of a constructor's value (a zero viewing distance) carry the constructor's
  message as the reason of `codec.field`, not its typed error.
- A resolved plan exposes its typed plan (`LoadedStudy.plan`), but its parameter and score types
  stay abstract; the journey still re-reads the plan and result through the route's typed codecs
  so that they share the route's own score types.
- No fixture has a scale where some pairs fail and others succeed (`bd-01M2SG47P8BZYP9BG35QWYNKHY`).
- Detection support assembly is quadratic in events x samples, which the 10 s recording shows as its
  slowest step (`bd-01M2S5AX3E1E3PRGS3CR322HAX`); a recording descriptor always states
  `SynchronousWholeOperation`, although the runner feeds its machine in sample chunks.
- An admission ledger's exclusions are not verified against the source by re-running the importer
  (`bd-01M2SC6N15J2N7PHD4DXBE43VD`).
- Binocular recordings and recording inputs with channels by reference are not packed
  (`bd-01M2SC6NKVX2E7DN8PD4Q990ND`).
- No realistic-size input is archived: the fixtures are small by design, and no permitted
  realistic-size dataset is in the repository.
- Recording and Gaussian results are rerun bit for bit within a runtime and across JVM processes, not
  from a JVM archive on Scala.js: the angular warp and Gaussian smoothing use transcendental functions
  that neither platform promises to round identically. Across runtimes, verify.py compares them
  within the named tolerances.

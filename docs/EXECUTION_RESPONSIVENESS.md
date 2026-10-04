# Execution responsiveness

This page states where the 100 ms JVM cancellation target holds on the current source, for
declared workloads on a pinned runtime.

**Current verdict.** On the pinned runtime every declared workload meets 100 ms except the
1024×1024 σ = 32 study, whose single estimation step takes 781 ms. That case is the
adversarial estimation workload; resumable estimation within a trial (X2) stays deferred
until a consumer needs grids beyond 256×256 with wide kernels. Recordings of 600,000
samples meet the target at default and smallest quanta. No larger envelope, hosted
supported-JDK qualification or universal real-time guarantee is claimed.

## What is measured

The plan's shared acceptance rule 8 proposes a 100 ms JVM cancellation target and
requires it to be frozen against a declared workload and a pinned runtime, not
inferred from a failure. The runner makes each step, one `advance` plus its
bookkeeping, an uncancelable region and cedes before every step, so two numbers
bound responsiveness:

- **Step duration.** The longest time the runner cannot yield. The harness drives
  each family's cursor through its `Stepwise` instance, exactly as the runner's
  uncancelable region calls it, and times every `advance`. The maximum is
  attributed to its stage kind (estimating, comparing, reducing, contrasting,
  preparing, synchronizing, warping, interpolating, detecting, assigning).
- **Cancellation latency.** The time from `run.cancel` to a settled outcome
  through `Execution.start` under the global `IORuntime`. A fresh run is started
  per trial. The between-step hook announces step `n`, and a fiber on another
  worker cancels at once, while step `n` is in flight. One trial targets the
  slowest step found above, which is the adversarial case. Six more are spread
  evenly over the run.

The harness also reports the **runner gap**, the longest interval between two of
the runner's yield points in a complete run through `Execution.start`. It includes
bookkeeping, progress publication, `cede` and any garbage collection that lands
there. The step-duration p95 comes from a logarithmic histogram (ratio 1.05), so
it reads at most 5% high. Runs longer than eight million steps get the pure profile
only, and their cancellation trials fall within the first eight million steps. The
table marks them.

Work buffering is structural, not measured. `start` holds at most one progress
value in its coalescing slot, and `events` emits at most two elements per step, so
no buffer grows with the length of a run. Retained results do grow: one mass per
trial per scale, `cells × 8` bytes each.

Nothing here is asserted. The harness exits non-zero only when a run breaks its
outcome contract: a cancelled run that holds a result, a completed run that
skipped steps, or a failure. The deterministic laws (cancellation only between
steps, first commit wins) are proved under `TestControl` in
`ExecutionConformanceSuite`.

## Pinned runtime

| Item | Value |
|---|---|
| Hardware | Apple M3 Max laptop, 14 cores |
| Operating system | Mac OS X 14.3, aarch64 |
| JVM | OpenJDK 64-Bit Server VM 25.0.1 (sbt's Java) |
| JVM arguments | `-Xms2g -Xmx2g -XX:+UseG1GC -XX:ActiveProcessorCount=4` (from `fs2ModuleJVM / Test / run / javaOptions`) |
| Processors visible | 4, so the global `IORuntime` compute pool matches a four-vCPU CI runner |
| Effect runtime | `cats.effect.unsafe.implicits.global` |
| Measured | 2026-09-19T13:04:19Z, `--profile full`, source `131970d` plus resumable detection assembly |

Timings are this machine's. The plan's own caveat applies: 100 ms is a target to
freeze and test on a declared workload, not a portable real-time guarantee.

## Declared workloads

| Workload | Route | Envelope |
|---|---|---|
| `study-fixture` | study | The R-pinned fixation fixture (rational cosine, decimal Gaussian): 24 trials, 2×2 grid, binned and Gaussian σ = 1 |
| `temporal-fixture` | temporal | The R-pinned temporal fixture: 36 trials, 4 windows × 2 repetitions, 2×2 grid, two scales |
| `recording-fixture` | recording | The recording suite's fixture: 40 samples at 100 Hz, IVT, one AOI |
| `recording-60s`, `recording-600s` | recording | Synthetic 1000 Hz recordings: 250 ms fixations on a 5×5 lattice, 20 ms saccades, a 40 ms loss burst in every seventh fixation, IVT, one AOI |
| `study-100x256-*` | study | 5 participants × 10 stimuli × {encode, recall} = 100 trials of 10 seeded fixations, 256×256 grid of one-pixel cells, binned or Gaussian σ = 8 or 32 cells (Truncate); 50 matched and 450 control pairs |
| `study-4x512-sigma32`, `study-4x1024-sigma32` | study | 4 trials, 512×512 or 1024×1024 grid, Gaussian σ = 32 cells: one large grid per trial, the adversarial estimation case |

"Default" quanta are `WorkQuanta.default`: pair 1024, comparison 65,536 cells,
sample 4096. "Smallest" quanta are 1 for every quantum.

## Results

All times are in milliseconds. The last column checks the largest of max step, runner gap
and cancellation latency against 100 ms. The full report, per-stage step durations and the
measured source digests are in [the assembly evidence](evidence/detection-assembly-final.md).

| workload | route | quanta | steps | max step (stage) | p95 step | runner max gap | cancel, adversarial | cancel, max | cancel, median | 100 ms |
|---|---|---|---:|---|---:|---:|---:|---:|---:|---|
| study-fixture | study | default | 110 | 0.083 (reducing) | 0.053 | 0.127 | 0.147 | 0.147 | 0.059 | meets |
| study-fixture | study | smallest | 376 | 0.384 (comparing) | 0.021 | 0.146 | 0.097 | 0.097 | 0.047 | meets |
| temporal-fixture | temporal | default | 1048 | 0.400 (preparing) | 0.033 | 2.341 | 0.130 | 0.200 | 0.071 | meets |
| temporal-fixture | temporal | smallest | 2960 | 0.057 (reducing) | 0.007 | 0.162 | 0.065 | 0.135 | 0.063 | meets |
| recording-fixture | recording | default | 12 | 0.580 (assigning) | 0.580 | 0.735 | 0.335 | 0.335 | 0.205 | meets |
| recording-fixture | recording | smallest | 373 | 0.234 (interpolating) | 0.024 | 0.669 | 0.134 | 0.207 | 0.051 | meets |
| recording-60s | recording | default | 136 | 12.835 (assigning) | 1.396 | 11.462 | 19.786 | 19.786 | 0.944 | meets |
| recording-60s | recording | smallest | 523724 | 19.378 (assigning) | 0.000 | 12.425 | 2.406 | 2.406 | 0.043 | meets |
| study-100x256-binned | study | default | 1107 | 0.807 (estimating) | 0.705 | 1.505 | 0.762 | 0.762 | 0.084 | meets |
| study-100x256-sigma8 | study | default | 1107 | 5.458 (estimating) | 5.212 | 9.069 | 5.031 | 5.031 | 0.074 | meets |
| study-100x256-sigma32 | study | default | 1107 | 19.329 (estimating) | 17.650 | 19.420 | 17.467 | 17.467 | 0.098 | meets |
| study-100x256-binned | study | smallest | 32770352 | 1.017 (comparing) | 0.000 | not run | 0.060 | 0.124 (first 8M steps) | 0.062 | meets |
| study-100x256-sigma8 | study | smallest | 32770352 | 6.533 (estimating) | 0.000 | not run | 4.640 | 4.640 (first 8M steps) | 0.098 | meets |
| study-100x256-sigma32 | study | smallest | 32770352 | 35.180 (estimating) | 0.000 | not run | 16.948 | 16.948 (first 8M steps) | 0.110 | meets |
| study-4x512-sigma32 | study | default | 31 | 96.581 (estimating) | 96.581 | 83.388 | 85.428 | 85.428 | 0.078 | meets |
| study-4x1024-sigma32 | study | default | 79 | 780.743 (estimating) | 685.390 | 702.891 | 658.544 | 658.544 | 0.116 | **misses** |
| recording-600s | recording | default | 1286 | 63.237 (assembly-Gaps) | 0.777 | 48.047 | 0.116 | 15.311 | 0.223 | meets |
| recording-600s | recording | smallest | 5235966 | 46.100 (interpolating) | 0.000 | 31.641 | 13.007 | 15.710 | 0.089 | meets |

Outcome-contract violations: none. The observation cap is eight million steps, which covers
every step of the smallest-quantum 600,000-sample run; the 32-million-step study rows are
observed over their first eight million steps.

## Study route and X2

With one trial per step, estimation is the largest step of every scaled study workload that
uses a Gaussian. On the 100-trial 256×256 study it costs at most 1.0 ms binned, 6.5 ms at
σ = 8 and 35.2 ms at σ = 32 (the smallest-quanta run; 19.3 ms at default quanta), and
cancellation settles within 17.5 ms. The step cost is per trial, so more trials make a run
longer, not its steps; what more trials cost is heap. Each estimated trial retains one mass
per scale, 65,536 cells × 8 bytes = 512 KiB at 256×256, so the pinned 2 GiB heap holds
roughly 3,000 retained trial-scales with headroom for the rest of the run. Beyond that the
collector, not the step, sets the pause. Cost grows with cells × kernel taps (193 taps at
σ = 32): a 512×512 σ = 32 step takes 96.6 ms, inside the target with no headroom, and a
1024×1024 step takes 781 ms.

Comparison, reduction and contrast steps never exceed 4.2 ms at any quanta, so pair-level
quanta meet the budget; shrinking the quanta cannot shorten estimation.

X2 (resumable estimation within a trial) is therefore **not needed** for the declared
envelope: grids of at most 65,536 cells (256×256) with Gaussian bandwidths up to 32 cells,
and as many trial-scales as the heap retains at 512 KiB each. The 512×512 σ = 32 case has no
headroom on this hardware, so a portable claim does not include it. Schedule X2 when a
consumer needs larger grids with wide kernels; its scope is the yields inside the two
convolution passes and the normalisation of one trial. Until then a consumer stays inside the
envelope by choosing the grid, and the M3 preflight is where to warn when cells × kernel taps
exceed it. CI's smoke profile reports the σ = 32 workload on GitHub's four-vCPU runner with
JDK 17, and that output extends or narrows the envelope there.

## Recording route

Detection is resumable through final assembly. Event support uses one binary
timestamp-bounded search shared by `Detection` and `EventSeries`, and `Recording` memoizes its
extent and content hash. `DetectionCursor.advance(sampleMaximum, assemblyMaximum)` bounds
feeding and assembly separately; its one-argument overload uses the same limit for both.
`DetectionPage.workUnits` counts fed samples before assembly and operations within one
assembly phase thereafter; `consumed` is a source sample count. The page that feeds the last
sample returns `More` with a cursor in assembly, and `consumed` equals `total` from then on;
only an assembly page (or a detector failure) returns `Done`, so a cursor consumer loops until
`Done`, not until `consumed == total`.

`RecordingPlan` uses `WorkQuanta.samples` for both limits, with
`RecordingStage.Assembling(phase)` and matching segments. Feeding keeps exact sample totals;
data-dependent assembly totals are `Unknown`, and empty phases may be absent. Assembly covers
emissions, support searches, structural validation, fixation summaries (including median
selection), source hashing and lineage, gap checks, and labels and report accounting. All
continuation state is immutable, direct execution drains the same work, hashes and numerical
accumulation order are preserved, and cancellation returns no partial artifact.

Synchronization, warping, interpolation finalization and AOI assignment remain whole steps.
At 600,000 samples synchronization takes about 4 ms, warping 21 ms, interpolation
finalization up to 46 ms (smallest quanta) and assignment 14–18 ms; by linear projection the
largest passes 100 ms at roughly 1.3 million samples. Custom machine
steps and flushes and irregular-recording setup are not bounded by assembly quanta.

## Bounded synchronous measures

Some measures run as one synchronous, uncancellable call with no cursor, so a
typed work limit, checked before any work, is what keeps one call short. Each row
is the current default and the measured cost of its largest admitted input.

| Measure | Work unit | Default limit | Largest admitted input | Measured at that input | Refusal |
|---|---|---:|---|---:|---|
| `Distribution.distanceCorrelation` (`MapSimilarityMethod.DistanceCorrelation`) | unordered cell pair, `n(n-1)/2` | 2^28 = 268,435,456 | 23,170 cells (152×152 = 23,104) | 442–472 ms | `CompareError.WorkLimitExceeded(measure, cells, pairs, limit)` |

Distance correlation visits each unordered cell pair twice: once for the
distance-matrix row sums and once for the centred products. It holds O(cells)
state. Measured 2026-09-25 on the pinned M3 Max, OpenJDK 25.0.1, in sbt's test JVM
with uniform random maps, one warm-up call per size:

| Grid | Cells | Pairs | Current ms | Previous full-matrix ms |
|---|---:|---:|---:|---:|
| 64×64 | 4,096 | 8,386,560 | 15 | 47 |
| 96×96 | 9,216 | 42,462,720 | 72 | 205 |
| 128×128 | 16,384 | 134,209,536 | 227 | 748 |
| 2317×10 | 23,170 | 268,412,865 | 446 | 1,264 |
| 160×160 | 25,600 | 327,667,200 | 556 | 1,573 |
| 256×256 | 65,536 | 2,147,450,880 | 3,575 | 10,981 |

That is about 1.7 ns per pair. The previous formulation visited all n² ordered
pairs three times; the current one visits n(n-1)/2 pairs twice. The two agree to
rounding: the largest relative difference observed was 2.8e-12, and the pinned
eyesim fixtures still pass at `MapTolerance = 1e-12`. A 256×256 grid is refused by
default; a caller that accepts a call of several seconds passes
`DistanceCorrelationLimit.of(n)` to `Distribution.distanceCorrelationWithin`,
`MapSimilarityMethod.similarityWithin` or `MapComparison.scales`. Repetition plans
use the default limit, and the limit is not on the plan wire.

## Reproduce

```sh
sbt "fs2ModuleJVM/Test/runMain eyes4s.fs2.ExecutionResponsivenessMain --profile smoke"
sbt "fs2ModuleJVM/Test/runMain eyes4s.fs2.ExecutionResponsivenessMain --profile full \
  --source-revision <sha> --hardware <label> --output target/execution-responsiveness.md"
```

The smoke profile covers the fixtures at both quanta, the 60 s recording and the
100-trial 256×256 study binned, at σ = 8 and at σ = 32, the widest bandwidth the
envelope claims. It takes about ten seconds, and CI runs it on `rootJVM` with Java
17. The full profile adds the smallest-quanta synthetic runs, the large grids and
the 600 s recording, and takes about 11 minutes. The harness is `fs2/.jvm/src/test/scala/eyes4s/fs2/ExecutionResponsivenessMain.scala`.

## Evidence

- [Resumable detection assembly](evidence/detection-assembly-final.md): the current full
  report, per-stage step durations and measured source digests.
- [Detection support, initial](evidence/detection-support-initial.md) and
  [final](evidence/detection-support-final.md): the binary support search, which removed the
  quadratic scan but still missed at 600,000 samples before assembly became resumable.
- [UI-X6, 2026-09-18](evidence/execution-responsiveness-x6-2026-09-18.md): the original
  measurement that kept X2 deferred and found the recording-route defect, with its per-stage
  appendix.

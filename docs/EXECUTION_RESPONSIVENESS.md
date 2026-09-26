# Execution responsiveness

## Update: resumable detection assembly, 2026-09-19

Follow-up `bd-01M2WTSXKJWP6AS2QVHX3CW9RB` makes final detection assembly
resumable. The unchanged 600k-sample workload now meets the 100 ms target on
the recorded M3 Max / OpenJDK 25.0.1 runtime. The [full report and measured
source digests](evidence/detection-assembly-final.md) retain every workload:

| Recording | Quanta | Max step ms | Max runner gap ms | Max cancellation ms | 100 ms |
|---|---|---:|---:|---:|---|
| 60k samples | default | 12.835 | 11.462 | 19.786 | meets |
| 60k samples | smallest | 19.378 | 12.425 | 2.406 | meets |
| 600k samples | default | 63.237 | 48.047 | 15.311 | meets |
| 600k samples | smallest | 46.100 | 31.641 | 15.710 | meets |

`DetectionCursor.advance(sampleMaximum, assemblyMaximum)` separately bounds
feeding and assembly operations; its one-argument overload uses the same limit
for both. `DetectionPage.workUnits` counts fed samples before assembly and
operations within one assembly phase thereafter (the field was previously
`samples`). `consumed` remains a source sample count. The page that feeds the last
sample now returns `More` with a cursor in assembly, and `consumed` equals `total`
from then on; only an assembly page (or a detector failure) returns `Done`. A cursor
consumer must loop until `Done`, not stop at `consumed == total`. `RecordingPlan` uses `WorkQuanta.samples` for both limits, with
`RecordingStage.Assembling(phase)` and matching segments. Feeding retains exact
sample totals; data-dependent assembly totals are explicitly `Unknown`. Empty
phases may be absent. Assembly covers emissions, support searches, structural
validation, fixation summaries (including median selection), source hashing and
lineage, gap checks, and labels/report accounting. All continuation state is
immutable; direct execution drains the same work. Hashes and numerical accumulation
order are preserved, and no partial artifact is returned on cancellation.

The observation cap increased to eight million steps to cover all 5,235,966
steps of the smallest-quantum 600k run. It changes neither the workload nor the
latency threshold. Outcome-contract violations: none. The 1024-square Gaussian
stress case still misses; X2 remains deferred under its existing consumer trigger.
Synchronization, warping, interpolation finalization and AOI assignment remain
whole steps where previously documented and are measured in this report. Custom
machine steps/flushes and irregular-recording setup are not universally bounded
by assembly quanta. No larger envelope, hosted supported-JDK qualification or
universal real-time guarantee is claimed.

The earlier support-search and original X6 reports below are historical evidence;
their recorded failures have not been removed.

## Update: detection support, 2026-09-19

The historical measurements below remain intact. The current local implementation
replaces both exhaustive event-support scans (Detection and EventSeries) with shared
binary timestamp bounds, and memoizes immutable Recording extent and content hash.
The first attempt removed quadratic scaling but narrowly missed the 60k-sample target;
its [full report](evidence/detection-support-initial.md) is retained. Duplicate hashing
was then removed, without changing the workload, runtime arguments or 100 ms threshold.
The [final full report](evidence/detection-support-final.md) records:

| Recording | Quanta | Max step ms | Max runner gap ms | Max cancellation ms | 100 ms |
|---|---|---:|---:|---:|---|
| 60k samples | default | 45.119 | 43.588 | 54.720 | meets |
| 60k samples | smallest | 46.985 | 60.254 | 35.895 | meets |
| 600k samples | default | 409.852 | 403.803 | 361.905 | misses |
| 600k samples | smallest | 331.708 | 401.948 | 319.441 | misses |

This qualifies the ticket's 60k workload on M3 Max/JDK 25 only. It removes the quadratic
failure but does not make final detection assembly resumable: the retained 600k case
still misses. APP-9's broader long-recording responsiveness remains open and requires
bounded assembly. All runs reported no outcome-contract violations. The JVM flags and
full workload results are in the reports. No supported-JDK hosted qualification is claimed.

Measured source: base `131970d` plus the support-search changes. Final source SHA-256:

- `core/src/main/scala/eyes4s/core/Recording.scala`: `11e085f799121d32577957ece141a1cae6046dcf00cf55c0e114524100013a4f`
- `core/src/main/scala/eyes4s/core/DetectionSupport.scala`: `f197eeea958e90206b54295e29b55c52efb428b94872e689f9567b52fb1cd9ba`
- `detect/src/main/scala/eyes4s/detect/DetectionResult.scala`: `f40dc3a8ccb2e6286983ac4f0fc3cc6e1b8a4e2b8a0d6000c7710325c85a537f`

Evidence for UI-X6 (`bd-01M2N2QCWATK57DT0TEWTRJTCP`), measured 2026-09-17 on base
revision `cb57ccf` plus the X6 working tree. It answers one question from the UI
foundation plan: does the shipped runner, which estimates one trial per step
(X2 deferred), meet the proposed 100 ms JVM cancellation target? The answer
decides whether X2 is scheduled.

**Decision.** Keep X2 deferred. On the measured runtime (below: an Apple M3 Max,
JDK 25.0.1, four visible processors, a 2 GiB heap), on every designated fixture and
on a 100-trial 256×256 study with Gaussian bandwidths up to 32 cells, the longest
step in the recorded full run was 19.6 ms and the slowest cancellation 17.2 ms (a
later smoke run of the σ = 32 workload measured 20.0 ms and 15.9 ms), five times
inside the budget. Estimation reaches the budget only at
512×512 with σ = 32 cells (98.6 ms) and misses it at 1024×1024 (647 ms per step,
955 ms to cancel). Schedule X2 when a consumer needs grids larger than 256×256 with
wide kernels. The recording route misses the target for an unrelated reason:
assembling detection support scans every sample once per event. That is a separate
defect, described [below](#recording-route-a-separate-miss), and X2 would not fix it.

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
it reads at most 5% high. Runs longer than two million steps get the pure profile
only, and their cancellation trials fall within the first two million steps. The
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
| Effect runtime | `cats.effect.unsafe.implicits.global`, Cats Effect 3.7.0 |
| Warm-up | three passes of the study fixture through the runner, then one default-quanta pass per workload before it is measured |
| Measured | 2026-09-18T02:06:46Z, `--profile full`, 683 s |

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

All times are in milliseconds. The last column checks the largest of max step,
runner gap and cancellation latency against 100 ms.

| workload | route | quanta | steps | max step (stage) | p95 step | runner max gap | cancel, adversarial | cancel, max | cancel, median | 100 ms |
|---|---|---|---:|---|---:|---:|---:|---:|---:|---|
| study-fixture | study | default | 110 | 0.788 (contrasting) | 0.061 | 0.131 | 0.098 | 0.098 | 0.052 | meets |
| study-fixture | study | smallest | 376 | 0.259 (comparing) | 0.017 | 0.124 | 0.050 | 0.077 | 0.048 | meets |
| temporal-fixture | temporal | default | 1048 | 0.485 (contrasting) | 0.034 | 0.973 | 0.064 | 0.259 | 0.087 | meets |
| temporal-fixture | temporal | smallest | 2960 | 0.093 (preparing) | 0.007 | 0.205 | 0.091 | 0.091 | 0.066 | meets |
| recording-fixture | recording | default | 5 | 0.854 (detecting) | 0.854 | 0.664 | 0.761 | 0.761 | 0.144 | meets |
| recording-fixture | recording | smallest | 83 | 0.432 (detecting) | 0.110 | 0.336 | 0.341 | 0.341 | 0.034 | meets |
| recording-60s | recording | default | 33 | 501.569 (detecting) | 11.946 | 500.944 | 385.454 | 385.454 | 0.669 | **misses** |
| recording-60s | recording | smallest | 120003 | 368.077 (detecting) | 0.001 | 369.251 | 378.025 | 378.025 | 0.042 | **misses** |
| study-100x256-binned | study | default | 1107 | 3.843 (estimating) | 0.640 | 0.835 | 0.759 | 0.759 | 0.064 | meets |
| study-100x256-sigma8 | study | default | 1107 | 5.495 (estimating) | 5.212 | 6.316 | 5.152 | 5.152 | 0.101 | meets |
| study-100x256-sigma32 | study | default | 1107 | 18.439 (estimating) | 17.650 | 17.831 | 17.159 | 17.159 | 0.065 | meets |
| study-100x256-binned | study | smallest | 32770352 | 1.165 (comparing) | 0.000 | not run | 0.041 | 0.083 (first 2M steps) | 0.060 | meets |
| study-100x256-sigma8 | study | smallest | 32770352 | 5.922 (estimating) | 0.000 | not run | 4.923 | 4.923 (first 2M steps) | 0.066 | meets |
| study-100x256-sigma32 | study | smallest | 32770352 | 19.637 (estimating) | 0.000 | not run | 16.407 | 16.407 (first 2M steps) | 0.101 | meets |
| study-4x512-sigma32 | study | default | 31 | 98.649 (estimating) | 92.721 | 84.627 | 88.255 | 88.255 | 0.132 | meets, no headroom |
| study-4x1024-sigma32 | study | default | 79 | 647.303 (estimating) | 592.066 | 893.115 | 954.717 | 954.717 | 0.307 | **misses** |
| recording-600s | recording | default | 297 | 55437.454 (detecting) | 3.528 | 53214.753 | 49899.410 | 49899.410 | 0.532 | **misses** |
| recording-600s | recording | smallest | 1200003 | 81190.768 (detecting) | 0.000 | 52605.767 | 59449.906 | 59449.906 | 0.179 | **misses** |

Maximum step by stage kind, in milliseconds (full per-stage output in the
[appendix](#appendix-per-stage-step-durations)):

| workload | quanta | estimating | comparing | reducing | contrasting |
|---|---|---:|---:|---:|---:|
| study-100x256-binned | default | 3.843 | 0.161 | 0.605 | 0.070 |
| study-100x256-sigma8 | default | 5.495 | 0.105 | 0.356 | 0.044 |
| study-100x256-sigma32 | default | 18.439 | 0.088 | 0.242 | 0.028 |
| study-100x256-binned | smallest | 0.403 | 1.165 | 0.153 | 0.032 |
| study-100x256-sigma8 | smallest | 5.922 | 0.944 | 0.138 | 0.017 |
| study-100x256-sigma32 | smallest | 19.637 | 1.170 | 0.109 | 0.017 |
| study-4x512-sigma32 | default | 98.649 | 0.058 | 0.043 | 0.022 |
| study-4x1024-sigma32 | default | 647.303 | 0.459 | 0.081 | 0.036 |

The adversarial trial did land mid-step. Its latency tracks the slowest step it
targeted (17.2 ms against 18.4 ms for σ = 32, 385 ms against 502 ms for the 60 s
recording), whereas spread trials settle in well under a millisecond.

## Verdict against 100 ms

**Fixtures.** All three routes meet the target at both default and smallest
quanta. No step exceeds 0.9 ms and no cancellation exceeds 0.8 ms.

**Estimation.** With one trial per step, estimation is the largest step of every
scaled study workload that uses a Gaussian. On the 100-trial 256×256 study it
costs at most 3.8 ms binned, 5.9 ms at σ = 8 and 19.6 ms at σ = 32, and cancellation
settles within 17.2 ms. That leaves at least five times headroom. The step cost is
per trial, so more trials make a run longer, not its steps; what more trials cost
is heap. Each estimated trial retains one mass per scale, 65,536 cells × 8 bytes
= 512 KiB at 256×256, and a Gaussian step allocates a few more such arrays
transiently. Against the pinned 2 GiB heap that is room for roughly 3,000
retained trial-scales with headroom for the rest of the run; the measurement
retained 100. Beyond that the collector, not the step, sets the pause, and this
measurement says nothing about it. Cost grows with cells × kernel taps (193 taps at
σ = 32) and faster than linearly once the grid leaves cache: about 0.7 ns per
multiply-add at 256×256, about 0.95 ns at 512×512 and about 1.5 ns at 1024×1024.
At 512×512 with σ = 32 a step takes 98.6 ms, and at 1024×1024 it takes 647 ms, with
955 ms to cancel.

**Quanta govern everything else.** Comparison, reduction and contrast steps never
exceed 1.2 ms at any quanta, and the largest of those are single-cell steps that
absorbed a collection pause. Pair-level quanta therefore meet the budget. Shrinking
the quanta cannot shorten estimation: at the smallest quanta the slowest step of
every Gaussian workload is still an estimation step of the same length.

## X2 recommendation

X2 (resumable estimation within a trial) is **not needed** for the declared
envelope and should stay deferred. The envelope supported by this evidence, for the
100 ms target on the measured runtime only, is grids of at most 65,536 cells
(256×256) with Gaussian bandwidths up to 32 cells, and as many trial-scales as the
heap retains at 512 KiB each (about 3,000 under the pinned 2 GiB; 100 measured).
It is not a claim for other hardware or JVMs: CI's smoke profile now reports the
σ = 32 workload on GitHub's four-vCPU runner with JDK 17, and that output is what
extends or narrows the envelope there. The 512×512 σ = 32 case meets the target
with no headroom on this hardware, so a portable claim should not include it.

Schedule X2 when a consumer needs grids beyond 256×256 with wide kernels. Its
scope is then exactly what this measurement isolates: yields inside the two
convolution passes and the normalisation of one trial. Until then a consumer can
stay inside the envelope by choosing the grid; the M3 preflight is the natural
place to warn when cells × kernel taps exceed it.

## Recording route: a separate miss

The recording route misses the target on realistic lengths, and X2 would not help.
The slowest step is the last detection step: 368–502 ms at 60,000 samples and
55–81 s at 600,000. That is 110–220 times the time for ten times the samples. The
sample quantum does not change it, because this work is charged to the step that
feeds the last sample. That step flushes the machine and assembles the
`DetectionResult`, and in the assembly
`Detection.supportFor` (`detect/src/main/scala/eyes4s/detect/DetectionResult.scala`)
finds each event's sample range by filtering every sample of the recording:

Historical implementation excerpt (not current executable guidance):

```text
indices = (0 until recording.size).filter(index =>
  event.span.contains(recording.samples(index).t)
)
```

That is O(events × samples). With one fixation and one saccade per 270 ms cycle,
the synthetic recordings have about 440 events against 60,000 samples, and about
4,400 against 600,000. Samples are time-ordered, so a binary search for each
event's first and last sample makes it O(events × log samples). That shortens
`RecordingPlan.run` itself, not only the time to cancel. The whole steps are linear
and within budget at 600,000 samples: synchronization 6–22 ms, warping 22–43 ms,
assignment 21–30 ms. By linear projection the first of them passes 100 ms at about
1.4 million samples (about 23 minutes at 1000 Hz). A follow-up ticket should fix
`supportFor` first, then decide whether assembly and the whole steps need their
own chunking for long recordings.

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

## Appendix: per-stage step durations

Pure step durations from the same run, in milliseconds. p95 is a histogram upper
bound; 0.000 means under a microsecond.

| workload | quanta | stage | steps | max | p95 | mean |
|---|---|---|---:|---:|---:|---:|
| study-fixture | default | estimating | 24 | 0.044 | 0.028 | 0.016 |
| study-fixture | default | comparing | 80 | 0.068 | 0.033 | 0.005 |
| study-fixture | default | reducing | 4 | 0.112 | 0.112 | 0.072 |
| study-fixture | default | contrasting | 2 | 0.788 | 0.788 | 0.416 |
| study-fixture | smallest | estimating | 24 | 0.028 | 0.028 | 0.013 |
| study-fixture | smallest | comparing | 280 | 0.259 | 0.004 | 0.004 |
| study-fixture | smallest | reducing | 60 | 0.090 | 0.011 | 0.006 |
| study-fixture | smallest | contrasting | 12 | 0.077 | 0.077 | 0.015 |
| temporal-fixture | default | preparing | 144 | 0.117 | 0.056 | 0.016 |
| temporal-fixture | default | estimating | 288 | 0.103 | 0.011 | 0.005 |
| temporal-fixture | default | comparing | 568 | 0.103 | 0.023 | 0.004 |
| temporal-fixture | default | reducing | 32 | 0.129 | 0.116 | 0.040 |
| temporal-fixture | default | contrasting | 16 | 0.485 | 0.485 | 0.068 |
| temporal-fixture | smallest | preparing | 144 | 0.093 | 0.038 | 0.010 |
| temporal-fixture | smallest | estimating | 288 | 0.056 | 0.008 | 0.004 |
| temporal-fixture | smallest | comparing | 1952 | 0.032 | 0.002 | 0.001 |
| temporal-fixture | smallest | reducing | 480 | 0.043 | 0.005 | 0.002 |
| temporal-fixture | smallest | contrasting | 96 | 0.032 | 0.020 | 0.004 |
| recording-fixture | default | synchronizing | 1 | 0.145 | 0.145 | 0.145 |
| recording-fixture | default | warping | 1 | 0.186 | 0.186 | 0.186 |
| recording-fixture | default | interpolating | 1 | 0.154 | 0.154 | 0.154 |
| recording-fixture | default | detecting | 1 | 0.854 | 0.854 | 0.854 |
| recording-fixture | default | assigning | 1 | 0.188 | 0.188 | 0.188 |
| recording-fixture | smallest | synchronizing | 1 | 0.078 | 0.078 | 0.078 |
| recording-fixture | smallest | warping | 1 | 0.068 | 0.068 | 0.068 |
| recording-fixture | smallest | interpolating | 40 | 0.353 | 0.068 | 0.020 |
| recording-fixture | smallest | detecting | 40 | 0.432 | 0.056 | 0.023 |
| recording-fixture | smallest | assigning | 1 | 0.197 | 0.197 | 0.197 |
| recording-60s | default | synchronizing | 1 | 2.903 | 2.903 | 2.903 |
| recording-60s | default | warping | 1 | 3.785 | 3.785 | 3.785 |
| recording-60s | default | interpolating | 15 | 4.213 | 4.213 | 1.162 |
| recording-60s | default | detecting | 15 | 501.569 | 501.569 | 34.471 |
| recording-60s | default | assigning | 1 | 11.818 | 11.818 | 11.818 |
| recording-60s | smallest | synchronizing | 1 | 0.647 | 0.647 | 0.647 |
| recording-60s | smallest | warping | 1 | 2.046 | 2.046 | 2.046 |
| recording-60s | smallest | interpolating | 60000 | 1.180 | 0.000 | 0.000 |
| recording-60s | smallest | detecting | 60000 | 368.077 | 0.001 | 0.006 |
| recording-60s | smallest | assigning | 1 | 1.896 | 1.896 | 1.896 |
| study-100x256-binned | default | estimating | 100 | 3.843 | 0.740 | 0.684 |
| study-100x256-binned | default | comparing | 1004 | 0.161 | 0.068 | 0.029 |
| study-100x256-binned | default | reducing | 2 | 0.605 | 0.605 | 0.354 |
| study-100x256-binned | default | contrasting | 1 | 0.070 | 0.070 | 0.070 |
| study-100x256-sigma8 | default | estimating | 100 | 5.495 | 5.473 | 5.165 |
| study-100x256-sigma8 | default | comparing | 1004 | 0.105 | 0.056 | 0.026 |
| study-100x256-sigma8 | default | reducing | 2 | 0.356 | 0.356 | 0.257 |
| study-100x256-sigma8 | default | contrasting | 1 | 0.044 | 0.044 | 0.044 |
| study-100x256-sigma32 | default | estimating | 100 | 18.439 | 17.650 | 17.136 |
| study-100x256-sigma32 | default | comparing | 1004 | 0.088 | 0.056 | 0.026 |
| study-100x256-sigma32 | default | reducing | 2 | 0.242 | 0.242 | 0.161 |
| study-100x256-sigma32 | default | contrasting | 1 | 0.028 | 0.028 | 0.028 |
| study-100x256-binned | smallest | estimating | 100 | 0.403 | 0.393 | 0.360 |
| study-100x256-binned | smallest | comparing | 32769602 | 1.165 | 0.000 | 0.000 |
| study-100x256-binned | smallest | reducing | 600 | 0.153 | 0.001 | 0.001 |
| study-100x256-binned | smallest | contrasting | 50 | 0.032 | 0.002 | 0.001 |
| study-100x256-sigma8 | smallest | estimating | 100 | 5.922 | 5.212 | 4.856 |
| study-100x256-sigma8 | smallest | comparing | 32769602 | 0.944 | 0.000 | 0.000 |
| study-100x256-sigma8 | smallest | reducing | 600 | 0.138 | 0.001 | 0.001 |
| study-100x256-sigma8 | smallest | contrasting | 50 | 0.017 | 0.006 | 0.001 |
| study-100x256-sigma32 | smallest | estimating | 100 | 19.637 | 18.532 | 16.975 |
| study-100x256-sigma32 | smallest | comparing | 32769602 | 1.170 | 0.000 | 0.000 |
| study-100x256-sigma32 | smallest | reducing | 600 | 0.109 | 0.001 | 0.001 |
| study-100x256-sigma32 | smallest | contrasting | 50 | 0.017 | 0.003 | 0.001 |
| study-4x512-sigma32 | default | estimating | 4 | 98.649 | 98.649 | 90.126 |
| study-4x512-sigma32 | default | comparing | 24 | 0.058 | 0.056 | 0.040 |
| study-4x512-sigma32 | default | reducing | 2 | 0.043 | 0.043 | 0.034 |
| study-4x512-sigma32 | default | contrasting | 1 | 0.022 | 0.022 | 0.022 |
| study-4x1024-sigma32 | default | estimating | 4 | 647.303 | 647.303 | 622.746 |
| study-4x1024-sigma32 | default | comparing | 72 | 0.459 | 0.056 | 0.056 |
| study-4x1024-sigma32 | default | reducing | 2 | 0.081 | 0.081 | 0.064 |
| study-4x1024-sigma32 | default | contrasting | 1 | 0.036 | 0.036 | 0.036 |
| recording-600s | default | synchronizing | 1 | 22.304 | 22.304 | 22.304 |
| recording-600s | default | warping | 1 | 43.222 | 43.222 | 43.222 |
| recording-600s | default | interpolating | 147 | 14.418 | 2.063 | 0.834 |
| recording-600s | default | detecting | 147 | 55437.454 | 3.528 | 378.411 |
| recording-600s | default | assigning | 1 | 20.761 | 20.761 | 20.761 |
| recording-600s | smallest | synchronizing | 1 | 6.185 | 6.185 | 6.185 |
| recording-600s | smallest | warping | 1 | 21.936 | 21.936 | 21.936 |
| recording-600s | smallest | interpolating | 600000 | 88.608 | 0.000 | 0.000 |
| recording-600s | smallest | detecting | 600000 | 81190.768 | 0.000 | 0.136 |
| recording-600s | smallest | assigning | 1 | 30.441 | 30.441 | 30.441 |

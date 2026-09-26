# eyes4s-execution-responsiveness-v1
# measured_at_utc: 2026-09-19T13:04:19.842517Z
# profile: full
# source_revision: 131970d+resumable-detection-assembly
# hardware: Apple-M3-Max
# operating_system: Mac OS X 14.3 aarch64
# runtime: OpenJDK 64-Bit Server VM 25.0.1
# jvm_arguments: -Xms2g -Xmx2g -XX:+UseG1GC -XX:ActiveProcessorCount=4
# available_processors: 4
# max_heap_bytes: 2147483648
# io_runtime: cats.effect.unsafe.implicits.global
# budget_ms: 100

| workload | route | quanta | envelope | steps | max step ms (stage) | p95 step ms | runner max gap ms | cancel adversarial ms | cancel max ms | cancel median ms | wall s | 100 ms |
|---|---|---|---|---:|---|---:|---:|---:|---:|---:|---:|---|
| study-fixture | study | default | R-pinned fixture: 24 trials, 2x2 grid, binned + gaussian sigma 1 | 110 | 0.083 (reducing) | 0.053 | 0.127 | 0.147 | 0.147 | 0.059 | 0.00 | meets |
| study-fixture | study | smallest | R-pinned fixture: 24 trials, 2x2 grid, binned + gaussian sigma 1 | 376 | 0.384 (comparing) | 0.021 | 0.146 | 0.097 | 0.097 | 0.047 | 0.00 | meets |
| temporal-fixture | temporal | default | R-pinned fixture: 36 trials, 4 windows x 2 repetitions, 2x2 grid, 2 scales | 1048 | 0.400 (preparing) | 0.033 | 2.341 | 0.130 | 0.200 | 0.071 | 0.02 | meets |
| temporal-fixture | temporal | smallest | R-pinned fixture: 36 trials, 4 windows x 2 repetitions, 2x2 grid, 2 scales | 2960 | 0.057 (reducing) | 0.007 | 0.162 | 0.065 | 0.135 | 0.063 | 0.02 | meets |
| recording-fixture | recording | default | 40 samples at 100 Hz, IVT | 12 | 0.580 (assigning) | 0.580 | 0.735 | 0.335 | 0.335 | 0.205 | 0.00 | meets |
| recording-fixture | recording | smallest | 40 samples at 100 Hz, IVT | 373 | 0.234 (interpolating) | 0.024 | 0.669 | 0.134 | 0.207 | 0.051 | 0.01 | meets |
| recording-60s | recording | default | 60000 samples at 1000 Hz, IVT, one AOI | 136 | 12.835 (assigning) | 1.396 | 11.462 | 19.786 | 19.786 | 0.944 | 0.07 | meets |
| recording-60s | recording | smallest | 60000 samples at 1000 Hz, IVT, one AOI | 523724 | 19.378 (assigning) | 0.000 | 12.425 | 2.406 | 2.406 | 0.043 | 0.30 | meets |
| study-100x256-binned | study | default | 100 trials x 10 fixations, 256x256 grid, binned | 1107 | 0.807 (estimating) | 0.705 | 1.505 | 0.762 | 0.762 | 0.084 | 0.10 | meets |
| study-100x256-sigma8 | study | default | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 8 cells, truncate | 1107 | 5.458 (estimating) | 5.212 | 9.069 | 5.031 | 5.031 | 0.074 | 0.55 | meets |
| study-100x256-sigma32 | study | default | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 32 cells, truncate | 1107 | 19.329 (estimating) | 17.650 | 19.420 | 17.467 | 17.467 | 0.098 | 1.75 | meets |
| study-100x256-binned | study | smallest | 100 trials x 10 fixations, 256x256 grid, binned | 32770352 | 1.017 (comparing) | 0.000 | not run (> 8000000 steps) | 0.060 | 0.124 (first 8000000 steps) | 0.062 | 2.98 | meets |
| study-100x256-sigma8 | study | smallest | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 8 cells, truncate | 32770352 | 6.533 (estimating) | 0.000 | not run (> 8000000 steps) | 4.640 | 4.640 (first 8000000 steps) | 0.098 | 3.41 | meets |
| study-100x256-sigma32 | study | smallest | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 32 cells, truncate | 32770352 | 35.180 (estimating) | 0.000 | not run (> 8000000 steps) | 16.948 | 16.948 (first 8000000 steps) | 0.110 | 5.19 | meets |
| study-4x512-sigma32 | study | default | 4 trials x 10 fixations, 512x512 grid, gaussian sigma 32 cells, truncate | 31 | 96.581 (estimating) | 96.581 | 83.388 | 85.428 | 85.428 | 0.078 | 0.33 | meets |
| study-4x1024-sigma32 | study | default | 4 trials x 10 fixations, 1024x1024 grid, gaussian sigma 32 cells, truncate | 79 | 780.743 (estimating) | 685.390 | 702.891 | 658.544 | 658.544 | 0.116 | 2.70 | MISSES |
| recording-600s | recording | default | 600000 samples at 1000 Hz, IVT, one AOI | 1286 | 63.237 (assembly-Gaps) | 0.777 | 48.047 | 0.116 | 15.311 | 0.223 | 0.49 | meets |
| recording-600s | recording | smallest | 600000 samples at 1000 Hz, IVT, one AOI | 5235966 | 46.100 (interpolating) | 0.000 | 31.641 | 13.007 | 15.710 | 0.089 | 2.57 | meets |

Per stage kind (pure step durations):

| workload | quanta | stage | steps | max ms | p95 ms | mean ms |
|---|---|---|---:|---:|---:|---:|
| study-fixture | default | estimating | 24 | 0.075 | 0.033 | 0.018 |
| study-fixture | default | comparing | 80 | 0.055 | 0.031 | 0.005 |
| study-fixture | default | reducing | 4 | 0.083 | 0.083 | 0.064 |
| study-fixture | default | contrasting | 2 | 0.052 | 0.052 | 0.045 |
| study-fixture | smallest | estimating | 24 | 0.046 | 0.021 | 0.015 |
| study-fixture | smallest | comparing | 280 | 0.384 | 0.010 | 0.006 |
| study-fixture | smallest | reducing | 60 | 0.119 | 0.012 | 0.007 |
| study-fixture | smallest | contrasting | 12 | 0.094 | 0.094 | 0.017 |
| temporal-fixture | default | preparing | 144 | 0.400 | 0.061 | 0.017 |
| temporal-fixture | default | estimating | 288 | 0.081 | 0.021 | 0.006 |
| temporal-fixture | default | comparing | 568 | 0.062 | 0.024 | 0.003 |
| temporal-fixture | default | reducing | 32 | 0.074 | 0.074 | 0.037 |
| temporal-fixture | default | contrasting | 16 | 0.104 | 0.104 | 0.040 |
| temporal-fixture | smallest | preparing | 144 | 0.055 | 0.034 | 0.008 |
| temporal-fixture | smallest | estimating | 288 | 0.024 | 0.009 | 0.005 |
| temporal-fixture | smallest | comparing | 1952 | 0.014 | 0.002 | 0.001 |
| temporal-fixture | smallest | reducing | 480 | 0.057 | 0.007 | 0.002 |
| temporal-fixture | smallest | contrasting | 96 | 0.021 | 0.016 | 0.004 |
| recording-fixture | default | synchronizing | 1 | 0.134 | 0.134 | 0.134 |
| recording-fixture | default | warping | 1 | 0.169 | 0.169 | 0.169 |
| recording-fixture | default | interpolating | 1 | 0.160 | 0.160 | 0.160 |
| recording-fixture | default | detecting | 1 | 0.258 | 0.258 | 0.258 |
| recording-fixture | default | assembly-Emissions | 1 | 0.025 | 0.025 | 0.025 |
| recording-fixture | default | assembly-Support | 1 | 0.038 | 0.038 | 0.038 |
| recording-fixture | default | assembly-EventValidation | 1 | 0.031 | 0.031 | 0.031 |
| recording-fixture | default | assembly-EventSummaries | 1 | 0.215 | 0.215 | 0.215 |
| recording-fixture | default | assembly-SourceIdentity | 1 | 0.230 | 0.230 | 0.230 |
| recording-fixture | default | assembly-Gaps | 1 | 0.061 | 0.061 | 0.061 |
| recording-fixture | default | assembly-LabelsAndReport | 1 | 0.517 | 0.517 | 0.517 |
| recording-fixture | default | assigning | 1 | 0.580 | 0.580 | 0.580 |
| recording-fixture | smallest | synchronizing | 1 | 0.056 | 0.056 | 0.056 |
| recording-fixture | smallest | warping | 1 | 0.041 | 0.041 | 0.041 |
| recording-fixture | smallest | interpolating | 40 | 0.234 | 0.046 | 0.015 |
| recording-fixture | smallest | detecting | 40 | 0.115 | 0.028 | 0.011 |
| recording-fixture | smallest | assembly-Emissions | 4 | 0.071 | 0.071 | 0.031 |
| recording-fixture | smallest | assembly-Support | 3 | 0.057 | 0.057 | 0.031 |
| recording-fixture | smallest | assembly-EventValidation | 9 | 0.039 | 0.039 | 0.009 |
| recording-fixture | smallest | assembly-EventSummaries | 111 | 0.024 | 0.009 | 0.004 |
| recording-fixture | smallest | assembly-SourceIdentity | 80 | 0.093 | 0.007 | 0.006 |
| recording-fixture | smallest | assembly-Gaps | 43 | 0.044 | 0.017 | 0.008 |
| recording-fixture | smallest | assembly-LabelsAndReport | 40 | 0.016 | 0.006 | 0.004 |
| recording-fixture | smallest | assigning | 1 | 0.165 | 0.165 | 0.165 |
| recording-60s | default | synchronizing | 1 | 2.371 | 2.371 | 2.371 |
| recording-60s | default | warping | 1 | 4.015 | 4.015 | 4.015 |
| recording-60s | default | interpolating | 15 | 4.512 | 4.512 | 1.233 |
| recording-60s | default | detecting | 15 | 1.395 | 1.395 | 1.083 |
| recording-60s | default | assembly-Emissions | 1 | 0.195 | 0.195 | 0.195 |
| recording-60s | default | assembly-Support | 1 | 0.710 | 0.710 | 0.710 |
| recording-60s | default | assembly-EventValidation | 1 | 0.236 | 0.236 | 0.236 |
| recording-60s | default | assembly-EventSummaries | 40 | 0.725 | 0.725 | 0.652 |
| recording-60s | default | assembly-SourceIdentity | 30 | 1.206 | 1.206 | 0.712 |
| recording-60s | default | assembly-Gaps | 15 | 1.362 | 1.362 | 0.316 |
| recording-60s | default | assembly-LabelsAndReport | 15 | 0.998 | 0.998 | 0.428 |
| recording-60s | default | assigning | 1 | 12.835 | 12.835 | 12.835 |
| recording-60s | smallest | synchronizing | 1 | 0.519 | 0.519 | 0.519 |
| recording-60s | smallest | warping | 1 | 1.992 | 1.992 | 1.992 |
| recording-60s | smallest | interpolating | 60000 | 1.165 | 0.000 | 0.000 |
| recording-60s | smallest | detecting | 60000 | 0.075 | 0.000 | 0.000 |
| recording-60s | smallest | assembly-Emissions | 477 | 0.011 | 0.001 | 0.000 |
| recording-60s | smallest | assembly-Support | 476 | 0.035 | 0.002 | 0.001 |
| recording-60s | smallest | assembly-EventValidation | 1428 | 0.004 | 0.000 | 0.000 |
| recording-60s | smallest | assembly-EventSummaries | 162104 | 0.054 | 0.000 | 0.000 |
| recording-60s | smallest | assembly-SourceIdentity | 120000 | 1.556 | 0.001 | 0.000 |
| recording-60s | smallest | assembly-Gaps | 59236 | 0.025 | 0.000 | 0.000 |
| recording-60s | smallest | assembly-LabelsAndReport | 60000 | 0.094 | 0.000 | 0.000 |
| recording-60s | smallest | assigning | 1 | 19.378 | 19.378 | 19.378 |
| study-100x256-binned | default | estimating | 100 | 0.807 | 0.740 | 0.685 |
| study-100x256-binned | default | comparing | 1004 | 0.183 | 0.059 | 0.029 |
| study-100x256-binned | default | reducing | 2 | 0.550 | 0.550 | 0.330 |
| study-100x256-binned | default | contrasting | 1 | 0.045 | 0.045 | 0.045 |
| study-100x256-sigma8 | default | estimating | 100 | 5.458 | 5.458 | 5.089 |
| study-100x256-sigma8 | default | comparing | 1004 | 0.112 | 0.059 | 0.026 |
| study-100x256-sigma8 | default | reducing | 2 | 0.351 | 0.351 | 0.232 |
| study-100x256-sigma8 | default | contrasting | 1 | 0.038 | 0.038 | 0.038 |
| study-100x256-sigma32 | default | estimating | 100 | 19.329 | 17.650 | 17.020 |
| study-100x256-sigma32 | default | comparing | 1004 | 0.076 | 0.061 | 0.027 |
| study-100x256-sigma32 | default | reducing | 2 | 0.274 | 0.274 | 0.173 |
| study-100x256-sigma32 | default | contrasting | 1 | 0.034 | 0.034 | 0.034 |
| study-100x256-binned | smallest | estimating | 100 | 0.401 | 0.393 | 0.361 |
| study-100x256-binned | smallest | comparing | 32769602 | 1.017 | 0.000 | 0.000 |
| study-100x256-binned | smallest | reducing | 600 | 0.121 | 0.000 | 0.001 |
| study-100x256-binned | smallest | contrasting | 50 | 0.026 | 0.005 | 0.001 |
| study-100x256-sigma8 | smallest | estimating | 100 | 6.533 | 5.212 | 4.821 |
| study-100x256-sigma8 | smallest | comparing | 32769602 | 1.311 | 0.000 | 0.000 |
| study-100x256-sigma8 | smallest | reducing | 600 | 0.157 | 0.000 | 0.001 |
| study-100x256-sigma8 | smallest | contrasting | 50 | 0.034 | 0.003 | 0.002 |
| study-100x256-sigma32 | smallest | estimating | 100 | 35.180 | 22.526 | 18.170 |
| study-100x256-sigma32 | smallest | comparing | 32769602 | 4.171 | 0.000 | 0.000 |
| study-100x256-sigma32 | smallest | reducing | 600 | 0.440 | 0.000 | 0.001 |
| study-100x256-sigma32 | smallest | contrasting | 50 | 0.091 | 0.001 | 0.004 |
| study-4x512-sigma32 | default | estimating | 4 | 96.581 | 96.581 | 94.398 |
| study-4x512-sigma32 | default | comparing | 24 | 0.101 | 0.095 | 0.045 |
| study-4x512-sigma32 | default | reducing | 2 | 0.108 | 0.108 | 0.081 |
| study-4x512-sigma32 | default | contrasting | 1 | 0.027 | 0.027 | 0.027 |
| study-4x1024-sigma32 | default | estimating | 4 | 780.743 | 780.743 | 710.489 |
| study-4x1024-sigma32 | default | comparing | 72 | 0.171 | 0.059 | 0.050 |
| study-4x1024-sigma32 | default | reducing | 2 | 0.054 | 0.054 | 0.049 |
| study-4x1024-sigma32 | default | contrasting | 1 | 0.023 | 0.023 | 0.023 |
| recording-600s | default | synchronizing | 1 | 4.280 | 4.280 | 4.280 |
| recording-600s | default | warping | 1 | 20.937 | 20.937 | 20.937 |
| recording-600s | default | interpolating | 147 | 11.661 | 0.189 | 0.217 |
| recording-600s | default | detecting | 147 | 0.654 | 0.609 | 0.535 |
| recording-600s | default | assembly-Emissions | 2 | 0.849 | 0.849 | 0.476 |
| recording-600s | default | assembly-Support | 2 | 2.674 | 2.674 | 1.558 |
| recording-600s | default | assembly-EventValidation | 4 | 0.521 | 0.521 | 0.428 |
| recording-600s | default | assembly-EventSummaries | 396 | 47.072 | 0.241 | 0.331 |
| recording-600s | default | assembly-SourceIdentity | 293 | 1.067 | 0.816 | 0.447 |
| recording-600s | default | assembly-Gaps | 145 | 63.237 | 0.122 | 0.546 |
| recording-600s | default | assembly-LabelsAndReport | 147 | 0.229 | 0.208 | 0.176 |
| recording-600s | default | assigning | 1 | 13.844 | 13.844 | 13.844 |
| recording-600s | smallest | synchronizing | 1 | 4.029 | 4.029 | 4.029 |
| recording-600s | smallest | warping | 1 | 21.107 | 21.107 | 21.107 |
| recording-600s | smallest | interpolating | 600000 | 46.100 | 0.000 | 0.000 |
| recording-600s | smallest | detecting | 600000 | 2.598 | 0.000 | 0.000 |
| recording-600s | smallest | assembly-Emissions | 4763 | 0.019 | 0.000 | 0.000 |
| recording-600s | smallest | assembly-Support | 4762 | 0.027 | 0.001 | 0.001 |
| recording-600s | smallest | assembly-EventValidation | 14286 | 0.714 | 0.000 | 0.000 |
| recording-600s | smallest | assembly-EventSummaries | 1620070 | 0.115 | 0.000 | 0.000 |
| recording-600s | smallest | assembly-SourceIdentity | 1200000 | 1.889 | 0.000 | 0.000 |
| recording-600s | smallest | assembly-Gaps | 592082 | 0.038 | 0.000 | 0.000 |
| recording-600s | smallest | assembly-LabelsAndReport | 600000 | 0.083 | 0.000 | 0.000 |
| recording-600s | smallest | assigning | 1 | 18.482 | 18.482 | 18.482 |

Contract violations: none

## Measured source identity

Base `131970d78fd0a7d18ed72e4b8ccf1d2a5a136279`, plus local assembly changes.
These SHA-256 digests identify the source used for this measurement, before
scalafmt and the page field rename from `samples` to `workUnits`. Those final
changes alter formatting and an accessor name, not the measured algorithm.

```json
{
  "core/src/main/scala/eyes4s/core/AssemblyWork.scala": "b3b78c664a27946222cc382861aa74b6939d2d7af16f120404db551e09f9a648",
  "core/src/main/scala/eyes4s/core/Recording.scala": "f13657d0967bdf3a5ea8f83a27a7feb6e68efb63277216f998cb3451604cdb40",
  "core/src/main/scala/eyes4s/core/DetectionSupport.scala": "608532ffacebb8e09d27759094799d51d5d563b58e1c274fb925b9cd617ea9cb",
  "detect/src/main/scala/eyes4s/detect/DetectionResult.scala": "9bca1b7da904c9f766f65d7921cd754e655541d69007b74f6a3427500a3622e0",
  "plan/src/main/scala/eyes4s/plan/RecordingWork.scala": "9899b721bd933790ff744393d37023838c37a625e33c598aa5dc9be6507107bf",
  "plan/src/main/scala/eyes4s/plan/Segments.scala": "0cd4c386c1ab2e5ec452c2c1b75a53ad8e72c4a7a314e27e45f29db68b5a9965",
  "fs2/src/main/scala/eyes4s/fs2/RecordingExecution.scala": "966f86c56e12507411239852db0e54b6d6bc775479752dd969147a6a534b9de6",
  "fs2/.jvm/src/test/scala/eyes4s/fs2/ExecutionResponsivenessMain.scala": "bd24ba2711c20cd3b810241404e3777faa1c06dff231a1981d1009807a8e8bc0"
}
```

The observation cap increased from 2,000,000 to 8,000,000 steps so the
5,235,966-step 600k recording is observed to completion at quantum one.
The synthetic input, quanta, JVM flags and 100 ms threshold are unchanged.
The 32-million-step study rows still have partial runner observations, explicitly
marked above; they are not a whole-run responsiveness guarantee.

## Final checked source identity

The final formatted source and renamed `workUnits` accessor are identified below.
The duration-accounting mutant was killed by assertion (two oracle comparisons
failed) and the original bytes were restored before all final checks.

```json
{
  "core/src/main/scala/eyes4s/core/AssemblyWork.scala": "2ec466210da2d196930a0207b1c1cf415988f04a1f1e7fb2bf8cde0bb484b5e5",
  "core/src/main/scala/eyes4s/core/Recording.scala": "196d356f3998d2af56e959fcd8c4af932cd60f90425afdd20c72fafe1a939866",
  "core/src/main/scala/eyes4s/core/DetectionSupport.scala": "e627bd41153b2245b3382e28b83d63b8291314c4f4c4c62b4b758cdd5034f862",
  "detect/src/main/scala/eyes4s/detect/DetectionResult.scala": "79be3e61204484b4dac5b25e5df4970d48ebe1d55ef327e56677bb72a24aed25",
  "plan/src/main/scala/eyes4s/plan/RecordingWork.scala": "dd248452e23a238cc4323b6bdf408d9fb811c3636dc73d7f32acd6de703212ee",
  "plan/src/main/scala/eyes4s/plan/Segments.scala": "4f40fde29146454789c2aa2caba4352db2895ef4573ee9e4aee5f6cb1870d41e",
  "fs2/src/main/scala/eyes4s/fs2/RecordingExecution.scala": "966f86c56e12507411239852db0e54b6d6bc775479752dd969147a6a534b9de6",
  "fs2/.jvm/src/test/scala/eyes4s/fs2/ExecutionResponsivenessMain.scala": "88eb1112d5a2cb4ea187afd58ca803370e1bc5a4e2d686e2011abf81a0ea8184"
}
```

Reproduce the measurement:

```sh
sbt -J-Xmx4g \
  "fs2ModuleJVM/Test/runMain eyes4s.fs2.ExecutionResponsivenessMain --profile full --source-revision 131970d+resumable-detection-assembly --hardware Apple-M3-Max --output /tmp/eyes4s-assembly-responsiveness.md"
```

Final local verification passed: 3,622 tests across 24 JVM/JavaScript module
totals, plus `headerCheckAll`, `scalafmtCheckAll`, `scalafmtSbtCheck`,
`githubWorkflowCheck` and `checkBoundaries`, using `sbt -J-Xmx4g`.
The deliberate duration mutant failed assertions and was restored before this run.

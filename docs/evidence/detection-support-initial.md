# eyes4s-execution-responsiveness-v1
# measured_at_utc: 2026-09-19T12:25:48.018103Z
# profile: full
# source_revision: 131970d+support-search
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
| study-fixture | study | default | R-pinned fixture: 24 trials, 2x2 grid, binned + gaussian sigma 1 | 110 | 0.183 (contrasting) | 0.091 | 0.272 | 0.143 | 0.143 | 0.069 | 0.00 | meets |
| study-fixture | study | smallest | R-pinned fixture: 24 trials, 2x2 grid, binned + gaussian sigma 1 | 376 | 0.278 (comparing) | 0.024 | 0.224 | 0.083 | 0.091 | 0.078 | 0.01 | meets |
| temporal-fixture | temporal | default | R-pinned fixture: 36 trials, 4 windows x 2 repetitions, 2x2 grid, 2 scales | 1048 | 0.151 (preparing) | 0.044 | 2.368 | 0.230 | 0.230 | 0.078 | 0.03 | meets |
| temporal-fixture | temporal | smallest | R-pinned fixture: 36 trials, 4 windows x 2 repetitions, 2x2 grid, 2 scales | 2960 | 0.076 (preparing) | 0.009 | 0.159 | 0.086 | 0.089 | 0.079 | 0.02 | meets |
| recording-fixture | recording | default | 40 samples at 100 Hz, IVT | 5 | 1.359 (detecting) | 1.359 | 1.030 | 1.018 | 1.018 | 0.196 | 0.00 | meets |
| recording-fixture | recording | smallest | 40 samples at 100 Hz, IVT | 83 | 0.510 (detecting) | 0.148 | 0.447 | 0.386 | 0.386 | 0.091 | 0.00 | meets |
| recording-60s | recording | default | 60000 samples at 1000 Hz, IVT, one AOI | 33 | 106.416 (detecting) | 21.453 | 55.723 | 61.144 | 61.144 | 1.500 | 0.11 | MISSES |
| recording-60s | recording | smallest | 60000 samples at 1000 Hz, IVT, one AOI | 120003 | 82.422 (detecting) | 0.001 | 103.773 | 55.121 | 55.121 | 0.076 | 0.31 | MISSES |
| study-100x256-binned | study | default | 100 trials x 10 fixations, 256x256 grid, binned | 1107 | 10.567 (estimating) | 0.945 | 1.553 | 1.032 | 1.032 | 0.096 | 0.13 | meets |
| study-100x256-sigma8 | study | default | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 8 cells, truncate | 1107 | 11.506 (estimating) | 7.334 | 13.916 | 7.607 | 7.607 | 0.116 | 0.78 | meets |
| study-100x256-sigma32 | study | default | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 32 cells, truncate | 1107 | 30.186 (estimating) | 24.835 | 29.494 | 24.658 | 24.658 | 0.094 | 2.54 | meets |
| study-100x256-binned | study | smallest | 100 trials x 10 fixations, 256x256 grid, binned | 32770352 | 4.061 (comparing) | 0.000 | not run (> 2000000 steps) | n/a | 0.187 (first 2000000 steps) | 0.079 | 4.43 | meets |
| study-100x256-sigma8 | study | smallest | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 8 cells, truncate | 32770352 | 37.454 (comparing) | 0.000 | not run (> 2000000 steps) | n/a | 0.410 (first 2000000 steps) | 0.106 | 5.60 | meets |
| study-100x256-sigma32 | study | smallest | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 32 cells, truncate | 32770352 | 33.390 (estimating) | 0.000 | not run (> 2000000 steps) | 24.229 | 24.229 (first 2000000 steps) | 0.196 | 6.58 | meets |
| study-4x512-sigma32 | study | default | 4 trials x 10 fixations, 512x512 grid, gaussian sigma 32 cells, truncate | 31 | 122.534 (estimating) | 122.534 | 128.476 | 116.678 | 116.678 | 0.107 | 0.51 | MISSES |
| study-4x1024-sigma32 | study | default | 4 trials x 10 fixations, 1024x1024 grid, gaussian sigma 32 cells, truncate | 79 | 1008.039 (estimating) | 918.488 | 917.092 | 875.425 | 875.425 | 0.143 | 3.58 | MISSES |
| recording-600s | recording | default | 600000 samples at 1000 Hz, IVT, one AOI | 297 | 610.573 (detecting) | 1.149 | 566.091 | 570.425 | 570.425 | 0.749 | 1.01 | MISSES |
| recording-600s | recording | smallest | 600000 samples at 1000 Hz, IVT, one AOI | 1200003 | 541.560 (detecting) | 0.000 | 554.766 | 518.008 | 518.008 | 0.092 | 1.78 | MISSES |

Per stage kind (pure step durations):

| workload | quanta | stage | steps | max ms | p95 ms | mean ms |
|---|---|---|---:|---:|---:|---:|
| study-fixture | default | estimating | 24 | 0.074 | 0.042 | 0.024 |
| study-fixture | default | comparing | 80 | 0.105 | 0.046 | 0.009 |
| study-fixture | default | reducing | 4 | 0.158 | 0.158 | 0.103 |
| study-fixture | default | contrasting | 2 | 0.183 | 0.183 | 0.119 |
| study-fixture | smallest | estimating | 24 | 0.052 | 0.031 | 0.020 |
| study-fixture | smallest | comparing | 280 | 0.278 | 0.009 | 0.005 |
| study-fixture | smallest | reducing | 60 | 0.111 | 0.027 | 0.009 |
| study-fixture | smallest | contrasting | 12 | 0.201 | 0.201 | 0.029 |
| temporal-fixture | default | preparing | 144 | 0.151 | 0.078 | 0.021 |
| temporal-fixture | default | estimating | 288 | 0.088 | 0.014 | 0.007 |
| temporal-fixture | default | comparing | 568 | 0.076 | 0.034 | 0.004 |
| temporal-fixture | default | reducing | 32 | 0.104 | 0.082 | 0.047 |
| temporal-fixture | default | contrasting | 16 | 0.111 | 0.111 | 0.051 |
| temporal-fixture | smallest | preparing | 144 | 0.076 | 0.044 | 0.009 |
| temporal-fixture | smallest | estimating | 288 | 0.030 | 0.011 | 0.006 |
| temporal-fixture | smallest | comparing | 1952 | 0.031 | 0.002 | 0.001 |
| temporal-fixture | smallest | reducing | 480 | 0.057 | 0.006 | 0.002 |
| temporal-fixture | smallest | contrasting | 96 | 0.069 | 0.023 | 0.006 |
| recording-fixture | default | synchronizing | 1 | 0.143 | 0.143 | 0.143 |
| recording-fixture | default | warping | 1 | 0.198 | 0.198 | 0.198 |
| recording-fixture | default | interpolating | 1 | 0.394 | 0.394 | 0.394 |
| recording-fixture | default | detecting | 1 | 1.359 | 1.359 | 1.359 |
| recording-fixture | default | assigning | 1 | 0.345 | 0.345 | 0.345 |
| recording-fixture | smallest | synchronizing | 1 | 0.069 | 0.069 | 0.069 |
| recording-fixture | smallest | warping | 1 | 0.048 | 0.048 | 0.048 |
| recording-fixture | smallest | interpolating | 40 | 0.347 | 0.078 | 0.025 |
| recording-fixture | smallest | detecting | 40 | 0.510 | 0.059 | 0.029 |
| recording-fixture | smallest | assigning | 1 | 0.231 | 0.231 | 0.231 |
| recording-60s | default | synchronizing | 1 | 4.810 | 4.810 | 4.810 |
| recording-60s | default | warping | 1 | 13.241 | 13.241 | 13.241 |
| recording-60s | default | interpolating | 15 | 6.582 | 6.582 | 1.581 |
| recording-60s | default | detecting | 15 | 106.416 | 106.416 | 8.550 |
| recording-60s | default | assigning | 1 | 20.527 | 20.527 | 20.527 |
| recording-60s | smallest | synchronizing | 1 | 0.820 | 0.820 | 0.820 |
| recording-60s | smallest | warping | 1 | 2.775 | 2.775 | 2.775 |
| recording-60s | smallest | interpolating | 60000 | 5.014 | 0.001 | 0.001 |
| recording-60s | smallest | detecting | 60000 | 82.422 | 0.000 | 0.002 |
| recording-60s | smallest | assigning | 1 | 27.674 | 27.674 | 27.674 |
| study-100x256-binned | default | estimating | 100 | 10.567 | 1.782 | 1.142 |
| study-100x256-binned | default | comparing | 1004 | 0.807 | 0.128 | 0.044 |
| study-100x256-binned | default | reducing | 2 | 0.710 | 0.710 | 0.427 |
| study-100x256-binned | default | contrasting | 1 | 0.071 | 0.071 | 0.071 |
| study-100x256-sigma8 | default | estimating | 100 | 11.506 | 7.701 | 7.229 |
| study-100x256-sigma8 | default | comparing | 1004 | 0.145 | 0.082 | 0.037 |
| study-100x256-sigma8 | default | reducing | 2 | 0.462 | 0.462 | 0.327 |
| study-100x256-sigma8 | default | contrasting | 1 | 0.044 | 0.044 | 0.044 |
| study-100x256-sigma32 | default | estimating | 100 | 30.186 | 26.077 | 24.260 |
| study-100x256-sigma32 | default | comparing | 1004 | 0.548 | 0.078 | 0.038 |
| study-100x256-sigma32 | default | reducing | 2 | 0.604 | 0.604 | 0.409 |
| study-100x256-sigma32 | default | contrasting | 1 | 0.067 | 0.067 | 0.067 |
| study-100x256-binned | smallest | estimating | 100 | 0.618 | 0.552 | 0.497 |
| study-100x256-binned | smallest | comparing | 32769602 | 4.061 | 0.000 | 0.000 |
| study-100x256-binned | smallest | reducing | 600 | 0.811 | 0.001 | 0.002 |
| study-100x256-binned | smallest | contrasting | 50 | 0.160 | 0.003 | 0.005 |
| study-100x256-sigma8 | smallest | estimating | 100 | 9.285 | 7.334 | 6.827 |
| study-100x256-sigma8 | smallest | comparing | 32769602 | 37.454 | 0.000 | 0.000 |
| study-100x256-sigma8 | smallest | reducing | 600 | 1.517 | 0.001 | 0.004 |
| study-100x256-sigma8 | smallest | contrasting | 50 | 0.162 | 0.002 | 0.007 |
| study-100x256-sigma32 | smallest | estimating | 100 | 33.390 | 26.077 | 24.857 |
| study-100x256-sigma32 | smallest | comparing | 32769602 | 1.686 | 0.000 | 0.000 |
| study-100x256-sigma32 | smallest | reducing | 600 | 0.145 | 0.001 | 0.001 |
| study-100x256-sigma32 | smallest | contrasting | 50 | 0.018 | 0.001 | 0.001 |
| study-4x512-sigma32 | default | estimating | 4 | 122.534 | 122.534 | 118.020 |
| study-4x512-sigma32 | default | comparing | 24 | 0.161 | 0.155 | 0.068 |
| study-4x512-sigma32 | default | reducing | 2 | 0.089 | 0.089 | 0.071 |
| study-4x512-sigma32 | default | contrasting | 1 | 0.029 | 0.029 | 0.029 |
| study-4x1024-sigma32 | default | estimating | 4 | 1008.039 | 1008.039 | 946.821 |
| study-4x1024-sigma32 | default | comparing | 72 | 0.227 | 0.128 | 0.074 |
| study-4x1024-sigma32 | default | reducing | 2 | 0.100 | 0.100 | 0.086 |
| study-4x1024-sigma32 | default | contrasting | 1 | 0.031 | 0.031 | 0.031 |
| recording-600s | default | synchronizing | 1 | 7.185 | 7.185 | 7.185 |
| recording-600s | default | warping | 1 | 88.721 | 88.721 | 88.721 |
| recording-600s | default | interpolating | 147 | 18.598 | 0.477 | 0.454 |
| recording-600s | default | detecting | 147 | 610.573 | 1.149 | 4.933 |
| recording-600s | default | assigning | 1 | 33.310 | 33.310 | 33.310 |
| recording-600s | smallest | synchronizing | 1 | 6.881 | 6.881 | 6.881 |
| recording-600s | smallest | warping | 1 | 28.839 | 28.839 | 28.839 |
| recording-600s | smallest | interpolating | 600000 | 75.520 | 0.000 | 0.000 |
| recording-600s | smallest | detecting | 600000 | 541.560 | 0.000 | 0.001 |
| recording-600s | smallest | assigning | 1 | 36.519 | 36.519 | 36.519 |

Contract violations: none

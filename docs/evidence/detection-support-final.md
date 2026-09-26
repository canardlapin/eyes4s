# eyes4s-execution-responsiveness-v1
# measured_at_utc: 2026-09-19T12:30:58.485600Z
# profile: full
# source_revision: 131970d+support-search-memoized-hash
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
| study-fixture | study | default | R-pinned fixture: 24 trials, 2x2 grid, binned + gaussian sigma 1 | 110 | 0.169 (reducing) | 0.065 | 0.218 | 0.194 | 0.194 | 0.060 | 0.00 | meets |
| study-fixture | study | smallest | R-pinned fixture: 24 trials, 2x2 grid, binned + gaussian sigma 1 | 376 | 0.312 (reducing) | 0.033 | 0.166 | 0.125 | 0.125 | 0.074 | 0.01 | meets |
| temporal-fixture | temporal | default | R-pinned fixture: 36 trials, 4 windows x 2 repetitions, 2x2 grid, 2 scales | 1048 | 0.103 (preparing) | 0.040 | 1.985 | 0.266 | 0.266 | 0.087 | 0.02 | meets |
| temporal-fixture | temporal | smallest | R-pinned fixture: 36 trials, 4 windows x 2 repetitions, 2x2 grid, 2 scales | 2960 | 0.075 (contrasting) | 0.009 | 0.317 | 0.114 | 0.114 | 0.088 | 0.03 | meets |
| recording-fixture | recording | default | 40 samples at 100 Hz, IVT | 5 | 1.183 (detecting) | 1.183 | 0.936 | 0.780 | 0.780 | 0.157 | 0.00 | meets |
| recording-fixture | recording | smallest | 40 samples at 100 Hz, IVT | 83 | 0.443 (detecting) | 0.141 | 0.368 | 0.346 | 0.346 | 0.090 | 0.00 | meets |
| recording-60s | recording | default | 60000 samples at 1000 Hz, IVT, one AOI | 33 | 45.119 (detecting) | 16.009 | 43.588 | 54.720 | 54.720 | 0.879 | 0.09 | meets |
| recording-60s | recording | smallest | 60000 samples at 1000 Hz, IVT, one AOI | 120003 | 46.985 (detecting) | 0.002 | 60.254 | 35.895 | 35.895 | 0.168 | 0.25 | meets |
| study-100x256-binned | study | default | 100 trials x 10 fixations, 256x256 grid, binned | 1107 | 1.600 (estimating) | 0.900 | 5.635 | 0.907 | 0.907 | 0.081 | 0.14 | meets |
| study-100x256-sigma8 | study | default | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 8 cells, truncate | 1107 | 9.899 (estimating) | 7.334 | 13.043 | 6.828 | 6.828 | 0.144 | 0.76 | meets |
| study-100x256-sigma32 | study | default | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 32 cells, truncate | 1107 | 29.738 (estimating) | 23.652 | 26.783 | 23.617 | 23.617 | 0.085 | 2.41 | meets |
| study-100x256-binned | study | smallest | 100 trials x 10 fixations, 256x256 grid, binned | 32770352 | 1.942 (comparing) | 0.000 | not run (> 2000000 steps) | 0.055 | 0.105 (first 2000000 steps) | 0.087 | 4.27 | meets |
| study-100x256-sigma8 | study | smallest | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 8 cells, truncate | 32770352 | 7.842 (estimating) | 0.000 | not run (> 2000000 steps) | 6.854 | 6.854 (first 2000000 steps) | 0.096 | 4.78 | meets |
| study-100x256-sigma32 | study | smallest | 100 trials x 10 fixations, 256x256 grid, gaussian sigma 32 cells, truncate | 32770352 | 31.044 (estimating) | 0.000 | not run (> 2000000 steps) | 23.245 | 23.245 (first 2000000 steps) | 0.203 | 6.37 | meets |
| study-4x512-sigma32 | study | default | 4 trials x 10 fixations, 512x512 grid, gaussian sigma 32 cells, truncate | 31 | 132.062 (estimating) | 124.255 | 162.061 | 130.149 | 130.149 | 0.124 | 0.52 | MISSES |
| study-4x1024-sigma32 | study | default | 4 trials x 10 fixations, 1024x1024 grid, gaussian sigma 32 cells, truncate | 79 | 919.472 (estimating) | 918.488 | 937.728 | 828.385 | 828.385 | 0.121 | 3.56 | MISSES |
| recording-600s | recording | default | 600000 samples at 1000 Hz, IVT, one AOI | 297 | 409.852 (detecting) | 1.964 | 403.803 | 361.905 | 361.905 | 0.684 | 0.69 | MISSES |
| recording-600s | recording | smallest | 600000 samples at 1000 Hz, IVT, one AOI | 1200003 | 331.708 (detecting) | 0.000 | 401.948 | 319.441 | 319.441 | 0.111 | 1.43 | MISSES |

Per stage kind (pure step durations):

| workload | quanta | stage | steps | max ms | p95 ms | mean ms |
|---|---|---|---:|---:|---:|---:|
| study-fixture | default | estimating | 24 | 0.041 | 0.040 | 0.022 |
| study-fixture | default | comparing | 80 | 0.085 | 0.038 | 0.007 |
| study-fixture | default | reducing | 4 | 0.169 | 0.169 | 0.089 |
| study-fixture | default | contrasting | 2 | 0.071 | 0.071 | 0.058 |
| study-fixture | smallest | estimating | 24 | 0.083 | 0.038 | 0.022 |
| study-fixture | smallest | comparing | 280 | 0.266 | 0.013 | 0.006 |
| study-fixture | smallest | reducing | 60 | 0.312 | 0.068 | 0.014 |
| study-fixture | smallest | contrasting | 12 | 0.154 | 0.154 | 0.028 |
| temporal-fixture | default | preparing | 144 | 0.103 | 0.075 | 0.019 |
| temporal-fixture | default | estimating | 288 | 0.067 | 0.013 | 0.007 |
| temporal-fixture | default | comparing | 568 | 0.053 | 0.033 | 0.004 |
| temporal-fixture | default | reducing | 32 | 0.082 | 0.082 | 0.043 |
| temporal-fixture | default | contrasting | 16 | 0.073 | 0.073 | 0.040 |
| temporal-fixture | smallest | preparing | 144 | 0.065 | 0.044 | 0.009 |
| temporal-fixture | smallest | estimating | 288 | 0.019 | 0.012 | 0.006 |
| temporal-fixture | smallest | comparing | 1952 | 0.016 | 0.002 | 0.001 |
| temporal-fixture | smallest | reducing | 480 | 0.070 | 0.008 | 0.003 |
| temporal-fixture | smallest | contrasting | 96 | 0.075 | 0.022 | 0.006 |
| recording-fixture | default | synchronizing | 1 | 0.183 | 0.183 | 0.183 |
| recording-fixture | default | warping | 1 | 0.200 | 0.200 | 0.200 |
| recording-fixture | default | interpolating | 1 | 0.224 | 0.224 | 0.224 |
| recording-fixture | default | detecting | 1 | 1.183 | 1.183 | 1.183 |
| recording-fixture | default | assigning | 1 | 0.328 | 0.328 | 0.328 |
| recording-fixture | smallest | synchronizing | 1 | 0.068 | 0.068 | 0.068 |
| recording-fixture | smallest | warping | 1 | 0.049 | 0.049 | 0.049 |
| recording-fixture | smallest | interpolating | 40 | 0.315 | 0.059 | 0.019 |
| recording-fixture | smallest | detecting | 40 | 0.443 | 0.044 | 0.026 |
| recording-fixture | smallest | assigning | 1 | 0.187 | 0.187 | 0.187 |
| recording-60s | default | synchronizing | 1 | 5.000 | 5.000 | 5.000 |
| recording-60s | default | warping | 1 | 7.999 | 7.999 | 7.999 |
| recording-60s | default | interpolating | 15 | 5.011 | 5.011 | 1.430 |
| recording-60s | default | detecting | 15 | 45.119 | 45.119 | 4.230 |
| recording-60s | default | assigning | 1 | 15.955 | 15.955 | 15.955 |
| recording-60s | smallest | synchronizing | 1 | 1.268 | 1.268 | 1.268 |
| recording-60s | smallest | warping | 1 | 4.452 | 4.452 | 4.452 |
| recording-60s | smallest | interpolating | 60000 | 3.824 | 0.002 | 0.000 |
| recording-60s | smallest | detecting | 60000 | 46.985 | 0.002 | 0.001 |
| recording-60s | smallest | assigning | 1 | 27.754 | 27.754 | 27.754 |
| study-100x256-binned | default | estimating | 100 | 1.600 | 1.042 | 0.888 |
| study-100x256-binned | default | comparing | 1004 | 0.221 | 0.141 | 0.042 |
| study-100x256-binned | default | reducing | 2 | 0.807 | 0.807 | 0.474 |
| study-100x256-binned | default | contrasting | 1 | 0.075 | 0.075 | 0.075 |
| study-100x256-sigma8 | default | estimating | 100 | 9.899 | 7.701 | 7.159 |
| study-100x256-sigma8 | default | comparing | 1004 | 0.108 | 0.075 | 0.036 |
| study-100x256-sigma8 | default | reducing | 2 | 0.515 | 0.515 | 0.330 |
| study-100x256-sigma8 | default | contrasting | 1 | 0.072 | 0.072 | 0.072 |
| study-100x256-sigma32 | default | estimating | 100 | 29.738 | 26.077 | 23.825 |
| study-100x256-sigma32 | default | comparing | 1004 | 0.218 | 0.075 | 0.036 |
| study-100x256-sigma32 | default | reducing | 2 | 0.361 | 0.361 | 0.243 |
| study-100x256-sigma32 | default | contrasting | 1 | 0.056 | 0.056 | 0.056 |
| study-100x256-binned | smallest | estimating | 100 | 0.572 | 0.552 | 0.500 |
| study-100x256-binned | smallest | comparing | 32769602 | 1.942 | 0.000 | 0.000 |
| study-100x256-binned | smallest | reducing | 600 | 0.184 | 0.001 | 0.001 |
| study-100x256-binned | smallest | contrasting | 50 | 0.068 | 0.006 | 0.003 |
| study-100x256-sigma8 | smallest | estimating | 100 | 7.842 | 6.985 | 6.712 |
| study-100x256-sigma8 | smallest | comparing | 32769602 | 3.286 | 0.000 | 0.000 |
| study-100x256-sigma8 | smallest | reducing | 600 | 0.284 | 0.001 | 0.001 |
| study-100x256-sigma8 | smallest | contrasting | 50 | 0.049 | 0.002 | 0.002 |
| study-100x256-sigma32 | smallest | estimating | 100 | 31.044 | 24.835 | 23.514 |
| study-100x256-sigma32 | smallest | comparing | 32769602 | 2.129 | 0.000 | 0.000 |
| study-100x256-sigma32 | smallest | reducing | 600 | 0.336 | 0.001 | 0.001 |
| study-100x256-sigma32 | smallest | contrasting | 50 | 0.087 | 0.003 | 0.003 |
| study-4x512-sigma32 | default | estimating | 4 | 132.062 | 132.062 | 122.039 |
| study-4x512-sigma32 | default | comparing | 24 | 0.086 | 0.071 | 0.054 |
| study-4x512-sigma32 | default | reducing | 2 | 0.049 | 0.049 | 0.045 |
| study-4x512-sigma32 | default | contrasting | 1 | 0.023 | 0.023 | 0.023 |
| study-4x1024-sigma32 | default | estimating | 4 | 919.472 | 919.472 | 898.426 |
| study-4x1024-sigma32 | default | comparing | 72 | 0.154 | 0.082 | 0.068 |
| study-4x1024-sigma32 | default | reducing | 2 | 0.081 | 0.081 | 0.068 |
| study-4x1024-sigma32 | default | contrasting | 1 | 0.035 | 0.035 | 0.035 |
| recording-600s | default | synchronizing | 1 | 7.714 | 7.714 | 7.714 |
| recording-600s | default | warping | 1 | 39.108 | 39.108 | 39.108 |
| recording-600s | default | interpolating | 147 | 24.092 | 0.857 | 0.605 |
| recording-600s | default | detecting | 147 | 409.852 | 2.274 | 4.577 |
| recording-600s | default | assigning | 1 | 24.574 | 24.574 | 24.574 |
| recording-600s | smallest | synchronizing | 1 | 7.035 | 7.035 | 7.035 |
| recording-600s | smallest | warping | 1 | 28.271 | 28.271 | 28.271 |
| recording-600s | smallest | interpolating | 600000 | 61.573 | 0.000 | 0.000 |
| recording-600s | smallest | detecting | 600000 | 331.708 | 0.000 | 0.001 |
| recording-600s | smallest | assigning | 1 | 25.301 | 25.301 | 25.301 |

Contract violations: none

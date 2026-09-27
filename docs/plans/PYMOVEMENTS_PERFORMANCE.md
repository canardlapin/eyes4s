# Frozen performance protocol: eyes4s / pymovements

Protocol: `eyes4s-pymovements-performance-v1`, fixed before timing on 2026-09-27.
Owner: PM3.1 `bd-01M3HYF5VMV818PJMJDKMJBK8P`; gate:
`bd-01M3HYF4J5965YW2DK08YV2A80`.

This is the performance protocol under the
[five-area comparison contract](PYMOVEMENTS_COMPARISON.md). It specifies the
experiment and its acceptance criteria. No comparative timings have been collected
by this slice, and no performance claim follows from a protocol check passing.

The machine-readable authority is
[`performance.json`](../../tools/pymovements/performance.json). It fixes workloads,
scientific contracts, dataset identities, machine/runtime pins, measurement policy,
budgets and next Mote owners. The raw receipt format is
[`receipt.schema.json`](../../tools/pymovements/receipt.schema.json).
The protocol identity is SHA-256 of sorted, compact JSON, with nonfinite numbers
forbidden. The configuration also binds this document's bytes, so its statistical
and scientific rules cannot change without changing the protocol identity.
Receipts bind that identity, clean source commit/tree, compiled artifact,
adapter, dependency lock, configuration, input and output identities. Updates to any
of those fields cannot be silently mixed into an existing experiment.

## Primary targets and limits

The thresholds below are prospective engineering targets, chosen for a meaningful
research benefit before collecting timings. They are not estimates of current
performance. All conditions must pass; averaging cannot excuse a failed cell.

| Quantity | Required result |
|---|---|
| Aggregate warm throughput | At least **1.25×** pymovements; upper one-sided 95% bound on eyes4s/reference time ratio ≤ **0.80** |
| Aggregate large-input process peak RSS | At least **20% lower**; upper 95% bound on eyes4s/reference ratio ≤ **0.80** |
| Every paired workload/scale, warm time | Upper 95% ratio bound ≤ **1.10** |
| Every paired workload/scale, process peak RSS | Upper 95% ratio bound ≤ **1.10** |
| Every paired workload/scale, cold end-to-end time | Upper 95% bound on eyes4s minus reference ≤ **1500 ms** |
| Eyes4s cold startup to ready | Upper 95% bound on the median ≤ **5000 ms**, separate from task execution |
| Studio selection feedback | Actual input-to-display p95 and its upper 95% bound **<100 ms** |
| Studio cached trial switch | Actual input-to-display p95 and its upper 95% bound **<250 ms** |
| Studio perspective switch | Actual input-to-display p95 and its upper 95% bound **<150 ms** |

Absolute warm wall/RSS limits also apply to eyes4s, including workloads with no
valid comparator. Wall limits appear in the table below; exact RSS caps and growth
limits are in JSON. Standard small/typical/large RSS caps are 512/2048/8192 MiB.
Complete study execution uses 1024/4096/8192 MiB. Streaming retains at most 512 MiB
at every scale. Studio is bounded by 3072 MiB for the fixture and 8192 MiB for 10×.
MiB means 2^20 bytes. These are whole-process high-water limits, not heap deltas.

For warm scaling, let `r` be the ratio of median times and `n` the ratio of input
rows between adjacent scales. Require `r^(log(10)/log(n))` ≤ the row's
`max_growth_per_10x`. It is 12 for linear workloads, 48 for density (grid area also
quadruples with each 10× event increase), and 120 for unsimplified MultiMatch.
Absolute limits must pass too. Startup ratios are excluded from scaling claims.

Cold-start costs and CPU/memory losses are reported even when within budget.
A failure or inconclusive bound keeps PM3 open. Changed budgets, removed cells,
different aggregation weights or narrowed output semantics require an explicit
owner decision and a new reviewed protocol before rerunning both implementations.
Preserve prior receipts. Never tune a threshold after seeing a losing result.

## Machine, runtime and dependency pins

The primary machine is `m3-max-36g-macos14.3-v1`: Apple M3 Max, 36 GiB RAM,
macOS 14.3 build 23D56, arm64. CPU, RAM and OS were read from the local machine.
Run on AC, low-power mode off, thermally settled, without competing compute jobs.
Record violations instead of deleting affected measurements. Another machine or
OS is a separate experiment; results are not pooled across machines.

The JVM is Homebrew OpenJDK 25.0.1, Scala 3.7.4, sbt 1.11.7. Use the JSON JVM
flags explicitly: G1, 256 MiB initial heap, 8 GiB maximum heap and one active
processor. The shell's default Java may differ; record the actual benchmark JVM.
The Python runtime is CPython 3.14.7. The comparator retains PM1.1's published
pymovements 0.28.0 wheel and source commit. The full Python dependency graph and
distribution hashes are fixed in
[`performance-requirements.txt`](../../tools/pymovements/performance-requirements.txt),
compiled for macOS arm64 / Python 3.14 from the adjacent `.in` file.
Dependency resolution is not proof that the installed benchmark executes; PM3.2
must validate the environment and output contract before timing.

Set Polars, BLAS, OpenMP and other listed thread variables to one. Use one active
compute worker in each adapter. Record actual flags and the complete package graph;
a mismatched runtime/thread configuration is rejected. A native all-core track may
be added later, but cannot replace the primary single-worker comparison. JVM and
Python are the primary performance targets here; this protocol does not claim
Scala.js performance or replace its correctness/consumer gates.

## Input identity and scale

[`fixtures.py`](../../tools/pymovements/fixtures.py) generates raw bytes using
integer arithmetic and fixed formatting. No private participant data or network
download is needed. Dataset entries pin both byte length and SHA-256. The generator
itself is hashed. Generate into an external cache; do not commit large data files.

- Raw small/typical/large inputs contain 10,000 / 100,000 / 1,000,000 samples.
  Clean 500 Hz steps exercise fixations and transitions. The 1000 Hz binocular
  case includes deterministic coordinate noise, sustained movement, 30-sample gaps,
  zero pupil and explicit validity. Mono inputs do not invent a second eye.
- Stationary I-DT inputs exercise one long candidate. Threshold-churning inputs
  separately expose expanding-window cost and scientific convention differences.
- Reading/AOI inputs contain 1,000 / 10,000 / 100,000 annotated event rows, including
  regressions, skipped/invalid support, and 72 ordered word regions. Detection cost
  is excluded from isolated measure cells and included in complete pipelines.
- The existing golden study fixes source file identities. Study sizes replicate
  participants 1× / 5× / 10× with explicit replica prefixes while preserving item
  identities. Recompute actual core results; mock scores are not results. A recipe
  hash identifies sources plus transformation, and each receipt also records the
  actual materialized bundle hash. Studio uses 1× and 10×, as S10.6 requires.

Synthetic load provides controlled scaling and failure cases. It is not a human
accuracy benchmark or evidence of population-wide speed. The existing public
human-corpus manifest is pinned as contextual provenance; PM5 owns its timing/label
adapters and scientific qualification. Real-data performance extensions retain
their own source/normalized-input identities and cannot replace this mandatory
matrix with easier examples.

## Scientific work and output agreement

Every workload has a contract, complete output column list and units in JSON.
Adapters must use public entry points where available. Record native extra work;
do not move a computation outside the timer merely because one API combines stages.
Represent missing/dropped/unsupported rows as data. Input changes, unit conversions,
silent truncation, changed thresholds and lazy outputs invalidate a paired timing.

For I-VT, fix velocity at 30 deg/s and a 50-sample minimum on the 500 Hz fixture.
Eyes4s computes central-neighbor velocity, inherits the adjacent interior class at
valid segment endpoints, and emits both fixations and saccades. Pymovements consumes
velocity and returns fixations. Its inclusive duration threshold is 98 ms for those
50 samples; eyes4s' half-open threshold is 100 ms. The reference adapter must produce
the same segment/endpoint classification and fixation summaries. Native eyes4s
saccade work stays inside its timed path. Preserve original event boundaries in
native output artifacts and normalize the stop to half-open support for comparison.

I-DT's sum-of-axis ranges, per-axis extent thresholds and violating-sample inclusion
are not interchangeable. Only the stationary subset is paired for equivalent work.
`idt-churn` measures and retains each native result descriptively; its speed ratio
cannot enter the aggregate. A reference timeout is a failed cell, not an infinite
eyes4s speedup. Microsaccades use fixed per-axis thresholds and the same five-point
supported interior; unsupported boundaries/gaps remain in the accounting.

Geometry fixes physical display size, distance, origin and the component-arctangent
formula. Smoothing fixes stencil, polynomial degree, gap handling and supported
interior. The public pymovements smoother lacks a median mode; median and bounded
gap interpolation therefore have absolute eyes4s budgets. A Python package outside
the declared comparator may not quietly become a substitute comparator.

QC fixes the loss denominator and per-eye status. Reading fixes ordered nonoverlapping
word regions, first visits, exits, regressions and invalid-event interruption. AOI
assignment is performed from geometry; the fixture's word index is an independent
expected label, not permission to skip assignment. Density fixes domain, grid,
duration weighting and normalization. MultiMatch fixes path lengths, all components
and no simplification. Study execution includes all actual results and diagnostics.

Materialize outputs before stopping the relevant clock. End-to-end cells include
canonical CSV/bundle construction and flush to the primed filesystem cache (no
`fsync` claim). Isolated kernels fully traverse all result buffers. Compute evidence
hashes after timing, over those completed outputs; do not include a checksum pass
for one side only. Canonical output is UTF-8/LF, stably sorted by scientific identity,
with exact integer times/counts/labels and decimal values rounded to six places.
The rounding quantum is 10^-6 of the named output unit; nonfinite values require
explicit null/status. Schema, units, counts and canonical hash must agree. Retain
native artifacts as well. Bin-boundary differences that survive this normalization
remain blocked for investigation; do not widen precision after observing them.

## Measurement and uncertainty

Use 20 independent process-pair rounds per workload/scale/mode. Alternate execution
order: eyes4s first on even-numbered rounds, pymovements first on odd rounds. Run
sequentially, never simultaneously. A process instance is one experimental unit;
reusing a process and calling it another independent round is rejected.

- **Cold**: fresh process, startup-to-ready recorded separately, then one complete
  task. The total wall clock includes process startup, imports/classloading,
  admission where relevant, computation and complete outputs. Zero warm-ups.
- **Warm**: fresh process for each round, five untimed complete warm-ups, then three
  timed iterations. Use their median as that process's time. Keep their raw values.
  Do not reuse computed results between iterations.
- **Display**: real backend, five untimed interactions, then 100 input-to-presented
  frame observations per fresh process. Record raw latencies and displayed refs.
  Synchronous handler-to-snapshot measurements cannot be relabelled display latency.

Display requests must change state. Continue one sequence across warm-ups and timed
observations: selection alternates `r00/P17 / Encoding / enc_03 / occurrence 1 /
ordinal 6` and ordinal 7 (base fixture records 7214/7215); cached trial switches
alternate that participant's `enc_03` and `enc_04` after priming both; perspectives
cycle Explore → Analysis → Compare. Record requested and presented references in
the raw artifact, verify they agree, and reject no-op requests or unchanged frames.

Prime the OS file cache once before either mode, outside its clocks. “Cold” means
fresh runtime, not uncached disk. Do not purge caches with privileged commands.
Use monotonic nanosecond clocks; report wall time and derive throughput from the
exact input bytes/rows. Stop and retain failed processes, timeouts (300 s) and OOM;
never score them as fast successful work or remove them from required coverage.

Measure whole-process peak RSS with a fresh child's `/usr/bin/time -l` maximum
resident set size, in macOS **bytes**. Include runtime/warm-up overhead; do not
subtract a convenient baseline or use a cumulative maximum across different
children. JVM retained heap after a documented settling GC, total allocation and
GC pause/count counters come from separate diagnostic runs on the same workload
and configuration. Do not force GC or enable asymmetric profiling in primary
timings. Retain each diagnostic receipt hash and instrument name. Python native
allocation and retained heap may be unavailable; use `null` plus a reason, never
zero. Python tracemalloc, JVM heap and process RSS are distinct quantities.

For paired warm time, compute each round's eyes4s/reference ratio from the process
medians. The cell estimate is `exp(median(log(ratio)))`; RSS uses the corresponding
process peak ratio. Aggregate in log space with equal weight per paired workload
group, equal weight per workload within each group, and scale weights 0.2/0.3/0.5.
Large-input RSS uses equal group/within-group weights at the large scale only.
Thus adding extra detector microbenchmarks cannot drown out ingestion or complete
pipelines. Absolute/descriptive cells never enter paired aggregates.

Use 10,000 paired process-round bootstrap resamples, CPython `random.Random(20260927)`
and 20 `randrange(20)` draws per resample. Reuse that index vector across all cells
to preserve round-level covariance; never resample the three warm iterations as
independent subjects. Recompute medians and aggregate for each resample. Use the
nearest-rank 95th percentile as the upper one-sided bound. Cold differences use
the median paired millisecond difference and its upper bound. Absolute wall/RSS
and scaling limits likewise use upper bootstrap bounds. These are conditional
benchmark uncertainty estimates, not hardware/population guarantees.

Studio's point estimate is the nearest-rank p95 of all 2,000 observations per
workload/scale. Bootstrap entire 100-observation process blocks and compute pooled
p95 on each resample. Both point and upper bound must be strictly below S10.6's
budget; keep per-process p95 values visible too.

If `(q95 - q05) / point_ratio > 0.20` for a paired time/RSS ratio, mark it
inconclusive. A documented external interference event permits one whole-suite
repeat; retain and report both runs, never select the faster result. Otherwise
fix the instability or revise the protocol prospectively with an owner decision.
No per-cell “keep running until significant” loop is allowed.

## Required matrix and ownership

For UI rows, the wall column is the strict p95 budget rather than a batch wall cap.
The typical UI scale is intentionally absent; S10.6 requires the fixture and 10×.
All other rows require all three scales, cold and warm modes, and every prescribed
round. Paired/descriptive rows require both implementations; absolute rows require
eyes4s. Missing QC/reading implementations remain required, with their adapter
tickets blocked by the existing feature slices.

<!-- performance-table:start -->

| Workload | Comparison | Warm wall cap, small / typical / large (ms) | Next owner |
|---|---|---|---|
| `ingest-csv` | paired | 1000 / 10000 / 120000 | `bd-01M3HYF63RDKD2VSQ9PFAXEKAN` |
| `ingest-asc` | paired | 1000 / 10000 / 120000 | `bd-01M3J4M6S3A366YNTWWVYB0MBD` |
| `pix2deg` | paired | 1000 / 10000 / 120000 | `bd-01M3J4M81MPP7DWB2AJHTEDGFN` |
| `velocity` | paired | 1000 / 10000 / 120000 | `bd-01M3J4M81MPP7DWB2AJHTEDGFN` |
| `median` | absolute | 1000 / 10000 / 120000 | `bd-01M3J4M99V1VWR17R10G4P27AK` |
| `savitzky-golay` | paired | 1000 / 10000 / 120000 | `bd-01M3J4M99V1VWR17R10G4P27AK` |
| `interpolate` | absolute | 1000 / 10000 / 120000 | `bd-01M3J4M99V1VWR17R10G4P27AK` |
| `ivt` | paired | 1000 / 10000 / 120000 | `bd-01M3HYF63RDKD2VSQ9PFAXEKAN` |
| `idt-stationary` | paired | 1000 / 10000 / 120000 | `bd-01M3J4MAJ232NXG490KAZXW69H` |
| `idt-churn` | descriptive | 1000 / 10000 / 120000 | `bd-01M3J4MAJ232NXG490KAZXW69H` |
| `microsaccades` | paired | 1000 / 10000 / 120000 | `bd-01M3J4MBTQCE63ZWQF9YJP345J` |
| `qc-loss` | paired | 1000 / 10000 / 120000 | `bd-01M3J4MD325MXQF2SPW1BAGKSD` |
| `aoi-dwell` | paired | 1000 / 10000 / 120000 | `bd-01M3J4MEN7PQMTPY89JWDRCNDT` |
| `reading-first-pass` | paired | 1000 / 10000 / 120000 | `bd-01M3J4MEN7PQMTPY89JWDRCNDT` |
| `pipeline-csv-ivt` | paired | 1000 / 10000 / 120000 | `bd-01M3HYF63RDKD2VSQ9PFAXEKAN` |
| `pipeline-asc-reading` | paired | 1000 / 10000 / 120000 | `bd-01M3J4MG7X41XQBSEVHMZYE61S` |
| `density` | absolute | 1000 / 10000 / 120000 | `bd-01M3J4MJ4HATT8SN34TG59EYQQ` |
| `multimatch` | absolute | 1000 / 10000 / 120000 | `bd-01M3J4MKCXZDQCKYWZ0Q9P52KE` |
| `study` | absolute | 2000 / 15000 / 40000 | `bd-01M3J4MMMGD2HZV8XBMWMH3X4C` |
| `streaming-asc` | absolute | 1000 / 10000 / 120000 | `bd-01M3J4M6S3A366YNTWWVYB0MBD` |
| `ui-selection` | absolute | 100 / — / 100 | `bd-01M3DPG25V0DGFR2BPANBDWMQF` |
| `ui-trial` | absolute | 250 / — / 250 | `bd-01M3DPG25V0DGFR2BPANBDWMQF` |
| `ui-perspective` | absolute | 150 / — / 150 | `bd-01M3DPG25V0DGFR2BPANBDWMQF` |

<!-- performance-table:end -->

## Checks and next executable slice

Run the commands in the [tooling README](../../tools/pymovements/README.md).
The checker rejects missing workloads/scales, changed pins/budgets, incomplete
pairings, bad units, differing/empty outputs, reused process units, unavailable
mandatory JVM counters, dirty source and failed processes. Requested complete
coverage cannot be satisfied by one successful pipeline. The JSON Schema follows
[jsonschema's documented validation interface](https://python-jsonschema.readthedocs.io/en/stable/validate/).

Passing receipt validation establishes structural completeness and declared output
comparability, **not** timing authenticity, execution correctness, threshold
success or scientific superiority. The validator does not execute commands or
recompute result artifacts. Retain raw native/canonical outputs, process logs and
diagnostic files in an evidence bundle addressed by their recorded hashes. Review
and the next collector/reducer must verify those artifacts and apply the exact
statistics above before PM3 can pass. Test receipts are synthetic counterexamples,
not benchmark results.

PM3.2 `bd-01M3HYF63RDKD2VSQ9PFAXEKAN` implements the first executable paired
CSV-to-I-VT baseline using the public paths identified above. It must first prove
the installed environment, fixture admission, endpoint/duration adapter and full
output equality, then run this frozen collection/reduction protocol. This slice
does not pretend that a paired benchmark runner already exists. The remaining
bounded adapter tickets reuse that collector; PM3.3 selects one bottleneck only
after profiling this baseline. Full Scala landing gates remain separate from
protocol-tooling validation.

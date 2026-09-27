# PM3.2 CSV-to-I-VT paired baseline

This slice implements the first end-to-end workload in the frozen
[performance protocol](PYMOVEMENTS_PERFORMANCE.md), `pipeline-csv-ivt`.
It does **not** yet contain a protocol-qualified 20-round baseline. The
machine had unrelated high-CPU R, Python, and compiler processes during
development on 2026-09-27. The collector refuses a complete receipt while
that condition persists. Exploratory runs are not scores against PM3's
performance budgets.

## Work matched before timing

Both adapters read every CSV row, retain validity rather than dropping rows,
convert 1920×1080 display coordinates under a 530×298 mm surface at 600 mm,
compute central-neighbor velocity at 500 Hz, classify at 30 deg/s, apply a
50-sample minimum, and write the same six-column fixation table. The output
is fully written inside each timed iteration. Native detector tables are
written separately inside the timer, and hashed outside it.

- [Eyes4s adapter](../../io/src/test/scala/eyes4s/io/PmCsvIvtMain.scala) uses
  `Delimited`, `Viewing.angularWarp`, and `Detectors.ivt`. Its native table
  includes its additional saccade computation, which remains timed.
- [Pymovements adapter](../../tools/pymovements/bench_csv_ivt.py) uses
  Polars CSV admission and the pinned pymovements 0.28.0 public numpy
  `pix2deg`/`pos2vel` and `events.ivt` functions. The explicit centered,
  y-up input corrects pymovements' different default origin. Each maximal
  tracked segment gets endpoint velocity inheritance to match eyes4s' event
  support. Pymovements' inclusive `offset` becomes a half-open `stop` by
  adding one 2 ms sample period. The 98 ms native minimum represents the
  same 50 samples as eyes4s' 100 ms half-open minimum.

Pre-timing correctness probes used the frozen generated files. Canonical
SHA-256 agrees exactly at all scales:

| Input rows | Fixation rows | Canonical SHA-256 |
|---:|---:|---|
| 10,000 | 40 | `95fb0ebcbd05f0cee4e1b5bb3bdf1e41c44b1802b62bc93f82102146d09add0d` |
| 100,000 | 400 | `0b9de4824d4dc475b4f2e576ee413fe88ab6c0da15cbfce43bad6335a3ea7dba` |
| 1,000,000 | 4,000 | `8c6c6267fdd626f2ea0d7b992d8d69f5755afaa3b8649aebe8db45010b9a0a58` |

A separate 1,000-row probe replaced 30 interior tracked rows with explicit
loss and blank coordinates. Both adapters retained 1,000 admitted rows and
produced four identical fixations, digest
`1f67ea533f15f7e94b6b8dcf2d4d558d62774a69f94f1235d4046574846f575a`.
This is an invalid-data policy check, not an extra timed workload or a frozen
input identity. The full benchmark uses the three all-valid frozen inputs.

## Collector and qualification

[`run_csv_ivt.py`](../../tools/pymovements/run_csv_ivt.py) launches fresh
child processes through macOS `/usr/bin/time -l`, alternates side order by
round, records child stdout/stderr, canonical and native CSV, peak process
RSS, cold startup/total wall, and warm iteration times. Separate JVM
diagnostic processes report ThreadMXBean allocation, retained heap after a
post-measurement settling GC, and collector count/pause. Python native
allocation/heap counters are explicitly unavailable. Every child is bound
to the frozen input and workload digests, the clean source commit/tree,
compiled artifact digest, adapter digest, lock digest, and process identity.
Every pair must match schema, units, row count and canonical hash. A failed
child, timeout, absent RSS, changed output, or incomplete matrix stops the
collector with raw evidence retained. The frozen receipt validator runs only
after all 20 rounds per scale/mode/side have succeeded.

Inputs and a pinned Python 3.14.7 environment can be prepared with:

```sh
uv venv --python 3.14.7 --no-project /private/tmp/eyes4s-pm3-2-20260927/venv
uv pip sync --python /private/tmp/eyes4s-pm3-2-20260927/venv/bin/python \
  --require-hashes tools/pymovements/performance-requirements.txt
python3 tools/pymovements/fixtures.py --write steps-small-csv \
  --output /private/tmp/eyes4s-pm3-2-20260927/steps-small.csv
python3 tools/pymovements/fixtures.py --write steps-typical-csv \
  --output /private/tmp/eyes4s-pm3-2-20260927/steps-typical.csv
python3 tools/pymovements/fixtures.py --write steps-large-csv \
  --output /private/tmp/eyes4s-pm3-2-20260927/steps-large.csv
```

Compile `ioJVM/Test/compile` with the protocol's JDK 25, capture
`show ioJVM/Test/fullClasspath`, and join its `Attributed(...)` entries with
colons into a classpath file. Run from a clean committed branch, with the
three frozen CSV files in the parent of the output directory. An exploratory
one-round run uses `--rounds 1`; it writes `exploratory.json` and cannot
qualify. A full run requires the operator to establish thermal settling and
passes `--thermal-settled`; the collector also checks AC power, normal power
mode and the current competing process snapshot before creating a complete
receipt. It writes raw logs and `samples.jsonl` as it goes. Output directories
must be new so previous evidence cannot be overwritten.

```sh
/private/tmp/eyes4s-pm3-2-20260927/venv/bin/python \
  tools/pymovements/run_csv_ivt.py \
  --classpath /private/tmp/eyes4s-pm3-2-20260927/classpath.txt \
  --output-dir /private/tmp/eyes4s-pm3-2-20260927/full-001 \
  --thermal-settled
/private/tmp/eyes4s-pm3-2-20260927/venv/bin/python \
  tools/pymovements/performance.py \
  --receipt /private/tmp/eyes4s-pm3-2-20260927/full-001/receipt.json
```

The 1,000,000-row exploratory JVM flight recording is retained at
`/private/tmp/eyes4s-pm3-2-20260927/ivt-large.jfr`. Its 125 hot-method
samples include `VectorStatics.mapElemsRest` and the eyes4s SHA-256 core at
10 samples each, plus `SeqOps.indexOf` at five samples. The allocation view
identifies row/vector construction and numeric parsing; multiple young GCs
occurred. This is directional profiling evidence under contention, not a
qualified timing. The single bounded optimization candidate for the next
slice is to resolve the CSV schema's column indexes once per imported file
instead of calling `header.indexOf` for each field of each row. Preserve
lossless row partitioning and both platform targets while testing that change.

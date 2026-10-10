# Private local fixation tests

An opt-in JVM integration harness for unpublished fixation CSVs. Normal tests and
CI never discover or open a dataset; the runner refuses CI environments. Use an
owned checkout with no other sbt process running, per the root `AGENTS.md`.

Keep the dataset, configuration and output **outside every Git checkout**, even
ignored directories. Symlink targets are checked too. The runner reads sources
without modifying them and creates a new private output directory. It uploads
nothing, installs no R packages and publishes no artifacts.

Create a configuration outside Git, with the actual columns and conventions:

```json
{
  "schema_version": 1,
  "coordinates": "screen-pixels-y-down",
  "width_px": 100,
  "height_px": 80,
  "time_unit": "milliseconds",
  "sample_rate_hz": 250,
  "ordinal": "source-record",
  "sources": [{
    "file": "fixations.csv",
    "columns": {
      "participant": "participant",
      "phase": "interest_period",
      "trial": ["block", "trial"],
      "item": "stimulus",
      "x": "x", "y": "y",
      "onset": "onset", "duration": "duration"
    }
  }]
}
```

The dimensions above are illustrative. Geometry and sample rate must come from
the acquisition protocol. The input must be UTF-8 CSV (an optional BOM is accepted).
Each source is checked separately; use distinct interest periods as phases and
include every trial-identity component in `trial`. Source-relative paths cannot
escape the dataset directory. Composite keys are encoded as JSON arrays.

```sh
python3 tools/private-fixtures/run.py \
  --data-root /absolute/private/dataset \
  --config /absolute/private/config.json \
  --preflight-only

python3 tools/private-fixtures/run.py \
  --data-root /absolute/private/dataset \
  --config /absolute/private/config.json \
  --output-dir /absolute/private/new-run
```

The full command compiles `ioJVM` with warnings treated as errors and starts a separate JVM for each source using
that checkout's generated classpath. It exercises `FixationCsv.admitInventory`,
scanpath summaries and the versioned study-input codec. Every admitted trial is
round-tripped, in batches of 128 to bound codec scratch space. Independent Python
calculations from the original CSV check fixation counts, dwell, first onset,
last offset, path length, derived sample counts and off-screen row identities.
Integer counts and microsecond times must agree exactly. Path length uses the
named tolerance `absolute=1e-7 px, relative=1e-12`.

Source record order supplies fixation ordinals; rows are never sorted, clipped or
dropped. Non-overlapping half-open intervals are required within each trial.
Finite off-screen positions remain in scanpaths and are reported explicitly;
bounds are `[0, width) × [0, height)`. Sample counts are explicitly **derived** by
the library from duration and the declared rate, not recovered observed support.
The generated inventory describes observed groups only and cannot establish which
planned trials had no fixations. Any rejected row makes the run fail. This harness
does not claim eyesim parity or validate the manuscript's statistical analyses.

Outputs include normalized CSVs with one-to-one record correspondence, observed
inventories, detailed import/summary reports, logs, source SHA-256 hashes and a
receipt. All are private, including failure reports. Do not copy them into issues,
PRs or public fixtures. A failed run writes `failure.json`; use a new output path
when rerunning. The repository contains only generic code and synthetic tests:

```sh
python3 -m unittest discover -s tools/private-fixtures -v
```

## Comparing with eyesim

`compare.py` installs an exact Git revision from a local eyesim checkout into the
new external run directory. Supply an R library containing its dependencies; the
runner does not install or update dependencies. It records the archived revision,
request hash and R session. Uncommitted edits to the reference checkout are ignored.

```sh
python3 tools/private-fixtures/compare.py \
  --request /absolute/private/comparison-request.json \
  --eyesim /absolute/reference/eyesim \
  --r-library /absolute/private/R-library \
  --output-dir /absolute/private/new-comparison
```

The request has `schema_version: 1`, `x_min`, `x_max`, `y_min`, `y_max`, `nx`, `ny`,
`sigma_px`, `min_fixations` (at least 2), and two arrays:

- `templates`: objects with unique string `id`, string `participant`, and `fixations`.
- `sources`: objects with unique string `id`, string `participant`, a `template` ID,
  and `fixations`.

Each `fixations` object contains equally sized numeric arrays `x`, `y`,
`duration_ms`, and `onset_ms`. Construct this request privately from the paradigm's
declared encoding period, retrieval periods, participant-scoped stimulus matches,
and crop. Record excluded records and unmatched or undersampled trials in the
private selection manifest. The producer refuses out-of-crop coordinates rather
than silently clipping. A source must match a template from the same participant.

eyesim estimates duration-weighted Gaussian maps with the explicit `ks` backend.
Its `template_similarity` results are compared with eyes4s cosine on those **same
arrays**, plus an independently enumerated mean over every distinct nonmatching
template represented in that participant's source cohort. At least one control
must exist. Exhaustive controls avoid comparing unrelated random samples. The
comparison checks observed similarity, control mean, corrected similarity and
control count; scores use `Tolerance(absolute=1e-12, relative=1e-12)`.

This isolates score and pairing conformance. It does not assert that eyesim's
pointwise KDE and eyes4s's grid-convolution smoother are numerically equivalent,
and it does not reproduce a manuscript's sampled baseline or statistical model.
Foils without encoding matches are not assigned a fabricated zero score.

Before reporting an upstream defect, check the current upstream revision and
existing issues, and reproduce it with synthetic inputs that contain no private
records, identifiers, stimulus names or manuscript results.

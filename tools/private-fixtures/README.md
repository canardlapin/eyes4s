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

## End-to-end density comparison and automatic UI harness

`end_to_end.py` starts both estimators from the same fixations and explicit
matched/control identities. It verifies each estimator against an independent
direct Gaussian calculation, then measures differences between their results.
It does **not** assert that the two native estimators are identical:

- This comparison explicitly selects eyesim/ks with `binned = FALSE` and
  `supp = 3.7`, endpoint evaluation coordinates and `zapsmall(digits = 7)` rounding.
  This fixes the algorithm rather than allowing ks to switch to binning at larger
  fixation counts. Its finite-support maps also pass an independent oracle.
- eyes4s bins points into cells, evaluates at cell centers, truncates its
  separable kernel at `ceil(3 sigma / cell size)` cells per axis, and normalizes
  the resulting map globally. The edge policy is `Truncate`.
- A separate eyesim run disables binning and extends support across the whole
  requested rectangle. Its independent oracle allows only the rounding error
  implied by that map's decimal precision. The eyes4s oracle explicitly models
  its binning and finite support, with named absolute/relative tolerances.

The output separates the reference's finite support, endpoint versus center
grid, point binning, and finite support. Array discrepancies compare corresponding
indices; endpoints and centers are different physical coordinates. Per-source
matched, control-mean, and corrected-score differences are reported by phase.
These descriptive checks are not an estimate of a manuscript effect or its model.

The request uses the schema above. Map IDs must be globally unique, and its
template pool must equal the distinct templates represented in the source cohort.
Every participant must have at least two such templates. Sources may add `phase`.
Maps may also supply `response`, `display_kind` and `image_file` for Studio's
inventory. Omitted display kinds remain `unknown`; missing images are never
relabeled as blank displays. Image bytes are not required for this comparison.
For the UI, every source needs a nonempty phase distinct from `Encoding`, and the
request additionally declares:

```json
{
  "screen_width_px": 800,
  "screen_height_px": 600,
  "sample_rate_hz": 250,
  "pixels_per_degree": 30,
  "geometry_note": "Synthetic geometry; declared 30 pixels per degree."
}
```

UI crop bounds must be integral pixels. Studio currently declares smoothing in
degrees: the harness sets `sigma_px / pixels_per_degree`, preserving the requested
pixel kernel. Document the source and approximation of a real angular conversion;
do not substitute a guessed calibration. CSV sample counts are explicitly derived
from the declared sample rate; they are not observed tracker counts.

With JDK 22+ (25 recommended), R, eyesim's dependencies, and the Studio pins installed
as described in the Studio build documentation, run this synthetic demonstration:

```sh
python3 tools/private-fixtures/synthetic.py --output /private/tmp/density-request.json
python3 tools/private-fixtures/end_to_end.py \
  --request /private/tmp/density-request.json \
  --eyesim /path/to/eyesim-checkout \
  --r-library /path/to/local/R-library \
  --output-dir /private/tmp/density-demonstration \
  --ui
```

Use the same command with an external private request. Each output directory must
be new. Omit `--ui` for numerical checks alone. The synthetic request exercises
asymmetric rectangular grids, unequal duration weights, subcell shifts, edge
points, multiple participants, and three phases without private records.

The automatic UI route uses the real `StudioWindow`, native admission and native
execution backend. Only file-dialog answers supply paths. The harness selects
column mappings, time units, display metadata and geometry in the real import
wizard, admits the dataset, chooses a preset and edits phase labels, grid, crop,
weighting and Gaussian scales through the actual recipe controls. It clicks Apply
settings and Save & run, verifies every raw query and exact pair identity, checks
the rendered results table, and reopens the saved project to verify its artifacts.
Each retrieval phase has its own saved revision and run; the harness never seeds
a custom recipe through a test-only command.

It uses Monocle headlessly and retains the normal machine-wide FX lock. The build
receives no private inputs; a separate JUnit JVM runs in the external directory,
with external snapshots and logs. The suite is opt-in and reads no dataset during
ordinary CI. The runner's final receipt requires both JUnit success and the UI
result, so a skipped or failed test cannot count as evidence.

Inspect these files under the output's `comparison/` directory:

- `end-to-end.json`: estimator decomposition, scores, exact controls, phase summaries.
- `phase-N-settings.png`, `phase-N-results.png`: the actual Studio window.
- `phase-N-ui-rows.json`: full-precision rows delivered to the UI.
- `ui-result.json`: UI route and result counts.
- `density.eyes/`: the saved native project; open it with Studio's **File → Open**.

`receipt.json` binds the run to the implementation, request, and exact eyesim
Git archive. All project bytes, screenshots, logs and derived results remain
outside Git. The harness never files an upstream issue automatically.

For a complete cohort, `cohort.py` accepts the same arguments and partitions the
request by participant. Every declared query and its entire within-participant
control pool are retained. It writes participant cells, equal-participant condition
means, and paired cue/probe contrasts; absent cells are never imputed. These are
descriptive comparisons, not a reproduction of sampled-control manuscript models.
All request metadata and reports remain in the external output directory.

```sh
python3 -m venv /private/tmp/eyes4s-analysis-python
/private/tmp/eyes4s-analysis-python/bin/python -m pip install numpy
/private/tmp/eyes4s-analysis-python/bin/python tools/private-fixtures/cohort.py \
  --request /private/path/all-trials.json --eyesim /private/path/eyesim \
  --r-library /private/path/R-library --output-dir /private/path/new-cohort-run
```

NumPy is optional and accelerates the independent per-point Gaussian sums and
cosine dot products. The synthetic tests compare it to the standard-library oracle
under the same tolerances. Use `--ui` to exercise all participant shards in Studio.
`--resume` accepts only the identical request, source revision and implementation;
failed attempts are preserved and rerun in a fresh directory.


`multimatch.py` consumes a verified density run and the identical original request:

```sh
python3 tools/private-fixtures/multimatch.py \
  --request /private/path/all-trials.json --density-run /private/path/cohort-run \
  --r-library /private/path/R-library --output-dir /private/path/new-multimatch-run
```

It reinstalls the archived reference revision, compares the five ungrouped
MultiMatch components on the exact density pair schedule, and normalizes spatial
components by the declared physical screen diagonal. The cropped fixation
sequences and their order remain unchanged. Every pair is retained, including
short paths and errors. The three-fixation baseline is explicit; under RequireAll,
an unavailable matched or control pair makes that query unavailable. Reports give
component-specific participant cells and conditional contrasts with those query
exclusions listed. The R package's sixth EMD component is outside this comparison.
A `review-required` receipt records disagreements rather than claiming parity.

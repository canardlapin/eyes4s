# Memory eye-tracking datasets

A source catalog, an explicit downloader and a sequential eyes4s qualification
runner for the memory datasets discussed in this project. Only the manifest,
tools and synthetic tests belong in Git. Downloads, converted tables, logs and
qualification receipts live in the ignored `.datasets/memory/` directory, or an
external directory selected with `EYES4S_DATASETS_DIR` or `--cache`.

The catalog covers all 16 leads. **One analysis adapter is implemented: FIGRIM
spatial occupancy.** Other entries report `BLOCKED` with a specific next step.
Finding a repository, downloading a file or parsing a table does not establish
that eyes4s can complete the intended analysis.

## Run a dataset

From the repository root, with Python 3.10+:

```sh
# Offline catalog inspection; does not download anything.
python3 tools/memory-datasets/datasets.py list
python3 tools/memory-datasets/datasets.py show figrim

# Explicit acquisition and offline integrity verification.
python3 tools/memory-datasets/datasets.py fetch figrim --file allImages_release.mat
python3 tools/memory-datasets/datasets.py verify figrim

# Optional dependencies for the MATLAB adapter (Python 3.11+), in a local environment.
python3 -m venv .datasets/venv
.datasets/venv/bin/pip install -r tools/memory-datasets/requirements.txt

# Execute real eyes4s APIs, then compare every score with an independent oracle.
.datasets/venv/bin/python tools/memory-datasets/datasets.py check figrim

# Independently decode the original MAT in Octave and SciPy and compare all
# metadata, raw coordinates, admissions, actual bin identities and executed pairs.
# Requires Octave; this does not invoke sbt or download data.
.datasets/venv/bin/python tools/memory-datasets/datasets.py audit figrim

# Check every entry in manifest order, continuing through failures and blockers.
# This does not fetch missing datasets. Exit 1 means at least one non-PASS.
.datasets/venv/bin/python tools/memory-datasets/datasets.py check --all
```

The Scala adapter requires the repository's `sbt` and JDK setup. It invokes
`ioJVM / Test / runMain eyes4s.examples.CheckMemoryLocations` with `GITHUB_ACTIONS=true`
to retain the CI warning policy. It may resolve **build dependencies**, but does
not fetch research datasets. Run one build/check process per checkout. It does
not publish artifacts or launch Studio. The first invocation compiles the needed
library modules; subsequent checks reuse that checkout's compiler outputs.

`PASS` from `verify` means file integrity only. `PASS` from `check` means the
adapter's stated analysis passed. `FAIL` means an integrity, conversion,
execution or numerical check failed. `BLOCKED` means an adapter, source release,
required dependency or metadata is missing. Checks write a separate
`<id>/check.json` including the source receipt digest, manifest-entry digest,
Git revision/tree and dirty flag. A dirty flag means the commit alone cannot
identify the exact executed tooling; qualify a committed candidate for sharing.
The independent raw-input audit writes `audit.json` separately. Its PASS applies
to admission fidelity, not participant linkage, chronological order or inference.
See [the FIGRIM data contract](FIGRIM.md) for evidence and unresolved requirements.

## Inspect and fetch other sources

```sh
# Network metadata only: resolve OSF folders and save a local file inventory.
python3 tools/memory-datasets/datasets.py files brooks2024
python3 tools/memory-datasets/datasets.py files johansson2022

# Choose exact returned paths; --file can be repeated.
python3 tools/memory-datasets/datasets.py fetch kragel2021 --file readme.txt

# Both files together exceed the default 256 MiB per-command budget.
python3 tools/memory-datasets/datasets.py fetch chiquet2022 \
  --file df_encoding.csv --file df_recall.csv --max-bytes 600000000

# External cache, e.g. on a research-data volume.
python3 tools/memory-datasets/datasets.py --cache /Volumes/Research/eyes4s-memory list
```

`fetch` requires named files or an explicit `--all-files`; there is no fetch-all
datasets command. The byte budget applies to the selected files together,
including cache hits. Provider size/checksums are checked when available, and
every successful file gets a local SHA-256 receipt. FIGRIM is pinned to a Git
commit and SHA-256. Live OSF inventories are recorded at discovery; their file
checksums and resolved download locations are recorded at acquisition. This is
first-acquisition pinning, not a promise that an upstream service retains old
versions forever. Preserve the cache/receipt when the provider has no immutable
version URL.

Archives are retained as downloaded and never automatically extracted. Large
Kragel EEG archives, images, videos and fMRI volumes are not fetched by default.
CNeuroMod and Helbing currently have manual acquisition instructions in their
entries; the command refuses to pretend that those recipes are implemented
downloaders. Wang's legacy server and Chakravarthula's unlocated release remain
explicit source blockers.

Corrupt, unreceipted or stale cache entries fail rather than being silently
overwritten. After deliberately updating a manifest entry, use a new cache root
to acquire and qualify that version; retain the old receipt for provenance.
Fetch can resume after a later file fails because each completed file receives
its receipt immediately. Cached payloads are rehashed. Downloads are streamed
to unique temporary files and atomically renamed after validation.

The manifest records dataset-specific license evidence and uncertainty. For
example, FIGRIM's site permits academic research use; an open paper license does
not establish a license for its stimuli or data. This repository redistributes
none of those files. Never use `git add -f` on the cache.

## What the FIGRIM check establishes

The published MATLAB file supplies `enc`, `rec`, `rec2` Nx2 fixation locations
and three recognition-response codes per image/observer slot. The adapter:

1. Preserves image, release observer-slot index, phase and SDT codes. Complete
   ledgers also retain recording label, initials, original trial number, raw
   floating coordinates, original row ordinal, duplicate family and disposition.
   Missing responses are retained without silently relabeling them; the check is not
   stratified by accuracy. Observer slots are not asserted to be unique people
   across recording sessions.
2. Follows the author's pixel-map convention: round to MATLAB image pixels,
   report points outside 1..1000, then subtract one for a zero origin. Records
   every excluded coordinate and retrieval trial without an eligible pair.
3. Builds uniformly weighted point measures on a 1000×1000 pixel frame with a
   10×10 grid using `PointMeasure`, `Grid` and `Surface`. It checks that binning
   preserves the admitted location count and verifies each occupied cell against
   the NumPy histogram, including the cell's x/y identity.
4. Computes `Distribution.cosine` for same-slot/same-image encoding versus
   retrieval and for one deterministic other-image encoding control from the
   same slot and scene category. The control is the first eligible filename in
   lexical order. Six duplicate-image pairs documented by the source are
   excluded from control identities. The difference is also exported.
5. Checks every output against NumPy `histogram2d` and vector normalization/dot
   products, with an absolute tolerance of `1e-12`. Missing, duplicate,
   nonfinite or numerically discrepant scores fail.

The NumPy score oracle shares the converted coordinates and pair schedule. Run
`audit figrim` for the additional independent Octave raw-input, cell and execution
pair comparison. Numerical agreement alone cannot establish that the scientific
interpretation is correct.

On the initial local run, all 630 target images were inspected: 182,535 source
locations, 182,532 admitted locations, 3 exclusions, 30,012 admitted trials and
19,718 matched/control comparisons. There were 208 unpaired retrieval trials.
Maximum absolute oracle discrepancy was `5.56e-16`. These are execution and
numerical-conformance results, not a memory-effect estimate.

This release has no fixation durations or onsets. The adapter fabricates
neither; it cannot establish temporal replay or duration-weighted similarity.
Its coarse grid and single deterministic control are a reproducible capability
check, not an inferential baseline or a replication of the paper. The generated
`trials.csv`, `pair-metadata.csv`, exclusion tables, scores and invocation record
are retained under a unique `analysis-*` directory for each run.

## Known source distinctions

| Entry | Current boundary |
| --- | --- |
| Johansson 2022 | OSF source data are in child `g72qb`; root `d9zng` has stimuli. The inspected workbook contains derived measures and figure/table data. Individual fixation sequences have not been located. Figshare has a supplementary DOCX. |
| Sherlock 2025 | Camera viewing gaze and MRI/DeepMReye recall estimates must remain distinct; narrative-event alignment is still required. |
| CNeuroMod-THINGS | Central fixation; suitable candidate for recognition/compliance analysis. Nested DataLad content and event/sidecar mapping remain to be implemented. |
| Kragel 2021 | Binary EyeLink EDF needs `edf2asc`; NEMAR does not supply aligned `events.tsv`. Original behavior and sync code are needed. |
| Chiquet 2022 | Verified encoding/recall CSV file GUIDs; 3D gaze/AOI data need a projection or AOI-specific analysis. |
| Wynn 2020 | Git-pinned R data files available. Study groups pool four viewings with restarted onsets; recover boundaries before temporal analysis. |
| MoMoView | Public inventory contains derived comparison tables and stimuli; individual fixation sequences have not been established. |
| Other entries | See each manifest `analysis.reason`, source evidence and caveats. |

All source URLs, access observations, licenses and next steps live in
[manifest.json](manifest.json); no mutable success badge is stored there.

## Extend and test

Add a manifest entry with stable identity, paper/repository URLs, license evidence,
access recipe, and explicit analysis blocker. HTTP files may carry size, SHA-256
and MD5 pins. The downloader also supports OSF node inventories and Figshare
article inventories. A source page must never be substituted for a data file.

Implement a named adapter in `adapters.py` only after verifying coordinate units,
axis conventions, clocks, phase/trial identities, missingness and pairing. Run
actual eyes4s APIs, account for rejected data, state the analysis scope and add an
independent correctness check. Converting or parsing files alone is not PASS.

```sh
python3 -m unittest discover -s tools/memory-datasets -p 'test_*.py'
GITHUB_ACTIONS=true sbt 'ioJVM / Test / compile' 'ioJVM / Test / scalafmtCheck'
```

Unit tests use generated fixtures only. Real-data qualification is opt-in and is
not part of normal CI or `testAll`. Full repository landing gates remain separate.

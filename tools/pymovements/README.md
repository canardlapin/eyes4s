# Pymovements comparison inventory

`manifest.json` is the source-pinned capability inventory for
[`PYMOVEMENTS_COMPARISON.md`](../../docs/plans/PYMOVEMENTS_COMPARISON.md).
This tooling checks the evidence contract; it does not run benchmarks or award
scientific/visual superiority. It uses Python 3.10+ and the standard library.

From the repository root:

```sh
python3 tools/pymovements/check.py
python3 -m unittest discover -s tools/pymovements -p 'test_*.py' -v
```

For complete inventory verification, download the exact `comparator.wheel_url`
from `manifest.json` into an external cache, then pass its path. The checker
verifies the whole-wheel hash and every referenced member's hash/anchors. It
does not extract, install or execute package code. The live tracker option uses
the read-only `mote --json ls --all` interface. In a worktree, name the shared
store explicitly:

```sh
python3 tools/pymovements/check.py \
  --wheel /path/to/pymovements-0.28.0-py3-none-any.whl \
  --mote-store /path/to/eyes4s/.mote
```

Optional checks omitted from a run are printed as **NOT CHECKED**. A requested
check failing or being unavailable is a failure. No network access occurs in
the checker. Mote changes and dataset downloads are never performed by it.

The manifest's `sources` map classifies API code, test code and historical
evidence separately. Each capability cites those entries and names its public
scope, conventions, fixtures, missing evidence and next Mote tickets. File
hashes detect source drift; declaration anchors make the intended API inspectable.
Paths are repository-relative (or wheel-member paths), with traversal and
symlink escape rejected. No third-party source is vendored here.

After a reviewed inventory change, regenerate the table, then run checks again:

```sh
python3 tools/pymovements/check.py --write-table
python3 tools/pymovements/check.py
```

Required capability IDs and target memberships are also fixed in `check.py`;
deleting a row from the manifest cannot silently remove a requirement. Positive
workflow/empirical/release statuses require a `{path, sha256}` receipt reference.
The receipt fields are `capability`, `dimension`, `outcome` (`passed`),
`source_commit`, `protocol_sha256`, `command`, `runtime`, `platforms`, `inputs`,
`config_sha256`, `outputs`, and `artifacts`. The three collections contain
`{id, sha256}` entries; `artifacts` must be nonempty for release availability and
use published artifact coordinates as IDs. The protocol digest binds the protocol
document with its generated inventory replaced by the two markers and a single
newline (see `protocol_digest`); changing inventory status does not change the
evaluation criteria. Pending statuses have `receipt: null`.

These structural checks cannot establish that a claimed pass is scientifically
sound or that a URL remains installable. Keep raw measurements and independently
review their commands, inputs and frozen criteria. The initial inventory has no
positive qualification receipts; existing Scala suites are pointers, not new
test-run claims. This slice adds a standalone check, not a generated CI workflow.

## Performance protocol (PM3.1)

The CSV-to-I-VT collector currently permits exploratory runs only. Qualified collection needs
source-bound build provenance for the compiled eyes4s classpath; a classpath hash and the current
Git revision alone do not establish that relationship. Full-round collection is refused until
that evidence is implemented. No timings in this landing establish a performance advantage.

[`PYMOVEMENTS_PERFORMANCE.md`](../../docs/plans/PYMOVEMENTS_PERFORMANCE.md) and
`performance.json` freeze the workload matrix, input identities, machine/runtime
pins and numeric budgets before measurement. `receipt.schema.json` describes raw
receipts; `performance.py` checks both that schema and cross-record comparability.
It uses `jsonschema==4.26.0` (also pinned in the performance dependency lock).

```sh
python3 tools/pymovements/performance.py --check-inputs --mote-store /path/to/eyes4s/.mote
python3 -m unittest discover -s tools/pymovements -p 'test_*.py' -v
python3 tools/pymovements/fixtures.py --write steps-small-csv --output /tmp/steps-small.csv
python3 tools/pymovements/performance.py --receipt /path/to/raw-receipt.json
python3 tools/pymovements/performance.py --receipt /path/to/raw-receipt.json --require-complete
```

The first command regenerates hashes without storing large inputs. Fixture writes
create a new file and refuse to overwrite an existing one. Receipt checking does
not execute benchmarks or award a speed/memory win; a subset receipt may cover the
first pipeline but cannot satisfy `--require-complete`.

Use an isolated CPython 3.14.7 environment for the benchmark. Install the resolved
lock with hashes, then record the actual package graph and interpreter in receipts:

```sh
uv venv --python 3.14.7 /tmp/eyes4s-pm-perf-env
uv pip sync --python /tmp/eyes4s-pm-perf-env/bin/python --require-hashes tools/pymovements/performance-requirements.txt
```

This prepares dependencies, not a passing benchmark. The lock was resolved with
`uv pip compile ... --python-version 3.14 --python-platform aarch64-apple-darwin
--generate-hashes`. Do not refresh it mid-experiment. To update the readable
workload table after a reviewed protocol change, use
`python3 tools/pymovements/performance.py --write-table`, refresh the reviewed
`bindings.protocol_doc.sha256`, and rerun validation. If the prose changed first,
refresh that binding before regeneration as well. The document binding deliberately
detects changes to scientific or statistical rules beyond the numeric JSON fields.

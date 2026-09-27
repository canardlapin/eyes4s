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

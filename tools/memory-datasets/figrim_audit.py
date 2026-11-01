"""Independent Octave/SciPy raw-input comparison; no downloads or Scala build."""

import csv
import hashlib
import json
import math
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

from figrim_contract import BIN_FIELDS, LOCATION_FIELDS, PHASE_FIELDS, configuration_error

PAIR_FIELDS = ["observer_slot", "image", "phase", "encoding_image", "control_image"]


def execution_pairs(directory):
    """Resolve the ACTUAL Scala input IDs, never a parallel descriptive table."""
    with (directory / "trials.csv").open(newline="") as f:
        records = list(csv.DictReader(f))
    trials = {r["trial_id"]: r for r in records}
    if len(trials) != len(records):
        raise ValueError("Duplicate trial IDs in execution metadata")
    result, ids = [], set()
    with (directory / "pairs.tsv").open(newline="") as f:
        for r in csv.DictReader(f, delimiter="\t"):
            if r["pair_id"] in ids:
                raise ValueError("Duplicate execution pair ID")
            ids.add(r["pair_id"])
            focal, encoding, control = (trials[r[k]] for k in ("focal_id", "encoding_id", "control_id"))
            if (focal["phase"] not in ("rec", "rec2") or encoding["phase"] != "enc" or control["phase"] != "enc"
                    or len({focal["observer_slot"], encoding["observer_slot"], control["observer_slot"]}) != 1
                    or len({focal["category"], encoding["category"], control["category"]}) != 1
                    or focal["image"] != encoding["image"] or focal["image"] == control["image"]):
                raise ValueError(f"Invalid encoding/control execution identities for {r['pair_id']}")
            result.append([focal["observer_slot"], focal["image"], focal["phase"], encoding["image"], control["image"]])
    return result


def compare_tables(left, right, fields, keys, numeric=()):
    """Compare keyed values exactly, allowing only numeric text-format differences."""
    def read(path):
        with path.open(newline="", encoding="utf-8") as f:
            reader = csv.DictReader(f, delimiter="\t")
            if not set(fields).issubset(reader.fieldnames or ()):
                raise ValueError(f"Missing comparison columns: {path.name}")
            out = {}
            for row in reader:
                key = tuple(row[k] for k in keys)
                if key in out:
                    raise ValueError(f"Duplicate witness key in {path.name}: {key}")
                out[key] = tuple(float(row[k]) if k in numeric and row[k] != "" else row[k] for k in fields)
            return out
    a, b = read(left), read(right)
    if a.keys() != b.keys():
        raise ValueError(f"Raw witness key mismatch in {left.name}: {len(a)} vs {len(b)} rows")
    for key in a:
        for field, x, y in zip(fields, a[key], b[key]):
            equal = x == y or (isinstance(x, float) and isinstance(y, float) and math.isnan(x) and math.isnan(y))
            if not equal:
                raise ValueError(f"Raw witness mismatch in {left.name} at {key}/{field}: {x!r} vs {y!r}")
    return len(a)


def compare_admission(python_dir, octave_dir):
    compared = {}
    for filename, fields, keys, numeric in (
        ("phase-ledger.tsv", PHASE_FIELDS, ("image_index", "observer_slot", "phase"),
         ("source_trial", "sdt", "source_locations", "admitted_locations")),
        ("location-ledger.tsv", LOCATION_FIELDS, ("image_index", "observer_slot", "phase", "ordinal"),
         ("raw_x", "raw_y", "rounded_x", "rounded_y", "admitted", "x_px", "y_px")),
        ("bins.tsv", BIN_FIELDS, ("image_index", "observer_slot", "phase", "bin_x", "bin_y"), ("count",)),
    ):
        compared[filename] = compare_tables(python_dir / filename, octave_dir / filename, fields, keys, numeric)
    return compared


def run_octave(source, directory, width=1000, height=1000, cols=10, rows=10):
    exe = shutil.which("octave")
    if exe is None:
        raise FileNotFoundError("Independent raw admission requires Octave; no shared-reader fallback")
    env = dict(os.environ, EYES4S_FIGRIM_SOURCE=str(source.resolve()),
               EYES4S_FIGRIM_OUT=str(directory.resolve()), EYES4S_FIGRIM_TOOLS=str(Path(__file__).resolve().parent))
    expression = ("addpath(getenv('EYES4S_FIGRIM_TOOLS')); "
                  f"figrim_witness(getenv('EYES4S_FIGRIM_SOURCE'),getenv('EYES4S_FIGRIM_OUT'),{width},{height},{cols},{rows});")
    command = [exe, "--no-init-file", "--no-site-file", "--no-history", "--no-gui", "--quiet", "--eval", expression]
    with (directory / "octave.log").open("w") as log:
        result = subprocess.run(command, env=env, stdout=log, stderr=subprocess.STDOUT, timeout=600)
    if result.returncode:
        raise ValueError(f"Octave witness exited {result.returncode}; see {directory / 'octave.log'}")
    return {"argv": command, "version": subprocess.check_output([exe, "--version"], text=True).splitlines()[0]}


def audit(dataset, dataset_cache, receipt):
    if dataset.get("id") != "figrim":
        return {"status": "BLOCKED", "reason": "Independent raw admission audit currently supports FIGRIM only"}
    error = configuration_error(dataset)
    if error:
        return {"status": "BLOCKED", "reason": error}
    if shutil.which("octave") is None:
        return {"status": "BLOCKED", "reason": "Install Octave to supply an independent MAT decoder"}
    try:
        import scipy
        import scipy.io
        import numpy
        from adapters import prepare_figrim, write_table
    except ImportError:
        return {"status": "BLOCKED", "reason": "Install requirements.txt for the SciPy MAT decoder"}
    required = "allImages_release.mat"
    if required not in {f["path"] for f in receipt["files"]}:
        return {"status": "BLOCKED", "reason": f"Fetch {required} first"}
    run = Path(tempfile.mkdtemp(prefix="admission-", dir=dataset_cache))
    py, oct = run / "python", run / "octave"
    py.mkdir(); oct.mkdir()
    start = time.monotonic()
    try:
        source = dataset_cache / required
        images = scipy.io.loadmat(source, simplify_cells=True)["allImages"]
        _, counts = prepare_figrim(images, py, dataset["analysis"]["duplicate_image_pairs"])
        python_seconds = time.monotonic() - start
        write_table(py / "semantic-pairs.tsv", PAIR_FIELDS, execution_pairs(py))
        octave_start = time.monotonic()
        invocation = run_octave(source, oct)
        octave_seconds = time.monotonic() - octave_start
        compared = compare_admission(py, oct)
        compared["pairs.tsv"] = compare_tables(py / "semantic-pairs.tsv", oct / "pairs.tsv", PAIR_FIELDS, PAIR_FIELDS[:3])
        invocation.update({"numpy": numpy.__version__, "scipy": scipy.__version__,
                           "source_sha256": hashlib.sha256(source.read_bytes()).hexdigest(),
                           "python_seconds": python_seconds, "octave_seconds": octave_seconds})
        (run / "invocation.json").write_text(json.dumps(invocation, indent=2) + "\n")
        artifacts = {str(p.relative_to(run)): hashlib.sha256(p.read_bytes()).hexdigest()
                     for p in sorted(run.rglob("*")) if p.is_file()}
        return {"status": "PASS", "reason": "Independent Octave and SciPy admission agree exactly on keyed metadata, raw coordinates, raster admissions, cells and baseline pairs",
                "scope": "Raw-input and spatial-admission fidelity only; no participant linkage, chronology or inferential qualification",
                "counts": counts, "compared_rows": compared, "numeric_tolerance": 0,
                "capabilities": {"raw_admission": "PASS", "participant_linkage": "BLOCKED", "chronological_order": "BLOCKED", "inference": "NOT_RUN"},
                "run_directory": str(run), "artifacts_sha256": artifacts}
    except (ValueError, KeyError, OSError, subprocess.SubprocessError) as error:
        return {"status": "FAIL", "reason": str(error), "run_directory": str(run)}

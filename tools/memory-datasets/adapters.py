"""Dataset-specific qualification, invoked only by an explicit `check` command."""

import csv
import hashlib
import json
import math
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
from collections import defaultdict

from figrim_contract import admit, configuration_error, families

ROOT = Path(__file__).resolve().parents[2]
# Double precision dot products over 100 nonnegative bins; allows accumulated
# rounding, while rejecting changes to coordinates, keys, weighting or pairing.
COSINE_ABSOLUTE_TOLERANCE = 1e-12
PHASES = ("enc", "rec", "rec2")


def write_table(path, header, rows, delimiter="\t"):
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f, delimiter=delimiter, lineterminator="\n")
        writer.writerow(header)
        writer.writerows(rows)


def prepare_figrim(images, directory, duplicate_pairs):
    """Keep release observer slots, image identity, phase, SDT and every exclusion.

    Reproduce the authors' location-map convention: round to nearest positive
    MATLAB pixel, exclude outside 1..1000, then subtract one for a zero origin.
    These coordinates support spatial occupancy, not timing or scanpath replay.
    """
    import numpy as np

    admitted_trials, admission = admit(images, directory, duplicate_pairs)
    aliases = families([i["filename"] for i in images], duplicate_pairs)
    points, metadata, exclusions, trials = [], [], [], {}
    for key, trial in admitted_trials.items():
        slot, image, phase = key
        trial_id = f"t{len(trials)}"
        trials[key] = (trial_id, trial.points, trial.category, trial.sdt)
        points.extend([trial_id, int(x), int(y)] for x, y in trial.points)
        metadata.append([trial_id, slot, image, trial.category, phase, trial.sdt,
                         trial.image_index, aliases[image]])
    # Compatibility export; the complete lossless source is location-ledger.tsv.
    with (directory / "location-ledger.tsv").open(newline="") as f:
        for row in csv.DictReader(f, delimiter="\t"):
            if row["admitted"] == "0":
                image = images[int(row["image_index"]) - 1]["filename"]
                exclusions.append([row["observer_slot"], image, row["phase"], row["ordinal"],
                                   row["raw_x"], row["raw_y"], row["reason"]])
    encodings = defaultdict(list)
    for key, value in sorted(trials.items()):
        if key[2] == "enc":
            encodings[(key[0], value[2])].append((key, value))
    pairs, pair_metadata, unpaired = [], [], []
    for (slot, image, phase), (focal, _, category, sdt) in sorted(trials.items()):
        if phase == "enc":
            continue
        encoding = trials.get((slot, image, "enc"))
        # Deterministic, same-observer-slot and same-category control. Exclude
        # the six documented duplicate-image identities as well as the item itself.
        controls = [(k, v) for k, v in encodings[(slot, category)]
                    if aliases.get(k[1], k[1]) != aliases.get(image, image)]
        if encoding is None or not controls:
            unpaired.append([slot, image, phase, "missing encoding" if encoding is None else "no distinct same-category control"])
            continue
        control_key, control = controls[0]
        pair_id = f"p{len(pairs)}"
        pairs.append([pair_id, focal, encoding[0], control[0]])
        pair_metadata.append([pair_id, slot, image, phase, sdt, control_key[1]])
    if not pairs:
        raise ValueError("No eligible encoding/retrieval pairs")
    write_table(directory / "locations.tsv", ["trial_id", "x_px", "y_px"], points)
    write_table(directory / "pairs.tsv", ["pair_id", "focal_id", "encoding_id", "control_id"], pairs)
    write_table(directory / "trials.csv", ["trial_id", "observer_slot", "image", "category", "phase", "sdt", "image_index", "image_family"], metadata, ",")
    write_table(directory / "pair-metadata.csv", ["pair_id", "observer_slot", "image", "phase", "sdt", "control_image"], pair_metadata, ",")
    write_table(directory / "excluded-locations.csv", ["observer_slot", "image", "phase", "ordinal", "source_x", "source_y", "reason"], exclusions, ",")
    write_table(directory / "unpaired-trials.csv", ["observer_slot", "image", "phase", "reason"], unpaired, ",")
    # Independent histogram implementation; cosine uses BLAS-backed vector
    # operations, not the Scala grid indexing or comparison implementation.
    histograms = {}
    for trial_id, locations, _, _ in trials.values():
        h, _, _ = np.histogram2d(locations[:, 0], locations[:, 1], bins=10, range=((0, 1000), (0, 1000)))
        histograms[trial_id] = h.ravel() / np.linalg.norm(h)
    expected = {pid: (float(histograms[f] @ histograms[e]), float(histograms[f] @ histograms[c]))
                for pid, f, e, c in pairs}
    counts = dict(admission["counts"], pairs=len(pairs), unpaired_retrieval_trials=len(unpaired))
    return expected, counts


def verify_results(path, expected):
    with path.open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f, delimiter="\t"))
    ids = [r["pair_id"] for r in rows]
    if len(ids) != len(set(ids)) or set(ids) != set(expected):
        raise ValueError("Scala results do not account for exactly the expected pairs")
    maximum = 0.0
    for row in rows:
        matched, control = expected[row["pair_id"]]
        for name, reference in (("matched", matched), ("control", control), ("difference", matched - control)):
            observed = float(row[name])
            error = abs(observed - reference)
            if not math.isfinite(observed) or error > COSINE_ABSOLUTE_TOLERANCE:
                raise ValueError(f"Independent oracle mismatch for {row['pair_id']}/{name}: {observed} vs {reference}")
            maximum = max(maximum, error)
    return maximum


def verify_bins(directory):
    """Check actual cell identities; global axis swaps can preserve all cosines."""
    with (directory / "trials.csv").open(newline="") as f:
        keys = {r["trial_id"]: (r["image_index"], r["observer_slot"], r["phase"])
                for r in csv.DictReader(f)}
    with (directory / "bins.tsv").open(newline="") as f:
        expected = {(r["image_index"], r["observer_slot"], r["phase"], r["bin_x"], r["bin_y"]): int(r["count"])
                    for r in csv.DictReader(f, delimiter="\t")}
    observed = {}
    with (directory / "scala-bins.tsv").open(newline="") as f:
        for r in csv.DictReader(f, delimiter="\t"):
            key = (*keys[r["trial_id"]], r["bin_x"], r["bin_y"])
            count = float(r["count"])
            if key in observed or not math.isfinite(count) or count <= 0 or count != int(count):
                raise ValueError(f"Invalid or duplicate Scala bin: {key}")
            observed[key] = int(count)
    if observed != expected:
        raise ValueError("Scala bin identities/counts disagree with raw admission histograms")


def check(dataset, dataset_cache, receipt):
    if dataset["analysis"]["adapter"] != "figrim_locations":
        return {"status": "BLOCKED", "reason": "No implemented adapter"}
    error = configuration_error(dataset)
    if error:
        return {"status": "BLOCKED", "reason": error}
    try:
        import scipy.io
        import scipy
        import numpy
    except ImportError:
        return {"status": "BLOCKED", "reason": "FIGRIM adapter needs NumPy and SciPy; install requirements.txt in a local venv"}
    if shutil.which("sbt") is None:
        return {"status": "BLOCKED", "reason": "sbt is required to execute the eyes4s API check"}
    required = "allImages_release.mat"
    if required not in {f["path"] for f in receipt["files"]}:
        return {"status": "BLOCKED", "reason": f"Fetch {required} first"}
    run = Path(tempfile.mkdtemp(prefix="analysis-", dir=dataset_cache))
    try:
        images = scipy.io.loadmat(dataset_cache / required, simplify_cells=True)["allImages"]
        expected, counts = prepare_figrim(images, run, dataset["analysis"]["duplicate_image_pairs"])
        # sbt parses this inner command; do not permit a path to inject commands.
        paths = [run / "locations.tsv", run / "pairs.tsv", run / "scores.tsv", run / "scala-bins.tsv"]
        if any(any(c in str(p) for c in '\n\r";') for p in paths):
            raise ValueError("Unsupported character in cache path for sbt launcher")
        command = ["sbt", "-batch", "ioJVM / Test / runMain eyes4s.examples.CheckMemoryLocations " + " ".join('"' + str(p) + '"' for p in paths)]
        environment = dict(os.environ)
        environment["GITHUB_ACTIONS"] = "true"
        (run / "invocation.json").write_text(json.dumps({"argv": command, "cwd": str(ROOT), "numpy": numpy.__version__, "scipy": scipy.__version__}, indent=2) + "\n")
        with (run / "sbt.log").open("w") as log:
            result = subprocess.run(command, cwd=ROOT, env=environment, stdout=log, stderr=subprocess.STDOUT, timeout=1800)
        if result.returncode:
            return {"status": "FAIL", "reason": f"eyes4s runner exited {result.returncode}; see {run / 'sbt.log'}", "run_directory": str(run)}
        error = verify_results(run / "scores.tsv", expected)
        verify_bins(run)
        artifacts = {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in run.iterdir() if p.is_file()}
        return {"status": "PASS", "reason": "Location-only spatial qualification passed; not temporal replay, inference or paper replication",
                "scope": "all target-image locations; uniform 10x10 pixel-grid cosine; one deterministic same-observer-slot same-category control per eligible retrieval",
                "counts": counts, "max_absolute_oracle_error": error,
                "oracle_absolute_tolerance": COSINE_ABSOLUTE_TOLERANCE,
                "run_directory": str(run), "artifacts_sha256": artifacts}
    except (ValueError, KeyError, OSError, subprocess.TimeoutExpired) as e:
        return {"status": "FAIL", "reason": str(e), "run_directory": str(run)}

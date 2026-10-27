#!/usr/bin/env python3
"""Opt-in local fixation checks. Inputs, manifests, logs and results stay outside Git."""

import argparse
from collections import defaultdict
import csv
from dataclasses import dataclass
from decimal import Decimal, ROUND_CEILING, ROUND_FLOOR
import hashlib
import json
import math
import os
from pathlib import Path
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[2]
HEADER = ["participant", "phase", "trial", "item", "ordinal", "x", "y", "onset", "duration"]
ROLES = ("participant", "phase", "item", "x", "y", "onset", "duration")
SCALES = {"seconds": 1000000, "milliseconds": 1000, "microseconds": 1}


@dataclass(frozen=True)
class Tolerance:
    absolute: float
    relative: float

    def matches(self, a, b):
        return (math.isfinite(a) and math.isfinite(b)
                and abs(a - b) <= self.absolute + self.relative * max(abs(a), abs(b)))


PATH_TOLERANCE = Tolerance(absolute=1e-7, relative=1e-12)


def outside_git(path):
    """Resolve symlinks, including existing parents of a not-yet-created output path."""
    path = path.expanduser().resolve()
    if any((p / ".git").exists() for p in (path, *path.parents)):
        raise ValueError("Private paths must be outside every Git checkout (including ignored paths)")
    return path


def digest(path):
    h = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def dump(path, value):
    with path.open("x", encoding="utf-8") as stream:
        json.dump(value, stream, indent=2, allow_nan=False)
        stream.write("\n")


def implementation():
    """Bind local evidence to the checkout and the unpublished harness sources too."""
    files = sorted(Path(__file__).parent.glob("*.py")) + sorted(Path(__file__).parent.glob("*.R"))
    files += [ROOT / "tools/study-cli" / name for name in
              ("PrivateFixationCli.scala", "SharedDensityParityCli.scala", "EndToEndDensityCli.scala", "PrivateMultiMatchCli.scala", "PlannedDensityCheckCli.scala")]
    files += sorted((ROOT / "studio/desktop/src/test/scala/eyes4s/studio/desktop/journey").glob("PrivateDensity*.scala"))
    files += sorted((ROOT / "studio/desktop/src/test/scala/eyes4s/studio/desktop/journey").glob("PrivateProduction*.scala"))
    files += sorted((ROOT / "studio/core/src/main/scala/eyes4s/studio/core/scanpath").glob("*.scala"))
    files += sorted((ROOT / "studio/desktop/src/main/scala/eyes4s/studio/desktop/scanpath").glob("*.scala"))
    files += [ROOT / "studio/desktop/src/test/scala/eyes4s/studio/desktop/journey/ScanpathWorkbenchFxSuite.scala"]
    return {
        "commit": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
        "tree": subprocess.check_output(["git", "rev-parse", "HEAD^{tree}"], cwd=ROOT, text=True).strip(),
        "tracked_diff_sha256": hashlib.sha256(subprocess.check_output(
            ["git", "diff", "HEAD", "--binary"], cwd=ROOT)).hexdigest(),
        "harness_sha256": {str(p.relative_to(ROOT)): digest(p) for p in files},
    }


def read_rows(path, columns):
    if not path.is_file() or path.stat().st_size == 0:
        raise ValueError("A source CSV is missing or empty; make the dataset available locally")
    with path.open(encoding="utf-8-sig", newline="") as stream:
        reader = csv.reader(stream, strict=True)
        header = next(reader, [])
        required = [columns[k] for k in ROLES] + columns["trial"]
        if len(set(header)) != len(header) or not set(required).issubset(header):
            raise ValueError("A source CSV has duplicate or missing mapped columns")
        count = 0
        for record, values in enumerate(reader, 2):
            if len(values) != len(header):
                raise ValueError(f"CSV record {record} has the wrong width; no rows were dropped")
            count += 1
            yield record, dict(zip(header, values))
        if count == 0:
            raise ValueError("A source CSV contains a header but no data records")


def trial_key(row, columns):
    # JSON arrays preserve composite boundaries, including commas and embedded newlines.
    parts = [row[c] for c in columns["trial"]]
    if any(not p.strip() for p in parts):
        raise ValueError("A source row has a blank trial-key component")
    return json.dumps(parts, ensure_ascii=False, separators=(",", ":"))


def validate_config(config):
    if not isinstance(config, dict) or type(config.get("schema_version")) is not int or config["schema_version"] != 1:
        raise ValueError("Expected schema_version 1")
    if config.get("coordinates") != "screen-pixels-y-down":
        raise ValueError("Declare coordinates as screen-pixels-y-down; convert other frames explicitly")
    for name in ("width_px", "height_px"):
        if type(config.get(name)) is not int or not 0 < config[name] <= 2147483647:
            raise ValueError(f"{name} must be a positive 32-bit integer")
    if config.get("time_unit") not in SCALES:
        raise ValueError("Declare seconds, milliseconds or microseconds")
    rate = config.get("sample_rate_hz")
    if type(rate) not in (int, float) or not math.isfinite(rate) or rate <= 0:
        raise ValueError("Declare the acquisition sample_rate_hz; counts are derived, not observed")
    if config.get("ordinal") != "source-record":
        raise ValueError("Declare ordinal as source-record; this harness never sorts fixation rows")
    if not isinstance(config.get("sources"), list) or not config["sources"]:
        raise ValueError("Declare at least one source")
    for source in config["sources"]:
        if not isinstance(source, dict):
            raise ValueError("Each source must be an object")
        if not isinstance(source.get("file"), str) or not source["file"]:
            raise ValueError("Each source needs a relative file path")
        cols = source.get("columns", {})
        if not isinstance(cols, dict):
            raise ValueError("columns must be an object")
        if any(not isinstance(cols.get(k), str) or not cols[k].strip() for k in ROLES):
            raise ValueError("Map every required column role")
        if (not isinstance(cols.get("trial"), list) or not cols["trial"]
                or any(not isinstance(c, str) or not c.strip() for c in cols["trial"])):
            raise ValueError("trial must be a nonempty array of column names")


def sources_in(config, data_root):
    sources = []
    for source in config["sources"]:
        relative = Path(source["file"])
        path = (data_root / relative).resolve()
        if relative.is_absolute() or not path.is_relative_to(data_root):
            raise ValueError("Source paths must stay within the data root, including through symlinks")
        # A full shape check precedes any build. Numeric admission remains eyes4s's job.
        count = sum(1 for _ in read_rows(path, source["columns"]))
        sources.append((source, path, count, digest(path)))
    return sources


def prepare(source, path, directory, config):
    directory.mkdir(mode=0o700)
    columns = source["columns"]
    inventory = set()
    with (directory / "input.csv").open("x", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(HEADER)
        for record, row in read_rows(path, columns):
            key = (row[columns["participant"]], row[columns["phase"]], trial_key(row, columns))
            inventory.add(key)
            writer.writerow([*key, row[columns["item"]], record,
                             *(row[columns[k]] for k in ("x", "y", "onset", "duration"))])
    with (directory / "inventory.csv").open("x", encoding="utf-8", newline="") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(["participant", "phase", "trial"])
        writer.writerows(sorted(inventory))
    dump(directory / "request.json", {k: config[k] for k in
         ("width_px", "height_px", "time_unit", "sample_rate_hz")})


def micros(raw, unit):
    return int((Decimal(raw) * SCALES[unit] + Decimal("0.5")).to_integral_value(rounding=ROUND_FLOOR))


def reference(source, path, config):
    """Compute from the original CSV, independently of normalized input and Scala output."""
    columns = source["columns"]
    groups = defaultdict(list)
    outside = []
    for record, row in read_rows(path, columns):
        key = (row[columns["participant"]], row[columns["phase"]],
               trial_key(row, columns), row[columns["item"]])
        x, y = (float(row[columns[k]]) for k in ("x", "y"))
        onset, duration = (micros(row[columns[k]], config["time_unit"])
                           for k in ("onset", "duration"))
        groups[key].append((x, y, onset, duration))
        if not (0 <= x < config["width_px"] and 0 <= y < config["height_px"]):
            outside.append(record)
    trials = {}
    for key, points in groups.items():
        trials[key] = {
            "fixations": len(points), "dwell_us": sum(p[3] for p in points),
            "first_onset_us": points[0][2], "last_offset_us": points[-1][2] + points[-1][3],
            "path_length_px": math.fsum(math.hypot(b[0] - a[0], b[1] - a[1])
                                        for a, b in zip(points, points[1:])),
            "derived_samples": sum(min(2147483647, int(
                (Decimal(p[3]) * Decimal(str(config["sample_rate_hz"])) / 1000000)
                .to_integral_value(rounding=ROUND_CEILING))) for p in points),
        }
    return trials, outside


def compare(actual, expected, outside, count):
    if (actual["source_rows"] != count or actual["admitted_rows"] != count
            or actual["rejected"] or actual["outside_frame_rows"] != outside):
        raise ValueError("Import accounting or outside-frame classification differs")
    observed = {tuple(t["key"]): t for t in actual["trials"]}
    if (len(observed) != len(actual["trials"]) or observed.keys() != expected.keys()
            or actual["roundtrip_trials"] != len(expected)):
        raise ValueError("Trial identities, cardinality or codec round-trip coverage differ")
    for key, metrics in expected.items():
        for name, value in metrics.items():
            found = observed[key][name]
            matches = (PATH_TOLERANCE.matches(found, value) if name == "path_length_px"
                       else found == value)
            if not matches:
                # This detail is saved only in the private receipt.
                raise ValueError(f"{name} differs for trial {key!r}: {found} vs {value}")


def build(output):
    with (output / "build.log").open("xb") as log:
        subprocess.run(["sbt", "-J-Xmx3g", "-J-XX:ActiveProcessorCount=4",
                        "ioJVM/Test/compile", "ioJVM/Test/resources"], cwd=ROOT,
                       env=dict(os.environ, GITHUB_ACTIONS="true"),
                       stdout=log, stderr=subprocess.STDOUT, check=True)
    paths = list((ROOT / "io/.jvm/target").glob(
        "scala-*/resource_managed/test/eyes4s/fresh-process/classpath.txt"))
    if len(paths) != 1:
        raise ValueError("Expected exactly one generated JVM test classpath; clean stale Scala targets")
    return paths[0].read_text().strip()


def run(config, sources, output):
    classpath = build(output)
    receipts = []
    for index, (source, path, count, source_hash) in enumerate(sources):
        directory = output / f"source-{index:03d}"
        prepare(source, path, directory, config)
        env = dict(os.environ, EYES4S_PRIVATE_RUN=str(directory))
        with (directory / "jvm.log").open("xb") as log:
            child = subprocess.run(["java", "-Xmx2g", "-cp", classpath,
                                    "eyes4s.examples.PrivateFixationCli"], cwd=output,
                                   env=env, stdout=log, stderr=subprocess.STDOUT)
        if child.returncode != 0:
            raise ValueError(f"Source {index} failed import or round-trip; inspect its private report")
        if digest(path) != source_hash:
            raise ValueError("A source changed during the run; results cannot qualify that source")
        actual = json.loads((directory / "actual.json").read_text())
        expected, outside = reference(source, path, config)
        compare(actual, expected, outside, count)
        if digest(path) != source_hash:
            raise ValueError("A source changed during reference calculation")
        receipts.append({"source": source["file"], "sha256": source_hash, "rows": count,
                         "trials": len(expected), "outside_frame_rows": len(outside)})
        print(f"Source {index + 1}: import, summaries and codec round trips passed", flush=True)
    return receipts


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data-root", required=True, type=Path)
    parser.add_argument("--config", required=True, type=Path)
    parser.add_argument("--output-dir", type=Path, help="new directory outside Git and the dataset")
    parser.add_argument("--preflight-only", action="store_true")
    args = parser.parse_args(argv)
    os.umask(0o077)
    output = None
    try:
        if os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"):
            raise ValueError("Private fixture execution is local-only and disabled in CI")
        data_root, manifest = outside_git(args.data_root), outside_git(args.config)
        config = json.loads(manifest.read_text(encoding="utf-8"))
        validate_config(config)
        sources = sources_in(config, data_root)
        if args.preflight_only:
            print(f"Preflight passed for {len(sources)} nonempty CSV sources; no build or analysis ran")
            return 0
        if args.output_dir is None:
            raise ValueError("--output-dir is required unless --preflight-only is used")
        candidate = outside_git(args.output_dir)
        if candidate.is_relative_to(data_root):
            raise ValueError("Output must be outside the source dataset")
        candidate.mkdir(mode=0o700, parents=True, exist_ok=False)
        output = candidate
        dump(output / "config.json", config)
        provenance = implementation()
        receipts = run(config, sources, output)
        if implementation() != provenance:
            raise ValueError("Implementation changed during the run; rerun on fixed inputs")
        dump(output / "receipt.json", {"status": "passed", "implementation": provenance,
             "sources": receipts, "path_tolerance": vars(PATH_TOLERANCE),
             "sample_counts": "ceil(duration_us * sample_rate_hz / 1000000), capped at Int.MaxValue",
             "oracle": "independent Python summaries; no eyesim parity claim"})
        print("Private fixture checks passed. All reports remain in the external output directory.")
        return 0
    except (ValueError, OSError, csv.Error, KeyError, TypeError, ArithmeticError,
            subprocess.SubprocessError) as error:
        if output is not None:
            dump(output / "failure.json", {"status": "failed", "error": str(error)})
            print("Private fixture checks failed; inspect failure.json in the output directory.", file=sys.stderr)
        else:
            print(f"Preflight failed: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())

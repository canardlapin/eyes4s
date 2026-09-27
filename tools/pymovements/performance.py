#!/usr/bin/env python3
"""Validate frozen performance inputs and raw receipt comparability, not performance wins."""

import argparse
import hashlib
import importlib.metadata
import json
import math
from pathlib import Path
import subprocess
import sys

from jsonschema import Draft202012Validator, ValidationError

import check
import fixtures

ROOT = Path(__file__).resolve().parents[2]
CONFIG = Path("tools/pymovements/performance.json")
SCHEMA = Path("tools/pymovements/receipt.schema.json")
DOC = Path("docs/plans/PYMOVEMENTS_PERFORMANCE.md")
START, END = "<!-- performance-table:start -->", "<!-- performance-table:end -->"
REQUIRED = {
    "ingest-csv": "paired", "ingest-asc": "paired", "pix2deg": "paired",
    "velocity": "paired", "median": "absolute", "savitzky-golay": "paired",
    "interpolate": "absolute", "ivt": "paired", "idt-stationary": "paired",
    "idt-churn": "descriptive", "microsaccades": "paired", "qc-loss": "paired",
    "aoi-dwell": "paired", "reading-first-pass": "paired", "pipeline-csv-ivt": "paired",
    "pipeline-asc-reading": "paired", "density": "absolute", "multimatch": "absolute",
    "study": "absolute", "streaming-asc": "absolute", "ui-selection": "absolute",
    "ui-trial": "absolute", "ui-perspective": "absolute",
}
LIMITS = {
    "max_warm_time_ratio": 1.10, "max_peak_rss_ratio": 1.10,
    "aggregate_max_warm_time_ratio": 0.80, "aggregate_large_max_rss_ratio": 0.80,
    "max_cold_extra_ms": 1500, "max_cold_startup_ms": 5000,
}
require = check.require


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"), allow_nan=False).encode()


def digest(value):
    return hashlib.sha256(canonical(value)).hexdigest()


def read(path):
    def reject_constant(value):
        raise ValueError(f"nonfinite JSON constant: {value}")
    return json.loads(path.read_text(), object_pairs_hook=check.unique_object,
                      parse_constant=reject_constant)


def positive(value, name):
    require(type(value) in (int, float) and math.isfinite(value) and value > 0, f"{name} must be finite and positive")


def table(config, root=ROOT, write=False):
    lines = [START, "", "| Workload | Comparison | Warm wall cap, small / typical / large (ms) | Next owner |",
             "|---|---|---|---|"]
    for work in config["workloads"]:
        caps = " / ".join(str(work["absolute_wall_ms"].get(s, "—")) for s in fixtures.SIZES)
        lines.append(f"| `{work['id']}` | {work['comparison']} | {caps} | `{work['owner']}` |")
    expected = "\n".join(lines) + "\n\n" + END
    path = root / DOC
    text = path.read_text()
    require(text.count(START) == text.count(END) == 1 and text.index(START) < text.index(END), "invalid workload table markers")
    before, rest = text.split(START)
    old, after = rest.split(END)
    if write:
        path.write_text(before + expected + after)
    else:
        require(START + old + END == expected, "stale workload table; use --write-table")


def validate_protocol(config, root=ROOT, verify_inputs=False, issues=None):
    check.fields(config, "schema_version protocol_id issue epic inventory_commit comparator machine "
                 "runtime bindings measurement budgets canonical_output datasets workloads", "performance protocol")
    require(type(config["schema_version"]) is int and config["schema_version"] == 1
            and config["protocol_id"] == "eyes4s-pymovements-performance-v1",
            "unsupported protocol version")
    check.digest(config["inventory_commit"], 40)
    for key in ("issue", "epic"):
        check.issue(config[key])
    inventory = read(root / check.MANIFEST)
    require(config["comparator"] == inventory["comparator"], "comparator pin drift")
    require(set(config["bindings"]) == {"generator", "dependency_input", "dependency_lock",
                                       "studio_fixture_generator", "corpus_manifest", "receipt_schema", "protocol_doc"},
            "missing evidence binding")
    for binding in config["bindings"].values():
        check.fields(binding, "path sha256", "binding")
        check.digest(binding["sha256"])
        require(check.sha(check.local(root, binding["path"]).read_bytes()) == binding["sha256"],
                f"changed binding: {binding['path']}")
    require(importlib.metadata.version("jsonschema") == "4.26.0", "use pinned jsonschema 4.26.0")
    Draft202012Validator.check_schema(read(root / SCHEMA))
    check.fields(config["machine"], "id cpu ram_bytes os os_version os_build arch power", "machine")
    for key, value in config["machine"].items():
        positive(value, key) if key == "ram_bytes" else check.string(value, key)
    runtime = config["runtime"]
    check.fields(runtime, "python java java_vendor scala sbt jvm_flags threads environment python_packages", "runtime")
    for key in ("python", "java", "java_vendor", "scala", "sbt"):
        check.string(runtime[key], key)
    require(type(runtime["threads"]) is int and runtime["threads"] == 1 and runtime["environment"] and
            all(v == "1" for v in runtime["environment"].values()), "single-thread policy drift")
    check.strings(runtime["jvm_flags"], "JVM flags")
    require(runtime["python_packages"].get("pymovements") == config["comparator"]["version"],
            "Python environment omits comparator")
    import re
    locked = dict(re.findall(r"^([A-Za-z0-9_-]+)==([^\s]+)",
                            (root / config["bindings"]["dependency_lock"]["path"]).read_text(), re.M))
    require(runtime["python_packages"] == locked, "Python dependency graph differs from lock")
    measure = config["measurement"]
    check.fields(measure, "rounds warmups warm_samples_per_process cold_samples_per_process "
                 "display_samples_per_process order timer_unit rss_unit cache materialization gc rss_instrument "
                 "timeout_seconds bootstrap_replicates bootstrap_seed confidence uncertainty noise", "measurement")
    require(all(type(measure[k]) is int for k in ("rounds", "warmups", "warm_samples_per_process",
                                                "cold_samples_per_process", "display_samples_per_process")),
            "measurement repetitions must be integers")
    require((measure["rounds"], measure["warmups"], measure["warm_samples_per_process"],
             measure["cold_samples_per_process"], measure["display_samples_per_process"])
            == (20, 5, 3, 1, 100), "measurement repetition policy drift")
    require(measure["timer_unit"] == "ns" and measure["rss_unit"] == "bytes", "measurement unit mismatch")
    require(measure["confidence"] == 0.95 and measure["bootstrap_replicates"] == 10000,
            "uncertainty policy drift")
    budgets = config["budgets"]
    require(all(type(budgets.get(k)) in (int, float) and budgets[k] == v for k, v in LIMITS.items()),
            "v1 numeric budgets changed; a new reviewed protocol is required")
    require(budgets["scale_weights"] == {"small": 0.2, "typical": 0.3, "large": 0.5}, "scale weights changed")
    datasets = {}
    for data in config["datasets"]:
        check.string(data["id"], "dataset id")
        require(data["id"] not in datasets, "duplicate dataset")
        require(data["scale"] in fixtures.SIZES and data["kind"] in ("synthetic", "recipe"), "unknown dataset kind/scale")
        check.digest(data["sha256"])
        positive(data["rows"], "dataset rows")
        if data["kind"] == "synthetic":
            check.fields(data, "id kind family format scale rows sha256 bytes", "synthetic input")
            require(data["family"] in fixtures.FAMILIES + ("reading-events",) and
                    data["format"] in ("csv", "asc") and
                    (data["family"] != "reading-events" or data["format"] == "csv"), "unknown synthetic family/format")
            expected_rows = (fixtures.EVENT_SIZES if data["family"] == "reading-events" else fixtures.SIZES)[data["scale"]]
            require(data["rows"] == expected_rows, "input size drift")
            positive(data["bytes"], "dataset bytes")
            if verify_inputs:
                require(fixtures.identity(data) == {k: data[k] for k in ("sha256", "bytes")}, "synthetic input drift")
        else:
            check.fields(data, "id kind family format scale rows replicas sources transform sha256", "recipe input")
            require(digest({k: v for k, v in data.items() if k != "sha256"}) == data["sha256"], "recipe identity drift")
            for source in data["sources"]:
                require(check.sha(check.local(root, source["path"]).read_bytes()) == source["sha256"], "recipe source changed")
        datasets[data["id"]] = data
    workloads = {}
    for work in config["workloads"]:
        check.fields(work, "id title group comparison owner datasets modes output_columns output_units contract "
                     "absolute_wall_ms absolute_peak_rss_mib max_growth_per_10x", "workload")
        require(work["id"] not in workloads, "duplicate workload")
        require(work["id"] in REQUIRED and work["comparison"] == REQUIRED[work["id"]],
                "unknown workload or comparison class drift")
        check.issue(work["owner"])
        if issues is not None:
            # A completed adapter's historical owner remains valid provenance for the frozen protocol.
            require(work["owner"] in issues, "missing workload owner")
        for key in ("title", "group", "contract"):
            check.string(work[key], key)
        scales = {"small", "large"} if work["group"] == "ui" else set(fixtures.SIZES)
        require(set(work["datasets"]) == scales, "workload omits a scale")
        for scale, id in work["datasets"].items():
            require(id in datasets and datasets[id]["scale"] == scale, "workload input/scale mismatch")
        require(work["modes"] == (["display"] if work["group"] == "ui" else ["cold", "warm"]), "measurement modes drift")
        check.strings(work["output_columns"], "output columns")
        require(isinstance(work["output_units"], dict) and work["output_units"], "missing output units")
        for value in work["output_units"].values():
            check.string(value, "output unit")
        for key in ("absolute_wall_ms", "absolute_peak_rss_mib"):
            require(set(work[key]) == scales, "absolute budget omits a scale")
            for value in work[key].values():
                positive(value, key)
        if work["group"] == "ui":
            cap = {"ui-selection": 100, "ui-trial": 250, "ui-perspective": 150}[work["id"]]
            require(all(v == cap for v in work["absolute_wall_ms"].values()), "Studio latency budget drift")
        positive(work["max_growth_per_10x"], "scaling budget")
        workloads[work["id"]] = work
    require(set(workloads) == set(REQUIRED), "missing mandatory workload")
    return workloads, datasets


def validate_receipt(data, config, root=ROOT, complete=False):
    schema = read(root / SCHEMA)
    Draft202012Validator(schema).validate(data)
    workloads, datasets = validate_protocol(config, root)
    require(data["protocol_sha256"] == digest(config), "receipt uses a different protocol")
    require(data["machine"] == config["machine"], "machine mismatch")
    require(data["runtime"] == config["runtime"], "runtime/version/thread mismatch")
    pin = config["comparator"]
    require(data["revisions"]["pymovements"] == {"commit": pin["source_commit"], "wheel_sha256": pin["wheel_sha256"]},
            "comparator revision mismatch")
    require(data["dependency_lock_sha256"] == config["bindings"]["dependency_lock"]["sha256"], "dependency lock mismatch")
    scope = data["scope"]
    require(len(scope) == len(set(scope)) and set(scope) <= set(workloads), "invalid receipt scope")
    require(not complete or set(scope) == set(workloads), "complete receipt omits mandatory workloads")
    observed, pairs, processes = set(), {}, set()
    for sample in data["samples"]:
        id, scale, mode, side, round_id = (sample[k] for k in ("workload", "scale", "mode", "side", "round"))
        require(id in scope and scale in workloads[id]["datasets"] and mode in workloads[id]["modes"],
                "sample is outside declared scope")
        work = workloads[id]
        sides = ("eyes4s",) if work["comparison"] == "absolute" else ("eyes4s", "pymovements")
        require(side in sides and 0 <= round_id < config["measurement"]["rounds"], "invalid side/round")
        key = (id, scale, mode, side, round_id)
        require(key not in observed, "duplicate measurement cell")
        observed.add(key)
        require(sample["process_instance"] not in processes, "process reused as independent replication")
        processes.add(sample["process_instance"])
        expected_order = 0 if len(sides) == 1 else int((side == "eyes4s") != (round_id % 2 == 0))
        require(sample["order"] == expected_order, "paired order policy mismatch")
        require(sample["status"] == "ok" and sample["exit_code"] == 0 and sample["failure"] is None,
                "failed/timeout/OOM process is not passing performance evidence")
        dataset = datasets[work["datasets"][scale]]
        require(sample["input_identity_sha256"] == dataset["sha256"], "input identity mismatch")
        if dataset["kind"] == "synthetic":
            require(sample["materialized_input_sha256"] == dataset["sha256"], "materialized input mismatch")
        require(sample["config_sha256"] == digest(work), "workload configuration mismatch")
        require(sample["warmups"] == (0 if mode == "cold" else config["measurement"]["warmups"]), "warmup mismatch")
        count = config["measurement"][mode + "_samples_per_process"]
        require(len(sample["wall_ns"]) == count, "missing raw timing repetitions")
        require((sample["startup_ns"] is not None) == (mode == "cold"), "startup measurement missing or mixed with warm/display")
        if mode == "cold":
            require(sample["startup_ns"] <= sample["wall_ns"][0], "startup exceeds total process time")
        require(sample["peak_rss_bytes"] is not None and sample["output"] is not None, "missing RSS or materialized output")
        output = sample["output"]
        require(output["rows"] > 0, "empty output cannot qualify these nonempty benchmark fixtures")
        require(output["columns"] == work["output_columns"] and output["units"] == work["output_units"],
                "output schema/unit mismatch")
        for name, counter in sample["diagnostics"].items():
            if name == "receipt_sha256":
                continue
            require((counter["value"] is None) == (counter["unavailable_reason"] is not None), "counter availability is ambiguous")
            require(side != "eyes4s" or counter["value"] is not None, "JVM diagnostic counter missing")
        pair_key = (id, scale, mode, round_id)
        pairs.setdefault(pair_key, {})[side] = sample
    expected = {
        (id, scale, mode, side, round_id)
        for id in scope for scale in workloads[id]["datasets"] for mode in workloads[id]["modes"]
        for side in (("eyes4s",) if workloads[id]["comparison"] == "absolute" else ("eyes4s", "pymovements"))
        for round_id in range(config["measurement"]["rounds"])
    }
    require(observed == expected, "missing required scale/mode/side/round measurements")
    for (id, _, _, _), pair in pairs.items():
        if workloads[id]["comparison"] != "paired":
            continue
        a, b = pair["eyes4s"], pair["pymovements"]
        require(a["materialized_input_sha256"] == b["materialized_input_sha256"], "paired inputs differ")
        require(all(a["output"][key] == b["output"][key] for key in ("columns", "units", "rows", "canonical_sha256")),
                "scientific outputs differ; timing cannot count as an equivalent comparison")
    return len(observed)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check-inputs", action="store_true", help="regenerate synthetic hashes without writing data")
    parser.add_argument("--mote-store", type=Path)
    parser.add_argument("--receipt", type=Path)
    parser.add_argument("--require-complete", action="store_true")
    parser.add_argument("--write-table", action="store_true")
    args = parser.parse_args()
    try:
        require(not args.require_complete or args.receipt is not None, "--require-complete needs --receipt")
        config = read(ROOT / CONFIG)
        issues = None
        if args.mote_store:
            result = subprocess.run(["mote", "--store", str(args.mote_store), "--json", "ls", "--all"],
                                    capture_output=True, text=True, check=True, timeout=60)
            issues = {row["id"]: row["status"] for row in json.loads(result.stdout)}
        workloads, datasets = validate_protocol(config, ROOT, args.check_inputs, issues)
        table(config, write=args.write_table)
        print(f"PASS: frozen protocol; {len(workloads)} workloads, {len(datasets)} input identities")
        print("Protocol SHA256:", digest(config))
        print("Synthetic bytes:", "verified" if args.check_inputs else "NOT REGENERATED (use --check-inputs)")
        print("Live ownership:", "verified" if issues is not None else "NOT CHECKED (use --mote-store)")
        if args.receipt:
            count = validate_receipt(read(args.receipt), config, complete=args.require_complete)
            print(f"PASS: {count} comparable measurement cells; scope {'complete' if args.require_complete else 'explicit subset'}")
        print("No benchmark executed; no performance threshold or superiority verdict awarded.")
        return 0
    except (ValueError, KeyError, TypeError, OSError, ValidationError, subprocess.SubprocessError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())

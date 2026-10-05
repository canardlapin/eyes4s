#!/usr/bin/env python3
"""Run PM3.2 paired fresh-process CSV-to-I-VT measurements.

Writes every child log and an append-only sample ledger. A complete, validated
receipt is emitted only after all 20 rounds pass semantic and environment gates.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import sys
import time

import performance

ROOT = Path(__file__).resolve().parents[2]
WORKLOAD = "pipeline-csv-ivt"
SCALES = ("small", "typical", "large")
MODES = ("cold", "warm")
SIDES = ("eyes4s", "pymovements")
JAVA = Path("/opt/homebrew/opt/openjdk/libexec/openjdk.jdk/Contents/Home/bin/java")
RSS = re.compile(rb"^\s*(\d+)\s+maximum resident set size\s*$", re.M)


def sha(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def file_sha(path: Path) -> str:
    return sha(path.read_bytes())


def git(*args: str) -> str:
    return subprocess.check_output(["git", *args], cwd=ROOT, text=True).strip()


def artifact_sha(classpath: list[Path]) -> str:
    digest = hashlib.sha256()
    for entry in classpath:
        if entry.is_dir():
            files = sorted(entry.rglob("*.class"))
        else:
            files = [entry]
        for item in files:
            digest.update(str(item.relative_to(entry) if entry.is_dir() else entry.name).encode())
            digest.update(bytes.fromhex(file_sha(item)))
    return digest.hexdigest()


def adapter_sha() -> str:
    digest = hashlib.sha256()
    for relative in (
        "io/src/test/scala/eyes4s/io/PmCsvIvtMain.scala",
        "tools/pymovements/bench_csv_ivt.py",
        "tools/pymovements/run_csv_ivt.py",
    ):
        digest.update(relative.encode())
        digest.update((ROOT / relative).read_bytes())
    return digest.hexdigest()


def environment(config: dict, scratch: Path) -> dict[str, str]:
    env = os.environ.copy()
    env.update(config["runtime"]["environment"])
    mpl = scratch / "mpl"
    mpl.mkdir(exist_ok=True)
    env["MPLCONFIGDIR"] = str(mpl)
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    return env


def conditions(output_dir: Path, thermal_settled: bool) -> dict:
    battery = subprocess.check_output(["pmset", "-g", "batt"], text=True)
    power = subprocess.check_output(["pmset", "-g", "custom"], text=True)
    processes = subprocess.check_output(
        ["ps", "-Ao", "pid=,pcpu=,comm="], text=True,
    )
    (output_dir / "conditions.log").write_text(
        "pmset -g batt\n" + battery + "\npmset -g custom\n" + power +
        "\nps -Ao pid=,pcpu=,comm=\n" + processes,
    )
    busy = busy_processes(processes)
    (output_dir / "competing.json").write_text(json.dumps(busy, indent=2) + "\n")
    ac = "Now drawing from 'AC Power'" in battery
    ac_settings = power.split("AC Power:", 1)[-1].split("Battery Power:", 1)[0]
    normal = re.search(r"^\s*powermode\s+0\s*$", ac_settings, re.M) is not None
    return {
        "ac_power": ac, "low_power_mode": not normal,
        "competing_compute": bool(busy),
        "thermal_settled": thermal_settled and not busy,
        "os_file_cache": "primed", "notes": (
            "Operator observed thermal settling; current process snapshot in conditions.log"
            if thermal_settled else
            "Thermal settling not attested; exploratory data only. Process snapshot in conditions.log"
        ),
    }


def busy_processes(processes: str) -> list[tuple[str, str, str]]:
    busy = []
    for line in processes.splitlines():
        fields = line.strip().split(None, 2)
        if len(fields) == 3:
            try:
                if float(fields[1]) >= 50:
                    busy.append((fields[0], fields[1], fields[2]))
            except ValueError:
                pass
    return busy


def check_interference(output_dir: Path, scale: str, mode: str,
                       round_id: int, stage: str) -> None:
    processes = subprocess.check_output(
        ["ps", "-Ao", "pid=,pcpu=,comm="], text=True,
    )
    busy = busy_processes(processes)
    with (output_dir / "interference.jsonl").open("a") as stream:
        stream.write(json.dumps({"scale": scale, "mode": mode,
                                 "round": round_id, "stage": stage,
                                 "busy": busy}) + "\n")
    if busy:
        raise RuntimeError(
            f"competing compute at {scale}/{mode}/{round_id}/{stage}; raw evidence retained"
        )


def run_child(command: list[str], output_dir: Path, label: str,
              env: dict[str, str], timeout: int) -> dict:
    start = time.monotonic_ns()
    try:
        child = subprocess.run(
            ["/usr/bin/time", "-l", *command], cwd=ROOT, env=env,
            capture_output=True, timeout=timeout,
        )
        elapsed = time.monotonic_ns() - start
        stdout, stderr = child.stdout, child.stderr
        code = child.returncode
        status = "ok" if code == 0 else ("oom" if code in (-9, 137) else "error")
        failure = None if code == 0 else f"exit code {code}"
    except subprocess.TimeoutExpired as error:
        elapsed = time.monotonic_ns() - start
        stdout, stderr = error.stdout or b"", error.stderr or b""
        code, status, failure = None, "timeout", f"exceeded {timeout}s"
    (output_dir / f"{label}.stdout").write_bytes(stdout)
    (output_dir / f"{label}.stderr").write_bytes(stderr)
    rss = RSS.search(stderr)
    if status == "ok" and rss is None:
        status, failure = "error", "macOS /usr/bin/time -l omitted peak RSS"
    return {
        "start_ns": start, "elapsed_ns": elapsed, "stdout": stdout, "stderr": stderr,
        "exit_code": code, "status": status, "failure": failure,
        "peak_rss_bytes": int(rss.group(1)) if rss else None,
    }


def diagnostics(side: str, values: dict | None, log_hash: str) -> dict:
    names = ("allocated_bytes", "retained_heap_bytes", "gc_pause_ns", "gc_collections")
    return {
        "receipt_sha256": log_hash,
        **{
            name: {
                "value": values[name] if values is not None else None,
                "instrument": ("JDK ThreadMXBean / MemoryMXBean / GarbageCollectorMXBean"
                               if side == "eyes4s" else "Python native heap and allocation unavailable"),
                "unavailable_reason": (None if values is not None else
                                       "No comparable native allocation, retained heap, or GC counter"),
            }
            for name in names
        },
    }


def command(side: str, input_file: Path, output_file: Path, warmups: int,
            samples: int, classpath: str, python: Path, diagnostic: bool = False) -> list[str]:
    if side == "eyes4s":
        args = [str(JAVA), "-Xms256m", "-Xmx8g", "-XX:+UseG1GC",
                "-XX:ActiveProcessorCount=1", "-cp", classpath,
                "eyes4s.io.PmCsvIvtMain"]
    else:
        args = [str(python), str(ROOT / "tools/pymovements/bench_csv_ivt.py")]
    args += [str(input_file), str(output_file), str(warmups), str(samples)]
    if diagnostic:
        args.append("diagnostic")
    return args


def sample(config: dict, work: dict, dataset: dict, side: str, scale: str,
           mode: str, round_id: int, order: int, output_dir: Path,
           classpath: str, python: Path, env: dict[str, str], diagnostic: dict) -> dict:
    label = f"{scale}-{mode}-{round_id:02d}-{side}"
    input_file = output_dir.parent / f"steps-{scale}.csv"
    output_file = output_dir / f"{label}.csv"
    assert file_sha(input_file) == dataset["sha256"]
    warmups = 0 if mode == "cold" else config["measurement"]["warmups"]
    repetitions = config["measurement"][mode + "_samples_per_process"]
    cmd = command(side, input_file, output_file, warmups, repetitions, classpath, python)
    result = run_child(cmd, output_dir, label, env, config["measurement"]["timeout_seconds"])
    parsed = None
    if result["status"] == "ok":
        try:
            parsed = json.loads(result["stdout"].decode().strip())
            if parsed["accepted_rows"] != dataset["rows"] or parsed["fixations"] < 1:
                raise ValueError("accepted/event row count differs")
            if file_sha(output_file) != parsed["output_sha256"]:
                raise ValueError("output digest differs from child report")
            if file_sha(Path(str(output_file) + ".native.csv")) != parsed["native_sha256"]:
                raise ValueError("native output digest differs from child report")
            if len(parsed["iterations_ns"]) != repetitions:
                raise ValueError("missing timed iteration")
        except (ValueError, KeyError, OSError) as error:
            result["status"], result["failure"] = "error", f"invalid child output: {error}"
    start_ns = None
    if parsed is not None and mode == "cold":
        # Both JVM System.nanoTime and Python monotonic_ns use the macOS
        # continuous monotonic clock on this frozen machine. Reject drift.
        start_ns = parsed["ready_ns"] - result["start_ns"]
        if start_ns < 0 or start_ns > result["elapsed_ns"]:
            result["status"], result["failure"] = "error", "startup clocks not comparable"
    digest = parsed["output_sha256"] if parsed else None
    return {
        "workload": WORKLOAD, "scale": scale, "mode": mode, "round": round_id,
        "side": side, "order": order, "process_instance": label,
        "command": cmd, "status": result["status"], "exit_code": result["exit_code"],
        "failure": result["failure"], "input_identity_sha256": dataset["sha256"],
        "materialized_input_sha256": dataset["sha256"],
        "config_sha256": performance.digest(work), "warmups": warmups,
        "wall_ns": ([result["elapsed_ns"]] if mode == "cold" else
                    parsed["iterations_ns"] if parsed else []),
        "startup_ns": start_ns, "peak_rss_bytes": result["peak_rss_bytes"],
        "output": ({"columns": work["output_columns"], "units": work["output_units"],
                    "rows": parsed["fixations"], "canonical_sha256": digest,
                    "native_sha256": parsed["native_sha256"]} if parsed else None),
        "diagnostics": diagnostic,
    }


def require_pair(pair: list[dict], scale: str, mode: str, round_id: int) -> None:
    if len(pair) != 2 or any(item["status"] != "ok" for item in pair):
        raise RuntimeError(f"failed process: {scale}/{mode}/{round_id}")
    left, right = (item["output"] for item in pair)
    if any(left[key] != right[key] for key in
           ("columns", "units", "rows", "canonical_sha256")):
        raise RuntimeError(f"semantic mismatch: {scale}/{mode}/{round_id}; raw outputs retained")


def throughput(item: dict, dataset: dict) -> dict:
    if item["status"] != "ok" or not item["wall_ns"]:
        raise ValueError("failed or unmeasured process has no throughput")
    elapsed_ns = statistics.median(item["wall_ns"])
    return {
        "process_instance": item["process_instance"],
        "input_sha256": item["input_identity_sha256"],
        "input_rows": dataset["rows"], "input_bytes": dataset["bytes"],
        "basis": "total cold process wall" if item["mode"] == "cold" else
                 "median of three timed warm iterations",
        "wall_ns": item["wall_ns"],
        "rows_per_second": dataset["rows"] * 1e9 / elapsed_ns,
        "bytes_per_second": dataset["bytes"] * 1e9 / elapsed_ns,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--rounds", type=int, default=20)
    parser.add_argument("--thermal-settled", action="store_true",
                        help="operator attests an idle, thermally settled machine before a full run")
    parser.add_argument("--python", type=Path, default=Path("/private/tmp/eyes4s-pm3-2-20260927/venv/bin/python"))
    parser.add_argument("--classpath", type=Path, default=Path("/private/tmp/eyes4s-pm3-2-20260927/classpath.txt"))
    args = parser.parse_args()
    config = performance.read(ROOT / performance.CONFIG)
    if not 1 <= args.rounds <= config["measurement"]["rounds"]:
        parser.error("round count outside protocol")
    if git("status", "--porcelain"):
        parser.error("commit adapters before collecting revision-bound measurements")
    if not JAVA.exists() or not args.python.exists():
        parser.error("pinned runtime missing")
    if subprocess.check_output([str(args.python), "--version"], text=True).strip() != "Python " + config["runtime"]["python"]:
        parser.error("Python runtime differs from protocol")
    if config["runtime"]["java"] not in subprocess.check_output([str(JAVA), "-version"], stderr=subprocess.STDOUT, text=True):
        parser.error("Java runtime differs from protocol")
    installed = json.loads(subprocess.check_output([
        str(args.python), "-c",
        "import importlib.metadata as m,json; print(json.dumps({d.metadata['Name'].lower().replace('_','-'):d.version for d in m.distributions()}))",
    ], text=True))
    if {name: installed.get(name) for name in config["runtime"]["python_packages"]} != config["runtime"]["python_packages"]:
        parser.error("installed Python dependency graph differs from protocol")
    if subprocess.check_output(["sw_vers", "-productVersion"], text=True).strip() != config["machine"]["os_version"]:
        parser.error("macOS version differs from protocol")
    if subprocess.check_output(["sw_vers", "-buildVersion"], text=True).strip() != config["machine"]["os_build"]:
        parser.error("macOS build differs from protocol")
    if subprocess.check_output(["uname", "-m"], text=True).strip() != config["machine"]["arch"]:
        parser.error("machine architecture differs from protocol")
    classpath = args.classpath.read_text().strip()
    entries = [Path(entry) for entry in classpath.split(":")]
    if not all(entry.exists() for entry in entries):
        parser.error("classpath has missing compiled entries")
    work = next(w for w in config["workloads"] if w["id"] == WORKLOAD)
    datasets = {d["id"]: d for d in config["datasets"]}
    output_dir = args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=False)
    env = environment(config, output_dir.parent)
    for scale in SCALES:
        source = output_dir.parent / f"steps-{scale}.csv"
        expected = datasets[work["datasets"][scale]]
        if not source.exists() or file_sha(source) != expected["sha256"]:
            parser.error(f"missing or changed {scale} fixture at {source}")
        source.read_bytes()  # Prime OS file cache outside all timed intervals.
    observed_conditions = conditions(output_dir, args.thermal_settled)
    if args.rounds == config["measurement"]["rounds"] and not (
        observed_conditions["ac_power"] and not observed_conditions["low_power_mode"]
        and not observed_conditions["competing_compute"]
        and observed_conditions["thermal_settled"]
    ):
        parser.error("full receipt needs AC, normal power, no competing compute and thermal settling; see conditions.log")
    report = {
        "schema_version": 1, "phase": "baseline",
        "protocol_sha256": performance.digest(config), "scope": [WORKLOAD],
        "machine": config["machine"], "runtime": config["runtime"],
        "revisions": {
            "eyes4s": {"commit": git("rev-parse", "HEAD"),
                       "tree": git("rev-parse", "HEAD^{tree}"), "dirty": False,
                       "artifact_sha256": artifact_sha(entries)},
            "pymovements": {"commit": config["comparator"]["source_commit"],
                            "wheel_sha256": config["comparator"]["wheel_sha256"]},
        },
        "adapter_sha256": adapter_sha(),
        "dependency_lock_sha256": config["bindings"]["dependency_lock"]["sha256"],
        "conditions": observed_conditions,
        "samples": [],
    }
    (output_dir / "run.json").write_text(json.dumps({
        "head": report["revisions"]["eyes4s"], "protocol_sha256": report["protocol_sha256"],
        "adapter_sha256": report["adapter_sha256"], "rounds_requested": args.rounds,
    }, indent=2) + "\n")
    for scale in SCALES:
        dataset = datasets[work["datasets"][scale]]
        input_file = output_dir.parent / f"steps-{scale}.csv"
        for mode in MODES:
            warmups = 0 if mode == "cold" else config["measurement"]["warmups"]
            count = config["measurement"][mode + "_samples_per_process"]
            diag = {}
            for side in SIDES:
                if side == "eyes4s":
                    label = f"diagnostic-{scale}-{mode}"
                    cmd = command(side, input_file, output_dir / f"{label}.csv",
                                  warmups, count, classpath, args.python, diagnostic=True)
                    observed = run_child(cmd, output_dir, label, env,
                                         config["measurement"]["timeout_seconds"])
                    if observed["status"] != "ok":
                        raise RuntimeError(f"diagnostic failed: {scale}/{mode}: {observed['failure']}")
                    values = json.loads(observed["stdout"])["diagnostic"]
                    digest = sha(observed["stdout"] + observed["stderr"])
                else:
                    values, digest = None, sha(b"Python native counters unavailable")
                diag[side] = diagnostics(side, values, digest)
            for round_id in range(args.rounds):
                if args.rounds == config["measurement"]["rounds"]:
                    check_interference(output_dir, scale, mode, round_id, "before-pair")
                order = SIDES if round_id % 2 == 0 else SIDES[::-1]
                pair = []
                for position, side in enumerate(order):
                    item = sample(config, work, dataset, side, scale, mode,
                                  round_id, position, output_dir, classpath,
                                  args.python, env, diag[side])
                    report["samples"].append(item)
                    pair.append(item)
                    with (output_dir / "samples.jsonl").open("a") as stream:
                        stream.write(json.dumps(item, sort_keys=True) + "\n")
                    if item["status"] != "ok":
                        raise RuntimeError(f"failed child: {scale}/{mode}/{round_id}/{side}: {item['failure']}")
                    with (output_dir / "throughput.jsonl").open("a") as stream:
                        stream.write(json.dumps(throughput(item, dataset), sort_keys=True) + "\n")
                    if args.rounds == config["measurement"]["rounds"]:
                        check_interference(output_dir, scale, mode, round_id,
                                           f"after-{side}")
                require_pair(pair, scale, mode, round_id)
                print(f"{scale} {mode} round {round_id + 1}/{args.rounds}: matched", flush=True)
    if args.rounds == config["measurement"]["rounds"]:
        performance.validate_receipt(report, config)
        (output_dir / "receipt.json").write_text(json.dumps(report, indent=2) + "\n")
    else:
        (output_dir / "exploratory.json").write_text(json.dumps(report, indent=2) + "\n")


if __name__ == "__main__":
    main()

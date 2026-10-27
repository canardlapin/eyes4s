#!/usr/bin/env python3
"""Opt-in production-host validation of explicitly prepared, external fixation studies."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import sys
import time

from run import ROOT, digest, implementation, outside_git

SUITE = "eyes4s.studio.desktop.journey.PrivateProductionFxSuite"
API = "eyes4s.studio.desktop.journey.PrivateProductionApiCli"
INPUTS = ("request.json", "fixations.csv", "trials.csv")


def classpath(path):
    value = path.read_text().strip()
    entries = value.split(os.pathsep)
    if not value or not all(Path(p).exists() for p in entries):
        raise ValueError("Missing classpath input")
    if any(Path(p).is_dir() and not Path(p).resolve().is_relative_to(ROOT) for p in entries):
        raise ValueError("Class directories must belong to this owned checkout")
    return value


def build(root):
    env = dict(os.environ, GITHUB_ACTIONS="true")
    with (root / "build.log").open("x") as log:
        subprocess.run(["sbt", "-batch", "-J-Xmx4g", "studioDesktop/Test/compile",
                        "export studioDesktop/Test/fullClasspath"], cwd=ROOT, env=env,
                       stdout=log, stderr=subprocess.STDOUT, check=True)
    lines = (root / "build.log").read_text().splitlines()
    value = next(line for line in reversed(lines) if line.startswith("/") and ".jar" in line)
    path = root / "classpath.txt"
    path.write_text(value + "\n")
    return classpath(path)


def execute(root, cp, stage, java, heap):
    env = dict(os.environ, EYES4S_PRIVATE_RUN=str(root))
    for key in ("CI", "GITHUB_ACTIONS", "EYES4S_PRIVATE_REOPEN", "JAVA_TOOL_OPTIONS",
                "JDK_JAVA_OPTIONS", "_JAVA_OPTIONS", "EYES4S_STUDIO_FX_VISIBLE"):
        env.pop(key, None)
    flags = []
    target = API
    if stage != "api":
        target = SUITE
        snapshots = root / (stage + "-snapshots")
        snapshots.mkdir()
        flags = ["-Deyes4s.studio.fx.headless=true", "-Dglass.platform=Monocle",
                 "-Dmonocle.platform=Headless", "-Dheadless.geometry=1920x1200-32",
                 "-Dprism.order=sw", "-Dprism.lcdtext=false", "-Djava.awt.headless=true",
                 f"-Deyes4s.studio.snapshots={snapshots}"]
        if stage == "reopen":
            env["EYES4S_PRIVATE_REOPEN"] = "1"
    command = [java, f"-Xmx{heap}", "-XX:ActiveProcessorCount=4", *flags, "-cp", cp]
    command += [target] if stage == "api" else ["org.junit.runner.JUnitCore", target]
    started = time.monotonic()
    state = {"status": "running", "stage": stage, "implementation": implementation(),
             "inputs": {name: digest(root / name) for name in INPUTS}}
    path = root / (stage + "-process.json")
    if path.exists():
        raise ValueError("This stage already has evidence; use a new run directory")
    with (root / (stage + ".log")).open("x") as log:
        process = subprocess.Popen(command, cwd=root, env=env, stdin=subprocess.DEVNULL,
                                   stdout=log, stderr=subprocess.STDOUT)
        state["pid"] = process.pid
        path.write_text(json.dumps(state, indent=2) + "\n")
        code = process.wait()
    state.update(status="passed" if code == 0 else "failed", exit=code,
                 elapsed_seconds=time.monotonic() - started)
    path.write_text(json.dumps(state, indent=2) + "\n")
    if implementation() != state["implementation"] or any(
            digest(root / name) != value for name, value in state["inputs"].items()):
        raise ValueError("Source or inputs changed during execution")
    if code:
        raise ValueError(f"{stage} failed; retained its process receipt and log")
    result_name = {"api": "api-result.json", "ui": "ui-result.json",
                   "reopen": "reopen-result.json"}[stage]
    result = json.loads((root / result_name).read_text())
    if result["status"] != "passed":
        raise ValueError("Missing passing result receipt")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--run-dir", type=Path, required=True,
                        help="existing external directory with the three explicit inputs")
    parser.add_argument("--stage", choices=("all", "api", "ui", "reopen"), default="all")
    parser.add_argument("--classpath-file", type=Path)
    parser.add_argument("--java", default="java")
    parser.add_argument("--heap", default="12g")
    args = parser.parse_args()
    os.umask(0o077)
    if os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"):
        raise ValueError("Private workflows are disabled in CI")
    root = outside_git(args.run_dir)
    if not root.is_dir() or any(not (root / name).is_file() for name in INPUTS):
        raise ValueError("An explicit external input directory is required")
    cp = classpath(args.classpath_file) if args.classpath_file else build(root)
    stages = ("api", "ui", "reopen") if args.stage == "all" else (args.stage,)
    results = {stage: execute(root, cp, stage, args.java, args.heap) for stage in stages}
    if args.stage == "all":
        if results["ui"]["pid"] == results["reopen"]["pid"]:
            raise ValueError("Reopen must use a separate JVM")
        if not (results["api"]["result_sha256"] == results["ui"]["result_sha256"] ==
                results["reopen"]["result_sha256"]):
            raise ValueError("API/window/reopen scientific identities differ")
        (root / "production.json").write_text(json.dumps(
            {"status": "passed", "results": results, "implementation": implementation()},
            indent=2) + "\n")
    print(f"Passed {args.stage}; private evidence retained in {root}")


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, subprocess.SubprocessError) as error:
        print(str(error), file=sys.stderr)
        sys.exit(1)

#!/usr/bin/env python3
"""Measure executable, explicitly scoped Scala.js probes; no size thresholds."""
import argparse
import gzip
import hashlib
import json
import platform
import zlib
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
PROBES = {"codec": "codecProbe", "plan": "planProbe", "fs2": "fs2Probe"}
MODES = {"fast": ("fastLinkJS", "fastopt"), "full": ("fullLinkJS", "opt")}
MODULES = ("kernel", "core", "detect", "surface", "aoi", "compare", "design", "plan", "results", "codec", "laws", "fs2", "io")


def output(*args, cwd=ROOT):
    return subprocess.check_output(args, cwd=cwd, text=True).strip()


def input_hash():
    paths = set(ROOT.glob("*.sbt"))
    paths.update((ROOT / "project").glob("*.scala"))
    paths.update((ROOT / "project").glob("*.sbt"))
    paths.add(ROOT / "project/build.properties")
    paths.add(ROOT / ".jvmopts")
    paths.add(ROOT / "studio/pins.properties")
    for module in MODULES:
        paths.update((ROOT / module / "src/main").rglob("*.scala"))
    for path in HERE.rglob("*"):
        if path.is_file() and not any(part in {"target", ".bsp", ".metals", "__pycache__"} for part in path.relative_to(HERE).parts):
            paths.add(path)
    digest = hashlib.sha256()
    for path in sorted(paths):
        digest.update(str(path.relative_to(ROOT)).encode() + b"\0" + path.read_bytes() + b"\0")
    return digest.hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--report-dir", type=Path, default=ROOT / "target/js-bundle-sizes")
    args = parser.parse_args()
    destination = args.report_dir.resolve()
    destination.mkdir(parents=True, exist_ok=True)
    for name in ("report.json", "report.md"):
        (destination / name).unlink(missing_ok=True)
    before = input_hash()
    with (destination / "prepare.log").open("w") as stream:
        subprocess.run(["sbt", "-batch", "-J-Xmx3g", "-J-XX:ActiveProcessorCount=4", "bundleProbeClasspaths"], cwd=ROOT, stdout=stream, stderr=subprocess.STDOUT, check=True)
    tasks = [f"{project}/Compile/{task}" for project in PROBES.values() for task, _ in MODES.values()] + ["probeToolchain"]
    log = destination / "link.log"
    with log.open("w") as stream:
        subprocess.run(["sbt", "-batch", "-J-Xmx3g", "-J-XX:ActiveProcessorCount=4", *tasks], cwd=HERE, stdout=stream, stderr=subprocess.STDOUT, check=True)
    def properties(path):
        return dict(line.split("=", 1) for line in path.read_text().splitlines() if line)
    library_versions = properties(ROOT / "target/js-bundle-sizes/classpaths/toolchain.properties")
    probe_versions = properties(ROOT / "target/js-bundle-sizes/probe-toolchain.properties")
    for module in PROBES:
        if library_versions[module + ".scala"] != probe_versions[module + ".scala"]:
            raise RuntimeError(f"Library/probe Scala versions differ for {module}")
    if library_versions["scalaJS"] != probe_versions["scalaJS"]:
        raise RuntimeError("Library/probe Scala.js versions differ")
    rows = []
    for module, project in PROBES.items():
        for mode, (_, suffix) in MODES.items():
            matches = list((HERE / module / "target").glob(f"scala-*/*-{suffix}"))
            if len(matches) != 1:
                raise RuntimeError(f"Expected one {module} {mode} output directory, found {matches}")
            directory = matches[0]
            scripts = sorted(p for p in directory.rglob("*") if p.suffix in {".js", ".mjs"})
            if not scripts or not (directory / "main.js").is_file():
                raise RuntimeError(f"Missing executable JS output in {directory}")
            smoke = output("node", str(directory / "main.js"), cwd=directory)
            valid = (json.loads(smoke).get("schema", {}).get("name") == "probe.frame" if module == "codec"
                     else smoke.startswith("plan: ") if module == "plan" else smoke.startswith("Completed("))
            if not valid:
                raise RuntimeError(f"Unexpected {module}/{mode} smoke result: {smoke}")
            files = [{"path": str(p.relative_to(directory)), "bytes": p.stat().st_size,
                      "gzipBytes": len(gzip.compress(p.read_bytes(), compresslevel=9, mtime=0)),
                      "sha256": hashlib.sha256(p.read_bytes()).hexdigest()} for p in scripts]
            rows.append({"module": module, "mode": mode, "files": files,
                         "bytes": sum(f["bytes"] for f in files),
                         "gzipBytes": sum(f["gzipBytes"] for f in files),
                         "smoke": "passed"})
    if input_hash() != before:
        raise RuntimeError("Source/build/probe inputs changed during measurement; rerun on fixed inputs")
    report = {"schemaVersion": 1, "commit": output("git", "rev-parse", "HEAD"),
              "tree": output("git", "rev-parse", "HEAD^{tree}"),
              "worktreeDirty": bool(output("git", "status", "--porcelain")),
              "inputsSha256": before, "libraryToolchain": library_versions, "probeToolchain": probe_versions,
              "nodeVersion": output("node", "--version"),
              "sbtRuntime": next(line.strip() for line in log.read_text().splitlines() if "welcome to sbt" in line),
              "platform": platform.platform(), "machine": platform.machine(),
              "pythonVersion": platform.python_version(), "zlibVersion": zlib.ZLIB_VERSION,
              "gzipCompressLevel": 9,
              "sourceMaps": probe_versions["sourceMaps"] == "true", "gzip": "sum of independently compressed JS files, mtime=0",
              "thresholds": None, "measurements": rows}
    (destination / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    table = ["# Scala.js probe sizes", "", "These are retained-code measurements of the documented probes, not whole-module sizes.", "",
             "| Module | Link | JS bytes | Gzip bytes | Smoke |", "|---|---|---:|---:|---|"]
    table += [f"| {r['module']} | {r['mode']} | {r['bytes']} | {r['gzipBytes']} | {r['smoke']} |" for r in rows]
    (destination / "report.md").write_text("\n".join(table) + "\n")
    print(f"Measured six executable bundles; reports and link log: {destination}")


if __name__ == "__main__":
    main()

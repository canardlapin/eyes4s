#!/usr/bin/env python3
"""Publish a local development version and test an isolated artifact consumer."""
import argparse
import hashlib
import json
import math
from pathlib import Path
import shutil
import subprocess
import tempfile
import zipfile

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
VERSION = "0.0.0-workflow-slices"
SBT = ["sbt", "-J-Xmx3g", "-J-XX:ActiveProcessorCount=6"]


def run(cwd, log, *commands):
    with log.open("w") as out:
        subprocess.run([*SBT, *commands], cwd=cwd, stdout=out, stderr=subprocess.STDOUT, check=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--skip-publish", action="store_true", help="Use artifacts already published by this script")
    args = parser.parse_args()
    candidate = Path(tempfile.mkdtemp(prefix="eyes4s-study-consumer-"))
    print(f"Isolated consumer and logs: {candidate}", flush=True)
    shutil.copy2(HERE / "build.sbt", candidate / "build.sbt")
    shutil.copy2(HERE / ".scalafmt.conf", candidate / ".scalafmt.conf")
    (candidate / "project").mkdir()
    for name in ("build.properties", "plugins.sbt"):
        shutil.copy2(HERE / "project" / name, candidate / "project" / name)
    shutil.copytree(HERE / "src", candidate / "src")
    if not args.skip_publish:
        run(REPO, candidate / "publish.log", f'set ThisBuild / version := "{VERSION}"', "publishLocal")
    artifacts = {}
    for module in ("kernel", "core", "detect", "surface", "aoi", "compare", "design", "plan", "codec", "laws", "fs2", "io"):
        for suffix in ("_3", "_sjs1_3"):
            name = f"eyes4s-{module}{suffix}"
            cache = Path.home() / ".ivy2/local/io.github.canardlapin" / name / VERSION
            jar = cache / "jars" / f"{name}.jar"
            artifacts[name] = hashlib.sha256(jar.read_bytes()).hexdigest()
            with zipfile.ZipFile(cache / "srcs" / f"{name}-sources.jar") as sources:
                source_root = REPO / module / "src/main/scala"
                for source in source_root.rglob("*.scala"):
                    if sources.read(str(source.relative_to(source_root))) != source.read_bytes():
                        raise RuntimeError(f"Packaged source is stale: {source}; rerun without --skip-publish")
    run(candidate, candidate / "consumer.log", "scalafmtCheckAll", "scalafmtSbtCheck", "consumerJVM/test", "consumerJS/test",
        "show consumerJVM/Compile/fullClasspath", "show consumerJS/Compile/fullClasspath")
    log = (candidate / "consumer.log").read_text()
    if str(REPO) in log:
        raise RuntimeError("Consumer output unexpectedly refers to the library source checkout")
    if f"eyes4s-io_3/{VERSION}/jars/eyes4s-io_3.jar" not in log:
        raise RuntimeError("Consumer classpath did not demonstrate the local JVM artifact")
    if f"eyes4s-io_sjs1_3/{VERSION}/jars/eyes4s-io_sjs1_3.jar" not in log:
        raise RuntimeError("Consumer classpath did not demonstrate the local Scala.js artifact")
    marker = "EYES4S_CROSS_RUNTIME="
    evidence = [json.loads(line.split(marker, 1)[1]) for line in log.splitlines() if marker in line]
    if len(evidence) != 2:
        raise RuntimeError(f"Expected actual JVM and Scala.js results, found {len(evidence)}")
    left, right = evidence
    for exact in ("plan", "input", "binned_bits"):
        if left[exact] != right[exact]:
            raise RuntimeError(f"JVM/Scala.js exact disagreement in {exact}")
    if len(left["gaussian"]) != 18 or len(right["gaussian"]) != 18:
        raise RuntimeError("Missing Gaussian contrasts in cross-runtime evidence")
    if not all(math.isclose(a, b, abs_tol=1e-12, rel_tol=0) for a, b in zip(left["gaussian"], right["gaussian"])):
        raise RuntimeError("JVM/Scala.js Gaussian contrast disagreement")
    detector_marker = "EYES4S_DETECTOR_RUNTIME="
    detector_evidence = [json.loads(line.split(detector_marker, 1)[1])
                         for line in log.splitlines() if detector_marker in line]
    if len(detector_evidence) != 2:
        raise RuntimeError(f"Expected actual JVM and Scala.js detector results, found {len(detector_evidence)}")
    if detector_evidence[0] != detector_evidence[1]:
        raise RuntimeError("JVM/Scala.js detector registration or execution disagreement")
    receipt = {
        "artifact_version": VERSION,
        "artifacts_sha256": artifacts,
        "cross_runtime": {"exact": ["plan", "input", "binned_bits"], "gaussian_absolute_tolerance": 1e-12},
        "runtime_evidence": evidence,
        "detector_runtime_evidence": detector_evidence,
        "consumer_directory": str(candidate),
        "tests": ["consumerJVM/test", "consumerJS/test"],
        "sources_sha256": {str(p.relative_to(candidate)): hashlib.sha256(p.read_bytes()).hexdigest()
                           for p in sorted((candidate / "src").rglob("*.scala"))},
    }
    (candidate / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print("Both artifact-consumer suites passed; receipt.json records the tested sources.", flush=True)


if __name__ == "__main__":
    main()

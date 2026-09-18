#!/usr/bin/env python3
"""Publish a local development version and test an isolated artifact consumer."""
import argparse
import csv
from decimal import Decimal
from fractions import Fraction
import hashlib
import json
import math
import os
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
        subprocess.run(
            [*SBT, *commands], cwd=cwd, stdout=out, stderr=subprocess.STDOUT, check=True
        )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--skip-publish",
        action="store_true",
        help="Use artifacts already published by this script",
    )
    args = parser.parse_args()
    candidate = Path(tempfile.mkdtemp(prefix="eyes4s-study-consumer-"))
    print(f"Isolated consumer and logs: {candidate}", flush=True)
    shutil.copy2(HERE / "build.sbt", candidate / "build.sbt")
    shutil.copy2(HERE / ".scalafmt.conf", candidate / ".scalafmt.conf")
    (candidate / "project").mkdir()
    for name in ("build.properties", "plugins.sbt"):
        shutil.copy2(HERE / "project" / name, candidate / "project" / name)
    shutil.copytree(HERE / "src", candidate / "src")
    shutil.copytree(HERE / ".jvm" / "src", candidate / ".jvm" / "src")
    if not args.skip_publish:
        run(
            REPO,
            candidate / "publish.log",
            f'set ThisBuild / version := "{VERSION}"',
            "publishLocal",
        )
    artifacts = {}
    for module in (
        "kernel",
        "core",
        "detect",
        "surface",
        "aoi",
        "compare",
        "design",
        "plan",
        "codec",
        "laws",
        "fs2",
        "io",
    ):
        for suffix in ("_3", "_sjs1_3"):
            name = f"eyes4s-{module}{suffix}"
            cache = Path.home() / ".ivy2/local/io.github.canardlapin" / name / VERSION
            jar = cache / "jars" / f"{name}.jar"
            artifacts[name] = hashlib.sha256(jar.read_bytes()).hexdigest()
            with zipfile.ZipFile(cache / "srcs" / f"{name}-sources.jar") as sources:
                source_root = REPO / module / "src/main/scala"
                for source in source_root.rglob("*.scala"):
                    if (
                        sources.read(str(source.relative_to(source_root)))
                        != source.read_bytes()
                    ):
                        raise RuntimeError(
                            f"Packaged source is stale: {source}; rerun without --skip-publish"
                        )
    run(
        candidate,
        candidate / "consumer.log",
        "scalafmtCheckAll",
        "scalafmtSbtCheck",
        "consumerJVM/test",
        "consumerJS/test",
        "show consumerJVM/Compile/fullClasspath",
        "show consumerJS/Compile/fullClasspath",
    )
    log = (candidate / "consumer.log").read_text()
    if str(REPO) in log:
        raise RuntimeError(
            "Consumer output unexpectedly refers to the library source checkout"
        )
    if f"eyes4s-io_3/{VERSION}/jars/eyes4s-io_3.jar" not in log:
        raise RuntimeError(
            "Consumer classpath did not demonstrate the local JVM artifact"
        )
    if f"eyes4s-io_sjs1_3/{VERSION}/jars/eyes4s-io_sjs1_3.jar" not in log:
        raise RuntimeError(
            "Consumer classpath did not demonstrate the local Scala.js artifact"
        )
    marker = "EYES4S_CROSS_RUNTIME="
    evidence = [
        json.loads(line.split(marker, 1)[1])
        for line in log.splitlines()
        if marker in line
    ]
    if len(evidence) != 2:
        raise RuntimeError(
            f"Expected actual JVM and Scala.js results, found {len(evidence)}"
        )
    left, right = evidence
    for exact in ("plan", "input", "binned_bits"):
        if left[exact] != right[exact]:
            raise RuntimeError(f"JVM/Scala.js exact disagreement in {exact}")
    if len(left["gaussian"]) != 18 or len(right["gaussian"]) != 18:
        raise RuntimeError("Missing Gaussian contrasts in cross-runtime evidence")
    if not all(
        math.isclose(a, b, abs_tol=1e-12, rel_tol=0)
        for a, b in zip(left["gaussian"], right["gaussian"])
    ):
        raise RuntimeError("JVM/Scala.js Gaussian contrast disagreement")
    detector_marker = "EYES4S_DETECTOR_RUNTIME="
    detector_evidence = [
        json.loads(line.split(detector_marker, 1)[1])
        for line in log.splitlines()
        if detector_marker in line
    ]
    if len(detector_evidence) != 2:
        raise RuntimeError(
            f"Expected actual JVM and Scala.js detector results, found {len(detector_evidence)}"
        )
    if detector_evidence[0] != detector_evidence[1]:
        raise RuntimeError(
            "JVM/Scala.js detector registration or execution disagreement"
        )
    journey = check_journey(log)
    fresh = check_fresh_process(log)
    reader_classpath = check_reader_classpath(candidate, artifacts)
    receipt = {
        "artifact_version": VERSION,
        "artifacts_sha256": artifacts,
        "cross_runtime": {
            "exact": ["plan", "input", "binned_bits"],
            "gaussian_absolute_tolerance": 1e-12,
        },
        "runtime_evidence": evidence,
        "detector_runtime_evidence": detector_evidence,
        "fixation_journey": {
            "exact_across_runtimes": [
                "input",
                "late",
                "plan",
                "binned bits",
                "segments",
                "cancelled",
                "fingerprint_size",
            ],
            "gaussian_absolute_tolerance": 1e-12,
            "oracles": [
                "tools/r-parity/fixtures/exact.json",
                "tools/r-parity/fixtures/multiscale.json",
            ],
            "runtime_evidence": journey,
        },
        "fresh_process_reader": {"receipts": fresh, "classpath": reader_classpath},
        "consumer_directory": str(candidate),
        "tests": ["consumerJVM/test", "consumerJS/test"],
        "sources_sha256": {
            str(p.relative_to(candidate)): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(
                [
                    *(candidate / "src").rglob("*.scala"),
                    *(candidate / ".jvm" / "src").rglob("*.scala"),
                ]
            )
        },
    }
    (candidate / "receipt.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print(
        "Both artifact-consumer suites passed; receipt.json records the tested sources.",
        flush=True,
    )


def evidence(log, marker):
    return [
        json.loads(line.split(marker, 1)[1])
        for line in log.splitlines()
        if marker in line
    ]


def check_journey(log):
    """Both routes on both runtimes: exact agreement and the independent oracles."""
    runs = evidence(log, "EYES4S_JOURNEY=")
    routes = sorted({run["route"] for run in runs})
    if routes != ["cosine", "scaled"] or len(runs) != 4:
        raise RuntimeError(
            f"Expected the cosine and scaled journeys on the JVM and Scala.js, found {len(runs)}"
        )
    exact = json.loads((REPO / "tools/r-parity/fixtures/exact.json").read_text())
    multiscale = json.loads(
        (REPO / "tools/r-parity/fixtures/multiscale.json").read_text()
    )
    rational = {r["id"]: Fraction(r["difference"]) for r in exact["reductions"]}
    means = {
        design: {r["id"]: Fraction(r[field]) for r in exact["reductions"]}
        for design, field in (("Matched", "matched"), ("Control", "control"))
    }
    with (REPO / "tools/r-parity/fixtures/matched-control.csv").open() as table:
        late = [
            str(int(row["onset_us"]) + 2**53 + 1)
            for row in csv.DictReader(table)
            if (row["participant"], row["image"], row["phase"]) == ("s2", "c", "recall")
        ]
    for route in routes:
        by_runtime = {run["runtime"]: run for run in runs if run["route"] == route}
        if sorted(by_runtime) != ["js", "jvm"]:
            raise RuntimeError(f"Expected one JVM and one Scala.js journey for {route}")
        left, right = by_runtime["jvm"], by_runtime["js"]
        for field in (
            "input",
            "late",
            "plan",
            "segments",
            "cancelled",
            "fingerprint_size",
            "multiplier",
        ):
            if left[field] != right[field]:
                raise RuntimeError(
                    f"JVM/Scala.js journey disagreement for {route} in {field}"
                )
        if [b["bits"] for b in left["binned"]] != [b["bits"] for b in right["binned"]]:
            raise RuntimeError(f"JVM/Scala.js binned bits disagree for {route}")
        if len(left["gaussian"]) != 18 or len(right["gaussian"]) != 18:
            raise RuntimeError(f"Missing Gaussian contrasts in the {route} journey")
        for a, b in zip(left["gaussian"], right["gaussian"]):
            if (a["sigma"], a["key"]) != (b["sigma"], b["key"]) or not math.isclose(
                a["value"], b["value"], abs_tol=1e-12, rel_tol=0
            ):
                raise RuntimeError(f"JVM/Scala.js Gaussian disagreement for {route}")
        m = Fraction(left["multiplier"])
        if left["late"] != late:
            raise RuntimeError(
                f"The {route} journey's reloaded 64-bit onsets differ from the table's"
            )
        for reduced in left["binned_reductions"]:
            target = means[reduced["design"]]
            if [r["key"] for r in reduced["values"]] != sorted(target):
                raise RuntimeError(f"Binned {route} reductions differ in keys from exact.json")
            for r in reduced["values"]:
                if not math.isclose(
                    r["value"], float(m * target[r["key"]]), abs_tol=1e-12, rel_tol=0
                ):
                    raise RuntimeError(
                        f"Binned {route} {reduced['design']} mean {r['key']} misses exact.json"
                    )
        if [b["key"] for b in left["binned"]] != sorted(rational):
            raise RuntimeError(
                f"Binned contrast keys differ from exact.json for {route}"
            )
        for row in left["binned"]:
            if not math.isclose(
                row["value"], float(m * rational[row["key"]]), abs_tol=1e-12, rel_tol=0
            ):
                raise RuntimeError(
                    f"Binned {route} contrast {row['key']} misses the rational oracle"
                )
        for row in left["gaussian"]:
            scale = next(
                s
                for s in multiscale["scales"]
                if Decimal(s["sigma"]) == Decimal(str(row["sigma"]))
            )
            target = Decimal(scale["differences"][row["key"]]) * Decimal(
                left["multiplier"]
            )
            if not math.isclose(row["value"], float(target), abs_tol=1e-12, rel_tol=0):
                raise RuntimeError(
                    f"Gaussian {route} contrast {row['key']} misses the decimal oracle"
                )
    return runs


def check_fresh_process(log):
    """The separate JVM reader reloaded and reran each route bit for bit."""
    receipts = evidence(log, "EYES4S_FRESH_PROCESS=")
    if sorted(r["route"] for r in receipts) != ["cosine", "scaled"]:
        raise RuntimeError(
            f"Expected fresh-process receipts for both routes, found {len(receipts)}"
        )
    for receipt in receipts:
        if (
            receipt["outcome"] != "completed"
            or receipt["process"] != receipt["launched"]
            or receipt["process"] == receipt["writer"]
        ):
            raise RuntimeError(
                f"The fresh reader did not reconstruct {receipt['route']} in its own process"
            )
        if (
            receipt["archived_bits"] != receipt["rerun_bits"]
            or not receipt["rerun_bits"]
        ):
            raise RuntimeError(
                f"The fresh reader's rerun of {receipt['route']} is not bit for bit"
            )
    return [
        {
            key: r[key]
            for key in (
                "route",
                "writer",
                "process",
                "address",
                "run",
                "input",
                "rerun_sha256",
                "steps",
                "total_units",
                "records",
            )
        }
        for r in receipts
    ]


def check_reader_classpath(candidate, artifacts):
    """Every entry of the fresh reader's classpath is this consumer's own output,
    a packaged eyes4s artifact published above, or a third-party jar; none is
    a path into the library checkout."""
    found = list(
        candidate.rglob("resource_managed/test/example/fresh-process/classpath.txt")
    )
    if len(found) != 1:
        raise RuntimeError(
            f"Expected one generated reader classpath, found {len(found)}"
        )
    ivy = Path.home() / ".ivy2/local/io.github.canardlapin"
    own, packaged, external = [], [], []
    for entry in found[0].read_text().split(os.pathsep):
        path = Path(entry).resolve()
        if path.is_relative_to(REPO.resolve()):
            raise RuntimeError(
                f"Reader classpath refers to the library checkout: {entry}"
            )
        if path.is_relative_to(candidate.resolve()):
            # The consumer's own output holds only its own package.
            foreign = [
                str(f.relative_to(path))
                for f in path.rglob("*")
                if f.is_file() and f.relative_to(path).parts[0] != "example"
            ]
            if not path.is_dir() or foreign:
                raise RuntimeError(f"Consumer output {entry} holds more than example/: {foreign}")
            own.append(str(path.relative_to(candidate.resolve())))
        elif path.name.startswith("eyes4s-"):
            name = path.name.removesuffix(".jar")
            if (
                path != (ivy / name / VERSION / "jars" / path.name).resolve()
                or name not in artifacts
            ):
                raise RuntimeError(
                    f"Reader classpath has an eyes4s entry that was not published here: {entry}"
                )
            if hashlib.sha256(path.read_bytes()).hexdigest() != artifacts[name]:
                raise RuntimeError(
                    f"Reader classpath jar differs from the published artifact: {entry}"
                )
            packaged.append(name)
        elif path.suffix == ".jar" and path.is_file():
            external.append(path.name)
        else:
            raise RuntimeError(f"Unexpected reader classpath entry: {entry}")
    expected = {
        f"eyes4s-{m}_3"
        for m in (
            "kernel",
            "core",
            "detect",
            "surface",
            "aoi",
            "compare",
            "design",
            "plan",
            "codec",
            "laws",
            "fs2",
            "io",
        )
    }
    if set(packaged) != expected:
        raise RuntimeError(
            f"Reader classpath packaged artifacts differ: {sorted(set(packaged) ^ expected)}"
        )
    return {
        "own": sorted(own),
        "packaged": sorted(packaged),
        "third_party": sorted(external),
    }


if __name__ == "__main__":
    main()

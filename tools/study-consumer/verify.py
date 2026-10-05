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
import platform
import re
import shutil
import subprocess
import zipfile

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
VERSION = "0.0.0-workflow-slices"
SBT = ["sbt", "-J-Xmx3g", "-J-XX:ActiveProcessorCount=6"]
MODULES = (
    "kernel", "core", "detect", "surface", "aoi", "compare", "design",
    "plan", "results", "codec", "laws", "fs2", "io",
)
# Independent oracles and pinned inputs the consumer's evidence is checked against.
FIXTURES = (
    "tools/r-parity/fixtures/exact.json",
    "tools/r-parity/fixtures/multiscale.json",
    "tools/r-parity/fixtures/matched-control.csv",
    "tools/r-parity/fixtures/temporal.json",
    "tools/r-parity/fixtures/temporal-study.csv",
    "tools/detector-conformance/reference.json",
    "tools/r-parity/fixtures/point-sampling.json",
)
# The facade journey's AdSERP excerpt and tidy export, as the library's own suite pins them.
FACADE_SOURCE_SHA256 = "57dc3230c1e2a701017011a9652e215fcb09b7f1b63a16fe00ca0ed1ca9639d0"
FACADE_CSV_SHA256 = "c9a2ec351a09d94533305ef2bdb7743ee4773f41a6c1d2d21c068a13927b9f76"
# Degrees: the recording route's angular warp is trigonometric.
ANGULAR_TOLERANCE = 1e-9
# The named tolerance of the Gaussian and temporal cosine oracles.
ORACLE_TOLERANCE = 1e-12
RECORDING_EXACT = [
    "input",
    "recording",
    "plan",
    "support",
    "labels",
    "areas",
    "segments",
    "cancelled",
    "fingerprint_size",
]
TEMPORAL_EXACT = [
    "base",
    "input",
    "plan",
    "ledgers",
    "segments",
    "cancelled",
    "fingerprint_size",
]
FRESH_ROUTES = ["recording-ivt", "recording-lab", "temporal-cosine", "temporal-scaled"]
ENVELOPE_WORKLOADS = {
    "study-journey-cosine",
    "study-journey-scaled",
    "recording-fixture-ivt",
    "recording-fixture-lab",
    "temporal-fixture-cosine",
    "temporal-fixture-scaled",
    "study-256x256-sigma32",
    "recording-10s",
}


# Under the build's target/ so an OS purge of temporary directories cannot delete the
# consumer and its receipt while check-docs.py still refers to them.
WORK_DIR = REPO / "target" / "study-consumer"
MARKER = ".eyes4s-study-consumer"


def prepare(candidate):
    """Replace a previous consumer build; refuse to delete a directory this script did not create."""
    if candidate.exists():
        if any(candidate.iterdir()) and not (candidate / MARKER).exists():
            raise SystemExit(f"Refusing to replace {candidate}: it is not a study-consumer build")
        shutil.rmtree(candidate)
    candidate.mkdir(parents=True)
    (candidate / MARKER).write_text("Created by tools/study-consumer/verify.py\n")
    return candidate


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
    parser.add_argument(
        "--work-dir",
        type=Path,
        default=WORK_DIR,
        help="Build directory for the isolated consumer (replaced on every run)",
    )
    args = parser.parse_args()
    candidate = prepare(args.work_dir.resolve())
    print(f"Isolated consumer and logs: {candidate}", flush=True)
    shutil.copy2(HERE / "build.sbt", candidate / "build.sbt")
    shutil.copy2(HERE / ".scalafmt.conf", candidate / ".scalafmt.conf")
    (candidate / "project").mkdir()
    for name in ("build.properties", "plugins.sbt"):
        shutil.copy2(HERE / "project" / name, candidate / "project" / name)
    shutil.copytree(HERE / "src", candidate / "src")
    shutil.copytree(HERE / ".jvm" / "src", candidate / ".jvm" / "src")
    if not args.skip_publish:
        # Separate commands keep Scaladoc invocations sequential: its signature
        # builder can fail when the root aggregate documents modules concurrently.
        publish = tuple(
            f'{"fs2Module" if module == "fs2" else module}{platform}/publishLocal'
            for module in MODULES
            for platform in ("JVM", "JS")
        )
        run(
            REPO,
            candidate / "publish.log",
            f'set ThisBuild / version := "{VERSION}"',
            *publish,
        )
    artifacts = {}
    for module in MODULES:
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
    # The consumer lives under REPO/target, so its own paths are removed first; any
    # remaining checkout path (library classes or sources) breaks isolation.
    if str(REPO) in log.replace(str(candidate), "<consumer>"):
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
    check_consumer_fixtures()
    recording = check_recording_journey(log)
    temporal = check_temporal_journey(log)
    fresh_routes = check_fresh_routes(log)
    facade = check_facade_journey(log)
    report = check_report_journey(log)
    repetition = check_repetition_journey(log)
    templates = check_template_journey(log)
    envelope = check_envelope(log)
    receipt = {
        "artifact_version": VERSION,
        "source_revision": source_revision(),
        "artifacts_sha256": artifacts,
        "fixtures_sha256": {
            name: hashlib.sha256((REPO / name).read_bytes()).hexdigest()
            for name in FIXTURES
        },
        "runtime": {
            "python": platform.python_version(),
            "platform": platform.platform(),
            "sbt": (HERE / "project/build.properties").read_text().strip(),
            "scala": re.search(
                r'scalaVersion := "([^"]+)"', (HERE / "build.sbt").read_text()
            ).group(1),
            "jvm": envelope["runtime"],
        },
        "gates": {
            "commands": [
                "publishLocal" if not args.skip_publish else "publishLocal (skipped)",
                "scalafmtCheckAll",
                "scalafmtSbtCheck",
                "consumerJVM/test",
                "consumerJS/test",
            ],
            "test_totals": test_totals(log),
            # The library's own gates (compileAll, testAll, checkBoundaries,
            # headerCheckAll, scalafmtCheckAll, scalafmtSbtCheck,
            # githubWorkflowCheck) run in the library build, not here.
            "not_run_here": [
                "library compileAll/testAll/checkBoundaries",
                "library headerCheckAll/scalafmtCheckAll/scalafmtSbtCheck/githubWorkflowCheck",
            ],
            "checks": [
                "consumer classpaths use only the packaged artifacts",
                "packaged sources equal the checkout",
                "fresh reader classpath: own classes, published jars, third-party jars",
                "transcribed tables equal the pinned fixture files",
                "fixation journey against exact.json and multiscale.json",
                "generate_multiscale.py --check and generate_temporal.py --check",
                "recording journey against the I-VT conformance fixture",
                "the fixture's fixations against the pymovements oracle events",
                "temporal journey against temporal.json targets and ledgers",
                "fresh-process reload and bit-for-bit rerun of every route",
                "facade journey: PsychologyWorkflow equals the explicit composition",
                "report journey: Report.evaluate equals the explicit reduction; exports",
                "repetition journey: RepetitionPlan and PointSamplingPlan against "
                "explicit compositions and point-sampling.json",
                "template journey: fits and decompositions equal the explicit kernels",
                "JVM and Scala.js agree exactly on portable evidence",
            ],
        },
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
        "recording_journey": {
            "exact_across_runtimes": RECORDING_EXACT + ["event kinds and microseconds"],
            "angular_absolute_tolerance": ANGULAR_TOLERANCE,
            "oracle": "tools/detector-conformance/reference.json#ivt-symmetric-central-boundaries",
            "runtime_evidence": recording,
        },
        "temporal_journey": {
            "exact_across_runtimes": TEMPORAL_EXACT + ["binned bits"],
            "gaussian_absolute_tolerance": ORACLE_TOLERANCE,
            "oracles": [
                "tools/r-parity/fixtures/temporal.json (targets)",
                "tools/r-parity/fixtures/temporal.json (ledgers)",
            ],
            "runtime_evidence": temporal,
        },
        "fresh_process_routes": fresh_routes,
        "facade_journey": {
            "exact_across_runtimes": ["source", "plan", "csv_sha256", "rows", "events"],
            "pinned": {"source_sha256": FACADE_SOURCE_SHA256, "csv_sha256": FACADE_CSV_SHA256},
            "runtime_evidence": facade,
        },
        "report_journey": {
            "exact_across_runtimes": ["report", "cells_sha256", "study_sha256", "contrast_bits"],
            "runtime_evidence": report,
        },
        "repetition_journey": {
            "exact_across_runtimes": [
                "plan",
                "edges",
                "matched",
                "controls",
                "point plan",
                "point values",
            ],
            "absolute_tolerance": ORACLE_TOLERANCE,
            "oracle": "tools/r-parity/fixtures/point-sampling.json",
            "runtime_evidence": repetition,
        },
        "template_journey": {
            "absolute_tolerance": ORACLE_TOLERANCE,
            "runtime_evidence": templates,
        },
        "response_envelope": envelope,
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


def source_revision():
    """The library checkout the artifacts were published from."""
    head = subprocess.run(
        ["git", "-C", str(REPO), "rev-parse", "HEAD"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    modified = subprocess.run(
        ["git", "-C", str(REPO), "status", "--porcelain", "--untracked-files=no"],
        capture_output=True,
        text=True,
        check=True,
    ).stdout.strip()
    return {"head": head, "modified_tracked_files": bool(modified)}


def test_totals(log):
    """The consumer's test totals, JVM first, as sbt reports them."""
    totals = [
        dict(zip(("total", "failed", "errors", "passed"), map(int, m.groups())))
        for m in re.finditer(
            r"Passed: Total (\d+), Failed (\d+), Errors (\d+), Passed (\d+)", log
        )
    ]
    if len(totals) != 2 or any(t["failed"] or t["errors"] for t in totals):
        raise RuntimeError(f"Expected two passing consumer test runs, found {totals}")
    return {"jvm": totals[0], "js": totals[1]}


def check_consumer_fixtures():
    """The consumer's transcribed tables are the pinned oracle inputs, verbatim."""
    for scala, source in (
        (
            "src/test/scala/example/ConsumerFixtures.scala",
            "tools/r-parity/fixtures/matched-control.csv",
        ),
        (
            "src/test/scala/example/TemporalConsumerFixtures.scala",
            "tools/r-parity/fixtures/temporal-study.csv",
        ),
    ):
        text = (HERE / scala).read_text()
        table = text.split('val csv = """', 1)[1].split('"""', 1)[0]
        if table != (REPO / source).read_text():
            raise RuntimeError(f"{scala} does not carry {source} verbatim")
    temporal = json.loads((REPO / "tools/r-parity/fixtures/temporal.json").read_text())
    table = (REPO / "tools/r-parity/fixtures/temporal-study.csv").read_bytes()
    if hashlib.sha256(table).hexdigest() != temporal["input_sha256"]:
        raise RuntimeError("temporal.json was not generated from temporal-study.csv")
    # The generators pin every embedded window, coverage, target and ledger too.
    # Transcribed verbatim from the library's own tests: the facade's AdSERP excerpt
    # and the generated point-sampling oracle.
    excerpt = (REPO / "io/src/test/scala/eyes4s/io/PsychologyWorkflowSuite.scala").read_text()
    excerpt = excerpt.split('private val publicTrial =', 1)[1].split('""".stripMargin', 1)[0]
    copy = (HERE / "src/test/scala/example/FacadeFixtures.scala").read_text()
    copy = copy.split("val csv: String =", 1)[1].split('""".stripMargin', 1)[0]
    if excerpt != copy:
        raise RuntimeError("FacadeFixtures.scala does not carry the library's AdSERP excerpt")
    reference = (REPO / "plan/src/test/scala/eyes4s/plan/PointSamplingReference.scala").read_text()
    oracle = (HERE / "src/test/scala/example/PointSamplingOracle.scala").read_text()
    body = lambda text, name: text.split(f"object {name}:", 1)[1]
    if body(reference, "PointSamplingReference") != body(oracle, "PointSamplingOracle"):
        raise RuntimeError("PointSamplingOracle.scala is not the generated point-sampling reference")
    for generator in ("generate_multiscale.py", "generate_temporal.py"):
        subprocess.run(
            ["python3", str(REPO / "tools/r-parity" / generator), "--check"],
            check=True,
            capture_output=True,
        )


def by_route_and_runtime(runs, routes, label):
    found = sorted((run["route"], run["runtime"]) for run in runs)
    expected = sorted((route, runtime) for route in routes for runtime in ("js", "jvm"))
    if found != expected:
        raise RuntimeError(f"Expected {label} evidence for {expected}, found {found}")
    return {
        route: {run["runtime"]: run for run in runs if run["route"] == route}
        for route in routes
    }


def check_recording_journey(log):
    """Both recording routes on both runtimes: exact agreement, and the
    pymovements I-VT oracle for every event's kind, span and places."""
    runs = evidence(log, "EYES4S_RECORDING_JOURNEY=")
    routes = by_route_and_runtime(runs, ("ivt", "lab"), "recording journey")
    reference = json.loads((REPO / "tools/detector-conformance/reference.json").read_text())
    fixture = next(
        f for f in reference["fixtures"] if f["id"] == "ivt-symmetric-central-boundaries"
    )
    expected = sorted(
        [
            (
                "fixation",
                f["onset_millis"] * 1000,
                f["offset_millis_exclusive"] * 1000,
                f["centre"],
            )
            for f in fixture["expected_eyes4s_fixations"]
        ]
        + [
            (
                "saccade",
                s["onset_millis"] * 1000,
                s["offset_millis_exclusive"] * 1000,
                [s["start"][0], s["end"][0]],
            )
            for s in fixture["expected_eyes4s_saccades"]
        ],
        key=lambda e: e[1],
    )
    period = fixture["period_millis"]
    support = [[e[1] // (1000 * period), e[2] // (1000 * period)] for e in expected]
    # The fixture's fixations are the pymovements oracle's, whose offsets are the
    # last classified sample: half-open support ends one period later.
    oracle = [
        (e["onset_millis"], e["offset_millis_inclusive"] + period)
        for e in fixture["oracle_events"]
        if e["name"] == "fixation"
    ]
    if oracle != [(e[1] // 1000, e[2] // 1000) for e in expected if e[0] == "fixation"]:
        raise RuntimeError("The fixture's fixations are not the pymovements oracle's")
    for route, by_runtime in routes.items():
        left, right = by_runtime["jvm"], by_runtime["js"]
        for field in RECORDING_EXACT:
            if left[field] != right[field]:
                raise RuntimeError(
                    f"JVM/Scala.js recording disagreement for {route} in {field}"
                )
        for run in (left, right):
            events = [
                (e["kind"], e["onsetMicros"], e["offsetMicros"], e["degrees"])
                for e in run["events"]
            ]
            if [e[:3] for e in events] != [e[:3] for e in expected]:
                raise RuntimeError(
                    f"The {route} events on {run['runtime']} differ from the pymovements oracle"
                )
            for found, target in zip(events, expected):
                if not all(
                    math.isclose(a, b, abs_tol=ANGULAR_TOLERANCE, rel_tol=0)
                    for a, b in zip(found[3], target[3])
                ):
                    raise RuntimeError(
                        f"The {route} event places on {run['runtime']} miss the oracle"
                    )
            if run["support"] != support:
                raise RuntimeError(f"The {route} event support differs from the oracle's samples")
    if routes["ivt"]["jvm"]["labels"] != routes["lab"]["jvm"]["labels"]:
        raise RuntimeError("The laboratory detector labelled samples unlike the shipped I-VT")
    return runs


def check_temporal_journey(log):
    """Both temporal routes on both runtimes: exact agreement, and the
    independent integer-overlap ledgers and 60-digit cosine targets."""
    runs = evidence(log, "EYES4S_TEMPORAL_JOURNEY=")
    routes = by_route_and_runtime(runs, ("cosine", "scaled"), "temporal journey")
    oracle = json.loads((REPO / "tools/r-parity/fixtures/temporal.json").read_text())
    targets = {
        (t["repetition"], t["key"], t["window"], t["sigma"]): t["difference"]
        for t in oracle["targets"]
    }
    ledgers = {
        (l["key"], l["window"]): [l["retained"], l["observed"], l["missing"]]
        for l in oracle["ledgers"]
    }
    for route, by_runtime in routes.items():
        left, right = by_runtime["jvm"], by_runtime["js"]
        for field in TEMPORAL_EXACT:
            if left[field] != right[field]:
                raise RuntimeError(f"JVM/Scala.js temporal disagreement for {route} in {field}")
        if len(left["contrasts"]) != len(right["contrasts"]):
            raise RuntimeError(f"JVM/Scala.js temporal contrasts differ in number for {route}")
        for a, b in zip(left["contrasts"], right["contrasts"]):
            place = ("repetition", "key", "window", "sigma")
            if [a[k] for k in place] != [b[k] for k in place] or (a["value"] is None) != (
                b["value"] is None
            ):
                raise RuntimeError(f"JVM/Scala.js temporal contrasts disagree for {route}")
            if a["sigma"] is None and a["bits"] != b["bits"]:
                raise RuntimeError(f"JVM/Scala.js binned temporal bits disagree for {route}")
            if a["value"] is not None and not math.isclose(
                a["value"], b["value"], abs_tol=ORACLE_TOLERANCE, rel_tol=0
            ):
                raise RuntimeError(f"JVM/Scala.js Gaussian temporal contrasts disagree for {route}")
        multiplier = Decimal(str(left["multiplier"]))
        for run in (left, right):
            found = {
                (c["repetition"], c["key"], c["window"], c["sigma"]): c["value"]
                for c in run["contrasts"]
            }
            if set(found) != set(targets) or len(run["contrasts"]) != len(targets):
                raise RuntimeError(f"The {route} temporal contrasts differ in keys from temporal.json")
            for at, target in targets.items():
                value = found[at]
                if target is None or value is None:
                    if (target is None) != (value is None):
                        raise RuntimeError(
                            f"Temporal {route} contrast {at} fails where the oracle does not"
                        )
                elif not math.isclose(
                    value,
                    float(Decimal(target) * multiplier),
                    abs_tol=ORACLE_TOLERANCE,
                    rel_tol=0,
                ):
                    raise RuntimeError(f"Temporal {route} contrast {at} misses the decimal oracle")
            printed = {
                (l["key"], l["window"]): [l["retained"], l["observed"], l["missing"]]
                for l in run["ledgers"]
            }
            if printed != ledgers:
                raise RuntimeError(f"Temporal {route} occupancy ledgers differ from temporal.json")
    return runs


def check_fresh_routes(log):
    """A separate JVM reloaded and reran every recording and temporal route bit for bit."""
    receipts = evidence(log, "EYES4S_FRESH_ROUTE=")
    if sorted(r["route"] for r in receipts) != FRESH_ROUTES:
        raise RuntimeError(
            f"Expected fresh-process receipts for {FRESH_ROUTES}, found {len(receipts)}"
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
        if receipt["archived_bits"] != receipt["rerun_bits"] or not receipt["rerun_bits"]:
            raise RuntimeError(f"The fresh reader's rerun of {receipt['route']} is not bit for bit")
    keys = ("route", "writer", "process", "address", "run", "input", "rerun_sha256")
    return [
        {**{key: r[key] for key in keys}, "steps": r["steps"], "total_units": r["total_units"]}
        for r in receipts
    ]


def runtime_pair(log, marker, label):
    """One run of a journey on each runtime."""
    runs = evidence(log, marker)
    if sorted(r["runtime"] for r in runs) != ["js", "jvm"]:
        raise RuntimeError(f"Expected one JVM and one Scala.js {label}, found {len(runs)}")
    return {r["runtime"]: r for r in runs}


def same_except_runtime(pair, label):
    left, right = ({k: v for k, v in pair[r].items() if k != "runtime"} for r in ("jvm", "js"))
    if left != right:
        raise RuntimeError(f"JVM/Scala.js {label} disagreement")


def check_facade_journey(log):
    """PsychologyWorkflow on both runtimes, against the library suite's pinned digests."""
    pair = runtime_pair(log, "EYES4S_FACADE_JOURNEY=", "facade journey")
    same_except_runtime(pair, "facade journey")
    run = pair["jvm"]
    if run["source"] != FACADE_SOURCE_SHA256 or run["csv_sha256"] != FACADE_CSV_SHA256:
        raise RuntimeError("The facade journey's input or tidy export differs from the pinned digests")
    if (run["rows"], run["events"], run["projected"], run["interpolated"]) != (10, 9, 72, 18):
        raise RuntimeError("The facade journey's report counts differ from the library suite's")
    return [pair["jvm"], pair["js"]]


def check_report_journey(log):
    """Report.evaluate and the exports on both runtimes, exactly."""
    pair = runtime_pair(log, "EYES4S_REPORT_JOURNEY=", "report journey")
    same_except_runtime(pair, "report journey")
    return [pair["jvm"], pair["js"]]


def check_repetition_journey(log):
    """RepetitionPlan and PointSamplingPlan on both runtimes; point values against the oracle."""
    repetition = runtime_pair(log, "EYES4S_REPETITION_JOURNEY=", "repetition journey")
    points = runtime_pair(log, "EYES4S_POINT_JOURNEY=", "point-sampling journey")
    left, right = repetition["jvm"], repetition["js"]
    for exact in ("plan", "edges", "matched", "controls"):
        if left[exact] != right[exact]:
            raise RuntimeError(f"JVM/Scala.js repetition disagreement in {exact}")
    if len(left["differences"]) != len(right["differences"]) or not all(
        math.isclose(a, b, abs_tol=ORACLE_TOLERANCE, rel_tol=0)
        for a, b in zip(left["differences"], right["differences"])
    ):
        raise RuntimeError("JVM/Scala.js repetition contrasts disagree")
    same_except_runtime(points, "point-sampling journey")
    return {"repetition": [left, right], "points": [points["jvm"], points["js"]]}


def check_template_journey(log):
    """Template decompositions and partial associations agree across runtimes.

    Least squares uses hypot, whose last bit may differ between the JVM and
    Scala.js, so values agree within the oracle tolerance rather than bit for bit.
    """
    pair = runtime_pair(log, "EYES4S_TEMPLATE_JOURNEY=", "template journey")
    left, right = pair["jvm"], pair["js"]
    values = lambda run: [v for fit in run["decompositions"] for v in fit] + [
        run["pearson"],
        run["spearman"],
    ]
    a, b = values(left), values(right)
    if len(a) != len(b) or not all(
        math.isclose(x, y, abs_tol=ORACLE_TOLERANCE, rel_tol=0) for x, y in zip(a, b)
    ):
        raise RuntimeError("JVM/Scala.js template journey disagreement")
    return [left, right]


def check_envelope(log):
    """The consumer's JVM response-envelope smoke run: reported, never asserted."""
    runtime = evidence(log, "EYES4S_ENVELOPE_RUNTIME=")
    workloads = evidence(log, "EYES4S_ENVELOPE=")
    found = sorted(w["workload"] for w in workloads)
    if len(runtime) != 1 or found != sorted(ENVELOPE_WORKLOADS):
        raise RuntimeError(
            f"Expected one runtime and the workloads {sorted(ENVELOPE_WORKLOADS)}, found {found}"
        )
    return {"runtime": runtime[0], "workloads": workloads}


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
        # The consumer is built under REPO/target: its own output is checked first.
        if path.is_relative_to(REPO.resolve()) and not path.is_relative_to(
            candidate.resolve()
        ):
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
            "results",
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

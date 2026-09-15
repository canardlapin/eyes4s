#!/usr/bin/env python3
"""Validate the finite eyesim baseline inventory and its pinned evidence."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import re
import shlex
import subprocess


ROOT = Path(__file__).resolve().parents[2]
DEFAULT_MANIFEST = ROOT / "tools/r-parity/baseline.json"
EXPECTED_ROWS = [
    "fixation-data",
    "spatial-transforms",
    "scanpaths",
    "density",
    "fixation-sampling",
    "entropy-map-arithmetic",
    "map-fixation-comparison",
    "matched-controls",
    "repetition",
    "multiscale",
    "temporal",
    "template-models",
    "reuse-export",
]
EXPECTED_STATUSES = [
    "verified-equivalent",
    "verified-intentional-divergence",
    "implementation-gap",
]
WORKFLOW_ISSUE = "bd-01M214CX2CTADNR6Q9D032Y1FB"


def run(args: list[str], cwd: Path = ROOT) -> str:
    result = subprocess.run(args, cwd=cwd, check=True, text=True, capture_output=True)
    return result.stdout


def require(condition: bool, message: str, failures: list[str]) -> None:
    if not condition:
        failures.append(message)


def nonempty_strings(value: object) -> bool:
    return isinstance(value, list) and bool(value) and all(
        isinstance(item, str) and bool(item.strip()) for item in value
    )


def covers(required: str, actual: str) -> bool:
    return (
        required == actual
        or actual.startswith(required + ".")
        or required.startswith(actual + ".")
    )


def verify_manifest(manifest: dict, failures: list[str]) -> None:
    require(manifest.get("schema") == 1, "manifest schema must be 1", failures)
    require(
        manifest.get("status_values") == EXPECTED_STATUSES,
        "status_values must be the three frozen classifications in order",
        failures,
    )

    eyesim = manifest.get("eyesim", {})
    revision = eyesim.get("revision", "")
    require(
        isinstance(revision, str) and re.fullmatch(r"[0-9a-f]{40}", revision) is not None,
        "eyesim.revision must be a full lowercase Git SHA",
        failures,
    )
    require(bool(eyesim.get("version")), "eyesim.version is required", failures)
    require(bool(eyesim.get("license")), "eyesim.license is required", failures)
    require(
        nonempty_strings(eyesim.get("inventory_sources")),
        "eyesim.inventory_sources must name source files",
        failures,
    )

    rows = manifest.get("rows")
    require(isinstance(rows, list), "rows must be a list", failures)
    if not isinstance(rows, list):
        return
    row_ids = [row.get("id") for row in rows if isinstance(row, dict)]
    require(row_ids == EXPECTED_ROWS, "rows must contain the frozen 13-row baseline", failures)
    row_by_id = {row.get("id"): row for row in rows if isinstance(row, dict)}
    for row_id in EXPECTED_ROWS:
        row = row_by_id.get(row_id, {})
        require(bool(row.get("label")), f"row {row_id} needs a label", failures)
        require(
            nonempty_strings(row.get("required_entry_points")),
            f"row {row_id} needs required_entry_points",
            failures,
        )

    fixtures = manifest.get("fixtures")
    require(isinstance(fixtures, dict) and bool(fixtures), "fixtures must be non-empty", failures)
    if not isinstance(fixtures, dict):
        fixtures = {}
    for fixture_id, fixture in fixtures.items():
        if not isinstance(fixture, dict):
            failures.append(f"fixture {fixture_id} must be an object")
            continue
        path = ROOT / str(fixture.get("path", ""))
        require(path.is_file(), f"fixture {fixture_id} path does not exist: {path}", failures)
        digest = fixture.get("sha256", "")
        require(
            isinstance(digest, str) and re.fullmatch(r"[0-9a-f]{64}", digest) is not None,
            f"fixture {fixture_id} needs a full SHA-256 digest",
            failures,
        )
        if path.is_file() and isinstance(digest, str):
            actual = hashlib.sha256(path.read_bytes()).hexdigest()
            require(actual == digest, f"fixture {fixture_id} digest drift: {actual}", failures)
        require(bool(fixture.get("license")), f"fixture {fixture_id} needs a license", failures)
        require(bool(fixture.get("role")), f"fixture {fixture_id} needs a role", failures)

    regenerators = manifest.get("regeneration")
    require(
        isinstance(regenerators, dict) and bool(regenerators),
        "regeneration must be non-empty",
        failures,
    )
    if not isinstance(regenerators, dict):
        regenerators = {}
    for regen_id, regen in regenerators.items():
        if not isinstance(regen, dict):
            failures.append(f"regenerator {regen_id} must be an object")
            continue
        command = regen.get("command", "")
        require(bool(command), f"regenerator {regen_id} needs a command", failures)
        require(
            nonempty_strings(regen.get("outputs")),
            f"regenerator {regen_id} needs outputs",
            failures,
        )
        for output in regen.get("outputs", []):
            require(output in fixtures, f"regenerator {regen_id} names unknown fixture {output}", failures)
        if isinstance(command, str) and command:
            for token in shlex.split(command.replace("{eyesim}", "/tmp/eyesim")):
                if token.startswith("tools/"):
                    require((ROOT / token).is_file(), f"regenerator {regen_id} script is missing: {token}", failures)

    cases = manifest.get("cases")
    require(isinstance(cases, list) and bool(cases), "cases must be non-empty", failures)
    if not isinstance(cases, list):
        return
    case_ids: set[str] = set()
    cases_by_row: dict[str, list[dict]] = {row: [] for row in EXPECTED_ROWS}
    required_fields = [
        "id",
        "row",
        "estimand",
        "independent_case",
        "status",
    ]
    required_lists = [
        "eyesim_entry_points",
        "methods",
        "conventions",
        "backend_dependencies",
        "eyes4s_public_api",
        "fixture_inputs",
    ]
    for index, case in enumerate(cases):
        if not isinstance(case, dict):
            failures.append(f"case[{index}] must be an object")
            continue
        case_id = case.get("id", f"case[{index}]")
        require(case_id not in case_ids, f"duplicate case id {case_id}", failures)
        if isinstance(case_id, str):
            case_ids.add(case_id)
        for field in required_fields:
            require(bool(case.get(field)), f"case {case_id} needs {field}", failures)
        for field in required_lists:
            require(nonempty_strings(case.get(field)), f"case {case_id} needs {field}", failures)
        row_id = case.get("row")
        require(row_id in row_by_id, f"case {case_id} names unknown row {row_id}", failures)
        if row_id in cases_by_row:
            cases_by_row[row_id].append(case)
        for fixture_id in case.get("fixture_inputs", []):
            require(fixture_id in fixtures, f"case {case_id} names unknown fixture {fixture_id}", failures)
        status = case.get("status")
        require(status in EXPECTED_STATUSES, f"case {case_id} has invalid status {status}", failures)
        evidence = case.get("evidence")
        require(isinstance(evidence, list), f"case {case_id} evidence must be a list", failures)
        if isinstance(evidence, list):
            for path_text in evidence:
                require(
                    isinstance(path_text, str) and (ROOT / path_text).is_file(),
                    f"case {case_id} evidence path is missing: {path_text}",
                    failures,
                )
        if status == "implementation-gap":
            require(
                isinstance(case.get("gap_issue"), str) and bool(case.get("gap_issue")),
                f"gap case {case_id} needs one exact gap_issue",
                failures,
            )
            require(bool(case.get("planned_evidence")), f"gap case {case_id} needs planned_evidence", failures)
        else:
            require(case.get("gap_issue") is None, f"verified case {case_id} cannot name gap_issue", failures)
            require(bool(evidence), f"verified case {case_id} needs evidence", failures)
        if status == "verified-intentional-divergence":
            require(bool(case.get("divergence")), f"divergence case {case_id} needs a reason", failures)
        for regen_id in case.get("regeneration_ids", []):
            require(regen_id in regenerators, f"case {case_id} names unknown regenerator {regen_id}", failures)

    for row_id, row_cases in cases_by_row.items():
        require(bool(row_cases), f"row {row_id} has no classified cases", failures)
        actual = [entry for case in row_cases for entry in case.get("eyesim_entry_points", [])]
        for required in row_by_id.get(row_id, {}).get("required_entry_points", []):
            require(
                any(covers(required, entry) for entry in actual),
                f"row {row_id} does not classify required entry point {required}",
                failures,
            )


def verify_eyesim(manifest: dict, checkout: Path, failures: list[str]) -> None:
    revision = manifest["eyesim"]["revision"]
    try:
        run(["git", "cat-file", "-e", f"{revision}^{{commit}}"], checkout)
    except (subprocess.CalledProcessError, FileNotFoundError):
        failures.append(f"eyesim checkout does not contain pinned revision {revision}: {checkout}")
        return
    for source in manifest["eyesim"]["inventory_sources"]:
        try:
            run(["git", "show", f"{revision}:{source}"], checkout)
        except subprocess.CalledProcessError:
            failures.append(f"pinned eyesim source is missing: {source}")
    description = run(["git", "show", f"{revision}:DESCRIPTION"], checkout)
    version = re.search(r"^Version:\s*(\S+)", description, re.MULTILINE)
    require(
        version is not None and version.group(1) == manifest["eyesim"]["version"],
        "pinned eyesim DESCRIPTION version does not match the manifest",
        failures,
    )


def verify_mote(manifest: dict, failures: list[str]) -> None:
    gap_issues = sorted(
        {case["gap_issue"] for case in manifest["cases"] if case["status"] == "implementation-gap"}
    )
    for issue in gap_issues:
        try:
            output = run(["mote", "show", issue])
        except (subprocess.CalledProcessError, FileNotFoundError):
            failures.append(f"gap issue is missing from Mote: {issue}")
            continue
        status = re.search(r"^status:\s+(\S+)", output, re.MULTILINE)
        require(
            status is not None and status.group(1) in {"open", "doing"},
            f"gap issue {issue} must remain open or doing",
            failures,
        )
    try:
        workflow = run(["mote", "show", WORKFLOW_ISSUE])
        for issue in gap_issues:
            require(issue in workflow, f"workflow issue is not blocked by gap issue {issue}", failures)
    except (subprocess.CalledProcessError, FileNotFoundError):
        failures.append(f"workflow issue is missing from Mote: {WORKFLOW_ISSUE}")


def run_regeneration(manifest: dict, eyesim: Path | None) -> None:
    for regen_id, regen in manifest["regeneration"].items():
        if regen["requires_eyesim"] and eyesim is None:
            raise SystemExit(f"--run-regeneration requires --eyesim for {regen_id}")
        command = regen["command"]
        if eyesim is not None:
            command = command.replace("{eyesim}", str(eyesim))
        print(f"running {regen_id}: {command}")
        subprocess.run(shlex.split(command), cwd=ROOT, check=True)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
    parser.add_argument("--eyesim", type=Path, help="Git checkout containing the pinned eyesim object")
    parser.add_argument("--mote", action="store_true", help="also require every gap task to be live")
    parser.add_argument("--run-regeneration", action="store_true", help="run every implemented fixture check")
    args = parser.parse_args()

    manifest = json.loads(args.manifest.read_text())
    failures: list[str] = []
    verify_manifest(manifest, failures)
    if args.eyesim is not None:
        verify_eyesim(manifest, args.eyesim, failures)
    if args.mote:
        verify_mote(manifest, failures)
    if failures:
        raise SystemExit("Baseline contract failed:\n- " + "\n- ".join(failures))
    if args.run_regeneration:
        run_regeneration(manifest, args.eyesim)

    counts = {status: 0 for status in EXPECTED_STATUSES}
    for case in manifest["cases"]:
        counts[case["status"]] += 1
    print(
        "Baseline contract: "
        f"{len(manifest['rows'])} rows, {len(manifest['cases'])} cases; "
        + ", ".join(f"{status}={counts[status]}" for status in EXPECTED_STATUSES)
        + "."
    )


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Verify whole-board review evidence, hashes and successful native test bindings."""

import argparse
import hashlib
import json
from pathlib import Path
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[2]
BOARDS = {"System", "Main", "MainDark", "Data", "Analysis", "Explore", "Results", "Figures"}
CHECKS = {
    "System": {"tokens", "roles", "maps", "type-states", "architecture-keyboard"},
    "Data": {"shell", "freshness", "mapping", "identity", "ledger", "geometry", "assets"},
    "Explore": {"shell", "trail", "marks-preview", "timeline", "records", "inspector"},
    "Analysis": {"shell", "presets", "descriptors", "scales", "draft", "resolved", "preflight"},
    "Main": {"shell", "navigator", "panels", "ladder", "readout", "inspector", "theme"},
    "MainDark": {"shell", "navigator", "panels", "ladder", "readout", "inspector", "theme"},
    "Results": {"shell", "running", "participants", "paired", "profile-table", "reporting"},
    "Figures": {"shell", "binding", "panels", "stale", "methods", "bundle"},
}


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def checked_file(entry):
    path = ROOT / entry["path"]
    if not path.is_file() or sha(path) != entry["sha256"]:
        raise ValueError(f"Missing or changed review input: {path}")
    return path


def passed(binding):
    path = ROOT / binding["report"]
    cases = [c for c in ET.parse(path).getroot().iter("testcase")
             if c.get("name") == binding["case"]]
    if len(cases) != 1 or any(cases[0].find(t) is not None
                              for t in ("failure", "error", "skipped")):
        raise ValueError(f"No unique successful case: {binding}")
    return {**binding, "report_sha256": sha(path)}


def verify(review):
    if review.get("schema") != 1 or review.get("board") not in BOARDS:
        raise ValueError("Unknown board or review schema")
    if review.get("reviewed") is not True or not review.get("findings"):
        raise ValueError("Manual visual review and findings required")
    inputs = review["inputs"]
    if not inputs or not review["captures"] or not review["checks"]:
        raise ValueError("Sources, native captures and checklist checks required")
    for entry in inputs + review["captures"]:
        checked_file(entry)
    names = []
    for check in review["checks"]:
        names.append(check["id"])
        if check["disposition"] not in ("match", "accepted deviation", "documentation only"):
            raise ValueError(f"Unresolved disposition: {check}")
        if not check["evidence"]:
            raise ValueError(f"Missing evidence: {check}")
        if check["disposition"] == "accepted deviation" and not check.get("basis"):
            raise ValueError(f"Deviation needs an existing decision or precise rationale: {check}")
    if len(names) != len(set(names)):
        raise ValueError("Duplicate checklist IDs")
    if set(names) != CHECKS[review["board"]]:
        raise ValueError(f"Incomplete checklist for {review['board']}: {names}")
    reference = f"docs/studio/design/reference/{review['board']}-1x.png"
    if reference not in {entry["path"] for entry in inputs}:
        raise ValueError("Pinned board reference required")
    if review["board"] != "System":
        metrics = json.loads(checked_file(review["metrics"]).read_text())
        if (metrics["board"], metrics["width"], metrics["height"]) != (review["board"], 1440, 900):
            raise ValueError("Wrong native board or review size")
        for key, expected in (("appBar", 44), ("contextStrip", 32), ("statusBar", 24)):
            if abs(metrics["heights"][key] - expected) > 1:
                raise ValueError(f"Wrong native {key} height")
        if not metrics["tabHeaders"] or any(abs(h - 28) > 1 for h in metrics["tabHeaders"]):
            raise ValueError("Wrong native tab metrics")
    bindings = [passed(b) for b in review["tests"]]
    if not bindings:
        raise ValueError("Native test binding required")
    return {"board": review["board"], "checks": len(names), "tests": bindings}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    try:
        reviews = [json.loads(p.read_text()) for p in sorted(args.directory.glob("*/review.json"))]
        if {r["board"] for r in reviews} != BOARDS or len(reviews) != len(BOARDS):
            raise ValueError("Exactly one review for each of the eight remaining boards required")
        result = [verify(r) for r in reviews]
    except (OSError, ValueError, KeyError, ET.ParseError) as error:
        print(f"FAIL: {error}")
        return 1
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps({"schema": 1, "reviews": result}, indent=2) + "\n")
    print(f"PASS: {len(result)} board reviews; hashes and named native cases verified")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

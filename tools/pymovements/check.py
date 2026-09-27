#!/usr/bin/env python3
"""Check the comparison inventory; this does not execute or certify scientific work."""

import argparse
import hashlib
import json
from pathlib import Path, PurePosixPath
import re
import subprocess
import sys
import zipfile

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = Path("tools/pymovements/manifest.json")
DOC = Path("docs/plans/PYMOVEMENTS_COMPARISON.md")
START = "<!-- comparison-table:start -->"
END = "<!-- comparison-table:end -->"
REQUIRED = {
    "geometry": "PM2", "datasets": "PM1", "csv-asc": "PM1",
    "ipc-bids": "PM1", "preprocessing": "PM1", "resampling": "PM1",
    "ivt": "PM1", "idt": "PM1", "microsaccades": "PM1", "blink": "PM1",
    "ihmm": "PM1", "qc": "PM1", "reading": "PM1", "aoi": "PM1",
    "study": "PM2", "distributions": "PM1", "comparison": "PM1",
    "first-analysis": "PM2", "persistence": "PM2", "studio": "PM1",
    "scanpath-heatmap": "PM4", "trace-events": "PM4",
    "reading-qc-main-sequence": "PM4", "publication": "PM4",
    "performance": "PM3", "responsiveness": "PM3", "empirical": "PM5",
    "streaming": "PM3",
}
SUCCESS = {"workflow": "verified", "empirical": "qualified", "release": "available"}


def require(condition, message):
    if not condition:
        raise ValueError(message)


def fields(value, names, where):
    require(isinstance(value, dict) and set(value) == set(names.split()),
            f"{where}: expected fields {names}")


def string(value, where):
    require(isinstance(value, str) and bool(value.strip()), f"{where}: missing text")


def strings(value, where, empty=False):
    require(isinstance(value, list), f"{where}: expected list")
    require(empty or bool(value), f"{where}: empty list")
    for item in value:
        string(item, where)
    require(len(value) == len(set(value)), f"{where}: duplicate values")


def digest(value, size=64):
    require(isinstance(value, str) and re.fullmatch(f"[0-9a-f]{{{size}}}", value),
            f"invalid {size}-character digest: {value!r}")


def sha(data):
    return hashlib.sha256(data).hexdigest()


def relative(value):
    string(value, "path")
    path = PurePosixPath(value)
    require(not path.is_absolute() and ".." not in path.parts and "\\" not in value
            and str(path) == value and value != ".", f"unsafe path: {value}")
    return path


def local(root, value):
    path = root / relative(value)
    require(path.resolve().is_relative_to(root.resolve()), f"path escapes root: {value}")
    require(path.is_file(), f"missing file: {value}")
    return path


def unique_object(pairs):
    result = {}
    for key, value in pairs:
        require(key not in result, f"duplicate JSON key: {key}")
        result[key] = value
    return result


def read_json(path):
    return json.loads(path.read_text(encoding="utf-8"), object_pairs_hook=unique_object)


def issue(value):
    require(isinstance(value, str) and re.fullmatch(r"bd-[0-9A-Z]{26}", value),
            f"invalid Mote issue: {value!r}")


def protocol_digest(root, manifest):
    text = local(root, manifest["protocol"]).read_text(encoding="utf-8")
    require(text.count(START) == text.count(END) == 1 and text.index(START) < text.index(END),
            "invalid protocol table markers")
    before, rest = text.split(START)
    _, after = rest.split(END)
    # Inventory status changes must not invalidate the protocol they were judged against.
    return sha((before + START + "\n" + END + after).encode("utf-8"))


def receipt(root, ref, row, dimension, manifest):
    fields(ref, "path sha256", "receipt reference")
    digest(ref["sha256"])
    path = local(root, ref["path"])
    require(sha(path.read_bytes()) == ref["sha256"], "receipt digest changed")
    data = read_json(path)
    fields(data, "capability dimension outcome source_commit protocol_sha256 command runtime "
           "platforms inputs config_sha256 outputs artifacts", "receipt")
    require(data["capability"] == row["id"] and data["dimension"] == dimension,
            "receipt capability/dimension mismatch")
    require(data["outcome"] == "passed", "receipt did not pass")
    require(data["source_commit"] == manifest["eyes4s_commit"], "receipt source is stale")
    digest(data["protocol_sha256"])
    digest(data["config_sha256"])
    # The protocol hash binds the reviewed protocol bytes, not a mutable title or URL.
    require(data["protocol_sha256"] == protocol_digest(root, manifest),
            "receipt protocol is stale")
    for key in ("command", "runtime"):
        string(data[key], key)
    strings(data["platforms"], "receipt platforms")
    require(set(row["platforms"]) <= set(data["platforms"]), "receipt omits a platform")
    for key in ("inputs", "outputs", "artifacts"):
        entries = data[key]
        require(isinstance(entries, list), f"receipt {key}: expected list")
        require(bool(entries) or (key == "artifacts" and dimension != "release"),
                f"receipt {key}: empty")
        ids = []
        for entry in entries:
            fields(entry, "id sha256", f"receipt {key}")
            string(entry["id"], key)
            digest(entry["sha256"])
            ids.append(entry["id"])
        require(len(ids) == len(set(ids)), f"receipt {key}: duplicate ids")


def validate(manifest, root, wheel=None, issues=None):
    fields(manifest, "schema_version epic snapshot_date eyes4s_commit protocol comparator "
           "targets sources capabilities", "manifest")
    require(type(manifest["schema_version"]) is int and manifest["schema_version"] == 1,
            "unsupported schema version")
    issue(manifest["epic"])
    digest(manifest["eyes4s_commit"], 40)
    require(isinstance(manifest["snapshot_date"], str)
            and re.fullmatch(r"\d{4}-\d{2}-\d{2}", manifest["snapshot_date"]), "invalid date")
    local(root, manifest["protocol"])
    pin = manifest["comparator"]
    fields(pin, "package version source_commit source_url wheel_url wheel_filename "
           "wheel_sha256 published requires_python", "comparator")
    for key, value in pin.items():
        string(value, f"comparator {key}")
    require(pin["package"] == "pymovements", "wrong comparator")
    require(re.fullmatch(r"\d+\.\d+\.\d+", pin["version"]), "unversioned comparator")
    require(pin["wheel_filename"] == f"pymovements-{pin['version']}-py3-none-any.whl",
            "wheel version mismatch")
    require(pin["wheel_url"].startswith("https://files.pythonhosted.org/")
            and pin["wheel_url"].endswith("/" + pin["wheel_filename"]), "invalid wheel URL")
    digest(pin["wheel_sha256"])
    digest(pin["source_commit"], 40)
    require(pin["source_url"] == "https://github.com/pymovements/pymovements/tree/"
            + pin["source_commit"], "source URL is not pinned")
    targets = manifest["targets"]
    require(isinstance(targets, dict) and set(targets) == {"PM1", "PM2", "PM3", "PM4", "PM5"},
            "all five targets are required")
    owners = {manifest["epic"]}
    for target in targets.values():
        fields(target, "title issue", "target")
        string(target["title"], "target title")
        issue(target["issue"])
        owners.add(target["issue"])
    sources = manifest["sources"]
    require(isinstance(sources, dict) and sources, "missing source evidence")
    for name, source in sources.items():
        fields(source, "repository kind path sha256 anchors", f"source {name}")
        require(source["repository"] in ("eyes4s", "pymovements"), "unknown repository")
        require(source["kind"] in ("api", "test", "historical"), "unknown source kind")
        relative(source["path"])
        digest(source["sha256"])
        strings(source["anchors"], f"{name} anchors")
        if source["repository"] == "eyes4s":
            data = local(root, source["path"]).read_bytes()
            check_source(name, source, data)
        else:
            require(source["path"].startswith("pymovements/"), "invalid wheel member")
    rows = manifest["capabilities"]
    require(isinstance(rows, list) and rows, "missing capabilities")
    ids = []
    for row in rows:
        fields(row, "id title targets comparator implementation platforms conventions fixtures "
               "workflow empirical release gate gap owners", "capability")
        for key in ("id", "title", "conventions"):
            string(row[key], key)
        ids.append(row["id"])
        strings(row["targets"], "targets")
        require(set(row["targets"]) <= set(targets), "unknown target")
        strings(row["platforms"], "platforms")
        require(set(row["platforms"]) <= {"JVM", "JS", "desktop-JVM"}, "unknown platform")
        strings(row["owners"], "owners")
        for owner in row["owners"]:
            issue(owner)
            owners.add(owner)
        fields(row["comparator"], "apis note", "comparator coverage")
        fields(row["implementation"], "status apis note", "implementation")
        fields(row["fixtures"], "existing planned", "fixtures")
        impl = row["implementation"]
        require(impl["status"] in ("present", "partial", "absent"), "unknown implementation status")
        string(impl["note"], "implementation note")
        string(row["comparator"]["note"], "comparator note")
        string(row["fixtures"]["planned"], "planned fixture protocol")
        for refs, repo in ((impl["apis"], "eyes4s"), (row["comparator"]["apis"], "pymovements"),
                           (row["fixtures"]["existing"], "eyes4s")):
            strings(refs, "evidence references", empty=True)
            require(all(ref in sources and sources[ref]["repository"] == repo for ref in refs),
                    "unknown or wrong-repository evidence")
        require(impl["status"] == "absent" or bool(impl["apis"]), "implementation lacks evidence")
        require(all(sources[ref]["kind"] == "api" for ref in impl["apis"]),
                "implementation must cite API source")
        for dimension, success in SUCCESS.items():
            evidence = row[dimension]
            fields(evidence, "status note receipt", dimension)
            require(evidence["status"] in ("pending", success), f"unknown {dimension} status")
            string(evidence["note"], dimension)
            if evidence["status"] == "pending":
                require(evidence["receipt"] is None, f"pending {dimension} has a success receipt")
            else:
                require(impl["status"] != "absent", "absent capability cannot be qualified")
                receipt(root, evidence["receipt"], row, dimension, manifest)
                require(dimension == "workflow" or row["workflow"]["status"] == "verified",
                        "qualification/release needs verified workflow")
        require(row["gate"] in ("open", "accepted"), "unknown capability gate")
        if row["gate"] == "accepted":
            require(impl["status"] == "present" and row["gap"] == ""
                    and all(row[k]["status"] == v for k, v in SUCCESS.items()),
                    "contradictory completion claim")
        else:
            string(row["gap"], "open capability gap")
        if issues is not None:
            for owner in row["owners"]:
                require(owner in issues, f"missing Mote owner: {owner}")
                require(row["gate"] == "accepted" or issues[owner] != "closed",
                        f"open gap has closed owner: {row['id']} / {owner}; name a successor")
    require(len(ids) == len(set(ids)), "duplicate capability id")
    by_id = {row["id"]: row for row in rows}
    require(set(REQUIRED) <= set(ids), "missing required capability")
    for name, target in REQUIRED.items():
        require(target in by_id[name]["targets"], f"{name} omits required target {target}")
    if issues is not None:
        require(owners <= set(issues), "missing Mote epic or target")
        for key, target in targets.items():
            require(issues[target["issue"]] != "closed" or
                    all(row["gate"] == "accepted" for row in rows if key in row["targets"]),
                    f"closed {key} gate retains open capabilities")
        require(issues[manifest["epic"]] != "closed" or
                all(row["gate"] == "accepted" for row in rows), "closed epic retains open capabilities")
    if wheel is not None:
        require(sha(wheel.read_bytes()) == pin["wheel_sha256"], "wheel digest mismatch")
        with zipfile.ZipFile(wheel) as archive:
            names = archive.namelist()
            require(len(names) == len(set(names)), "duplicate wheel member")
            for name, source in sources.items():
                if source["repository"] == "pymovements":
                    require(source["path"] in names, f"missing wheel member: {source['path']}")
                    check_source(name, source, archive.read(source["path"]))


def check_source(name, source, data):
    require(sha(data) == source["sha256"], f"source changed: {name}")
    text = data.decode("utf-8")
    require(all(anchor in text for anchor in source["anchors"]), f"missing source anchor: {name}")


def render(manifest):
    def escape(text):
        return text.replace("|", "\\|").replace("\n", " ")
    lines = [START, "", "| Capability | Targets | Implementation | Workflow / empirical / release | Next owners |",
             "|---|---|---|---|---|"]
    for row in manifest["capabilities"]:
        cells = [f"`{row['id']}` — {row['title']}", ", ".join(row["targets"]),
                 row["implementation"]["status"],
                 " / ".join(row[k]["status"] for k in SUCCESS),
                 "<br>".join(f"`{owner}`" for owner in row["owners"])]
        lines.append("| " + " | ".join(escape(cell) for cell in cells) + " |")
    return "\n".join(lines) + "\n\n" + END


def table(root, manifest, write=False):
    path = local(root, str(DOC))
    text = path.read_text(encoding="utf-8")
    require(text.count(START) == text.count(END) == 1 and text.index(START) < text.index(END),
            "missing or repeated comparison table markers")
    before, rest = text.split(START)
    old, after = rest.split(END)
    expected = render(manifest)
    if write:
        path.write_text(before + expected + after, encoding="utf-8")
    else:
        require(START + old + END == expected, "stale comparison table; run --write-table")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--wheel", type=Path, help="verify the downloaded pinned wheel without executing it")
    parser.add_argument("--mote-store", type=Path, help="verify current gap owners and gate states")
    parser.add_argument("--write-table", action="store_true")
    args = parser.parse_args()
    try:
        manifest = read_json(args.root / MANIFEST)
        issues = None
        if args.mote_store:
            process = subprocess.run(["mote", "--store", str(args.mote_store), "--json", "ls", "--all"],
                                     check=True, capture_output=True, text=True, timeout=60)
            records = json.loads(process.stdout, object_pairs_hook=unique_object)
            require(isinstance(records, list), "invalid Mote response")
            issues = {}
            for item in records:
                require(isinstance(item, dict) and "id" in item and "status" in item, "invalid Mote row")
                require(item["id"] not in issues, "duplicate Mote issue")
                require(item["status"] in ("open", "doing", "blocked", "review", "closed"),
                        "unknown Mote status")
                issues[item["id"]] = item["status"]
        validate(manifest, args.root, args.wheel, issues)
        table(args.root, manifest, args.write_table)
        print(f"PASS: {len(manifest['capabilities'])} capabilities; local sources and table checked")
        print("Comparator wheel: " + ("verified" if args.wheel else "NOT CHECKED (use --wheel)"))
        print("Live Mote ownership: " + ("verified" if issues is not None else "NOT CHECKED (use --mote-store)"))
        print("Scientific workflows, benchmarks, visual review and release availability were NOT executed.")
        return 0
    except (ValueError, OSError, zipfile.BadZipFile, subprocess.SubprocessError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())

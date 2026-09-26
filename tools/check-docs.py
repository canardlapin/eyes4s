#!/usr/bin/env python3
"""Keep the public migration map complete and internal documents out of the site.

Execution evidence is platform-aware: `--platform jvm` checks only the rendered site and the
JVM test reports a rootJVM CI job produces, `--platform js` only the Scala.js reports, `all`
(the default) both, and `none` only the static inventory. Every check selected is strict.
The isolated artifact consumer is checked from its cached receipt unless `--run-consumer`
rebuilds it or `--skip-consumer` explicitly leaves it to the scheduled workflow.
"""
import argparse
import hashlib
import importlib.util
import json
from pathlib import Path
import re
import subprocess
import sys
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
PLATFORMS = {"all": ("jvm", "js"), "jvm": ("jvm",), "js": ("js",), "none": ()}
parser = argparse.ArgumentParser(
    description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
)
parser.add_argument(
    "--platform",
    choices=tuple(PLATFORMS),
    default="all",
    help="test-report platform(s) whose execution evidence is checked",
)
consumer_mode = parser.add_mutually_exclusive_group()
consumer_mode.add_argument(
    "--run-consumer",
    action="store_true",
    help="publish locally, rebuild the isolated consumer and check it",
)
consumer_mode.add_argument(
    "--skip-consumer",
    action="store_true",
    help="do not check isolated-consumer evidence",
)
args = parser.parse_args()
axes = PLATFORMS[args.platform]

SOURCE = ROOT / "site-docs"
manifest = json.loads((ROOT / "tools/r-parity/baseline.json").read_text())
guide = (SOURCE / "migration.md").read_text()
entries = {entry for row in manifest["rows"] for entry in row["required_entry_points"]}
missing = sorted(
    entry
    for entry in entries
    if f"`{entry}`" not in guide and f"`{entry.split('.')[0]}`" not in guide
)
if missing:
    raise SystemExit(f"Migration guide lacks required public entry points: {missing}")
expected = {
    "index",
    "getting-started",
    "concepts",
    "fixation-studies",
    "recordings",
    "repetition",
    "templates",
    "exports",
    "migration",
    "reference",
}
actual = {p.stem for p in SOURCE.rglob("*.md")}
if actual != expected:
    raise SystemExit(
        f"Review public page inventory: expected={expected}, actual={actual}"
    )
for page in SOURCE.rglob("*.md"):
    if re.search(r"^```scala\s*$", page.read_text(), re.M):
        raise SystemExit(
            f"Unverified Scala fence in {page}; use mdoc or label pseudocode explicitly"
        )
rendered = ROOT / "site/target/docs/site"
if rendered.exists():
    html = {p.stem for p in rendered.rglob("*.html")}
    if html != expected:
        raise SystemExit(f"Unexpected rendered public pages: {html ^ expected}")
print(
    f"Public documentation: {len(expected)} pages; all {len(entries)} required baseline entry points mapped."
)

# A finite inventory, backed by actual successful test reports. Compilation and a
# prose name match alone are insufficient. This does not measure every API entry.
inventory = json.loads((ROOT / "tools/doc-examples.json").read_text())
# docs/studio belongs to the Studio application, not to the library documentation.
paths = {"README.md"} | {
    str(p.relative_to(ROOT))
    for base in ("docs", "site-docs")
    for p in (ROOT / base).rglob("*.md")
    if not p.is_relative_to(ROOT / "docs/studio")
}
if paths != set(inventory["pages"]):
    raise SystemExit(
        f"Review documentation page classification: {paths ^ set(inventory['pages'])}"
    )
links = set()
for name, kind in inventory["pages"].items():
    page = ROOT / name
    fences = re.findall(r"^```scala([^\n]*)\n(.*?)^```", page.read_text(), re.M | re.S)
    if kind.startswith("internal"):
        continue
    if name.startswith("site-docs/"):
        if any(
            not mode.strip().startswith("mdoc") or "compile-only" in mode
            for mode, _ in fences
        ):
            raise SystemExit(f"Non-executed site example: {name}")
        # docs/tlSite is JVM evidence: it runs only in the rootJVM job.
        if "jvm" in axes:
            output = rendered / (page.stem + ".html")
            if not output.exists() or output.stat().st_mtime < page.stat().st_mtime:
                raise SystemExit(f"Build current docs/tlSite first: {name}")
    elif len(fences) != inventory["fences"].get(name, {}).get("count", 0):
        raise SystemExit(f"Unmapped Scala fences: {name}")
    for link in re.findall(r"\]\(([^)]+\.scala)(?:#[^)]*)?\)", page.read_text()):
        prefix = "https://github.com/canardlapin/eyes4s/blob/main/"
        path = (
            ROOT / link[len(prefix) :]
            if link.startswith(prefix)
            else (page.parent / link).resolve()
        )
        if not path.is_file():
            raise SystemExit(f"Broken example link: {name}: {link}")
        links.add(str(path.relative_to(ROOT)))
if links != set(inventory["linked_sources"]):
    raise SystemExit(
        f'Review linked example inventory: {links ^ set(inventory["linked_sources"])}'
    )
for name, entry in inventory["linked_sources"].items():
    if entry.get("runner") != "isolated consumer" and not set(entry["platforms"]) <= {
        "jvm",
        "js",
    }:
        raise SystemExit(f'Unknown platform for {name}: {entry["platforms"]}')


def passing(path, suite, names=()):
    if not path.exists():
        raise SystemExit(f"Missing execution evidence: {path}")
    tree = ET.parse(path).getroot()
    cases = tree.findall("testcase")
    successful = {
        c.attrib["name"]
        for c in cases
        if not any(c.find(x) is not None for x in ("failure", "error", "skipped"))
    }
    if (
        tree.attrib.get("name") != suite
        or not successful
        or len(successful) != len(cases)
        or set(names) - successful
    ):
        raise SystemExit(
            f"Failed, skipped or misnamed execution evidence: {path}: {set(names)-successful}"
        )
    return successful


checked = {axis: 0 for axis in axes}
if "jvm" in axes:
    report = (
        ROOT
        / "io/.jvm/target/test-reports/TEST-docconsumer.DocumentationExamplesSuite.xml"
    )
    passing(
        report,
        "docconsumer.DocumentationExamplesSuite",
        [
            "README viewing geometry executes verbatim",
            "README finite control design executes verbatim",
            "ASC guide executes construction byte admission and resource-safe file import",
        ],
    )
    for name in (*inventory["fences"], "tools/documentation-examples.py"):
        if report.stat().st_mtime < (ROOT / name).stat().st_mtime:
            raise SystemExit(f"Rerun DocumentationExamplesSuite after changing {name}")
    checked["jvm"] += 1

for name, entry in inventory["linked_sources"].items():
    if entry.get("runner") == "isolated consumer":
        continue
    for axis in entry["platforms"]:
        if axis not in axes:
            continue
        path = (
            ROOT
            / entry["module"]
            / f'.{axis}/target/test-reports/TEST-{entry["suite"]}.xml'
        )
        passing(path, entry["suite"], entry["tests"])
        if path.stat().st_mtime < (ROOT / name).stat().st_mtime:
            raise SystemExit(f'Rerun {entry["suite"]} after changing {name}')
        checked[axis] += 1
empty = [axis for axis, count in checked.items() if count == 0]
if empty:
    raise SystemExit(
        f"No execution evidence is mapped to platform(s) {empty}; review tools/doc-examples.json"
    )

# The isolated consumer owns its classpath/source identity and numerical evidence.
# Cache only an exact source candidate; a changed library, consumer or oracle fixture
# requires a rerun.
consumer_note = "isolated consumer skipped (--skip-consumer)"
if not args.skip_consumer:
    spec = importlib.util.spec_from_file_location(
        "study_consumer_verify", ROOT / "tools/study-consumer/verify.py"
    )
    verify = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(verify)
    consumer_inputs = sorted(
        set(
            [
                p
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
                )
                for p in (ROOT / module).rglob("*.scala")
                if "/src/main/" in str(p) and "/target/" not in str(p)
            ]
            + list((ROOT / "tools/study-consumer").rglob("*.scala"))
            + [ROOT / name for name in verify.FIXTURES]
            + [
                ROOT / "build.sbt",
                ROOT / "tools/study-consumer/build.sbt",
                ROOT / "tools/study-consumer/verify.py",
            ]
        )
    )
    digest = hashlib.sha256()
    for p in consumer_inputs:
        digest.update(str(p.relative_to(ROOT)).encode())
        digest.update(p.read_bytes())
    fingerprint = digest.hexdigest()
    cache = ROOT / "target/documentation-consumer.json"
    if args.run_consumer:
        run = subprocess.run(
            [sys.executable, "tools/study-consumer/verify.py"],
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
        )
        log = ROOT / "target/documentation-consumer.log"
        log.parent.mkdir(exist_ok=True)
        log.write_text(run.stdout)
        print(run.stdout)
        if run.returncode:
            raise SystemExit(
                "Isolated consumer failed; see target/documentation-consumer.log"
            )
        directory = Path(
            re.search(r"Isolated consumer and logs: (.+)", run.stdout).group(1)
        ).resolve()
        stored = (
            str(directory.relative_to(ROOT))
            if directory.is_relative_to(ROOT)
            else str(directory)
        )
        cache.write_text(
            json.dumps({"fingerprint": fingerprint, "directory": stored}, indent=2)
            + "\n"
        )
    if (
        not cache.exists()
        or json.loads(cache.read_text())["fingerprint"] != fingerprint
    ):
        raise SystemExit(
            "Run python3 tools/check-docs.py --run-consumer for this source candidate"
        )
    consumer = ROOT / json.loads(cache.read_text())["directory"]
    receipt_path = consumer / "receipt.json"
    if not receipt_path.is_file():
        raise SystemExit(
            f"Isolated consumer receipt {receipt_path} is missing (the build directory was "
            "removed); rerun python3 tools/check-docs.py --run-consumer"
        )
    receipt = json.loads(receipt_path.read_text())
    consumers = 0
    for name, entry in inventory["linked_sources"].items():
        if entry.get("runner") == "isolated consumer":
            relative = name.removeprefix("tools/study-consumer/")
            if (
                hashlib.sha256((ROOT / name).read_bytes()).hexdigest()
                != receipt["sources_sha256"][relative]
            ):
                raise SystemExit(f"Consumer source identity drift: {name}")
            for axis in ("jvm", "js"):
                passing(
                    consumer / f'.{axis}/target/test-reports/TEST-{entry["suite"]}.xml',
                    entry["suite"],
                )
            consumers += 1
    consumer_note = f"{consumers} isolated-consumer programs on jvm and js"
print(
    f'Executed documentation inventory ({args.platform}): {len(paths)} classified pages, '
    f'{sum(v["count"] for v in inventory["fences"].values())} verbatim external fences, '
    f'{len(links)} linked programs; reports checked per platform {checked}; {consumer_note}; '
    'no unexplained gaps.'
)

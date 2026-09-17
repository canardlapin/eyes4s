#!/usr/bin/env python3
"""Keep the public migration map complete and internal documents out of the site."""
import json
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[1]
SOURCE = ROOT / "site-docs"
manifest = json.loads((ROOT / "tools/r-parity/baseline.json").read_text())
guide = (SOURCE / "migration.md").read_text()
entries = {entry for row in manifest["rows"] for entry in row["required_entry_points"]}
missing = sorted(entry for entry in entries if f"`{entry}`" not in guide and
                 f"`{entry.split('.')[0]}`" not in guide)
if missing:
    raise SystemExit(f"Migration guide lacks required public entry points: {missing}")
expected = {"index", "getting-started", "concepts", "fixation-studies", "recordings",
            "repetition", "templates", "migration", "reference"}
actual = {p.stem for p in SOURCE.rglob("*.md")}
if actual != expected:
    raise SystemExit(f"Review public page inventory: expected={expected}, actual={actual}")
for page in SOURCE.rglob("*.md"):
    if re.search(r"^```scala\s*$", page.read_text(), re.M):
        raise SystemExit(f"Unverified Scala fence in {page}; use mdoc or label pseudocode explicitly")
rendered = ROOT / "site/target/docs/site"
if rendered.exists():
    html = {p.stem for p in rendered.rglob("*.html")}
    if html != expected:
        raise SystemExit(f"Unexpected rendered public pages: {html ^ expected}")
print(f"Public documentation: {len(expected)} pages; all {len(entries)} required baseline entry points mapped.")

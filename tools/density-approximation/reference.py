#!/usr/bin/env python3
"""Measure the public cohort through the standard locked R parity session."""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import sys

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "tools/r-parity"))
sys.path.insert(0, str(ROOT / "tools/private-fixtures"))
import parity
from end_to_end import MAP_TOLERANCE, SCORE_TOLERANCE, assert_close, gaussian
from analyze import identity, source, rounded, scores, reference_axes

INPUT = ROOT / "tools/r-parity/fixtures/cases/density-approximation.json"
OUTPUT = ROOT / "tools/r-parity/fixtures/density-approximation.json"


def request(data, nx, ny, sigma):
    templates, sources = [], []
    for item in data["items"]:
        row = dict(id=identity(item), participant=item["participant"], **source(item))
        row["fixations"]["onset_ms"] = [value / 1000 for value in item["onsets_us"]]
        if item["phase"] == "encode":
            templates.append(row)
        else:
            row["template"] = row["id"].rsplit("/", 1)[0] + "/encode"
            sources.append(row)
    return dict(schema_version=1, templates=templates, sources=sources,
                x_min=data["bounds"][0], y_min=data["bounds"][1],
                x_max=data["bounds"][2], y_max=data["bounds"][3],
                nx=nx, ny=ny, sigma_px=sigma, min_fixations=2, end_to_end=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--eyesim", type=Path, required=True)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--retain", type=Path, help="Optional external directory for complete raw maps and audited sessions")
    args = parser.parse_args()
    data = json.loads(INPUT.read_text())
    items = {identity(item): item for item in data["items"]}
    contexts = []
    with parity.r_session("eyes4s-density-reference-", args.eyesim) as session:
        for nx, ny in data["resolutions"]:
            for sigma in data["sigmas"]:
                name = f"n{nx}-s{sigma}"
                folder = session.tmp / name
                folder.mkdir()
                p = request(data, nx, ny, sigma)
                input_path = folder / "request.json"
                input_path.write_text(json.dumps(p, indent=2) + "\n")
                session.rscript(ROOT / "tools/private-fixtures/shared_density.R", session.eyesim_library, input_path, folder)
                audits = sorted(session.tmp.glob("audit-*.tsv"), key=lambda p: int(p.stem.split("-")[1]))
                if args.retain:
                    destination = args.retain / name
                    shutil.copytree(folder, destination)
                    shutil.copy2(audits[-1], destination / "audit.tsv")
                shared = json.loads((folder / "shared.json").read_text())
                runtime_header = (folder / "R-session.txt").read_text().split("locale:")[0].strip()
                details = json.loads((folder / "eyesim-maps.json").read_text())["maps"]
                xs, ys = reference_axes(p)
                measured, max_error, full_error = {}, 0.0, 0.0
                for row in details:
                    assert row["x"] == xs and row["y"] == ys, (name, "reference axes")
                    assert row["exact_x"] == xs and row["exact_y"] == ys
                    oracle = rounded(gaussian(p, source(items[row["id"]]), xs, ys, support_sigma=3.7))
                    full = rounded(gaussian(p, source(items[row["id"]]), xs, ys))
                    assert_close(row["values"], oracle, MAP_TOLERANCE, f"R-support={name}/{row['id']}")
                    assert_close(row["exact"], full, MAP_TOLERANCE, f"R-full={name}/{row['id']}")
                    max_error = max(max_error, max(abs(a-b) for a,b in zip(row["values"], oracle)))
                    full_error = max(full_error, max(abs(a-b) for a,b in zip(row["exact"], full)))
                    measured[row["id"]] = row["values"]
                rows, score_error = [], 0.0
                for row in shared["sources"]:
                    key = row["id"]
                    controls = sorted(k for k in items if k.startswith(row["participant"] + "/") and k.endswith("/encode") and k != row["template"])
                    expected = scores(measured, key, controls)
                    assert row["n_perm"] == len(controls) == 2
                    for a,b in ((row["observed"],expected["matched"]), (row["baseline"],expected["control"]), (row["difference"],expected["difference"])):
                        assert SCORE_TOLERANCE.matches(a,b), (name,key,a,b)
                        score_error = max(score_error, abs(a-b))
                    rows.append(dict(query=key, observed=row["observed"], baseline=row["baseline"], difference=row["difference"], controls=controls))
                loaded = [line.split("\t")[:2] for line in audits[-1].read_text().splitlines()[1:]]
                contexts.append(dict(nx=nx, ny=ny, sigma=sigma, maps=len(details), support_map_max_absolute_error=max_error,
                                     full_map_max_absolute_error=full_error, score_max_absolute_error=score_error,
                                     loaded_namespaces=dict(loaded), scores=rows))
                print(f"verified {name}: maps={len(details)}, scores={len(rows)}, map_error={max_error:.3g}", flush=True)
    document = dict(schema=1, eyesim_revision=parity.pinned_revision(), R=parity.load_lock()["R"],
                    packages={name:version for context in contexts for name,version in context["loaded_namespaces"].items()},
                    runtime_header=runtime_header,
                    input_sha256=hashlib.sha256(INPUT.read_bytes()).hexdigest(),
                    conventions="ks unbinned supp=3.7; endpoint grid; duration weights; normalized then zapsmall7; exhaustive within-participant controls",
                    map_tolerance=vars(MAP_TOLERANCE), score_tolerance=vars(SCORE_TOLERANCE), contexts=contexts)
    rendered = json.dumps(document,indent=2) + "\n"
    if args.check:
        assert OUTPUT.read_text() == rendered, "reference output drift"
    else:
        OUTPUT.write_text(rendered)


if __name__ == "__main__":
    main()

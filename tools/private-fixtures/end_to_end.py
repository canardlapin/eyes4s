#!/usr/bin/env python3
"""Local estimator comparison, independent numerical oracles, and optional real Studio UI run."""
import argparse
from collections import defaultdict
import csv
import hashlib
import json
import math
import os
from pathlib import Path
import subprocess
import sys

from run import ROOT, Tolerance, digest, dump, implementation, micros, outside_git

MAP_TOLERANCE = Tolerance(absolute=2e-13, relative=2e-12)
SCORE_TOLERANCE = Tolerance(absolute=2e-12, relative=2e-12)


def validate(p):
    if p.get("schema_version") != 1:
        raise ValueError("Expected request schema_version 1")
    for key in ("nx", "ny", "min_fixations"):
        if type(p.get(key)) is not int or p[key] < 2:
            raise ValueError(f"{key} must be an integer >= 2")
    for key in ("x_min", "x_max", "y_min", "y_max", "sigma_px"):
        if not isinstance(p.get(key), (int, float)) or not math.isfinite(p[key]):
            raise ValueError(f"{key} must be finite")
    if p["x_max"] <= p["x_min"] or p["y_max"] <= p["y_min"] or p["sigma_px"] <= 0:
        raise ValueError("Bounds must have area and sigma must be positive")
    if not p["templates"] or not p["sources"]:
        raise ValueError("Both templates and sources are required")
    ids = set()
    for item in p["templates"] + p["sources"]:
        for key in ("id", "participant"):
            if not isinstance(item.get(key), str) or not item[key].strip():
                raise ValueError("Every map needs a nonempty id and participant")
        if item["id"] in ids:
            raise ValueError("Map IDs must be globally unique")
        ids.add(item["id"])
        f = item["fixations"]
        columns = [f[k] for k in ("x", "y", "duration_ms", "onset_ms")]
        if len({len(v) for v in columns}) != 1 or len(columns[0]) < p["min_fixations"]:
            raise ValueError("Unequal fixation columns or too few fixations")
        for x, y, duration, onset in zip(*columns):
            if not all(math.isfinite(v) for v in (x, y, duration, onset)) or duration <= 0:
                raise ValueError("Nonfinite fixation or nonpositive duration")
            if not (p["x_min"] <= x < p["x_max"] and p["y_min"] <= y < p["y_max"]):
                raise ValueError("Crop exclusions must be accounted for before this request")
    templates = {t["id"]: t for t in p["templates"]}
    eligible = defaultdict(set)
    for source in p["sources"]:
        if source.get("template") not in templates:
            raise ValueError("Missing matched template")
        if templates[source["template"]]["participant"] != source["participant"]:
            raise ValueError("Cross-participant match")
        eligible[source["participant"]].add(source["template"])
    if any(len(v) < 2 for v in eligible.values()):
        raise ValueError("Each participant needs distinct nonmatching controls")
    if set(templates) != set().union(*eligible.values()):
        raise ValueError("Template pool must equal the templates represented by sources")


def normalized(values):
    total = math.fsum(values)
    if total <= 0 or not math.isfinite(total):
        raise ValueError("Nonpositive density total")
    return [v / total for v in values]


def axes(p, centers=True):
    def axis(lo, hi, n):
        step = (hi - lo) / (n if centers else n - 1)
        return [lo + (i + (0.5 if centers else 0)) * step for i in range(n)]
    return axis(p["x_min"], p["x_max"], p["nx"]), axis(p["y_min"], p["y_max"], p["ny"])


def gaussian(p, item, xs, ys, quantize=False, truncate=False):
    """Direct sum of per-point outer products; independent of convolution passes."""
    f = item["fixations"]
    dx = (p["x_max"] - p["x_min"]) / p["nx"]
    dy = (p["y_max"] - p["y_min"]) / p["ny"]
    rx, ry = math.ceil(3 * p["sigma_px"] / dx), math.ceil(3 * p["sigma_px"] / dy)
    sigma = p["sigma_px"]
    result = [0.0] * (len(xs) * len(ys))
    for x, y, weight in zip(f["x"], f["y"], f["duration_ms"]):
        ix = min(p["nx"] - 1, int((x - p["x_min"]) / dx))
        iy = min(p["ny"] - 1, int((y - p["y_min"]) / dy))
        if quantize:
            x, y = p["x_min"] + (ix + .5) * dx, p["y_min"] + (iy + .5) * dy
        kx = [math.exp(-.5 * ((v - x) / sigma) ** 2)
              if not truncate or abs(j - ix) <= rx else 0.0 for j, v in enumerate(xs)]
        ky = [math.exp(-.5 * ((v - y) / sigma) ** 2)
              if not truncate or abs(j - iy) <= ry else 0.0 for j, v in enumerate(ys)]
        for row, ay in enumerate(ky):
            for col, ax in enumerate(kx):
                result[row * len(xs) + col] += weight * ax * ay
    return normalized(result)


def cosine(a, b):
    return math.fsum(x * y for x, y in zip(a, b)) / math.sqrt(
        math.fsum(x * x for x in a) * math.fsum(x * x for x in b))


def discrepancy(a, b):
    if len(a) != len(b):
        raise ValueError("Grid cardinality differs")
    return {"max_abs": max(abs(x - y) for x, y in zip(a, b)),
            "l1": math.fsum(abs(x - y) for x, y in zip(a, b)), "cosine": cosine(a, b)}


def assert_close(a, b, tolerance, label):
    if len(a) != len(b) or not all(tolerance.matches(x, y) for x, y in zip(a, b)):
        raise ValueError(f"{label} differs from its independent oracle")


def analyze(p, output):
    reference = json.loads((output / "eyesim-maps.json").read_text())
    native = json.loads((output / "eyes4s.json").read_text())
    shared = json.loads((output / "shared.json").read_text())
    r = {m["id"]: m for m in reference["maps"]}
    s = {m["id"]: m["values"] for m in native["maps"]}
    expected_ids = {i["id"] for i in p["templates"] + p["sources"]}
    if (set(r) != expected_ids or set(s) != expected_ids
            or len(reference["maps"]) != len(expected_ids) or len(native["maps"]) != len(expected_ids)):
        raise ValueError("Estimator map identities differ")
    xs, ys = axes(p)
    details = []
    for item in p["templates"] + p["sources"]:
        ident = item["id"]
        exact_r = gaussian(p, item, r[ident]["exact_x"], r[ident]["exact_y"])
        # eyesim deliberately rounds normalized cells relative to the map maximum.
        # R's zapsmall passes fractional digits to round(); round chooses the
        # nearest integer number of decimal places, rather than floor(log10).
        digits = max(0, math.floor(7 - math.log10(max(exact_r)) + .5))
        rounding = Tolerance(absolute=.50001 * 10 ** -digits + 2e-13, relative=0)
        assert_close(r[ident]["exact"], exact_r, rounding, "eyesim unbinned Gaussian")
        direct_centers = gaussian(p, item, xs, ys)
        binned_centers = gaussian(p, item, xs, ys, quantize=True)
        truncated = gaussian(p, item, xs, ys, quantize=True, truncate=True)
        assert_close(s[ident], truncated, MAP_TOLERANCE, "eyes4s grid Gaussian")
        details.append({"id": ident,
            "eyesim_exact_oracle": discrepancy(r[ident]["exact"], exact_r),
            "eyesim_rounding_decimal_places": digits,
            "eyesim_rounding_tolerance": rounding.__dict__,
            "eyesim_default_vs_exact": discrepancy(normalized(r[ident]["values"]), exact_r),
            "endpoint_vs_center_grid": discrepancy(exact_r, direct_centers),
            "point_binning": discrepancy(direct_centers, binned_centers),
            "finite_kernel_support": discrepancy(binned_centers, truncated),
            "eyes4s_oracle": discrepancy(s[ident], truncated),
            "native_estimators": discrepancy(normalized(r[ident]["values"]), s[ident])})
    actual = {v["id"]: v for v in native["sources"]}
    r_scores = {v["id"]: v for v in shared["sources"]}
    source_ids = {v["id"] for v in p["sources"]}
    if (set(actual) != source_ids or set(r_scores) != source_ids
            or len(native["sources"]) != len(source_ids) or len(shared["sources"]) != len(source_ids)):
        raise ValueError("Score row identities or cardinality differ")
    eligible = defaultdict(set)
    for source in p["sources"]:
        eligible[source["participant"]].add(source["template"])
    scores = []
    for source in p["sources"]:
        ident, match = source["id"], source["template"]
        controls = sorted(eligible[source["participant"]] - {match})
        found = actual[ident]
        if found["template"] != match or sorted(found["controls"]) != controls:
            raise ValueError("Exact pair identities differ")
        observed = cosine(s[ident], s[match])
        baseline = math.fsum(cosine(s[ident], s[t]) for t in controls) / len(controls)
        values = [observed, baseline, observed - baseline]
        names = ("observed", "baseline", "difference")
        assert_close([found[n] for n in names], values, SCORE_TOLERANCE, "eyes4s scores")
        rv = r_scores[ident]
        if rv["n_perm"] != len(controls):
            raise ValueError("Reference control cardinality differs")
        r_observed = cosine(r[ident]["values"], r[match]["values"])
        r_baseline = math.fsum(cosine(r[ident]["values"], r[t]["values"]) for t in controls) / len(controls)
        assert_close([rv[n] for n in names], [r_observed, r_baseline, r_observed-r_baseline],
                     SCORE_TOLERANCE, "eyesim scores")
        scores.append({"id": ident, "participant": source["participant"],
                       "phase": source.get("phase", "Retrieval"), "template": match, "controls": controls,
                       "eyes4s": dict(zip(names, values)), "eyesim": {n: rv[n] for n in names},
                       "delta": {n: values[i] - rv[n] for i, n in enumerate(names)}})
    grouped = defaultdict(list)
    for row in scores:
        grouped[row["phase"]].append(row)
    summary = {phase: {"sources": len(rows),
        "participants": len({v["participant"] for v in rows}),
        "mean_delta": {n: math.fsum(v["delta"][n] for v in rows) / len(rows) for n in names},
        "maximum_absolute_delta": {n: max(abs(v["delta"][n]) for v in rows) for n in names}}
        for phase, rows in grouped.items()}
    result = {"status": "passed", "scope": "Each native estimator verified against its own explicit oracle; estimator differences measured, not asserted zero",
              "map_tolerance": MAP_TOLERANCE.__dict__, "score_tolerance": SCORE_TOLERANCE.__dict__,
              "maps": details, "scores": scores, "by_phase": summary}
    dump(output / "end-to-end.json", result)
    return result


def write_ui_inputs(p, output):
    for key in ("screen_width_px", "screen_height_px", "sample_rate_hz", "pixels_per_degree"):
        if not isinstance(p.get(key), (int, float)) or not math.isfinite(p[key]) or p[key] <= 0:
            raise ValueError(f"UI requires a positive declared {key}")
    if not p.get("geometry_note"):
        raise ValueError("UI requires geometry_note stating the source of the angular conversion")
    for key in ("x_min", "x_max", "y_min", "y_max", "screen_width_px", "screen_height_px"):
        if p[key] != int(p[key]):
            raise ValueError("Studio image placement requires integral pixel bounds")
    if any(not s.get("phase") or s["phase"] == "Encoding" for s in p["sources"]):
        raise ValueError("UI sources require explicit retrieval phase labels distinct from Encoding")
    for name in ("fixations.csv", "trials.csv"):
        with (output / name).open("x", newline="") as stream:
            writer = csv.writer(stream)
            if name == "fixations.csv":
                writer.writerow(["participant", "phase", "trial", "occurrence", "ordinal", "x", "y", "onset_ms", "duration_ms", "sample_count"])
            else:
                writer.writerow(["participant", "phase", "trial", "occurrence", "item", "response", "display_kind", "image_file"])
            for item in p["templates"] + p["sources"]:
                phase = item.get("phase", "Encoding") if "template" in item else "Encoding"
                key = [item["participant"], phase, item["id"], 1]
                if name == "trials.csv":
                    writer.writerow(key + [item.get("template", item["id"]),
                        item.get("response", "unspecified"), item.get("display_kind", "unknown"),
                        item.get("image_file", "")])
                else:
                    f = item["fixations"]
                    for ordinal, (x, y, onset, duration) in enumerate(zip(f["x"], f["y"], f["onset_ms"], f["duration_ms"]), 1):
                        count = math.ceil(micros(str(duration), "milliseconds") * p["sample_rate_hz"] / 1e6)
                        writer.writerow(key + [ordinal, x, y, onset, duration, count])


def ui(output):
    # Build has no data path or private environment; test framework runs separately.
    env = dict(os.environ, GITHUB_ACTIONS="true")
    with (output / "studio-build.log").open("x") as log:
        subprocess.run(["sbt", "-J-Xmx3g", "-J-XX:ActiveProcessorCount=4",
                        "studioDesktop/Test/compile", "export studioDesktop/Test/fullClasspath"],
                       cwd=ROOT, env=env, stdout=log, stderr=subprocess.STDOUT, check=True)
    lines = (output / "studio-build.log").read_text().splitlines()
    classpath = next(line for line in reversed(lines) if line.startswith("/") and ".jar" in line)
    flags = ["-Deyes4s.studio.fx.headless=true", "-Dglass.platform=Monocle",
             "-Dmonocle.platform=Headless", "-Dheadless.geometry=1920x1200-32",
             "-Dprism.order=sw", "-Dprism.lcdtext=false", "-Djava.awt.headless=true",
             f"-Deyes4s.studio.snapshots={output / 'snapshots'}"]
    with (output / "ui.log").open("x") as log:
        subprocess.run(["java", "-Xmx3g", "-XX:ActiveProcessorCount=4", *flags, "-cp", classpath,
                        "org.junit.runner.JUnitCore", "eyes4s.studio.desktop.journey.PrivateDensityFxSuite"],
                       cwd=output, env=dict(os.environ, EYES4S_PRIVATE_RUN=str(output)),
                       stdout=log, stderr=subprocess.STDOUT, check=True)
    result = json.loads((output / "ui-result.json").read_text())
    if result["status"] != "passed":
        raise ValueError("UI did not produce a passing result receipt")
    return result


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--request", required=True, type=Path)
    ap.add_argument("--eyesim", required=True, type=Path)
    ap.add_argument("--r-library", required=True, type=Path)
    ap.add_argument("--output-dir", required=True, type=Path)
    ap.add_argument("--ui", action="store_true", help="Exercise the real Studio window headlessly (JDK 22+)")
    args = ap.parse_args()
    os.umask(0o077)
    output = None
    try:
        if os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"):
            raise ValueError("Private comparisons are disabled in CI")
        source = outside_git(args.request)
        payload = source.read_bytes()
        source_hash = hashlib.sha256(payload).hexdigest()
        p = json.loads(payload)
        validate(p)
        p["end_to_end"] = True
        candidate = outside_git(args.output_dir)
        candidate.mkdir(parents=True, mode=0o700, exist_ok=False)
        output = candidate
        provenance = implementation()
        dump(output / "request-original.json", p)
        reference = output / "comparison"
        subprocess.run([sys.executable, str(Path(__file__).with_name("compare.py")),
                        "--request", str(output / "request-original.json"), "--eyesim", str(args.eyesim),
                        "--r-library", str(args.r_library), "--output-dir", str(reference)], check=True)
        paths = list((ROOT / "io/.jvm/target").glob("scala-*/resource_managed/test/eyes4s/fresh-process/classpath.txt"))
        if len(paths) != 1:
            raise ValueError("Expected exactly one freshly generated classpath")
        with (reference / "native.log").open("x") as log:
            subprocess.run(["java", "-Xmx2g", "-cp", paths[0].read_text(),
                            "eyes4s.examples.EndToEndDensityCli"], cwd=reference,
                           env=dict(os.environ, EYES4S_PRIVATE_RUN=str(reference)),
                           stdout=log, stderr=subprocess.STDOUT, check=True)
        result = analyze(p, reference)
        ui_result = None
        if args.ui:
            write_ui_inputs(p, reference)
            ui_result = ui(reference)
        if digest(source) != source_hash:
            raise ValueError("Source request changed during the run")
        if implementation() != provenance:
            raise ValueError("Implementation changed during this run")
        dump(output / "receipt.json", {"status": "passed", "implementation": provenance,
             "input_sha256": source_hash, "reference_receipt_sha256": digest(reference / "receipt.json"),
             "by_phase": result["by_phase"], "ui": ui_result})
        print("End-to-end checks passed; results and any UI evidence remain in the external output directory.")
        return 0
    except (ValueError, TypeError, KeyError, OSError, StopIteration, subprocess.SubprocessError) as error:
        if output and output.is_dir():
            (output / "failure.json").write_text(json.dumps({"status": "failed", "error": str(error)}, indent=2))
        print("End-to-end run failed; inspect the external logs.", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())

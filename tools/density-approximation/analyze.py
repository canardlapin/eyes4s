#!/usr/bin/env python3
"""Independent native-map checks and ordered density-convention score decomposition."""
import argparse
import hashlib
import json
import math
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "tools/private-fixtures"))
from end_to_end import MAP_TOLERANCE, SCORE_TOLERANCE, assert_close, axes, gaussian, cosine
from generate_inputs import gram

INPUT = ROOT / "tools/r-parity/fixtures/cases/density-approximation.json"


def identity(item):
    return "/".join(item[k] for k in ("participant", "stimulus", "phase"))


def source(item):
    return {"fixations": {"x": [p[0] for p in item["points"]],
                          "y": [p[1] for p in item["points"]],
                          "duration_ms": [value / 1000 for value in item["durations_us"]]}}


def rounded(values):
    # R zapsmall uses one decimal-place count for the whole normalized map.
    maximum = max(abs(value) for value in values)
    # zapsmall passes 7 - log10(maximum) to round; R's fround rounds
    # that fractional decimal-place count with floor(digits + 0.5).
    digits = math.floor(max(0, 7 - math.log10(maximum)) + 0.5) if maximum else 7
    return [round(value, digits) for value in values]


def reference_axes(p):
    # R seq(..., length.out=n) places the supplied endpoints verbatim.
    # Multiplying the step by n-1 need not reproduce the final endpoint bit.
    xs, ys = axes(p, False)
    xs[0], xs[-1] = p["x_min"], p["x_max"]
    ys[0], ys[-1] = p["y_min"], p["y_max"]
    return xs, ys


def scores(maps, key, controls):
    matched = cosine(maps[key], maps[key.rsplit("/", 1)[0] + "/encode"])
    baseline = math.fsum(cosine(maps[key], maps[control]) for control in controls) / len(controls)
    return {"matched": matched, "control": baseline, "difference": matched - baseline}


def continuous_scores(items, key, controls, sigma):
    def points(identity):
        item = items[identity]
        return [(x, y, weight) for (x, y), weight in zip(item["points"], item["durations_us"])]
    query = points(key)
    query_norm = math.sqrt(gram(query, query, sigma))
    def similarity(identity):
        reference = points(identity)
        return gram(query, reference, sigma) / (query_norm * math.sqrt(gram(reference, reference, sigma)))
    matched = similarity(key.rsplit("/", 1)[0] + "/encode")
    baseline = math.fsum(similarity(control) for control in controls) / len(controls)
    return {"matched": matched, "control": baseline, "difference": matched - baseline}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--native-log", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--reference", type=Path, help="Actual package-audited R reference receipt")
    args = parser.parse_args()
    data = json.loads(INPUT.read_text())
    items = {identity(item): item for item in data["items"]}
    jobs = [json.loads(line.split("DENSITY_COURT=", 1)[1])
            for line in args.native_log.read_text().splitlines() if "DENSITY_COURT=" in line]
    assert jobs, "no actual native study output"
    grouped = {}
    for job in jobs:
        context = (job["nx"], job["ny"], job["sigma"], job["phase"])
        grouped.setdefault(context, []).append(job)
    multiplicities = {len(group) for group in grouped.values()}
    assert multiplicities in ({1}, {2}), "incomplete or duplicated execution contexts"
    exact_runtime_agreement = multiplicities == {2}
    if exact_runtime_agreement:
        for context, group in grouped.items():
            assert group[0] == group[1], f"JVM/Scala.js disagreement={context}"
        jobs = [group[0] for group in grouped.values()]
    assert len({job["input"] for job in jobs}) == 1, "input changed during the sweep"
    expected_digest = hashlib.sha256(INPUT.read_bytes()).hexdigest()
    records, map_checks = [], []
    for job in jobs:
        assert job["source_sha256"] == expected_digest
        p = dict(x_min=data["bounds"][0], y_min=data["bounds"][1],
                 x_max=data["bounds"][2], y_max=data["bounds"][3],
                 nx=job["nx"], ny=job["ny"], sigma_px=job["sigma"])
        xs, ys = axes(p)
        assert job["x"] == xs and job["y"] == ys
        expected_keys = {key for key, item in items.items() if item["phase"] in ("encode", job["phase"])}
        native = {row["id"]: row["cells"] for row in job["maps"]}
        assert set(native) == set(items)
        assert {row["id"] for row in job["contrast"]} == {key for key, item in items.items() if item["phase"] == job["phase"]}
        assert set(job["excluded"]) == set(items) - expected_keys
        endpoint_x, endpoint_y = reference_axes(p)
        stages = {name: {} for name in ("native", "unbinned-native-support", "ks-support-centres",
                                        "ks-support-endpoints", "rounded-reference", "full-gaussian-centres")}
        stages["native"] = native
        for key, values in native.items():
            item = source(items[key])
            independent = gaussian(p, item, xs, ys, quantize=True, truncate=True)
            assert_close(values, independent, MAP_TOLERANCE, f"native={key}, grid={job['nx']}x{job['ny']}")
            map_checks.append({"nx":job["nx"], "ny":job["ny"], "sigma":job["sigma"], "phase":job["phase"], "id":key,
                               "max_absolute_error":max(abs(a-b) for a,b in zip(values, independent)),
                               "retained_weight_fraction":next(row["raw_total"] / row["input_total"] for row in job["maps"] if row["id"] == key)})
            stages["unbinned-native-support"][key] = gaussian(p, item, xs, ys, truncate=True)
            stages["ks-support-centres"][key] = gaussian(p, item, xs, ys, support_sigma=3.7)
            endpoint = gaussian(p, item, endpoint_x, endpoint_y, support_sigma=3.7)
            stages["ks-support-endpoints"][key] = endpoint
            stages["rounded-reference"][key] = rounded(endpoint)
            stages["full-gaussian-centres"][key] = gaussian(p, item, xs, ys)
        for row in job["contrast"]:
            key = row["id"]; participant, stimulus, _ = key.split("/")
            controls = sorted(k for k, item in items.items()
                              if item["participant"] == participant and item["phase"] == "encode" and item["stimulus"] != stimulus)
            assert row["controls"] == len(controls) == 2
            actual_controls = sorted(pair["right"] for pair in job["control_pairs"] if pair["left"] == key)
            assert actual_controls == controls
            actual_matched = [pair["right"] for pair in job["matched_pairs"] if pair["left"] == key]
            assert actual_matched == [participant + "/" + stimulus + "/encode"]
            measured = {name:scores(maps,key,controls) for name,maps in stages.items()}
            measured["continuous-full-gaussian"] = continuous_scores(items,key,controls,job["sigma"])
            for field in ("matched", "control", "difference"):
                assert SCORE_TOLERANCE.matches(row[field], measured["native"][field]), (key,field)
            ordered = ["native", "unbinned-native-support", "ks-support-centres", "ks-support-endpoints", "rounded-reference"]
            changes = {label: measured[after]["difference"] - measured[before]["difference"]
                       for label,before,after in zip(("binning","support","placement","rounding"),ordered,ordered[1:])}
            records.append({"nx":job["nx"], "ny":job["ny"], "sigma":job["sigma"], "phase":job["phase"],
                            "query":key,"matched_template":actual_matched[0],"control_pool":controls,
                            "scores":measured,"ordered_changes":changes,
                            "signed_native_to_reference":measured["rounded-reference"]["difference"]-row["difference"],
                            "strict_sign_disagreement":(row["difference"]>0)!=(measured["rounded-reference"]["difference"]>0)})
    document = {"version":1,"scope":"actual native StudyPlan and independent ordered direct-sum oracles; R execution recorded separately",
                "input_sha256":expected_digest,"native_log_sha256":hashlib.sha256(args.native_log.read_bytes()).hexdigest(),
                "input_identity":jobs[0]["input"],"jobs":len(jobs),"exact_jvm_js_agreement":exact_runtime_agreement,"map_oracle_tolerance":vars(MAP_TOLERANCE),
                "score_oracle_tolerance":vars(SCORE_TOLERANCE),"map_checks":map_checks,"scores":records,
                "attribution":"ordered binning/support/placement/rounding changes, not an order-independent causal attribution"}
    if args.reference:
        reference = json.loads(args.reference.read_text())
        assert reference["input_sha256"] == expected_digest
        expected_contexts = {(r["nx"],r["ny"],r["sigma"]) for r in records}
        assert {(c["nx"],c["ny"],c["sigma"]) for c in reference["contexts"]} == expected_contexts
        actual = {(c["nx"],c["ny"],c["sigma"],r["query"]): r
                  for c in reference["contexts"] for r in c["scores"]}
        max_error = 0.0
        for row in records:
            r = actual[(row["nx"],row["ny"],row["sigma"],row["query"])]
            assert r["controls"] == row["control_pool"]
            expected = row["scores"]["rounded-reference"]
            for field, r_field in (("matched","observed"),("control","baseline"),("difference","difference")):
                assert SCORE_TOLERANCE.matches(expected[field],r[r_field]), (row["query"],field)
                max_error = max(max_error,abs(expected[field]-r[r_field]))
        assert len(actual) == len(records)
        document["scope"] = "actual native JVM/Scala.js StudyPlan, direct-sum oracles and actual package-audited R calls"
        document["actual_r_reference"] = dict(sha256=hashlib.sha256(args.reference.read_bytes()).hexdigest(),
                                              query_checks=len(actual),max_score_model_absolute_error=max_error)
    args.output.parent.mkdir(parents=True,exist_ok=True)
    args.output.write_text(json.dumps(document,indent=2)+"\n")
    print(json.dumps({"jobs":len(jobs),"maps":len(map_checks),"queries":len(records),
                      "strict_sign_disagreements":sum(r["strict_sign_disagreement"] for r in records),
                      "max_signed_score_change":max(abs(r["signed_native_to_reference"]) for r in records)}))


if __name__ == "__main__":
    main()

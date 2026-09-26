#!/usr/bin/env python3
"""Measure static temporal sampling against an independent step/grid/mean oracle."""
import argparse, json, hashlib, sys, math, bisect
from pathlib import Path

sys.dont_write_bytecode = True
import parity

HERE, ROOT = parity.HERE, parity.ROOT
INPUT = HERE / "fixtures/cases/eyesim-temporal.json"
OUTPUT = HERE / "fixtures/point-sampling.json"
SCALA = ROOT / "plan/src/test/scala/eyes4s/plan/PointSamplingReference.scala"
ORACLE_TOLERANCE = 1e-12


def seq(v):
    return v if isinstance(v, list) else [v]


def mean(v):
    good = [x for x in v if x is not None]
    return sum(good) / len(good) if good else None


def close(a, b):
    assert (a is None and b is None) or (
        a is not None and b is not None and abs(a - b) <= ORACLE_TOLERANCE
    ), (a, b)


def normalize(v, n):
    if n == "none" or (n == "zscore" and max(v) == min(v)):
        return v
    if n == "max":
        return [x / max(v) for x in v]
    if n == "sum":
        return [x / sum(v) for x in v]
    m = mean(v)
    sd = math.sqrt(sum((x - m) ** 2 for x in v) / (len(v) - 1))
    return [(x - m) / sd for x in v]


def sample(source, values, t):
    onsets = source["onset"]
    # The final fixation is held after its onset; only a time before the first onset is missing.
    if t < onsets[0]:
        return None
    i = bisect.bisect_right(onsets, t) - 1
    # R rounds the ONE-based grid index to even before clamping.
    x = max(1, min(2, round(source["x"][i] + 0.5))) - 1
    y = max(1, min(2, round(source["y"][i] + 0.5))) - 1
    return values[x + 2 * y]


def bins(t, b):
    # Every bin is half-open, including the last: a time on the final break is unbinned.
    if t < b[0] or t >= b[-1]:
        return None
    return min(len(b) - 2, bisect.bisect_right(b, t) - 1)


def artifacts(inputs, actual):
    membership = [bins(t, inputs["bins"]) for t in inputs["queries"]]
    assert actual["bin_membership"] == [
        None if b is None else b + 1 for b in membership
    ]
    assert len(actual["boundaries"]["unmatched"]["result"]) == len(inputs["sources"])
    assert all(
        r["key"] != "unmatched" for r in actual["boundaries"]["unmatched"]["result"]
    )
    assert (
        actual["boundaries"]["duplicate"]["result"]
        == actual["normalizations"]["none"]["cap_20"]["result"]
    )
    assert actual["boundaries"]["empty_source"]["result"] == []
    assert all(
        r["sampled"] == [] and r["bin_1"] is None
        for r in actual["boundaries"]["empty_queries"]["result"]
    )
    assert all("bin_1" not in r for r in actual["boundaries"]["no_bins"]["result"])
    results = []
    for n, cases in actual["normalizations"].items():
        fields = [normalize(t["values"], n) for t in inputs["templates"]]
        for selection in actual["selected"]:
            cap = selection["cap"]
            rows = cases[f"cap_{cap}"]["result"]
            for s, r, sel in zip(
                inputs["sources"], rows, selection["rows"], strict=True
            ):
                idx = next(
                    i
                    for i, t in enumerate(inputs["templates"])
                    if t["key"] == s["matched"]
                )
                eligible = list(
                    dict.fromkeys(
                        next(
                            i
                            for i, t in enumerate(inputs["templates"])
                            if t["key"] == a["matched"]
                        )
                        + 1
                        for a in inputs["sources"]
                        if a["stratum"] == s["stratum"] and a["matched"] != s["matched"]
                    )
                )
                assert seq(sel["eligible"]) == eligible
                selected = seq(sel["selected"])
                assert all(i in eligible for i in selected) and len(selected) == min(
                    cap, len(eligible)
                )
                values = [sample(s, fields[idx], t) for t in inputs["queries"]]
                controls = (
                    [
                        mean([sample(s, fields[i - 1], t) for i in selected])
                        for t in inputs["queries"]
                    ]
                    if cap
                    else None
                )
                assert [x["time"] for x in r["sampled"]] == inputs["queries"]
                for a, b in zip(values, [x["z"] for x in r["sampled"]], strict=True):
                    close(a, b)
                if cap:
                    for a, b in zip(
                        controls, [x["z"] for x in r["perm_sampled"]], strict=True
                    ):
                        close(a, b)
                observed_bins = [
                    mean([v for v, b in zip(values, membership) if b == i])
                    for i in range(len(inputs["bins"]) - 1)
                ]
                control_bins = (
                    [
                        mean([v for v, b in zip(controls, membership) if b == i])
                        for i in range(len(inputs["bins"]) - 1)
                    ]
                    if cap
                    else None
                )
                for i, v in enumerate(observed_bins):
                    close(v, r[f"bin_{i+1}"])
                    if cap:
                        close(control_bins[i], r[f"perm_bin_{i+1}"])
                        close(
                            None
                            if v is None or control_bins[i] is None
                            else v - control_bins[i],
                            r[f"diff_bin_{i+1}"],
                        )
                results.append(
                    dict(
                        normalization=n,
                        cap=cap,
                        key=s["key"],
                        values=values,
                        controls=controls,
                        bins=observed_bins,
                        control_bins=control_bins,
                    )
                )
    partial = [t["values"][:] for t in inputs["templates"]]
    partial[1] = [2, None, 3, 9]
    partial[2] = [9, 3, None, 1]
    control_times = [
        [sample(inputs["sources"][0], partial[i], t) for i in [1, 2]]
        for t in inputs["queries"]
    ]
    # The single bin [0, 30) of point_sampling.R: both controls succeed early, only one late.
    partial_membership = [bins(t, [0, 30]) for t in inputs["queries"]]
    nested = mean(
        [mean(v) for v, b in zip(control_times, partial_membership) if b == 0]
    )
    pooled = mean(
        [x for v, b in zip(control_times, partial_membership) if b == 0 for x in v]
    )
    close(nested, actual["boundaries"]["partial_controls"]["result"][0]["perm_bin_1"])
    assert abs(nested - pooled) > 0.1, (nested, pooled)
    header = (
        (ROOT / "core/src/main/scala/eyes4s/core/Scanpath.scala")
        .read_text()
        .split("package ")[0]
    )
    scala = (
        header
        + "package eyes4s.pointconsumer\n\n// Generated by tools/r-parity/generate_point_sampling.py.\n// format: off\nobject PointSamplingReference:\n"
    )
    scala += "  final case class Template(key: String,stratum: String,values: Vector[Double])\n  final case class Source(key: String,matched: String,stratum: String,x: Vector[Double],y: Vector[Double],onsets: Vector[Long],durations: Vector[Long])\n  final case class Result(normalization: String,cap: Int,key: String,values: Vector[Option[Double]],controls: Option[Vector[Option[Double]]],bins: Vector[Option[Double]],controlBins: Option[Vector[Option[Double]]])\n"

    def vec(v, fn=str):
        return "Vector(" + ",".join(fn(x) for x in v) + ")"

    def ds(v):
        return vec(v, lambda x: repr(float(x)))

    def longs(v):
        return vec(v, lambda x: str(x * 1000) + "L")

    def optional(x):
        return "None" if x is None else "Some(" + repr(float(x)) + ")"

    def ovs(v):
        return vec(v, optional)

    def optvec(v):
        return "None" if v is None else "Some(" + ovs(v) + ")"

    scala += (
        "  val templates = "
        + vec(
            inputs["templates"],
            lambda t: "Template("
            + ",".join(
                [json.dumps(t["key"]), json.dumps(t["stratum"]), ds(t["values"])]
            )
            + ")",
        )
        + "\n"
    )
    scala += (
        "  val sources = "
        + vec(
            inputs["sources"],
            lambda s: "Source("
            + ",".join(
                [
                    json.dumps(s["key"]),
                    json.dumps(s["matched"]),
                    json.dumps(s["stratum"]),
                    ds(s["x"]),
                    ds(s["y"]),
                    longs(s["onset"]),
                    longs(s["duration"]),
                ]
            )
            + ")",
        )
        + "\n"
    )
    scala += (
        "  val queries = "
        + longs(inputs["queries"])
        + "\n  val boundaries = "
        + longs(inputs["bins"])
        + "\n"
    )
    scala += (
        "  val results = Vector(\n"
        + ",\n".join(
            "    Result("
            + ",".join(
                [
                    json.dumps(r["normalization"]),
                    str(r["cap"]),
                    json.dumps(r["key"]),
                    ovs(r["values"]),
                    optvec(r["controls"]),
                    ovs(r["bins"]),
                    optvec(r["control_bins"]),
                ]
            )
            + ")"
            for r in results
        )
        + "\n  )\n// format: on\n"
    )
    doc = dict(
        eyesim_revision=parity.pinned_revision(),
        input_sha256=hashlib.sha256(INPUT.read_bytes()).hexdigest(),
        tolerance=dict(absolute=ORACLE_TOLERANCE, relative=0),
        independent=dict(
            partial_control_nested=nested,
            partial_control_pooled=pooled,
            bin_indices=membership,
        ),
        reference=actual,
    )
    return [(OUTPUT, json.dumps(doc, indent=2) + "\n"), (SCALA, scala)]


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--eyesim", type=Path, required=True)
    p.add_argument("--check", action="store_true")
    args = p.parse_args()
    with parity.r_session("eyes4s-point-sampling-") as r:
        lib = r.install_eyesim(args.eyesim)
        out = r.tmp / "reference.json"
        r.rscript(HERE / "point_sampling.R", lib, INPUT, out)
        actual = json.loads(out.read_text())
    for path, content in artifacts(json.loads(INPUT.read_text()), actual):
        if args.check:
            assert path.read_text() == content, f"Point sampling fixture drift: {path}"
        else:
            path.write_text(content)
    print(
        "Static sampling, all normalizations, finite controls and nested bin means independently verified; boundary failures retained."
    )


if __name__ == "__main__":
    main()

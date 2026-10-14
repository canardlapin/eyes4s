#!/usr/bin/env python3
"""Public weighted cohort; near-zero queries use an independent analytic Gaussian Gram matrix."""
import argparse
import hashlib
import json
import math
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
INPUT = ROOT / "tools/r-parity/fixtures/cases/density-approximation.json"
SCALA = ROOT / "plan/src/test/scala/eyes4s/plan/DensityApproximationInputs.scala"
BOUNDS = (-1.0, -1.0, 7.0, 5.0)
CALIBRATION_SIGMA = 0.8
GaussianProductOracleTolerance = 2e-12


def gram(a, b, sigma):
    aw, bw = sum(p[2] for p in a), sum(p[2] for p in b)
    def term(p, q):
        mx, my = (p[0] + q[0]) / 2, (p[1] + q[1]) / 2
        xshare = (math.erf((BOUNDS[2] - mx) / sigma) - math.erf((BOUNDS[0] - mx) / sigma)) / 2
        yshare = (math.erf((BOUNDS[3] - my) / sigma) - math.erf((BOUNDS[1] - my) / sigma)) / 2
        separation = ((p[0] - q[0])**2 + (p[1] - q[1])**2) / (4 * sigma * sigma)
        return p[2] / aw * q[2] / bw * math.pi * sigma * sigma * math.exp(-separation) * xshare * yshare
    return math.fsum(term(p, q) for p in a for q in b)


def quadrature_gram(points, sigma):
    nodes, weights = np.polynomial.legendre.leggauss(96)
    xs = (nodes + 1) * (BOUNDS[2] - BOUNDS[0]) / 2 + BOUNDS[0]
    ys = (nodes + 1) * (BOUNDS[3] - BOUNDS[1]) / 2 + BOUNDS[1]
    area_weights = np.outer(weights, weights) * (BOUNDS[2] - BOUNDS[0]) * (BOUNDS[3] - BOUNDS[1]) / 4
    fields = []
    for item in points:
        total = sum(p[2] for p in item)
        z = np.zeros((len(ys), len(xs)))
        for x, y, weight in item:
            z += weight / total * np.exp(-((xs[None, :] - x)**2 + (ys[:, None] - y)**2) / (2 * sigma * sigma))
        fields.append(z)
    return [[float(np.sum(left * right * area_weights)) for right in fields] for left in fields]


def create():
    base = [
        [(-.99, -.99, 120000), (1.45, 1.15, 300000), (2.05, 3.35, 60000)],
        [(6.99, 4.99, 320000), (4.65, 2.75, 100000), (.05, 3.95, 80000)],
        [(1.85, .75, 200000), (3.15, 2.35, 160000), (4.30, 4.15, 140000)],
    ]
    items, calibration = [], []
    def clipped(x, y):
        return min(6.99, max(-.99, x)), min(4.99, max(-.99, y))
    def item(participant, stimulus, phase, points):
        return {"participant": participant, "stimulus": stimulus, "phase": phase,
                "points": [[x, y] for x, y, _ in points],
                "durations_us": [int(weight) for _, _, weight in points],
                "onsets_us": [i * 1000000 for i in range(len(points))]}
    for participant, offset in (("P1", 0.0), ("P2", .13)):
        templates = [[(*clipped(x + offset, y - offset / 2), weight) for x, y, weight in points] for points in base]
        analytic = [[gram(a, b, CALIBRATION_SIGMA) for b in templates] for a in templates]
        quadrature = quadrature_gram(templates, CALIBRATION_SIGMA)
        error = max(abs(analytic[i][j] - quadrature[i][j]) for i in range(3) for j in range(3))
        assert error <= GaussianProductOracleTolerance, error
        for index, stimulus in enumerate(("A", "B", "C")):
            points = templates[index]
            controls = [i for i in range(3) if i != index]
            projection = [analytic[j][index] / math.sqrt(analytic[index][index]) -
                          sum(analytic[j][k] / math.sqrt(analytic[k][k]) for k in controls) / 2
                          for j in range(3)]
            control_projection = sum(projection[k] for k in controls) / 2
            q = -control_projection / (projection[index] - control_projection)
            assert 0 < q < 1
            mixture = []
            for k in range(3):
                share = q if k == index else (1 - q) / 2
                total = sum(p[2] for p in templates[k])
                mixture.extend((x, y, max(1, round(1000000 * share * weight / total)))
                               for x, y, weight in templates[k])
            shifted = [(*clipped(x + .06, y + .03), weight) for x, y, weight in points]
            items.extend([item(participant, stimulus, "encode", points),
                          item(participant, stimulus, "matched", shifted),
                          item(participant, stimulus, "near-midpoint", mixture),
                          item(participant, stimulus, "mismatch", templates[(index + 1) % 3])])
            calibration.append({"participant": participant, "stimulus": stimulus, "mixture_target_share": q,
                                "sigma": CALIBRATION_SIGMA, "gram": analytic,
                                "quadrature_max_absolute_difference": error})
    return {"version": 1, "license": "Apache-2.0; independently authored synthetic points and cohort",
            "bounds": list(BOUNDS), "resolutions": [[n, 3 * n // 4] for n in (8, 16, 32, 64, 128)],
            "sigmas": [.2, .4, .8, 1.6], "phases": ["matched", "near-midpoint", "mismatch"],
            "weighting": "fixed positive integer fixation duration in microseconds",
            "controls": "all two nonmatching encoding stimuli within the same participant",
            "sample_counts": "synthetic summary count1; no observed recording support claim",
            "calibration": {"kind": "finite-window analytic products of full Gaussian point kernels",
                            "independent_check": "96x96 Gauss-Legendre quadrature",
                            "absolute_tolerance": GaussianProductOracleTolerance, "rows": calibration},
            "items": items}


def render(value):
    header = (ROOT / "plan/src/main/scala/eyes4s/plan/StudyPlan.scala").read_text().split("package ")[0]
    def vec(values):
        return "Vector(" + ", ".join(values) + ")"
    rows = []
    for item in value["items"]:
        strings = ", ".join(json.dumps(item[k]) for k in ("participant", "stimulus", "phase"))
        points = vec([f"({x!r}, {y!r})" for x, y in item["points"]])
        durations = vec([f"{n}L" for n in item["durations_us"]])
        rows.append(f"    Item({strings}, {points}, {durations})")
    digest = hashlib.sha256((json.dumps(value, indent=2) + "\n").encode()).hexdigest()
    return header + """package eyes4s.plan

// Independently generated by tools/density-approximation/generate_inputs.py.
// format: off
private[plan] object DensityApproximationInputs:
  final case class Item(participant: String, stimulus: String, phase: String, points: Vector[(Double, Double)], durations: Vector[Long])
""" + f'  val inputSha256 = "{digest}"\n' + "  val items = Vector(\n" + ",\n".join(rows) + "\n  )\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    value = create()
    outputs = {INPUT: json.dumps(value, indent=2) + "\n", SCALA: render(value)}
    for path, content in outputs.items():
        if args.check:
            assert path.read_text() == content, f"stale input={path}"
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(content)
    print("Public density cohort checked" if args.check else "Public density cohort generated")


if __name__ == "__main__":
    main()

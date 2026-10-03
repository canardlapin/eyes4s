#!/usr/bin/env python3
"""Pin public eyesim NNLS and rank (partial Spearman) calls against independent exact rational oracles."""
import argparse
from fractions import Fraction as F
import hashlib
import json
import math
from pathlib import Path
import sys

sys.dont_write_bytecode = True
import parity

HERE, ROOT = parity.HERE, parity.ROOT
INPUT = HERE / "fixtures/cases/bd-01M420XJ0Y2H6R3M4B3WXMHA1A.json"
OUTPUT = HERE / "fixtures/decomposition-constrained.json"
SCALA = ROOT / "laws/src/test/scala/eyes4s/laws/ConstrainedDecompositionReference.scala"
TOLERANCE = 1e-12


def norm(xs):
    return [F(x, sum(xs)) for x in xs]


def dot(a, b):
    return sum(x * y for x, y in zip(a, b))


def solve(a, b):
    """Exact Gauss-Jordan; None when singular."""
    p = len(b)
    m = [list(row) + [v] for row, v in zip(a, b)]
    for k in range(p):
        pivot = next((i for i in range(k, p) if m[i][k]), None)
        if pivot is None:
            return None
        m[k], m[pivot] = m[pivot], m[k]
        m[k] = [v / m[k][k] for v in m[k]]
        for i in range(p):
            if i != k:
                f = m[i][k]
                m[i] = [v - f * w for v, w in zip(m[i], m[k])]
    return [row[-1] for row in m]


def nnls(columns, y):
    """Every active set's exact normal equations; the unique one meeting every KKT condition."""
    p, found = len(columns), []
    for mask in range(1 << p):
        active = [j for j in range(p) if mask >> j & 1]
        z = solve(
            [[dot(columns[i], columns[j]) for j in active] for i in active],
            [dot(columns[i], y) for i in active],
        )
        if z is None or any(v <= 0 for v in z):
            continue
        w = [F(0)] * p
        for j, v in zip(active, z):
            w[j] = v
        r = [yi - sum(columns[j][i] * w[j] for j in range(p)) for i, yi in enumerate(y)]
        if all(dot(columns[j], r) <= 0 for j in range(p) if j not in active):
            found.append(w)
    assert len(found) == 1, found
    return found[0]


def ranks(values):
    return [
        F(2 * sum(v < x for v in values) + sum(v == x for v in values) + 1, 2)
        for x in values
    ]


def partial(columns):
    """Signed partial correlation of columns 0 and 1 given the rest, via the inverse covariance."""
    n = len(columns[0])
    centered = [[v - sum(c) / n for v in c] for c in columns]
    cov = [[dot(a, b) for b in centered] for a in centered]
    k = len(columns)
    e0 = solve(cov, [F(int(i == 0)) for i in range(k)])
    e1 = solve(cov, [F(int(i == 1)) for i in range(k)])
    squared = e0[1] * e0[1] / (e0[0] * e1[1])
    return -math.copysign(math.sqrt(float(squared)), e0[1]) if e0[1] else 0.0


def oracle_rank(case, transform):
    y, b, x2 = (
        transform([F(v) for v in case[k]]) for k in ("source", "baseline", "reference")
    )
    return partial([b, y, x2]), partial([x2, y, b])


def vec(xs):
    return "Vector(" + ",".join(repr(float(x)) for x in xs) + ")"


def ints(xs):
    return "Vector(" + ",".join(str(int(x)) for x in xs) + ")"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--eyesim", type=Path, required=True)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    cases = json.loads(INPUT.read_text())
    with parity.r_session("eyes4s-decomposition-constrained-") as session:
        lib = session.install_eyesim(args.eyesim)
        out = session.tmp / "reference.json"
        session.rscript(HERE / "decomposition_constrained.R", lib, INPUT, out)
        reference = json.loads(out.read_text())
    exact = dict(nnls=[], rank=[])
    for case, actual in zip(cases["nnls"], reference["nnls"], strict=True):
        assert actual["terms"] == list(case["maps"]), (case["name"], actual["terms"])
        w = nnls([norm(v) for v in case["maps"].values()], norm(case["response"]))
        for a, b in zip(actual["coefficients"], w, strict=True):
            assert abs(a - float(b)) <= TOLERANCE, (case["name"], a, b)
        # eyesim silently ignores intercept = TRUE for nnls.
        assert actual["coefficients_intercept_true"] == actual["coefficients"], case[
            "name"
        ]
        exact["nnls"].append(dict(name=case["name"], coefficients=[str(v) for v in w]))
    for case, actual in zip(cases["rank"], reference["rank"], strict=True):
        spearman = oracle_rank(case, ranks)
        pearson = oracle_rank(case, lambda c: c)
        observed = (
            (actual["beta_baseline"], actual["beta_source"]),
            (actual["pearson_baseline"], actual["pearson_source"]),
        )
        for got, want in zip(observed, (spearman, pearson)):
            for a, b in zip(got, want):
                assert abs(a - b) <= TOLERANCE, (case["name"], a, b)
        exact["rank"].append(
            dict(name=case["name"], spearman=list(spearman), pearson=list(pearson))
        )
    document = dict(
        eyesim_revision=parity.pinned_revision(),
        input_sha256=hashlib.sha256(INPUT.read_bytes()).hexdigest(),
        tolerance=dict(absolute=TOLERANCE, relative=0),
        exact=exact,
        reference=reference,
    )
    header = (
        (ROOT / "design/src/main/scala/eyes4s/design/Template.scala")
        .read_text()
        .split("package ")[0]
    )
    scala = (
        header
        + "package eyes4s.laws\n\n// Generated by tools/r-parity/generate_decomposition_constrained.py. Do not hand-edit.\n// format: off\nobject ConstrainedDecompositionReference:\n"
    )
    scala += '  /** Integer cell weights, normalized to masses, and pinned eyesim template_multireg(method = "nnls") coefficients. */\n'
    scala += "  final case class Nnls(name: String, predictors: Vector[(String, Vector[Int])], response: Vector[Int], coefficients: Vector[Double])\n"
    scala += '  /** Pinned eyesim template_regression(method = "rank") outputs and ppcor Pearson on the same layout. */\n'
    scala += "  final case class Rank(name: String, source: Vector[Int], baseline: Vector[Int], reference: Vector[Int], betaBaseline: Double, betaSource: Double, pearsonBaseline: Double, pearsonSource: Double)\n"
    rows = []
    for case, actual in zip(cases["nnls"], reference["nnls"]):
        maps = (
            "Vector("
            + ",".join(f"({json.dumps(k)},{ints(v)})" for k, v in case["maps"].items())
            + ")"
        )
        rows.append(
            f'    Nnls({json.dumps(case["name"])},{maps},{ints(case["response"])},{vec(actual["coefficients"])})'
        )
    scala += "  val nnls: Vector[Nnls] = Vector(\n" + ",\n".join(rows) + "\n  )\n"
    rows = []
    for case, actual in zip(cases["rank"], reference["rank"]):
        values = ",".join(
            repr(float(actual[k]))
            for k in (
                "beta_baseline",
                "beta_source",
                "pearson_baseline",
                "pearson_source",
            )
        )
        rows.append(
            f'    Rank({json.dumps(case["name"])},{ints(case["source"])},{ints(case["baseline"])},{ints(case["reference"])},{values})'
        )
    scala += (
        "  val rank: Vector[Rank] = Vector(\n"
        + ",\n".join(rows)
        + "\n  )\n// format: on\n"
    )
    for path, content in {
        OUTPUT: json.dumps(document, indent=2) + "\n",
        SCALA: scala,
    }.items():
        if args.check:
            assert path.read_text() == content, f"Fixture drift: {path}"
        else:
            path.write_text(content)
    print(
        "Constrained decomposition: public eyesim nnls and rank calls agree with exact rational NNLS and inverse-covariance partial correlations."
    )


if __name__ == "__main__":
    main()

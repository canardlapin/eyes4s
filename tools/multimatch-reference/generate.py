#!/usr/bin/env python3
"""Independent exhaustive monotone paths and 80/120-digit costs; no eyes4s imports."""
import argparse
from collections import Counter
from decimal import Decimal, localcontext
from fractions import Fraction
import json
import math
from pathlib import Path
import struct

ROOT = Path(__file__).resolve().parents[2]
SCALA = ROOT / "compare/src/test/scala/eyes4s/compare/MultiMatchAlignmentReference.scala"
JSON = ROOT / "tools/multimatch-reference/fixtures.json"


def paths(n, m):
    def visit(i, j, prefix):
        if (i, j) == (n - 1, m - 1):
            yield prefix
        for di, dj in ((1, 1), (1, 0), (0, 1)):
            if i + di < n and j + dj < m:
                yield from visit(i + di, j + dj, prefix + [(i + di, j + dj)])
    return list(visit(0, 0, [(0, 0)]))


def squared_cells(left, right):
    def vectors(points):
        return [(x1 - x0, y1 - y0) for (x0, y0), (x1, y1) in zip(points, points[1:])]
    return [[(ax - bx)**2 + (ay - by)**2 for bx, by in vectors(right)]
            for ax, ay in vectors(left)]


def symbolic_cost(path, cells):
    # Identical multisets of exact squared distances prove identical radical sums.
    # No tolerance declares an exact tie. Different expressions whose decimal
    # intervals cannot be separated are refused rather than classified as ties.
    return Counter(cells[i][j] for i, j in path[1:] if cells[i][j])


def decimal_cost(expression, precision):
    with localcontext() as context:
        context.prec = precision
        return sum((Decimal(q.numerator) / Decimal(q.denominator)).sqrt() * count
                   for q, count in expression.items())


def traceback_priority(path):
    codes = {(1, 1): 0, (1, 0): 1, (0, 1): 2}
    return tuple(reversed([codes[(i - pi, j - pj)]
                           for (pi, pj), (i, j) in zip(path, path[1:])]))


def median(values):
    ordered = sorted(values)
    n = len(ordered)
    return ordered[n // 2] if n % 2 else (ordered[n // 2 - 1] + ordered[n // 2]) / 2


def components(left, right, path):
    left = [(float(x), float(y)) for x, y in left]
    right = [(float(x), float(y)) for x, y in right]
    a = [(x1 - x0, y1 - y0) for (x0, y0), (x1, y1) in zip(left, left[1:])]
    b = [(x1 - x0, y1 - y0) for (x0, y0), (x1, y1) in zip(right, right[1:])]
    def direction(x, y):
        difference = abs(math.atan2(x[1], x[0]) - math.atan2(y[1], y[0]))
        return min(difference, 2 * math.pi - difference)
    diagonal = 1280.0  # exact 1024 by 768 frame diagonal
    return [
        1 - median([math.hypot(a[i][0] - b[j][0], a[i][1] - b[j][1]) for i, j in path]) / (2 * diagonal),
        1 - median([direction(a[i], b[j]) for i, j in path]) / math.pi,
        1 - median([abs(math.hypot(*a[i]) - math.hypot(*b[j])) for i, j in path]) / diagonal,
        1 - median([math.hypot(left[i][0] - right[j][0], left[i][1] - right[j][1]) for i, j in path]) / diagonal,
        1.0,  # all fixation durations are exactly 100ms
    ]


def witness(name, epsilon):
    left = [(Fraction(x), Fraction(y)) for x, y in ((100, 100), (100, 103), (104, 106))]
    right = [(Fraction(200), Fraction(200)), (Fraction(201), Fraction(205)),
             (Fraction(203) + epsilon, Fraction(206)), (Fraction(208), Fraction(206))]
    # Coordinates are exactly representable binary64 values, including the near ties.
    assert all(Fraction.from_float(float(v)) == v for points in (left, right) for p in points for v in p)
    cells = squared_cells(left, right)
    all_paths = paths(len(left) - 1, len(right) - 1)
    expressions = [symbolic_cost(path, cells) for path in all_paths]
    costs80 = [decimal_cost(expression, 80) for expression in expressions]
    costs120 = [decimal_cost(expression, 120) for expression in expressions]
    winner = min(range(len(all_paths)), key=lambda i: costs120[i])
    optimal = [i for i, expression in enumerate(expressions) if expression == expressions[winner]]
    for i in range(len(all_paths)):
        if i not in optimal:
            # At most four terms with magnitude below 100. This guard is vastly
            # wider than their 80-digit sqrt/add rounding intervals, and is only
            # an oracle separation check, never a tie-selection tolerance.
            assert costs80[i] - costs80[winner] > Decimal("1e-70")
            assert costs120[i] > costs120[winner]
    selected = min(optimal, key=lambda i: traceback_priority(all_paths[i]))
    return {
        "name": name,
        "epsilon": str(epsilon),
        "left": [[float(x), float(y)] for x, y in left],
        "right": [[float(x), float(y)] for x, y in right],
        "coordinate_bits": [[struct.pack(">d", float(v)).hex() for p in points for v in p]
                            for points in (left, right)],
        "middle_squared_cost_difference": str(cells[0][1] - cells[1][1]),
        "enumerated_path_count": len(all_paths),
        "optimal_paths": [all_paths[i] for i in optimal],
        "selected_path": all_paths[selected],
        "minimum_cost_80_digits": str(costs80[winner]),
        "minimum_cost_120_digits": str(costs120[winner]),
        "expected": components(left, right, all_paths[selected]),
        "optimal_components": [components(left, right, all_paths[i]) for i in optimal],
        "all_path_costs": [{"path": path, "cost_80_digits": str(costs80[i]),
                            "exact_radical_terms": [[str(q), count] for q, count in sorted(expressions[i].items())]}
                           for i, path in enumerate(all_paths)],
    }


def render(cases):
    header = (ROOT / "compare/src/test/scala/eyes4s/compare/MultiMatchConformanceSuite.scala").read_text().split("package eyes4s.compare")[0]
    def points(values):
        return "Vector(" + ", ".join(f"({x!r}, {y!r})" for x, y in values) + ")"
    def path(values):
        return "Vector(" + ", ".join(f"({i}, {j})" for i, j in values) + ")"
    rows = []
    for case in cases:
        rows.append("    Case(" + json.dumps(case["name"]) + ", " + points(case["left"]) + ", " + points(case["right"]) + ", " + path(case["selected_path"]) + ", Vector(" + ", ".join(repr(v) for v in case["expected"]) + "))")
    return header + """package eyes4s.compare

// Generated by tools/multimatch-reference/generate.py; no eyes4s numeric code used.
// format: off
private[compare] object MultiMatchAlignmentReference:
  final case class Case(name: String, left: Vector[(Double, Double)], right: Vector[(Double, Double)], selected: Vector[(Int, Int)], expected: Vector[Double])
  val cases: Vector[Case] = Vector(
""" + ",\n".join(rows) + "\n  )\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    delta = Fraction(1, 2**42)
    cases = [witness("exact-radical-tie", Fraction(0)),
             witness("near-tie-positive", delta), witness("near-tie-negative", -delta)]
    value = {"version": 1, "scope": "independent exact rational coordinates, exhaustive paths and 80/120-digit costs; component reduction oracle uses Python math at named 1e-12 tolerance",
             "frame": [1024, 768], "fixation_duration_us": 100000, "onset_spacing_us": 150000,
             "tie_identity": "equal multisets of exact squared distances, origin excluded",
             "cases": cases}
    outputs = {SCALA: render(cases), JSON: json.dumps(value, indent=2) + "\n"}
    for path, content in outputs.items():
        if args.check:
            assert path.read_text() == content, f"stale fixture={path}"
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(content)
    print("Independent MultiMatch fixture generation checked" if args.check else "Independent MultiMatch fixtures generated")


if __name__ == "__main__":
    main()

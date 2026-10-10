import copy
import math
import unittest

from end_to_end import MAP_TOLERANCE, analyze, assert_close, axes, cosine, gaussian, validate
from synthetic import request


class DensityOracleTests(unittest.TestCase):
    def test_identity_and_exclusion_guards(self):
        p = request()
        for change in ("match", "edge", "duplicate", "weight", "missing", "pool"):
            q = copy.deepcopy(p)
            if change == "match":
                q["sources"][0]["participant"] = "P2"
            elif change == "edge":
                q["sources"][0]["fixations"]["x"][0] = q["x_max"]
            elif change == "duplicate":
                q["sources"][0]["id"] = q["templates"][0]["id"]
            elif change == "weight":
                q["sources"][0]["fixations"]["duration_ms"][0] = 0
            elif change == "missing":
                q["sources"][0]["fixations"]["y"].pop()
            else:
                q["sources"] = [s for s in q["sources"] if s["template"] != q["templates"][0]["id"]]
            with self.subTest(change=change), self.assertRaises(ValueError):
                validate(q)

    def test_grid_is_asymmetric_x_fastest_and_centers_differ_from_endpoints(self):
        p = request()
        xs, ys = axes(p)
        self.assertEqual((xs[0], xs[-1], ys[0], ys[-1]), (65, 375, 45, 235))
        self.assertEqual((axes(p, False)[0][0], axes(p, False)[1][-1]), (60, 240))
        item = {"fixations": {"x": [65], "y": [95], "duration_ms": [1]}}
        m = gaussian(p, item, xs, ys)
        self.assertEqual(m.index(max(m)), 5 * p["nx"])
        self.assertAlmostEqual(math.fsum(m), 1)

    def test_oracle_detects_weight_sigma_axis_and_edge_policy_mutations(self):
        p = request()
        xs, ys = axes(p)
        item = p["templates"][0]
        expected = gaussian(p, item, xs, ys, True, True)
        mutants = []
        q = copy.deepcopy(item)
        q["fixations"]["duration_ms"] = [1, 1, 1]
        mutants.append(gaussian(p, q, xs, ys, True, True))
        q = dict(p, sigma_px=p["sigma_px"] / 4)
        mutants.append(gaussian(q, item, xs, ys, True, True))
        mutants.append(list(reversed(expected)))
        f = item["fixations"]
        parts = [gaussian(p, {"fixations": {"x": [x], "y": [y], "duration_ms": [d]}},
                          xs, ys, True, True)
                 for x, y, d in zip(f["x"], f["y"], f["duration_ms"])]
        total = sum(f["duration_ms"])
        mutants.append([sum(part[i] * d for part, d in zip(parts, f["duration_ms"])) / total
                        for i in range(len(expected))])
        for mutant in mutants:
            with self.assertRaises(ValueError):
                assert_close(mutant, expected, MAP_TOLERANCE, "mutated map")
        self.assertAlmostEqual(cosine(expected, expected), 1)

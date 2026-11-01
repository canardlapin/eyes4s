"""Synthetic metadata/pairing regressions; no downloaded data in the test suite."""
import csv
from pathlib import Path
import tempfile
import unittest

import adapters


def image(name, points, category="scene"):
    return {"filename": name, "category": category, "userdata": [
        {"SDT": [4, 1, 3], "fixations": {"enc": points, "rec": points, "rec2": []}}
    ]}


class FigrimAdapterTests(unittest.TestCase):
    def test_accounting_identity_and_analytic_limits(self):
        images = [image("a", [[1, 1], [1000, 1000], [1001, 20]]),
                  image("b", [[500, 500]])]
        with tempfile.TemporaryDirectory() as d:
            p = Path(d)
            expected, counts = adapters.prepare_figrim(images, p, [])
            self.assertEqual(counts["source_locations"], 8)
            self.assertEqual(counts["admitted_locations"], 6)
            self.assertEqual(counts["excluded_locations"], 2)
            self.assertEqual(counts["pairs"], 2)
            self.assertEqual(counts["missing_or_empty_phases"], 2)
            for matched, control in expected.values():
                self.assertAlmostEqual(matched, 1)
                self.assertEqual(control, 0)
            self.assertNotIn("duration", (p / "locations.tsv").read_text())
            self.assertIn("\t0\t0\n", (p / "locations.tsv").read_text())
            self.assertIn("\t999\t999\n", (p / "locations.tsv").read_text())
            with (p / "trials.csv").open() as f:
                rows = list(csv.DictReader(f))
            self.assertEqual({r["sdt"] for r in rows}, {"1", "4"})

    def test_duplicate_images_never_form_controls(self):
        images = [image("a", [[1, 1]]), image("b", [[1, 1]]), image("c", [[900, 900]])]
        with tempfile.TemporaryDirectory() as d:
            p = Path(d)
            adapters.prepare_figrim(images, p, [["a", "b"]])
            with (p / "pair-metadata.csv").open() as f:
                rows = list(csv.DictReader(f))
            for r in rows:
                if r["image"] in {"a", "b"}:
                    self.assertEqual(r["control_image"], "c")

    def test_no_cross_observer_or_cross_category_control(self):
        images = [image("a", [[1, 1]]), image("b", [[900, 900]], "other")]
        with tempfile.TemporaryDirectory() as d:
            with self.assertRaisesRegex(ValueError, "No eligible"):
                adapters.prepare_figrim(images, Path(d), [])

    def test_reject_wrong_shape_and_duplicate_source_identity(self):
        with tempfile.TemporaryDirectory() as d:
            with self.assertRaisesRegex(ValueError, "Nx2"):
                adapters.prepare_figrim([image("a", [[1, 2, 3]])], Path(d), [])
            with self.assertRaisesRegex(ValueError, "Duplicate source"):
                adapters.prepare_figrim([image("a", [[1, 2]]), image("a", [[3, 4]])], Path(d), [])

    def test_result_gate_rejects_mutated_missing_duplicate_and_nonfinite_scores(self):
        expected = {"p0": (1., 0.)}
        with tempfile.TemporaryDirectory() as d:
            p = Path(d) / "scores.tsv"
            header = ["pair_id", "matched", "control", "difference"]
            adapters.write_table(p, header, [["p0", 1, 0, 1]])
            self.assertEqual(adapters.verify_results(p, expected), 0)
            for bad in ([["p0", 0, 0, 0]], [], [["p0", 1, 0, 1]] * 2, [["p0", "nan", 0, 1]]):
                adapters.write_table(p, header, bad)
                with self.assertRaises(ValueError):
                    adapters.verify_results(p, expected)


if __name__ == "__main__":
    unittest.main()

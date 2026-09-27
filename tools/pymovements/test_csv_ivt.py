"""Fail-closed checks for the PM3.2 paired collector."""

import copy
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parent))
import run_csv_ivt  # noqa: E402


class PairedResultSuite(unittest.TestCase):
    def setUp(self):
        output = {
            "columns": ["onset_us", "stop_us", "samples", "centre_x", "centre_y", "rms"],
            "units": {"time": "us", "position": "deg", "dispersion": "deg"},
            "rows": 40,
            "canonical_sha256": "a" * 64,
        }
        self.pair = [
            {"status": "ok", "output": copy.deepcopy(output)},
            {"status": "ok", "output": copy.deepcopy(output)},
        ]

    def test_matching_pair_passes(self):
        run_csv_ivt.require_pair(self.pair, "small", "cold", 0)

    def test_scientific_mismatch_is_rejected(self):
        for name, different in (
            ("canonical_sha256", "b" * 64),
            ("rows", 39),
            ("units", {"time": "ms", "position": "deg", "dispersion": "deg"}),
        ):
            with self.subTest(name=name):
                pair = copy.deepcopy(self.pair)
                pair[1]["output"][name] = different
                with self.assertRaisesRegex(RuntimeError, "semantic mismatch"):
                    run_csv_ivt.require_pair(pair, "small", "cold", 0)

    def test_failed_child_cannot_be_scored(self):
        pair = copy.deepcopy(self.pair)
        pair[1]["status"] = "timeout"
        pair[1]["output"] = None
        with self.assertRaisesRegex(RuntimeError, "failed process"):
            run_csv_ivt.require_pair(pair, "large", "warm", 7)


if __name__ == "__main__":
    unittest.main()

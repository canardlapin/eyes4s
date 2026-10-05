"""Fail-closed checks for the PM3.2 paired collector."""

import copy
import json
import os
from pathlib import Path
import sys
import tempfile
import time
import unittest
from unittest.mock import patch

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

    def test_throughput_uses_process_wall_or_median_warm_iteration(self):
        item = {"status": "ok", "process_instance": "p0", "input_identity_sha256": "a" * 64,
                "mode": "warm", "wall_ns": [1_000_000_000, 2_000_000_000, 9_000_000_000]}
        result = run_csv_ivt.throughput(item, {"rows": 100, "bytes": 300})
        self.assertEqual(result["rows_per_second"], 50)
        self.assertEqual(result["bytes_per_second"], 150)
        item["status"] = "error"
        with self.assertRaisesRegex(ValueError, "no throughput"):
            run_csv_ivt.throughput(item, {"rows": 100, "bytes": 300})

    def test_unlisted_high_cpu_process_blocks_and_retains_stage_evidence(self):
        idle = "123 1.2 /usr/bin/python3\n456 0.0 /Applications/Blender.app/Blender\n"
        busy = "123 1.2 /usr/bin/python3\n456 190.5 /Applications/Blender.app/Blender\n"
        with tempfile.TemporaryDirectory() as temporary:
            output_dir = Path(temporary)
            with patch.object(run_csv_ivt.subprocess, "check_output",
                              side_effect=[idle, busy]) as observed:
                run_csv_ivt.check_interference(output_dir, "large", "warm", 7,
                                               "before-pair")
                with self.assertRaisesRegex(RuntimeError, "after-eyes4s"):
                    run_csv_ivt.check_interference(output_dir, "large", "warm", 7,
                                                   "after-eyes4s")
            self.assertEqual(observed.call_count, 2)
            rows = [json.loads(line) for line in
                    (output_dir / "interference.jsonl").read_text().splitlines()]
            self.assertEqual([row["stage"] for row in rows],
                             ["before-pair", "after-eyes4s"])
            self.assertEqual(rows[0]["busy"], [])
            self.assertEqual(rows[1]["busy"],
                             [["456", "190.5", "/Applications/Blender.app/Blender"]])


class CollectorLifecycleSuite(unittest.TestCase):
    def test_full_protocol_requires_source_bound_build_provenance(self):
        run_csv_ivt.validate_rounds(19, 20)
        with self.assertRaisesRegex(ValueError, "source-bound build provenance"):
            run_csv_ivt.validate_rounds(20, 20)
        with self.assertRaisesRegex(ValueError, "outside protocol"):
            run_csv_ivt.validate_rounds(21, 20)

    def test_timeout_kills_owned_process_group_and_retains_raw_logs(self):
        with tempfile.TemporaryDirectory() as temporary:
            output_dir = Path(temporary)
            leaked = output_dir / "leaked"
            started = output_dir / "started"
            descendant = (
                f"import pathlib, time; pathlib.Path({str(started)!r}).write_text('started'); "
                "time.sleep(1); "
                f"pathlib.Path({str(leaked)!r}).write_text('leaked')"
            )
            child = (
                "import subprocess, sys, time; "
                f"subprocess.Popen([sys.executable, '-c', {descendant!r}]); "
                "time.sleep(60)"
            )
            result = run_csv_ivt.run_child(
                [sys.executable, "-c", child], output_dir, "timeout", os.environ.copy(), 1,
            )
            self.assertEqual(result["status"], "timeout")
            self.assertTrue(started.exists(), "synthetic descendant never started")
            self.assertTrue((output_dir / "timeout.stdout").exists())
            self.assertTrue((output_dir / "timeout.stderr").exists())
            time.sleep(1.5)
            self.assertFalse(leaked.exists(), "benchmark descendant survived collector timeout")

    def test_interruption_kills_and_drains_owned_process_group(self):
        class InterruptedChild:
            pid = 4242

            def __init__(self):
                self.calls = []

            def communicate(self, timeout=None):
                self.calls.append(timeout)
                if timeout is not None:
                    raise KeyboardInterrupt
                return b"partial stdout", b"partial stderr"

        child = InterruptedChild()
        with tempfile.TemporaryDirectory() as temporary:
            with patch.object(run_csv_ivt.subprocess, "Popen", return_value=child) as popen, \
                 patch.object(run_csv_ivt.os, "killpg") as killpg:
                with self.assertRaises(KeyboardInterrupt):
                    run_csv_ivt.run_child([], Path(temporary), "interrupt", {}, 1)
        self.assertEqual(child.calls, [1, None])
        killpg.assert_called_once_with(4242, run_csv_ivt.signal.SIGKILL)
        self.assertTrue(popen.call_args.kwargs["start_new_session"])


if __name__ == "__main__":
    unittest.main()

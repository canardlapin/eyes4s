"""Synthetic-only regression tests for the private fixture adapter and oracle."""

import copy
import csv
import importlib.util
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location("private_fixture_run", Path(__file__).with_name("run.py"))
run = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = run
SPEC.loader.exec_module(run)


def config():
    return {
        "schema_version": 1, "coordinates": "screen-pixels-y-down",
        "width_px": 100, "height_px": 80, "time_unit": "milliseconds",
        "sample_rate_hz": 250, "ordinal": "source-record",
        "sources": [{"file": "synthetic.csv", "columns": {
            "participant": "pid", "phase": "period", "trial": ["block", "trial"],
            "item": "item", "x": "x", "y": "y", "onset": "onset", "duration": "duration",
        }}],
    }


def fixture(directory):
    path = directory / "synthetic.csv"
    with path.open("w", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["pid", "period", "block", "trial", "item", "x", "y", "onset", "duration"])
        writer.writerows([
            ['person, "A"\nsecond line', "encode", "a,b", "c", "scene", 0, 0, "-0.0005", 10],
            ['person, "A"\nsecond line', "encode", "a,b", "c", "scene", 3, 4, 20, 20],
            ['person, "A"\nsecond line', "encode", "a", "b,c", "scene", 100, 80, 0, 1],
            ['person, "A"\nsecond line', "recall", "a,b", "c", "scene", -1, 1, 0, 4],
        ])
    return path


class AdapterSuite(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="eyes4s-private-synthetic-")
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name).resolve()
        self.config = config()
        self.source = self.config["sources"][0]
        self.path = fixture(self.root)

    def actual(self):
        expected, outside = run.reference(self.source, self.path, self.config)
        return expected, outside, {
            "source_rows": 4, "admitted_rows": 4, "outside_frame_rows": outside,
            "rejected": [], "roundtrip_trials": len(expected),
            "trials": [{"key": list(k), **v} for k, v in expected.items()],
        }

    def test_composite_trial_keys_periods_and_quoted_identifiers_are_preserved(self):
        directory = self.root / "prepared"
        run.prepare(self.source, self.path, directory, self.config)
        with (directory / "input.csv").open(newline="") as f:
            rows = list(csv.DictReader(f))
        self.assertEqual([int(r["ordinal"]) for r in rows], [2, 3, 4, 5])
        self.assertEqual(rows[0]["participant"], 'person, "A"\nsecond line')
        self.assertNotEqual(rows[0]["trial"], rows[2]["trial"])
        expected, outside, actual = self.actual()
        self.assertEqual(len(expected), 3)
        self.assertEqual(outside, [4, 5])
        metrics = next(v for v in expected.values() if v["fixations"] == 2)
        self.assertEqual(metrics["dwell_us"], 30000)
        self.assertEqual(metrics["path_length_px"], 5)
        self.assertEqual(metrics["derived_samples"], 8)
        self.assertEqual(metrics["first_onset_us"], 0)
        run.compare(actual, expected, outside, 4)

    def test_oracle_detects_lost_rows_misgrouping_clipping_and_changed_summaries(self):
        expected, outside, actual = self.actual()
        mutations = [
            lambda a: a.update(admitted_rows=3),
            lambda a: a.update(outside_frame_rows=[]),
            lambda a: a["trials"].pop(),
            lambda a: a["trials"].append(a["trials"][0]),
            lambda a: a["trials"][0].update(dwell_us=0),
            lambda a: a["trials"][0].update(path_length_px=0),
            lambda a: a.update(roundtrip_trials=0),
        ]
        for mutate in mutations:
            altered = copy.deepcopy(actual)
            mutate(altered)
            with self.assertRaises(ValueError):
                run.compare(altered, expected, outside, 4)

    def test_missing_empty_header_only_and_malformed_files_fail(self):
        columns = self.source["columns"]
        for text in ["", "pid,period\n", self.path.read_text().splitlines()[0] + "\n",
                     self.path.read_text() + "short,row\n", '"unclosed']:
            self.path.write_text(text)
            with self.assertRaises((ValueError, csv.Error)):
                list(run.read_rows(self.path, columns))
        self.path.unlink()
        with self.assertRaises(ValueError):
            list(run.read_rows(self.path, columns))

    def test_git_checkouts_and_symlink_escapes_are_refused(self):
        checkout = self.root / "checkout"
        checkout.mkdir()
        (checkout / ".git").write_text("gitdir: elsewhere")
        alias = self.root / "alias"
        alias.symlink_to(checkout, target_is_directory=True)
        for path in (checkout / "target/private", alias / "new-output"):
            with self.assertRaises(ValueError):
                run.outside_git(path)
        data = self.root / "data"
        data.mkdir()
        (data / "synthetic.csv").symlink_to(self.path)
        with self.assertRaises(ValueError):
            run.sources_in(self.config, data)

    def test_ci_and_invalid_units_are_refused(self):
        manifest = self.root / "config.json"
        run.dump(manifest, self.config)
        with patch.dict(os.environ, {"CI": "true"}):
            self.assertEqual(run.main(["--data-root", str(self.root), "--config", str(manifest),
                                       "--preflight-only"]), 1)
        for field, value in [("time_unit", "guess"), ("sample_rate_hz", 0),
                             ("coordinates", "degrees"), ("ordinal", "sort-by-time")]:
            altered = dict(self.config, **{field: value})
            with self.assertRaises(ValueError):
                run.validate_config(altered)

    def test_preflight_never_builds(self):
        manifest = self.root / "config.json"
        run.dump(manifest, self.config)
        with patch.dict(os.environ, {}, clear=True), patch.object(run, "build") as build:
            self.assertEqual(run.main(["--data-root", str(self.root), "--config", str(manifest),
                                       "--preflight-only"]), 0)
            build.assert_not_called()


if __name__ == "__main__":
    unittest.main()

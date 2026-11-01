"""Analytic admission cases and independent-decoder/mutation regressions."""

import csv
import copy
import json
import math
from pathlib import Path
import shutil
import tempfile
import unittest

import numpy as np
import scipy.io

import adapters
from figrim_contract import admit, configuration_error, families
from figrim_audit import audit, compare_admission, execution_pairs, run_octave


def user(points, **overrides):
    result = {"trial": float("nan"), "RECORDING_SESSION_LABEL": "session-A", "sinit": "label-A",
              "SDT": [-1, 1, 99], "fixations": {"enc": points, "rec": [[2, 3]], "rec2": []}}
    result.update(overrides)
    return result


def fixture():
    empty = {k: [] for k in user([])}
    return [{"filename": "a", "category": "scene", "userdata": [user([
        [0.5, 1.49], [2.5, 4.5], [10.49, 6.49], [-0.5, 2],
        [10.5, 6], [float("nan"), 1], [float("inf"), 1], [4.25, 2.75]]), empty]},
        {"filename": "b", "category": "scene", "userdata": [user([[1, 1]],
            fixations={"enc": [[1, 1]], "rec": [[3, 5]]}), empty]}]


def matlab_struct(records):
    """Encode synthetic structs without depending on the production admission."""
    fields = list(records[0])
    a = np.empty((1, len(records)), dtype=[(f, object) for f in fields])
    for index, record in enumerate(records):
        for name, value in record.items():
            if isinstance(value, dict):
                value = matlab_struct([value])
            elif isinstance(value, list) and value and isinstance(value[0], dict):
                value = matlab_struct(value)
            a[name][0, index] = value
    return a


def read(path):
    with path.open(newline="") as f:
        return list(csv.DictReader(f, delimiter="\t"))


class FigrimContractTests(unittest.TestCase):
    def test_configuration_cannot_qualify_unexecuted_axes_grid_or_duplicate_contract(self):
        m = json.loads(Path(__file__).with_name("manifest.json").read_text())
        original = next(d for d in m["datasets"] if d["id"] == "figrim")
        self.assertIsNone(configuration_error(original))
        for field, value in (("geometry", dict(original["analysis"]["geometry"], y="up")),
                             ("grid", [5, 20]), ("duplicate_image_pairs", [])):
            mutated = copy.deepcopy(original)
            mutated["analysis"][field] = value
            self.assertIsNotNone(configuration_error(mutated))
            self.assertEqual(audit(mutated, Path("/not-used"), {})["status"], "BLOCKED")
            self.assertEqual(adapters.check(mutated, Path("/not-used"), {})["status"], "BLOCKED")

    def test_pair_gate_checks_executed_ids_not_parallel_descriptive_metadata(self):
        with tempfile.TemporaryDirectory() as tmp:
            d = Path(tmp)
            adapters.prepare_figrim(fixture(), d, [])
            before = execution_pairs(d)
            self.assertTrue(before)
            descriptive = (d / "pair-metadata.csv").read_bytes()
            actual = read(d / "pairs.tsv")
            actual[0]["encoding_id"], actual[0]["control_id"] = actual[0]["control_id"], actual[0]["encoding_id"]
            with (d / "pairs.tsv").open("w", newline="") as f:
                writer = csv.DictWriter(f, fieldnames=list(actual[0]), delimiter="\t", lineterminator="\n")
                writer.writeheader(); writer.writerows(actual)
            self.assertEqual(descriptive, (d / "pair-metadata.csv").read_bytes())
            with self.assertRaisesRegex(ValueError, "execution identities"):
                execution_pairs(d)

    def test_nonsquare_asymmetric_rounding_and_lossless_dispositions(self):
        with tempfile.TemporaryDirectory() as tmp:
            d = Path(tmp)
            trials, summary = admit(fixture(), d, [], 10, 6, 5, 3)
            self.assertEqual(summary["counts"]["phase_records"], 12)
            self.assertEqual(summary["counts"]["source_locations"], 11)
            self.assertEqual(summary["counts"]["excluded_locations"], 4)
            self.assertEqual(summary["counts"]["unknown_trial_records"], 2)
            trial = trials[(1, "a", "enc")]
            self.assertEqual(trial.ordinals.tolist(), [1, 2, 3, 8])
            self.assertEqual(trial.points.tolist(), [[0, 0], [2, 4], [9, 5], [3, 2]])
            bins = {(int(r["bin_x"]), int(r["bin_y"])): int(r["count"]) for r in read(d / "bins.tsv")
                    if r["image_index"] == "1" and r["phase"] == "enc"}
            self.assertEqual(bins, {(0, 0): 1, (1, 2): 1, (4, 2): 1, (1, 1): 1})
            phases = read(d / "phase-ledger.tsv")
            self.assertEqual(sum(r["slot_state"] == "empty_slot" for r in phases), 6)
            self.assertEqual(sum(r["gaze_state"] == "missing_phase" for r in phases), 1)
            self.assertEqual(sum(r["gaze_state"] == "empty_phase" for r in phases), 1)
            self.assertEqual([r["response_state"] for r in phases[:3]], ["missing_sentinel", "recorded", "unknown_code"])
            raw = read(d / "location-ledger.tsv")
            self.assertEqual(raw[3]["rounded_x"], "-1.0")
            self.assertEqual(float(raw[7]["raw_x"]), 4.25)
            self.assertTrue(math.isnan(float(raw[5]["raw_x"])))
            self.assertEqual(raw[3]["x_px"], "")
            self.assertIsNone(summary["identity"]["independent_participant_count"])

    def test_all_invalid_phase_is_accounted_and_codes_are_not_coerced(self):
        images = fixture()
        images[0]["userdata"][0]["fixations"]["enc"] = [[-20, -20]]
        with tempfile.TemporaryDirectory() as tmp:
            d = Path(tmp)
            trials, summary = admit(images, d, [], 10, 6, 5, 3)
            self.assertNotIn((1, "a", "enc"), trials)
            self.assertEqual(summary["counts"]["all_invalid_phases"], 1)
            images[0]["userdata"][0]["SDT"] = [4, 1.2, 3]
            with self.assertRaisesRegex(ValueError, "finite integer"):
                admit(images, d, [])
            images[0]["userdata"][0]["SDT"] = [4, 1]
            with self.assertRaisesRegex(ValueError, "SDT width"):
                admit(images, d, [])

    def test_duplicate_family_is_transitive_without_dropping_originals(self):
        self.assertEqual(families(["a", "b", "c", "d"], [["a", "b"], ["b", "c"]]),
                         {"a": "a", "b": "a", "c": "a", "d": "d"})

    def test_scala_bin_gate_catches_axis_swap_even_when_cosine_is_preserved(self):
        with tempfile.TemporaryDirectory() as tmp:
            d = Path(tmp)
            adapters.write_table(d / "trials.csv", ["trial_id", "image_index", "observer_slot", "phase"],
                                 [["t0", 1, 1, "enc"]], ",")
            adapters.write_table(d / "bins.tsv", ["image_index", "observer_slot", "phase", "bin_x", "bin_y", "count"],
                                 [[1, 1, "enc", 2, 7, 1]])
            header = ["trial_id", "bin_x", "bin_y", "count"]
            adapters.write_table(d / "scala-bins.tsv", header, [["t0", 2, 7, 1]])
            adapters.verify_bins(d)
            adapters.write_table(d / "scala-bins.tsv", header, [["t0", 7, 2, 1]])
            with self.assertRaisesRegex(ValueError, "identities/counts"):
                adapters.verify_bins(d)

    @unittest.skipUnless(shutil.which("octave"), "Optional Octave independent decoder")
    def test_independent_raw_decoder_and_metadata_coordinate_mutants(self):
        with tempfile.TemporaryDirectory() as tmp:
            d = Path(tmp); py = d / "python"; oct = d / "octave"
            py.mkdir(); oct.mkdir()
            source = d / "synthetic.mat"
            scipy.io.savemat(source, {"allImages": matlab_struct(fixture())})
            decoded = scipy.io.loadmat(source, simplify_cells=True)["allImages"]
            admit(decoded, py, [], 10, 6, 5, 3)
            run_octave(source, oct, 10, 6, 5, 3)
            self.assertEqual(compare_admission(py, oct)["location-ledger.tsv"], 11)
            # Each mutant changes an observable contract, not the expected formula.
            mutants = [("phase-ledger.tsv", "observer_slot", "2"),
                       ("phase-ledger.tsv", "sdt", "4"),
                       ("phase-ledger.tsv", "phase", "rec2"),
                       ("location-ledger.tsv", "ordinal", "99"),
                       ("location-ledger.tsv", "raw_x", "0.25"),
                       ("location-ledger.tsv", "rounded_x", "0"),
                       ("bins.tsv", "bin_y", "2")]
            for filename, field, value in mutants:
                path = oct / filename
                original = path.read_text()
                rows = read(path)
                rows[0][field] = value
                with path.open("w", newline="") as f:
                    writer = csv.DictWriter(f, fieldnames=list(rows[0]), delimiter="\t", lineterminator="\n")
                    writer.writeheader(); writer.writerows(rows)
                with self.assertRaises(ValueError, msg=f"surviving mutant: {filename}/{field}"):
                    compare_admission(py, oct)
                path.write_text(original)


if __name__ == "__main__":
    unittest.main()

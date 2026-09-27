"""Independent counterexamples for the pre-measurement contract."""

import copy
import json
from pathlib import Path
import tempfile
import unittest

from jsonschema import ValidationError

import fixtures
import performance as perf


class PerformanceProtocolTests(unittest.TestCase):
    def setUp(self):
        self.config = perf.read(perf.ROOT / perf.CONFIG)

    def receipt(self, scope=("pipeline-csv-ivt",)):
        config = self.config
        work = {w["id"]: w for w in config["workloads"]}
        datasets = {d["id"]: d for d in config["datasets"]}
        data = {
            "schema_version": 1, "phase": "baseline", "protocol_sha256": perf.digest(config),
            "scope": list(scope), "machine": copy.deepcopy(config["machine"]),
            "runtime": copy.deepcopy(config["runtime"]),
            "revisions": {"eyes4s": {"commit": "1" * 40, "tree": "2" * 40, "dirty": False,
                                      "artifact_sha256": "3" * 64},
                          "pymovements": {"commit": config["comparator"]["source_commit"],
                                          "wheel_sha256": config["comparator"]["wheel_sha256"]}},
            "adapter_sha256": "4" * 64,
            "dependency_lock_sha256": config["bindings"]["dependency_lock"]["sha256"],
            "conditions": {"ac_power": True, "low_power_mode": False, "competing_compute": False,
                           "thermal_settled": True, "os_file_cache": "primed", "notes": "synthetic test only"},
            "samples": [],
        }
        for id in scope:
            w = work[id]
            sides = ["eyes4s"] if w["comparison"] == "absolute" else ["eyes4s", "pymovements"]
            for scale, input_id in w["datasets"].items():
                for mode in w["modes"]:
                    for round_id in range(20):
                        for side in sides:
                            sample = {
                                "workload": id, "scale": scale, "mode": mode, "round": round_id,
                                "side": side, "order": 0 if len(sides) == 1 else int((side == "eyes4s") != (round_id % 2 == 0)),
                                "process_instance": f"{id}-{scale}-{mode}-{round_id}-{side}",
                                "command": ["synthetic-test-receipt-not-a-benchmark"], "status": "ok", "exit_code": 0,
                                "failure": None, "input_identity_sha256": datasets[input_id]["sha256"],
                                "materialized_input_sha256": datasets[input_id]["sha256"], "config_sha256": perf.digest(w),
                                "warmups": 0 if mode == "cold" else 5,
                                "wall_ns": [1_000_000] * config["measurement"][mode + "_samples_per_process"],
                                "startup_ns": 1000 if mode == "cold" else None, "peak_rss_bytes": 1_000_000,
                                "output": {"columns": w["output_columns"], "units": copy.deepcopy(w["output_units"]),
                                           "rows": 10, "canonical_sha256": "5" * 64, "native_sha256": "6" * 64},
                                "diagnostics": {"receipt_sha256": "7" * 64, **{
                                    k: {"value": 10, "instrument": "synthetic separate diagnostic run", "unavailable_reason": None}
                                    for k in ("allocated_bytes", "retained_heap_bytes", "gc_pause_ns", "gc_collections")}},
                            }
                            data["samples"].append(sample)
        return data

    def reject(self, data, message, complete=False):
        with self.assertRaisesRegex((ValueError, ValidationError), message):
            perf.validate_receipt(data, self.config, complete=complete)

    def test_protocol_has_every_workload_and_fixed_budget(self):
        work, inputs = perf.validate_protocol(self.config)
        self.assertEqual(len(work), 23)
        self.assertEqual(len(inputs), 24)
        self.assertEqual(work["median"]["comparison"], "absolute")
        self.assertEqual(work["idt-churn"]["comparison"], "descriptive")

    def test_document_and_schema_are_bound_to_protocol_identity(self):
        for name in ("protocol_doc", "receipt_schema"):
            changed = copy.deepcopy(self.config)
            changed["bindings"][name]["sha256"] = "0" * 64
            with self.assertRaisesRegex(ValueError, "changed binding"):
                perf.validate_protocol(changed)

    def test_readable_table_cannot_keep_stale_budgets(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            page = root / perf.DOC
            page.parent.mkdir(parents=True)
            page.write_text((perf.ROOT / perf.DOC).read_text())
            self.config["workloads"][0]["absolute_wall_ms"]["small"] += 1
            with self.assertRaisesRegex(ValueError, "stale workload table"):
                perf.table(self.config, root)
            perf.table(self.config, root, write=True)
            perf.table(self.config, root)

    def test_omitted_workloads_cannot_change_the_required_set(self):
        for id in perf.REQUIRED:
            with self.subTest(workload=id):
                changed = copy.deepcopy(self.config)
                changed["workloads"] = [w for w in changed["workloads"] if w["id"] != id]
                with self.assertRaisesRegex(ValueError, "missing mandatory workload"):
                    perf.validate_protocol(changed)

    def test_relaxed_budget_and_omitted_scale_fail(self):
        changed = copy.deepcopy(self.config)
        changed["budgets"]["max_warm_time_ratio"] = 2
        with self.assertRaisesRegex(ValueError, "numeric budgets changed"):
            perf.validate_protocol(changed)
        del self.config["workloads"][0]["datasets"]["large"]
        with self.assertRaisesRegex(ValueError, "omits a scale"):
            perf.validate_protocol(self.config)

    def test_non_equivalent_idt_cannot_be_aggregated_as_paired(self):
        next(w for w in self.config["workloads"] if w["id"] == "idt-churn")["comparison"] = "paired"
        with self.assertRaisesRegex(ValueError, "comparison class drift"):
            perf.validate_protocol(self.config)

    def test_ownership_must_resolve_but_completion_does_not_rewrite_the_protocol(self):
        issues = {w["owner"]: "closed" for w in self.config["workloads"]}
        perf.validate_protocol(self.config, issues=issues)
        issues.pop(next(iter(issues)))
        with self.assertRaisesRegex(ValueError, "missing workload owner"):
            perf.validate_protocol(self.config, issues=issues)

    def test_complete_subset_receipt_is_structurally_admitted(self):
        data = self.receipt()
        self.assertEqual(perf.validate_receipt(data, self.config), 240)
        self.reject(data, "omits mandatory workloads", complete=True)

    def test_absolute_and_display_receipts_need_no_invented_comparator(self):
        data = self.receipt(("interpolate", "ui-selection"))
        self.assertEqual(perf.validate_receipt(data, self.config), 160)
        data["samples"][0]["side"] = "pymovements"
        self.reject(data, "invalid side/round")

    def test_missing_environment_versions_and_dirty_sources_fail(self):
        data = self.receipt()
        del data["runtime"]["python"]
        self.reject(data, "runtime/version/thread mismatch")
        data = self.receipt()
        data["revisions"]["eyes4s"]["dirty"] = True
        self.reject(data, "False was expected")

    def test_changed_protocol_and_comparator_and_lock_fail(self):
        for key in ("protocol_sha256", "dependency_lock_sha256"):
            with self.subTest(key=key):
                data = self.receipt()
                data[key] = "0" * 64
                self.reject(data, "different protocol|lock mismatch")
        data = self.receipt()
        data["revisions"]["pymovements"]["commit"] = "0" * 40
        self.reject(data, "comparator revision mismatch")

    def test_input_and_configuration_mismatch_fail(self):
        for key in ("input_identity_sha256", "materialized_input_sha256", "config_sha256"):
            with self.subTest(key=key):
                data = self.receipt()
                data["samples"][0][key] = "0" * 64
                self.reject(data, "input.*mismatch|configuration mismatch")

    def test_wrong_units_dropped_columns_and_wrong_outputs_fail(self):
        for key, value in (("units", {"time": "ms"}), ("columns", ["onset_us"]),
                           ("rows", 9), ("canonical_sha256", "0" * 64)):
            with self.subTest(key=key):
                data = self.receipt()
                data["samples"][0]["output"][key] = value
                self.reject(data, "schema/unit mismatch|scientific outputs differ")

    def test_matching_empty_outputs_are_not_success(self):
        data = self.receipt()
        for sample in data["samples"]:
            sample["output"]["rows"] = 0
        self.reject(data, "empty output")

    def test_errors_timeouts_and_oom_are_never_successful_timings(self):
        for status in ("error", "timeout", "oom"):
            with self.subTest(status=status):
                data = self.receipt()
                sample = data["samples"][0]
                sample.update(status=status, exit_code=None, failure="intentionally failed test",
                              wall_ns=[], output=None, peak_rss_bytes=None)
                self.reject(data, "failed/timeout/OOM")

    def test_unpaired_or_missing_round_and_duplicate_cells_fail(self):
        data = self.receipt()
        data["samples"].pop()
        self.reject(data, "missing required scale/mode/side/round")
        data = self.receipt()
        data["samples"].append(copy.deepcopy(data["samples"][0]))
        self.reject(data, "duplicate measurement cell")

    def test_process_reuse_order_and_warmup_drift_fail(self):
        data = self.receipt()
        data["samples"][1]["process_instance"] = data["samples"][0]["process_instance"]
        self.reject(data, "process reused")
        for key, value, message in (("order", 1, "order policy"), ("warmups", 5, "warmup mismatch")):
            data = self.receipt()
            data["samples"][0][key] = value
            self.reject(data, message)

    def test_wrong_timing_repetitions_startup_and_rss_fail(self):
        for key, value, message in (("wall_ns", [1000, 1000], "timing repetitions"),
                                    ("startup_ns", None, "startup measurement"),
                                    ("peak_rss_bytes", None, "missing RSS")):
            data = self.receipt()
            data["samples"][0][key] = value
            self.reject(data, message)

    def test_counter_unavailability_must_be_explicit_and_jvm_counters_required(self):
        data = self.receipt()
        counter = data["samples"][0]["diagnostics"]["allocated_bytes"]
        counter["value"] = None
        self.reject(data, "availability is ambiguous")
        counter["unavailable_reason"] = "unavailable"
        self.reject(data, "JVM diagnostic counter missing")

    def test_unknown_fields_and_boolean_or_negative_measurements_fail(self):
        data = self.receipt()
        data["passed"] = True
        self.reject(data, "Additional properties")
        for value in (True, -1, 0):
            data = self.receipt()
            data["samples"][0]["wall_ns"] = [value]
            self.reject(data, "not of type|less than the minimum")

    def test_no_fabricated_speed_verdict_from_structural_admission(self):
        data = self.receipt()
        for sample in data["samples"]:
            sample["wall_ns"] = [999_000_000_000] * len(sample["wall_ns"])
        self.assertEqual(perf.validate_receipt(data, self.config), 240)

    def test_nonfinite_and_duplicate_json_are_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "bad.json"
            for text in ('{"x": NaN}', '{"x": Infinity}', '{"x":1,"x":2}'):
                path.write_text(text)
                with self.assertRaises(ValueError):
                    perf.read(path)

    def test_fixture_bytes_and_missingness_from_hand_examples(self):
        stream = fixtures.chunks({"family": "stationary", "scale": "small", "format": "csv"})
        self.assertEqual(next(stream), b"time_ms,x_px,y_px,pupil,valid,right_x_px,right_y_px,right_pupil,right_valid\n")
        self.assertEqual(next(stream), b"0,960.00,540.00,1000,1,,,,\n")
        self.assertEqual(next(stream), b"2,960.00,540.00,1000,1,,,,\n")
        self.assertEqual(fixtures.sample(450, "gaps-binocular")[-2:], ("0", 0))
        self.assertEqual(fixtures.sample(480, "gaps-binocular")[-2:], ("1000", 1))
        # The high-rate fixture has a sustained 20-sample movement, not only isolated jumps.
        positions = [float(fixtures.sample(i, "gaps-binocular")[1]) for i in range(20)]
        self.assertTrue(all(b - a > 3 for a, b in zip(positions, positions[1:])))
        # A threshold-churn fixture must include a qualifying stationary run, not empty outputs.
        self.assertEqual({fixtures.sample(i, "dispersion-churn")[1] for i in range(81)}, {"960.00"})
        self.assertEqual(fixtures.sample(100, "dispersion-churn")[1], "980.00")


if __name__ == "__main__":
    unittest.main()

"""Adversarial inventory checks: omissions and stale claims must not pass."""

import copy
import json
from pathlib import Path
import shutil
import tempfile
import unittest
import zipfile

import check


class ComparisonContractTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.manifest = check.read_json(check.ROOT / check.MANIFEST)
        paths = {s["path"] for s in self.manifest["sources"].values()
                 if s["repository"] == "eyes4s"} | {str(check.DOC)}
        for name in paths:
            destination = self.root / name
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(check.ROOT / name, destination)
        self.issues = {self.manifest["epic"]: "open"}
        self.issues.update({t["issue"]: "open" for t in self.manifest["targets"].values()})
        for row in self.manifest["capabilities"]:
            self.issues.update({owner: "open" for owner in row["owners"]})

    def validate(self, manifest=None, **kwargs):
        check.validate(self.manifest if manifest is None else manifest, self.root,
                       issues=self.issues, **kwargs)

    def row(self, id="geometry"):
        return next(row for row in self.manifest["capabilities"] if row["id"] == id)

    def reject(self, pattern, **kwargs):
        with self.assertRaisesRegex(ValueError, pattern):
            self.validate(**kwargs)

    def make_receipt(self, dimension, row=None):
        row = self.row() if row is None else row
        data = {
            "capability": row["id"], "dimension": dimension, "outcome": "passed",
            "source_commit": self.manifest["eyes4s_commit"],
            "protocol_sha256": check.protocol_digest(self.root, self.manifest),
            "command": "synthetic test receipt; no scientific work executed",
            "runtime": "test-runtime", "platforms": row["platforms"],
            "inputs": [{"id": "test-input", "sha256": "a" * 64}],
            "config_sha256": "b" * 64,
            "outputs": [{"id": "test-output", "sha256": "c" * 64}],
            "artifacts": [{"id": "test:artifact:0", "sha256": "d" * 64}],
        }
        path = self.root / f"{dimension}.json"
        self.save_receipt(path, data, dimension, row)
        return path, data

    def save_receipt(self, path, data, dimension, row=None):
        row = self.row() if row is None else row
        path.write_text(json.dumps(data), encoding="utf-8")
        row[dimension] = {"status": check.SUCCESS[dimension], "note": "synthetic test only",
                          "receipt": {"path": path.name, "sha256": check.sha(path.read_bytes())}}

    def test_inventory_and_generated_table(self):
        self.validate()
        check.table(self.root, self.manifest)

    def test_every_required_capability_is_independently_required(self):
        for id in check.REQUIRED:
            with self.subTest(id=id):
                altered = copy.deepcopy(self.manifest)
                altered["capabilities"] = [r for r in altered["capabilities"] if r["id"] != id]
                self.reject("missing required capability", manifest=altered)

    def test_all_five_targets_are_required(self):
        del self.manifest["targets"]["PM3"]
        self.reject("all five targets")

    def test_performance_cannot_be_relabelled_as_visual_only(self):
        self.row("performance")["targets"] = ["PM4"]
        self.reject("performance omits required target PM3")

    def test_missing_owner_evidence_or_status_fails(self):
        for field in ("owners", "fixtures", "workflow", "implementation"):
            with self.subTest(field=field):
                altered = copy.deepcopy(self.manifest)
                del altered["capabilities"][0][field]
                self.reject("expected fields", manifest=altered)
        self.row()["implementation"]["apis"] = []
        self.reject("implementation lacks evidence")

    def test_empty_owner_and_closed_owner_fail(self):
        owner = self.row()["owners"][0]
        self.issues[owner] = "closed"
        self.reject("open gap has closed owner")
        self.issues[owner] = "open"
        self.row()["owners"] = []
        self.reject("owners: empty list")

    def test_missing_live_owner_fails(self):
        del self.issues[self.row()["owners"][0]]
        self.reject("missing Mote owner")

    def test_closed_target_and_epic_cannot_hide_open_rows(self):
        target = self.manifest["targets"]["PM3"]["issue"]
        self.issues[target] = "closed"
        self.reject("closed PM3 gate")
        self.issues[target] = "open"
        self.issues[self.manifest["epic"]] = "closed"
        self.reject("closed epic")

    def test_unknown_evidence_and_source_as_test_fail(self):
        self.row()["implementation"]["apis"] = ["not-a-source"]
        self.reject("unknown or wrong-repository evidence")
        self.row()["implementation"]["apis"] = ["workflow-test"]
        self.reject("must cite API source")

    def test_duplicate_ids_and_unknown_status_fail(self):
        self.manifest["capabilities"].append(copy.deepcopy(self.row()))
        self.reject("duplicate capability")
        self.manifest["capabilities"].pop()
        self.row()["workflow"]["status"] = "probably-good"
        self.reject("unknown workflow status")

    def test_source_or_test_presence_cannot_complete_a_row(self):
        self.row()["gate"] = "accepted"
        self.reject("contradictory completion")
        self.row()["gate"] = "open"
        self.row()["workflow"]["status"] = "verified"
        self.reject("receipt reference")

    def test_absent_implementation_cannot_be_qualified(self):
        self.make_receipt("workflow", self.row("blink"))
        self.reject("absent capability")

    def test_receipt_success_is_structurally_usable_without_circular_table_hash(self):
        for dimension in check.SUCCESS:
            self.make_receipt(dimension)
        self.row()["gate"] = "accepted"
        self.row()["gap"] = ""
        self.validate()
        check.table(self.root, self.manifest, write=True)
        self.validate()
        check.table(self.root, self.manifest)

    def test_failed_stale_wrong_scope_and_incomplete_receipts_fail(self):
        path, original = self.make_receipt("workflow")
        cases = [
            ("outcome", "failed", "did not pass"),
            ("source_commit", "0" * 40, "source is stale"),
            ("capability", "blink", "capability/dimension mismatch"),
            ("dimension", "empirical", "capability/dimension mismatch"),
            ("protocol_sha256", "0" * 64, "protocol is stale"),
            ("platforms", ["JVM"], "omits a platform"),
            ("inputs", [], "inputs: empty"),
            ("outputs", [], "outputs: empty"),
        ]
        for key, value, pattern in cases:
            with self.subTest(key=key):
                altered = copy.deepcopy(original)
                altered[key] = value
                self.save_receipt(path, altered, "workflow")
                self.reject(pattern)

    def test_empirical_and_release_require_workflow(self):
        self.make_receipt("empirical")
        self.reject("needs verified workflow")

    def test_release_requires_published_artifact_identity(self):
        self.make_receipt("workflow")
        path, data = self.make_receipt("release")
        data["artifacts"] = []
        self.save_receipt(path, data, "release")
        self.reject("artifacts: empty")

    def test_changed_receipt_and_changed_protocol_fail(self):
        path, _ = self.make_receipt("workflow")
        with path.open("a") as stream:
            stream.write(" ")
        self.reject("receipt digest changed")
        self.make_receipt("workflow")
        with (self.root / check.DOC).open("a") as stream:
            stream.write("\nChanged evaluation criteria.\n")
        self.reject("protocol is stale")

    def test_changed_missing_and_unanchored_local_source_fail(self):
        source = self.manifest["sources"]["frame"]
        path = self.root / source["path"]
        path.write_text("Changed implementation.\n")
        self.reject("source changed")
        source["sha256"] = check.sha(path.read_bytes())
        self.reject("missing source anchor")
        path.unlink()
        self.reject("missing file")

    def test_traversal_and_symlink_escape_fail(self):
        source = self.manifest["sources"]["frame"]
        source["path"] = "../outside.scala"
        self.reject("unsafe path")
        source["path"] = "escape.scala"
        (self.root / "escape.scala").symlink_to(check.ROOT / "eyes4s.md")
        self.reject("path escapes root")

    def test_duplicate_json_keys_are_rejected(self):
        path = self.root / "duplicate.json"
        path.write_text('{"status":"pending","status":"qualified"}')
        with self.assertRaisesRegex(ValueError, "duplicate JSON key"):
            check.read_json(path)

    def test_unknown_fields_are_not_ignored(self):
        self.row()["qualified"] = True
        self.reject("expected fields")

    def test_stale_table_fails_and_regenerates(self):
        self.row()["title"] = "Changed capability title"
        with self.assertRaisesRegex(ValueError, "stale comparison table"):
            check.table(self.root, self.manifest)
        check.table(self.root, self.manifest, write=True)
        check.table(self.root, self.manifest)

    def test_wheel_hash_and_member_evidence_are_both_checked(self):
        wheel = self.root / "synthetic.whl"
        members = {}
        for source in self.manifest["sources"].values():
            if source["repository"] == "pymovements":
                members.setdefault(source["path"], set()).update(source["anchors"])
        payloads = {path: "\n".join(sorted(anchors)).encode() for path, anchors in members.items()}
        for source in self.manifest["sources"].values():
            if source["repository"] == "pymovements":
                source["sha256"] = check.sha(payloads[source["path"]])
        def write_wheel(omit=None):
            with zipfile.ZipFile(wheel, "w") as archive:
                for path, data in payloads.items():
                    if path != omit:
                        archive.writestr(path, data)
            self.manifest["comparator"]["wheel_sha256"] = check.sha(wheel.read_bytes())
        write_wheel()
        self.validate(wheel=wheel)
        with wheel.open("ab") as stream:
            stream.write(b"tamper")
        self.reject("wheel digest mismatch", wheel=wheel)
        path = next(iter(payloads))
        payloads[path] += b"changed source"
        write_wheel()
        self.reject("source changed", wheel=wheel)
        write_wheel(omit=path)
        self.reject("missing wheel member", wheel=wheel)


if __name__ == "__main__":
    unittest.main()

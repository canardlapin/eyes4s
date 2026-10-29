import hashlib
import importlib.util
import io
import json
import contextlib
from pathlib import Path
import tempfile
import unittest

HERE = Path(__file__).parent
spec = importlib.util.spec_from_file_location("datasets", HERE / "datasets.py")
datasets = importlib.util.module_from_spec(spec)
spec.loader.exec_module(datasets)

PAYLOAD = b"small fixture bytes"

class Response(io.BytesIO):
    def __init__(self, body, content_type="application/octet-stream", advertised=None):
        super().__init__(body); self.headers={"Content-Type":content_type, "Content-Length":str(len(body) if advertised is None else advertised)}
    def __enter__(self): return self
    def __exit__(self, *args): self.close()
    def geturl(self): return "https://cdn.example.test/resolved.bin"

class DatasetCliTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(); self.base=Path(self.tmp.name); self.cache=self.base/"cache"; self.manifest=self.base/"manifest.json"
        self.item={"id":"fixture","title":"Fixture","acquisition":{"kind":"http","files":[{"path":"data.bin","url":"https://example.test/data.bin","size_bytes":len(PAYLOAD),"sha256":hashlib.sha256(PAYLOAD).hexdigest()}]},"analysis":{"adapter":None,"reason":"not an analysis dataset"}}
        self.manifest.write_text(json.dumps({"schema_version":1,"datasets":[self.item]}))
    def tearDown(self): self.tmp.cleanup()
    def fetch(self, *extra): return self.call_cli(["--manifest",str(self.manifest),"--cache",str(self.cache),"fetch","fixture",*extra])
    def call_cli(self, argv):
        with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
            return datasets.main(argv)
    def test_refuses_implicit_whole_download(self):
        self.assertEqual(self.fetch(),2)
    def test_download_and_verify_hash(self):
        old=datasets.urlopen; datasets.urlopen=lambda request,timeout: Response(PAYLOAD)
        try: self.assertEqual(self.fetch("--file","data.bin"),0)
        finally: datasets.urlopen=old
        self.assertEqual(self.call_cli(["--manifest",str(self.manifest),"--cache",str(self.cache),"verify","fixture"]),0)
        (self.cache/"fixture/data.bin").write_bytes(b"corrupt")
        self.assertEqual(datasets.verify(self.item,self.cache)["status"],"FAIL")
        self.assertEqual(self.call_cli(["--manifest",str(self.manifest),"--cache",str(self.cache),"verify","fixture"]),1)
    def test_partial_download_is_removed_on_checksum_failure(self):
        self.item["acquisition"]["files"][0]["sha256"]="0"*64; self.manifest.write_text(json.dumps({"schema_version":1,"datasets":[self.item]}))
        old=datasets.urlopen; datasets.urlopen=lambda request,timeout: Response(PAYLOAD)
        try: self.assertEqual(self.fetch("--file","data.bin"),2)
        finally: datasets.urlopen=old
        self.assertFalse((self.cache/"fixture/data.bin").exists()); self.assertFalse((self.cache/"fixture/data.bin.part").exists())
    def test_streamed_limit_and_html_are_refused(self):
        old=datasets.urlopen; datasets.urlopen=lambda request,timeout: Response(PAYLOAD)
        try: self.assertEqual(self.fetch("--file","data.bin","--max-bytes","2"),2)
        finally: datasets.urlopen=old
        datasets.urlopen=lambda request,timeout: Response(b"<html>no</html>","text/html")
        try: self.assertEqual(self.fetch("--file","data.bin"),2)
        finally: datasets.urlopen=old

    def test_truncated_advertised_response_is_refused_without_descriptor_hash(self):
        self.item["acquisition"]["files"][0].pop("size_bytes")
        self.item["acquisition"]["files"][0].pop("sha256")
        self.manifest.write_text(json.dumps({"schema_version":1,"datasets":[self.item]}))
        old=datasets.urlopen; datasets.urlopen=lambda request,timeout: Response(b"abc", advertised=8)
        try: self.assertEqual(self.fetch("--file","data.bin"),2)
        finally: datasets.urlopen=old
        self.assertFalse((self.cache/"fixture/data.bin").exists())
        self.assertFalse((self.cache/"fixture/receipt.json").exists())
        self.assertEqual(list((self.cache/"fixture").glob("*.part")), [])
    def test_path_traversal_and_reserved_metadata_are_refused(self):
        for path in ("../escape", "/absolute", "receipt.json", "x.part", "a\\b", "./a"):
            with self.assertRaises(ValueError): datasets.safe_path(path)
        for ident in ("../escape", "a/b", "a\\b", "."):
            with self.assertRaises(ValueError): datasets.validate_id(ident)

    def test_symlink_escape_is_rejected(self):
        base=self.cache/"fixture"; base.mkdir(parents=True)
        (base/"link").symlink_to(self.base, target_is_directory=True)
        with self.assertRaises(ValueError): datasets.ensure_contained(base,"link/escape")

    def test_osf_fixture_recurses_using_relationships_and_materialized_path(self):
        pages={
            "root":{"data":[{"attributes":{"kind":"folder","name":"nested"},"relationships":{"files":{"links":{"related":{"href":"child"}}}}}],"links":{"next":None}},
            "child":{"data":[{"attributes":{"kind":"file","name":"bad-guid","materialized_path":"/nested/actual.tsv","size":2,"extra":{"hashes":{"sha256":"aa"}}},"links":{"download":"https://osf.test/file"}}],"links":{"next":None}},
        }
        old=datasets.request_json; datasets.request_json=lambda url: pages[url]
        try: found=datasets.osf_files("root")
        finally: datasets.request_json=old
        self.assertEqual(found[0]["path"],"nested/actual.tsv")
    def test_stale_manifest_blocks_cache_reuse(self):
        target=self.cache/"fixture"; target.mkdir(parents=True); (target/"data.bin").write_bytes(PAYLOAD)
        (target/"receipt.json").write_text(json.dumps({"manifest_entry_sha256":"old","files":[]}))
        old=datasets.urlopen; datasets.urlopen=lambda request,timeout: Response(PAYLOAD)
        try: self.assertEqual(self.fetch("--file","data.bin"),2)
        finally: datasets.urlopen=old
    def test_receipt_detects_source_drift_on_redownload(self):
        target=self.cache/"fixture"; target.mkdir(parents=True)
        receipt={"schema_version":1,"dataset_id":"fixture","manifest_entry_sha256":datasets.digest(self.item),"files":[{"path":"data.bin","resolved_url":"https://example.test/data.bin","size_bytes":3,"sha256":hashlib.sha256(b"old").hexdigest()}]}
        (target/"receipt.json").write_text(json.dumps(receipt))
        old=datasets.urlopen; datasets.urlopen=lambda request,timeout: Response(PAYLOAD)
        try: self.assertEqual(self.fetch("--file","data.bin"),2)
        finally: datasets.urlopen=old
        self.assertFalse((target/"data.bin").exists())
    def test_check_all_mixed_statuses_continues(self):
        second=dict(self.item); second["id"]="missing"; second["analysis"]={"adapter":"figrim_locations","reason":"adapter"}
        self.manifest.write_text(json.dumps({"schema_version":1,"datasets":[self.item,second]}))
        self.assertEqual(self.call_cli(["--manifest",str(self.manifest),"--cache",str(self.cache),"check","--all"]),1)
        self.assertEqual(json.loads((self.cache/"fixture/check.json").read_text())["status"],"BLOCKED")
        self.assertEqual(json.loads((self.cache/"missing/check.json").read_text())["status"],"FAIL")

    def test_partial_two_file_download_is_receipted_and_retryable(self):
        second = b"second fixture"
        self.item["acquisition"]["files"].append({"path":"second.bin", "url":"https://example.test/second.bin", "size_bytes":len(second), "sha256":hashlib.sha256(second).hexdigest()})
        self.manifest.write_text(json.dumps({"schema_version":1,"datasets":[self.item]}))
        calls = 0
        def fail_second(request, timeout):
            nonlocal calls
            calls += 1
            if calls == 2: raise OSError("network lost")
            return Response(PAYLOAD)
        old=datasets.urlopen; datasets.urlopen=fail_second
        try: self.assertEqual(self.fetch("--all-files"),2)
        finally: datasets.urlopen=old
        receipt=json.loads((self.cache/"fixture/receipt.json").read_text())
        self.assertEqual([r["path"] for r in receipt["files"]],["data.bin"])
        def succeed(request, timeout): return Response(second if request.full_url.endswith("second.bin") else PAYLOAD)
        datasets.urlopen=succeed
        try: self.assertEqual(self.fetch("--all-files"),0)
        finally: datasets.urlopen=old
        self.assertEqual(datasets.verify(self.item,self.cache)["status"],"PASS")

if __name__ == "__main__": unittest.main()

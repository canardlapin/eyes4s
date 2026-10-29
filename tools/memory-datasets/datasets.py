#!/usr/bin/env python3
"""Conservative manifest-driven downloader for optional memory datasets."""
import argparse
import hashlib
from http.client import HTTPException
import json
import os
from pathlib import Path, PurePosixPath
import subprocess
import sys
import tempfile
from datetime import datetime, timezone
from urllib.error import HTTPError, URLError
from urllib.parse import urlparse
from urllib.request import Request, urlopen


ROOT = Path(__file__).resolve().parents[2]
DEFAULT_MAX = 256 * 1024 * 1024
RESERVED = {"receipt.json", "check.json", "inventory.json"}


def canonical_json(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":"))


def digest(value):
    return hashlib.sha256(canonical_json(value).encode()).hexdigest()


def entry_digest(item):
    return digest(item)


def validate_id(ident):
    if not isinstance(ident, str) or not ident or "/" in ident or "\\" in ident or ident in (".", ".."):
        raise ValueError("unsafe dataset id")
    return ident


def safe_path(value):
    if not isinstance(value, str) or not value or "\\" in value:
        raise ValueError("unsafe dataset path: " + repr(value))
    if any(segment in ("", ".", "..") for segment in value.split("/")):
        raise ValueError("unsafe dataset path: " + value)
    parsed = PurePosixPath(value)
    if parsed.is_absolute() or "." in parsed.parts or ".." in parsed.parts:
        raise ValueError("unsafe dataset path: " + value)
    if parsed.name in RESERVED or parsed.name.endswith(".part"):
        raise ValueError("unsafe or reserved dataset path: " + value)
    return Path(*parsed.parts)


def load_manifest(path):
    data = json.loads(Path(path).read_text(encoding="utf-8"))
    if data.get("schema_version") != 1 or not isinstance(data.get("datasets"), list):
        raise ValueError("unsupported manifest schema")
    ids = set()
    for item in data["datasets"]:
        ident = validate_id(item.get("id"))
        if ident in ids:
            raise ValueError("duplicate dataset id: " + ident)
        ids.add(ident)
        files = item.get("acquisition", {}).get("files", [])
        paths = set()
        for file in files:
            normalized = safe_path(file.get("path")).as_posix()
            if normalized in paths:
                raise ValueError("duplicate file path: " + normalized)
            paths.add(normalized)
    return data


def dataset(manifest, ident):
    for item in manifest["datasets"]:
        if item["id"] == ident:
            return item
    raise ValueError("unknown dataset: " + ident)


def cache_root(value):
    target = Path(value or os.environ.get("EYES4S_DATASETS_DIR") or ROOT / ".datasets/memory").expanduser().resolve()
    try:
        target.relative_to(ROOT)
    except ValueError:
        return target
    try:
        target.relative_to((ROOT / ".datasets").resolve())
    except ValueError:
        raise ValueError("custom cache inside repository must be under .datasets")
    return target


def request_json(url):
    with urlopen(Request(url, headers={"Accept": "application/json", "User-Agent": "eyes4s-datasets/1"}), timeout=30) as response:
        return json.load(response)


def osf_files(url, parent=""):
    found = []
    while url:
        page = request_json(url)
        for entry in page.get("data", []):
            attrs = entry.get("attributes", {})
            relationships = entry.get("relationships", {})
            related = relationships.get("files", {}).get("links", {}).get("related", {}).get("href")
            kind = attrs.get("kind")
            if kind == "folder":
                folder = attrs.get("name", "")
                found.extend(osf_files(related, parent + folder + "/"))
            elif kind == "file":
                hashes = attrs.get("extra", {}).get("hashes", {})
                materialized = attrs.get("materialized_path")
                name = materialized.lstrip("/") if materialized else parent + attrs.get("name", "")
                found.append({
                    "path": name,
                    "url": entry.get("links", {}).get("download"),
                    "size_bytes": attrs.get("size"),
                    "sha256": hashes.get("sha256"),
                    "md5": hashes.get("md5"),
                })
        url = page.get("links", {}).get("next")
    return found


def discover(item):
    acquisition = item.get("acquisition", {})
    kind = acquisition.get("kind")
    if kind == "http":
        return acquisition.get("files", [])
    if kind == "figshare":
        article = acquisition.get("article")
        if article is None:
            raise ValueError("figshare acquisition lacks article")
        remote = request_json("https://api.figshare.com/v2/articles/%s" % article).get("files", [])
        return [{"path": f.get("name"), "url": f.get("download_url"), "size_bytes": f.get("size"), "md5": f.get("computed_md5")} for f in remote]
    if kind == "osf":
        node = acquisition.get("node")
        if not node:
            raise ValueError("osf acquisition lacks node")
        return osf_files("https://api.osf.io/v2/nodes/%s/files/osfstorage/" % node)
    if kind in ("manual", "unavailable"):
        raise ValueError("%s acquisition cannot be downloaded" % kind)
    raise ValueError("unknown acquisition kind: %s" % kind)


def dataset_dir(cache, item):
    path = cache / validate_id(item["id"])
    if path.exists() and path.is_symlink():
        raise ValueError("dataset cache directory is a symlink")
    return path


def receipt_path(cache, item):
    return dataset_dir(cache, item) / "receipt.json"


def read_receipt(cache, item):
    path = receipt_path(cache, item)
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        return None
    except json.JSONDecodeError as error:
        raise ValueError("malformed receipt: " + str(error))
    if not isinstance(value, dict) or value.get("schema_version") != 1 or value.get("dataset_id") != item["id"]:
        raise ValueError("invalid receipt")
    if not isinstance(value.get("files"), list) or not value["files"]:
        raise ValueError("empty or malformed receipt")
    return value


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile("w", encoding="utf-8", dir=path.parent, delete=False) as handle:
        json.dump(value, handle, indent=2, sort_keys=True)
        handle.write("\n")
    Path(handle.name).replace(path)


def write_inventory(cache, item, receipt=None, discovered_files=None):
    path = cache / "inventory.json"
    try:
        inventory = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError:
        inventory = {"schema_version": 1, "datasets": {}}
    record = inventory.setdefault("datasets", {}).setdefault(item["id"], {})
    record["manifest_entry_sha256"] = entry_digest(item)
    if receipt is not None:
        record["receipt_sha256"] = digest(receipt)
    if discovered_files is not None:
        record["discovered_at"] = datetime.now(timezone.utc).isoformat()
        record["discovered_files"] = discovered_files
    write_json(path, inventory)


def response_hashes(response, part, maximum):
    sha256 = hashlib.sha256()
    md5 = hashlib.md5()
    total = 0
    first = b""
    with part.open("wb") as output:
        while True:
            block = response.read(1024 * 1024)
            if not block:
                break
            if len(first) < 512:
                first += block[:512 - len(first)]
            total += len(block)
            if total > maximum:
                raise ValueError("max-byte limit exceeded")
            sha256.update(block)
            md5.update(block)
            output.write(block)
    if b"<html" in first.lower() or b"<!doctype html" in first.lower():
        raise ValueError("refusing HTML response")
    return total, sha256.hexdigest(), md5.hexdigest()


def stream_hashes(path):
    sha256 = hashlib.sha256()
    md5 = hashlib.md5()
    size = 0
    with path.open("rb") as source:
        while block := source.read(1024 * 1024):
            size += len(block)
            sha256.update(block)
            md5.update(block)
    return size, sha256.hexdigest(), md5.hexdigest()


def ensure_contained(base, relative):
    if base.exists() and base.is_symlink():
        raise ValueError("dataset cache directory is a symlink")
    target = base / safe_path(relative)
    try:
        target.resolve().relative_to(base.resolve())
    except ValueError:
        raise ValueError("dataset path escapes cache")
    return target


def download(file, destination, remaining):
    original = file.get("url")
    if urlparse(original or "").scheme not in ("https", "http"):
        raise ValueError("download URL must be http(s)")
    expected_size = file.get("size_bytes")
    if expected_size is not None and expected_size > remaining:
        raise ValueError("max-byte limit exceeded")
    destination.parent.mkdir(parents=True, exist_ok=True)
    # A unique, exclusive temporary file avoids following an attacker-created
    # predictable .part symlink.
    handle = tempfile.NamedTemporaryFile("wb", dir=destination.parent, prefix=destination.name + ".", suffix=".part", delete=False)
    part = Path(handle.name)
    handle.close()
    try:
        with urlopen(Request(original, headers={"User-Agent": "eyes4s-datasets/1"}), timeout=60) as response:
            if "html" in response.headers.get("Content-Type", "").lower():
                raise ValueError("refusing HTML response")
            advertised = response.headers.get("Content-Length")
            if advertised and int(advertised) > remaining:
                raise ValueError("max-byte limit exceeded")
            total, actual_sha256, actual_md5 = response_hashes(response, part, remaining)
            resolved = response.geturl()
        if advertised and total != int(advertised):
            raise ValueError("truncated response: Content-Length mismatch")
        if expected_size is not None and total != expected_size:
            raise ValueError("size mismatch")
        if file.get("sha256") and actual_sha256.lower() != file["sha256"].lower():
            raise ValueError("sha256 mismatch")
        if file.get("md5") and actual_md5.lower() != file["md5"].lower():
            raise ValueError("md5 mismatch")
        part.replace(destination)
        return total, actual_sha256, actual_md5, resolved
    except Exception:
        part.unlink(missing_ok=True)
        raise


def cmd_fetch(args, item, cache):
    if args.max_bytes < 0:
        raise ValueError("max bytes cannot be negative")
    wanted = set(args.file or [])
    if not wanted and not args.all_files:
        raise ValueError("refusing implicit whole-dataset download; use --file or --all-files")
    files = discover(item)
    selected = files if args.all_files else [file for file in files if file.get("path") in wanted]
    if wanted - {file.get("path") for file in selected}:
        raise ValueError("requested file not found")
    if not selected:
        raise ValueError("no files selected")
    normalized = set()
    for file in selected:
        path = safe_path(file.get("path")).as_posix()
        if path in normalized:
            raise ValueError("duplicate discovered file path: " + path)
        normalized.add(path)
    previous = read_receipt(cache, item)
    current_digest = entry_digest(item)
    if previous and previous.get("manifest_entry_sha256") != current_digest:
        raise ValueError("stale receipt: manifest entry changed")
    base = dataset_dir(cache, item)
    remaining = args.max_bytes
    records = []
    for file in selected:
        target = ensure_contained(base, file["path"])
        prior = next((record for record in (previous or {}).get("files", []) if record.get("path") == file["path"]), None)
        if target.exists():
            if not prior:
                raise ValueError("unreceipted cache file exists: " + file["path"])
            size, actual_sha256, actual_md5 = stream_hashes(target)
            if size != prior.get("size_bytes") or actual_sha256 != prior.get("sha256"):
                raise ValueError("corrupt cached file: " + file["path"])
            if file.get("size_bytes") is not None and size != file["size_bytes"]:
                raise ValueError("source drift: discovered size differs from cache")
            if file.get("sha256") and actual_sha256.lower() != file["sha256"].lower():
                raise ValueError("source drift: discovered sha256 differs from cache")
            if file.get("md5") and actual_md5.lower() != file["md5"].lower():
                raise ValueError("source drift: discovered md5 differs from cache")
            if size > remaining:
                raise ValueError("max-byte limit exceeded")
            resolved = prior.get("resolved_url")
        else:
            size, actual_sha256, actual_md5, resolved = download(file, target, remaining)
            if prior and actual_sha256 != prior.get("sha256"):
                target.unlink(missing_ok=True)
                raise ValueError("source drift: downloaded checksum differs from receipt")
        remaining -= size
        records.append({
            "path": file["path"],
            "original_url": file["url"],
            "resolved_url": resolved,
            "size_bytes": size,
            "sha256": actual_sha256,
            "md5": actual_md5,
        })
        done = {record["path"] for record in records}
        retained = [record for record in (previous or {}).get("files", []) if record["path"] not in done]
        receipt = {"schema_version": 1, "dataset_id": item["id"], "manifest_entry_sha256": current_digest, "files": retained + records}
        write_json(receipt_path(cache, item), receipt)
        write_inventory(cache, item, receipt)
    return {"status": "PASS", "downloaded": [record["path"] for record in records]}


def verify(item, cache):
    try:
        receipt = read_receipt(cache, item)
    except ValueError as error:
        return {"status": "FAIL", "reason": str(error)}
    if not receipt or receipt.get("manifest_entry_sha256") != entry_digest(item):
        return {"status": "FAIL", "reason": "missing or stale receipt"}
    pinned = {file.get("path"): file for file in item.get("acquisition", {}).get("files", [])}
    seen = set()
    for record in receipt["files"]:
        try:
            path = record["path"]
            normalized = safe_path(path).as_posix()
            if normalized in seen:
                return {"status": "FAIL", "reason": "duplicate receipt path"}
            seen.add(normalized)
            target = ensure_contained(dataset_dir(cache, item), path)
            if not target.is_file():
                return {"status": "FAIL", "reason": "missing " + path}
            size, actual_sha256, actual_md5 = stream_hashes(target)
        except (KeyError, TypeError, ValueError) as error:
            return {"status": "FAIL", "reason": "malformed receipt record: " + str(error)}
        if size != record.get("size_bytes") or actual_sha256 != record.get("sha256"):
            return {"status": "FAIL", "reason": "corrupt " + path}
        if pinned:
            expected = pinned.get(path)
            if expected is None:
                return {"status": "FAIL", "reason": "receipt file absent from manifest: " + path}
            if expected.get("size_bytes") is not None and size != expected["size_bytes"]:
                return {"status": "FAIL", "reason": "manifest size mismatch: " + path}
            if expected.get("sha256") and actual_sha256.lower() != expected["sha256"].lower():
                return {"status": "FAIL", "reason": "manifest checksum mismatch: " + path}
            if expected.get("md5") and actual_md5.lower() != expected["md5"].lower():
                return {"status": "FAIL", "reason": "manifest md5 mismatch: " + path}
    return {"status": "PASS", "files": len(receipt["files"]), "receipt_sha256": digest(receipt)}


def git_state():
    def run(*args):
        return subprocess.check_output(args, cwd=ROOT, text=True).strip()
    try:
        return {"head": run("git", "rev-parse", "HEAD"), "tree": run("git", "rev-parse", "HEAD^{tree}"), "dirty": bool(run("git", "status", "--porcelain"))}
    except (OSError, subprocess.CalledProcessError):
        return {"head": None, "tree": None, "dirty": None}


def check(item, cache):
    integrity = verify(item, cache)
    analysis = item.get("analysis", {})
    receipt_exists = receipt_path(cache, item).exists()
    try:
        receipt = read_receipt(cache, item)
    except ValueError:
        receipt = None
    if integrity["status"] != "PASS" and receipt is None and not receipt_exists and not analysis.get("adapter"):
        result = {"status": "BLOCKED", "reason": analysis.get("reason", "no analysis adapter")}
    elif integrity["status"] != "PASS":
        result = integrity
    elif not analysis.get("adapter"):
        result = {"status": "BLOCKED", "reason": analysis.get("reason", "no analysis adapter")}
    else:
        try:
            import adapters
            result = adapters.check(item, dataset_dir(cache, item), receipt)
        except ImportError:
            result = {"status": "BLOCKED", "reason": "analysis adapters unavailable"}
        except Exception as error:
            result = {"status": "FAIL", "reason": "adapter error: " + str(error)}
    result["status"] = result.get("status", "FAIL").upper()
    result.update({
        "schema_version": 1,
        "dataset_id": item["id"],
        "manifest_entry_sha256": entry_digest(item),
        "receipt_sha256": digest(receipt) if receipt else None,
        "git": git_state(),
        "checked_at": datetime.now(timezone.utc).isoformat(),
    })
    write_json(dataset_dir(cache, item) / "check.json", result)
    return result


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--manifest", default=Path(__file__).with_name("manifest.json"))
    parser.add_argument("--cache")
    subcommands = parser.add_subparsers(dest="command", required=True)
    subcommands.add_parser("list")
    show = subcommands.add_parser("show"); show.add_argument("id")
    files = subcommands.add_parser("files"); files.add_argument("id")
    fetch = subcommands.add_parser("fetch")
    fetch.add_argument("id"); fetch.add_argument("--file", action="append")
    fetch.add_argument("--all-files", action="store_true"); fetch.add_argument("--max-bytes", type=int, default=DEFAULT_MAX)
    verify_parser = subcommands.add_parser("verify"); verify_parser.add_argument("id")
    check_parser = subcommands.add_parser("check")
    check_parser.add_argument("id", nargs="?"); check_parser.add_argument("--all", action="store_true")
    args = parser.parse_args(argv)
    try:
        manifest, cache = load_manifest(args.manifest), cache_root(args.cache)
        if args.command == "list":
            output = [{key: item.get(key) for key in ("id", "title")} for item in manifest["datasets"]]
            code = 0
        elif args.command == "show": output, code = dataset(manifest, args.id), 0
        elif args.command == "files":
            item = dataset(manifest, args.id)
            output = discover(item)
            write_inventory(cache, item, discovered_files=output)
            code = 0
        elif args.command == "fetch": output, code = cmd_fetch(args, dataset(manifest, args.id), cache), 0
        elif args.command == "verify":
            output = verify(dataset(manifest, args.id), cache); code = 0 if output["status"] == "PASS" else 1
        else:
            if bool(args.id) == bool(args.all): raise ValueError("check needs ID or --all")
            if args.all:
                output = {}
                for item in manifest["datasets"]:
                    try: output[item["id"]] = check(item, cache)
                    except Exception as error: output[item["id"]] = {"status": "FAIL", "reason": "check error: " + str(error)}
            else: output = {args.id: check(dataset(manifest, args.id), cache)}
            code = 0 if all(result["status"] == "PASS" for result in output.values()) else 1
        print(json.dumps(output, indent=2)); return code
    except (ValueError, HTTPError, URLError, HTTPException, OSError, json.JSONDecodeError) as error:
        print(json.dumps({"status": "FAIL", "reason": str(error)}), file=sys.stderr); return 2


if __name__ == "__main__":
    sys.exit(main())

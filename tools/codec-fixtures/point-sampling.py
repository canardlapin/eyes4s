#!/usr/bin/env python3
"""Check built-in point-family fixtures using an independent canonical renderer.

The source is the unchanged legacy v1 fixture. --write regenerates the two new
JSON files; the default verifies bytes. This renderer covers their finite JSON
numbers, insertion-ordered objects and exact UTF-16 strings without Scala code.
"""
import argparse
import hashlib
import json
from pathlib import Path
import struct

ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "codec/src/test/resources/eyes4s"


def string(value):
    data = value.encode("utf-16-be")
    return struct.pack(">I", len(data) // 2) + data


def canonical(value):
    if value is None:
        return b"\x00"
    if isinstance(value, bool):
        return bytes([2 if value else 1])
    if isinstance(value, (int, float)):
        integer = int(value)
        if integer == value and abs(integer) > 2**53:
            return b"\x04" + struct.pack(">q", integer)
        return b"\x03" + struct.pack(">d", float(value))
    if isinstance(value, str):
        return b"\x05" + string(value)
    if isinstance(value, list):
        return b"\x06" + struct.pack(">I", len(value)) + b"".join(map(canonical, value))
    return b"\x07" + struct.pack(">I", len(value)) + b"".join(
        string(key) + canonical(item) for key, item in value.items()
    )


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()
    legacy = json.loads((FIXTURES / "point-sampling-v1.json").read_text())
    plan = legacy["value"]["plan"]
    plan["schema"] = {"name": "eyes4s.point-sampling-plan", "version": 1}
    digest = hashlib.sha256(canonical(plan)).hexdigest()
    result = {
        "schema": {"name": "eyes4s.point-sampling-result", "version": 1},
        "value": {
            "runStamp": {"plan": digest, "input": digest},
            "plan": plan,
            "result": legacy["value"]["result"],
        },
    }
    for name, value in [("point-sampling-plan-v1.json", plan),
                        ("point-sampling-result-v1.json", result)]:
        path = FIXTURES / name
        expected = (json.dumps(value, ensure_ascii=False, separators=(",", ":")) + "\n").encode()
        if args.write:
            path.write_bytes(expected)
        elif path.read_bytes() != expected:
            raise SystemExit(f"Fixture differs: {path}")
    print(f"Point-family canonical plan stamp: {digest}; fixtures verified")


if __name__ == "__main__":
    main()

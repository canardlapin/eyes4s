#!/usr/bin/env python3
"""Regenerate the pinned Engbert five-point velocity fixture on stdout."""

from __future__ import annotations

import ast
import json
import urllib.request

import numpy as np


COMMIT = "a3eba6e9f1464c953c81fbd87944ba7678c2cf64"
REPOSITORY = "https://github.com/lschwetlick/EngbertMicrosaccadeToolbox"
SOURCE = "EngbertMicrosaccadeToolbox/microsac_detection.py"
RAW_URL = f"https://raw.githubusercontent.com/lschwetlick/EngbertMicrosaccadeToolbox/{COMMIT}/{SOURCE}"


def load_vecvel():
    source = urllib.request.urlopen(RAW_URL, timeout=30).read().decode("utf-8")
    tree = ast.parse(source, filename=RAW_URL)
    function = next(
        node for node in tree.body if isinstance(node, ast.FunctionDef) and node.name == "vecvel"
    )
    namespace = {"np": np}
    exec(compile(ast.Module(body=[function], type_ignores=[]), RAW_URL, "exec"), namespace)
    return namespace["vecvel"]


def main() -> None:
    positions = np.asarray(
        [[0.0, 0.0], [0.0, 0.0], [0.0, 0.0], [1.0, 0.0], [3.0, 0.0],
         [6.0, 0.0], [8.0, 0.0], [9.0, 0.0], [9.0, 0.0]],
        dtype=float,
    )
    velocity = load_vecvel()(positions, sampling=1000.0)
    start, end = 2, 6
    peak = float(np.max(np.linalg.norm(velocity[start:end], axis=1)))
    fixture = {
        "schema": "eyes4s.engbert-kernels.v1",
        "oracle": {
            "repository": REPOSITORY,
            "commit": COMMIT,
            "source": SOURCE,
            "function": "vecvel",
        },
        "sampling_hz": 1000.0,
        "positions_deg": positions.tolist(),
        "interior_indices": list(range(2, len(positions) - 2)),
        "interior_velocity_deg_per_second": velocity[2:-2].tolist(),
        "event": {
            "start_index": start,
            "end_index": end,
            "peak_velocity_deg_per_second": peak,
        },
    }
    print(json.dumps(fixture, indent=2))


if __name__ == "__main__":
    main()

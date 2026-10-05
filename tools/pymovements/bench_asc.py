#!/usr/bin/env python3
"""Correctness preflight for pinned pymovements binocular ASC admission."""

import argparse
import hashlib
import json
from pathlib import Path
import warnings

from pymovements.gaze.io import from_asc


HEADER = "time_us,eye,x_px,y_px,pupil,status\n"
MESSAGES = [
    "RECCFG CR 1000 2 2 2 2 LR",
    "GAZE_COORDS 0 0 1919 1079",
    "DISPLAY_COORDS 0 0 1919 1079",
]


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def execute(source: Path, destination: Path, expected_rows: int) -> dict:
    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", message="No mount configuration found")
        warnings.filterwarnings("ignore", message="No eye tracker .* found")
        gaze = from_asc(source, messages=True)
    if gaze.samples.height != expected_rows:
        raise ValueError("ASC sample count changed")
    if gaze.samples.columns != ["time", "pupil", "pixel"]:
        raise ValueError("ASC sample schema changed")
    tracker = gaze.experiment.eyetracker
    if (tracker.sampling_rate != 1000 or tracker.left is not True or
            tracker.right is not True or gaze.experiment.screen.resolution != (1920, 1080)):
        raise ValueError("ASC eye, rate, or screen metadata changed")
    if gaze.messages.get_column("content").to_list() != MESSAGES:
        raise ValueError("ASC messages changed")

    with destination.open("w", encoding="utf-8", newline="\n", buffering=65536) as output:
        output.write(HEADER)
        for time, pupils, pixels in gaze.samples.iter_rows():
            if len(pupils) != 2 or len(pixels) != 4:
                raise ValueError("ASC binocular layout changed")
            for eye, offset in (("left", 0), ("right", 1)):
                x, y = pixels[2 * offset:2 * offset + 2]
                pupil = pupils[offset]
                if x is None and y is None and pupil == 0.0:
                    output.write(f"{time * 1000},{eye},,,,lost\n")
                elif x is not None and y is not None and pupil is not None:
                    output.write(
                        f"{time * 1000},{eye},{x:.6f},{y:.6f},"
                        f"{pupil:.6f},tracked\n"
                    )
                else:
                    raise ValueError("ASC row has inconsistent position or pupil")
    return {
        "input_sha256": sha256(source),
        "output_sha256": sha256(destination),
        "sample_rows": expected_rows,
        "output_rows": expected_rows * 2,
        "messages": len(MESSAGES),
        "eyes": 2,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("expected_rows", type=int)
    args = parser.parse_args()
    if args.expected_rows < 1:
        parser.error("expected row count must be positive")
    print(json.dumps(execute(args.input, args.output, args.expected_rows), sort_keys=True))


if __name__ == "__main__":
    main()

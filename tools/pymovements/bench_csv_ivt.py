#!/usr/bin/env python3
"""Pinned pymovements side of the PM3.2 CSV-to-I-VT paired experiment."""

import argparse
import hashlib
import json
import time
from pathlib import Path

import numpy as np
import polars as pl
from pymovements.events.detection import ivt
from pymovements.transforms.numpy import pix2deg, pos2vel

HEADER = "onset_us,stop_us,samples,centre_x,centre_y,rms\n"
COLUMNS = [
    "time_ms", "x_px", "y_px", "pupil", "valid",
    "right_x_px", "right_y_px", "right_pupil", "right_valid",
]


def execute(source: Path, destination: Path) -> tuple[int, int, int, str, str]:
    start = time.monotonic_ns()
    raw = pl.read_csv(source)
    if raw.columns != COLUMNS:
        raise ValueError(f"CSV schema differs: {raw.columns}")
    if raw.is_empty() or raw["time_ms"].is_null().any():
        raise ValueError("missing time or no rows")
    if raw["valid"].is_null().any() or not raw["valid"].is_in([0, 1]).all():
        raise ValueError("unknown validity token")
    times = raw["time_ms"].to_numpy()
    if not np.all(np.diff(times) > 0):
        raise ValueError("nonincreasing timestamp")
    valid = raw["valid"].to_numpy() == 1
    x = raw["x_px"].to_numpy()
    y = raw["y_px"].to_numpy()
    if np.any(valid & (~np.isfinite(x) | ~np.isfinite(y))):
        raise ValueError("tracked point lacks finite position")
    if np.any((~valid) & (np.isfinite(x) | np.isfinite(y))):
        raise ValueError("lost point carries a position")

    # PM's upper-left convention uses (width-1)/2 and y-down. Supplying
    # display-centered, y-up pixels to its public numpy transform matches the
    # frozen 1920x1080 physical viewing geometry of eyes4s' angular warp.
    centered = np.column_stack((x - 960.0, 540.0 - y))
    degrees = pix2deg(
        centered, screen_px=(1920, 1080), screen_cm=(53.0, 29.8),
        distance_cm=60.0, origin="center",
    )
    # Lost samples remain admitted rows, but cannot support a central velocity.
    # Run the reference detector on each maximal tracked segment; a segment of
    # fewer than three samples has no classified centre and emits no event.
    edges = np.flatnonzero(np.diff(np.r_[False, valid, False]))
    event_tables = []
    for first, stop in edges.reshape(-1, 2):
        if stop - first < 3:
            continue
        velocity = pos2vel(degrees[first:stop], sampling_rate=500.0, method="neighbors")
        # The public numpy velocity transform supplies zero at both ends. Eyes4s
        # inherits the adjacent interior classification at each segment endpoint.
        velocity[0] = velocity[1]
        velocity[-1] = velocity[-2]
        event_tables.append(ivt(
            velocity, timesteps=times[first:stop], minimum_duration=98,
            velocity_threshold=30.0,
        ).frame)
    event_table = pl.concat(event_tables) if event_tables else pl.DataFrame(
        schema={"name": pl.String, "onset": pl.Int64, "offset": pl.Int64, "duration": pl.Int64},
    )
    rows = [HEADER]
    for event in event_table.iter_rows(named=True):
        onset_ms = int(event["onset"])
        offset_ms = int(event["offset"])
        first = int(np.searchsorted(times, onset_ms))
        last = int(np.searchsorted(times, offset_ms))
        if first >= len(times) or last >= len(times) or times[first] != onset_ms or times[last] != offset_ms:
            raise ValueError("I-VT event boundary absent from source")
        support = degrees[first:last + 1]
        centre = np.mean(support, axis=0)
        rms = float(np.sqrt(np.mean(np.sum((support - centre) ** 2, axis=1))))
        rows.append(
            f"{onset_ms * 1000},{(offset_ms + 2) * 1000},{len(support)},"
            f"{centre[0]:.6f},{centre[1]:.6f},{rms:.6f}\n"
        )
    destination.write_text("".join(rows), encoding="utf-8", newline="\n")
    native_path = Path(str(destination) + ".native.csv")
    event_table.write_csv(native_path)
    elapsed = time.monotonic_ns() - start
    return (elapsed, raw.height, event_table.height,
            hashlib.sha256(destination.read_bytes()).hexdigest(),
            hashlib.sha256(native_path.read_bytes()).hexdigest())


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("warmups", type=int)
    parser.add_argument("samples", type=int)
    args = parser.parse_args()
    if args.warmups < 0 or args.samples < 1:
        parser.error("invalid warmup/sample count")
    ready_ns = time.monotonic_ns()
    warmup_runs = [execute(args.input, args.output) for _ in range(args.warmups)]
    runs = [execute(args.input, args.output) for _ in range(args.samples)]
    if len({run[1:] for run in warmup_runs + runs}) != 1:
        raise RuntimeError("iteration output changed")
    data = args.output.read_bytes()
    native = Path(str(args.output) + ".native.csv").read_bytes()
    print(json.dumps({
        "ready_ns": ready_ns,
        "iterations_ns": [run[0] for run in runs],
        "accepted_rows": runs[0][1],
        "fixations": runs[0][2],
        "output_sha256": hashlib.sha256(data).hexdigest(),
        "native_sha256": hashlib.sha256(native).hexdigest(),
        "output_bytes": len(data),
    }, sort_keys=True))


if __name__ == "__main__":
    main()

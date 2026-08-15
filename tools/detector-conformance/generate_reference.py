#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12,<3.13"
# dependencies = [
#   "numpy==1.26.4",
#   "pymovements==0.26.2",
# ]
# ///
"""Generate the pinned I-VT and I-DT detector conformance fixture.

The generated JSON is printed to stdout.  Keeping generation separate from the
Scala implementation makes the checked-in values an independent executable
oracle rather than output produced by the code under test.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path
from typing import Any

import numpy
import pymovements


PYMOVEMENTS_REVISION = "6753fdf8b81da40b890576dd6b369edb81243b06"
PERIOD_MILLIS = 1


def event_rows(events: Any) -> list[dict[str, int | str]]:
    columns = events.frame.to_dict(as_series=False)
    return [
        {
            "name": str(name),
            "onset_millis": int(onset),
            "offset_millis_inclusive": int(offset),
            "duration_millis": int(duration),
        }
        for name, onset, offset, duration in zip(
            columns["name"],
            columns["onset"],
            columns["offset"],
            columns["duration"],
            strict=True,
        )
    ]


def half_open(rows: list[dict[str, int | str]]) -> list[dict[str, int | str]]:
    """Map the oracle's inclusive last-sample timestamp to eyes4s support."""
    return [
        {
            "name": row["name"],
            "onset_millis": row["onset_millis"],
            "offset_millis_exclusive": int(row["offset_millis_inclusive"])
            + PERIOD_MILLIS,
        }
        for row in rows
    ]


def centroid(
    positions: list[list[float]], onset: int, offset_inclusive: int
) -> list[float]:
    support = numpy.asarray(positions[onset : offset_inclusive + 1], dtype=float)
    return support.mean(axis=0).tolist()


def with_centres(
    positions: list[list[float]], rows: list[dict[str, int | str]]
) -> list[dict[str, Any]]:
    mapped = half_open(rows)
    return [
        {
            **event,
            "centre": centroid(
                positions,
                int(raw["onset_millis"]),
                int(raw["offset_millis_inclusive"]),
            ),
        }
        for raw, event in zip(rows, mapped, strict=True)
    ]


def main() -> None:
    if pymovements.__version__ != "0.26.2":
        raise RuntimeError(f"unexpected pymovements version: {pymovements.__version__}")
    if numpy.__version__ != "1.26.4":
        raise RuntimeError(f"unexpected numpy version: {numpy.__version__}")

    ivt_positions = [
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [5.0, 0.0],
        [10.0, 0.0],
        [10.0, 0.0],
        [10.0, 0.0],
        [10.0, 0.0],
        [10.0, 0.0],
    ]
    # These are the hand-computable symmetric central velocities associated
    # with the samples above, with eyes4s endpoint inheritance made explicit.
    ivt_velocities = [
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [2500.0, 0.0],
        [5000.0, 0.0],
        [2500.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
    ]
    ivt_rows = event_rows(
        pymovements.events.ivt(
            velocities=ivt_velocities,
            timesteps=list(range(len(ivt_velocities))),
            minimum_duration=2,
            velocity_threshold=2000.0,
        )
    )

    idt_positions = [
        [9.0, 0.0],
        [1.0, 0.0],
        [1.1, 0.0],
        [1.0, 0.0],
        [1.1, 0.0],
        [1.0, 0.0],
    ]
    idt_rows = event_rows(
        pymovements.events.idt(
            positions=idt_positions,
            timesteps=list(range(len(idt_positions))),
            minimum_duration=2,
            dispersion_threshold=0.5,
        )
    )

    boundary_positions = [
        [0.0, 0.0],
        [0.1, 0.0],
        [0.0, 0.0],
        [0.1, 0.0],
        [9.0, 0.0],
        [1.0, 0.0],
        [1.1, 0.0],
        [1.0, 0.0],
        [1.1, 0.0],
        [1.0, 0.0],
    ]
    boundary_rows = event_rows(
        pymovements.events.idt(
            positions=boundary_positions,
            timesteps=list(range(len(boundary_positions))),
            minimum_duration=2,
            dispersion_threshold=0.5,
        )
    )

    fixture = {
        "$schema": "eyes4s.detector-conformance.v1",
        "oracle": {
            "project": "pymovements",
            "package_version": pymovements.__version__,
            "repository": "https://github.com/aeye-lab/pymovements",
            "revision": PYMOVEMENTS_REVISION,
            "license": "MIT",
            "numpy_version": numpy.__version__,
            "source_paths": {
                "ivt": "src/pymovements/events/detection/ivt.py",
                "idt": "src/pymovements/events/detection/idt.py",
            },
            "event_interval_convention": (
                "offset is the timestamp of the last classified sample; "
                "duration is offset minus onset"
            ),
        },
        "fixtures": [
            {
                "id": "ivt-symmetric-central-boundaries",
                "algorithm": "ivt",
                "period_millis": PERIOD_MILLIS,
                "positions_deg": ivt_positions,
                "oracle_velocities_deg_per_second": ivt_velocities,
                "parameters": {
                    "minimum_duration_millis": 2,
                    "velocity_threshold_deg_per_second": 2000.0,
                },
                "oracle_events": ivt_rows,
                "expected_eyes4s_fixations": with_centres(ivt_positions, ivt_rows),
                "expected_eyes4s_saccades": [
                    {
                        "onset_millis": 3,
                        "offset_millis_exclusive": 6,
                        "start": [0.0, 0.0],
                        "end": [10.0, 0.0],
                    }
                ],
            },
            {
                "id": "idt-advances-to-stable-window",
                "algorithm": "idt",
                "period_millis": PERIOD_MILLIS,
                "positions_deg": idt_positions,
                "parameters": {
                    "minimum_duration_millis": 2,
                    "dispersion_threshold": 0.5,
                },
                "oracle_events": idt_rows,
                "expected_eyes4s_fixations": with_centres(idt_positions, idt_rows),
            },
            {
                "id": "idt-violating-sample-policy",
                "algorithm": "idt",
                "period_millis": PERIOD_MILLIS,
                "positions_deg": boundary_positions,
                "parameters": {
                    "minimum_duration_millis": 2,
                    "dispersion_threshold": 0.5,
                },
                "oracle_events": boundary_rows,
                "oracle_events_half_open_with_centres": with_centres(
                    boundary_positions, boundary_rows
                ),
                "expected_eyes4s_fixations": [
                    {
                        "name": "fixation",
                        "onset_millis": 0,
                        "offset_millis_exclusive": 4,
                        "centre": [0.05, 0.0],
                    },
                    {
                        "name": "fixation",
                        "onset_millis": 5,
                        "offset_millis_exclusive": 10,
                        "centre": [1.04, 0.0],
                    },
                ],
                "documented_deviation": (
                    "eyes4s excludes the first threshold-violating sample from "
                    "the completed fixation and starts the next candidate at that sample; "
                    "pymovements 0.26.2 includes it in the completed fixation"
                ),
                "expected_error_report_against_oracle": {
                    "event_count_error": 0,
                    "onset_absolute_error_millis": [0.0, 0.0],
                    "offset_absolute_error_millis": [1.0, 0.0],
                    "duration_absolute_error_millis": [1.0, 0.0],
                    "centre_euclidean_error_deg": [1.79, 0.0],
                },
            },
        ],
    }
    if "--check" in sys.argv[1:]:
        checked_in = json.loads(Path(__file__).with_name("reference.json").read_text())
        if checked_in != fixture:
            raise SystemExit("reference.json differs from the pinned oracle output")
        print("reference.json matches the pinned oracle output")
    else:
        print(json.dumps(fixture, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()

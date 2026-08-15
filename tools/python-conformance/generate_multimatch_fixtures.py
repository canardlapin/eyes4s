#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12,<3.13"
# dependencies = [
#   "multimatch-gaze==0.1.3",
#   "numpy==1.26.4",
#   "pandas==3.0.5",
#   "scipy==1.17.1",
# ]
# ///
"""Generate the checked-in MultiMatch conformance fixtures.

The Python package is an oracle used only while regenerating fixtures. The
normal Scala test suite has no Python dependency.
"""

from __future__ import annotations

import argparse
from importlib.metadata import version
from pathlib import Path
from typing import TypeAlias

import multimatch_gaze
import numpy as np


Fixation: TypeAlias = tuple[float, float, float]
Fixture: TypeAlias = tuple[str, tuple[Fixation, ...], tuple[Fixation, ...]]

SCREEN = (1000, 800)
GROUPING = False
OUTPUT = (
    Path(__file__).resolve().parents[2]
    / "compare"
    / "src"
    / "test"
    / "scala"
    / "eyes4s"
    / "compare"
    / "MultiMatchPythonFixtures.scala"
)

# These cases deliberately separate the five result dimensions and exercise
# both equal- and unequal-length alignment lattices. Durations are seconds.
FIXTURES: tuple[Fixture, ...] = (
    (
        "identical",
        (
            (100.0, 100.0, 0.10),
            (400.0, 120.0, 0.16),
            (430.0, 410.0, 0.12),
            (760.0, 460.0, 0.20),
        ),
        (
            (100.0, 100.0, 0.10),
            (400.0, 120.0, 0.16),
            (430.0, 410.0, 0.12),
            (760.0, 460.0, 0.20),
        ),
    ),
    (
        "near-copy",
        (
            (100.0, 100.0, 0.10),
            (400.0, 120.0, 0.16),
            (430.0, 410.0, 0.12),
            (760.0, 460.0, 0.20),
            (850.0, 700.0, 0.14),
        ),
        (
            (112.0, 94.0, 0.12),
            (392.0, 135.0, 0.14),
            (445.0, 398.0, 0.18),
            (740.0, 480.0, 0.15),
            (865.0, 680.0, 0.22),
        ),
    ),
    (
        "translated-shape",
        (
            (80.0, 100.0, 0.10),
            (280.0, 100.0, 0.20),
            (280.0, 300.0, 0.15),
            (520.0, 360.0, 0.12),
        ),
        (
            (380.0, 300.0, 0.10),
            (580.0, 300.0, 0.20),
            (580.0, 500.0, 0.15),
            (820.0, 560.0, 0.12),
        ),
    ),
    (
        "unequal-length",
        (
            (80.0, 80.0, 0.11),
            (230.0, 110.0, 0.17),
            (390.0, 260.0, 0.09),
            (600.0, 240.0, 0.21),
            (830.0, 500.0, 0.13),
        ),
        (
            (95.0, 70.0, 0.15),
            (250.0, 125.0, 0.12),
            (610.0, 230.0, 0.18),
            (815.0, 520.0, 0.20),
        ),
    ),
    (
        "direction-wrap-around",
        (
            (500.0, 400.0, 0.10),
            (300.0, 405.0, 0.12),
            (120.0, 390.0, 0.14),
            (100.0, 650.0, 0.16),
        ),
        (
            (500.0, 400.0, 0.11),
            (300.0, 395.0, 0.10),
            (120.0, 410.0, 0.18),
            (90.0, 640.0, 0.13),
        ),
    ),
    (
        "duration-only",
        (
            (150.0, 150.0, 0.08),
            (350.0, 150.0, 0.12),
            (350.0, 350.0, 0.20),
            (650.0, 350.0, 0.10),
        ),
        (
            (150.0, 150.0, 0.16),
            (350.0, 150.0, 0.09),
            (350.0, 350.0, 0.25),
            (650.0, 350.0, 0.20),
        ),
    ),
)


def as_record_array(fixations: tuple[Fixation, ...]) -> np.ndarray:
    dtype = np.dtype(
        [("start_x", "f8"), ("start_y", "f8"), ("duration", "f8")]
    )
    # NumPy interprets a tuple-of-tuples as one structured record in some
    # versions; a list unambiguously means one record per fixation.
    return np.array(list(fixations), dtype=dtype)


def scala_double(value: float) -> str:
    rendered = repr(float(value))
    return rendered if "." in rendered or "e" in rendered.lower() else f"{rendered}.0"


def render_fixations(fixations: tuple[Fixation, ...]) -> str:
    values = ",\n".join(
        "        Fixation("
        f"{scala_double(x)}, {scala_double(y)}, {scala_double(duration)})"
        for x, y, duration in fixations
    )
    return f"Vector(\n{values}\n      )"


def render() -> str:
    package_version = version("multimatch-gaze")
    cases: list[str] = []
    for name, left, right in FIXTURES:
        scores = multimatch_gaze.docomparison(
            as_record_array(left),
            as_record_array(right),
            screensize=list(SCREEN),
            grouping=GROUPING,
        )
        if not np.all(np.isfinite(scores)):
            raise RuntimeError(f"{name}: reference returned non-finite scores: {scores}")
        expected = ",\n".join(
            f"        {dimension} = {scala_double(score)}"
            for dimension, score in zip(
                ("shape", "direction", "length", "position", "duration"),
                scores,
                strict=True,
            )
        )
        cases.append(
            f'''    Case(
      "{name}",
      {render_fixations(left)},
      {render_fixations(right)},
      Expected(
{expected}
      )
    )'''
        )

    rendered_cases = ",\n".join(cases)
    return f"""/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// Generated by tools/python-conformance/generate_multimatch_fixtures.py.
// Oracle: multimatch-gaze {package_version}, grouping = false.
// Do not edit by hand.

package eyes4s.compare

private[compare] object MultiMatchPythonFixtures:

  final case class Fixation(x: Double, y: Double, durationSeconds: Double)

  final case class Expected(
      shape: Double,
      direction: Double,
      length: Double,
      position: Double,
      duration: Double
  )

  final case class Case(
      name: String,
      left: Vector[Fixation],
      right: Vector[Fixation],
      expected: Expected
  )

  val screenWidth: Int  = {SCREEN[0]}
  val screenHeight: Int = {SCREEN[1]}

  val cases: Vector[Case] = Vector(
{rendered_cases}
  )

end MultiMatchPythonFixtures
"""


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--check",
        action="store_true",
        help="fail if the checked-in fixture differs instead of rewriting it",
    )
    args = parser.parse_args()
    generated = render()

    if args.check:
        if not OUTPUT.exists() or OUTPUT.read_text() != generated:
            raise SystemExit(
                f"{OUTPUT} is stale; regenerate it with:\n"
                "uv run tools/python-conformance/generate_multimatch_fixtures.py"
            )
        print(f"{OUTPUT}: up to date")
    else:
        OUTPUT.parent.mkdir(parents=True, exist_ok=True)
        OUTPUT.write_text(generated)
        print(f"wrote {OUTPUT}")


if __name__ == "__main__":
    main()

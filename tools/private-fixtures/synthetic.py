#!/usr/bin/env python3
"""Generate a redistributable asymmetric, weighted, edge-sensitive UI/parity request."""
import argparse
from pathlib import Path
from run import dump
from end_to_end import validate


def request():
    p = {"schema_version": 1, "x_min": 60, "y_min": 40, "x_max": 380, "y_max": 240,
         "nx": 32, "ny": 20, "sigma_px": 20, "min_fixations": 2,
         "screen_width_px": 800, "screen_height_px": 600, "sample_rate_hz": 250,
         "pixels_per_degree": 30, "geometry_note": "Synthetic display with explicitly declared 30 px/degree; no measured participant geometry.",
         "templates": [], "sources": []}
    def fixations(points):
        return {"x": [v[0] for v in points], "y": [v[1] for v in points],
                "duration_ms": [v[2] for v in points], "onset_ms": [0, 500, 1000]}
    for participant, offset in (("P1", 0), ("P2", 13)):
        for name, points in (("a", [(60.1, 40.1, 120), (145 + offset, 95, 300), (205, 155, 60)]),
                             ("b", [(379.9, 239.9, 320), (265, 135 + offset, 100), (105, 195, 80)])):
            ident = f"{participant}-{name}"
            p["templates"].append({"id": ident, "participant": participant, "fixations": fixations(points)})
            for phase, shift in (("Cue", .9), ("Delay", 4.9), ("Probe", -.05)):
                shifted = [(min(379.99, max(60, x + shift)), y, d) for x, y, d in points]
                p["sources"].append({"id": f"{ident}-{phase}", "participant": participant,
                                     "phase": phase, "template": ident, "fixations": fixations(shifted)})
    validate(p)
    return p


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    dump(parser.parse_args().output, request())

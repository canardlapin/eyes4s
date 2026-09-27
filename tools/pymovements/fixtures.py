#!/usr/bin/env python3
"""Deterministic synthetic benchmark inputs; no scientific method is executed."""

import argparse
import hashlib
import json
from pathlib import Path

SIZES = {"small": 10_000, "typical": 100_000, "large": 1_000_000}
EVENT_SIZES = {"small": 1_000, "typical": 10_000, "large": 100_000}
FAMILIES = ("steps", "gaps-binocular", "stationary", "dispersion-churn")


def decimal(value):
    return f"{value // 100}.{value % 100:02d}"


def sample(index, family):
    rate = 1000 if family == "gaps-binocular" else 500
    x, y = ((48000, 27000), (72000, 54000), (144000, 81000), (96000, 30000))[(index // 250) % 4]
    if family == "stationary":
        x, y = 96000, 54000
    elif family == "dispersion-churn":
        # An 81-sample plateau admits a real fixation before the threshold-churning ramp.
        x, y = 96000 + min(max(index % 200 - 80, 0), 100) * 100, 54000
    else:
        x += ((index * 17 + 3) % 7) - 3
        y += ((index * 31 + 5) % 7) - 3
        if family == "gaps-binocular" and index % 250 < 20:
            # Sustained movement exercises min-duration microsaccades, not only isolated jumps.
            x += ((index % 250) - 19) * 350
    valid = not (family == "gaps-binocular" and index % 1000 in range(450, 480))
    return (index * (1000 // rate), decimal(x), decimal(y), "1000" if valid else "0", int(valid))


def chunks(spec):
    """Yield canonical UTF-8 bytes; integer arithmetic fixes bytes across runtimes."""
    family, scale, form = spec["family"], spec["scale"], spec["format"]
    if family == "reading-events":
        yield b"ordinal,onset_us,offset_us,x_px,y_px,word_index,valid\n"
        for i in range(EVENT_SIZES[scale]):
            word = (i - 2 if i % 11 == 10 else i) % 72
            x, y = 300 + (word % 12) * 110, 270 + (word // 12) * 80
            yield f"{i},{i*220000},{i*220000+200000},{x},{y},{word},{int(i%97!=96)}\n".encode()
        return
    if family not in FAMILIES or form not in ("csv", "asc"):
        raise ValueError("unknown synthetic fixture family/format")
    binocular = family == "gaps-binocular"
    if form == "csv":
        yield b"time_ms,x_px,y_px,pupil,valid,right_x_px,right_y_px,right_pupil,right_valid\n"
    else:
        eyes = "LEFT RIGHT" if binocular else "LEFT"
        rate = 1000 if binocular else 500
        yield (f"** Synthetic eyes4s performance fixture v1\n"
               f"SAMPLES GAZE {eyes} RATE {rate}.00\n"
               f"START 0 {eyes} SAMPLES\nMSG 0 DISPLAY_COORDS 0 0 1919 1079\n").encode()
    for i in range(SIZES[scale]):
        t, x, y, pupil, valid = sample(i, family)
        if form == "csv":
            right = f"{x},{y},{pupil},{valid}" if binocular else ",,,"
            left = f"{x},{y}" if valid else ","
            if binocular and not valid:
                right = ",,0,0"
            yield f"{t},{left},{pupil},{valid},{right}\n".encode()
        else:
            gaze = f"{x}\t{y}\t{pupil}" if valid else ".\t.\t0"
            yield (f"{t}\t{gaze}" + (f"\t{gaze}" if binocular else "") + "\n").encode()
    if form == "asc":
        last_t = (SIZES[scale] - 1) * (1 if binocular else 2)
        yield f"END {last_t} SAMPLES EVENTS RES 30.00 30.00\n".encode()


def identity(spec):
    digest, size = hashlib.sha256(), 0
    for data in chunks(spec):
        digest.update(data)
        size += len(data)
    return {"sha256": digest.hexdigest(), "bytes": size}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--write", metavar="DATASET_ID")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    config = json.loads(Path(__file__).with_name("performance.json").read_text())
    data = {d["id"]: d for d in config["datasets"] if d["kind"] == "synthetic"}
    if args.check:
        for id, spec in data.items():
            if identity(spec) != {k: spec[k] for k in ("sha256", "bytes")}:
                parser.exit(1, f"FAIL: fixture drift: {id}\n")
        print(f"PASS: {len(data)} synthetic input byte identities (no benchmark executed)")
    elif args.write and args.output:
        if args.write not in data:
            parser.error("unknown synthetic dataset")
        spec = data[args.write]
        if identity(spec) != {k: spec[k] for k in ("sha256", "bytes")}:
            parser.exit(1, "FAIL: generator/config drift\n")
        with args.output.open("xb") as stream:
            for chunk in chunks(spec):
                stream.write(chunk)
        print(args.output)
    else:
        parser.error("use --check or --write DATASET_ID --output NEW_FILE")


if __name__ == "__main__":
    main()

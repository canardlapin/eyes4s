#!/usr/bin/env python3
"""Regenerate the Eyes Studio acceptance fixture (fixtures/studio-golden).

One deterministic study that both eyes4s core and the studio read. Its counts
realise docs/studio/fixture/FIXTURE.md exactly; its scores are NOT the mock's
(those are random draws; real scores are frozen later from eyes4s output).

The design (participants, item assignment, encoding order, quarantined and
absent trials, retrieval responses) is taken from the mock study by running
docs/studio/fixture/make_fixture.py in a temporary directory, so the two
cannot drift apart. Everything else is drawn from random.Random seeded with
SEED and from integer arithmetic. Stimuli are palette PNGs written by a small
self-contained encoder (fixed-Huffman deflate, run-length and previous-row
matches only), so their bytes depend on neither Pillow nor the zlib build.

    python3 tools/studio-fixture/generate.py          # write the fixture
    python3 tools/studio-fixture/generate.py --check  # byte-compare, exit 1 on drift
"""
from __future__ import annotations

import argparse
import contextlib
import hashlib
import io
import math
import os
import random
import re
import runpy
import struct
import sys
import tempfile
import zlib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MOCK = ROOT / "docs" / "studio" / "fixture" / "make_fixture.py"
OUT = ROOT / "fixtures" / "studio-golden"

SEED = 20260925
SCREEN_W, SCREEN_H = 1920, 1080
IMG_W, IMG_H = 1024, 768
OFF_X, OFF_Y = 448, 156  # image frame top-left on screen
SAMPLE_HZ = 500  # sample_count = duration_ms * 500 / 1000
TOTAL_RECORDS = 11520
OUTSIDE_RECORDS, OUTSIDE_TRIALS = 543, 409
FOCUS_RECORD = 7214  # 1-based record number of P17 enc_03 ordinal 6
MISSING_IMAGES = ("forest-044", "kitchen-081")

FIX_HEADER = [
    "participant",
    "phase",
    "trial",
    "occurrence",
    "ordinal",
    "x",
    "y",
    "onset_ms",
    "duration_ms",
    "sample_count",
]
TRIAL_HEADER = [
    "participant",
    "phase",
    "trial",
    "occurrence",
    "item",
    "display_kind",
    "image_file",
    "response",
]
PHASE = {"enc": "Encoding", "ret": "Retrieval"}


# --------------------------------------------------------------------------- design


def load_mock() -> dict:
    """Execute the mock study in a scratch directory and return its globals."""
    here = os.getcwd()
    with tempfile.TemporaryDirectory() as tmp:
        try:
            os.chdir(tmp)
            with contextlib.redirect_stdout(io.StringIO()):
                return runpy.run_path(str(MOCK))
        finally:
            os.chdir(here)


def design(mock: dict) -> dict:
    parts = mock["parts"]
    enc_of_item = mock["enc_of_item"]
    trials = []  # ordered: participant, enc_01..enc_20, ret_01..ret_20
    for P in mock["study"]["participants"]:
        p = P["id"]
        queries = P["queries"]
        enc_item = {}
        for k, q in enumerate(queries):
            assert q["trial"] == f"ret_{k + 1:02d}" and q["match"] == enc_of_item(k)
            enc_item[q["match"]] = q["item"]
        for n in range(1, 21):
            t = f"enc_{n:02d}"
            trials.append(dict(p=p, ph="enc", t=t, item=enc_item[t], response=""))
        for q in queries:
            trials.append(
                dict(
                    p=p, ph="ret", t=q["trial"], item=q["item"], response=q["response"]
                )
            )
    assert [t["p"] for t in trials[::40]] == parts and len(trials) == 960
    return dict(
        trials=trials,
        quarantine=dict(mock["quarantine"]),
        absent=set(mock["absent"]),
        failed=set(mock["failed_q"]),
        enc_of_item=enc_of_item,
    )


# --------------------------------------------------------------------------- stimuli

# A fixed palette: index 0..15. Backgrounds use 0..7, shapes 8..15.
PALETTE = [
    (214, 196, 160),
    (120, 158, 112),
    (196, 190, 178),
    (150, 150, 158),
    (170, 200, 150),
    (200, 200, 208),
    (228, 214, 186),
    (176, 186, 204),
    (200, 60, 50),
    (40, 90, 170),
    (240, 190, 40),
    (60, 140, 70),
    (120, 60, 140),
    (30, 30, 36),
    (245, 245, 240),
    (230, 120, 40),
]


def item_rng(item: str) -> random.Random:
    return random.Random(
        int.from_bytes(hashlib.sha256(f"{SEED}:{item}".encode()).digest()[:8], "big")
    )


def scene(item: str, category_index: int) -> dict:
    """Flat shapes on a two-band background. Salient points are shape centres."""
    r = item_rng(item)
    bg = category_index % 8
    band = (bg + 1 + r.randrange(7)) % 8
    horizon = r.randrange(300, 560)
    shapes = []  # (kind, cx, cy, rx, ry, colour); rect uses half-sizes
    n = 3 + r.randrange(3)
    for _ in range(n):
        for _attempt in range(40):
            rx, ry = r.randrange(40, 130), r.randrange(40, 130)
            cx = r.randrange(rx + 30, IMG_W - rx - 30)
            cy = r.randrange(ry + 30, IMG_H - ry - 30)
            if all(
                abs(cx - s[1]) > rx + s[3] + 10 or abs(cy - s[2]) > ry + s[4] + 10
                for s in shapes
            ):
                break
        shapes.append(
            (
                "ellipse" if r.random() < 0.5 else "rect",
                cx,
                cy,
                rx,
                ry,
                8 + r.randrange(8),
            )
        )
    weights = [s[3] * s[4] + r.randrange(2000, 8000) for s in shapes]
    return dict(bg=bg, band=band, horizon=horizon, shapes=shapes, weights=weights)


def ellipse_span(cx: int, cy: int, rx: int, ry: int, y: int):
    """Pixel columns whose centres lie inside the ellipse, in integer arithmetic."""
    dy = 2 * y + 1 - 2 * cy
    rhs = 4 * rx * rx * ry * ry - dy * dy * rx * rx
    if rhs < 0:
        return None
    # (2x+1-2cx)^2 * ry^2 <= rhs  <=>  |2x+1-2cx| <= isqrt(rhs // ry^2)
    m = math.isqrt(rhs // (ry * ry))
    hi = (m + 2 * cx - 1) // 2
    lo = -((-(2 * cx - 1 - m)) // 2)  # ceil((2cx - 1 - m) / 2)
    return max(lo, 0), min(hi + 1, IMG_W)


def raster_rows(sc: dict):
    """Yield each row of palette indices as bytes."""
    for y in range(IMG_H):
        row = bytearray([sc["bg"] if y < sc["horizon"] else sc["band"]]) * IMG_W
        for kind, cx, cy, rx, ry, colour in sc["shapes"]:
            if kind == "rect":
                if cy - ry <= y < cy + ry:
                    row[cx - rx : cx + rx] = bytes([colour]) * (2 * rx)
            else:
                span = ellipse_span(cx, cy, rx, ry, y)
                if span and span[1] > span[0]:
                    row[span[0] : span[1]] = bytes([colour]) * (span[1] - span[0])
        yield bytes(row)


class BitWriter:
    def __init__(self) -> None:
        self.out = bytearray()
        self.acc = 0
        self.n = 0

    def bits(self, value: int, count: int) -> None:  # LSB-first
        self.acc |= value << self.n
        self.n += count
        while self.n >= 8:
            self.out.append(self.acc & 0xFF)
            self.acc >>= 8
            self.n -= 8

    def huff(self, code: int, length: int) -> None:  # Huffman codes MSB-first
        rev = 0
        for _ in range(length):
            rev = (rev << 1) | (code & 1)
            code >>= 1
        self.bits(rev, length)

    def flush(self) -> bytes:
        if self.n:
            self.out.append(self.acc & 0xFF)
        return bytes(self.out)


LEN_BASE = [
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    13,
    15,
    17,
    19,
    23,
    27,
    31,
    35,
    43,
    51,
    59,
    67,
    83,
    99,
    115,
    131,
    163,
    195,
    227,
    258,
]
LEN_EXTRA = [0] * 8 + [1] * 4 + [2] * 4 + [3] * 4 + [4] * 4 + [5] * 4 + [0]
DIST_BASE = [
    1,
    2,
    3,
    4,
    5,
    7,
    9,
    13,
    17,
    25,
    33,
    49,
    65,
    97,
    129,
    193,
    257,
    385,
    513,
    769,
    1025,
    1537,
    2049,
    3073,
]
DIST_EXTRA = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10]


def fixed_litlen(w: BitWriter, sym: int) -> None:
    if sym < 144:
        w.huff(0x30 + sym, 8)
    elif sym < 256:
        w.huff(0x190 + sym - 144, 9)
    elif sym < 280:
        w.huff(sym - 256, 7)
    else:
        w.huff(0xC0 + sym - 280, 8)


def match(w: BitWriter, length: int, dist: int) -> None:
    i = max(k for k in range(29) if LEN_BASE[k] <= length)
    fixed_litlen(w, 257 + i)
    w.bits(length - LEN_BASE[i], LEN_EXTRA[i])
    j = max(k for k in range(24) if DIST_BASE[k] <= dist)
    w.huff(j, 5)
    w.bits(dist - DIST_BASE[j], DIST_EXTRA[j])


def copy(w: BitWriter, total: int, dist: int) -> None:
    while total > 0:
        if total < 3:
            raise AssertionError("copy remainder below the minimum match")
        n = min(total, 258)
        if 0 < total - n < 3:
            n = total - 3
        match(w, n, dist)
        total -= n


def deflate(rows) -> bytes:
    """A zlib stream: one fixed-Huffman block. Each scanline is filter 0 +
    indices; a line equal to the previous one is a single back-reference at
    the line stride, otherwise runs become a literal plus a distance-1 copy."""
    stride = IMG_W + 1
    w = BitWriter()
    w.bits(1, 1)  # BFINAL
    w.bits(1, 2)  # fixed Huffman
    prev = None
    adler = 1
    pending = 0  # bytes of whole lines still to copy from the previous line
    for row in rows:
        line = b"\x00" + row
        adler = zlib.adler32(line, adler)
        if line == prev:
            pending += stride
            continue
        if pending:
            copy(w, pending, stride)
            pending = 0
        for m in re.finditer(rb"(.)\1*", line, re.S):
            value, rest = m.group()[0], len(m.group()) - 1
            fixed_litlen(w, value)
            if rest >= 3:
                copy(w, rest, 1)
            else:
                for _ in range(rest):
                    fixed_litlen(w, value)
        prev = line
    if pending:
        copy(w, pending, stride)
    fixed_litlen(w, 256)
    return b"\x78\x01" + w.flush() + struct.pack(">I", adler)


def chunk(kind: bytes, data: bytes) -> bytes:
    return (
        struct.pack(">I", len(data))
        + kind
        + data
        + struct.pack(">I", zlib.crc32(kind + data))
    )


def png(sc: dict) -> bytes:
    ihdr = struct.pack(">IIBBBBB", IMG_W, IMG_H, 8, 3, 0, 0, 0)
    plte = b"".join(bytes(c) for c in PALETTE)
    return (
        b"\x89PNG\r\n\x1a\n"
        + chunk(b"IHDR", ihdr)
        + chunk(b"PLTE", plte)
        + chunk(b"IDAT", deflate(raster_rows(sc)))
        + chunk(b"IEND", b"")
    )


# --------------------------------------------------------------------------- gaze

INSIDE = (OFF_X + 4, OFF_Y + 4, OFF_X + IMG_W - 4, OFF_Y + IMG_H - 4)


def clip_inside(x: float, y: float) -> tuple[float, float]:
    return min(max(x, INSIDE[0]), INSIDE[2]), min(max(y, INSIDE[1]), INSIDE[3])


def outside_point(r: random.Random) -> tuple[float, float]:
    """A point on screen, at least 24 px outside the image frame and 24 px inside the screen."""
    side = r.randrange(4)
    if side == 0:  # left margin
        return r.uniform(24, OFF_X - 24), r.uniform(24, SCREEN_H - 24)
    if side == 1:  # right margin
        return r.uniform(OFF_X + IMG_W + 24, SCREEN_W - 24), r.uniform(
            24, SCREEN_H - 24
        )
    if side == 2:  # top margin
        return r.uniform(24, SCREEN_W - 24), r.uniform(24, OFF_Y - 24)
    return r.uniform(24, SCREEN_W - 24), r.uniform(OFF_Y + IMG_H + 24, SCREEN_H - 24)


def salient_point(r: random.Random, sc: dict) -> tuple[float, float]:
    if r.random() < 0.12:  # background exploration
        return clip_inside(
            r.uniform(INSIDE[0], INSIDE[2]), r.uniform(INSIDE[1], INSIDE[3])
        )
    s = r.choices(sc["shapes"], weights=sc["weights"])[0]
    return clip_inside(
        OFF_X + s[1] + r.gauss(0, s[3] / 3), OFF_Y + s[2] + r.gauss(0, s[4] / 3)
    )


def fmt(v: float) -> str:
    return f"{v:.1f}"


# --------------------------------------------------------------------------- counts


def allocate_counts(d: dict, r: random.Random) -> dict:
    """Records per trial. Totals are exact; P17 enc_03 ordinal 6 lands on FOCUS_RECORD."""
    keys = [
        (t["p"], t["t"]) for t in d["trials"] if (t["p"], t["t"]) not in d["absent"]
    ]
    assert len(keys) == 954
    pinned = {("P17", "ret_07"): 12, ("P17", "enc_03"): 13}
    pinned.update({k: 11 for k in d["failed"]})
    focus_at = keys.index(("P17", "enc_03"))
    before, after = keys[:focus_at], keys[focus_at + 1 :]
    rate = {p: r.uniform(10.3, 12.3) for p in sorted({k[0] for k in keys})}
    counts = {}
    for k in keys:
        counts[k] = pinned.get(k, max(6, min(18, round(rate[k[0]] + r.gauss(0, 1.8)))))
    # Participants differ in fixation rate; the two targets place the focus record.
    for group, target in (
        (before, FOCUS_RECORD - 1 - 5),
        (after, TOTAL_RECORDS - (FOCUS_RECORD - 1 - 5) - 13),
    ):
        free = [k for k in group if k not in pinned]
        i = 0
        while sum(counts[k] for k in group) != target:
            k = free[(i * 7919) % len(free)]
            diff = target - sum(counts[k] for k in group)
            step = 1 if diff > 0 else -1
            if 6 <= counts[k] + step <= 18:
                counts[k] += step
            i += 1
    assert sum(counts.values()) == TOTAL_RECORDS
    return counts


def allocate_outside(d: dict, counts: dict, r: random.Random) -> dict:
    """Outside-window records per trial; none in quarantined trials."""
    outside = {k: 0 for k in counts}
    for k in d["failed"]:
        outside[k] = counts[k]
    outside[("P17", "ret_07")] = 1
    outside[("P17", "enc_03")] = 1
    fixed = set(d["failed"]) | {("P17", "ret_07"), ("P17", "enc_03")}
    eligible = [k for k in counts if k not in fixed and k not in d["quarantine"]]
    need_trials = OUTSIDE_TRIALS - len(fixed)
    chosen = sorted(r.sample(eligible, need_trials), key=eligible.index)
    for k in chosen:
        outside[k] = 1
    extra = OUTSIDE_RECORDS - sum(outside.values())
    for k in sorted(r.sample(chosen, extra), key=eligible.index):
        outside[k] = 2
    assert sum(outside.values()) == OUTSIDE_RECORDS
    assert sum(v > 0 for v in outside.values()) == OUTSIDE_TRIALS
    return outside


# --------------------------------------------------------------------------- records


def timeline(r: random.Random, n: int) -> list[tuple[int, int]]:
    onset = r.randrange(40, 120)
    out = []
    for _ in range(n):
        dur = 2 * max(40, min(350, round(r.gauss(290, 90) / 2)))  # even: whole 500 Hz samples
        out.append((onset, dur))
        onset += dur + r.randrange(24, 70)
    return out


FOCUS_ENC_DURATIONS = [330, 380, 350, 420, 370, 412, 340, 310, 360, 126, 330, 300, 350]
FOCUS_ENC_OUTSIDE = 10  # ordinal of the outside fixation in P17 enc_03
FOCUS_RET_OUTSIDE = 9  # ordinal of the outside fixation in P17 ret_07


def build(d: dict):
    r_counts = random.Random(SEED)
    r_out = random.Random(SEED + 1)
    r_gaze = random.Random(SEED + 2)
    counts = allocate_counts(d, r_counts)
    outside = allocate_outside(d, counts, r_out)

    cats = sorted({t["item"].split("-")[0] for t in d["trials"]})
    scenes = {}
    for t in d["trials"]:
        if t["item"] not in scenes:
            scenes[t["item"]] = scene(t["item"], cats.index(t["item"].split("-")[0]))

    enc_points: dict[tuple[str, str], list[tuple[float, float]]] = {}
    rows = []
    for t in d["trials"]:
        key = (t["p"], t["t"])
        if key in d["absent"]:
            continue
        n, n_out = counts[key], outside[key]
        sc = scenes[t["item"]]
        r = r_gaze
        times = timeline(r, n)
        # which ordinals (1-based) fall outside
        if key == ("P17", "enc_03"):
            out_set = {FOCUS_ENC_OUTSIDE}
            times, o = [], 60
            for dur in FOCUS_ENC_DURATIONS:
                times.append((o, dur))
                o += dur + 50
            assert times[5] == (2160, 412)
        elif key == ("P17", "ret_07"):
            out_set = {FOCUS_RET_OUTSIDE}
            # the outside fixation carries 4% of the trial's fixation duration
            others = sum(
                dur for i, (_, dur) in enumerate(times) if i + 1 != FOCUS_RET_OUTSIDE
            )
            target = 2 * round(others * 0.02 / 0.96)
            times = [
                (o, target if i + 1 == FOCUS_RET_OUTSIDE else dur)
                for i, (o, dur) in enumerate(times)
            ]
            times = [
                (times[0][0] + sum(dd + 45 for _, dd in times[:i]), dur)
                for i, (_, dur) in enumerate(times)
            ]
        else:
            out_set = set(sorted(r.sample(range(1, n + 1), n_out))) if n_out else set()
        points = []
        for i in range(1, n + 1):
            if i in out_set:
                points.append(outside_point(r))
            elif t["ph"] == "enc":
                points.append(salient_point(r, sc))
            else:
                echo = 0.75 if t["response"] == "Remembered" else 0.25
                memory = enc_points.get((t["p"], d["enc_of_item"](int(t["t"][4:]) - 1)))
                u = r.random()
                if memory and u < echo:
                    mx, my = r.choice(memory)
                    points.append(clip_inside(mx + r.gauss(0, 35), my + r.gauss(0, 35)))
                elif u < echo + 0.2 or not memory:  # the central fixation cross
                    points.append(
                        clip_inside(960 + r.gauss(0, 40), 540 + r.gauss(0, 40))
                    )
                else:
                    points.append(
                        salient_point(r, scenes[t["item"]])
                        if r.random() < 0.3
                        else clip_inside(
                            r.uniform(INSIDE[0], INSIDE[2]),
                            r.uniform(INSIDE[1], INSIDE[3]),
                        )
                    )
        if key == ("P17", "enc_03"):
            points[5] = (1148.0, 456.0)
        if t["ph"] == "enc":
            enc_points[key] = [p for i, p in enumerate(points, 1) if i not in out_set]

        ordinals = list(range(1, n + 1))
        sample_counts = [dur * SAMPLE_HZ // 1000 for _, dur in times]
        cause = d["quarantine"].get(key)
        if cause == "duplicate-ordinals":
            j = r.randrange(2, n + 1)  # record j repeats the ordinal of record j-1
            ordinals = [o if o < j else o - 1 for o in ordinals]
        elif cause == "overlap":
            j = r.randrange(1, n)  # 0-based index of the overlapping fixation
            po, pd = times[j - 1]
            shift = times[j][0] - (po + pd - 40)
            times = times[:j] + [(o - shift, dur) for o, dur in times[j:]]
            assert times[j][0] > times[j - 1][0]
        elif cause == "rejected-records":
            sample_counts[r.randrange(n)] = 0
        elif cause == "no-fixations":
            sample_counts = [0] * n
        for i in range(n):
            x, y = points[i]
            rows.append(
                [
                    t["p"],
                    PHASE[t["ph"]],
                    t["t"],
                    "1",
                    str(ordinals[i]),
                    fmt(x),
                    fmt(y),
                    str(times[i][0]),
                    str(times[i][1]),
                    str(sample_counts[i]),
                ]
            )
    return rows, scenes


def csv_text(header: list[str], rows: list[list[str]]) -> str:
    for row in rows:
        assert all("," not in c and '"' not in c for c in row)
    return "\n".join(",".join(r) for r in [header] + rows) + "\n"


def trial_rows(d: dict) -> list[list[str]]:
    out = []
    for t in d["trials"]:
        enc = t["ph"] == "enc"
        out.append(
            [
                t["p"],
                PHASE[t["ph"]],
                t["t"],
                "1",
                t["item"],
                "image" if enc else "blank+fixation-cross",
                f"{t['item']}.png" if enc else "",
                t["response"],
            ]
        )
    return out


# Files of the fixture directory that eyes4s writes, not this script.
EYES4S_OUTPUTS = {"SCORES.json"}


def readme(d: dict, n_items: int) -> str:
    q = d["quarantine"]
    by_cause = {c: sum(v == c for v in q.values()) for c in sorted(set(q.values()))}
    return f"""# Eyes Studio acceptance fixture (studio-golden)

Generated by `tools/studio-fixture/generate.py` (seed {SEED}); do not hand-edit.
`python3 tools/studio-fixture/generate.py --check` byte-compares every file here
against a fresh in-memory regeneration. `python3 tools/studio-fixture/verify_counts.py`
recomputes the counts below from these files.

The design (participants, items, encoding order, quarantined and absent trials,
retrieval responses) is the mock study of `docs/studio/fixture/make_fixture.py`,
summarised in `docs/studio/fixture/FIXTURE.md`. The scores are not the mock's:
real scores are computed by eyes4s from these files (ticket S0.7b) and frozen in
`SCORES.json`, which this script does not write. `sbt "ioJVM/Test/runMain
eyes4s.io.StudioScoresMain"` regenerates it; `StudioFixtureRealSuite` checks that
it regenerates byte for byte and that every FIXTURE.md count reproduces.

## Counts

| quantity | value |
|---|---|
| participants | 24 (P01–P24), each enc_01–enc_20 then ret_01–ret_20 |
| inventory (`trials.csv`) | 960 trials |
| admitted / quarantined / absent | 937 / 17 / 6 |
| quarantined by cause | {", ".join(f"{k} {v}" for k, v in by_cause.items())} |
| fixation records (`fixations.csv`) | {TOTAL_RECORDS:,} |
| records outside the image frame (on screen) | {OUTSIDE_RECORDS} in {OUTSIDE_TRIALS} trials; none in quarantined trials |
| records outside the screen | 0 |
| distinct items | {n_items}; images present {n_items - 2}, missing 2 (`forest-044.png`, `kitchen-081.png`) |
| P05 ret_04, ret_11, ret_16 | 11 records each, all outside the image frame |
| P17 ret_07 (beach-042, Remembered) | 12 fixations, 1 outside (~4% of fixation duration) |
| P17 enc_03 (beach-042, matched reference) | 13 fixations, 1 outside (~3% of fixation duration) |
| focus record | record 7,214 = P17 enc_03 ordinal 6: screen (1148, 456) = image (700, 300), onset 2160 ms, duration 412 ms |

Record numbers are 1-based in file order, excluding the header. The "% of
duration" is outside-fixation duration over total fixation duration of the trial.

## Geometry and units

- Screen 1920×1080 px, origin top-left, y down. `x`, `y` are screen px.
- Image frame (analysis window) 1024×768 at offset (448, 156): half-open
  `448 <= x < 1472`, `156 <= y < 924`. Image px = screen px − (448, 156).
- **Time units are declared, not inferred:** `onset_ms` and `duration_ms` are
  integer milliseconds from trial start. `sample_count` is the number of gaze
  samples at a declared {SAMPLE_HZ} Hz (`duration_ms` is always even and `sample_count` = `duration_ms` / 2 on every valid record).
- Declared 35 px/°; degrees are from image centre, x right, y up.

## `trials.csv` (960 rows)

`participant, phase, trial, occurrence, item, display_kind, image_file, response`

- Key: `participant, phase, trial, occurrence` (occurrence always 1). `item` is an
  attribute, not part of the key; a retrieval trial is matched to the encoding trial
  of the same participant with the same `item`.
- `phase`: `Encoding` | `Retrieval`. `display_kind`: `image` (Encoding) or
  `blank+fixation-cross` (Retrieval). `image_file`: `<item>.png` for Encoding, blank for
  Retrieval. `response`: `Remembered` | `Forgotten` for Retrieval (from the mock study's
  `fixture.json`), blank for Encoding.
- The 6 absent trials are listed here and have no records in `fixations.csv`.

## `fixations.csv` ({TOTAL_RECORDS:,} records)

`participant, phase, trial, occurrence, ordinal, x, y, onset_ms, duration_ms, sample_count`

Sorted by participant, then enc_01..enc_20, ret_01..ret_20, then record order within
the trial. Ordinals are 1-based. Coordinates are finite and on screen; durations are
positive; onsets increase within every trial; intervals do not overlap except in the
6 overlap trials. Gaze clusters on each image's shapes; retrieval gaze for a
Remembered item mostly revisits that participant's encoding fixations on the item,
for a Forgotten item mostly the central cross or elsewhere.

## Quarantine triggers (eyes4s `FixationCsv.read`)

`FixationCsv.read` rejects individual rows first (width, key, a non-negative integer
ordinal, a **positive integer sample count**, finite x/y inside the reading frame,
positive duration). It then groups the valid rows by key and, per trial, reports
`RejectedRecords` if any row of that key was rejected, else `DuplicateOrdinals` if
two rows share an ordinal, else `Scanpath.of`'s error: `Overlap` when, in ordinal
order, a fixation begins before the previous one ends.

| cause | construction here | trials |
|---|---|---|
| duplicate-ordinals | one record repeats the ordinal of the record before it (`…, k, k, k+1, …`) | {by_cause["duplicate-ordinals"]} |
| overlap | one fixation's onset is 40 ms before the previous fixation's offset (onsets still increase) | {by_cause["overlap"]} |
| rejected-records | one record has `sample_count` 0; the others are valid | {by_cause["rejected-records"]} |
| no-fixations | **every** record has `sample_count` 0 | {by_cause["no-fixations"]} |

An inventory trial whose every record is rejected is NoFixations; this takes
precedence over RejectedRecords (rule requested for UI-H). The current reader cannot
report it: `FixationCsv.read` builds a trial group only from valid rows, so
`Scanpath.of` never sees an empty trial, and these records come back as row-level
`Number` rejections carrying the trial key.

Read with a string key over `participant, phase, trial, occurrence`, the screen frame
and `TimestampUnit.Milliseconds`, the current reader reports 937 accepted trials,
11,311 admitted and 209 rejected records: `DuplicateOrdinals` 4, `Overlap` 6 and
`RejectedRecords` 2 trials, plus row-level `Number` rejections for the 5 no-fixations
trials. Its `rowNumber` counts the header as row 1, so record 7,214 is `rowNumber` 7215.

The reading frame is the screen (1920×1080 px). Records outside the image frame
are valid records; they are excluded from maps by the analysis window, not by import.

## Stimuli

`stimuli/<item>.png`: 1024×768 8-bit palette PNGs of flat synthetic shapes,
authored for eyes4s (Apache-2.0, redistributable). `forest-044.png` and
`kitchen-081.png` are deliberately absent (missing assets).
"""


def outputs() -> dict[str, bytes]:
    d = design(load_mock())
    rows, scenes = build(d)
    files = {
        "fixations.csv": csv_text(FIX_HEADER, rows).encode(),
        "trials.csv": csv_text(TRIAL_HEADER, trial_rows(d)).encode(),
        "README.md": readme(d, len(scenes)).encode(),
    }
    for item in sorted(scenes):
        if item not in MISSING_IMAGES:
            files[f"stimuli/{item}.png"] = png(scenes[item])
    return files


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--check", action="store_true", help="byte-compare instead of writing"
    )
    args = parser.parse_args()
    files = outputs()
    if args.check:
        drift = [
            p
            for p, c in files.items()
            if not (OUT / p).is_file() or (OUT / p).read_bytes() != c
        ]
        present = (
            {str(p.relative_to(OUT)) for p in OUT.rglob("*") if p.is_file()}
            if OUT.exists()
            else set()
        )
        # SCORES.json is eyes4s's output (S0.7b), checked by StudioFixtureRealSuite.
        drift += sorted(present - set(files) - EYES4S_OUTPUTS)
        if drift:
            print(
                f"Fixture drift in {len(drift)} file(s): {', '.join(drift[:10])}",
                file=sys.stderr,
            )
            return 1
    else:
        (OUT / "stimuli").mkdir(parents=True, exist_ok=True)
        for old in (OUT / "stimuli").glob("*.png"):
            if f"stimuli/{old.name}" not in files:
                old.unlink()
        for p, c in files.items():
            (OUT / p).write_bytes(c)
    size = sum(len(c) for c in files.values())
    print(
        f'{"Checked" if args.check else "Generated"} {len(files)} files ({size / 1e6:.2f} MB) in {OUT.relative_to(ROOT)}'
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())

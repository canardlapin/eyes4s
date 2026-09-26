#!/usr/bin/env python3
"""Recompute every FIXTURE.md count from fixtures/studio-golden and assert it.

Independent of generate.py: reads only the CSVs and the stimulus folder, uses
the frame geometry from the fixture README, and reimplements the four
quarantine triggers of eyes4s FixationCsv.read as documented there. The mock
study (docs/studio/fixture/make_fixture.py) is run in a scratch directory only
to compare the per-trial quarantine/absent sets and retrieval responses.
"""
from __future__ import annotations

import contextlib
import csv
import io
import math
import os
import runpy
import struct
import sys
import tempfile
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
FIX = ROOT / "fixtures" / "studio-golden"
MOCK = ROOT / "docs" / "studio" / "fixture" / "make_fixture.py"
SCREEN = (0, 0, 1920, 1080)
WINDOW = (448, 156, 448 + 1024, 156 + 768)


def inside(box, x, y):
    return box[0] <= x < box[2] and box[1] <= y < box[3]


def mock():
    here = os.getcwd()
    with tempfile.TemporaryDirectory() as tmp:
        try:
            os.chdir(tmp)
            with contextlib.redirect_stdout(io.StringIO()):
                return runpy.run_path(str(MOCK))
        finally:
            os.chdir(here)


def row_ok(r) -> bool:
    """FixationCsv.read's row-level admission, restricted to what this file can fail."""
    try:
        ordinal, count = int(r["ordinal"]), int(r["sample_count"])
        x, y = float(r["x"]), float(r["y"])
        duration = float(r["duration_ms"])
        float(r["onset_ms"])
    except ValueError:
        return False
    return (
        ordinal >= 0
        and count > 0
        and math.isfinite(x)
        and math.isfinite(y)
        and inside(SCREEN, x, y)
        and duration > 0
    )


def trial_status(rows) -> str:
    valid = [r for r in rows if row_ok(r)]
    if not valid:
        return "no-fixations"  # every record rejected; see README
    if len(valid) != len(rows):
        return "rejected-records"
    ords = [int(r["ordinal"]) for r in valid]
    if len(set(ords)) != len(ords):
        return "duplicate-ordinals"
    ordered = sorted(valid, key=lambda r: int(r["ordinal"]))
    for a, b in zip(ordered, ordered[1:]):
        if float(b["onset_ms"]) < float(a["onset_ms"]) + float(a["duration_ms"]):
            return "overlap"
    return "admitted"


def png_size(path: Path):
    head = path.read_bytes()[:24]
    assert head[:8] == b"\x89PNG\r\n\x1a\n" and head[12:16] == b"IHDR", path
    return struct.unpack(">II", head[16:24])


def main() -> int:
    trials = list(csv.DictReader((FIX / "trials.csv").open()))
    records = list(csv.DictReader((FIX / "fixations.csv").open()))
    key = lambda r: (r["participant"], r["trial"], r["occurrence"])
    by_trial = defaultdict(list)
    for n, r in enumerate(records, 1):
        r["_n"] = n
        by_trial[key(r)].append(r)
    inv = {key(t): t for t in trials}
    assert len(inv) == len(trials), "duplicate trial keys"
    assert set(by_trial) <= set(inv), "records for trials outside the inventory"

    status = {
        k: ("absent" if k not in by_trial else trial_status(by_trial[k])) for k in inv
    }
    cause = lambda c: sum(v == c for v in status.values())
    quarantined = {k: v for k, v in status.items() if v not in ("admitted", "absent")}

    out_rec = defaultdict(int)
    offscreen = nonfinite = 0
    for r in records:
        x, y = float(r["x"]), float(r["y"])
        nonfinite += not (math.isfinite(x) and math.isfinite(y))
        offscreen += not inside(SCREEN, x, y)
        if inside(SCREEN, x, y) and not inside(WINDOW, x, y):
            out_rec[key(r)] += 1

    # timing: positive durations, increasing onsets, overlap only where quarantined for it
    bad_time = []
    for k, rows in by_trial.items():
        on = [float(r["onset_ms"]) for r in rows]
        du = [float(r["duration_ms"]) for r in rows]
        overl = any(on[i] < on[i - 1] + du[i - 1] for i in range(1, len(rows)))
        if (
            min(du) <= 0
            or any(b <= a for a, b in zip(on, on[1:]))
            or (overl and status[k] != "overlap")
        ):
            bad_time.append(k)

    # images
    items = {t["item"] for t in trials}
    pngs = {p.stem: p for p in (FIX / "stimuli").glob("*.png")}
    sizes_ok = all(png_size(p) == (1024, 768) for p in pngs.values())
    missing = sorted(items - set(pngs))

    # query contrasts (retrieval trials) and controls
    enc_by_item = {
        (t["participant"], t["item"]): (t["participant"], t["trial"], t["occurrence"])
        for t in trials
        if t["phase"] == "Encoding"
    }
    qc = defaultdict(int)
    controls = defaultdict(int)
    for k, t in inv.items():
        if t["phase"] != "Retrieval":
            continue
        qc["requested"] += 1
        m = enc_by_item.get((t["participant"], t["item"]))
        if status[k] != "admitted":
            qc["query_not_admitted"] += 1
        elif m is None or status[m] != "admitted":
            qc["no_match"] += 1
        else:
            if out_rec[k] == len(by_trial[k]):
                qc["failed"] += 1
            else:
                qc["contributing"] += 1
            n = sum(
                1
                for e, s in status.items()
                if e[0] == k[0]
                and e[1].startswith("enc")
                and s == "admitted"
                and e != m
            )
            controls[n] += 1

    def pct_outside(k):
        rows = by_trial[k]
        tot = sum(float(r["duration_ms"]) for r in rows)
        out = sum(
            float(r["duration_ms"])
            for r in rows
            if not inside(WINDOW, float(r["x"]), float(r["y"]))
        )
        return round(100 * out / tot)

    f17r, f17e = ("P17", "ret_07", "1"), ("P17", "enc_03", "1")
    rec = records[7214 - 1]
    p05 = [("P05", t, "1") for t in ("ret_04", "ret_11", "ret_16")]

    M = mock()
    exp_q = {(p, t, "1"): c for (p, t), c in M["quarantine"].items()}
    exp_abs = {(p, t, "1") for (p, t) in M["absent"]}
    resp = {
        (P["id"], q["trial"], "1"): q["response"]
        for P in M["study"]["participants"]
        for q in P["queries"]
    }
    enc_of = M["enc_of_item"]
    got_resp = {k: t["response"] for k, t in inv.items() if t["phase"] == "Retrieval"}

    checks = [
        ("inventory trials", len(trials), 960),
        ("admitted", cause("admitted"), 937),
        ("quarantined", len(quarantined), 17),
        ("  duplicate-ordinals", cause("duplicate-ordinals"), 4),
        ("  no-fixations", cause("no-fixations"), 5),
        ("  overlap", cause("overlap"), 6),
        ("  rejected-records", cause("rejected-records"), 2),
        ("absent", cause("absent"), 6),
        (
            "quarantine/absent sets = mock",
            quarantined == exp_q
            and {k for k, v in status.items() if v == "absent"} == exp_abs,
            True,
        ),
        ("fixation records", len(records), 11520),
        ("records outside window (on screen)", sum(out_rec.values()), 543),
        ("trials with outside records", sum(1 for v in out_rec.values() if v), 409),
        (
            "outside records in quarantined trials",
            sum(out_rec[k] for k in quarantined),
            0,
        ),
        ("off-screen records", offscreen, 0),
        ("non-finite coordinates", nonfinite, 0),
        ("trials violating timing rules", len(bad_time), 0),
        ("distinct items", len(items), 259),
        ("images present", len(items & set(pngs)), 257),
        ("images missing", ", ".join(missing), "forest-044, kitchen-081"),
        ("extra images", len(set(pngs) - items), 0),
        ("all images 1024x768", sizes_ok, True),
        ("queries requested", qc["requested"], 480),
        ("  contributing", qc["contributing"], 454),
        ("  failed (all outside window)", qc["failed"], 3),
        ("  no match", qc["no_match"], 9),
        ("  query not admitted", qc["query_not_admitted"], 14),
        ("controls per eligible query", sorted(controls), [18, 19]),
        ("pair rows per scale (1 + controls)", sum((1 + n) * c for n, c in controls.items()), 8969),
        (
            "P05 ret_04/11/16 records, outside",
            [(len(by_trial[k]), out_rec[k]) for k in p05],
            [(11, 11)] * 3,
        ),
        (
            "P17 ret_07 fixations, outside, % dur",
            (len(by_trial[f17r]), out_rec[f17r], pct_outside(f17r)),
            (12, 1, 4),
        ),
        (
            "P17 enc_03 fixations, outside, % dur",
            (len(by_trial[f17e]), out_rec[f17e], pct_outside(f17e)),
            (13, 1, 3),
        ),
        (
            "P17 ret_07 item / matched enc",
            (inv[f17r]["item"], enc_by_item[("P17", "beach-042")][1]),
            ("beach-042", "enc_03"),
        ),
        (
            "record 7,214",
            (
                key(rec),
                rec["ordinal"],
                rec["x"],
                rec["y"],
                rec["onset_ms"],
                rec["duration_ms"],
            ),
            (f17e, "6", "1148.0", "456.0", "2160", "412"),
        ),
        (
            "record 7,214 image px",
            (float(rec["x"]) - 448, float(rec["y"]) - 156),
            (700.0, 300.0),
        ),
        ("retrieval responses = mock", got_resp == resp, True),
        (
            "encoding order = mock",
            all(
                enc_by_item[(t["participant"], t["item"])][1]
                == enc_of(int(t["trial"][4:]) - 1)
                for t in trials
                if t["phase"] == "Retrieval"
            ),
            True,
        ),
        (
            "display kinds",
            sorted(
                {(t["phase"], t["display_kind"], bool(t["image_file"])) for t in trials}
            ),
            [("Encoding", "image", True), ("Retrieval", "blank+fixation-cross", False)],
        ),
    ]
    width = max(len(c[0]) for c in checks)
    failures = 0
    for name, got, want in checks:
        ok = got == want
        failures += not ok
        print(
            f"{'ok  ' if ok else 'FAIL'} {name:<{width}}  {got}"
            + ("" if ok else f"   expected {want}")
        )

    # qualitative: retrieval gaze echoes encoding gaze more for Remembered items
    def echo(group):
        d = []
        for k, t in inv.items():
            if (
                t["phase"] != "Retrieval"
                or t["response"] != group
                or status[k] != "admitted"
            ):
                continue
            m = enc_by_item[(t["participant"], t["item"])]
            if status[m] != "admitted":
                continue
            ref = [(float(r["x"]), float(r["y"])) for r in by_trial[m]]
            for r in by_trial[k]:
                x, y = float(r["x"]), float(r["y"])
                d.append(min(math.hypot(x - a, y - b) for a, b in ref))
        return sorted(d)[len(d) // 2]

    rem, forg = echo("Remembered"), echo("Forgotten")
    print(
        f"info median distance to nearest encoding fixation: Remembered {rem:.0f} px, Forgotten {forg:.0f} px"
    )
    if not rem < forg:
        failures += 1
        print(
            "FAIL Remembered retrieval gaze should echo encoding more closely than Forgotten"
        )
    print(
        f"{len(checks) - failures if failures else len(checks)} of {len(checks)} checks passed"
        if not failures
        else f"{failures} check(s) FAILED"
    )
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())

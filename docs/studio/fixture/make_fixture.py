"""Deterministic mock study for the Eyes Studio design boards. Every number on every
board must come from fixture.json (or FIXTURE.md, which summarises it).

Science values (every M, B and D, the focus query's control scores, participant and
grand means) are read from the executable golden fixture's SCORES.json, which eyes4s
computed and which is authoritative for them (DESIGN_SPEC §15, owner decision
2026-10-11). This script computes none of them."""
import csv, json, random, statistics as st, sys
from decimal import ROUND_HALF_UP, Decimal
from pathlib import Path

rng = random.Random(20260925)
CATS = [
    "beach",
    "forest",
    "kitchen",
    "street",
    "garden",
    "office",
    "dog",
    "bridge",
    "market",
    "tower",
    "harbor",
    "library",
    "desert",
    "church",
    "station",
    "field",
    "cafe",
    "museum",
    "canyon",
    "bakery",
    "stadium",
    "lake",
    "alley",
    "farm",
]
ITEMS = [f"{CATS[i % 24]}-{(i * 37) % 900 + 7:03d}" for i in range(240)]
# Pin the focus story
P17_ITEMS = [
    "street-112",
    "dog-077",
    "office-203",
    "kitchen-017",
    "forest-008",
    "garden-054",
    "beach-042",
    "bridge-019",
    "market-066",
    "tower-150",
    "harbor-231",
    "library-388",
    "desert-305",
    "church-417",
    "station-529",
    "field-613",
    "cafe-702",
    "museum-814",
    "canyon-126",
    "bakery-240",
]
SCALES = ["0.5°", "1°", "2°", "4°"]
parts = [f"P{i:02d}" for i in range(1, 25)]

# Non-admitted trials (inventory 960 = 937 admitted + 17 quarantined + 6 absent)
quarantine = {  # (participant, trial): cause
    ("P03", "enc_11"): "overlap",
    ("P08", "enc_04"): "no-fixations",
    ("P11", "enc_16"): "duplicate-ordinals",
    ("P14", "enc_02"): "rejected-records",
    ("P21", "enc_09"): "overlap",
    ("P02", "ret_05"): "no-fixations",
    ("P04", "ret_12"): "overlap",
    ("P06", "ret_03"): "no-fixations",
    ("P09", "ret_18"): "duplicate-ordinals",
    ("P12", "ret_07"): "overlap",
    ("P13", "ret_15"): "no-fixations",
    ("P16", "ret_01"): "duplicate-ordinals",
    ("P19", "ret_10"): "overlap",
    ("P20", "ret_06"): "no-fixations",
    ("P22", "ret_13"): "rejected-records",
    ("P23", "ret_19"): "overlap",
    ("P24", "ret_08"): "duplicate-ordinals",
}
absent = {
    ("P07", "enc_13"),
    ("P10", "enc_18"),
    ("P15", "enc_05"),
    ("P18", "enc_12"),
    ("P01", "ret_17"),
    ("P17", "ret_09"),
}
assert len(quarantine) == 17 and len(absent) == 6
# P05: 3 retrieval trials whose fixations all fall outside the analysis window -> empty map -> failed
failed_q = {("P05", "ret_04"), ("P05", "ret_11"), ("P05", "ret_16")}


def enc_of_item(k):  # encoding order differs from retrieval order
    return f"enc_{((k * 7) % 20) + 1:02d}"


# The executable golden fixture. The path is resolved from this file, never from the
# working directory (tools/studio-fixture runs this script in a scratch directory).
GOLDEN = Path(__file__).resolve().parents[3] / "fixtures" / "studio-golden"
if not (GOLDEN / "SCORES.json").is_file():
    sys.exit(f"make_fixture.py: {GOLDEN / 'SCORES.json'} is missing; it is the science source")
golden_scores = json.load(open(GOLDEN / "SCORES.json"))
GOLDEN_SCALES = ["0.5", "1", "2", "4"]  # SCORES.json scale keys, in SCALES order
FOCUS_SCALE = "2"  # σ 2°, the scale of every single-scale summary below
golden_query = {(q["participant"], q["trial"]): q for q in golden_scores["queries"]}
golden_summary = golden_scores["summaries"]


study = {"participants": [], "summary": {}}
pair_rows = 0
qc = {
    "requested": 0,
    "query_not_admitted": 0,
    "no_match": 0,
    "failed": 0,
    "contributing": 0,
}
for pi, p in enumerate(parts):
    items = P17_ITEMS if p == "P17" else [ITEMS[(pi * 10 + j) % 240] for j in range(20)]
    enc_admitted = {enc_of_item(k) for k in range(20)} - {
        t
        for (pp, t) in list(quarantine) + list(absent)
        if pp == p and t.startswith("enc")
    }
    # The seed's draws are kept, in order, so the design they decide (each retrieval
    # response) stays the one the golden trials table was generated from. The draws
    # that once made illustrative scores are discarded.
    rng.gauss(0.18, 0.09)
    queries = []
    for k, item in enumerate(items):
        rt = f"ret_{k+1:02d}"
        et = enc_of_item(k)
        qc["requested"] += 1
        remembered = rng.random() < 0.7
        q = {
            "trial": rt,
            "item": item,
            "match": et,
            "response": "Remembered" if remembered else "Forgotten",
        }
        if (p, rt) in quarantine or (p, rt) in absent:
            q["status"] = "query not admitted"
            q["reason"] = quarantine.get((p, rt), "absent")
            qc["query_not_admitted"] += 1
        elif et not in enc_admitted:
            q["status"] = "no match"
            q["reason"] = "matched Encoding trial not admitted"
            qc["no_match"] += 1
        else:
            nctrl = len(enc_admitted) - 1
            q["controls"] = nctrl
            pair_rows += 1 + nctrl
            if (p, rt) in failed_q:
                q["status"] = "failed"
                q["reason"] = "empty map: 0 of 11 fixations inside analysis window"
                qc["failed"] += 1
            else:
                q["status"] = "ok"
                qc["contributing"] += 1
                for _ in range(5):
                    rng.gauss(0, 1)
                g = golden_query[(p, rt)]
                assert g["status"] == "contributing", (p, rt, g["status"])
                for k in ("M", "B", "D"):
                    q[k] = [g["scales"][s][k] for s in GOLDEN_SCALES]
        queries.append(q)
    study["participants"].append({"id": p, "queries": queries})

# Focus query P17 ret_07 beach-042: its 19 control scores at σ 2°, from SCORES.json
# focusQuery, highest first (ties in eyes4s's pair order).
foc = next(q for q in study["participants"][16]["queries"] if q["item"] == "beach-042")
foc["response"] = "Remembered"  # the story's pin, which the golden trials table carries
assert (foc["trial"], foc["status"], foc["controls"]) == ("ret_07", "ok", 19)
# The design agrees with the golden fixture's query by query.
for P in study["participants"]:
    for q in P["queries"]:
        g = golden_query.get((P["id"], q["trial"]))
        if g is None:
            assert q["status"] == "query not admitted", (P["id"], q["trial"])
            continue
        assert (g["item"], g["response"], g["controls"]) == (q["item"], q["response"], q.get("controls")), (P["id"], q["trial"])
focus_golden = golden_scores["focusQuery"]
assert (focus_golden["participant"], focus_golden["trial"]) == ("P17", "ret_07")
foc["control_scores_2deg"] = sorted(
    (
        {"item": c["item"], "trial": c["reference"], "cos": c["score"]}
        for c in focus_golden["controlPairs"][FOCUS_SCALE]
    ),
    key=lambda c: -c["cos"],
)
assert len(foc["control_scores_2deg"]) == 19
assert all(c["item"] == P17_ITEMS[[enc_of_item(k) for k in range(20)].index(c["trial"])]
           for c in foc["control_scores_2deg"])
# B is the mean of the control scores, each rounded to 6 places as SCORES.json stores them.
assert abs(st.mean(c["cos"] for c in foc["control_scores_2deg"]) - foc["B"][2]) <= 1e-6

# Participant summaries (reporting spec: participant means, equal weight), as eyes4s
# reported them in SCORES.json summaries.
rows = []
for P in study["participants"]:
    ok = [q for q in P["queries"] if q["status"] == "ok"]
    gp = {s: golden_summary[s]["participants"][P["id"]] for s in GOLDEN_SCALES}
    assert gp[FOCUS_SCALE]["queries"] == len(ok), P["id"]
    r = {
        "id": P["id"],
        "requested": len(P["queries"]),
        "contributing": len(ok),
        "failed": sum(q["status"] == "failed" for q in P["queries"]),
        "no_match": sum(q["status"] == "no match" for q in P["queries"]),
        "not_admitted": sum(q["status"] == "query not admitted" for q in P["queries"]),
    }
    for g in ("Remembered", "Forgotten"):
        gg = gp[FOCUS_SCALE]["byResponse"][g]
        assert gg["queries"] == sum(q["response"] == g for q in ok), (P["id"], g)
        r[g] = (
            None
            if not gg["queries"]
            else {"n": gg["queries"], "M": gg["M"], "B": gg["B"], "D": gg["D"]}
        )
    r["all"] = {
        "M": gp[FOCUS_SCALE]["M"],
        "B": gp[FOCUS_SCALE]["B"],
        "D": gp[FOCUS_SCALE]["D"],
        "D_by_scale": [gp[s]["D"] for s in GOLDEN_SCALES],
    }
    rows.append(r)
study["summary"]["participants"] = rows
grand = lambda s: golden_summary[s]["grand"]["D"]["estimate"]
by_response = lambda s, g: golden_summary[s]["byResponse"][g]
paired = [r for r in rows if r["Remembered"] and r["Forgotten"]]
assert golden_summary[FOCUS_SCALE]["rememberedMinusForgotten"]["paired"] == len(paired)
study["summary"].update(
    {
        "inventory_trials": 960,
        "admitted": 937,
        "quarantined": 17,
        "absent": 6,
        "quarantine_by_cause": {
            c: sum(v == c for v in quarantine.values())
            for c in sorted(set(quarantine.values()))
        },
        "fixation_records": 11520,
        "items_in_pool": 259,
        "images_found": 257,
        "images_missing": 2,
        "query_contrasts": qc,
        "pair_rows_per_scale": pair_rows,
        "pair_rows_all_scales": pair_rows * 4,
        "scales": SCALES,
        "grand_D_all": grand(FOCUS_SCALE),
        "grand_D_by_scale": [grand(s) for s in GOLDEN_SCALES],
        "grand_D_Remembered": by_response(FOCUS_SCALE, "Remembered")["estimate"],
        "grand_D_Forgotten": by_response(FOCUS_SCALE, "Forgotten")["estimate"],
        "n_Remembered": by_response(FOCUS_SCALE, "Remembered")["participants"],
        "n_Forgotten": by_response(FOCUS_SCALE, "Forgotten")["participants"],
        "n_paired": len(paired),
    }
)
json.dump(study, open("fixture.json", "w"), indent=1)

S = study["summary"]


def fmt(value, signed=False):
    """2 places, half-up on the shortest decimal of the double: the app's
    Format.decimal / Format.signed, so a cell reads as the app's (FIXTURE.md keeps an
    ASCII '-' where the app draws U+2212)."""
    text = str(Decimal(repr(value)).quantize(Decimal("0.01"), rounding=ROUND_HALF_UP))
    return "+" + text if signed and not text.startswith("-") and text != "0.00" else text


fmts = lambda vs: "[" + ", ".join(fmt(v) for v in vs) + "]"
ranked = foc["control_scores_2deg"]
lines = [
    "# Mock study fixture (generated by make_fixture.py — do not hand-edit numbers)",
    "",
    "Design: 24 participants (P01–P24). Each encodes 20 items (enc_01–enc_20) then retrieves them",
    "(ret_01–ret_20, retrieval shown as grey screen + central fixation cross). Items: 259 distinct",
    "(pool of 240 plus P17's 19 pinned items); each participant sees 20. Trial key = participant + phase + trial (+ occurrence, always 1 here).",
    f"Inventory (trials.csv): {S['inventory_trials']} trials. Admitted {S['admitted']}, quarantined {S['quarantined']} "
    f"({', '.join(f'{k} {v}' for k,v in S['quarantine_by_cause'].items())}), absent {S['absent']} (in trials.csv, no fixation records).",
    f"fixations.csv: {S['fixation_records']} records. Distinct items (stimulus images): {S['items_in_pool']}; images found {S['images_found']}, missing {S['images_missing']}.",
    f"Scales σ: {', '.join(S['scales'])} (declared 35 px/°, not calibrated). Grid 64×48 cells on image frame 1024×768 → cell 16 px = 0.46°.",
    "Controls: same participant, Encoding, other items, all admitted → 19 per query (18 where one encoding trial was not admitted).",
    f"Query contrasts (retrieval trials): {json.dumps(S['query_contrasts'])}.",
    f"Pair rows: {S['pair_rows_per_scale']} per scale, {S['pair_rows_all_scales']} across 4 scales.",
    "Science values: every M, B and D below is eyes4s's, read from fixtures/studio-golden/SCORES.json (6 places in fixture.json, shown here to 2 places as the app rounds them).",
    f"Grand mean D (participant means, equal weight, σ 2°): {fmt(S['grand_D_all'])}. By scale: {fmts(S['grand_D_by_scale'])}.",
    f"Reporting by retrieval response: Remembered mean D {fmt(S['grand_D_Remembered'])} (n={S['n_Remembered']} participants), "
    f"Forgotten {fmt(S['grand_D_Forgotten'])} (n={S['n_Forgotten']}); paired n={S['n_paired']}.",
    "",
    "Focus query: P17 · ret_07 · beach-042 (Remembered). Matched reference: P17 · enc_03 · beach-042 (encoding order differs from retrieval order). P17 ret_09 is absent (not admitted)..",
    f"  M/B/D by scale: M {fmts(foc['M'])} B {fmts(foc['B'])} D {fmts(foc['D'])}. 19 controls at 2°, highest {ranked[0]['item']} ({ranked[0]['trial']}) {fmt(ranked[0]['cos'])}, mean {fmt(foc['B'][2])}.",
    "",
    "## Participant table (σ 2°)",
    "",
    "| P | req | contrib | failed | no match | not admitted | mean M | mean B | mean D | Rem D (n) | Forg D (n) |",
    "|---|---|---|---|---|---|---|---|---|---|---|",
]
for r in rows:
    f = lambda g: "—" if not r[g] else f"{fmt(r[g]['D'], True)} ({r[g]['n']})"
    lines.append(
        f"| {r['id']} | {r['requested']} | {r['contributing']} | {r['failed']} | {r['no_match']} | {r['not_admitted']} | {fmt(r['all']['M'])} | {fmt(r['all']['B'])} | {fmt(r['all']['D'], True)} | {f('Remembered')} | {f('Forgotten')} |"
    )
open("FIXTURE.md", "w").write("\n".join(lines) + "\n")
print("\n".join(lines[:16]))
print("focus match trial:", foc["match"])

# ---- Round 3 additions: every remaining board number is generated here ----
# Record-level facts (records per trial, which records lie outside the analysis
# window, durations, the focus fixations) come from the executable golden
# fixture, which is authoritative for them (owner decision D2, 2026-10-11).
SCREEN_BOX = (0, 0, 1920, 1080)  # screen frame, px, y down, half-open
WINDOW_BOX = (448, 156, 448 + 1024, 156 + 768)  # image frame on screen = analysis window


def in_box(box, x, y):
    return box[0] <= x < box[2] and box[1] <= y < box[3]


if not (GOLDEN / "fixations.csv").is_file():
    sys.exit(f"make_fixture.py: {GOLDEN / 'fixations.csv'} is missing; it is the record-level source")
golden_rows = list(csv.DictReader(open(GOLDEN / "fixations.csv", newline="")))
records = {}  # (participant, trial) -> [(ordinal, x, y, onset_ms, duration_ms, sample_count)]
for r in golden_rows:
    records.setdefault((r["participant"], r["trial"]), []).append(
        (int(r["ordinal"]), float(r["x"]), float(r["y"]), int(r["onset_ms"]),
         int(r["duration_ms"]), int(r["sample_count"]))
    )
assert set(records) == {
    (P["id"], f"{ph}_{k:02d}") for P in study["participants"] for ph in ("enc", "ret")
    for k in range(1, 21)
} - absent, "golden trials with records != inventory minus absent"
counts = {key: len(v) for key, v in records.items()}
outside = {key: sum(not in_box(WINDOW_BOX, x, y) for (_, x, y, *_r) in v) for key, v in records.items()}
assert sum(counts.values()) == 11520 == S["fixation_records"]
quarantined_records = sum(counts[key] for key in quarantine)
admitted_keys = [key for key in counts if key not in quarantine]
assert quarantined_records == 209, quarantined_records
assert sum(counts[key] for key in admitted_keys) == 11311
assert sum(outside[key] for key in quarantine) == 0, "outside-window records in quarantined trials"
assert sum(outside[key] for key in admitted_keys) == sum(outside.values()) == 543
assert sum(1 for key in admitted_keys if outside[key]) == 409
for key in failed_q:
    assert counts[key] == outside[key] == 11, key
dur = lambda key, keep: sum(d for (_, x, y, _o, d, _n) in records[key] if keep(x, y))
outside_ms = sum(dur(k, lambda x, y: not in_box(WINDOW_BOX, x, y)) for k in admitted_keys)
admitted_ms = sum(dur(k, lambda x, y: True) for k in admitted_keys)
assert (outside_ms, admitted_ms) == (159142, 3282108), (outside_ms, admitted_ms)
outside_screen = sum(not in_box(SCREEN_BOX, r[1], r[2]) for v in records.values() for r in v)
assert outside_screen == golden_scores["counts"]["recordsOutsideScreen"] == 0
S["outside_window_records"] = sum(outside.values())
S["outside_window_trials"] = sum(1 for v in outside.values() if v > 0)
S["records_in_quarantined_trials"] = quarantined_records
S["records_in_admitted_trials"] = sum(counts[key] for key in admitted_keys)
S["outside_window_records_in_quarantined_trials"] = sum(outside[key] for key in quarantine)
S["outside_window_duration_ms"] = outside_ms
S["admitted_fixation_duration_ms"] = admitted_ms
S["outside_window_duration_share"] = round(outside_ms / admitted_ms, 6)
S["outside_screen_records"] = golden_scores["counts"]["recordsOutsideScreen"]


def focus_outside_text(key):
    share = dur(key, lambda x, y: not in_box(WINDOW_BOX, x, y)) / dur(key, lambda x, y: True)
    return f"{outside[key]} of {counts[key]} fixations · {round(100 * share)}% of duration"


S["focus_outside"] = {"ret_07": "1 of 12 fixations · 4% of duration", "enc_03": "1 of 13 fixations · 3% of duration"}
assert S["focus_outside"] == {t: focus_outside_text(("P17", t)) for t in ("ret_07", "enc_03")}
S["focus_fixation"] = {"trial": "P17 enc_03", "fixation": 6, "record": 7214,
    "image_px": [700, 300], "screen_px": [1148, 456],
    "deg_from_centre_y_up": [round((700 - 512) / 35, 1), round((384 - 300) / 35, 1)],
    "onset_ms": 2160, "duration_ms": 412}
_ff = S["focus_fixation"]
assert golden_rows[_ff["record"] - 1]["participant"] == "P17"
assert records[("P17", "enc_03")][_ff["fixation"] - 1] == (
    _ff["fixation"], *map(float, _ff["screen_px"]), _ff["onset_ms"], _ff["duration_ms"], _ff["duration_ms"] // 2
)
assert [a - b for a, b in zip(_ff["screen_px"], WINDOW_BOX)] == _ff["image_px"]
# Missing images: two pool items; count the encoding trials that display them
seen = {}
for P in study["participants"]:
    for q in P["queries"]:
        seen.setdefault(q["item"], []).append(P["id"])
missing = ["forest-044", "kitchen-081"]
for m in missing:
    assert m in seen, m
S["missing_images"] = {m: {"participants": seen[m], "encoding_trials": len(seen[m])} for m in missing}
S["dataset_history"] = {"r2": "onset units undeclared; Block not mapped to occurrence",
    "r3": "onset declared ms; Block → occurrence; 4 trials change status vs r2 (overlap → admitted 3, admitted → no-fixations 1)"}
S["runs"] = {"run 5": "analysis rev 3 · data r2 · stale", "run 6": "rev 4 · data r3 · cancelled at Comparing",
    "run 7": "rev 4 · data r3 · current", "run 8": "rev 5 · data r3 · running (moment t3)"}
S["pair_rows_rev5"] = S["pair_rows_per_scale"] * 5
S["eligible_queries"] = qc["contributing"] + qc["failed"]
# eyes4s counts candidates over admitted trials (S0.7b, owner/lead decision
# 2026-10-04): admitted retrieval x admitted encoding.
not_admitted = list(quarantine) + list(absent)
S["candidate_focal_admitted"] = 480 - sum(t.startswith("ret") for (_, t) in not_admitted)
S["candidate_reference_admitted"] = 480 - sum(t.startswith("enc") for (_, t) in not_admitted)
S["candidate_pairs_cartesian_per_scale"] = (
    S["candidate_focal_admitted"] * S["candidate_reference_admitted"]
)
S["paired_group_n_range"] = [min(min(r["Remembered"]["n"], r["Forgotten"]["n"]) for r in rows),
                            max(max(r["Remembered"]["n"], r["Forgotten"]["n"]) for r in rows)]
S["P17_queries_as_control_for_enc_03"] = 18
S["grand_D_by_scale_Remembered"] = [by_response(s, "Remembered")["estimate"] for s in GOLDEN_SCALES]
S["grand_D_by_scale_Forgotten"] = [by_response(s, "Forgotten")["estimate"] for s in GOLDEN_SCALES]

# Paired descriptive (Results "Δ Remembered − Forgotten"), σ 2°, status ok. The mean is
# eyes4s's contrast (SCORES.json rememberedMinusForgotten). SCORES.json holds no
# per-participant difference, so each one is described here as the app computes it: mean
# Remembered D − mean Forgotten D over the participant's SCORES.json query D values,
# before rounding participant means. It agrees with the difference of SCORES.json's group
# means to their 6-place rounding (half a unit from each of the four 6-place values). These
# descriptive values are written to 9 places: a difference of means of 6-place values can
# fall exactly on a 6-place rounding boundary (P01: 0.1185755).
contrast = golden_summary[FOCUS_SCALE]["rememberedMinusForgotten"]
diffs = {}
for P, r in zip(study["participants"], rows):
    ok = [q for q in P["queries"] if q["status"] == "ok"]
    g = {k: [q["D"][2] for q in ok if q["response"] == k] for k in ("Remembered", "Forgotten")}
    if g["Remembered"] and g["Forgotten"]:
        diffs[P["id"]] = st.mean(g["Remembered"]) - st.mean(g["Forgotten"])
        assert abs(diffs[P["id"]] - (r["Remembered"]["D"] - r["Forgotten"]["D"])) <= 2e-6, P["id"]
assert round(st.mean(diffs.values()), 6) == contrast["estimate"]
lo, hi = min(diffs, key=diffs.get), max(diffs, key=diffs.get)
S["paired_D_diff"] = {
    "scale": "2°",
    "status": "ok",
    "rounding": "computed before rounding participant means",
    "mean": round(st.mean(diffs.values()), 9),
    "min": {"participant": lo, "value": round(diffs[lo], 9)},
    "max": {"participant": hi, "value": round(diffs[hi], 9)},
    "positive_k": sum(v > 0 for v in diffs.values()),
    "paired_n": len(diffs),
    "missing_pairs": len(parts) - len(diffs),
    "by_participant": {p: round(v, 9) for p, v in sorted(diffs.items())},
}
assert S["paired_D_diff"]["paired_n"] == S["n_paired"] == contrast["paired"]
assert contrast["unpaired"] == S["paired_D_diff"]["missing_pairs"]

# Control scores exist only at σ 2° (control_scores_2deg). Other scales are not invented: a board that
# draws them tags them "illustrative".
ctrl_trials = [c["trial"] for c in foc["control_scores_2deg"]]

# Focus fixation lists, copied from the golden fixations.csv (screen px, y down): the query
# P17 ret_07, its matched reference P17 enc_03 and its 19 control encoding trials.
assert sorted(ctrl_trials) == sorted(
    t for (p, t) in counts if p == "P17" and t.startswith("enc") and t != "enc_03"
)
study["focus_fixations"] = {
    "source": "fixtures/studio-golden/fixations.csv",
    "frame": "screen px, x right, y down; image px = screen px − (448, 156)",
    "columns": ["ordinal", "x", "y", "onset_ms", "duration_ms", "sample_count", "in_window"],
    "trials": {
        f"P17 {t}": [list(r) + [in_box(WINDOW_BOX, r[1], r[2])] for r in records[("P17", t)]]
        for t in ["ret_07", "enc_03"] + ctrl_trials
    },
}
json.dump(study, open("fixture.json", "w"), indent=1)
with open("FIXTURE.md", "a") as f:
    f.write(f"- Grand D by scale (0.5/1/2/4°) by group: Remembered {fmts(S['grand_D_by_scale_Remembered'])}, Forgotten {fmts(S['grand_D_by_scale_Forgotten'])}.\n")
    f.write("\n## Round 3 additions (generated)\n\n")
    f.write(f"- Fixation records outside the image frame (analysis window): {S['outside_window_records']} of 11,520, in {S['outside_window_trials']} trials. Excluded from maps, reported.\n")
    f.write(f"- Focus query outside window: ret_07 {S['focus_outside']['ret_07']}; enc_03 {S['focus_outside']['enc_03']}. P05's 3 failed queries: 11 of 11 fixations outside.\n")
    ff = S['focus_fixation']
    f.write(f"- Focus fixation: {ff['trial']} fixation {ff['fixation']} = fixations.csv record {ff['record']:,}; image px {ff['image_px']}, screen px {ff['screen_px']}, degrees from image centre (x right, y up) {ff['deg_from_centre_y_up']}; onset {ff['onset_ms']} ms, duration {ff['duration_ms']} ms. Degree convention everywhere: from image centre, x right, y up, declared 35 px/°.\n")
    f.write(f"- Missing images: " + "; ".join(f"{m}.png (encoding trials of {', '.join(v['participants'])}: {v['encoding_trials']})" for m, v in S['missing_images'].items()) + ".\n")
    f.write(f"- Dataset history: r2 = {S['dataset_history']['r2']}; r3 = {S['dataset_history']['r3']}.\n")
    f.write(f"- Runs: " + "; ".join(f"{k}: {v}" for k, v in S['runs'].items()) + ".\n")
    f.write(f"- Rev 5 (5 scales incl. 8°) pair rows: {S['pair_rows_rev5']:,}. Rev 4: {S['pair_rows_all_scales']:,}.\n")
    f.write(f"- Eligible (computed) queries: {S['eligible_queries']} (454 contributing + 3 failed). Cartesian candidate pairs before paging, per scale: {S['candidate_pairs_cartesian_per_scale']:,} ({S['candidate_focal_admitted']} admitted retrieval × {S['candidate_reference_admitted']} admitted encoding; eyes4s candidatePairCount).\n")
    f.write(f"- Per-group n range across participants (Remembered/Forgotten): {S['paired_group_n_range']}.\n")
    f.write("- enc_03 (beach-042) is used by ret_07 as the matched reference and by the 18 other admitted P17 queries as a control.\n")
    f.write("- Matched cardinality: every eligible query has exactly 1 matched reference (0 duplicates, checked). The 9 no-match queries are reported under the persisted policy 'Queries without a matched reference: report as no match'.\n")
    f.write(f"- Records by admission (golden fixations.csv): {S['records_in_admitted_trials']:,} in admitted trials, {S['records_in_quarantined_trials']} in the 17 quarantined trials; outside-window records in quarantined trials: {S['outside_window_records_in_quarantined_trials']}.\n")
    f.write(f"- Methods: of {S['records_in_admitted_trials']:,} fixation records in admitted trials, {S['outside_window_records']} lay outside the analysis window ({S['outside_window_duration_ms']:,} / {S['admitted_fixation_duration_ms']:,} ms = {100 * S['outside_window_duration_share']:.2f}% of those trials' fixation duration). Records outside the screen: {S['outside_screen_records']} (SCORES.json counts.recordsOutsideScreen).\n")
    pd = S["paired_D_diff"]
    f.write(f"- Paired Δ Remembered − Forgotten (σ 2°, status ok, {pd['rounding']}): mean {pd['mean']:.6f} (SCORES.json rememberedMinusForgotten {contrast['estimate']}); min {pd['min']['participant']} {pd['min']['value']:.6f}, max {pd['max']['participant']} {pd['max']['value']:.6f}; higher in {pd['positive_k']} of {pd['paired_n']} paired participants; missing pairs {pd['missing_pairs']}.\n")
    f.write(f"- Focus fixation lists (fixture.json focus_fixations, from the golden fixations.csv): {len(study['focus_fixations']['trials'])} trials, P17 ret_07, enc_03 and its 19 controls.\n")
    f.write("\n## Story moments (boards are snapshots in one timeline)\n")
    f.write("- t1 Data · verify: dataset r3 draft (re-import of r2). Run 5 (rev 3, data r2) exists and becomes stale when r3 is admitted. No run on r3 yet.\n")
    f.write("- t2 Explore / Analysis / Compare·query / Figures: data r3 admitted; rev 4 · run 7 current; draft rev 5 (adds σ 8°) is ready and not run. No jobs.\n")
    f.write("- t3 Compare·summary: user pressed Save & run rev 5; run 8 running (Comparing, x / 44,845 pairs); view stays on run 7 until Show.\n")
print(open("FIXTURE.md").read().split("## Round 3")[1])

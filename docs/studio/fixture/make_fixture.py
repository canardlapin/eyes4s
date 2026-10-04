"""Deterministic mock study for the Eyes Studio design boards. Every number on every
board must come from fixture.json (or FIXTURE.md, which summarises it)."""
import json, random, statistics as st

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
    skill = rng.gauss(0.18, 0.09)
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
                base = skill + (0.12 if remembered else -0.06) + rng.gauss(0, 0.1)
                prof = [0.5, 0.78, 1.0, 0.62]
                q["D"] = [round(base * f, 2) for f in prof]
                B = [round(v, 2) for v in (0.22, 0.29, 0.35, 0.63)]
                q["B"] = [round(b + rng.gauss(0, 0.03), 2) for b in B]
                q["M"] = [round(q["B"][i] + q["D"][i], 2) for i in range(4)]
        queries.append(q)
    study["participants"].append({"id": p, "queries": queries})

# Pin focus query P17 ret_07 beach-042
foc = next(q for q in study["participants"][16]["queries"] if q["item"] == "beach-042")
foc.update(
    {
        "response": "Remembered",
        "status": "ok",
        "M": [0.41, 0.58, 0.73, 0.86],
        "B": [0.22, 0.29, 0.35, 0.63],
        "D": [0.19, 0.29, 0.38, 0.23],
    }
)
ctrl2 = [0.61,0.52,0.47,0.44,0.41,0.39,0.37,0.36,0.35,0.33,0.31,0.30,0.29,0.27,0.27,0.26,0.26,0.24,0.20]
assert len(ctrl2) == 19
foc["controls"] = 19
foc["control_scores_2deg"] = [
    {"item": it, "trial": enc_of_item(P17_ITEMS.index(it)), "cos": c}
    for it, c in zip(
        [i for i in P17_ITEMS if i != "beach-042"], sorted(ctrl2, reverse=True)
    )
]
b = round(st.mean(ctrl2), 2)
assert b == 0.35, b

# Participant summaries (reporting spec: participant means, equal weight)
rows = []
for P in study["participants"]:
    ok = [q for q in P["queries"] if q["status"] == "ok"]
    r = {
        "id": P["id"],
        "requested": len(P["queries"]),
        "contributing": len(ok),
        "failed": sum(q["status"] == "failed" for q in P["queries"]),
        "no_match": sum(q["status"] == "no match" for q in P["queries"]),
        "not_admitted": sum(q["status"] == "query not admitted" for q in P["queries"]),
    }
    for g in ("Remembered", "Forgotten"):
        gq = [q for q in ok if q["response"] == g]
        r[g] = (
            None
            if not gq
            else {
                "n": len(gq),
                "M": round(st.mean(q["M"][2] for q in gq), 2),
                "B": round(st.mean(q["B"][2] for q in gq), 2),
                "D": round(st.mean(q["D"][2] for q in gq), 2),
            }
        )
    r["all"] = {
        "M": round(st.mean(q["M"][2] for q in ok), 2),
        "B": round(st.mean(q["B"][2] for q in ok), 2),
        "D": round(st.mean(q["D"][2] for q in ok), 2),
        "D_by_scale": [round(st.mean(q["D"][i] for q in ok), 2) for i in range(4)],
    }
    rows.append(r)
# P05: force Forgotten group empty to demonstrate "no value, not zero"
p05 = next(r for r in rows if r["id"] == "P05")
study["summary"]["participants"] = rows
gm = lambda g: round(st.mean(r[g]["D"] for r in rows if r[g]), 2)
paired = [r for r in rows if r["Remembered"] and r["Forgotten"]]
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
        "grand_D_all": round(st.mean(r["all"]["D"] for r in rows), 2),
        "grand_D_by_scale": [
            round(st.mean(r["all"]["D_by_scale"][i] for r in rows), 2) for i in range(4)
        ],
        "grand_D_Remembered": gm("Remembered"),
        "grand_D_Forgotten": gm("Forgotten"),
        "n_Remembered": sum(1 for r in rows if r["Remembered"]),
        "n_Forgotten": sum(1 for r in rows if r["Forgotten"]),
        "n_paired": len(paired),
    }
)
json.dump(study, open("fixture.json", "w"), indent=1)

S = study["summary"]
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
    f"Grand mean D (participant means, equal weight, σ 2°): {S['grand_D_all']}. By scale: {S['grand_D_by_scale']}.",
    f"Reporting by retrieval response: Remembered mean D {S['grand_D_Remembered']} (n={S['n_Remembered']} participants), "
    f"Forgotten {S['grand_D_Forgotten']} (n={S['n_Forgotten']}); paired n={S['n_paired']}.",
    "",
    "Focus query: P17 · ret_07 · beach-042 (Remembered). Matched reference: P17 · enc_03 · beach-042 (encoding order differs from retrieval order). P17 ret_09 is absent (not admitted)..",
    f"  M/B/D by scale: M {foc['M']} B {foc['B']} D {foc['D']}. 19 controls at 2°, highest street-112 (enc_01) 0.61, mean 0.35.",
    "",
    "## Participant table (σ 2°)",
    "",
    "| P | req | contrib | failed | no match | not admitted | mean M | mean B | mean D | Rem D (n) | Forg D (n) |",
    "|---|---|---|---|---|---|---|---|---|---|---|",
]
for r in rows:
    f = lambda g: "—" if not r[g] else f"{r[g]['D']:+.2f} ({r[g]['n']})"
    lines.append(
        f"| {r['id']} | {r['requested']} | {r['contributing']} | {r['failed']} | {r['no_match']} | {r['not_admitted']} | {r['all']['M']:.2f} | {r['all']['B']:.2f} | {r['all']['D']:+.2f} | {f('Remembered')} | {f('Forgotten')} |"
    )
open("FIXTURE.md", "w").write("\n".join(lines) + "\n")
print("\n".join(lines[:16]))
print("focus match trial:", foc["match"])

# ---- Round 3 additions: every remaining board number is generated here ----
rng3 = random.Random(3)
inv = []  # (participant, trial) for every inventory trial that has fixation records
for P in study["participants"]:
    for ph in ("enc", "ret"):
        for k in range(1, 21):
            t = f"{ph}_{k:02d}"
            if (P["id"], t) not in absent:
                inv.append((P["id"], t))
# 954 trials with records share 11,520 fixation records
counts = {key: 12 for key in inv}
extra = 11520 - 12 * len(inv)
keys = sorted(inv)
for i in range(abs(extra)):
    counts[keys[(i * 37) % len(keys)]] += 1 if extra > 0 else -1
assert sum(counts.values()) == 11520
counts[("P17", "ret_07")] = 12
counts[("P17", "enc_03")] = 13
outside = {}
for key in keys:
    outside[key] = sum(rng3.random() < 0.045 for _ in range(counts[key]))
for key in failed_q:
    counts[key] = 11
    outside[key] = 11
outside[("P17", "ret_07")] = 1
outside[("P17", "enc_03")] = 1
# rebalance the totals after the pinned values
diff = 11520 - sum(counts.values())
counts[keys[0]] += diff
assert sum(counts.values()) == 11520
S["outside_window_records"] = sum(outside.values())
S["outside_window_trials"] = sum(1 for v in outside.values() if v > 0)
S["focus_outside"] = {"ret_07": "1 of 12 fixations · 4% of duration", "enc_03": "1 of 13 fixations · 3% of duration"}
S["focus_fixation"] = {"trial": "P17 enc_03", "fixation": 6, "record": 7214,
    "image_px": [700, 300], "screen_px": [1148, 456],
    "deg_from_centre_y_up": [round((700 - 512) / 35, 1), round((384 - 300) / 35, 1)],
    "onset_ms": 2160, "duration_ms": 412}
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
def gscale(g):
    out = []
    for i in range(4):
        vals = []
        for P in study["participants"]:
            gq = [q for q in P["queries"] if q["status"] == "ok" and q["response"] == g]
            if gq:
                vals.append(st.mean(q["D"][i] for q in gq))
        out.append(round(st.mean(vals), 2))
    return out
S["grand_D_by_scale_Remembered"] = gscale("Remembered")
S["grand_D_by_scale_Forgotten"] = gscale("Forgotten")
json.dump(study, open("fixture.json", "w"), indent=1)
with open("FIXTURE.md", "a") as f:
    f.write(f"- Grand D by scale (0.5/1/2/4°) by group: Remembered {S['grand_D_by_scale_Remembered']}, Forgotten {S['grand_D_by_scale_Forgotten']}.\n")
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
    f.write("\n## Story moments (boards are snapshots in one timeline)\n")
    f.write("- t1 Data · verify: dataset r3 draft (re-import of r2). Run 5 (rev 3, data r2) exists and becomes stale when r3 is admitted. No run on r3 yet.\n")
    f.write("- t2 Explore / Analysis / Compare·query / Figures: data r3 admitted; rev 4 · run 7 current; draft rev 5 (adds σ 8°) is ready and not run. No jobs.\n")
    f.write("- t3 Compare·summary: user pressed Save & run rev 5; run 8 running (Comparing, x / 44,845 pairs); view stays on run 7 until Show.\n")
print(open("FIXTURE.md").read().split("## Round 3")[1])

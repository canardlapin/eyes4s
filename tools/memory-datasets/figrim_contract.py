"""Lossless FIGRIM admission ledger. Does not infer people, time or missing trials."""

from collections import Counter, defaultdict
from contextlib import ExitStack
import csv
from dataclasses import dataclass
import json
import math
from pathlib import Path

PHASES = ("enc", "rec", "rec2")
PHASE_FIELDS = ["image_index", "image", "category", "observer_slot", "recording_label",
                "initials", "source_trial", "phase", "slot_state", "gaze_state", "sdt",
                "response_state", "source_locations", "admitted_locations"]
LOCATION_FIELDS = ["image_index", "observer_slot", "phase", "ordinal", "raw_x", "raw_y",
                   "rounded_x", "rounded_y", "admitted", "x_px", "y_px"]
BIN_FIELDS = ["image_index", "observer_slot", "phase", "bin_x", "bin_y", "count"]


def configuration_error(dataset):
    """This adapter qualifies the shipped FIGRIM profile, not arbitrary metadata."""
    expected_geometry = {
        "width_px": 1000, "height_px": 1000, "x": "right", "y": "down",
        "input_origin": "MATLAB one-based image pixels",
        "conversion": "round as in author code, filter to 1..1000, subtract one",
    }
    analysis = dataset.get("analysis", {})
    if (analysis.get("adapter"), analysis.get("geometry"), analysis.get("grid"), analysis.get("weighting")) != (
            "figrim_locations", expected_geometry, [10, 10], "uniform locations"):
        return "FIGRIM adapter requires the declared 1000x1000 right/down one-based pixel conversion, 10x10 grid and uniform weights"
    shipped = json.loads(Path(__file__).with_name("manifest.json").read_text())
    duplicates = next(d for d in shipped["datasets"] if d["id"] == "figrim")["analysis"]["duplicate_image_pairs"]
    if analysis.get("duplicate_image_pairs") != duplicates:
        return "FIGRIM independent witness supports only the duplicate-image contract in the shipped manifest"
    return None


@dataclass
class Trial:
    image_index: int
    image: str
    observer_slot: int
    phase: str
    category: str
    sdt: object
    points: object
    ordinals: object


def families(names, duplicate_pairs):
    """Transitive duplicate families, preserving every original filename."""
    parent = {name: name for name in names}

    def root(name):
        while parent[name] != name:
            name = parent[name]
        return name

    for a, b in duplicate_pairs:
        if a not in parent or b not in parent:
            continue
        a, b = root(a), root(b)
        parent[max(a, b)] = min(a, b)
    return {name: root(name) for name in names}


def scalar(value, field, text=False, missing_nan=False):
    import numpy as np
    a = np.asarray(value)
    if a.size == 0:
        return ""
    if a.size != 1:
        raise ValueError(f"{field} must be scalar, found {a.shape}")
    v = a.item()
    if text:
        if not isinstance(v, str) or any(c in v for c in "\t\r\n"):
            raise ValueError(f"{field} must be a single-line string")
        return v
    if missing_nan and isinstance(v, (int, float)) and math.isnan(v):
        return v
    if isinstance(v, (str, bool)) or not math.isfinite(float(v)) or int(v) != v:
        raise ValueError(f"{field} must be a finite integer")
    return int(v)


def has_value(value):
    import numpy as np
    return bool(value) if isinstance(value, dict) else np.asarray(value).size > 0


def admit(images, directory, duplicate_pairs, width=1000, height=1000, cols=10, rows=10):
    """Return admitted trials and write all phase/coordinate dispositions.

    The raster path follows published MATLAB round/filter/subtract-one semantics.
    Raw double coordinates and source row ordinals remain available in the ledger.
    No empty slot is relabeled as a failed response or invalid fixation.
    """
    import numpy as np

    if any(type(n) is not int or n <= 0 for n in (width, height, cols, rows)):
        raise ValueError("Frame and grid dimensions must be positive integers")
    images = [images] if isinstance(images, dict) else list(images)
    names = [scalar(i["filename"], "filename", text=True) for i in images]
    if len(set(names)) != len(names) or not all(names):
        raise ValueError("Duplicate source image identifier or empty filename")
    canonical = families(names, duplicate_pairs)
    counts = Counter(images=len(images))
    states, responses = Counter(), Counter()
    identities = defaultdict(lambda: {"recording_labels": set(), "initials": set()})
    trials = {}
    slot_count = None

    with ExitStack() as stack:
        def table(name, header):
            f = stack.enter_context((directory / name).open("w", newline="", encoding="utf-8"))
            w = csv.writer(f, delimiter="\t", lineterminator="\n")
            w.writerow(header)
            return w
        phases = table("phase-ledger.tsv", PHASE_FIELDS + ["image_family"])
        locations = table("location-ledger.tsv", LOCATION_FIELDS + ["raw_zero_x", "raw_zero_y", "reason"])
        bins = table("bins.tsv", BIN_FIELDS)
        for image_index, item in enumerate(images, 1):
            name, category = names[image_index - 1], scalar(item["category"], "category", text=True)
            users = item["userdata"]
            users = [users] if isinstance(users, dict) else list(users)
            if slot_count is None:
                slot_count = len(users)
            if not users or len(users) != slot_count:
                raise ValueError(f"Inconsistent userdata slot count for {name}")
            for slot, user in enumerate(users, 1):
                if not isinstance(user, dict):
                    raise ValueError(f"Expected userdata struct: {name}/{slot}")
                label = scalar(user.get("RECORDING_SESSION_LABEL", []), "recording label", text=True)
                initials = scalar(user.get("sinit", []), "initials", text=True)
                trial_number = scalar(user.get("trial", []), "trial", missing_nan=True)
                if isinstance(trial_number, float) and math.isnan(trial_number):
                    counts["unknown_trial_records"] += 1
                slot_state = "metadata_present" if any(has_value(v) for v in user.values()) else "empty_slot"
                if slot_state != "empty_slot":
                    if label:
                        identities[slot]["recording_labels"].add(label)
                    if initials:
                        identities[slot]["initials"].add(initials)
                sdt = np.atleast_1d(user.get("SDT", []))
                if sdt.size and sdt.shape != (3,):
                    raise ValueError(f"SDT width for {name}/{slot}: {sdt.shape}")
                codes = [scalar(v, "SDT") for v in sdt]
                fixes = user.get("fixations")
                for phase_index, phase in enumerate(PHASES):
                    code = codes[phase_index] if codes else ""
                    response_state = ("absent" if code == "" else "missing_sentinel" if code == -1
                                      else "recorded" if code in (1, 2, 3, 4) else "unknown_code")
                    raw = np.empty((0, 2))
                    if not isinstance(fixes, dict):
                        if fixes is not None and has_value(fixes):
                            raise ValueError(f"Nonempty nonstruct fixations: {name}/{slot}")
                        gaze_state = "absent_fixation_struct"
                    elif phase not in fixes:
                        gaze_state = "missing_phase"
                    else:
                        raw = np.asarray(fixes[phase], dtype=float)
                        gaze_state = "empty_phase" if raw.size == 0 else "locations"
                        if raw.size == 0:
                            raw = np.empty((0, 2))
                        elif raw.ndim == 1 and raw.shape == (2,):
                            raw = raw.reshape(1, 2)
                        elif raw.ndim != 2 or raw.shape[1] != 2:
                            raise ValueError(f"Expected Nx2 locations for {name}/{slot}/{phase}: {raw.shape}")
                    rounded = np.copysign(np.floor(np.abs(raw) + 0.5), raw)
                    valid = (np.all(np.isfinite(raw), axis=1) & np.all(rounded >= 1, axis=1)
                             & (rounded[:, 0] <= width) & (rounded[:, 1] <= height))
                    admitted = rounded[valid] - 1
                    for ordinal, (xy, raster, keep) in enumerate(zip(raw, rounded, valid), 1):
                        reason = "admitted" if keep else "nonfinite" if not np.all(np.isfinite(xy)) else "outside_pixel_bounds"
                        locations.writerow([image_index, slot, phase, ordinal, *xy, *raster, int(keep),
                                            *(raster - 1 if keep else ("", "")), *(xy - 1), reason])
                    phases.writerow([image_index, name, category, slot, label, initials, trial_number,
                                     phase, slot_state, gaze_state, code, response_state, len(raw), len(admitted), canonical[name]])
                    counts["phase_records"] += 1
                    counts["source_locations"] += len(raw)
                    counts["admitted_locations"] += len(admitted)
                    counts["excluded_locations"] += len(raw) - len(admitted)
                    states[gaze_state] += 1
                    responses[f"{phase}:{response_state}:{code}"] += 1
                    if len(admitted):
                        key = slot, name, phase
                        trials[key] = Trial(image_index, name, slot, phase, category, code, admitted, np.flatnonzero(valid) + 1)
                        # NumPy's histogram reference is independent of eyes4s Grid.
                        h, _, _ = np.histogram2d(admitted[:, 0], admitted[:, 1], bins=(cols, rows),
                                               range=((0, width), (0, height)))
                        for bx, by in np.argwhere(h):
                            bins.writerow([image_index, slot, phase, int(bx), int(by), int(h[bx, by])])
                    elif len(raw):
                        counts["all_invalid_phases"] += 1
    identity = {str(s): {k: sorted(v) for k, v in fields.items()} for s, fields in sorted(identities.items())}
    counts["admitted_trials"] = len(trials)
    counts["observer_slots"] = len(identities)
    counts["missing_or_empty_phases"] = sum(v for k, v in states.items() if k != "locations")
    summary = {
        "schema_version": 1, "counts": dict(counts), "gaze_states": dict(states), "response_states": dict(responses),
        "identity": {"slots": identity, "unique_recording_labels": len({x for v in identity.values() for x in v["recording_labels"]}),
                     "distinct_initials_labels": len({x for v in identity.values() for x in v["initials"]}),
                     "person_mapping_status": "UNRESOLVED", "independent_participant_count": None},
        "geometry": {"width": width, "height": height, "cols": cols, "rows": rows,
                     "raster": "MATLAB round ties away from zero; retain 1..width/height; subtract one",
                     "raw_precision": "raw doubles retained; raw_zero columns are not a qualified alternative estimator"},
        "order": "source row ordinal retained; chronological meaning not established by this decoder",
        "limits": ["No timestamps or fixation durations", "No participant linkage inferred from initials",
                   "Empty source slots indicate unpresented images per README; missing phases remain distinct",
                   "Source -1 response code retained; cause of missing response unknown"],
    }
    (directory / "admission.json").write_text(json.dumps(summary, indent=2) + "\n")
    return trials, summary

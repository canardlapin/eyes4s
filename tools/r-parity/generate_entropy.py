#!/usr/bin/env python3
"""Regenerate the pinned eyesim entropy and map-arithmetic reference and its oracle.

eyesim's `fixation_entropy.eye_density` (which is `entropy_from_mass` on the
map's cells) and `Ops.eye_density` are called on an isolated installation of
the pinned revision, on hand-built `eye_density` objects over the small
lattice in `baseline-cases.json`. Where the formula is closed-form the result
is compared with an oracle computed here without either implementation: exact
rational arithmetic for the two-map mean and the difference, and 60-digit
decimal logarithms for the Shannon entropy of a normalised vector and the log
ratio away from zero cells. Where eyesim's answer is a non-finite value (`NA`,
`Inf`, `-Inf`, `NaN`) or an error, that output is pinned as a literal so the
divergence from eyes4s is evidenced rather than asserted. The reading of
eyesim's positive-cell formula on signed maps is labelled as such: it
documents what eyesim computes, not an independent estimand. No network
access or writes to the source checkout occur.
"""
from __future__ import annotations

import argparse
from decimal import Decimal, getcontext
from fractions import Fraction
import hashlib
import json
from pathlib import Path
import sys

sys.dont_write_bytecode = True
import parity  # noqa: E402

ROOT = parity.ROOT
HERE = parity.HERE
REVISION = parity.pinned_revision()
INPUT = HERE / "fixtures/baseline-cases.json"
JSON_OUT = HERE / "fixtures/entropy.json"
SCALA = ROOT / "kernel/src/test/scala/eyes4s/kernel/EntropyFixtures.scala"
TOLERANCE = 1e-12  # Logarithms of small rationals and single sums of decimal inputs.
FLOOR = Decimal("1e-12")  # eyes4s Mass.logRatio default floor.
MACHINE_EPS = Decimal("2.220446049250313e-16")  # R .Machine$double.eps.
BASES = {"2": Decimal(2), "e": Decimal(1).exp()}
NA = "NA"

getcontext().prec = 60


def frac(value) -> Fraction:
    return Fraction(str(value))


def dec(value) -> Decimal:
    if isinstance(value, Fraction):
        return Decimal(value.numerator) / Decimal(value.denominator)
    return Decimal(str(value))


def ln(value) -> Decimal:
    return dec(value).ln()


def shannon(mass: list[Fraction]) -> dict[str, dict[str, Decimal]]:
    """Entropy of a non-negative vector after sum-normalisation, by base, raw and relative.

    Zero cells contribute nothing (the limit of p log p). The relative value
    divides by the log of the cell count, the maximum on this lattice.
    """
    total = sum(mass)
    assert total > 0
    probs = [m / total for m in mass]
    nats = -sum((dec(p) * ln(p) for p in probs if p > 0), Decimal(0))
    n = Decimal(len(mass))
    return {
        label: {"raw": nats / ln(base), "normalized": nats / n.ln()}
        for label, base in BASES.items()
    }


def log_ratio(a: list[Fraction], b: list[Fraction]) -> list[Decimal | str]:
    """Natural-log ratio cell by cell; a zero cell is named for what R returns."""
    out: list[Decimal | str] = []
    for x, y in zip(a, b):
        if x > 0 and y > 0:
            out.append(ln(x / y))
        elif x == 0 and y == 0:
            out.append("NaN")
        elif x == 0:
            out.append("-Inf")
        else:
            out.append("Inf")
    return out


def floored_log_ratio(a: list[Fraction], b: list[Fraction]) -> list[Decimal]:
    """A restatement of the eyes4s Mass.logRatio floor rule, the counterpart of
    eyesim_formula for the eyes4s side: both cells are floored at 1e-12 before
    the ratio. It records what eyes4s computes at a zero cell so the divergence
    from eyesim's unfloored -Inf/Inf/NaN is a pinned number, not an estimand.
    """
    return [(max(dec(x), FLOOR) / max(dec(y), FLOOR)).ln() for x, y in zip(a, b)]


def eyesim_formula(cells: list, base: Decimal, normalize: bool) -> Decimal | str:
    """A reading of eyesim's entropy_from_mass at the pinned revision.

    Non-finite cells are dropped, the remainder is divided by its signed total,
    only positive cells enter the sum, and the normaliser is the log of the count
    of finite cells. This is what eyesim computes on a signed map, not a claim
    that the number means anything.
    """
    finite = [dec(c) for c in cells if not isinstance(c, str)]
    if not finite:
        return NA
    total = sum(finite, Decimal(0))
    if total <= MACHINE_EPS:
        return NA
    probs = [v / total for v in finite]
    probs = [p for p in probs if p > 0]
    if not probs:
        return NA
    nats = -sum((p * p.ln() for p in probs), Decimal(0))
    if not normalize:
        return nats / base.ln()
    if len(finite) <= 1:
        return Decimal(0)
    return nats / Decimal(len(finite)).ln()


def formula_by_base(cells: list) -> dict[str, dict[str, Decimal | str]]:
    return {
        label: {
            "raw": eyesim_formula(cells, base, False),
            "normalized": eyesim_formula(cells, base, True),
        }
        for label, base in BASES.items()
    }


def oracle(spec: dict) -> dict:
    lattice = spec["lattice"]
    nx, ny = len(lattice["x"]), len(lattice["y"])
    assert (nx, ny) == (2, 2), "the pinned lattice is two by two"
    maps = {
        k: [frac(v) for v in spec[k]] for k in ["mass_p", "mass_q", "mass_r", "counts"]
    }
    signed = [frac(v) for v in spec["signed"]]
    p, q, r, counts = (maps[k] for k in ["mass_p", "mass_q", "mass_r", "counts"])
    assert all(len(m) == nx * ny for m in [p, q, r, counts, signed])
    assert sum(p) == 1 and sum(q) == 1 and sum(r) == 1
    assert p.count(0) == 1 and all(v > 0 for v in q) and all(v > 0 for v in r)
    assert (
        any(v < 0 for v in signed) and sum(signed) > 0
    ), "signed must have a positive total"
    assert [c / sum(counts) for c in counts] == p, "counts must normalise to mass_p"
    assert [float(b) for b in spec["log_bases"]] == [2.0, float(BASES["e"])]

    difference = [a - b for a, b in zip(p, q)]
    assert sum(difference) == 0, "an exact difference of masses has zero total"
    assert any(v < 0 for v in difference)
    assert "mass_p" in spec and spec["shifted_lattice"]["x"] != lattice["x"]

    exact = {
        "lattice": {
            "frame": [frac(v) for v in lattice["frame"]],
            "x": [frac(v) for v in lattice["x"]],
            "y": [frac(v) for v in lattice["y"]],
            "nx": nx,
            "ny": ny,
        },
        "mass_p": p,
        "mass_q": q,
        "mass_r": r,
        "counts": counts,
        "signed": signed,
        "entropy": {k: shannon(m) for k, m in maps.items()},
        "mean_p_q": [(a + b) / 2 for a, b in zip(p, q)],
        "difference_p_q": difference,
        "log_ratio_r_q": log_ratio(r, q),
        "log_ratio_p_q": log_ratio(p, q),
        "log_ratio_q_p": log_ratio(q, p),
        "log_ratio_p_p": log_ratio(p, p),
        "floored_log_ratio_p_q": floored_log_ratio(p, q),
        "floored_log_ratio_q_p": floored_log_ratio(q, p),
        "floored_log_ratio_p_p": floored_log_ratio(p, p),
        # Readings of eyesim's own formula on maps eyes4s refuses by type.
        "eyesim_formula": {
            "signed": formula_by_base(signed),
            "minus_p_q": formula_by_base(difference),
            "div_p_q": formula_by_base(log_ratio(p, q)),
            "div_p_p": formula_by_base(log_ratio(p, p)),
        },
    }
    assert exact["entropy"]["mass_p"]["2"]["raw"] == Decimal(
        "1.5"
    ), "the selected mass has 1.5 bits"
    assert exact["eyesim_formula"]["minus_p_q"]["e"]["raw"] == NA
    assert exact["eyesim_formula"]["div_p_p"]["e"]["raw"] == NA
    assert exact["eyesim_formula"]["div_p_q"]["e"]["raw"] == Decimal(0)
    assert isinstance(exact["eyesim_formula"]["signed"]["e"]["raw"], Decimal)
    return exact


def run_r(eyesim: Path) -> dict:
    # The pinned archive in the locked R session; never an arbitrary installed eyesim package.
    with parity.r_session("eyes4s-r-entropy-", eyesim) as r:
        output = r.tmp / "entropy.json"
        r.rscript(HERE / "entropy.R", r.eyesim_library, INPUT, output)
        return json.loads(output.read_text())


def close(r_value: dict, target: Decimal | str, where) -> None:
    if isinstance(target, str):
        assert r_value["kind"] == target, (where, r_value, target)
    else:
        assert r_value["kind"] == "finite", (where, r_value, target)
        assert abs(Decimal(repr(r_value["value"])) - target) <= Decimal(
            repr(TOLERANCE)
        ), (
            where,
            r_value,
            target,
        )


def verify(reference: dict, exact: dict) -> None:
    assert float(reference["machine_eps"]) == float(MACHINE_EPS), reference["machine_eps"]
    for name in ["mass_p", "mass_q", "mass_r", "counts"]:
        for base in BASES:
            for kind in ["raw", "normalized"]:
                close(
                    reference["entropy"][name][base][kind],
                    exact["entropy"][name][base][kind],
                    (name, base, kind),
                )
    assert reference["entropy"]["mass_p_density_class"] == reference["entropy"]["mass_p"], (
        "fixation_entropy.density and fixation_entropy.eye_density must agree"
    )
    for name in ["signed", "minus_p_q", "div_p_q", "div_p_p"]:
        for base in BASES:
            for kind in ["raw", "normalized"]:
                close(
                    reference["entropy"][name][base][kind],
                    exact["eyesim_formula"][name][base][kind],
                    ("formula", name, base, kind),
                )
    ops = reference["ops"]
    pairs = {
        "plus_p_q": [dec(v) for v in exact["mean_p_q"]],
        "minus_p_q": [dec(v) for v in exact["difference_p_q"]],
        "div_p_q": exact["log_ratio_p_q"],
        "div_q_p": exact["log_ratio_q_p"],
        "div_r_q": exact["log_ratio_r_q"],
        "div_p_p": exact["log_ratio_p_p"],
    }
    for name, targets in pairs.items():
        cells = ops[name]["cells"]
        assert len(cells) == len(targets), name
        for i, (cell, target) in enumerate(zip(cells, targets)):
            close(cell, target, (name, i))
        assert ops[name]["class"][1:] == ["eye_density", "density", "list"], ops[name][
            "class"
        ]
        assert ops[name]["class"][0] == {
            "plus_p_q": "eye_density_add",
            "minus_p_q": "eye_density_delta",
        }.get(name, "eye_density_div")
        assert ops[name]["has_sigma"] is False, name
    # The pinned non-finite cells are exactly the zero-cell positions.
    zero = exact["mass_p"].index(Fraction(0))
    assert [c["kind"] for c in ops["div_p_q"]["cells"]].count("-Inf") == 1
    assert ops["div_p_q"]["cells"][zero]["kind"] == "-Inf"
    assert ops["div_q_p"]["cells"][zero]["kind"] == "Inf"
    assert ops["div_p_p"]["cells"][zero]["kind"] == "NaN"
    assert all(c["kind"] == "finite" for c in ops["div_r_q"]["cells"])
    assert ops["minus_p_q"]["total"] == {"kind": "finite", "value": 0}
    for name, fragment in [
        ("product_p_q", "undefined operation"),
        ("shifted_lattice_minus", "e1$x == e2$x"),
    ]:
        outcome = reference["outcomes"][name]
        assert outcome["outcome"] == "error" and fragment in outcome["detail"], (
            name,
            outcome,
        )


def rational(f: Fraction) -> str:
    return f"{f.numerator}.0 / {f.denominator}.0"


def scala(exact: dict, reference: dict) -> str:
    header = (
        (ROOT / "kernel/src/test/scala/eyes4s/kernel/OccupancySuite.scala")
        .read_text()
        .split("package ")[0]
    )

    def rv(cell: dict) -> str:
        kind = cell["kind"]
        if kind == "finite":
            return f"RValue.Finite({float(cell['value'])!r})"
        return {
            "NA": "RValue.NA",
            "NaN": "RValue.NaN",
            "Inf": "RValue.Inf",
            "-Inf": "RValue.NegInf",
        }[kind]

    def rvs(cells: list) -> str:
        return "Vector(" + ", ".join(rv(c) for c in cells) + ")"

    def fracs(values: list) -> str:
        return "Vector(" + ", ".join(rational(v) for v in values) + ")"

    def decs(values: list) -> str:
        return "Vector(" + ", ".join(repr(float(v)) for v in values) + ")"

    def opt_decs(values: list) -> str:
        return (
            "Vector("
            + ", ".join(
                "None" if isinstance(v, str) else f"Some({float(v)!r})" for v in values
            )
            + ")"
        )

    def eyesim_entropy(name: str) -> str:
        e = reference["entropy"][name]
        return (
            f"EyesimEntropy({rv(e['2']['raw'])}, {rv(e['2']['normalized'])}, "
            f"{rv(e['e']['raw'])}, {rv(e['e']['normalized'])})"
        )

    def exact_entropy(e: dict) -> str:
        return (
            f"ExactEntropy({float(e['2']['raw'])!r}, {float(e['2']['normalized'])!r}, "
            f"{float(e['e']['raw'])!r}, {float(e['e']['normalized'])!r})"
        )

    def operation(name: str) -> str:
        op = reference["ops"][name]
        return (
            f"Operation({json.dumps(name)}, {rvs(op['cells'])}, "
            f"{json.dumps(op['class'][0])}, {'true' if op['has_sigma'] else 'false'})"
        )

    lat = exact["lattice"]
    lines = [
        header.rstrip(),
        "",
        "package eyes4s.kernel",
        "",
        "// Generated by tools/r-parity/generate_entropy.py. Do not hand-edit.",
        "// format: off",
        "object EntropyFixtures:",
        f'  val eyesimRevision = "{REVISION}"',
        f'  val inputSha256 = "{hashlib.sha256(INPUT.read_bytes()).hexdigest()}"',
        f"  val tolerance = {TOLERANCE!r}",
        "  /** The eyes4s Mass.logRatio default floor, restated so the floored oracle names it. */",
        f"  val floor = {float(FLOOR)!r}",
        f"  val machineEpsilon = {float(MACHINE_EPS)!r}",
        "  /** A value as R reports it. NA is not a Double, so it is a case of its own. */",
        "  enum RValue derives CanEqual:",
        "    case Finite(value: Double)",
        "    case NA",
        "    case NaN",
        "    case Inf",
        "    case NegInf",
        "  /** eyesim entropy output: base 2 raw and normalised, then base e raw and normalised. */",
        "  final case class EyesimEntropy(bits: RValue, relativeBits: RValue, nats: RValue, relativeNats: RValue)",
        "  final case class ExactEntropy(bits: Double, relativeBits: Double, nats: Double, relativeNats: Double)",
        "  /** One Ops.eye_density result: its cells in Grid index order, its first class, and whether a sigma survives. */",
        "  final case class Operation(name: String, cells: Vector[RValue], firstClass: String, hasSigma: Boolean)",
        "  final case class Outcome(operation: String, outcome: String, detail: String)",
        f"  val frameWidth = {rational(lat['frame'][0])}",
        f"  val frameHeight = {rational(lat['frame'][1])}",
        f"  val nx = {lat['nx']}",
        f"  val ny = {lat['ny']}",
        f"  val latticeX = {fracs(lat['x'])}",
        f"  val latticeY = {fracs(lat['y'])}",
        f"  val massP = {fracs(exact['mass_p'])}",
        f"  val massQ = {fracs(exact['mass_q'])}",
        f"  val massR = {fracs(exact['mass_r'])}",
        f"  val counts = {fracs(exact['counts'])}",
        f"  val signed = {fracs(exact['signed'])}",
        "  // eyesim entropy_from_mass, reached through the public eye_density entropy method.",
        f"  val eyesimEntropyP = {eyesim_entropy('mass_p')}",
        f"  val eyesimEntropyQ = {eyesim_entropy('mass_q')}",
        f"  val eyesimEntropyR = {eyesim_entropy('mass_r')}",
        f"  val eyesimEntropyCounts = {eyesim_entropy('counts')}",
        "  // The p cells under the bare `density` class, through the other public entropy method.",
        f"  val eyesimEntropyPDensityClass = {eyesim_entropy('mass_p_density_class')}",
        "  // The same entry point on maps eyes4s refuses by type.",
        f"  val eyesimEntropySigned = {eyesim_entropy('signed')}",
        f"  val eyesimEntropyDifferencePQ = {eyesim_entropy('minus_p_q')}",
        f"  val eyesimEntropyLogRatioPQ = {eyesim_entropy('div_p_q')}",
        f"  val eyesimEntropyLogRatioPP = {eyesim_entropy('div_p_p')}",
        "  // 60-digit decimal oracle of the Shannon entropy of each normalised vector.",
        f"  val exactEntropyP = {exact_entropy(exact['entropy']['mass_p'])}",
        f"  val exactEntropyQ = {exact_entropy(exact['entropy']['mass_q'])}",
        f"  val exactEntropyR = {exact_entropy(exact['entropy']['mass_r'])}",
        "  // A reading of eyesim's positive-cell formula on the signed vector: what it computes, not an estimand.",
        f"  val formulaEntropySigned = {exact_entropy(exact['eyesim_formula']['signed'])}",
        "  // Ops.eye_density at the pinned revision.",
        f"  val eyesimPlusPQ = {operation('plus_p_q')}",
        f"  val eyesimMinusPQ = {operation('minus_p_q')}",
        f"  val eyesimDivPQ = {operation('div_p_q')}",
        f"  val eyesimDivQP = {operation('div_q_p')}",
        f"  val eyesimDivRQ = {operation('div_r_q')}",
        f"  val eyesimDivPP = {operation('div_p_p')}",
        "  // Exact rational mean and difference; decimal log ratio, None where a cell is zero.",
        f"  val exactMeanPQ = {fracs(exact['mean_p_q'])}",
        f"  val exactDifferencePQ = {fracs(exact['difference_p_q'])}",
        f"  val exactLogRatioRQ = {decs(exact['log_ratio_r_q'])}",
        f"  val exactLogRatioPQ = {opt_decs(exact['log_ratio_p_q'])}",
        f"  val exactLogRatioQP = {opt_decs(exact['log_ratio_q_p'])}",
        f"  val exactLogRatioPP = {opt_decs(exact['log_ratio_p_p'])}",
        "  // The eyes4s floored log ratio, computed from the same rationals with both cells floored at `floor`.",
        f"  val flooredLogRatioPQ = {decs(exact['floored_log_ratio_p_q'])}",
        f"  val flooredLogRatioQP = {decs(exact['floored_log_ratio_q_p'])}",
        f"  val flooredLogRatioPP = {decs(exact['floored_log_ratio_p_p'])}",
        "  // Observed failures: the product and a shifted lattice.",
        "  val eyesimOutcomes = Vector(",
    ]
    for name in ["product_p_q", "shifted_lattice_minus"]:
        outcome = reference["outcomes"][name]
        lines.append(
            f"    Outcome({json.dumps(name)}, {json.dumps(outcome['outcome'])}, {json.dumps(outcome['detail'])}),"
        )
    lines += ["  )", "// format: on", ""]
    return "\n".join(lines)


def jsonable(value):
    if isinstance(value, (Fraction, Decimal)):
        return str(value)
    if isinstance(value, dict):
        return {k: jsonable(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [jsonable(v) for v in value]
    return value


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--eyesim",
        type=Path,
        required=True,
        help="Local Git checkout containing the pinned revision",
    )
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    spec = json.loads(INPUT.read_text())["entropy_and_arithmetic"]
    exact = oracle(spec)
    reference = run_r(args.eyesim)
    verify(reference, exact)
    payload = {
        "eyesim_revision": REVISION,
        "input_sha256": hashlib.sha256(INPUT.read_bytes()).hexdigest(),
        "input_license": "Apache-2.0; synthetic data authored for eyes4s",
        "oracle": (
            "Exact rational arithmetic for the two-map mean and the difference; 60-digit decimal "
            "logarithms for the Shannon entropy of each normalised vector and for the log ratio "
            "away from zero cells. eyesim's fixation_entropy.eye_density (entropy_from_mass) and "
            "Ops.eye_density are called on hand-built eye_density objects over the pinned lattice. "
            "Non-finite cells, NA entropies and errors are pinned as eyesim literals. The "
            "eyesim_formula block is a reading of entropy_from_mass on signed maps: it records what "
            "eyesim computes there, not an estimand eyes4s implements."
        ),
        "lattice_order": spec["lattice"]["order"],
        "numerical_tolerance": {"absolute": TOLERANCE, "relative": 0},
        "eyes4s_log_ratio_floor": str(FLOOR),
        "exact": jsonable(exact),
        "eyesim": {
            "machine_eps": reference["machine_eps"],
            "entropy": reference["entropy"],
            "ops": reference["ops"],
            "outcomes": reference["outcomes"],
        },
        "runtime": reference["runtime"],
    }
    files = {
        JSON_OUT: json.dumps(payload, indent=2) + "\n",
        SCALA: scala(exact, reference),
    }
    for path, content in files.items():
        if args.check:
            if not path.exists() or path.read_text() != content:
                raise RuntimeError(f"Reference drift: {path.relative_to(ROOT)}")
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(content)
    print(
        f'{"Checked" if args.check else "Generated"} {len(files)} artifacts: '
        f"{len(reference['entropy'])} entropy inputs in two bases, {len(reference['ops'])} operator "
        f"results, {len(reference['outcomes'])} observed failures; oracle agrees with eyesim."
    )


if __name__ == "__main__":
    main()

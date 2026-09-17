#!/usr/bin/env python3
"""Regenerate the pinned eyesim coordinate-transform reference and an exact affine oracle.

eyesim's `center`, `rescale` and `normalize` are public `fixation_group` methods.
They are called on an isolated installation of the pinned revision and compared,
point by point, with an exact rational oracle computed here without either
implementation. The homogeneous affine matrix, its training-only solve from the
three fitting pairs and the held-out target have no eyesim coordinate entry
point and are exact-oracle values only. eyesim's `affine_transform` and
`contract_transform` are fitted density-space maps, not coordinate maps; the
harness records how they respond to coordinate tables so that boundary is pinned
rather than asserted. No network access or writes to the source checkout occur.
"""
from __future__ import annotations

import argparse
from fractions import Fraction
import hashlib
import json
import os
from pathlib import Path
import subprocess
import tarfile
import tempfile

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
REVISION = "ecb9c496257bce51acd5330af6a5e7a8d5b84e05"
INPUT = HERE / "fixtures/baseline-cases.json"
JSON_OUT = HERE / "fixtures/transforms.json"
SCALA = ROOT / "core/src/test/scala/eyes4s/core/TransformFixtures.scala"
TOLERANCE = 1e-12  # Single products and differences of small decimal inputs.


def checked(*args, **kwargs):
    return subprocess.run(args, check=True, text=True, **kwargs)


def frac(value) -> Fraction:
    return Fraction(str(value))


def apply_affine(m, p):
    (a, b, tx), (c, d, ty), bottom = m
    assert bottom == [Fraction(0), Fraction(0), Fraction(1)]
    return [a * p[0] + b * p[1] + tx, c * p[0] + d * p[1] + ty]


def solve3(rows, rhs):
    """Exact Gaussian elimination for a 3x3 system; rows and rhs are Fractions."""
    m = [list(r) + [v] for r, v in zip(rows, rhs)]
    for col in range(3):
        pivot = next(i for i in range(col, 3) if m[i][col] != 0)
        m[col], m[pivot] = m[pivot], m[col]
        m[col] = [v / m[col][col] for v in m[col]]
        for i in range(3):
            if i != col and m[i][col] != 0:
                factor = m[i][col]
                m[i] = [a - factor * b for a, b in zip(m[i], m[col])]
    return [m[i][3] for i in range(3)]


def oracle(spec):
    points = [[frac(x), frac(y)] for x, y in spec["points"]]
    n = len(points)
    origin = [frac(v) for v in spec["origin"]]
    mean = [sum(p[0] for p in points) / n, sum(p[1] for p in points) / n]
    sx, sy = (frac(v) for v in spec["rescale"])
    xmin, xmax, ymin, ymax = (frac(v) for v in spec["bounds"])
    assert xmax - xmin != ymax - ymin, "the case must use non-square bounds"
    matrix = [[frac(v) for v in row] for row in spec["affine_homogeneous"]]
    pairs = [
        ([frac(v) for v in pr["source"]], [frac(v) for v in pr["target"]])
        for pr in spec["fit_pairs"]
    ]
    held_source = [frac(v) for v in spec["held_out"]["source"]]
    held_target = [frac(v) for v in spec["held_out"]["target"]]

    # Training-only solve: three pairs determine the six coefficients exactly.
    assert len(pairs) == 3
    rows = [[s[0], s[1], Fraction(1)] for s, _ in pairs]
    a, b, tx = solve3(rows, [t[0] for _, t in pairs])
    c, d, ty = solve3(rows, [t[1] for _, t in pairs])
    fitted = [[a, b, tx], [c, d, ty], [Fraction(0), Fraction(0), Fraction(1)]]
    assert fitted == matrix, "the fitting pairs must determine the declared matrix"
    assert (
        apply_affine(fitted, held_source) == held_target
    ), "held-out point disagrees with the fit"
    assert all(apply_affine(matrix, s) == t for s, t in pairs)

    # Falsification witnesses: each mutant must miss the held-out target.
    swapped = [
        [matrix[0][1], matrix[0][0], matrix[0][2]],
        [matrix[1][1], matrix[1][0], matrix[1][2]],
        matrix[2],
    ]
    no_translation = [
        [matrix[0][0], matrix[0][1], Fraction(0)],
        [matrix[1][0], matrix[1][1], Fraction(0)],
        matrix[2],
    ]
    mutants = {
        "axis_swap_held_out": apply_affine(swapped, held_source),
        "no_translation_held_out": apply_affine(no_translation, held_source),
    }
    assert all(v != held_target for v in mutants.values())

    return {
        "points": points,
        "origin": origin,
        "mean_origin": mean,
        "scale": [sx, sy],
        "bounds": [xmin, xmax, ymin, ymax],
        "center_supplied": [[p[0] - origin[0], p[1] - origin[1]] for p in points],
        "center_default": [[p[0] - mean[0], p[1] - mean[1]] for p in points],
        "rescale": [[p[0] * sx, p[1] * sy] for p in points],
        "normalize": [
            [(p[0] - xmin) / (xmax - xmin), (p[1] - ymin) / (ymax - ymin)]
            for p in points
        ],
        "affine": matrix,
        "affine_points": [apply_affine(matrix, p) for p in points],
        "fit_pairs": pairs,
        "fitted_affine": fitted,
        "held_out": {"source": held_source, "target": held_target},
        "mutants": mutants,
    }


def run_r(eyesim: Path) -> dict:
    env = os.environ | {"LC_ALL": "C", "LANG": "C", "RGL_USE_NULL": "TRUE"}
    with tempfile.TemporaryDirectory(prefix="eyes4s-r-transforms-") as tmp:
        tmp = Path(tmp)
        src, lib = tmp / "source", tmp / "library"
        src.mkdir()
        lib.mkdir()
        archive = tmp / "source.tar"
        checked(
            "git",
            "-C",
            str(eyesim),
            "archive",
            "--format=tar",
            "--output",
            str(archive),
            REVISION,
        )
        with tarfile.open(archive) as tar:
            tar.extractall(src, filter="data")
        # Install the exact archived source, never an arbitrary installed eyesim package.
        with (tmp / "install.log").open("w") as log:
            result = subprocess.run(
                ["R", "CMD", "INSTALL", f"--library={lib}", str(src)],
                env=env,
                stdout=log,
                stderr=subprocess.STDOUT,
            )
        if result.returncode:
            raise RuntimeError((tmp / "install.log").read_text())
        output = tmp / "transforms.json"
        checked(
            "Rscript",
            "--vanilla",
            str(HERE / "transforms.R"),
            str(lib),
            str(INPUT),
            str(output),
            env=env,
        )
        return json.loads(output.read_text())


def verify(reference: dict, exact: dict) -> None:
    for key in ["center_supplied", "center_default", "rescale", "normalize"]:
        got = reference["supplied"][key]
        assert len(got) == len(exact[key]), (key, got)
        for row, expected in zip(got, exact[key]):
            for value, target in zip(row, expected):
                assert abs(float(value) - float(target)) <= TOLERANCE, (
                    key,
                    row,
                    expected,
                )
    # Supplied outputs must be distinguishable from each other; a harness that
    # returned its input unchanged would otherwise pass the wrong comparison.
    assert (
        reference["supplied"]["center_supplied"]
        != reference["supplied"]["center_default"]
    )
    assert reference["supplied"]["rescale"] != reference["supplied"]["normalize"]
    for name in ["affine_transform", "contract_transform"]:
        outcome = reference["fitted"][name]
        assert outcome["outcome"] in {"error", "value"} and outcome["detail"], (
            name,
            outcome,
        )


def rational(f: Fraction) -> str:
    return f"{f.numerator}.0 / {f.denominator}.0"


def scala(exact: dict, reference: dict) -> str:
    header = (
        (ROOT / "core/src/test/scala/eyes4s/core/ScanpathSuite.scala")
        .read_text()
        .split("package ")[0]
    )

    def p(v) -> str:
        return f"P({rational(v[0])}, {rational(v[1])})"

    def pf(v) -> str:
        return f"P({float(v[0])!r}, {float(v[1])!r})"

    def affine(m) -> str:
        (a, b, tx), (c, d, ty), _ = m
        return f"Affine({rational(a)}, {rational(b)}, {rational(tx)}, {rational(c)}, {rational(d)}, {rational(ty)})"

    lines = [
        header.rstrip(),
        "",
        "package eyes4s.core",
        "",
        "// Generated by tools/r-parity/generate_transforms.py. Do not hand-edit.",
        "// format: off",
        "object TransformFixtures:",
        f'  val eyesimRevision = "{REVISION}"',
        f'  val inputSha256 = "{hashlib.sha256(INPUT.read_bytes()).hexdigest()}"',
        f"  val tolerance = {TOLERANCE!r}",
        "  final case class P(x: Double, y: Double)",
        "  final case class Affine(a: Double, b: Double, tx: Double, c: Double, d: Double, ty: Double)",
        "  final case class Pair(source: P, target: P)",
        "  final case class Outcome(entryPoint: String, outcome: String, detail: String)",
        "  /** xMin, xMax, yMin, yMax: the eyesim bounds convention. */",
        "  val bounds = (" + ", ".join(rational(v) for v in exact["bounds"]) + ")",
        "  val points = Vector(" + ", ".join(p(v) for v in exact["points"]) + ")",
        f"  val origin = {p(exact['origin'])}",
        f"  val meanOrigin = {p(exact['mean_origin'])}",
        f"  val scale = ({rational(exact['scale'][0])}, {rational(exact['scale'][1])})",
        f"  val affine = {affine(exact['affine'])}",
        "  val fitPairs = Vector("
        + ", ".join(f"Pair({p(s)}, {p(t)})" for s, t in exact["fit_pairs"])
        + ")",
        f"  val heldOut = Pair({p(exact['held_out']['source'])}, {p(exact['held_out']['target'])})",
        "  // Public eyesim fixation_group outputs at the pinned revision.",
        "  val eyesimCenterSupplied = Vector("
        + ", ".join(pf(v) for v in reference["supplied"]["center_supplied"])
        + ")",
        "  val eyesimCenterDefault = Vector("
        + ", ".join(pf(v) for v in reference["supplied"]["center_default"])
        + ")",
        "  val eyesimRescale = Vector("
        + ", ".join(pf(v) for v in reference["supplied"]["rescale"])
        + ")",
        "  val eyesimNormalize = Vector("
        + ", ".join(pf(v) for v in reference["supplied"]["normalize"])
        + ")",
        "  // Exact rational oracle; eyesim has no coordinate affine entry point.",
        "  val exactCenterSupplied = Vector("
        + ", ".join(p(v) for v in exact["center_supplied"])
        + ")",
        "  val exactCenterDefault = Vector("
        + ", ".join(p(v) for v in exact["center_default"])
        + ")",
        "  val exactRescale = Vector("
        + ", ".join(p(v) for v in exact["rescale"])
        + ")",
        "  val exactNormalize = Vector("
        + ", ".join(p(v) for v in exact["normalize"])
        + ")",
        "  val exactAffinePoints = Vector("
        + ", ".join(p(v) for v in exact["affine_points"])
        + ")",
        f"  val exactFittedAffine = {affine(exact['fitted_affine'])}",
        f"  val mutantAxisSwapHeldOut = {p(exact['mutants']['axis_swap_held_out'])}",
        f"  val mutantNoTranslationHeldOut = {p(exact['mutants']['no_translation_held_out'])}",
        "  // How eyesim's fitted density-space transforms respond to coordinate tables.",
        "  val eyesimFittedTransforms = Vector(",
    ]
    for name in ["affine_transform", "contract_transform"]:
        outcome = reference["fitted"][name]
        lines.append(
            f"    Outcome({json.dumps(name)}, {json.dumps(outcome['outcome'])}, {json.dumps(outcome['detail'])}),"
        )
    lines += ["  )", "// format: on", ""]
    return "\n".join(lines)


def jsonable(value):
    if isinstance(value, Fraction):
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
    spec = json.loads(INPUT.read_text())["transforms"]
    exact = oracle(spec)
    reference = run_r(args.eyesim)
    verify(reference, exact)
    payload = {
        "eyesim_revision": REVISION,
        "input_sha256": hashlib.sha256(INPUT.read_bytes()).hexdigest(),
        "input_license": "Apache-2.0; synthetic data authored for eyes4s",
        "oracle": (
            "Exact rational arithmetic for every supplied map, a training-only solve of the "
            "affine from the three fitting pairs, the held-out target, and axis-swap and "
            "no-translation mutants. center, rescale and normalize are compared with public "
            "eyesim fixation_group output; the affine has no eyesim coordinate entry point."
        ),
        "fitted_transforms": (
            "eyesim affine_transform and contract_transform fit density-space maps from matched "
            "density objects and resample densities; they are not coordinate transforms. Their "
            "response to coordinate tables is recorded below and eyes4s does not implement them."
        ),
        "numerical_tolerance": {"absolute": TOLERANCE, "relative": 0},
        "exact": jsonable(exact),
        "eyesim": {"supplied": reference["supplied"], "fitted": reference["fitted"]},
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
        f"{len(exact['points'])} points, four supplied maps, one training-only affine with a held-out "
        "target and two mutants; rational oracle agrees with eyesim."
    )


if __name__ == "__main__":
    main()

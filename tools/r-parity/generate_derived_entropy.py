#!/usr/bin/env python3
"""Regenerate the pinned eyesim fixation_entropy reference on derived inputs and its oracle.

eyesim's `fixation_entropy` is called on an isolated installation of the pinned
revision for a fixation group (the `grid` and `density` methods), for the
`eye_density_multiscale` object `eye_density` builds from a sigma vector, and
for hand-built multiscale objects over supplied maps. Every value is compared
with an oracle computed here without either implementation:

- grid entropy: exact rational breakpoints and cell counts (half-open cells, a
  closed last cell, outside fixations clamped to the edge cell) and 60-digit
  decimal logarithms;
- entropy of eyesim's own density maps: the decimal Shannon entropy of the
  pinned cells, which isolates the entropy step from the estimator;
- eyesim's density estimate: a continuous isotropic Gaussian evaluated at the
  endpoint-inclusive evaluation lattice, each observation confined to the
  support box `ks::kde` evaluates (3.7 standard deviations, widened to the
  lattice point below), which agrees only to the rounding
  eyesim applies to every map (`zapsmall(z, 7)`), so it is checked at a looser,
  named tolerance;
- the eyes4s native estimate: a direct two-dimensional sum of the truncated,
  normalised discrete Gaussian on cell centres under both edge policies, an
  oracle for `FixationEntropy.density` and `multiscale` that does not repeat the
  production separable convolution;
- multiscale reductions: per-scale decimal entropies, their mean, and an
  explicitly weighted mean that eyesim does not offer.

Where eyesim's answer is `NA` or an error, the literal is pinned. No network
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
INPUT = HERE / "fixtures/cases/eyesim-entropy.json"
JSON_OUT = HERE / "fixtures/derived-entropy.json"
SCALA = ROOT / "surface/src/test/scala/eyes4s/surface/DerivedEntropyReference.scala"
TOLERANCE = 1e-12  # Decimal logarithms and sums of a few dozen cells.
# eyesim passes every density map through zapsmall(z, digits = 7), which here
# rounds each cell to 8 decimal places (at most 5e-9 away). The continuous
# oracle of the unrounded map is checked at these named tolerances: cells, and
# the entropy computed from them.
ROUNDED_CELL_TOLERANCE = 1e-8
ROUNDED_MAP_TOLERANCE = 1e-7
# fixation_entropy.fixation_group's default outdim; the call under test passes none.
DEFAULT_OUTDIM = (50, 50)
KS_SUPPORT = Decimal("3.7")  # ks::kde default `supp`, in standard deviations.
BREAKPOINT_MARGIN = Fraction(
    1, 10**6
)  # padded-lattice points must be clear of a breakpoint
BASES = {"2": Decimal(2), "e": Decimal(1).exp()}
NA = "NA"

getcontext().prec = 60


def frac(value) -> Fraction:
    return Fraction(str(value))


def dec(value) -> Decimal:
    if isinstance(value, Fraction):
        return Decimal(value.numerator) / Decimal(value.denominator)
    return Decimal(repr(value)) if isinstance(value, float) else Decimal(str(value))


def shannon(weights: list) -> dict[str, dict[str, Decimal]]:
    """Entropy of non-negative weights after sum-normalisation, by base, raw and
    relative to the log of the cell count (eyesim `normalize`)."""
    values = [dec(w) for w in weights]
    total = sum(values, Decimal(0))
    assert total > 0
    nats = -sum(((v / total) * (v / total).ln() for v in values if v > 0), Decimal(0))
    n = len(values)
    relative = nats / Decimal(n).ln() if n > 1 else Decimal(0)
    return {
        label: {"raw": nats / base.ln(), "normalized": relative}
        for label, base in BASES.items()
    }


# ---------------------------------------------------------------- grid method


def cell_along(v: Fraction, lo: Fraction, hi: Fraction, n: int) -> tuple[int, bool]:
    """findInterval(v, seq(lo, hi, length.out = n + 1), rightmost.closed = TRUE,
    all.inside = TRUE), zero-based, with whether v lay outside [lo, hi]."""
    if v < lo:
        return 0, True
    if v > hi:
        return n - 1, True
    if v == hi:
        return n - 1, False
    step = (hi - lo) / n
    return min(int((v - lo) / step), n - 1), False


def lattice_counts(xs, ys, weights, bounds, nx, ny, clamp=True):
    x0, x1, y0, y1 = bounds
    counts = [Fraction(0)] * (nx * ny)
    outside = []
    for i, (x, y, w) in enumerate(zip(xs, ys, weights)):
        ix, ox = cell_along(x, x0, x1, nx)
        iy, oy = cell_along(y, y0, y1, ny)
        if ox or oy:
            outside.append(i)
            if not clamp:
                continue
        counts[iy * nx + ix] += w
    return counts, outside


def padded_bounds(xs, ys, padding: Fraction):
    def pad(values):
        lo, hi = min(values), max(values)
        span = hi - lo if hi > lo else Fraction(1)
        return lo - padding * span, hi + padding * span

    (x0, x1), (y0, y1) = pad(xs), pad(ys)
    return x0, x1, y0, y1


def clear_of_breakpoints(values, lo, hi, n) -> bool:
    step = (hi - lo) / n
    return all(
        abs(v - (lo + k * step)) > BREAKPOINT_MARGIN
        for v in values
        for k in range(1, n)
    )


# ------------------------------------------------------------ density methods


def gaussian_weight(d: Decimal, sigma: Decimal) -> Decimal:
    return (-(d * d) / (2 * sigma * sigma)).exp()


def support_window(grid: list[Decimal], centre: Decimal, half_width: Decimal) -> range:
    """The evaluation points ks::kde visits for one observation along one axis
    (ks:::find.gridpts on the box of ks:::make.supp): from the last point at or
    below `centre - half_width` (or the first point) to the last point at or
    below `centre + half_width`, zero-based."""
    low = sum(1 for g in grid if centre - half_width >= g)
    high = sum(1 for g in grid if centre + half_width >= g)
    assert high >= 1, "every observation's support meets the lattice in this fixture"
    return range(max(low, 1) - 1, high)


def continuous_map(xs, ys, weights, sigma, xbounds, ybounds, outdim) -> list[Decimal]:
    """eyesim's density estimand under its default ks backend with fewer than
    500 points (direct, unbinned evaluation): the weighted isotropic Gaussian at
    `outdim` endpoint-inclusive points per axis, each observation contributing
    only inside its ks support box of `KS_SUPPORT` standard deviations,
    normalised to sum to one. Column-major over z[ix, iy], so cell
    k = iy * nx + ix."""
    nx, ny = outdim
    gx = [dec(xbounds[0]) + (dec(xbounds[1]) - dec(xbounds[0])) * i / (nx - 1) for i in range(nx)]
    gy = [dec(ybounds[0]) + (dec(ybounds[1]) - dec(ybounds[0])) * j / (ny - 1) for j in range(ny)]
    s = dec(sigma)
    z = [Decimal(0)] * (nx * ny)
    for px, py, w in zip(xs, ys, weights):
        for j in support_window(gy, dec(py), KS_SUPPORT * s):
            for i in support_window(gx, dec(px), KS_SUPPORT * s):
                z[j * nx + i] += (
                    dec(w) * gaussian_weight(gx[i] - dec(px), s) * gaussian_weight(gy[j] - dec(py), s)
                )
    total = sum(z, Decimal(0))
    return [v / total for v in z]


def discrete_kernel(sigma_cells: Decimal) -> list[Decimal]:
    radius = max(1, int((3 * sigma_cells).to_integral_value(rounding="ROUND_CEILING")))
    k = [gaussian_weight(Decimal(d), sigma_cells) for d in range(-radius, radius + 1)]
    total = sum(k, Decimal(0))
    return [v / total for v in k]


def native_map(
    xs, ys, weights, frame, grid, sigma, edges
) -> tuple[list[Decimal], list[int]]:
    """The eyes4s native estimate as a direct 2-D sum: each fixation inside the
    half-open frame is binned to its cell, spread by the product of the two
    normalised 1-D kernels truncated at ceil(3 sigma / cell) cells and, under
    Renormalise, divided by the share of its kernel that lands on the grid."""
    width, height = frame
    nx, ny = grid
    cw, ch = Fraction(width, nx), Fraction(height, ny)
    kx = discrete_kernel(dec(sigma) / dec(cw))
    ky = discrete_kernel(dec(sigma) / dec(ch))
    rx, ry = len(kx) // 2, len(ky) // 2

    def k2(dx, dy):
        if abs(dx) > rx or abs(dy) > ry:
            return Decimal(0)
        return kx[dx + rx] * ky[dy + ry]

    out = [Decimal(0)] * (nx * ny)
    outside = []
    for i, (x, y, w) in enumerate(zip(xs, ys, weights)):
        if not (0 <= x < width and 0 <= y < height):
            outside.append(i)
            continue
        sx, sy = int(x / cw), int(y / ch)
        retained = (
            Decimal(1)
            if edges == "Truncate"
            else sum(
                (k2(cx - sx, cy - sy) for cy in range(ny) for cx in range(nx)),
                Decimal(0),
            )
        )
        for cy in range(ny):
            for cx in range(nx):
                out[cy * nx + cx] += dec(w) * k2(cx - sx, cy - sy) / retained
    total = sum(out, Decimal(0))
    return [v / total for v in out], outside


def type7_iqr(values: list[Fraction]) -> Fraction:
    s = sorted(values)

    def q(p: Fraction) -> Fraction:
        h = (len(s) - 1) * p
        lo = int(h)
        hi = min(lo + 1, len(s) - 1)
        return s[lo] + (h - lo) * (s[hi] - s[lo])

    return q(Fraction(3, 4)) - q(Fraction(1, 4))


def suggested_sigma(xs, ys, bounds=None) -> Decimal:
    """suggest_sigma: sqrt((IQRx^2 + IQRy^2) / 2) / 1.349 * n^(-1/6), clamped to
    1-15% of the mean display span when bounds are given."""
    ix, iy = type7_iqr(xs), type7_iqr(ys)
    n = Decimal(len(xs))
    sigma = (
        (dec((ix * ix + iy * iy) / 2)).sqrt()
        / Decimal("1.349")
        * (n.ln() * Decimal(-1) / 6).exp()
    )
    if bounds is not None:
        x0, x1, y0, y1 = bounds
        scale = dec(((x1 - x0) + (y1 - y0)) / 2)
        sigma = min(max(sigma, scale * Decimal("0.01")), scale * Decimal("0.15"))
    return sigma


# ------------------------------------------------------------------- oracle


def oracle(spec: dict) -> dict:
    g = spec["fixation_group"]
    xs, ys = [frac(v) for v in g["x"]], [frac(v) for v in g["y"]]
    durations = [frac(v) for v in g["duration_ms"]]
    ones = [Fraction(1)] * len(xs)
    lat = spec["explicit_lattice"]
    nx, ny = lat["grid"]
    explicit = tuple(frac(v) for v in lat["xbounds"] + lat["ybounds"])
    explicit = (explicit[0], explicit[1], explicit[2], explicit[3])

    clamped, outside = lattice_counts(xs, ys, ones, explicit, nx, ny, clamp=True)
    excluded, _ = lattice_counts(xs, ys, ones, explicit, nx, ny, clamp=False)
    dwell, _ = lattice_counts(xs, ys, durations, explicit, nx, ny, clamp=True)
    assert outside == [5], outside
    assert clamped == [
        2,
        0,
        1,
        0,
        2,
        1,
    ], clamped  # the corner and the clamped fixation share cell 5

    padded = []
    for lattice in spec["padded_lattices"]:
        pnx, pny = lattice["grid"]
        bounds = padded_bounds(xs, ys, frac(lattice["padding"]))
        assert clear_of_breakpoints(xs, bounds[0], bounds[1], pnx)
        assert clear_of_breakpoints(ys, bounds[2], bounds[3], pny)
        counts, out = lattice_counts(xs, ys, ones, bounds, pnx, pny)
        assert out == []
        padded.append(
            {
                "grid": [pnx, pny],
                "bounds": bounds,
                "counts": counts,
                "entropy": shannon(counts),
            }
        )

    dens = spec["density"]
    frame = spec["frame"]
    grid = dens["outdim"]
    native = {}
    for weighting, weights in [("uniform", ones), ("duration", durations)]:
        for edges in ["Truncate", "Renormalise"]:
            mass, native_outside = native_map(
                xs, ys, weights, frame, grid, dens["sigma"], edges
            )
            assert native_outside == [4, 5], native_outside
            native[f"{weighting}_{edges}"] = {"mass": mass, "entropy": shannon(mass)}
    continuous = {
        weighting: continuous_map(
            xs, ys, weights, dens["sigma"], dens["xbounds"], dens["ybounds"], grid
        )
        for weighting, weights in [
            ("unweighted", ones),
            ("duration_weighted", durations),
        ]
    }

    ms = spec["multiscale"]
    native_scales = {}
    for s in sorted(ms["sigmas"]):
        mass, _ = native_map(xs, ys, ones, frame, grid, s, "Truncate")
        native_scales[str(s)] = shannon(mass)
    ms_weights = {str(s): frac(w) for s, w in ms["scale_weights"]}

    def weighted(per_scale: dict, weights: dict, kind: str, base: str) -> Decimal:
        total = sum((dec(w) for w in weights.values()), Decimal(0))
        return (
            sum(
                (dec(weights[s]) * per_scale[s][base][kind] for s in weights),
                Decimal(0),
            )
            / total
        )

    sup = spec["supplied_scales"]
    sup_entropy = {
        str(s): shannon([frac(v) for v in m])
        for s, m in zip(sup["sigmas"], sup["maps"])
    }
    sup_weights = {str(s): frac(w) for s, w in sup["scale_weights"]}

    def mean(per_scale: dict, kind: str, base: str) -> Decimal:
        return sum((e[base][kind] for e in per_scale.values()), Decimal(0)) / len(
            per_scale
        )

    kinds = ["raw", "normalized"]
    return {
        "explicit_bounds": explicit,
        "explicit_clamped_counts": clamped,
        "explicit_excluded_counts": excluded,
        "explicit_duration_counts": dwell,
        "explicit_outside": outside,
        "grid_explicit": shannon(clamped),
        "grid_explicit_excluded": shannon(excluded),
        "grid_explicit_duration": shannon(dwell),
        "grid_padded": padded,
        "native_density": native,
        "continuous_density": continuous,
        "suggested_sigma_unclamped": suggested_sigma(xs, ys),
        "suggested_sigma_padded": suggested_sigma(
            xs, ys, padded_bounds(xs, ys, frac(spec["padded_lattices"][0]["padding"]))
        ),
        # fixation_entropy(fg, method = "density") with every default: the
        # clamped suggestion over the padded bounds at the default outdim.
        "continuous_density_default": shannon(
            continuous_map(
                xs,
                ys,
                ones,
                suggested_sigma(xs, ys, padded_bounds(xs, ys, frac(spec["padded_lattices"][0]["padding"]))),
                padded_bounds(xs, ys, frac(spec["padded_lattices"][0]["padding"]))[0:2],
                padded_bounds(xs, ys, frac(spec["padded_lattices"][0]["padding"]))[2:4],
                DEFAULT_OUTDIM,
            )
        )["e"]["normalized"],
        "native_multiscale": native_scales,
        "native_multiscale_mean": {
            b: {k: mean(native_scales, k, b) for k in kinds} for b in BASES
        },
        "native_multiscale_weighted": {
            b: {k: weighted(native_scales, ms_weights, k, b) for k in kinds}
            for b in BASES
        },
        "supplied": sup_entropy,
        "supplied_mean": {
            b: {k: mean(sup_entropy, k, b) for k in kinds} for b in BASES
        },
        "supplied_weighted": {
            b: {k: weighted(sup_entropy, sup_weights, k, b) for k in kinds}
            for b in BASES
        },
    }


# --------------------------------------------------------------- verification


def close(
    r_value: dict, target: Decimal | str, where, tolerance: float = TOLERANCE
) -> None:
    if isinstance(target, str):
        assert r_value["kind"] == target, (where, r_value, target)
    else:
        assert r_value["kind"] == "finite", (where, r_value, target)
        assert abs(Decimal(repr(r_value["value"])) - target) <= Decimal(
            repr(tolerance)
        ), (
            where,
            r_value,
            target,
        )


def close_grid(
    reference: dict, exact: dict, where, tolerance: float = TOLERANCE
) -> None:
    for base in BASES:
        for kind in ["raw", "normalized"]:
            close(
                reference[base][kind], exact[base][kind], (where, base, kind), tolerance
            )


def named(reference: dict) -> dict[str, dict]:
    assert reference["kind"] == "vector", reference
    return dict(zip(reference["names"], reference["values"]))


def verify(reference: dict, exact: dict, spec: dict) -> None:
    grid = reference["grid"]
    close_grid(grid["explicit"], exact["grid_explicit"], "grid explicit")
    assert (
        grid["explicit_duration_weighted"] == grid["explicit"]
    ), "the grid method ignores durations"
    assert len(grid["padded"]) == len(exact["grid_padded"])
    for i, (r, e) in enumerate(zip(grid["padded"], exact["grid_padded"])):
        close_grid(r, e["entropy"], ("grid padded", i))
    for base in BASES:
        for kind in ["raw", "normalized"]:
            close(grid["single"][base][kind], Decimal(0), ("single grid", base, kind))

    for weighting in ["unweighted", "duration_weighted"]:
        d = reference["density"][weighting]
        z = d["map"]["z"]
        nx, ny = spec["density"]["outdim"]
        assert len(z) == nx * ny and d["map"]["sigma"] == spec["density"]["sigma"]
        assert d["map"]["x"][0] == spec["density"]["xbounds"][0]
        assert (
            d["map"]["x"][-1] == spec["density"]["xbounds"][1]
        ), "evaluation points include both ends"
        close_grid(
            d["entropy"], shannon(z), ("density entropy of eyesim's map", weighting)
        )
        close_grid(
            d["entropy"],
            shannon(exact["continuous_density"][weighting]),
            ("density against the continuous oracle", weighting),
            ROUNDED_MAP_TOLERANCE,
        )
        for cell, target in zip(z, exact["continuous_density"][weighting]):
            assert abs(Decimal(repr(cell)) - target) <= Decimal(repr(ROUNDED_CELL_TOLERANCE)), (
                weighting,
                cell,
                target,
            )
    dd = reference["density_default"]
    close(
        dd["suggested_sigma_unclamped"],
        exact["suggested_sigma_unclamped"],
        "suggest_sigma",
    )
    close(
        dd["suggested_sigma_padded"],
        exact["suggested_sigma_padded"],
        "suggest_sigma padded",
    )
    close(
        dd["entropy"],
        exact["continuous_density_default"],
        "default density entropy against the continuous oracle",
        ROUNDED_MAP_TOLERANCE,
    )
    assert dd["single_default"] == {"kind": NA} and dd["single_explicit_sigma"] == {
        "kind": NA
    }

    ms = reference["multiscale"]
    assert ms["class"] == ["eye_density_multiscale", "list"]
    assert ms["sigmas_vector"] == spec["multiscale"]["sigmas"], "request order is kept"
    per_scale = {str(m["sigma"]): shannon(m["z"]) for m in ms["maps"]}
    for base in BASES:
        for kind in ["raw", "normalized"]:
            none = named(ms["none"][base][kind])
            assert list(none) == [f"sigma_{s}" for s in spec["multiscale"]["sigmas"]]
            for s, value in none.items():
                close(
                    value,
                    per_scale[s.removeprefix("sigma_")][base][kind],
                    ("ms none", s),
                )
            target = sum((e[base][kind] for e in per_scale.values()), Decimal(0)) / len(
                per_scale
            )
            close(ms["mean"][base][kind], target, ("ms mean", base, kind))
            assert (
                ms["from_group"][base][kind] == ms["mean"][base][kind]
            ), "a sigma vector gives the mean"
            assert (
                ms["from_group_none"][base][kind] == ms["none"][base][kind]
            ), "aggregate = none from a fixation group gives every scale"

    sup = reference["supplied_scales"]
    zero = f"sigma_{spec['supplied_scales']['zero_sigma']}"
    for base in BASES:
        for kind in ["raw", "normalized"]:
            none = named(sup["none"][base][kind])
            assert list(none) == [
                f"sigma_{s}" for s in spec["supplied_scales"]["sigmas"]
            ]
            for s, value in none.items():
                close(
                    value,
                    exact["supplied"][s.removeprefix("sigma_")][base][kind],
                    ("sup", s),
                )
            close(
                sup["mean"][base][kind],
                exact["supplied_mean"][base][kind],
                ("sup mean", base),
            )
            with_zero = named(sup["zero_none"][base][kind])
            assert with_zero[zero] == {"kind": NA}
            assert {k: v for k, v in with_zero.items() if k != zero} == none
            close(
                sup["zero_mean"][base][kind],
                exact["supplied_mean"][base][kind],
                "NA scale dropped",
            )


# ------------------------------------------------------------------- output


def run_r(eyesim: Path) -> dict:
    # The pinned archive in the locked R session; never an arbitrary installed eyesim package.
    with parity.r_session("eyes4s-r-derived-entropy-", eyesim) as r:
        output = r.tmp / "derived-entropy.json"
        r.rscript(HERE / "derived_entropy.R", r.eyesim_library, INPUT, output)
        return json.loads(output.read_text())


def scala(exact: dict, reference: dict, spec: dict) -> str:
    header = (
        (ROOT / "surface/src/main/scala/eyes4s/surface/Pyramid.scala")
        .read_text()
        .split("package ")[0]
    )

    def rv(cell: dict) -> str:
        kind = cell["kind"]
        if kind == "finite":
            return f"RValue.Finite({float(cell['value'])!r})"
        if kind == "error":
            return f"RValue.Error({json.dumps(cell['detail'])})"
        return {
            "NA": "RValue.NA",
            "NaN": "RValue.NaN",
            "Inf": "RValue.Inf",
            "-Inf": "RValue.NegInf",
        }[kind]

    def eyesim_entropy(e: dict) -> str:
        return (
            f"EyesimEntropy({rv(e['2']['raw'])}, {rv(e['2']['normalized'])}, "
            f"{rv(e['e']['raw'])}, {rv(e['e']['normalized'])})"
        )

    def exact_entropy(e: dict) -> str:
        return (
            f"ExactEntropy({float(e['2']['raw'])!r}, {float(e['2']['normalized'])!r}, "
            f"{float(e['e']['raw'])!r}, {float(e['e']['normalized'])!r})"
        )

    def doubles(values) -> str:
        return "Vector(" + ", ".join(repr(float(v)) for v in values) + ")"

    def labelled(reference_grid: dict) -> str:
        # Per-scale eyesim values, by sigma label, in eyesim's order.
        rows = []
        for base in ["2", "e"]:
            for kind in ["raw", "normalized"]:
                items = named(reference_grid[base][kind])
                rows.append(
                    "Vector("
                    + ", ".join(
                        f"({float(k.removeprefix('sigma_'))!r}, {rv(v)})"
                        for k, v in items.items()
                    )
                    + ")"
                )
        return f"ScaleValues({', '.join(rows)})"

    g = spec["fixation_group"]
    dens = spec["density"]
    ms = reference["multiscale"]
    sup = spec["supplied_scales"]
    lines = [
        header.rstrip(),
        "",
        "package eyes4s.surface",
        "",
        "// Generated by tools/r-parity/generate_derived_entropy.py. Do not hand-edit.",
        "// format: off",
        "object DerivedEntropyReference:",
        f'  val eyesimRevision = "{REVISION}"',
        f'  val inputSha256 = "{hashlib.sha256(INPUT.read_bytes()).hexdigest()}"',
        f"  val tolerance = {TOLERANCE!r}",
        "  /** eyesim rounds each density map (zapsmall to 7 digits); the entropy of a continuous oracle of the unrounded map agrees to this. */",
        f"  val roundedMapTolerance = {ROUNDED_MAP_TOLERANCE!r}",
        "  /** A value as R reports it. NA is not a Double, so it is a case of its own; Error is a refusal and its message. */",
        "  enum RValue derives CanEqual:",
        "    case Finite(value: Double)",
        "    case NA",
        "    case NaN",
        "    case Inf",
        "    case NegInf",
        "    case Error(message: String)",
        "  /** eyesim entropy output: base 2 raw and normalised, then base e raw and normalised. */",
        "  final case class EyesimEntropy(bits: RValue, relativeBits: RValue, nats: RValue, relativeNats: RValue)",
        "  final case class ExactEntropy(bits: Double, relativeBits: Double, nats: Double, relativeNats: Double)",
        '  /** eyesim aggregate = "none" output, (sigma, value) in eyesim\'s order, per base and normalisation. */',
        "  final case class ScaleValues(bits: Vector[(Double, RValue)], relativeBits: Vector[(Double, RValue)], nats: Vector[(Double, RValue)], relativeNats: Vector[(Double, RValue)])",
        "  final case class PaddedLattice(nx: Int, ny: Int, padding: Double, xMin: Double, xMax: Double, yMin: Double, yMax: Double, counts: Vector[Double], eyesim: EyesimEntropy, exact: ExactEntropy)",
        "",
        "  // The fixation group, in the frame [0, width) x [0, height).",
        f"  val frameWidth = {spec['frame'][0]}",
        f"  val frameHeight = {spec['frame'][1]}",
        f"  val x = {doubles(g['x'])}",
        f"  val y = {doubles(g['y'])}",
        f"  val onsetMs = Vector({', '.join(str(v) for v in g['onset_ms'])})",
        f"  val durationMs = Vector({', '.join(str(v) for v in g['duration_ms'])})",
        "",
        '  // method = "grid" on explicit bounds.',
        f"  val latticeNx = {spec['explicit_lattice']['grid'][0]}",
        f"  val latticeNy = {spec['explicit_lattice']['grid'][1]}",
        f"  val latticeBounds = {doubles(exact['explicit_bounds'])}",
        f"  val clampedCounts = {doubles(exact['explicit_clamped_counts'])}",
        f"  val excludedCounts = {doubles(exact['explicit_excluded_counts'])}",
        f"  val durationCounts = {doubles(exact['explicit_duration_counts'])}",
        f"  val outsideLattice = Vector({', '.join(str(i) for i in exact['explicit_outside'])})",
        f"  val eyesimGridExplicit = {eyesim_entropy(reference['grid']['explicit'])}",
        f"  val eyesimGridExplicitDurationWeighted = {eyesim_entropy(reference['grid']['explicit_duration_weighted'])}",
        f"  val exactGridExplicit = {exact_entropy(exact['grid_explicit'])}",
        "  // eyes4s-only: the outside fixation excluded, and dwell in place of counts.",
        f"  val exactGridExcluded = {exact_entropy(exact['grid_explicit_excluded'])}",
        f"  val exactGridDuration = {exact_entropy(exact['grid_explicit_duration'])}",
        "  // eyesim's default bounds: the observed range padded by 5% of its span.",
        "  val paddedLattices = Vector(",
    ]
    for lattice, ref, sp in zip(
        exact["grid_padded"], reference["grid"]["padded"], spec["padded_lattices"]
    ):
        b = lattice["bounds"]
        lines.append(
            f"    PaddedLattice({lattice['grid'][0]}, {lattice['grid'][1]}, {float(sp['padding'])!r}, "
            f"{float(b[0])!r}, {float(b[1])!r}, {float(b[2])!r}, {float(b[3])!r}, "
            f"{doubles(lattice['counts'])}, {eyesim_entropy(ref)}, {exact_entropy(lattice['entropy'])}),"
        )
    lines += [
        "  )",
        f"  val eyesimGridSingle = {eyesim_entropy(reference['grid']['single'])}",
        "",
        '  // method = "density": explicit sigma, bounds and output lattice.',
        f"  val sigma = {float(dens['sigma'])!r}",
        f"  val densityNx = {dens['outdim'][0]}",
        f"  val densityNy = {dens['outdim'][1]}",
        "  // eyesim's own maps (eye_density with the same arguments), cell k = iy * nx + ix.",
        f"  val eyesimDensityMap = {doubles(reference['density']['unweighted']['map']['z'])}",
        f"  val eyesimDensityMapDurationWeighted = {doubles(reference['density']['duration_weighted']['map']['z'])}",
        f"  val eyesimDensityX = {doubles(reference['density']['unweighted']['map']['x'])}",
        f"  val eyesimDensityY = {doubles(reference['density']['unweighted']['map']['y'])}",
        f"  val eyesimDensity = {eyesim_entropy(reference['density']['unweighted']['entropy'])}",
        f"  val eyesimDensityDurationWeighted = {eyesim_entropy(reference['density']['duration_weighted']['entropy'])}",
        "  // The continuous Gaussian at eyesim's evaluation points, unrounded.",
        f"  val continuousDensity = {exact_entropy(shannon(exact['continuous_density']['unweighted']))}",
        f"  val continuousDensityDurationWeighted = {exact_entropy(shannon(exact['continuous_density']['duration_weighted']))}",
        "  // The eyes4s native discrete estimate, by weighting and edge policy.",
    ]
    for key in [
        "uniform_Truncate",
        "uniform_Renormalise",
        "duration_Truncate",
        "duration_Renormalise",
    ]:
        name = "native" + "".join(part.capitalize() for part in key.split("_"))
        lines.append(
            f"  val {name} = {exact_entropy(exact['native_density'][key]['entropy'])}"
        )
        lines.append(
            f"  val {name}Mass = {doubles(exact['native_density'][key]['mass'])}"
        )
    dd = reference["density_default"]
    lines += [
        "  // Default bandwidth: suggest_sigma with and without eyesim's padded display clamp.",
        f"  val eyesimDensityDefault = {rv(dd['entropy'])}",
        "  /** The continuous oracle of that default call: sigma clamped on the padded range, padded bounds, 50 by 50, relative nats. */",
        f"  val continuousDensityDefault = {float(exact['continuous_density_default'])!r}",
        f"  val eyesimSuggestedSigmaUnclamped = {rv(dd['suggested_sigma_unclamped'])}",
        f"  val eyesimSuggestedSigmaPadded = {rv(dd['suggested_sigma_padded'])}",
        f"  val eyesimSingleDensityDefault = {rv(dd['single_default'])}",
        f"  val eyesimSingleDensityExplicitSigma = {rv(dd['single_explicit_sigma'])}",
        "",
        "  // eye_density with a sigma vector, requested in this order.",
        f"  val multiscaleSigmas = {doubles(spec['multiscale']['sigmas'])}",
        "  val eyesimMultiscaleMaps = Vector(",
    ]
    for m in ms["maps"]:
        lines.append(f"    ({float(m['sigma'])!r}, {doubles(m['z'])}),")
    lines += [
        "  )",
        f"  val eyesimMultiscaleNone = {labelled(ms['none'])}",
        f"  val eyesimMultiscaleMean = {eyesim_entropy(ms['mean'])}",
        f"  val eyesimMultiscaleFromGroup = {eyesim_entropy(ms['from_group'])}",
        f"  val eyesimMultiscaleFromGroupNone = {labelled(ms['from_group_none'])}",
        "  val nativeMultiscale = Vector(",
    ]
    for s, e in exact["native_multiscale"].items():
        lines.append(f"    ({float(s)!r}, {exact_entropy(e)}),")
    lines += [
        "  )",
        f"  val nativeMultiscaleMean = {exact_entropy(exact['native_multiscale_mean'])}",
        "  /** (sigma, weight): an eyes4s reduction eyesim does not offer. */",
        f"  val multiscaleWeights = Vector({', '.join(f'({float(s)!r}, {float(w)!r})' for s, w in spec['multiscale']['scale_weights'])})",
        f"  val nativeMultiscaleWeighted = {exact_entropy(exact['native_multiscale_weighted'])}",
        "",
        "  // Supplied maps on a two-by-two lattice packaged as one multiscale object.",
        f"  val suppliedWidth = {float(sup['lattice']['frame'][0])!r}",
        f"  val suppliedHeight = {float(sup['lattice']['frame'][1])!r}",
        f"  val suppliedSigmas = {doubles(sup['sigmas'])}",
        "  val suppliedMaps = Vector(",
    ]
    for m in sup["maps"]:
        lines.append(f"    {doubles(m)},")
    lines += [
        "  )",
        f"  val zeroSigma = {float(sup['zero_sigma'])!r}",
        f"  val eyesimSuppliedNone = {labelled(reference['supplied_scales']['none'])}",
        f"  val eyesimSuppliedMean = {eyesim_entropy(reference['supplied_scales']['mean'])}",
        f"  val eyesimSuppliedWithZeroNone = {labelled(reference['supplied_scales']['zero_none'])}",
        f"  val eyesimSuppliedWithZeroMean = {eyesim_entropy(reference['supplied_scales']['zero_mean'])}",
        "  val exactSupplied = Vector(",
    ]
    for s, e in exact["supplied"].items():
        lines.append(f"    ({float(s)!r}, {exact_entropy(e)}),")
    lines += [
        "  )",
        f"  val exactSuppliedMean = {exact_entropy(exact['supplied_mean'])}",
        f"  val suppliedWeights = Vector({', '.join(f'({float(s)!r}, {float(w)!r})' for s, w in sup['scale_weights'])})",
        f"  val exactSuppliedWeighted = {exact_entropy(exact['supplied_weighted'])}",
        "// format: on",
        "",
    ]
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
    spec = json.loads(INPUT.read_text())
    exact = oracle(spec)
    reference = run_r(args.eyesim)
    verify(reference, exact, spec)
    payload = {
        "eyesim_revision": REVISION,
        "input_sha256": hashlib.sha256(INPUT.read_bytes()).hexdigest(),
        "input_license": "Apache-2.0; synthetic data authored for eyes4s",
        "oracle": (
            "Exact rational lattice counts and 60-digit decimal entropies for the grid method; the "
            "decimal entropy of eyesim's own pinned density maps; a continuous isotropic Gaussian at "
            "eyesim's endpoint-inclusive evaluation points, checked at the rounding tolerance of "
            "eyesim's zapsmall; a direct two-dimensional discrete-kernel sum for the eyes4s native "
            "estimate; per-scale entropies, their mean and an explicitly weighted mean for the "
            "multiscale reductions. NA values and errors are pinned as eyesim literals."
        ),
        "map_order": "z is the nx-by-ny matrix z[ix, iy]; as.numeric(z) is column-major, so cell k = iy * nx + ix, the eyes4s Grid index",
        "numerical_tolerance": {
            "absolute": TOLERANCE,
            "rounded_map_absolute": ROUNDED_MAP_TOLERANCE,
            "relative": 0,
        },
        "exact": jsonable(exact),
        "eyesim": {k: v for k, v in reference.items() if k != "runtime"},
        "runtime": reference["runtime"],
    }
    files = {
        JSON_OUT: json.dumps(payload, indent=2) + "\n",
        SCALA: scala(exact, reference, spec),
    }
    for path, content in files.items():
        if args.check:
            if not path.exists() or path.read_text() != content:
                raise RuntimeError(f"Reference drift: {path.relative_to(ROOT)}")
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(content)
    print(
        f'{"Checked" if args.check else "Generated"} {len(files)} artifacts: grid, density and '
        "multiscale fixation_entropy in two bases; oracle agrees with eyesim."
    )


if __name__ == "__main__":
    main()

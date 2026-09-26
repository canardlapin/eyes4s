# OLS over mass surfaces

`SurfaceDecomposition.ols(response, predictors, intercept, rankTolerance)` fits one
response `Mass[U]` from a checked `PredictorSet[U]`. Each predictor has a nonblank,
nominal `PredictorId`; duplicate IDs and empty sets fail. `Agreement.grids` checks
predictor and response grids, including nominal identity and geometry. Rows are
cells; this is a descriptive map fit, not population inference over participants.

Construct predictors with `PredictorSet.of(Vector(idA -> mapA, idB -> mapB))`.
`Intercept.Include` adds a constant column; `Intercept.Exclude` does not. The result
retains keyed coefficients in input order, an optional intercept, a `Signed` fitted
surface and `Signed` residual. Neither result map is normalized or clipped.
Predictor ordering, input provenance, intercept and rank threshold enter provenance.

The native `LeastSquares.fit(rows, response, tolerance)` kernel is reusable by
future training-only template fitting. It scales columns to unit Euclidean norm,
uses Householder QR, then solves the triangular system and restores coefficient
units. It never forms floating-point normal equations or silently drops a column.
The baseline requires rows >= columns and full column rank. Zero or dependent
columns fail with their index, observed pivot and threshold. The checked
`RelativeRankTolerance` defaults to 1e-7 on scaled columns, the threshold `lm` uses
and the one the imported template path already assumed; this is an explicit numerical
rank policy. A tighter tolerance must be requested explicitly: at 1e-12 a design with
`x2 = x1 ± 1e-10` and a nonzero residual returns coefficients near ±3e9 that are
rounding noise. Nonfinite inputs and arithmetic failures are named values, and an
arithmetic failure names only the column or the row it concerns.

Diagnostics report rank, residual sum of squares and the ratio of extreme absolute
scaled R diagonals. The latter is a conditioning warning proxy, **not a condition
number**. R-squared is centered with an intercept and uncentered without one,
matching `summary.lm`; a zero reference sum of squares is `None`. There are no
coefficient standard errors or p-values: spatial cells are not independent replicates.

The per-ticket fixture calls pinned eyesim `template_multireg(method="lm")` with
both intercept policies and `template_regression(method="lm")` for its implicit
intercept convention. `template_multireg` normalizes input maps; `template_regression`
is supplied already normalized maps here. Exact rational normal equations solved
by Gauss-Jordan independently check coefficients, fitted values, residuals and
R-squared. `SurfaceDecompositionSuite` consumes the resulting Scala fixtures on
JVM/JS, checks deficient/ill-conditioned and widely scaled inputs, grid/key failures,
and verifies that fitted maps cannot typecheck as `Mass`.

Regenerate the offline reference with:

```sh
python3 tools/r-parity/generate_decomposition.py --eyesim /path/to/eyesim --check
```

No R is needed by the public API or Scala tests. This slice does not replace the
existing template workflow automatically, construct learned features, provide
NNLS/simplex solvers, or implement partial association. Those retain their own tickets.

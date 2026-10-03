# Surface decomposition and partial association

## OLS over mass surfaces

`Template.decompose(response, predictors, intercept, rankTolerance)`, the template family's
decomposition entry point, fits one
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
existing template workflow automatically or construct learned features.

## Constrained fits: NNLS and simplex mixtures

Each solver's result type states the representation it proves (PRD X-13):

| Entry point | Constraint | Fit | Coefficients |
|---|---|---|---|
| `Template.decompose` | none | `Signed[U]` | signed slopes, optional intercept |
| `Template.decomposeNonNegative` | coefficients >= 0, no intercept | `Intensity[U]` | non-negative scale factors, **not** mixture weights |
| `Template.decomposeMixture` | coefficients >= 0 summing to one | `Mass[U]` | mixture weights |

Every residual is `Signed[U]`. NNLS is intercept-free by construction: a free intercept
could make the fit negative, and an `Intensity` it would no longer be. Its coefficients
need not sum to one, so they are not mixture weights. Only `decomposeMixture` reports
weights. Its intercept policy is explicit and changes the estimand: `Intercept.Exclude`
mixes the predictor maps alone, while `Intercept.Include` adds a uniform mass over the grid
as one more mixture component, reported as `background`. The weights and the background
then sum to one, and the fit is still a `Mass`. A free additive intercept is not offered,
because it would break the sum-to-one and non-negativity guarantees that make the fit a mass.

`ConstrainedLeastSquares` implements both solvers as active-set methods after Lawson and
Hanson (1974, ch. 23). Every active subproblem is solved by the shared native
`LeastSquares.fit` Householder QR. The whole design must first have full column rank under
the `RelativeRankTolerance`, so the solution is unique; zero or dependent columns fail as
`DecompositionError.Solve` with the `LeastSquaresError.RankDeficient` column. The simplex
solver enforces the sum exactly by eliminating one active coefficient through a reference
column, rather than appending a heavily weighted sum-to-one row to NNLS, whose result
depends on the weight and satisfies the sum only approximately. It starts from the single
closest predictor map and compares each excluded predictor's gradient with the equality
multiplier, not with zero.

A predictor outside the active set enters only while its gradient exceeds
`RelativeDualTolerance` (default `1e-10`) times its Euclidean norm times the response's.
A predictor whose own coefficient is not positive on the step that admitted it is set aside
until the active set next changes, which prevents cycling on rounding. The solver stops
after `ConstrainedLeastSquares.iterationLimit(p) = 30p` steps with
`LeastSquaresError.NotConverged`; Lawson-Hanson terminates in finitely many steps, so this
limit is a safety net.

`ConstrainedDiagnostics` is descriptive: checked design rank, active predictor count,
iterations, the largest relative KKT violation left at termination, residual sum of squares,
the scaled-diagonal conditioning proxy, and R-squared with its `RSquaredReference`. NNLS uses
the uncentered reference because it has no intercept. Mixtures use the centered reference,
because a mixture fit and its response are both masses with the same mean. A constrained
R-squared can be negative. As for OLS, there are no coefficient standard errors or p-values.

## Partial association

`PartialAssociation.of(x, y, covariates, method)` is a separate operation and result, not a
coefficient (PRD X-15). It removes from `x` and from `y` their least-squares fit on an
intercept and the covariate maps, then correlates the residuals. `AssociationMethod.Pearson`
uses the cell values. `AssociationMethod.Spearman` first ranks `x`, `y` and every covariate,
giving tied cells their average rank, as R's `rank` does. The `estimate` is `None` when either
residual's norm is at most the `RelativeRankTolerance` times that map's centered norm. This
covers a constant map and a map that the covariates explain, which have no partial association
and would otherwise return a correlation of rounding noise. The result has no p-value,
records its method and covariate keys, and carries provenance over every input.

## Evidence

`ConstrainedDecompositionSuite` runs on the JVM and Scala.js against exact rational oracles in
`ExactDecompositionOracle`, which share no code with the floating-point paths. For NNLS and
the simplex, the oracle enumerates every active set and solves its first-order conditions by exact
Gauss-Jordan elimination. It then returns the unique set satisfying every KKT condition. Generated
full-rank designs with one to four predictors check coefficients, fitted maps, residuals,
R-squared, active counts and the representation. The simplex is checked with and without a background.
The partial-association oracle uses the inverse covariance matrix, the route `ppcor` takes, rather
than residuals; ranks are exact average ranks. Further properties check invariance to predictor
order, the ordering `RSS(OLS) <= RSS(NNLS) <= RSS(simplex)`, and symmetry in `x` and `y`. Named
examples cover exact mixture recovery, a zeroed negative OLS slope and a multiplier-sensitive
simplex path. They also cover undefined associations, average tie ranks, rank, grid and tolerance
refusals, and `typeCheckErrors` proofs that an NNLS fit is not a `Mass` while a mixture fit is.

The suite killed six source mutants: dropping the simplex multiplier, changing the sign of the
eliminated weight, coarsening the KKT threshold, dropping the association intercept,
first-rank ties and treating a constant map as defined. A seventh mutant, a full step to the
subproblem solution in place of the Lawson-Hanson interpolation, changes only the solver's
path: the outer KKT loop readmits any predictor removed early, so its results are the same.

eyesim's `template_multireg(method = "nnls")` fits the same intercept-free NNLS on normalized
maps; it ignores its `intercept` argument. `template_regression(method = "rank")` returns `ppcor`
partial Spearman correlations of the source map with each of the baseline and reference maps
given the other, labelled as beta weights. In eyes4s these are
`PartialAssociation.of(source, baseline, given reference, Spearman)` and the symmetric call.
Pinned eyesim fixtures for these two methods are not yet recorded: `nnls` and `ppcor` are not in
`tools/r-parity/r-lock.json`. The equivalence is a stated convention, not measured parity.

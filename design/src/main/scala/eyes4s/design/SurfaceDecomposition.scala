/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eyes4s.design

import cats.syntax.all.*
import eyes4s.kernel.*

final class PredictorId private (val value: String) derives CanEqual:
  override def equals(other: Any): Boolean = other match
    case that: PredictorId => value == that.value
    case _                 => false
  override def hashCode: Int    = value.hashCode
  override def toString: String = value
object PredictorId:
  def of(value: String): Either[DecompositionError, PredictorId] =
    Either.cond(
      value.trim.nonEmpty,
      new PredictorId(value),
      DecompositionError.PredictorName(value)
    )

enum Intercept derives CanEqual:
  case Include, Exclude

enum DecompositionError derives CanEqual:
  case PredictorName(value: String)
  case PredictorKeys(keys: Vector[PredictorId])
  case Geometry(operand: String, underlying: SurfaceError)
  case Solve(keys: Vector[PredictorId], intercept: Intercept, underlying: LeastSquaresError)
  case Numerical(operation: String, value: Double)
  def message: String = this match
    case PredictorName(v)  => s"Predictor ID '$v' must be nonblank."
    case PredictorKeys(ks) => s"Predictor set keys=$ks must be nonempty and unique."
    case Geometry(k, e)    => s"Surface decomposition operand=$k: ${e.message}"
    case Solve(ks, i, e)   =>
      s"Surface decomposition predictors=$ks intercept=$i: ${e.message}${designColumn(ks, i, e)}"
    case Numerical(op, v) => s"Surface decomposition $op produced nonfinite value=$v."

  /** A least-squares column indexes the whole design, which with
    * [[Intercept.Include]] has the intercept (or a mixture's uniform
    * background) at column 0 and predictor k at column k + 1.
    */
  private def designColumn(
      keys: Vector[PredictorId],
      intercept: Intercept,
      underlying: LeastSquaresError
  ): String =
    val column = underlying match
      case LeastSquaresError.RankDeficient(c, _, _) => Some(c)
      case LeastSquaresError.ColumnArithmetic(_, c) => Some(c)
      case LeastSquaresError.Stalled(c, _, _)       => Some(c)
      case _                                        => None
    val offset = if intercept == Intercept.Include then 1 else 0
    column.fold("") { c =>
      if c < offset then s" Design column=$c is the intercept or uniform background."
      else keys.lift(c - offset).fold("")(k => s" Design column=$c is predictor=$k.")
    }

final class PredictorSet[U <: Unit2D] private (
    val entries: Vector[(PredictorId, Mass[U])],
    val grid: Grid[U]
)
object PredictorSet:
  def of[U <: Unit2D](
      entries: Vector[(PredictorId, Mass[U])]
  ): Either[DecompositionError, PredictorSet[U]] =
    val keys = entries.map(_._1)
    if keys.isEmpty || keys.distinct.size != keys.size then
      Left(DecompositionError.PredictorKeys(keys))
    else
      val grid = entries.head._2.grid
      entries
        .traverse_ { case (key, mass) =>
          Agreement
            .grids(grid, mass.grid)
            .left
            .map(e => DecompositionError.Geometry(key.value, e))
        }
        .map(_ => new PredictorSet(entries, grid))

/** Descriptive over cells only; no independence or coefficient-inference claim.
  * R-squared is centered with an intercept and uncentered without, as in lm.
  * A zero reference sum of squares is explicitly undefined.
  */
final case class OlsDiagnostics(
    rank: Int,
    residualSumSquares: Double,
    rSquared: Option[Double],
    scaledDiagonalRatio: Double
)
final class SurfaceOlsFit[U <: Unit2D] private[design] (
    val coefficients: Vector[(PredictorId, Double)],
    val intercept: Option[Double],
    val fitted: Signed[U],
    val residual: Signed[U],
    val diagnostics: OlsDiagnostics
)

/** Which reference sum of squares an R-squared divides by. */
enum RSquaredReference derives CanEqual:
  /** Deviations from the response mean, as `lm` with an intercept. */
  case Centered

  /** The response's raw sum of squares, as `lm` without an intercept. */
  case Uncentered

/** Descriptive diagnostics of an active-set surface fit: no coefficient
  * standard errors or p-values, because grid cells are not independent
  * replicates. `rank` is the checked full column rank of the whole design,
  * `active` the number of predictors with a positive coefficient, and
  * `dualViolation` the largest relative KKT violation left at termination.
  * `rSquared` is `None` when its reference sum of squares is zero, and under a
  * constraint it may be negative.
  */
final case class ConstrainedDiagnostics(
    rank: Int,
    active: Int,
    iterations: Int,
    dualViolation: Double,
    residualSumSquares: Double,
    rSquared: Option[Double],
    rSquaredReference: RSquaredReference,
    scaledDiagonalRatio: Double
)

/** An intercept-free non-negative least-squares fit. Coefficients are
  * non-negative scale factors, not mixture weights: they need not sum to one,
  * so the fit is an [[Intensity]], not a [[Mass]].
  */
final class SurfaceNnlsFit[U <: Unit2D] private[design] (
    val coefficients: Vector[(PredictorId, Double)],
    val fitted: Intensity[U],
    val residual: Signed[U],
    val diagnostics: ConstrainedDiagnostics
)

/** A simplex-constrained fit: non-negative weights summing to one, so the fit
  * is itself a [[Mass]] and the weights are mixture weights. With
  * [[Intercept.Include]] a uniform mass over the grid joins the mixture and
  * `background` is its weight; `weights` and `background` then sum to one.
  */
final class SurfaceMixtureFit[U <: Unit2D] private[design] (
    val weights: Vector[(PredictorId, Double)],
    val background: Option[Double],
    val fitted: Mass[U],
    val residual: Signed[U],
    val diagnostics: ConstrainedDiagnostics
)

private[design] object SurfaceDecomposition:
  def ols[U <: Unit2D](
      response: Mass[U],
      predictors: PredictorSet[U],
      intercept: Intercept,
      rankTolerance: RelativeRankTolerance = RelativeRankTolerance.default
  ): Either[DecompositionError, SurfaceOlsFit[U]] =
    for
      grid <- Agreement
        .grids(predictors.grid, response.grid)
        .left
        .map(e => DecompositionError.Geometry("response", e))
      rows = Vector.tabulate(response.size) { i =>
        val xs = predictors.entries.map(_._2.values(i))
        if intercept == Intercept.Include then 1.0 +: xs else xs
      }
      y = response.values.toVector
      fit <- LeastSquares
        .fit(rows, y, rankTolerance)
        .left
        .map(e => DecompositionError.Solve(predictors.entries.map(_._1), intercept, e))
      rss  = fit.residuals.foldLeft(0.0)((s, r) => s + r * r)
      mean =
        if intercept == Intercept.Include then
          y.head + y.foldLeft(0.0)((s, v) => s + (v - y.head)) / y.size
        else 0.0
      total = y.foldLeft(0.0)((s, v) => s + (v - mean) * (v - mean))
      r2    = Option.when(total > 0.0)(1.0 - rss / total)
      _ <- Either.cond(
        rss.isFinite && total.isFinite && r2.forall(_.isFinite),
        (),
        DecompositionError.Numerical("fit diagnostics", r2.getOrElse(rss))
      )
      inputs = ContentHash.combineAll(response.provenance.digest +: predictors.entries.flatMap {
        case (id, m) =>
          Vector(ContentHash.ofString(id.value), m.provenance.digest)
      })
      provenance = Provenance
        .raw(inputs)
        .andThen(
          Provenance.Step(
            "surface-ols-householder",
            Vector(
              "intercept"             -> Provenance.Param.Text(intercept.toString),
              "relativeRankTolerance" -> Provenance.Param.Num(rankTolerance.value)
            )
          )
        )
      fitted <- Surface
        .signed(grid, IArray.from(fit.fitted), provenance)
        .left
        .map(e => DecompositionError.Geometry("fitted", e))
      residual <- Surface
        .signed(
          grid,
          IArray.from(fit.residuals),
          provenance.andThen(Provenance.Step("residual"))
        )
        .left
        .map(e => DecompositionError.Geometry("residual", e))
      offset = if intercept == Intercept.Include then 1 else 0
    yield new SurfaceOlsFit(
      predictors.entries.map(_._1).zip(fit.coefficients.drop(offset)),
      if intercept == Intercept.Include then fit.coefficients.headOption else None,
      fitted,
      residual,
      OlsDiagnostics(fit.rank, rss, r2, fit.scaledDiagonalRatio)
    )

  def nonNegative[U <: Unit2D](
      response: Mass[U],
      predictors: PredictorSet[U],
      rankTolerance: RelativeRankTolerance,
      dualTolerance: RelativeDualTolerance
  ): Either[DecompositionError, SurfaceNnlsFit[U]] =
    val keys = predictors.entries.map(_._1)
    for
      grid <- Agreement
        .grids(predictors.grid, response.grid)
        .left
        .map(e => DecompositionError.Geometry("response", e))
      rows = Vector.tabulate(response.size)(i => predictors.entries.map(_._2.values(i)))
      y    = response.values.toVector
      fit <- ConstrainedLeastSquares
        .nonNegative(rows, y, rankTolerance, dualTolerance)
        .left
        .map(e => DecompositionError.Solve(keys, Intercept.Exclude, e))
      diagnostics <- constrainedDiagnostics(y, fit, RSquaredReference.Uncentered)
      provenance = derived(
        response,
        predictors,
        Provenance.Step(
          "surface-nnls-lawson-hanson",
          Vector(
            "relativeRankTolerance" -> Provenance.Param.Num(rankTolerance.value),
            "relativeDualTolerance" -> Provenance.Param.Num(dualTolerance.value)
          )
        )
      )
      fitted <- Surface
        .intensity(grid, IArray.from(fit.fitted), provenance)
        .left
        .map(e => DecompositionError.Geometry("fitted", e))
      residual <- residualOf(grid, fit, provenance)
    yield new SurfaceNnlsFit(keys.zip(fit.coefficients), fitted, residual, diagnostics)

  def mixture[U <: Unit2D](
      response: Mass[U],
      predictors: PredictorSet[U],
      intercept: Intercept,
      rankTolerance: RelativeRankTolerance,
      dualTolerance: RelativeDualTolerance
  ): Either[DecompositionError, SurfaceMixtureFit[U]] =
    val keys       = predictors.entries.map(_._1)
    val background = intercept == Intercept.Include
    for
      grid <- Agreement
        .grids(predictors.grid, response.grid)
        .left
        .map(e => DecompositionError.Geometry("response", e))
      uniform = 1.0 / response.size
      rows    = Vector.tabulate(response.size) { i =>
        val xs = predictors.entries.map(_._2.values(i))
        if background then uniform +: xs else xs
      }
      y = response.values.toVector
      fit <- ConstrainedLeastSquares
        .simplex(rows, y, rankTolerance, dualTolerance)
        .left
        .map(e => DecompositionError.Solve(keys, intercept, e))
      diagnostics <- constrainedDiagnostics(y, fit, RSquaredReference.Centered)
      provenance = derived(
        response,
        predictors,
        Provenance.Step(
          "surface-simplex-active-set",
          Vector(
            "intercept"             -> Provenance.Param.Text(intercept.toString),
            "relativeRankTolerance" -> Provenance.Param.Num(rankTolerance.value),
            "relativeDualTolerance" -> Provenance.Param.Num(dualTolerance.value)
          )
        )
      )
      // A convex combination of admitted masses is admitted at the loosest
      // deviation from one among them, plus the rounding of its own sum.
      deviation = predictors.entries.map((_, m) => math.abs(m.sum - 1.0)).max
      admission = math.max(MassAdmission, deviation) + response.size * SumRounding
      fitted <- Surface
        .mass(grid, IArray.from(fit.fitted), provenance, admission)
        .left
        .map(e => DecompositionError.Geometry("fitted", e))
      residual <- residualOf(grid, fit, provenance)
      offset = if background then 1 else 0
    yield new SurfaceMixtureFit(
      keys.zip(fit.coefficients.drop(offset)),
      Option.when(background)(fit.coefficients.head),
      fitted,
      residual,
      diagnostics
    )

  /** The default admission tolerance of [[Surface.mass]]. */
  private val MassAdmission = 1e-9

  /** Four units in the last place of one, per summed cell. */
  private val SumRounding = 4 * math.ulp(1.0)

  private def constrainedDiagnostics(
      y: Vector[Double],
      fit: ConstrainedFit,
      reference: RSquaredReference
  ): Either[DecompositionError, ConstrainedDiagnostics] =
    val rss  = fit.residuals.foldLeft(0.0)((s, r) => s + r * r)
    val mean =
      if reference == RSquaredReference.Centered then
        y.head + y.foldLeft(0.0)((s, v) => s + (v - y.head)) / y.size
      else 0.0
    val total = y.foldLeft(0.0)((s, v) => s + (v - mean) * (v - mean))
    val r2    = Option.when(total > 0.0)(1.0 - rss / total)
    Either.cond(
      rss.isFinite && total.isFinite && r2.forall(_.isFinite),
      ConstrainedDiagnostics(
        fit.rank,
        fit.active,
        fit.iterations,
        fit.dualViolation,
        rss,
        r2,
        reference,
        fit.scaledDiagonalRatio
      ),
      DecompositionError.Numerical("constrained fit diagnostics", r2.getOrElse(rss))
    )

  private def derived[U <: Unit2D](
      response: Mass[U],
      predictors: PredictorSet[U],
      step: Provenance.Step
  ): Provenance =
    Provenance
      .raw(ContentHash.combineAll(response.provenance.digest +: predictors.entries.flatMap {
        case (id, m) => Vector(ContentHash.ofString(id.value), m.provenance.digest)
      }))
      .andThen(step)

  private def residualOf[U <: Unit2D](
      grid: Grid[U],
      fit: ConstrainedFit,
      provenance: Provenance
  ): Either[DecompositionError, Signed[U]] =
    Surface
      .signed(grid, IArray.from(fit.residuals), provenance.andThen(Provenance.Step("residual")))
      .left
      .map(e => DecompositionError.Geometry("residual", e))

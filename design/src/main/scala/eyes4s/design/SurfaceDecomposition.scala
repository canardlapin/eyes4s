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
    case Solve(ks, i, e)   => s"Surface OLS predictors=$ks intercept=$i: ${e.message}"
    case Numerical(op, v)  => s"Surface OLS $op produced nonfinite value=$v."

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

object SurfaceDecomposition:
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

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

import eyes4s.kernel.*

enum AssociationMethod derives CanEqual:
  /** Product-moment correlation of the cell values. */
  case Pearson

  /** Pearson on cell ranks; tied cells share their average rank. */
  case Spearman

/** The correlation between two surfaces after removing, from each, its
  * least-squares fit on an intercept and the covariate surfaces. It is an
  * association, not a regression coefficient, and carries no p-value: grid
  * cells are not independent replicates.
  *
  * `estimate` is `None` when either residual's norm is at most the
  * [[RelativeRankTolerance]] times that surface's centered norm, so that a
  * surface the covariates explain, or a constant surface, has no partial
  * association rather than a correlation of rounding noise.
  *
  * The covariates are a design, not an operand: a constant covariate, which is
  * collinear with the intercept, or covariates collinear with one another fail
  * as [[DecompositionError.Solve]] with [[LeastSquaresError.RankDeficient]].
  * A constant `x` or `y` is a well-posed operand and returns `estimate = None`.
  */
final class PartialAssociation[U <: Unit2D] private (
    val method: AssociationMethod,
    val covariates: Vector[PredictorId],
    val estimate: Option[Double],
    val cells: Int,
    val provenance: Provenance
)

object PartialAssociation:
  /** Partial association of `x` and `y` given `covariates`, all on one grid.
    * Spearman ranks `x`, `y` and every covariate before the residual fits.
    */
  def of[U <: Unit2D](
      x: Mass[U],
      y: Mass[U],
      covariates: PredictorSet[U],
      method: AssociationMethod,
      rankTolerance: RelativeRankTolerance = RelativeRankTolerance.default
  ): Either[DecompositionError, PartialAssociation[U]] =
    val keys = covariates.entries.map(_._1)
    for
      _ <- Agreement
        .grids(covariates.grid, x.grid)
        .left
        .map(e => DecompositionError.Geometry("x", e))
      _ <- Agreement
        .grids(covariates.grid, y.grid)
        .left
        .map(e => DecompositionError.Geometry("y", e))
      transform = (s: Surface[U]) =>
        if method == AssociationMethod.Spearman then ranks(s.values) else s.values.toVector
      columns = covariates.entries.map(e => transform(e._2))
      rows    = Vector.tabulate(x.size)(i => 1.0 +: columns.map(_(i)))
      xs      = transform(x)
      ys      = transform(y)
      rx <- residual(rows, xs, keys, rankTolerance)
      ry <- residual(rows, ys, keys, rankTolerance)
      estimate =
        for
          a <- unexplained(rx, xs, rankTolerance)
          b <- unexplained(ry, ys, rankTolerance)
        yield
          val cross = a.indices.foldLeft(0.0)((s, i) => s + a(i) * b(i))
          math.max(-1.0, math.min(1.0, cross / (norm(a) * norm(b))))
      _ <- Either.cond(
        estimate.forall(_.isFinite),
        (),
        DecompositionError.Numerical("partial association", estimate.getOrElse(Double.NaN))
      )
      provenance = Provenance
        .raw(
          ContentHash.combineAll(
            Vector(x.provenance.digest, y.provenance.digest) ++
              covariates.entries.flatMap { case (id, m) =>
                Vector(ContentHash.ofString(id.value), m.provenance.digest)
              }
          )
        )
        .andThen(
          Provenance.Step(
            "surface-partial-association",
            Vector(
              "method"                -> Provenance.Param.Text(method.toString),
              "relativeRankTolerance" -> Provenance.Param.Num(rankTolerance.value)
            )
          )
        )
    yield new PartialAssociation(method, keys, estimate, x.size, provenance)

  private def residual(
      rows: Vector[Vector[Double]],
      values: Vector[Double],
      keys: Vector[PredictorId],
      tolerance: RelativeRankTolerance
  ): Either[DecompositionError, Vector[Double]] =
    LeastSquares
      .fit(rows, values, tolerance)
      .map(_.residuals)
      .left
      .map(e => DecompositionError.Solve(keys, Intercept.Include, e))

  /** The residual, unless it is rounding: no larger than the tolerance times
    * the centered norm, which is zero for a constant surface.
    */
  private def unexplained(
      residual: Vector[Double],
      values: Vector[Double],
      tolerance: RelativeRankTolerance
  ): Option[Vector[Double]] =
    val centered = centeredNorm(values)
    Option.when(centered > 0.0 && norm(residual) > tolerance.value * centered)(residual)

  private def norm(v: Vector[Double]): Double = v.foldLeft(0.0)(math.hypot)

  private def centeredNorm(v: Vector[Double]): Double =
    val mean = v.sum / v.size
    v.foldLeft(0.0)((s, a) => math.hypot(s, a - mean))

  /** One-based ranks; a run of tied values shares the mean of its ranks. */
  private[design] def ranks(values: IArray[Double]): Vector[Double] =
    val order  = values.indices.sortBy(values(_))(using Ordering.Double.TotalOrdering)
    val result = Array.fill(values.length)(0.0)
    var start  = 0
    while start < order.size do
      var end = start + 1
      while end < order.size && values(order(end)) == values(order(start)) do end += 1
      val rank = (start + 1 + end) / 2.0
      (start until end).foreach(k => result(order(k)) = rank)
      start = end
    result.toVector

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

package eyes4s.compare

import eyes4s.kernel.*

/** Optimal-transport comparisons between two distributions.
  *
  * ==Why these exist alongside the cell-wise measures==
  *
  * Correlation, cosine and total variation treat grid cells as exchangeable
  * labels. A map shifted by one cell and the same map shifted across the whole
  * display can score identically, because none of them knows that cells have
  * positions. For gaze data that is often the wrong question: two observers
  * looking a little to the left and a little to the right of the same face are
  * more alike than one looking at the face and one at the corner, and only a
  * transport measure can say so.
  */
object Transport:

  /** Sliced Wasserstein-1 distance.
    *
    * ==How it works==
    *
    * Project both distributions onto a direction, where the Wasserstein-1
    * distance has a closed form -- the integral of the absolute difference of
    * the cumulative distributions -- and average over directions. Each slice is
    * exact; the approximation is only in using finitely many directions.
    *
    * ==A finite-projection semimetric==
    *
    * Every projection is a one-dimensional Wasserstein metric, so their average
    * is symmetric, non-negative, and zero on identical inputs. A finite set of
    * projections need not separate distinct two-dimensional distributions:
    * with one horizontal direction, distributions that differ only vertically
    * have distance zero. The public interface therefore makes no identity-of-
    * indiscernibles claim.
    *
    * ==Directions are evenly spaced, not random==
    *
    * A randomised projection would make the result depend on a seed and two
    * runs on the same data disagree. Evenly spaced directions over a half-turn
    * cover the same ground deterministically, which is what DET-1 requires.
    */
  def slicedWasserstein[U <: Unit2D](
      directions: ProjectionDirections = ProjectionDirections.default
  ): Semimetric[Mass[U]] =
    new Semimetric[Mass[U]]:
      private val directionCount = directions.value

      val info = MeasureInfo(
        "sliced Wasserstein-1",
        "finite average of exact 1-D transport distances; a semimetric that may not separate distinct 2-D inputs",
        MeasureScale.DistanceLike,
        Some("Rabin et al. (2011)")
      )

      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, MeasureDistance] =
        Agreement.grids(a.grid, b.grid).left.map(CompareError.Grids.apply).flatMap { g =>
          val centres = g.centres
          var total   = 0.0
          var k       = 0
          while k < directionCount do
            val theta = math.Pi * k / directionCount
            val cx    = math.cos(theta)
            val cy    = math.sin(theta)
            total += sliceDistance(centres, a, b, cx, cy)
            k += 1
          MeasureDistance.computed("sliced Wasserstein-1", total / directionCount)
        }

      /** Exact one-dimensional Wasserstein-1 along one projection.
        *
        * `W1 = integral |F_a(t) - F_b(t)| dt`, evaluated over the sorted
        * projected support.
        */
      private def sliceDistance(
          centres: IArray[Pt[U]],
          a: Mass[U],
          b: Mass[U],
          cx: Double,
          cy: Double
      ): Double =
        val n     = centres.length
        val order = Array.tabulate(n)(identity)
        val proj  = Array.tabulate(n)(i => centres(i).x * cx + centres(i).y * cy)
        // Sort indices by projection. A stable sort keeps the result
        // independent of how ties happen to be ordered.
        val sorted = order.sortBy(proj.apply)

        var acc = 0.0
        var fa  = 0.0
        var fb  = 0.0
        var i   = 0
        while i < n - 1 do
          val idx = sorted(i)
          fa += a.at(idx)
          fb += b.at(idx)
          val width = proj(sorted(i + 1)) - proj(idx)
          acc += math.abs(fa - fb) * width
          i += 1
        acc

  /** Entropic optimal transport by Sinkhorn iteration.
    *
    * ==Not a metric, and the reason matters==
    *
    * Entropic regularisation biases the solution toward a spread-out plan, and
    * the bias does not vanish when the two inputs are identical: for any
    * distribution with mass on more than one cell, `d(x, x)` is strictly
    * greater than zero and grows with the regularisation. Tests assert this
    * rather than hiding it, because a caller who assumes a self-distance of
    * zero -- for instance by treating the diagonal of a similarity matrix as a
    * reference point -- will get a systematically wrong answer that looks
    * reasonable.
    *
    * The exception is a distribution concentrated on a single cell, where the
    * marginals admit exactly one transport plan and entropy has no freedom to
    * act. The bias comes from having somewhere to spread to; it is not an
    * unconditional property of the method.
    *
    * `eyesim` reaches optimal transport through three interchangeable backends
    * that are *not* numerically equivalent -- exact unnormalised, entropic
    * normalised, and exact normalised -- selected by whichever package happens
    * to be installed. The regularisation is the difference between two of them,
    * and here it is an explicit parameter with its consequence documented.
    *
    * ==Cost and the size guard==
    *
    * This forms the full cell-by-cell cost matrix, which is quadratic in the
    * number of cells. Above `maxCells` it refuses rather than allocating
    * silently: a 128x128 grid is 268 million entries, and a library that tries
    * is a library that hangs. [[slicedWasserstein]] scales linearly and is the
    * right tool at that size.
    *
    * ==Finite iteration is orientation-sensitive==
    *
    * The exact entropic optimum is symmetric for this symmetric ground cost,
    * but alternating row and column updates stopped after a fixed iteration
    * count need not be. Therefore this finite solver ships as a generic
    * [[Compare]], not [[SymmetricCompare]]. Callers may increase the iteration
    * budget to reduce numerical asymmetry, but no admitted finite budget is
    * promoted into a proof of convergence.
    */
  def sinkhorn[U <: Unit2D](
      config: SinkhornConfig = SinkhornConfig.default
  ): Compare[Mass[U], Mass[U], MeasureDistance] =
    new Compare[Mass[U], Mass[U], MeasureDistance]:
      private val epsilon    = config.regularisation.value
      private val iterations = config.iterations.value
      private val maxCells   = config.cellLimit.value

      val info = MeasureInfo(
        "Sinkhorn (entropic OT)",
        "finite-iteration regularised transport cost; orientation-sensitive and NOT a metric",
        MeasureScale.DistanceLike,
        Some("Cuturi (2013)")
      )

      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, MeasureDistance] =
        for
          g <- Agreement.grids(a.grid, b.grid).left.map(CompareError.Grids.apply)
          _ <- Either.cond(
            g.size <= maxCells,
            (),
            CompareError.CostMatrixLimitExceeded("Sinkhorn", g.size, maxCells)
          )
          result <- run(g, a, b)
        yield result

      private def run(
          g: Grid[U],
          a: Mass[U],
          b: Mass[U]
      ): Either[CompareError, MeasureDistance] =
        val n       = g.size
        val centres = g.centres
        val cost    = Array.ofDim[Double](n, n)
        val kern    = Array.ofDim[Double](n, n)
        var i       = 0
        while i < n do
          var j = 0
          while j < n do
            val d = centres(i).distanceTo(centres(j))
            cost(i)(j) = d * d
            kern(i)(j) = math.exp(-cost(i)(j) / epsilon)
            j += 1
          i += 1

        val u = Array.fill(n)(1.0)
        val v = Array.fill(n)(1.0)
        var t = 0
        while t < iterations do
          i = 0
          while i < n do
            var s = 0.0
            var j = 0
            while j < n do
              s += kern(i)(j) * v(j)
              j += 1
            u(i) = if s > 0.0 then a.at(i) / s else 0.0
            i += 1
          var j = 0
          while j < n do
            var s = 0.0
            i = 0
            while i < n do
              s += kern(i)(j) * u(i)
              i += 1
            v(j) = if s > 0.0 then b.at(j) / s else 0.0
            j += 1
          t += 1

        // The transport cost under the recovered plan.
        var total = 0.0
        i = 0
        while i < n do
          var j = 0
          while j < n do
            total += u(i) * kern(i)(j) * v(j) * cost(i)(j)
            j += 1
          i += 1
        MeasureDistance.computed("Sinkhorn", total)

end Transport

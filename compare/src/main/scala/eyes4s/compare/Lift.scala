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

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.surface.Smoother

/** Turning a comparison of one kind of thing into a comparison of another.
  *
  * ==Why this is a combinator and not a function==
  *
  * `eyesim`'s template similarity is an eighteen-hundred-line orchestrator that
  * bundles smoothing, matching, permutation and reshaping into one call. Almost
  * all of it is plumbing around a single idea: to compare two paths as
  * distributions, smooth each into a distribution and compare those. Written as
  * a combinator, that idea is four lines and composes with every measure rather
  * than with a fixed list of method names.
  */
object Lift:

  /** Compare two paths by comparing the maps they induce.
    *
    * The smoother and the grid are supplied once and shared, which is what
    * makes the two maps commensurable. `eyesim` lets each density carry its own
    * data-dependent default bounds and then correlates the results cell by
    * cell regardless, so two maps computed over different extents are compared
    * as though they were aligned.
    */
  def viaSmoothing[U <: Unit2D, S](
      inner: Compare[Mass[U], Mass[U], S],
      smoother: Smoother[U],
      grid: Grid[U],
      weight: Weight = Weight.Duration
  ): Compare[Scanpath[U], Scanpath[U], S] =
    new Compare[Scanpath[U], Scanpath[U], S]:
      val info = inner.info.copy(
        name = s"${inner.info.name} (smoothed)",
        summary = s"${inner.info.summary}; paths smoothed at sigma=${smoother.bandwidth.value}"
      )

      def compare(a: Scanpath[U], b: Scanpath[U]): Either[CompareError, S] =
        for
          ma <- density(a)
          mb <- density(b)
          s  <- inner.compare(ma, mb)
        yield s

      private def density(p: Scanpath[U]): Either[CompareError, Mass[U]] =
        p.occupancy(weight)
          .left
          .map(CompareError.Grids.apply)
          .flatMap(smoother.density(_, grid).left.map(CompareError.Estimation.apply))

  /** The same lift, preserving symmetry.
    *
    * Symmetry survives smoothing -- both sides go through the same estimator --
    * so a symmetric measure lifts to a symmetric one and can still be used for
    * an unordered pair. Without this overload the lift would silently widen the
    * type and lose that (PRD C-9).
    */
  def viaSmoothingSymmetric[U <: Unit2D, S](
      inner: SymmetricCompare[Mass[U], S],
      smoother: Smoother[U],
      grid: Grid[U],
      weight: Weight = Weight.Duration
  ): SymmetricCompare[Scanpath[U], S] =
    val lifted = viaSmoothing(inner, smoother, grid, weight)
    new SymmetricCompare[Scanpath[U], S]:
      val info                                                             = lifted.info
      def compare(a: Scanpath[U], b: Scanpath[U]): Either[CompareError, S] =
        lifted.compare(a, b)

end Lift

/** Scoring a predicted map against observed positions.
  *
  * ==The slot eyesim does not have== (PRD C-8)
  *
  * This is a map compared against a point set -- two different kinds of thing --
  * and it is the primitive the entire saliency-evaluation literature is built
  * on. `eyesim` can compare a map to a map and a path to a path, and has no way
  * to express this at all, which is why that literature is out of its reach.
  *
  * It is also the clearest illustration of the occupancy layer's claim: every
  * score here is `integrate` against a different transformation of the map.
  */
object Saliency:

  /** Normalised Scanpath Saliency: the mean of the z-scored map at the observed
    * positions.
    *
    * Zero means the observed positions are no better predicted than chance;
    * positive means the model put mass where the eye went. Being a mean of
    * z-scores it is unbounded, which the scale records.
    *
    * The whole computation is one integral against a z-scored surface, which is
    * what the duality in the architecture is for.
    */
  def nss[U <: Unit2D]: Compare[Mass[U], PointMeasure[U], Similarity] =
    new Compare[Mass[U], PointMeasure[U], Similarity]:
      val info = MeasureInfo(
        "NSS",
        "mean z-scored map value at the observed positions; 0 is chance, unbounded above",
        MeasureScale.UnboundedSimilarity,
        Some("Peters et al. (2005)")
      )

      def compare(
          model: Mass[U],
          observed: PointMeasure[U]
      ): Either[CompareError, Similarity] =
        Agreement
          .frames(model.grid.frame, observed.frame)
          .left
          .map(CompareError.Frames.apply)
          .flatMap { _ =>
            val n    = model.size
            val mean = model.sum / n
            var ss   = 0.0
            var i    = 0
            while i < n do
              val d = model.at(i) - mean
              ss += d * d
              i += 1
            val sd = math.sqrt(ss / n)
            if sd <= 1e-12 * math.max(math.abs(mean), java.lang.Double.MIN_NORMAL) then
              Left(CompareError.ConstantInput("NSS", CompareOperand.Model))
            else if observed.total <= 0.0 then
              Left(CompareError.EmptyInput("NSS", CompareOperand.Observed, observed.total))
            else
              // The integral: the z-scored map, sampled at each observed
              // position and weighted by its mass.
              val acc = observed.integrate { p =>
                model.sampleAt(p).map(v => (v - mean) / sd).getOrElse(0.0)
              }
              Right(Similarity(acc / observed.total))
          }

end Saliency

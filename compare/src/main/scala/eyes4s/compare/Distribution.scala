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

/** Comparisons between two distributions on the same grid.
  *
  * ==Each ships under the interface it satisfies==
  *
  * This is where PRD C-3 is paid for rather than asserted. `eyesim` offers
  * eight measures through one `method=` string and calls all of them
  * similarity; several are not, and one is not even symmetric. Here the
  * interface a measure extends is a claim the law suite checks:
  *
  *   - total variation is a genuine [[Metric]] on the simplex;
  *   - Hellinger is a genuine [[Metric]];
  *   - Jensen-Shannon's square root is a metric, but the divergence itself is
  *     only a [[Semimetric]] -- it fails the triangle inequality;
  *   - cosine and Pearson are [[SymmetricCompare]] and nothing stronger;
  *   - Kullback-Leibler is a [[Divergence]], asymmetric, and therefore cannot
  *     be used for an unordered comparison at all.
  */
object Distribution:

  private def aligned[U <: Unit2D](
      a: Mass[U],
      b: Mass[U]
  ): Either[CompareError, Int] =
    Agreement.grids(a.grid, b.grid).left.map(CompareError.Grids.apply).map(_ => a.size)

  // ---------------------------------------------------------------------------
  // Metrics
  // ---------------------------------------------------------------------------

  /** Total variation: half the L1 distance between two distributions.
    *
    * A true metric on the simplex, bounded in `[0, 1]`, and interpretable --
    * the largest difference in probability the two assign to any event.
    *
    * `eyesim` computes `1 - TV` and calls it "l1", which is a similarity built
    * from a metric and is itself not one.
    */
  def totalVariation[U <: Unit2D]: Metric[Mass[U]] =
    new Metric[Mass[U]]:
      val info = MeasureInfo(
        "total variation",
        "half the L1 distance; the largest probability difference on any event",
        MeasureScale.Bounded(0.0, 1.0),
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, MeasureDistance] =
        aligned(a, b).flatMap { n =>
          var s = 0.0
          var i = 0
          while i < n do
            s += math.abs(a.at(i) - b.at(i))
            i += 1
          MeasureDistance.computed("total variation", s / 2.0)
        }

  /** Hellinger distance: the L2 distance between the square roots, scaled.
    *
    * A true metric, bounded in `[0, 1]`, and less sensitive than KL to cells
    * where one distribution has almost no mass -- which for a gaze density is
    * most of them.
    */
  def hellinger[U <: Unit2D]: Metric[Mass[U]] =
    new Metric[Mass[U]]:
      val info = MeasureInfo(
        "Hellinger",
        "L2 distance between the square roots; robust where one map is near zero",
        MeasureScale.Bounded(0.0, 1.0),
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, MeasureDistance] =
        aligned(a, b).flatMap { n =>
          var s = 0.0
          var i = 0
          while i < n do
            val d = math.sqrt(a.at(i)) - math.sqrt(b.at(i))
            s += d * d
            i += 1
          MeasureDistance.computed("Hellinger", math.sqrt(s / 2.0))
        }

  // ---------------------------------------------------------------------------
  // Semimetric
  // ---------------------------------------------------------------------------

  /** Jensen-Shannon divergence: the symmetrised, bounded relative entropy.
    *
    * Symmetric and zero only on identical inputs, but **not** a metric -- it
    * fails the triangle inequality. Its square root is a metric; if that is
    * what you want, take it explicitly rather than assuming this one behaves.
    */
  def jensenShannon[U <: Unit2D](base: LogBase = LogBase.Two): Semimetric[Mass[U]] =
    new Semimetric[Mass[U]]:
      val info = MeasureInfo(
        "Jensen-Shannon",
        "symmetrised relative entropy; bounded, but not a metric (its square root is)",
        MeasureScale.Bounded(0.0, 1.0),
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, MeasureDistance] =
        aligned(a, b).flatMap { n =>
          val lb = math.log(base.value)
          var s  = 0.0
          var i  = 0
          while i < n do
            val p = a.at(i)
            val q = b.at(i)
            val m = (p + q) / 2.0
            if p > 0.0 && m > 0.0 then s += 0.5 * p * math.log(p / m) / lb
            if q > 0.0 && m > 0.0 then s += 0.5 * q * math.log(q / m) / lb
            i += 1
          MeasureDistance.computed("Jensen-Shannon", math.max(s, 0.0))
        }

  // ---------------------------------------------------------------------------
  // Divergence -- asymmetric, and structurally barred from unordered use
  // ---------------------------------------------------------------------------

  /** Exact Kullback-Leibler divergence from the right input to the left.
    *
    * Asymmetric, unbounded, and undefined where `b` has no mass but `a` does.
    * That undefined case is a named [[CompareError.RelativeEntropySupport]];
    * returning a finite sentinel or silently flooring the reference mass would
    * change the algorithm and can destroy separation of distinct inputs.
    *
    * Note what the type prevents: this does not extend [[SymmetricCompare]], so
    * it cannot be handed to an unordered pair evaluation. In `eyesim` every
    * measure goes through the same `method=` string and nothing distinguishes
    * this case.
    */
  def kullbackLeibler[U <: Unit2D](
      base: LogBase = LogBase.Two
  ): Divergence[Mass[U]] =
    new Divergence[Mass[U]]:
      val info = MeasureInfo(
        "Kullback-Leibler",
        "exact relative entropy; ASYMMETRIC and undefined on incompatible support",
        MeasureScale.DistanceLike,
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, MeasureDistance] =
        aligned(a, b).flatMap { n =>
          (0 until n).find(i => a.at(i) > 0.0 && b.at(i) <= 0.0) match
            case Some(i) =>
              Left(CompareError.RelativeEntropySupport("Kullback-Leibler", i, a.at(i), b.at(i)))
            case None =>
              val lb = math.log(base.value)
              var s  = 0.0
              var i  = 0
              while i < n do
                val p = a.at(i)
                if p > 0.0 then s += p * math.log(p / b.at(i)) / lb
                i += 1
              MeasureDistance.computed("Kullback-Leibler", math.max(s, 0.0))
        }

  /** Finite floor approximation to Kullback-Leibler divergence.
    *
    * The floor is an explicit scientific policy. It can make two distinct
    * distributions compare as zero and therefore does not satisfy the
    * separation law promised by [[Divergence]]. Its generic [[Compare]] return
    * type preserves the useful approximation without promoting it into a false
    * law-bearing value.
    */
  def flooredKullbackLeibler[U <: Unit2D](
      floor: ProbabilityFloor,
      base: LogBase = LogBase.Two
  ): Compare[Mass[U], Mass[U], MeasureDistance] =
    new Compare[Mass[U], Mass[U], MeasureDistance]:
      private val probabilityFloor = floor.value

      val info = MeasureInfo(
        "Floored Kullback-Leibler",
        "finite floor approximation; ASYMMETRIC and not guaranteed to separate inputs",
        MeasureScale.DistanceLike,
        None
      )

      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, MeasureDistance] =
        aligned(a, b).flatMap { n =>
          val lb = math.log(base.value)
          var s  = 0.0
          var i  = 0
          while i < n do
            val p = a.at(i)
            if p > 0.0 then s += p * math.log(p / math.max(b.at(i), probabilityFloor)) / lb
            i += 1
          MeasureDistance.computed("Floored Kullback-Leibler", math.max(s, 0.0))
        }

  // ---------------------------------------------------------------------------
  // Symmetric similarities that are nothing stronger
  // ---------------------------------------------------------------------------

  /** Cosine similarity between the two maps read as vectors.
    *
    * Symmetric and bounded, and **not** a metric: `1 - cos` fails the triangle
    * inequality. The angular distance derived from it is a metric; cosine
    * itself is not, and shipping it as one would be a false claim.
    */
  def cosine[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
    new SymmetricCompare[Mass[U], Similarity]:
      val info = MeasureInfo(
        "cosine",
        "inner product over norms; symmetric, bounded, NOT a metric",
        MeasureScale.Bounded(0.0, 1.0),
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
        aligned(a, b).flatMap { n =>
          var dot = 0.0
          var na  = 0.0
          var nb  = 0.0
          var i   = 0
          while i < n do
            dot += a.at(i) * b.at(i)
            na += a.at(i) * a.at(i)
            nb += b.at(i) * b.at(i)
            i += 1
          val den = math.sqrt(na) * math.sqrt(nb)
          if den <= 0.0 then Left(CompareError.ZeroNorm("cosine", math.sqrt(na), math.sqrt(nb)))
          else Similarity.computed("cosine", dot / den)
        }

  /** Pearson correlation over the cells.
    *
    * The default in much of this literature, and worth two warnings. It is
    * bounded in `[-1, 1]` and therefore **not safe to average** -- use
    * [[fisherZ]] for that, which is what the scale on the info says. And it
    * treats cells as exchangeable observations, so it is blind to how far apart
    * two disagreeing cells are: a map shifted by one cell and a map shifted
    * across the display can score identically. An optimal-transport measure is
    * the honest choice when spatial displacement is what you care about.
    */
  def pearson[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
    new SymmetricCompare[Mass[U], Similarity]:
      val info = MeasureInfo(
        "Pearson correlation",
        "cell-wise correlation; NOT averageable, and blind to spatial displacement",
        MeasureScale.Correlation,
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
        aligned(a, b).flatMap { n =>
          val ma  = a.sum / n
          val mb  = b.sum / n
          var sab = 0.0
          var saa = 0.0
          var sbb = 0.0
          var i   = 0
          while i < n do
            val da = a.at(i) - ma
            val db = b.at(i) - mb
            sab += da * db
            saa += da * da
            sbb += db * db
            i += 1
          // A RELATIVE constancy test, not `variance <= 0`.
          //
          // Summing a hundred copies of 0.01 does not give exactly 1.0, so a
          // perfectly uniform map has a mean a few ulps off its own cells and a
          // variance of order 1e-33 -- strictly positive. An absolute guard
          // therefore never fires, and the correlation returned is the ratio of
          // two quantities made entirely of rounding noise: a number between -1
          // and 1 that means nothing and looks like a result.
          val leftConstant  = isConstant(saa, ma, n)
          val rightConstant = isConstant(sbb, mb, n)
          if leftConstant || rightConstant then
            val operand =
              if leftConstant && rightConstant then CompareOperand.Both
              else if leftConstant then CompareOperand.Left
              else CompareOperand.Right
            Left(CompareError.ConstantInput("Pearson correlation", operand))
          else
            Similarity.computed("Pearson correlation", sab / (math.sqrt(saa) * math.sqrt(sbb)))
        }

  /** True when a map's variation is below a relative tolerance of its own
    * scale, and is therefore rounding noise rather than signal.
    */
  private def isConstant(sumSq: Double, mean: Double, n: Int): Boolean =
    val sd    = math.sqrt(sumSq / n)
    val scale = math.max(math.abs(mean), java.lang.Double.MIN_NORMAL)
    sd <= 1e-12 * scale

  /** Fisher-z transformed Pearson correlation.
    *
    * The form to average across trials or participants. Unbounded, which is
    * the point: averaging raw correlations understates the mean because the
    * scale compresses near the ends.
    */
  def fisherZ[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
    new SymmetricCompare[Mass[U], Similarity]:
      private val r = pearson[U]
      val info      = MeasureInfo(
        "Fisher z",
        "atanh of the Pearson correlation; unbounded, and the form to average",
        MeasureScale.FisherZ,
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
        r.compare(a, b).flatMap { s =>
          val clamped = math.max(-0.999999999999, math.min(0.999999999999, s.value))
          Similarity.computed("Fisher z", 0.5 * math.log((1 + clamped) / (1 - clamped)))
        }

end Distribution

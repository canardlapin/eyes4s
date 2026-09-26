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
  *   - cosine is a [[Kernel]] (dot products of unit-length map vectors);
  *   - Pearson is a [[SymmetricCompare]];
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
            s += math.abs(a.unsafeAt(i) - b.unsafeAt(i))
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
            val d = math.sqrt(a.unsafeAt(i)) - math.sqrt(b.unsafeAt(i))
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
            val p = a.unsafeAt(i)
            val q = b.unsafeAt(i)
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
          (0 until n).find(i => a.unsafeAt(i) > 0.0 && b.unsafeAt(i) <= 0.0) match
            case Some(i) =>
              Left(
                CompareError.RelativeEntropySupport(
                  "Kullback-Leibler",
                  i,
                  a.unsafeAt(i),
                  b.unsafeAt(i)
                )
              )
            case None =>
              val lb = math.log(base.value)
              var s  = 0.0
              var i  = 0
              while i < n do
                val p = a.unsafeAt(i)
                if p > 0.0 then s += p * math.log(p / b.unsafeAt(i)) / lb
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
            val p = a.unsafeAt(i)
            if p > 0.0 then
              s += p * math.log(p / math.max(b.unsafeAt(i), probabilityFloor)) / lb
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
    *
    * The dot product and both squared norms accumulate one cell per work unit
    * through a [[ComparisonCursor]], so the comparison can pause inside a large
    * grid. Whole and incremental evaluation share this one accumulation.
    */
  def cosine[U <: Unit2D]: Kernel[Mass[U]] & BoundedCompare[Mass[U], Mass[U], Similarity] =
    new Kernel[Mass[U]] with BoundedCompare[Mass[U], Mass[U], Similarity]:
      val info = MeasureInfo(
        "cosine",
        "inner product over norms; symmetric, bounded, NOT a metric",
        MeasureScale.Bounded(0.0, 1.0),
        None
      )
      def start(a: Mass[U], b: Mass[U]): ComparisonCursor[Similarity] =
        Agreement.grids(a.grid, b.grid) match
          case Left(error) => ComparisonCursor.decided(Left(CompareError.Grids(error)))
          case Right(_)    => new CosineCursor(a, b, 0, 0.0, 0.0, 0.0)

  /** Sequential accumulation from `index`; a cut between steps changes no operation. */
  private final class CosineCursor[U <: Unit2D](
      a: Mass[U],
      b: Mass[U],
      index: Int,
      dot: Double,
      leftNorm: Double,
      rightNorm: Double
  ) extends ComparisonCursor[Similarity]:
    def remaining: Long = (a.size - index).toLong

    def advance(quantum: ComparisonQuantum): ComparisonStep[Similarity] =
      val n   = a.size
      val end = if quantum.value >= n - index then n else index + quantum.value
      var i   = index
      var d   = dot
      var na  = leftNorm
      var nb  = rightNorm
      while i < end do
        d += a.unsafeAt(i) * b.unsafeAt(i)
        na += a.unsafeAt(i) * a.unsafeAt(i)
        nb += b.unsafeAt(i) * b.unsafeAt(i)
        i += 1
      if end < n then ComparisonStep.More(end - index, new CosineCursor(a, b, end, d, na, nb))
      else
        val den = math.sqrt(na) * math.sqrt(nb)
        ComparisonStep.Done(
          end - index,
          if den <= 0.0 then Left(CompareError.ZeroNorm("cosine", math.sqrt(na), math.sqrt(nb)))
          else Similarity.computed("cosine", d / den)
        )

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
            val da = a.unsafeAt(i) - ma
            val db = b.unsafeAt(i) - mb
            sab += da * db
            saa += da * da
            sbb += db * db
            i += 1
          constantOperands(info.name, a, b, n) match
            case Some(error) => Left(error)
            case None        =>
              Similarity.computed(info.name, sab / (math.sqrt(saa) * math.sqrt(sbb)))
        }

  /** The one constancy check shared by every correlation-shaped measure,
    * applied to the RAW cell values before any rank or distance transform.
    *
    * It is RELATIVE, not `variance <= 0`. Summing a hundred copies of 0.01
    * does not give exactly 1.0, so a perfectly uniform map has a mean a few
    * ulps off its own cells and a variance of order 1e-33 -- strictly positive.
    * An absolute guard therefore never fires, and the correlation returned is
    * the ratio of two quantities made entirely of rounding noise. Ranking or
    * double-centring does not remove that noise; it rescales it to a unit
    * spread, which is why the check precedes the transform.
    */
  private def constantOperands[U <: Unit2D](
      measure: String,
      a: Mass[U],
      b: Mass[U],
      n: Int
  ): Option[CompareError] =
    def constant(m: Mass[U]): Boolean =
      val mean  = m.sum / n
      var sumSq = 0.0
      var i     = 0
      while i < n do
        val d = m.unsafeAt(i) - mean
        sumSq += d * d
        i += 1
      val sd    = math.sqrt(sumSq / n)
      val scale = math.max(math.abs(mean), java.lang.Double.MIN_NORMAL)
      sd <= 1e-12 * scale
    val left  = constant(a)
    val right = constant(b)
    Option.when(left || right)(
      CompareError.ConstantInput(
        measure,
        if left && right then CompareOperand.Both
        else if left then CompareOperand.Left
        else CompareOperand.Right
      )
    )

  /** Rank correlation with average ranks for exact ties. Constant inputs are errors. */
  def spearman[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
    new SymmetricCompare[Mass[U], Similarity]:
      val info = MeasureInfo(
        "Spearman correlation",
        "Pearson correlation of average cell ranks; not a metric",
        MeasureScale.Correlation,
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
        aligned(a, b).flatMap(n => constantOperands(info.name, a, b, n).toLeft(n)).flatMap {
          n =>
            def ranks(m: Mass[U]): Array[Double] =
              val order  = (0 until n).sortBy(m.values(_))
              val result = new Array[Double](n)
              var start  = 0
              while start < n do
                var end = start + 1
                while end < n && m.values(order(end)) == m.values(order(start)) do end += 1
                val rank = (start.toDouble + end + 1) / 2
                var j    = start
                while j < end do
                  result(order(j)) = rank
                  j += 1
                start = end
              result
            val x  = ranks(a); val y = ranks(b); val mean = (n.toDouble + 1) / 2
            var xx = 0.0; var yy     = 0.0; var xy        = 0.0
            var i  = 0
            while i < n do
              val dx = x(i) - mean; val dy = y(i) - mean
              xx += dx * dx; yy += dy * dy; xy += dx * dy; i += 1
            // Raw values that are not constant have at least two distinct ranks.
            Similarity.computed(
              info.name,
              math.max(-1.0, math.min(1.0, xy / math.sqrt(xx) / math.sqrt(yy)))
            )
        }

  /** Extended Jaccard (Tanimoto): dot/(squared norms minus dot), not min/max overlap. */
  def extendedJaccard[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
    new SymmetricCompare[Mass[U], Similarity]:
      val info = MeasureInfo(
        "extended Jaccard",
        "Tanimoto similarity of nonnegative cell vectors; not a metric",
        MeasureScale.Bounded(0, 1),
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
        aligned(a, b).flatMap { n =>
          var dot = 0.0; var aa = 0.0; var bb = 0.0; var i = 0
          while i < n do
            val x = a.values(i); val y = b.values(i)
            dot += x * y; aa += x * x; bb += y * y; i += 1
          val denominator = aa + bb - dot
          if denominator <= 0 then Left(CompareError.ZeroNorm(info.name, aa, bb))
          else Similarity.computed(info.name, dot / denominator)
        }

  /** Biased sample distance correlation of the cell values (energy::dcor convention),
    * refused above [[DistanceCorrelationLimit.default]] cell pairs. See
    * [[distanceCorrelationWithin]].
    */
  def distanceCorrelation[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
    distanceCorrelationWithin(DistanceCorrelationLimit.default)

  /** Biased sample distance correlation of the cell values (energy::dcor convention).
    * O(n squared) time in grid cells and O(n) storage; this baseline instance is not a
    * spatial metric. Constant operands are errors, including identical uniform maps.
    *
    * A grid of `n` cells visits `n(n-1)/2` unordered cell pairs, twice. When that pair
    * count exceeds `limit.maximumPairs` the call returns
    * [[CompareError.WorkLimitExceeded]] before any pair is visited; the check follows the
    * grid check and precedes the constancy check.
    */
  def distanceCorrelationWithin[U <: Unit2D](
      limit: DistanceCorrelationLimit
  ): SymmetricCompare[Mass[U], Similarity] =
    new SymmetricCompare[Mass[U], Similarity]:
      val info = MeasureInfo(
        "distance correlation",
        "double-centred pairwise cell-value distances; not spatial transport",
        MeasureScale.Bounded(0, 1),
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
        aligned(a, b)
          .flatMap { n =>
            val pairs = DistanceCorrelationLimit.pairs(n)
            Either.cond(
              pairs <= limit.maximumPairs,
              n,
              CompareError.WorkLimitExceeded(info.name, n, pairs, limit.maximumPairs)
            )
          }
          .flatMap(n => constantOperands(info.name, a, b, n).toLeft(n))
          .flatMap { n =>
            val x = a.values; val y = b.values
            // Pass 1: row sums of both distance matrices, visiting each unordered pair once.
            val rowX = new Array[Double](n); val rowY = new Array[Double](n)
            var i    = 0
            while i < n do
              val xi = x(i); val yi = y(i)
              var sx = 0.0; var sy  = 0.0
              var j  = i + 1
              while j < n do
                val dx = math.abs(xi - x(j)); val dy = math.abs(yi - y(j))
                sx += dx; sy += dy; rowX(j) += dx; rowY(j) += dy
                j += 1
              rowX(i) += sx; rowY(i) += sy
              i += 1
            var grandX = 0.0; var grandY = 0.0
            i = 0
            while i < n do
              rowX(i) /= n; rowY(i) /= n; grandX += rowX(i); grandY += rowY(i)
              i += 1
            grandX /= n; grandY /= n
            // Pass 2: centred products. The matrices are symmetric with a zero raw diagonal,
            // so each off-diagonal pair counts twice and the diagonal once.
            var xx  = 0.0; var yy  = 0.0; var xy  = 0.0
            var dxx = 0.0; var dyy = 0.0; var dxy = 0.0
            i = 0
            while i < n do
              val xi = x(i); val yi             = y(i)
              val cx = grandX - rowX(i); val cy = grandY - rowY(i)
              val ux = cx - rowX(i); val uy     = cy - rowY(i)
              dxx += ux * ux; dyy += uy * uy; dxy += ux * uy
              var j = i + 1
              while j < n do
                val p = math.abs(xi - x(j)) + cx - rowX(j)
                val q = math.abs(yi - y(j)) + cy - rowY(j)
                xx += p * p; yy += q * q; xy += p * q
                j += 1
              i += 1
            xx = 2 * xx + dxx; yy = 2 * yy + dyy; xy = 2 * xy + dxy
            // Non-constant raw values have a nonzero double-centred distance matrix.
            Similarity.computed(
              info.name,
              math.sqrt(math.max(0.0, math.min(1.0, xy / math.sqrt(xx) / math.sqrt(yy))))
            )
          }

  /** Fisher-z transformed Pearson correlation, with explicit endpoints.
    *
    * Its measure name, "Fisher z (machine epsilon endpoints)", is the name
    * saved results have always carried for this policy. Before CR2 this
    * function was the legacy clamp, whose results carry the name "Fisher z"
    * (now `EyesimCompat.fisherZLegacy`).
    *
    * The form to average across trials or participants. Unbounded, which is
    * the point: averaging raw correlations understates the mean because the
    * scale compresses near the ends.
    *
    * A Pearson r within 64 machine epsilons (64 * 2^-52) of plus or minus one is
    * rounding error on a perfect correlation, such as a rescaled copy of a map,
    * and is snapped to exactly plus or minus one; the result is then clamped to
    * plus/minus (1-2^-52). Every perfect correlation therefore gives
    * atanh(1-2^-52), about 18.3684, rather than a value that depends on the
    * rounding of r. This is also eyesim's `similarity(method = "fisherz")`
    * endpoint policy. The historical 1e-12 clamp is
    * `eyes4s.compare.eyesim.EyesimCompat.fisherZLegacy`.
    *
    * Constant maps are not correlations: two constant maps, even identical ones, are
    * [[CompareError.ConstantInput]], as for [[pearson]]. eyesim instead returns atanh(1-2^-52)
    * for identical constant maps; that is a recorded intentional divergence (baseline case
    * `distribution-method-matrix`).
    */
  def fisherZ[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
    new SymmetricCompare[Mass[U], Similarity]:
      val info = MeasureInfo(
        "Fisher z (machine epsilon endpoints)",
        "atanh(Pearson), r within 64 eps of plus/minus 1 snapped to it, clamped at plus/minus (1-2^-52); unbounded, and the form to average",
        MeasureScale.FisherZ,
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
        pearson[U].compare(a, b).flatMap { r =>
          val eps     = math.ulp(1.0)
          val snap    = 64 * eps
          val snapped =
            if r.value > 1 - snap then 1.0 else if r.value < -1 + snap then -1.0 else r.value
          val bound = 1.0 - eps
          val value = math.max(-bound, math.min(bound, snapped))
          Similarity.computed(info.name, 0.5 * math.log((1 + value) / (1 - value)))
        }

end Distribution

/** Bound on the unordered cell pairs one distance-correlation call may visit.
  *
  * Distance correlation is quadratic in grid cells and runs as one synchronous,
  * uncancellable call, so an unbounded call on a large grid blocks its caller for
  * seconds (about 3.6 s at 256 by 256). A caller that accepts a longer call raises the
  * limit explicitly with [[DistanceCorrelationLimit.of]].
  */
final class DistanceCorrelationLimit private (val maximumPairs: Long)
object DistanceCorrelationLimit:
  /** 2^28 pairs: the largest admitted grid has 23,170 cells (152 by 152 is 23,104).
    * Measured at about 0.45 s per call on a laptop JVM (docs/EXECUTION_RESPONSIVENESS.md),
    * matching the half-second precedent of `FixationTransportConfig.DefaultMaximumWork`.
    */
  val DefaultMaximumPairs: Long = 1L << 28

  val default: DistanceCorrelationLimit = new DistanceCorrelationLimit(DefaultMaximumPairs)

  /** Unordered distinct cell pairs of a grid of `cells` cells: `cells(cells-1)/2`. */
  def pairs(cells: Int): Long = cells.toLong * (cells.toLong - 1) / 2

  /** A nonnegative pair limit; `Long.MaxValue` removes the bound. */
  def of(maximumPairs: Long): Either[ComparisonWorkError, DistanceCorrelationLimit] =
    if maximumPairs < 0 then Left(ComparisonWorkError.InvalidBudget(maximumPairs))
    else Right(new DistanceCorrelationLimit(maximumPairs))

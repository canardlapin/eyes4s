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

/** ScanMatch: comparing two label sequences by global alignment.
  *
  * ==Generic over the label, on purpose==
  *
  * The published method operates on a sequence of area-of-interest letters,
  * but nothing about the algorithm needs them to be areas of interest. Keeping
  * it generic means it works today, before `eyes4s-aoi` exists, and takes AOI
  * sequences unchanged when it does. It is equally usable on any labelling of a
  * path -- semantic categories, object identities, screen quadrants.
  *
  * ==A substitution cost, not an equality test==
  *
  * The substitution function is what distinguishes this from edit distance.
  * Looking at the nose instead of the mouth is a smaller error than looking at
  * the nose instead of the far corner, and a matrix built from inter-region
  * distance says so. Passing a 0/1 function reduces this to Levenshtein, which
  * is a legitimate choice and a different measure.
  *
  * ==Gaps are the point==
  *
  * A lattice alignment forces every element into a correspondence. Two paths
  * visiting the same regions in the same order, one of them pausing somewhere
  * extra, should score well -- and only a gapped alignment can express the
  * extra stop as an extra stop rather than as a mismatch.
  */
object ScanMatch:

  /** Normalised similarity in `[0, 1]`, higher meaning more alike.
    *
    * The alignment cost is divided by the worst cost the same pair could have
    * incurred -- every element gapped on both sides -- so sequences of
    * different lengths remain comparable. Without that normalisation a long
    * pair of scanpaths would always look less similar than a short one.
    */
  def similarity[A](
      substitution: (A, A) => Double,
      gap: ScanMatchGap
  ): Compare[Vector[A], Vector[A], Similarity] =
    comparison(substitution, gap)

  /** ScanMatch with the built-in exact-match cost.
    *
    * Unlike [[similarity]], this returns [[SymmetricCompare]] because the
    * library owns and can establish symmetry of the substitution cost. An
    * arbitrary caller-supplied function receives the weaker [[Compare]]
    * contract: its type alone cannot prove that `cost(a, b) == cost(b, a)`.
    */
  def exactSimilarity[A](
      gap: ScanMatchGap = ScanMatchGap.unit
  ): SymmetricCompare[Vector[A], Similarity] =
    val delegate = comparison(exactMatch[A], gap)
    new SymmetricCompare[Vector[A], Similarity]:
      val info                                                                  = delegate.info
      def compare(a: Vector[A], b: Vector[A]): Either[CompareError, Similarity] =
        delegate.compare(a, b)

  private def comparison[A](
      substitution: (A, A) => Double,
      gap: ScanMatchGap
  ): Compare[Vector[A], Vector[A], Similarity] =
    new Compare[Vector[A], Vector[A], Similarity]:

      private val gapCost = gap.value

      val info = MeasureInfo(
        "ScanMatch",
        "gapped global alignment of label sequences; symmetry depends on the substitution cost",
        MeasureScale.Bounded(0.0, 1.0),
        Some("Cristino, Mathot, Theeuwes & Gilchrist (2010)")
      )

      def compare(a: Vector[A], b: Vector[A]): Either[CompareError, Similarity] =
        if a.isEmpty then Left(CompareError.TooShort("a label sequence", 0, 1))
        else if b.isEmpty then Left(CompareError.TooShort("a label sequence", 0, 1))
        else
          validateCosts(a, b, substitution).flatMap { _ =>
            Alignment.needlemanWunsch(gapCost).align(a, b)(substitution).flatMap { path =>
              val worst = gapCost * (a.length + b.length)
              Similarity.computed(
                "ScanMatch",
                math.max(0.0, math.min(1.0, 1.0 - path.cost / worst))
              )
            }
          }

      private def validateCosts(
          a: Vector[A],
          b: Vector[A],
          substitution: (A, A) => Double
      ): Either[CompareError, Unit] =
        var i                             = 0
        var failure: Option[CompareError] = None
        while i < a.length && failure.isEmpty do
          var j = 0
          while j < b.length && failure.isEmpty do
            val cost = substitution(a(i), b(j))
            if !cost.isFinite || cost < 0.0 then
              failure = Some(CompareError.InvalidSubstitutionCost("ScanMatch", i, j, cost))
            j += 1
          i += 1
        failure.toLeft(())

  /** The simplest substitution: identical labels cost nothing, different ones
    * cost one. Reduces the measure to a normalised edit distance.
    */
  def exactMatch[A]: (A, A) => Double = (x, y) => if x == y then 0.0 else 1.0

end ScanMatch

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

/** One move in an alignment. */
enum AlignmentStep derives CanEqual:
  case Match(left: Int, right: Int)
  case SkipLeft(left: Int)
  case SkipRight(right: Int)

/** A correspondence between two sequences, with the cost it incurred. */
final case class AlignmentPath(steps: Vector[AlignmentStep], cost: Double):
  def matches: Vector[(Int, Int)] = steps.collect { case AlignmentStep.Match(i, j) => (i, j) }
  def length: Int                 = steps.length
  def render: String              = f"path(${steps.length} steps, cost=$cost%.4g)"

/** Aligning two sequences under a cost model.
  *
  * ==Factored out because it is one algorithm, not four==
  *
  * MultiMatch, ScanMatch, dynamic time warping and the discrete Frechet
  * distance are all monotone paths through a cost lattice; they differ in which
  * cells contribute, how costs combine, and whether skips are allowed. `eyesim`
  * implements only MultiMatch's case, and does it by building an `igraph`
  * object and running Dijkstra over the lattice.
  *
  * That is a real dependency for no reason. The lattice is a DAG whose
  * topological order is `i + j`, so a shortest path is an O(nm) dynamic
  * program in a dozen lines. The graph library, its object construction, and
  * its character-keyed vertex names all go away, and so does eyesim's comment
  * apologising for relying on internal igraph vertex ordering.
  */
trait Alignment:
  def name: String
  def align[A, B](xs: IndexedSeq[A], ys: IndexedSeq[B])(
      cost: (A, B) => Double
  ): Either[CompareError, AlignmentPath]

object Alignment:

  /** How the cost of a lattice path accumulates. */
  private enum Accumulate:
    /** Sum, counting the origin cell. Dynamic time warping. */
    case SumInclusive

    /** Sum, NOT counting the origin cell.
      *
      * MultiMatch's convention: its edges are weighted by the cost of the cell
      * they arrive at, so the starting cell never contributes. Reproducing it
      * exactly is what makes conformance against the reference implementation
      * meaningful rather than approximate.
      */
    case SumExclusive

    /** The largest cost on the path. The discrete Frechet bottleneck. */
    case Bottleneck

  private def lattice(mode: Accumulate): Alignment =
    new Alignment:
      val name = mode match
        case Accumulate.SumInclusive => "dtw"
        case Accumulate.SumExclusive => "monotone lattice"
        case Accumulate.Bottleneck   => "discrete Frechet"

      def align[A, B](xs: IndexedSeq[A], ys: IndexedSeq[B])(
          cost: (A, B) => Double
      ): Either[CompareError, AlignmentPath] =
        val n = xs.length
        val m = ys.length
        if n == 0 then Left(CompareError.TooShort("the left sequence", 0, 1))
        else if m == 0 then Left(CompareError.TooShort("the right sequence", 0, 1))
        else
          val c = Array.tabulate(n, m)((i, j) => cost(xs(i), ys(j)))
          val d = Array.ofDim[Double](n, m)

          def combine(prev: Double, here: Double): Double = mode match
            case Accumulate.Bottleneck => math.max(prev, here)
            case _                     => prev + here

          d(0)(0) = mode match
            case Accumulate.SumExclusive => 0.0
            case _                       => c(0)(0)

          var i = 0
          while i < n do
            var j = 0
            while j < m do
              if i != 0 || j != 0 then
                val best =
                  if i == 0 then d(0)(j - 1)
                  else if j == 0 then d(i - 1)(0)
                  else math.min(d(i - 1)(j), math.min(d(i)(j - 1), d(i - 1)(j - 1)))
                d(i)(j) = combine(best, c(i)(j))
              j += 1
            i += 1

          // Walk back along the choices the forward pass implies.
          val steps = scala.collection.mutable.ArrayBuffer.empty[AlignmentStep]
          var pi    = n - 1
          var pj    = m - 1
          while pi > 0 || pj > 0 do
            steps += AlignmentStep.Match(pi, pj)
            if pi == 0 then pj -= 1
            else if pj == 0 then pi -= 1
            else
              val diag = d(pi - 1)(pj - 1)
              val up   = d(pi - 1)(pj)
              val left = d(pi)(pj - 1)
              if diag <= up && diag <= left then { pi -= 1; pj -= 1 }
              else if up <= left then pi -= 1
              else pj -= 1
          steps += AlignmentStep.Match(0, 0)

          Right(AlignmentPath(steps.reverse.toVector, d(n - 1)(m - 1)))

  /** MultiMatch's alignment: a monotone lattice path whose cost excludes the
    * origin cell.
    */
  val monotoneLattice: Alignment = lattice(Accumulate.SumExclusive)

  /** Dynamic time warping: the same lattice, counting every visited cell. */
  val dtw: Alignment = lattice(Accumulate.SumInclusive)

  /** Discrete Frechet: minimise the WORST correspondence rather than the total.
    *
    * A different question from DTW, and the right one when the claim is "these
    * two paths never diverge by more than X" rather than "these two paths are
    * close on average".
    */
  val frechet: Alignment = lattice(Accumulate.Bottleneck)

  /** Needleman-Wunsch global alignment with a linear gap penalty.
    *
    * Unlike the lattice alignments, this may SKIP an element on either side
    * rather than forcing every element into a correspondence. That is what
    * ScanMatch needs: two scanpaths visiting the same regions in the same order
    * but one of them pausing somewhere extra should align well, and a lattice
    * alignment has no way to express the extra stop except by matching it to
    * something.
    */
  def needlemanWunsch(gap: Double): Alignment =
    new Alignment:
      val name = "Needleman-Wunsch"

      def align[A, B](xs: IndexedSeq[A], ys: IndexedSeq[B])(
          cost: (A, B) => Double
      ): Either[CompareError, AlignmentPath] =
        val n = xs.length
        val m = ys.length
        if n == 0 then Left(CompareError.TooShort("the left sequence", 0, 1))
        else if m == 0 then Left(CompareError.TooShort("the right sequence", 0, 1))
        else
          val d = Array.ofDim[Double](n + 1, m + 1)
          var i = 0
          while i <= n do
            d(i)(0) = i * gap
            i += 1
          var j = 0
          while j <= m do
            d(0)(j) = j * gap
            j += 1

          i = 1
          while i <= n do
            j = 1
            while j <= m do
              val matched = d(i - 1)(j - 1) + cost(xs(i - 1), ys(j - 1))
              val skipL   = d(i - 1)(j) + gap
              val skipR   = d(i)(j - 1) + gap
              d(i)(j) = math.min(matched, math.min(skipL, skipR))
              j += 1
            i += 1

          val steps = scala.collection.mutable.ArrayBuffer.empty[AlignmentStep]
          var pi    = n
          var pj    = m
          while pi > 0 || pj > 0 do
            if pi > 0 && pj > 0 &&
              d(pi)(pj) == d(pi - 1)(pj - 1) + cost(xs(pi - 1), ys(pj - 1))
            then
              steps += AlignmentStep.Match(pi - 1, pj - 1)
              pi -= 1
              pj -= 1
            else if pi > 0 && d(pi)(pj) == d(pi - 1)(pj) + gap then
              steps += AlignmentStep.SkipLeft(pi - 1)
              pi -= 1
            else
              steps += AlignmentStep.SkipRight(pj - 1)
              pj -= 1

          Right(AlignmentPath(steps.reverse.toVector, d(n)(m)))

end Alignment

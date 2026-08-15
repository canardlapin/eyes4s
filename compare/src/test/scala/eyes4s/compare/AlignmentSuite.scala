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
import eyes4s.kernel.Unit2D.Px

class AlignmentSuite extends munit.FunSuite:

  private def absCost(a: Double, b: Double): Double = math.abs(a - b)

  /** Brute force over every monotone path, for small cases.
    *
    * The point of having this: the dynamic program replaces eyesim's Dijkstra,
    * and "it agrees with an exhaustive search" is the only check that the
    * replacement is exact rather than merely plausible.
    */
  private def bruteForce(
      xs: IndexedSeq[Double],
      ys: IndexedSeq[Double],
      excludeOrigin: Boolean
  ): Double =
    val n                                       = xs.length
    val m                                       = ys.length
    def go(i: Int, j: Int, acc: Double): Double =
      val here = if i == 0 && j == 0 && excludeOrigin then 0.0 else absCost(xs(i), ys(j))
      val cost = acc + here
      if i == n - 1 && j == m - 1 then cost
      else
        val options = Seq(
          if i + 1 < n then Some(go(i + 1, j, cost)) else None,
          if j + 1 < m then Some(go(i, j + 1, cost)) else None,
          if i + 1 < n && j + 1 < m then Some(go(i + 1, j + 1, cost)) else None
        ).flatten
        options.min
    go(0, 0, 0.0)

  // -------------------------------------------------------------------------
  // The DP is exact
  // -------------------------------------------------------------------------

  test("the monotone lattice DP agrees with an exhaustive search") {
    val cases = Seq(
      (Vector(1.0, 2.0, 3.0), Vector(1.0, 3.0)),
      (Vector(0.0, 5.0, 2.0, 7.0), Vector(1.0, 4.0, 2.0)),
      (Vector(3.0), Vector(1.0, 2.0, 3.0, 4.0)),
      (Vector(2.0, 2.0, 2.0), Vector(2.0, 2.0, 2.0))
    )
    cases.foreach { (xs, ys) =>
      val dp = Alignment.monotoneLattice.align(xs, ys)(absCost).toOption.get
      assertEqualsDouble(
        dp.cost,
        bruteForce(xs, ys, excludeOrigin = true),
        1e-12,
        clue((xs, ys))
      )
    }
  }

  test("dynamic time warping agrees with an exhaustive search that counts the origin") {
    val xs = Vector(0.0, 5.0, 2.0, 7.0)
    val ys = Vector(1.0, 4.0, 2.0)
    val dp = Alignment.dtw.align(xs, ys)(absCost).toOption.get
    assertEqualsDouble(dp.cost, bruteForce(xs, ys, excludeOrigin = false), 1e-12)
  }

  test("the two lattice modes differ exactly by the origin cell") {
    val xs = Vector(10.0, 2.0, 3.0)
    val ys = Vector(0.0, 2.0, 3.0)
    val ex = Alignment.monotoneLattice.align(xs, ys)(absCost).toOption.get.cost
    val in = Alignment.dtw.align(xs, ys)(absCost).toOption.get.cost
    assertEqualsDouble(in - ex, absCost(10.0, 0.0), 1e-12)
  }

  // -------------------------------------------------------------------------
  // Path shape
  // -------------------------------------------------------------------------

  test("a path is monotone and spans both sequences end to end") {
    val xs = Vector(1.0, 4.0, 2.0, 8.0, 3.0)
    val ys = Vector(1.0, 3.0, 9.0)
    val p  = Alignment.monotoneLattice.align(xs, ys)(absCost).toOption.get
    val ms = p.matches

    assertEquals(ms.head, (0, 0))
    assertEquals(ms.last, (xs.length - 1, ys.length - 1))
    ms.sliding(2).foreach {
      case Seq((i0, j0), (i1, j1)) =>
        assert(i1 >= i0 && j1 >= j0, clue(((i0, j0), (i1, j1))))
        assert(i1 + j1 > i0 + j0, "every step must advance")
      case _ => ()
    }
  }

  test("identical sequences align on the diagonal at zero cost") {
    val xs = Vector(1.0, 2.0, 3.0, 4.0)
    val p  = Alignment.dtw.align(xs, xs)(absCost).toOption.get
    assertEqualsDouble(p.cost, 0.0, 1e-12)
    assertEquals(p.matches, Vector((0, 0), (1, 1), (2, 2), (3, 3)))
  }

  test("an empty sequence is refused rather than aligned with nothing") {
    assert(Alignment.dtw.align(Vector.empty[Double], Vector(1.0))(absCost).isLeft)
    assert(Alignment.dtw.align(Vector(1.0), Vector.empty[Double])(absCost).isLeft)
  }

  // -------------------------------------------------------------------------
  // Frechet asks a different question
  // -------------------------------------------------------------------------

  test("Frechet reports the worst correspondence, where DTW reports the total") {
    val xs = Vector(0.0, 0.0, 0.0)
    val ys = Vector(0.0, 9.0, 0.0)
    val f  = Alignment.frechet.align(xs, ys)(absCost).toOption.get
    val d  = Alignment.dtw.align(xs, ys)(absCost).toOption.get

    // A monotone path must visit every column, so an outlier occupying a whole
    // column cannot be stepped around -- it is genuinely the bottleneck. Here
    // it is also the ONLY nonzero pair, so the sum and the maximum coincide.
    // That agreement is worth pinning: it is the boundary case, and the next
    // test shows where the two measures separate.
    assertEqualsDouble(f.cost, 9.0, 1e-12)
    assertEqualsDouble(d.cost, 9.0, 1e-12)
  }

  test("Frechet ignores how long a path stays close, DTW does not") {
    val short = (Vector(0.0, 0.0), Vector(0.0, 3.0))
    val long  = (Vector(0.0, 0.0, 0.0, 0.0), Vector(0.0, 3.0, 3.0, 3.0))
    def fr(p: (Vector[Double], Vector[Double])) =
      Alignment.frechet.align(p._1, p._2)(absCost).toOption.get.cost
    def dt(p: (Vector[Double], Vector[Double])) =
      Alignment.dtw.align(p._1, p._2)(absCost).toOption.get.cost

    assertEqualsDouble(fr(short), fr(long), 1e-12) // same worst divergence
    assert(dt(long) > dt(short), clue((dt(short), dt(long))))
  }

  test("Frechet on a genuinely divergent pair reports the divergence") {
    val xs = Vector(0.0, 0.0)
    val ys = Vector(7.0, 7.0)
    assertEqualsDouble(Alignment.frechet.align(xs, ys)(absCost).toOption.get.cost, 7.0, 1e-12)
  }

  // -------------------------------------------------------------------------
  // Needleman-Wunsch can skip
  // -------------------------------------------------------------------------

  test("gaps let one sequence carry an extra element") {
    // Same order, one extra stop in the middle. A lattice alignment must force
    // the extra element into a correspondence; NW can skip it.
    val xs = Vector(1.0, 5.0, 9.0)
    val ys = Vector(1.0, 5.0, 3.0, 9.0)
    val p  = Alignment.needlemanWunsch(gap = 0.5).align(xs, ys)(absCost).toOption.get

    assert(p.steps.exists(_.isInstanceOf[AlignmentStep.SkipRight]), clue(p.steps))
    assertEquals(p.matches.length, 3)
    assert(p.cost < 1.0, clue(p.cost))
  }

  test("a large gap penalty forces matching instead of skipping") {
    val xs = Vector(1.0, 5.0, 9.0)
    val ys = Vector(1.0, 5.0, 3.0, 9.0)
    val p  = Alignment.needlemanWunsch(gap = 1000.0).align(xs, ys)(absCost).toOption.get
    assertEquals(p.steps.count(_.isInstanceOf[AlignmentStep.SkipRight]), 1)
  }

  test("identical sequences need no gaps at all") {
    val xs = Vector(1.0, 2.0, 3.0)
    val p  = Alignment.needlemanWunsch(gap = 0.5).align(xs, xs)(absCost).toOption.get
    assertEquals(p.matches, Vector((0, 0), (1, 1), (2, 2)))
    assertEqualsDouble(p.cost, 0.0, 1e-12)
  }

  // -------------------------------------------------------------------------
  // MultiMatch
  // -------------------------------------------------------------------------

  val screen = Frame.screen("display", 1000, 1000).toOption.get
  val clock  = ClockId("tracker")

  private def fix(fromMs: Long, toMs: Long, x: Double, y: Double) =
    Event.Fixation
      .of(
        Interval.of(clock, Instant.millis(fromMs), Instant.millis(toMs)).toOption.get,
        Pt[Px](x, y),
        1.0,
        DispersionMethod.RmsRadius,
        10
      )
      .toOption
      .get

  private def path(pts: (Long, Long, Double, Double)*) =
    Scanpath.of(screen, clock, IArray.from(pts.map(fix))).toOption.get

  val a = path((0, 100, 100, 100), (150, 250, 400, 100), (300, 400, 400, 400))
  val b = path((0, 100, 110, 105), (150, 250, 405, 110), (300, 400, 395, 405))
  val c = path((0, 100, 900, 900), (150, 250, 100, 900), (300, 400, 500, 100))

  test("a scanpath is maximally similar to itself on every dimension") {
    val s = MultiMatch[Px].compare(a, a).toOption.get
    assertEqualsDouble(s.shape, 1.0, 1e-12)
    assertEqualsDouble(s.direction, 1.0, 1e-12)
    assertEqualsDouble(s.length, 1.0, 1e-12)
    assertEqualsDouble(s.position, 1.0, 1e-12)
    assertEqualsDouble(s.duration, 1.0, 1e-12)
  }

  test("a near copy scores high, a different path scores lower") {
    val near = MultiMatch[Px].compare(a, b).toOption.get
    val far  = MultiMatch[Px].compare(a, c).toOption.get
    assert(near.position > 0.98, clue(near.render))
    assert(far.position < near.position, clue((near.render, far.render)))
  }

  test("the dimensions are separable: alike in shape, unalike in position") {
    // The same L shape, translated far across the display. Shape and direction
    // should survive; position should not. This is the entire reason MultiMatch
    // reports five numbers, and a single aggregate would hide it.
    val shifted = path((0, 100, 600, 600), (150, 250, 900, 600), (300, 400, 900, 900))
    val s       = MultiMatch[Px].compare(a, shifted).toOption.get
    assertEqualsDouble(s.shape, 1.0, 1e-9)
    assertEqualsDouble(s.direction, 1.0, 1e-9)
    assertEqualsDouble(s.length, 1.0, 1e-9)
    // Shifted by (500, 500) on a 1000x1000 display: the offset is exactly half
    // the diagonal, so position lands on 0.5 by construction.
    assertEqualsDouble(s.position, 0.5, 1e-9)
    assert(s.position < s.shape, clue(s.render))
  }

  test("the score is symmetric") {
    val ab = MultiMatch[Px].compare(a, c).toOption.get
    val ba = MultiMatch[Px].compare(c, a).toOption.get
    assertEqualsDouble(ab.shape, ba.shape, 1e-12)
    assertEqualsDouble(ab.direction, ba.direction, 1e-12)
    assertEqualsDouble(ab.position, ba.position, 1e-12)
  }

  test("every dimension stays in the unit interval") {
    Seq(MultiMatch[Px].compare(a, b), MultiMatch[Px].compare(a, c)).foreach { r =>
      val s = r.toOption.get
      Seq(s.shape, s.direction, s.length, s.position, s.duration).foreach { v =>
        assert(v >= 0.0 && v <= 1.0, clue(v))
      }
    }
  }

  test("a single-fixation scanpath has no saccades and is refused") {
    val one = path((0, 100, 500, 500))
    val r   = MultiMatch[Px].compare(one, a)
    assert(r.isLeft)
    r.left.foreach(e => assert(clue(e.message).contains("at least 2")))
  }

  test("scanpaths in different frames are refused") {
    val other     = Frame.screen("other", 1000, 1000).toOption.get
    val elsewhere = Scanpath
      .of(other, clock, IArray(fix(0, 100, 1, 1), fix(150, 250, 2, 2)))
      .toOption
      .get
    assert(MultiMatch[Px].compare(a, elsewhere).isLeft)
  }

  test("the normalising diagonal comes from the frame, not from an argument") {
    // eyesim requires a screensize at every call site and recomputes the
    // diagonal inline in five places. Here the scanpath carries its frame, so
    // the value cannot be forgotten or supplied inconsistently.
    assertEqualsDouble(a.frame.diagonal, math.hypot(1000.0, 1000.0), 1e-12)
  }

  test("MultiMatch is symmetric but is not offered as a metric") {
    val s: SymmetricCompare[Scanpath[Px], MultiMatchScore] = MultiMatch[Px]
    assert(s.info.summary.contains("NOT a metric"))
  }

end AlignmentSuite

class ScanMatchSuite extends munit.FunSuite:

  private val unitGap = ScanMatchGap.unit
  private val sm      = ScanMatch.exactSimilarity[Char](unitGap)

  test("a sequence is maximally similar to itself") {
    assertEqualsDouble(
      sm.compare("ABCD".toVector, "ABCD".toVector).toOption.get.value,
      1.0,
      1e-12
    )
  }

  test("an extra stop costs one gap, not a cascade of mismatches") {
    // The reason a gapped alignment is used. ABXCD against ABCD differs by one
    // insertion; a lattice alignment would have to mismatch everything after it.
    val s = sm.compare("ABXCD".toVector, "ABCD".toVector).toOption.get.value
    assert(s > 0.8, clue(s))
  }

  test("a reordered sequence scores lower than an interrupted one") {
    val inserted = sm.compare("ABXCD".toVector, "ABCD".toVector).toOption.get.value
    val shuffled = sm.compare("DCBA".toVector, "ABCD".toVector).toOption.get.value
    assert(shuffled < inserted, clue((shuffled, inserted)))
  }

  test("disjoint sequences score low") {
    val s = sm.compare("AAAA".toVector, "BBBB".toVector).toOption.get.value
    assert(s < 0.6, clue(s))
  }

  test("the score is symmetric") {
    val ab = sm.compare("ABCDE".toVector, "AXCE".toVector).toOption.get.value
    val ba = sm.compare("AXCE".toVector, "ABCDE".toVector).toOption.get.value
    assertEqualsDouble(ab, ba, 1e-12)
  }

  test("normalisation keeps sequences of different lengths comparable") {
    // Doubling both sequences, preserving the pattern, must not change the
    // score. Without normalisation the longer pair would always look less alike.
    val shortPair = sm.compare("ABC".toVector, "ABD".toVector).toOption.get.value
    val longPair  = sm.compare("ABCABC".toVector, "ABDABD".toVector).toOption.get.value
    assertEqualsDouble(shortPair, longPair, 1e-9)
  }

  test("a graded substitution distinguishes a near miss from a far one") {
    // Looking at the nose instead of the mouth is a smaller error than looking
    // at the far corner, and only a substitution matrix can say so.
    val position: Map[Char, Double] = Map('A' -> 0.0, 'B' -> 1.0, 'C' -> 10.0)
    val graded                      = ScanMatch.similarity[Char](
      (x, y) => math.abs(position(x) - position(y)) / 10.0,
      unitGap
    )
    val near = graded.compare("AB".toVector, "AA".toVector).toOption.get.value
    val far  = graded.compare("AC".toVector, "AA".toVector).toOption.get.value
    assert(near > far, clue((near, far)))
  }

  test("an empty sequence is refused") {
    assert(sm.compare(Vector.empty[Char], "AB".toVector).isLeft)
  }

  test("the library-owned exact cost is symmetric but is not offered as a metric") {
    val s: SymmetricCompare[Vector[Char], Similarity] = sm
    assert(s.info.summary.contains("symmetry depends"))
  }

  test("a caller-supplied substitution cost is not certified as symmetric") {
    val errors = scala.compiletime.testing.typeCheckErrors("""
      import eyes4s.compare.*
      val gap = ScanMatchGap.of(1.0).toOption.get
      val claimed: SymmetricCompare[Vector[Char], Similarity] =
        ScanMatch.similarity[Char]((left, right) => if left < right then 0.0 else 1.0, gap)
    """)
    assert(errors.nonEmpty, "an arbitrary substitution function was certified as symmetric")
  }

  test("a caller-supplied substitution cost is validated on the named operands") {
    val invalid = ScanMatch.similarity[Char]((_, _) => Double.NaN, unitGap)
    assert(
      invalid.compare(Vector('A'), Vector('B')) match
        case Left(CompareError.InvalidSubstitutionCost("ScanMatch", 0, 0, value)) =>
          value.isNaN
        case _ => false
    )
  }

end ScanMatchSuite

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
import eyes4s.kernel.Unit2D.Px

import scala.compiletime.testing.typeCheckErrors

class ComparisonWorkSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  /** Hand-calculated rational targets are compared at this named tolerance;
    * everything that must be bit-identical is compared exactly.
    */
  private val HandCalculated = 1e-12

  private val frame = get(Frame.screen("work", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val other = get(Grid.over(get(Frame.screen("work", 4, 4)), 2, 2))

  private def mass(g: Grid[Px], values: Double*): Mass[Px] =
    get(Surface.mass(g, IArray.from(values), Provenance.raw(ContentHash.empty)))

  private val cosine = Distribution.cosine[Px]

  /** Every step of one comparison at a fixed quantum, with the contract checked. */
  private def steps(
      cursor: ComparisonCursor[Similarity],
      quantum: Int
  ): (Vector[Int], Either[CompareError, Similarity]) =
    val q     = get(ComparisonQuantum.of(quantum))
    val units = Vector.newBuilder[Int]
    @annotation.tailrec
    def loop(cursor: ComparisonCursor[Similarity]): Either[CompareError, Similarity] =
      val before = cursor.remaining
      cursor.advance(q) match
        case ComparisonStep.More(work, next) =>
          assert(work > 0 && work <= quantum, clue((work, quantum)))
          assertEquals(next.remaining, before - work)
          units += work
          loop(next)
        case ComparisonStep.Done(work, result) =>
          assert(work >= 0 && work <= quantum, clue((work, quantum)))
          assertEquals(before, work.toLong)
          units += work
          result
    val result = loop(cursor)
    units.result() -> result

  test("hand-calculated cosine: identical, orthogonal and half-overlapping maps") {
    val a = mass(grid, 0.5, 0.5, 0.0, 0.0)
    val b = mass(grid, 0.5, 0.0, 0.5, 0.0)
    val c = mass(grid, 0.0, 0.0, 0.5, 0.5)
    // (0.5*0.5) / (sqrt(0.5) * sqrt(0.5)) = 0.25 / 0.5 = 0.5
    assertEqualsDouble(get(cosine.compare(a, b)).value, 0.5, HandCalculated)
    assertEqualsDouble(get(cosine.compare(a, a)).value, 1.0, HandCalculated)
    // Disjoint support: the dot product is exactly zero, so the result is exactly zero.
    assertEquals(get(cosine.compare(a, c)).value, 0.0)
    // (3/7, 4/7) . (4/7, 3/7) = 24/49 over (5/7)(5/7) = 25/49: exactly 24/25.
    val p = mass(grid, 3.0 / 7, 4.0 / 7, 0.0, 0.0)
    val q = mass(grid, 4.0 / 7, 3.0 / 7, 0.0, 0.0)
    assertEqualsDouble(get(cosine.compare(p, q)).value, 0.96, HandCalculated)
    assertEquals(cosine.compare(p, q), cosine.compare(q, p))
  }

  test("zero norms and incompatible grids are named outcomes with no work") {
    val zero = get(
      Surface.mass(grid, IArray(0.0, 0.0, 0.0, 0.0), Provenance.raw(ContentHash.empty), 1.0)
    )
    assertEquals(cosine.compare(zero, zero), Left(CompareError.ZeroNorm("cosine", 0.0, 0.0)))
    val a          = mass(grid, 0.5, 0.5, 0.0, 0.0)
    val elsewhere  = mass(other, 0.5, 0.5, 0.0, 0.0)
    val mismatched = cosine.start(a, elsewhere)
    assertEquals(mismatched.remaining, 0L)
    mismatched.advance(ComparisonQuantum.default) match
      case ComparisonStep.Done(0, Left(CompareError.Grids(_))) => ()
      case other => fail(s"expected an immediate grid refusal, got $other")
    assert(cosine.compare(a, elsewhere).isLeft)
  }

  test("every quantum cut yields the identical result and accounts for every cell") {
    val a     = mass(grid, 0.1, 0.2, 0.3, 0.4)
    val b     = mass(grid, 0.4, 0.3, 0.2, 0.1)
    val whole = get(cosine.compare(a, b)).value
    // 0.04 + 0.06 + 0.06 + 0.04 = 0.2 over 0.3: 2/3.
    assertEqualsDouble(whole, 2.0 / 3.0, HandCalculated)
    Vector(1, 2, 3, 4, 5, 1024).foreach { quantum =>
      val (units, result) = steps(cosine.start(a, b), quantum)
      assertEquals(get(result).value, whole, clue(quantum))
      assertEquals(units.sum, grid.size)
      assertEquals(units.size, math.max(1, (grid.size + quantum - 1) / quantum), clue(quantum))
    }
  }

  test("a long single comparison yields between bounded steps") {
    val wide  = get(Grid.over(get(Frame.screen("wide", 400, 400)), 400, 400))
    val cells = wide.size
    val left  = get(
      Surface.mass(
        wide,
        IArray.tabulate(cells)(i => (i + 1).toDouble / (cells.toDouble * (cells + 1) / 2)),
        Provenance.raw(ContentHash.empty),
        1e-6
      )
    )
    val right = get(
      Surface.mass(
        wide,
        IArray.tabulate(cells)(i => (cells - i).toDouble / (cells.toDouble * (cells + 1) / 2)),
        Provenance.raw(ContentHash.empty),
        1e-6
      )
    )
    val (units, result) = steps(cosine.start(left, right), 1000)
    assertEquals(units.size, 160)
    assert(units.init.forall(_ == 1000))
    assertEquals(units.sum, cells)
    assertEquals(get(result).value, get(cosine.compare(left, right)).value)
    assertEquals(ComparisonWork.complete(cosine.start(left, right))._2, cells.toLong)
  }

  test("declared work is refused before a cell is visited when it exceeds the budget") {
    val a = mass(grid, 0.5, 0.5, 0.0, 0.0)
    assertEquals(
      ComparisonWork.start(cosine, a, a, get(ComparisonBudget.of(3))).left.toOption,
      Some(ComparisonWorkError.WorkBudget("cosine", 4, 3))
    )
    assert(ComparisonWork.start(cosine, a, a, get(ComparisonBudget.of(4))).isRight)
    assertEquals(
      ComparisonBudget.of(-1).left.toOption,
      Some(ComparisonWorkError.InvalidBudget(-1))
    )
    assertEquals(
      ComparisonQuantum.of(0).left.toOption,
      Some(ComparisonWorkError.InvalidQuantum(0))
    )
    assertEquals(ComparisonBudget.default.maxWorkUnits, Long.MaxValue)
  }

  test("a scaled cosine is a supported bounded extension with the source's work") {
    val factor = 3.0
    val scaled = cosine.mapScore(
      MeasureInfo(
        "scaled cosine",
        "cosine times a factor",
        MeasureScale.Bounded(0, factor),
        None
      )
    )(s =>
      Similarity.of(s.value * factor).left.map(CompareError.InvalidScore("scaled cosine", _))
    )
    val a = mass(grid, 0.5, 0.5, 0.0, 0.0)
    val b = mass(grid, 0.5, 0.0, 0.5, 0.0)
    assertEqualsDouble(get(scaled.compare(a, b)).value, 1.5, HandCalculated)
    assertEquals(get(scaled.compare(a, b)).value, get(cosine.compare(a, b)).value * factor)
    Vector(1, 3).foreach { quantum =>
      val (units, result) = steps(scaled.start(a, b), quantum)
      assertEquals(units, steps(cosine.start(a, b), quantum)._1)
      assertEquals(get(result).value, get(cosine.compare(a, b)).value * factor)
    }
    val overflowing = cosine.mapScore(scaled.info)(s =>
      Similarity
        .of(s.value * Double.MaxValue * Double.MaxValue)
        .left
        .map(CompareError.InvalidScore("scaled cosine", _))
    )
    assertEquals(
      overflowing.compare(a, b),
      Left(
        CompareError.InvalidScore(
          "scaled cosine",
          ComparisonValueError.NonFiniteSimilarity(Double.PositiveInfinity)
        )
      )
    )
    val bounded: BoundedCompare[Mass[Px], Mass[Px], Similarity] = scaled
    assertEquals(bounded.info.name, "scaled cosine")
  }

  test("a synchronous comparison or a closure cannot be advertised as bounded work") {
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.kernel.*
        import eyes4s.kernel.Unit2D.Px
        val bounded: BoundedCompare[Mass[Px], Mass[Px], MeasureDistance] =
          Distribution.totalVariation[Px]
      """).nonEmpty,
      "a whole synchronous metric was accepted as bounded work"
    )
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.kernel.*
        import eyes4s.kernel.Unit2D.Px
        val closure: (Mass[Px], Mass[Px]) => Either[CompareError, Similarity] =
          (a, b) => Distribution.cosine[Px].compare(a, b)
        val bounded: BoundedCompare[Mass[Px], Mass[Px], Similarity] = closure
      """).nonEmpty,
      "an arbitrary closure was accepted as bounded work"
    )
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.kernel.*
        import eyes4s.kernel.Unit2D.Px
        val wrongScore: BoundedCompare[Mass[Px], Mass[Px], MeasureDistance] =
          Distribution.cosine[Px]
      """).nonEmpty,
      "a Similarity comparison was accepted as a MeasureDistance one"
    )
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.kernel.*
        import eyes4s.kernel.Unit2D.Px
        Distribution.cosine[Px].mapScore(Distribution.cosine[Px].info)((d: MeasureDistance) =>
          MeasureDistance.of(d.value).left.map(CompareError.InvalidScore("x", _))
        )
      """).nonEmpty,
      "mapScore accepted a transformation of the wrong score type"
    )
  }

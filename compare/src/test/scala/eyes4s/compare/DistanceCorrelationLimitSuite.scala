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

package eyes4s.compareconsumer

import eyes4s.compare.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class DistanceCorrelationLimitSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  /** The optimised pass and the full-matrix formulation it replaced sum in a
    * different order; they agree to rounding, not bit for bit.
    */
  private val Rounding = 1e-12

  private def grid(nx: Int, ny: Int): Grid[Px] =
    get(Grid.over(get(Frame.screen(s"dcor-${nx}x$ny", nx, ny)), nx, ny))

  private def mass(g: Grid[Px], f: Int => Double): Mass[Px] =
    get(
      Surface
        .intensity(g, IArray.tabulate(g.size)(f), Provenance.raw(ContentHash.empty))
        .flatMap(_.normalised)
    )

  private def limit(pairs: Long): DistanceCorrelationLimit = get(
    DistanceCorrelationLimit.of(pairs)
  )

  /** The previous implementation: both full n-by-n double-centred matrices. */
  private def fullMatrix(a: Mass[Px], b: Mass[Px]): Double =
    val n                                                 = a.size
    def means(v: IArray[Double]): (Array[Double], Double) =
      val rows = Array.fill(n)(0.0)
      for i <- 0 until n; j <- 0 until n do rows(i) += math.abs(v(i) - v(j)) / n
      (rows, rows.sum / n)
    val (ax, gx) = means(a.values); val (by, gy) = means(b.values)
    var xx       = 0.0; var yy                   = 0.0; var xy = 0.0
    for i <- 0 until n; j <- 0 until n do
      val x = math.abs(a.values(i) - a.values(j)) - ax(i) - ax(j) + gx
      val y = math.abs(b.values(i) - b.values(j)) - by(i) - by(j) + gy
      xx += x * x; yy += y * y; xy += x * y
    math.sqrt(math.max(0.0, math.min(1.0, xy / math.sqrt(xx) / math.sqrt(yy))))

  private val g100  = grid(10, 10)
  private val ramp  = mass(g100, i => (i + 1).toDouble)
  private val wave  = mass(g100, i => 2 + math.sin(i * 0.37))
  private val pairs = 4950L // 100 * 99 / 2

  test("pairs counts unordered distinct cell pairs without Int overflow") {
    assertEquals(DistanceCorrelationLimit.pairs(0), 0L)
    assertEquals(DistanceCorrelationLimit.pairs(1), 0L)
    assertEquals(DistanceCorrelationLimit.pairs(100), pairs)
    assertEquals(DistanceCorrelationLimit.pairs(65536), 2147450880L)
  }

  test("the bound refuses one pair below the request and admits it exactly and one above") {
    val unbounded = get(Distribution.distanceCorrelation[Px].compare(ramp, wave)).value
    assertEquals(
      Distribution.distanceCorrelationWithin[Px](limit(pairs - 1)).compare(ramp, wave),
      Left(CompareError.WorkLimitExceeded("distance correlation", 100, pairs, pairs - 1))
    )
    assertEquals(
      get(Distribution.distanceCorrelationWithin[Px](limit(pairs)).compare(ramp, wave)).value,
      unbounded
    )
    assertEquals(
      get(
        Distribution.distanceCorrelationWithin[Px](limit(pairs + 1)).compare(ramp, wave)
      ).value,
      unbounded
    )
  }

  test("the refusal names the measure, the cells, the pair count and the limit") {
    val error = Distribution
      .distanceCorrelationWithin[Px](limit(pairs - 1))
      .compare(ramp, wave)
      .left
      .toOption
      .get
    val message = error.message
    assert(message.contains("distance correlation"), clue(message))
    assert(message.contains("cells=100"), clue(message))
    assert(message.contains(s"pairs=$pairs"), clue(message))
    assert(message.contains(s"limit=${pairs - 1}"), clue(message))
  }

  test("the default admits 23,170 cells and refuses 23,171 before visiting a pair") {
    val most = DistanceCorrelationLimit.DefaultMaximumPairs
    assertEquals(DistanceCorrelationLimit.default.maximumPairs, most)
    assert(DistanceCorrelationLimit.pairs(23170) <= most)
    assert(DistanceCorrelationLimit.pairs(23171) > most)
    val big = grid(1363, 17) // 23,171 cells
    val a   = mass(big, i => (i % 97).toDouble + 1)
    val b   = mass(big, i => (i % 89).toDouble + 1)
    assertEquals(
      Distribution.distanceCorrelation[Px].compare(a, b),
      Left(
        CompareError.WorkLimitExceeded(
          "distance correlation",
          23171,
          DistanceCorrelationLimit.pairs(23171),
          most
        )
      )
    )
  }

  test("grid disagreement precedes the bound, and the bound precedes the constancy check") {
    val other   = mass(grid(20, 5), i => i.toDouble + 1)
    val refuses = Distribution.distanceCorrelationWithin[Px](limit(0))
    assert(
      refuses.compare(ramp, other).left.exists {
        case CompareError.Grids(_) => true
        case _                     => false
      }
    )
    val uniform = mass(g100, _ => 1.0)
    assertEquals(
      refuses.compare(uniform, uniform),
      Left(CompareError.WorkLimitExceeded("distance correlation", 100, pairs, 0L))
    )
  }

  test("below the bound the result matches the full-matrix formulation") {
    val rng = new scala.util.Random(20260925)
    for (nx, ny) <- Vector((1, 2), (2, 1), (3, 3), (10, 10), (17, 13), (40, 25)) do
      val g = grid(nx, ny)
      for _ <- 0 until 3 do
        val a = mass(g, _ => rng.nextDouble())
        val b = mass(g, _ => if rng.nextInt(4) == 0 then 0.0 else rng.nextDouble())
        val r = get(Distribution.distanceCorrelation[Px].compare(a, b)).value
        assertEqualsDouble(r, fullMatrix(a, b), Rounding, clue((nx, ny)))
        assertEqualsDouble(
          get(Distribution.distanceCorrelation[Px].compare(b, a)).value,
          r,
          Rounding
        )
    assertEqualsDouble(
      get(Distribution.distanceCorrelation[Px].compare(ramp, ramp)).value,
      1.0,
      Rounding
    )
  }

  test("a negative limit is a typed error; zero and Long.MaxValue are valid") {
    assertEquals(
      DistanceCorrelationLimit.of(-1L).map(_.maximumPairs),
      Left(ComparisonWorkError.InvalidBudget(-1L))
    )
    assertEquals(limit(0L).maximumPairs, 0L)
    assertEquals(limit(Long.MaxValue).maximumPairs, Long.MaxValue)
  }

  test("map comparison threads an explicit limit, and linear methods ignore it") {
    val one   = get(Sigma.px(1))
    val left  = get(MapScales.of(Vector(one -> ramp)))
    val right = get(MapScales.of(Vector(one -> wave)))
    val tight = limit(pairs - 1)
    assertEquals(
      MapComparison.scales(left, right, MapSimilarityMethod.DistanceCorrelation, tight).rows,
      Vector(
        one -> Left(
          MapScaleFailure.Comparison(
            CompareError.WorkLimitExceeded("distance correlation", 100, pairs, pairs - 1)
          )
        )
      )
    )
    assertEquals(
      MapComparison
        .scales(left, right, MapSimilarityMethod.DistanceCorrelation, limit(pairs))
        .rows
        .map(_._2),
      MapComparison.scales(left, right, MapSimilarityMethod.DistanceCorrelation).rows.map(_._2)
    )
    MapSimilarityMethod.values.filterNot(_ == MapSimilarityMethod.DistanceCorrelation).foreach {
      method =>
        assertEquals(
          method.instanceWithin[Px](limit(0)).compare(ramp, wave),
          method.instance[Px].compare(ramp, wave),
          clue(method)
        )
    }
  }

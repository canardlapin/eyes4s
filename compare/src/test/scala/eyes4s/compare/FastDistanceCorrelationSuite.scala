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

class FastDistanceCorrelationSuite extends munit.FunSuite:
  private val EstimatorRounding                 = 1e-12
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def grid(n: Int): Grid[Px]            =
    get(Grid.over(get(Frame.screen(s"fast-dcor-$n", n, 1)), n, 1))
  private def mass(g: Grid[Px], values: Vector[Double]): Mass[Px] =
    get(
      Surface
        .intensity(g, IArray.from(values), Provenance.raw(ContentHash.empty))
        .flatMap(_.normalised)
    )
  private def score(a: Mass[Px], b: Mass[Px]): Double =
    get(Distribution.distanceCorrelation[Px].compare(a, b)).value

  private def fullMatrix(a: Mass[Px], b: Mass[Px]): Double =
    val n                                                = a.size
    def centred(v: IArray[Double]): Array[Array[Double]] =
      val raw   = Array.tabulate(n, n)((i, j) => math.abs(v(i) - v(j)))
      val means = raw.map(_.sum / n)
      val grand = means.sum / n
      Array.tabulate(n, n)((i, j) => raw(i)(j) - means(i) - means(j) + grand)
    val x  = centred(a.values)
    val y  = centred(b.values)
    var xx = 0.0
    var yy = 0.0
    var xy = 0.0
    for i <- 0 until n; j <- 0 until n do
      xx += x(i)(j) * x(i)(j)
      yy += y(i)(j) * y(i)(j)
      xy += x(i)(j) * y(i)(j)
    math.sqrt(math.max(0.0, math.min(1.0, xy / math.sqrt(xx) / math.sqrt(yy))))

  test("all small ternary operands include exact ties and agree with double centering") {
    for n <- 2 to 4 do
      val g       = grid(n)
      val vectors = (0 until math.pow(3, n).toInt)
        .map { code =>
          Vector.tabulate(n)(i => ((code / math.pow(3, i).toInt) % 3).toDouble)
        }
        .filter(v => v.distinct.size > 1)
        .map(v => mass(g, v))
      for a <- vectors; b <- vectors do
        val actual = score(a, b)
        assertEqualsDouble(actual, fullMatrix(a, b), EstimatorRounding)
        assertEquals(score(b, a), actual)
  }

  test("dense, near-uniform, sparse and wide-range maps preserve the biased estimator") {
    val rng = new scala.util.Random(20261014)
    for n <- Vector(5, 25, 64, 128) do
      val g = grid(n)
      for kind <- 0 until 4; iteration <- 0 until 12 do
        def values(): Vector[Double] = Vector.tabulate(n) { _ =>
          kind match
            case 0 => rng.nextDouble()
            case 1 => 1.0 + rng.nextDouble() * 1e-10
            case 2 => if rng.nextInt(5) == 0 then rng.nextDouble() else 0.0
            case _ => math.pow(10, -250 * rng.nextDouble())
        }
        val x = values()
        val y = values()
        if x.distinct.size > 1 && y.distinct.size > 1 then
          val a      = mass(g, x)
          val b      = mass(g, y)
          val actual = score(a, b)
          assertEqualsDouble(
            actual,
            fullMatrix(a, b),
            EstimatorRounding,
            clue((n, kind, iteration))
          )
          assertEquals(score(b, a), actual)
  }

  test("large sparse binary maps match absolute Pearson correlation without cancellation") {
    val n = 65536
    val g = grid(n)
    val a = mass(g, Vector.tabulate(n)(i => if i == 0 then 1.0 else 0.0))
    val b = mass(g, Vector.tabulate(n)(i => if i == n - 1 then 1.0 else 0.0))
    assertEqualsDouble(score(a, b), 1.0 / (n - 1), EstimatorRounding)
    assertEqualsDouble(score(a, a), 1.0, EstimatorRounding)
    val quarter = mass(g, Vector.tabulate(n)(i => if i < n / 4 then 1.0 else 0.0))
    val half    = mass(g, Vector.tabulate(n)(i => if i < n / 2 then 1.0 else 0.0))
    assertEqualsDouble(score(quarter, half), 1.0 / math.sqrt(3.0), EstimatorRounding)
    assertEquals(score(half, quarter), score(quarter, half))
  }

  test("paired permutations retain the correlation when sort and rank ties change") {
    val n           = 64
    val g           = grid(n)
    val x           = Vector.tabulate(n)(i => (i % 7).toDouble)
    val y           = Vector.tabulate(n)(i => ((i * 13) % 11).toDouble)
    val original    = score(mass(g, x), mass(g, y))
    val permutation = new scala.util.Random(729).shuffle((0 until n).toVector)
    val permuted    = score(mass(g, permutation.map(x)), mass(g, permutation.map(y)))
    assertEqualsDouble(permuted, original, EstimatorRounding)
  }

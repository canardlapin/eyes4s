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

package eyes4s.surface

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.compiletime.testing.typeCheckErrors

class AnisotropicSuite extends munit.FunSuite:
  // Small-grid sums of exp evaluations: absolute error budget, not a fitted threshold.
  private val OracleTolerance                   = 1e-12
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def sigma(value: Double): Sigma[Px]   = get(Sigma.px(value))
  private val frame                             = get(Frame.screen("anisotropic", 12, 10))
  private val grid   = get(Grid.over(frame, 6, 10)) // rectangular cells: 2 x 1
  private val points = Vector((0, 0, 2.0), (2, 5, 3.0), (5, 9, 0.5))
  private def measure(g: Grid[Px], ps: Vector[(Int, Int, Double)]) = get(
    PointMeasure.of(
      g.frame,
      IArray.from(ps.map((x, y, _) => g.cellCentre(g.indexAt(x, y).get).get)),
      IArray.from(ps.map(_._3))
    )
  )
  private val input = measure(grid, points)

  // Independent direct 2-D source deposition: no separable passes, production kernel,
  // production binning or source-side factorisation. Full finite support is normalized
  // before clipping for Truncate; on-grid support is normalized for Renormalise.
  private def oracle(sx: Double, sy: Double, policy: EdgePolicy): Vector[Double] =
    val rx      = math.ceil(3 * sx / grid.cellWidth).toInt.max(1)
    val ry      = math.ceil(3 * sy / grid.cellHeight).toInt.max(1)
    val offsets = (for
      dy <- -ry to ry
      dx <- -rx to rx
    yield (
      dx,
      dy,
      math.exp(
        -0.5 * (
          math.pow(dx * grid.cellWidth / sx, 2) + math.pow(dy * grid.cellHeight / sy, 2)
        )
      )
    )).toVector
    val out = Array.fill(grid.size)(0.0)
    points.foreach { (x, y, mass) =>
      val onGrid      = offsets.filter((dx, dy, _) => grid.indexAt(x + dx, y + dy).isDefined)
      val denominator =
        (if policy == EdgePolicy.Truncate then offsets else onGrid).map(_._3).sum
      onGrid.foreach { (dx, dy, w) =>
        out(grid.indexAt(x + dx, y + dy).get) += mass * w / denominator
      }
    }
    out.toVector

  EdgePolicy.values.foreach { policy =>
    test(s"direct two-dimensional oracle agrees on rectangular cells: $policy") {
      val out = get(Smoother.anisotropic(sigma(2), sigma(0.7), policy).smooth(input, grid))
      out.values.toVector
        .zip(oracle(2, 0.7, policy))
        .foreach((a, b) => assertEqualsDouble(a, b, OracleTolerance))
      if policy == EdgePolicy.Renormalise then
        assertEqualsDouble(out.sum, input.total, OracleTolerance)
      else assert(out.sum < input.total)
    }

    test(s"equal axis widths preserve isotropic numerical output exactly: $policy") {
      val old  = get(Smoother.gaussian(sigma(2), policy).smooth(input, grid))
      val next = get(Smoother.anisotropic(sigma(2), sigma(2), policy).smooth(input, grid))
      assertEquals(next.values.toVector, old.values.toVector)
      assertNotEquals(next.provenance.digest, old.provenance.digest)
    }

    test(s"transposing coordinates, grid and sigmas transposes the estimate: $policy") {
      val other      = get(Frame.screen("transpose", 10, 12))
      val transposed = get(Grid.over(other, 10, 6))
      val ps         = points.map((x, y, w) => (y, x, w))
      val a = get(Smoother.anisotropic(sigma(2), sigma(0.7), policy).smooth(input, grid))
      val b = get(
        Smoother
          .anisotropic(sigma(0.7), sigma(2), policy)
          .smooth(measure(transposed, ps), transposed)
      )
      for x <- 0 until grid.nx; y <- 0 until grid.ny do
        assertEqualsDouble(
          a.unsafeAt(grid.indexAt(x, y).get),
          b.unsafeAt(transposed.indexAt(y, x).get),
          OracleTolerance
        )
    }

    test(s"weighted linearity and normalized density: $policy") {
      val s     = Smoother.anisotropic(sigma(2), sigma(0.7), policy)
      val all   = get(s.smooth(input, grid))
      val parts = points.map(p => get(s.smooth(measure(grid, Vector(p)), grid)))
      (0 until grid.size).foreach(i =>
        assertEqualsDouble(all.unsafeAt(i), parts.map(_.unsafeAt(i)).sum, OracleTolerance)
      )
      assertEqualsDouble(get(s.density(input, grid)).sum, 1.0, OracleTolerance)
    }
  }

  test("per-axis resolution failures identify the correct axis and operands") {
    assertEquals(
      Smoother.anisotropic(sigma(0.3), sigma(1), EdgePolicy.Truncate).smooth(input, grid),
      Left(EstimateError.DegenerateAxisBandwidth(SmoothingAxis.X, 0.3, 2.0))
    )
    assertEquals(
      Smoother.anisotropic(sigma(1), sigma(0.1), EdgePolicy.Truncate).smooth(input, grid),
      Left(EstimateError.DegenerateAxisBandwidth(SmoothingAxis.Y, 0.1, 1.0))
    )
    assert(
      Smoother
        .anisotropic(sigma(0.4), sigma(0.2), EdgePolicy.Truncate)
        .smooth(input, grid)
        .isRight
    )
  }

  test("empty and foreign-frame measures fail explicitly") {
    val s = Smoother.anisotropic(sigma(2), sigma(1), EdgePolicy.Truncate)
    assertEquals(s.smooth(PointMeasure.empty(frame), grid), Left(EstimateError.NoMass))
    val other = get(Frame.screen("other", 12, 10))
    assert(
      s.smooth(PointMeasure.empty(other), grid)
        .swap
        .toOption
        .exists(_.isInstanceOf[EstimateError.FrameMismatch])
    )
  }

  test("both widths and the edge convention affect provenance") {
    def hash(x: Double, y: Double, p: EdgePolicy) =
      get(Smoother.anisotropic(sigma(x), sigma(y), p).smooth(input, grid)).provenance.digest
    val hashes = Vector(
      hash(2, 1, EdgePolicy.Truncate),
      hash(1, 2, EdgePolicy.Truncate),
      hash(2, 2, EdgePolicy.Truncate),
      hash(2, 1, EdgePolicy.Renormalise)
    )
    assertEquals(hashes.distinct.size, hashes.size)
    assertEquals(
      Smoother.anisotropic(sigma(2), sigma(1), EdgePolicy.Truncate).bandwidth,
      KernelBandwidth.AxisAligned(sigma(2), sigma(1))
    )
  }

  test("the two bandwidths cannot mix spatial units") {
    assert(typeCheckErrors("""
      import eyes4s.surface.*
      import eyes4s.kernel.*
      Smoother.anisotropic(Sigma.px(2).toOption.get, Sigma.deg(1).toOption.get, EdgePolicy.Truncate)
    """).nonEmpty)
  }

  test("unrepresentable kernel support returns a typed error without allocating") {
    val s = Smoother.anisotropic(sigma(2), sigma(Double.MaxValue), EdgePolicy.Truncate)
    assertEquals(
      s.smooth(input, grid),
      Left(EstimateError.KernelSupportOverflow(SmoothingAxis.Y, Double.MaxValue, 1.0))
    )
  }

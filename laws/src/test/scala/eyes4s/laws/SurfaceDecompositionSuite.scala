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

package eyes4s.laws

import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

class SurfaceDecompositionSuite extends munit.FunSuite:
  private val oracleTolerance               = Tolerance(absolute = 1e-12, relative = 1e-12)
  private val illConditionedTolerance       = Tolerance(absolute = 1e-5, relative = 1e-7)
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private val frame                         = get(Frame.angular("decomposition", 20, 20))
  private val grid                          = get(Grid.over(frame, 3, 2))
  private val a                             = get(PredictorId.of("a"))
  private val b                             = get(PredictorId.of("b"))
  private def mass(values: Vector[Double], on: Grid[Deg] = grid): Mass[Deg] =
    get(
      Surface.mass(on, IArray.from(values), Provenance.raw(ContentHash.of(IArray.from(values))))
    )
  private def near(
      actual: Double,
      expected: Double,
      tolerance: Tolerance = oracleTolerance
  ): Unit =
    assert(tolerance.approxEquals(actual, expected), s"$actual != $expected under $tolerance")

  DecompositionReference.cases.foreach { c =>
    test(s"${c.name}: native surface OLS agrees with exact rational and pinned public lm") {
      val predictors = get(PredictorSet.of(Vector(a -> mass(c.a), b -> mass(c.b))))
      val policy     = if c.intercept then Intercept.Include else Intercept.Exclude
      val fit        = get(Template.decompose(mass(c.y), predictors, policy))
      val beta       = fit.intercept.toVector ++ fit.coefficients.map(_._2)
      beta.zip(c.coefficients).foreach((x, y) => near(x, y))
      assertEquals(fit.coefficients.map(_._1), Vector(a, b))
      fit.fitted.values.toVector.zip(c.fitted).foreach((x, y) => near(x, y))
      fit.residual.values.toVector.zip(c.residuals).foreach((x, y) => near(x, y))
      near(fit.diagnostics.rSquared.get, c.rSquared)
      near(fit.diagnostics.residualSumSquares, c.residuals.map(r => r * r).sum)
      assertEquals(fit.diagnostics.rank, c.coefficients.size)
      assert(fit.diagnostics.scaledDiagonalRatio.isFinite)
      val reversed = get(
        Template.decompose(
          mass(c.y),
          get(PredictorSet.of(predictors.entries.reverse)),
          policy
        )
      )
      reversed.coefficients.toMap.foreach((key, value) =>
        near(value, fit.coefficients.toMap.apply(key))
      )
      assertNotEquals(fit.fitted.provenance.digest, fit.residual.provenance.digest)
    }
  }

  test("rank refusal retains every column rather than dropping a predictor") {
    val m          = mass(Vector(0.5, 0, 0, 0.5, 0, 0))
    val predictors = get(PredictorSet.of(Vector(a -> m, b -> m)))
    assert(Template.decompose(m, predictors, Intercept.Exclude) match
      case Left(
            DecompositionError
              .Solve(keys, Intercept.Exclude, LeastSquaresError.RankDeficient(1, _, _))
          ) =>
        keys == Vector(a, b)
      case _ => false)
    assert(LeastSquares.fit(Vector(Vector(1.0, 2.0)), Vector(3.0)).isLeft)
    assert(LeastSquares.fit(Vector(Vector(1.0), Vector(1.0, 2.0)), Vector(3.0, 4.0)).isLeft)
    assert(LeastSquares.fit(Vector(Vector(Double.NaN)), Vector(3.0)).isLeft)
    assert(LeastSquares.fit(Vector(Vector(1.0)), Vector(Double.PositiveInfinity)).isLeft)
    assert(RelativeRankTolerance.of(0.0).isLeft)
  }

  test("nominal predictor and grid identity survive validation") {
    val m = mass(Vector(0.5, 0, 0, 0.5, 0, 0))
    assert(PredictorId.of(" ").isLeft)
    assert(PredictorSet.of(Vector.empty[(PredictorId, Mass[Deg])]).isLeft)
    assert(PredictorSet.of(Vector(a -> m, a -> m)).isLeft)
    val foreign = mass(m.values.toVector, get(Grid.of(GridId("foreign"), frame, 3, 2)))
    assert(PredictorSet.of(Vector(a -> m, b -> foreign)).isLeft)
    assert(
      Template
        .decompose(foreign, get(PredictorSet.of(Vector(a -> m))), Intercept.Exclude)
        .isLeft
    )
  }

  test("constant-response centered R-squared is explicitly undefined") {
    val x   = mass(Vector(0.5, 0, 0, 0.5, 0, 0))
    val y   = mass(Vector.fill(6)(1.0 / 6.0))
    val fit =
      get(Template.decompose(y, get(PredictorSet.of(Vector(a -> x))), Intercept.Include))
    assertEquals(fit.diagnostics.rSquared, None)
    near(fit.intercept.get, 1.0 / 6.0)
    near(fit.coefficients.head._2, 0.0)
  }

  test("column scaling retains coefficients across extreme physical magnitudes") {
    val rows = Vector(Vector(1e-100, 0.0), Vector(0.0, 1e100), Vector(1e-100, 1e100))
    val fit  = get(LeastSquares.fit(rows, Vector(2.0, 3.0, 5.0)))
    near(fit.coefficients(0) / 1e100, 2.0)
    near(fit.coefficients(1) / 1e-100, 3.0)
    fit.residuals.foreach(r => near(r, 0.0))
  }

  test("nearly collinear columns use the stated relative rank policy") {
    val rows = Vector(0.0, 1.0, 2.0, 3.0, 4.0, 5.0).zipWithIndex.map { case (x, i) =>
      Vector(1.0 + x, 1.0 + x + (if i % 2 == 0 then 1e-8 else -1e-8))
    }
    val y = rows.map(r => 2.0 * r(0) - r(1))
    // The default (1e-7, as lm) refuses a 1e-8 perturbation; a caller who
    // explicitly accepts a looser policy still recovers the exact coefficients.
    assert(LeastSquares.fit(rows, y).isLeft)
    val fit = get(LeastSquares.fit(rows, y, get(RelativeRankTolerance.of(1e-12))))
    near(fit.coefficients(0), 2.0, illConditionedTolerance)
    near(fit.coefficients(1), -1.0, illConditionedTolerance)
    assert(LeastSquares.fit(rows, y, get(RelativeRankTolerance.of(1e-6))).isLeft)
  }

  test("a near-collinear design with a nonzero residual is rank deficient by default") {
    // x2 = x1 +/- 1e-10: the response is not in the span of either column alone,
    // so a 1e-12 threshold admits coefficients near +/-3e9 made of rounding noise.
    val rows = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0).zipWithIndex.map { case (x, i) =>
      Vector(x, x + (if i % 2 == 0 then 1e-10 else -1e-10))
    }
    val y = Vector(1.0, 0.0, 2.0, 1.0, 3.0, 5.0)
    assert(
      LeastSquares.fit(rows, y) match
        case Left(LeastSquaresError.RankDeficient(1, pivot, threshold)) =>
          pivot <= threshold && threshold == RelativeRankTolerance.default.value
        case other => fail(s"expected a rank refusal, found $other")
    )
  }

  test("arithmetic failures name only the operands that exist") {
    val overflow = LeastSquares.fit(
      Vector(Vector(Double.MaxValue), Vector(Double.MaxValue)),
      Vector(1.0, 1.0)
    )
    assertEquals(
      overflow.left.map(_.message),
      Left("Least squares column norm is nonfinite at column=0.")
    )
    assertEquals(overflow, Left(LeastSquaresError.ColumnArithmetic("column norm", 0)))
    assertEquals(
      LeastSquaresError.RowArithmetic("prediction/residual", 3).message,
      "Least squares prediction/residual is nonfinite at row=3."
    )
  }

  test("OLS fitted maps and residuals cannot be used as masses") {
    assert(
      scala.compiletime.testing
        .typeCheckErrors("""
      import eyes4s.design.*
      import eyes4s.kernel.*
      def invalid[U <: Unit2D](fit: SurfaceOlsFit[U]): Mass[U] = fit.fitted
    """).nonEmpty
    )
  }

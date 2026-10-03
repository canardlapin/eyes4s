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
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll

/** NNLS, simplex mixtures and partial association against exhaustive exact
  * rational oracles (`ExactDecompositionOracle`) on the JVM and Scala.js.
  */
class ConstrainedDecompositionSuite extends munit.ScalaCheckSuite:
  import ExactDecompositionOracle as Exact

  /** Solver rounding on small, well-separated designs; inputs are exact. */
  private val oracleTolerance = Tolerance(absolute = 1e-12, relative = 1e-9)

  /** A simplex sum, which elimination preserves to a few units in the last place. */
  private val sumTolerance = Tolerance(absolute = 1e-14, relative = 0.0)

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)
  private val frame                         = get(Frame.angular("constrained", 20, 20))
  private val grids = (3 to 12).map(n => n -> get(Grid.over(frame, n, 1))).toMap
  private val ids   = Vector("a", "b", "c", "d").map(s => get(PredictorId.of(s)))

  private def mass(weights: Vector[Int]): Mass[Deg] =
    val total  = weights.sum.toDouble
    val values = IArray.from(weights.map(_ / total))
    get(Surface.mass(grids(weights.size), values, Provenance.raw(ContentHash.of(values))))

  private def exact(m: Surface[Deg]): Vector[Q] = m.values.toVector.map(Q.of)

  private def near(actual: Double, expected: Q, tolerance: Tolerance = oracleTolerance): Unit =
    assert(
      tolerance.approxEquals(actual, expected.toDouble),
      s"$actual != ${expected.toDouble} under $tolerance"
    )

  /** Integer cell weights; predictor k alone occupies cell k, so every design
    * has full column rank and moderate conditioning.
    */
  private final case class Design(predictors: Vector[Vector[Int]], response: Vector[Int]):
    def set: PredictorSet[Deg] = get(PredictorSet.of(ids.zip(predictors.map(mass))))
    def y: Mass[Deg]           = mass(response)

  private val genDesign: Gen[Design] =
    for
      p     <- Gen.choose(1, 4)
      n     <- Gen.choose(p + 3, 12)
      cells <- Gen.listOfN(p, Gen.listOfN(n, Gen.choose(0, 9)))
      y     <- Gen.listOfN(n, Gen.choose(0, 9))
      lift  <- Gen.choose(0, n - 1)
    yield Design(
      cells.zipWithIndex.map { (c, k) =>
        c.toVector.zipWithIndex.map((w, i) => if i == k then 30 else if i < p then 0 else w)
      }.toVector,
      y.toVector.updated(lift, y(lift) + 1)
    )

  property("NNLS equals the exhaustive exact KKT solution and is an Intensity") {
    forAll(genDesign) { d =>
      val fit    = get(Template.decomposeNonNegative(d.y, d.set))
      val oracle = Exact.nonNegative(d.set.entries.map(e => exact(e._2)), exact(d.y))
      assertEquals(fit.coefficients.map(_._1), ids.take(d.predictors.size))
      fit.coefficients.map(_._2).zip(oracle).foreach((x, q) => near(x, q))
      assert(fit.coefficients.forall(_._2 >= 0.0))
      assertEquals(fit.diagnostics.active, oracle.count(_.signum > 0))
      assertEquals(fit.diagnostics.rank, d.predictors.size)
      assertEquals(fit.diagnostics.rSquaredReference, RSquaredReference.Uncentered)
      assert(fit.diagnostics.dualViolation <= RelativeDualTolerance.default.value)
      val fitted = d.set.entries.indices.foldLeft(Vector.fill(d.y.size)(Q.zero)) { (acc, k) =>
        acc.zip(exact(d.set.entries(k)._2)).map((s, x) => s + x * oracle(k))
      }
      fit.fitted.values.toVector.zip(fitted).foreach((x, q) => near(x, q))
      fit.residual.values.toVector
        .zip(exact(d.y).zip(fitted).map(_ - _))
        .foreach((x, q) => near(x, q))
      val rss = Q.sum(exact(d.y).zip(fitted).map((a, b) => (a - b) * (a - b)))
      near(fit.diagnostics.rSquared.get, Q.one - rss / Q.sum(exact(d.y).map(v => v * v)))
    }
  }

  property("simplex mixtures equal the exhaustive exact KKT solution and are a Mass") {
    forAll(genDesign, Gen.oneOf(Intercept.Exclude, Intercept.Include)) { (d, intercept) =>
      val fit     = get(Template.decomposeMixture(d.y, d.set, intercept))
      val uniform = Vector.fill(d.y.size)(Q.of(1.0 / d.y.size))
      val columns =
        (if intercept == Intercept.Include then Vector(uniform) else Vector.empty) ++
          d.set.entries.map(e => exact(e._2))
      val oracle  = Exact.simplex(columns, exact(d.y))
      val weights = fit.background.toVector ++ fit.weights.map(_._2)
      assertEquals(fit.background.isDefined, intercept == Intercept.Include)
      weights.zip(oracle).foreach((x, q) => near(x, q))
      assert(weights.forall(_ >= 0.0))
      assert(sumTolerance.approxEquals(weights.sum, 1.0), s"${weights.sum}")
      assert(sumTolerance.approxEquals(fit.fitted.sum, 1.0))
      assertEquals(fit.diagnostics.active, oracle.count(_.signum > 0))
      assertEquals(fit.diagnostics.rSquaredReference, RSquaredReference.Centered)
      assert(fit.diagnostics.dualViolation <= RelativeDualTolerance.default.value)
      val fitted = columns.indices.foldLeft(Vector.fill(d.y.size)(Q.zero)) { (acc, k) =>
        acc.zip(columns(k)).map((s, x) => s + x * oracle(k))
      }
      fit.fitted.values.toVector.zip(fitted).foreach((x, q) => near(x, q))
      fit.residual.values.toVector
        .zip(exact(d.y).zip(fitted).map(_ - _))
        .foreach((x, q) => near(x, q))
    }
  }

  property("keyed results do not depend on predictor order") {
    forAll(genDesign) { d =>
      val reversed = get(PredictorSet.of(d.set.entries.reverse))
      val nnls     = get(Template.decomposeNonNegative(d.y, d.set)).coefficients.toMap
      val mixture  = get(Template.decomposeMixture(d.y, d.set, Intercept.Exclude)).weights.toMap
      get(Template.decomposeNonNegative(d.y, reversed)).coefficients.foreach((k, v) =>
        assert(oracleTolerance.approxEquals(v, nnls(k)))
      )
      get(Template.decomposeMixture(d.y, reversed, Intercept.Exclude)).weights.foreach((k, v) =>
        assert(oracleTolerance.approxEquals(v, mixture(k)))
      )
    }
  }

  property("constraints never beat OLS, and the simplex never beats NNLS") {
    forAll(genDesign) { d =>
      val ols     = get(Template.decompose(d.y, d.set, Intercept.Exclude)).diagnostics
      val nnls    = get(Template.decomposeNonNegative(d.y, d.set)).diagnostics
      val mixture = get(Template.decomposeMixture(d.y, d.set, Intercept.Exclude)).diagnostics
      val slack   = 1e-15
      Prop(
        ols.residualSumSquares <= nnls.residualSumSquares + slack &&
          nnls.residualSumSquares <= mixture.residualSumSquares + slack
      )
    }
  }

  test("an exact mixture is recovered, with and without a uniform background") {
    val a     = mass(Vector(4, 0, 0, 2, 2, 0))
    val b     = mass(Vector(0, 3, 1, 0, 1, 3))
    val set   = get(PredictorSet.of(Vector(ids(0) -> a, ids(1) -> b)))
    val mixed = get(
      Surface.mass(
        a.grid,
        IArray.tabulate(6)(i => 0.25 * a.values(i) + 0.75 * b.values(i)),
        Provenance.raw(ContentHash.ofString("mixed"))
      )
    )
    val fit = get(Template.decomposeMixture(mixed, set, Intercept.Exclude))
    near(fit.weights(0)._2, Q.int(1) / Q.int(4))
    near(fit.weights(1)._2, Q.int(3) / Q.int(4))
    assertEquals(fit.background, None)
    fit.residual.values.foreach(r => near(r, Q.zero))
    near(fit.diagnostics.rSquared.get, Q.one)
    val withBackground = get(
      Surface.mass(
        a.grid,
        IArray.tabulate(6)(i => 0.5 / 6 + 0.5 * a.values(i)),
        Provenance.raw(ContentHash.ofString("background"))
      )
    )
    val bg = get(Template.decomposeMixture(withBackground, set, Intercept.Include))
    near(bg.background.get, Q.int(1) / Q.int(2))
    near(bg.weights(0)._2, Q.int(1) / Q.int(2))
    near(bg.weights(1)._2, Q.zero)
    assertEquals(bg.diagnostics.active, 2)
  }

  test("simplex gradients are compared with the equality multiplier, not with zero") {
    // At the closest vertex a, the residual r = (-0.6, 0, 0.6) gives a multiplier
    // a.r = -0.12 and b.r = 0: b still lowers the residual, though its raw
    // gradient is not positive.
    val a   = mass(Vector(6, 0, 4))
    val b   = mass(Vector(3, 4, 3))
    val y   = mass(Vector(0, 0, 1))
    val fit = get(
      Template.decomposeMixture(
        y,
        get(PredictorSet.of(Vector(ids(0) -> a, ids(1) -> b))),
        Intercept.Exclude
      )
    )
    val oracle = Exact.simplex(Vector(exact(a), exact(b)), exact(y))
    assert(oracle(1).signum > 0)
    fit.weights.map(_._2).zip(oracle).foreach((x, q) => near(x, q))
  }

  test("NNLS zeroes a predictor whose unconstrained coefficient is negative") {
    val a   = mass(Vector(4, 0, 2, 2))
    val b   = mass(Vector(0, 4, 2, 2))
    val y   = mass(Vector(6, 0, 1, 1))
    val set = get(PredictorSet.of(Vector(ids(0) -> a, ids(1) -> b)))
    assert(get(Template.decompose(y, set, Intercept.Exclude)).coefficients(1)._2 < 0.0)
    val fit    = get(Template.decomposeNonNegative(y, set))
    val oracle = Exact.nonNegative(Vector(exact(a), exact(b)), exact(y))
    assertEquals(fit.coefficients(1)._2, 0.0)
    near(fit.coefficients(0)._2, oracle(0))
    assertEquals(fit.diagnostics.active, 1)
  }

  test("a zero NNLS response has zero coefficients and takes no step") {
    val rows = Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0))
    val fit  = get(ConstrainedLeastSquares.nonNegative(rows, Vector(0.0, 0.0, 0.0)))
    assertEquals(fit.coefficients, Vector(0.0, 0.0))
    assertEquals(fit.active, 0)
    assertEquals(fit.iterations, 0)
  }

  test("rank, shape, tolerance and grid failures are named values") {
    val a = mass(Vector(1, 0, 0, 1))
    val y = mass(Vector(1, 1, 1, 1))
    assert(
      Template
        .decomposeNonNegative(y, get(PredictorSet.of(Vector(ids(0) -> a, ids(1) -> a)))) match
        case Left(
              DecompositionError.Solve(
                keys,
                Intercept.Exclude,
                LeastSquaresError.RankDeficient(1, _, _)
              )
            ) =>
          keys == ids.take(2)
        case _ => false
    )
    // The uniform background is collinear with a uniform predictor.
    assert(
      Template
        .decomposeMixture(a, get(PredictorSet.of(Vector(ids(0) -> y))), Intercept.Include) match
        case Left(
              DecompositionError
                .Solve(_, Intercept.Include, LeastSquaresError.RankDeficient(1, _, _))
            ) =>
          true
        case _ => false
    )
    assertEquals(RelativeDualTolerance.of(0.0), Left(LeastSquaresError.DualTolerance(0.0)))
    assert(RelativeDualTolerance.of(1.0).isLeft)
    assert(RelativeDualTolerance.of(Double.NaN).isLeft)
    assertEquals(get(RelativeDualTolerance.of(1e-8)).value, 1e-8)
    assert(ConstrainedLeastSquares.simplex(Vector(Vector(1.0, 2.0)), Vector(1.0)).isLeft)
    val foreign = get(
      Surface.mass(
        get(Grid.of(GridId("foreign"), frame, 4, 1)),
        a.values,
        Provenance.raw(ContentHash.ofString("foreign"))
      )
    )
    val set = get(PredictorSet.of(Vector(ids(0) -> a)))
    assert(Template.decomposeNonNegative(foreign, set) match
      case Left(DecompositionError.Geometry("response", _)) => true
      case _                                                => false)
    assert(Template.decomposeMixture(foreign, set, Intercept.Exclude).isLeft)
    assert(PartialAssociation.of(foreign, y, set, AssociationMethod.Pearson) match
      case Left(DecompositionError.Geometry("x", _)) => true
      case _                                         => false)
    assert(PartialAssociation.of(y, foreign, set, AssociationMethod.Pearson) match
      case Left(DecompositionError.Geometry("y", _)) => true
      case _                                         => false)
    assertEquals(
      LeastSquaresError.NotConverged(60, 60, 0.5).message,
      "Active-set least squares stopped after iterations=60 at limit=60 with relative dual violation=0.5."
    )
  }

  test("fits state their representation: NNLS is not a Mass; the simplex fit is") {
    assert(
      scala.compiletime.testing
        .typeCheckErrors("""
      import eyes4s.design.*
      import eyes4s.kernel.*
      def invalid[U <: Unit2D](fit: SurfaceNnlsFit[U]): Mass[U] = fit.fitted
    """).nonEmpty
    )
    assert(
      scala.compiletime.testing
        .typeCheckErrors("""
      import eyes4s.design.*
      import eyes4s.kernel.*
      def invalid[U <: Unit2D](fit: SurfaceMixtureFit[U]): Mass[U] = fit.residual
    """).nonEmpty
    )
    assert(
      scala.compiletime.testing
        .typeCheckErrors("""
      import eyes4s.design.*
      import eyes4s.kernel.*
      def valid[U <: Unit2D](fit: SurfaceMixtureFit[U]): Mass[U] = fit.fitted
      def intensity[U <: Unit2D](fit: SurfaceNnlsFit[U]): Intensity[U] = fit.fitted
    """).isEmpty
    )
  }

  // ---------------------------------------------------------------- partial association

  private val genAssociation: Gen[(Vector[Int], Vector[Int], Vector[Vector[Int]])] =
    for
      k  <- Gen.choose(1, 2)
      n  <- Gen.choose(k + 5, 12)
      x  <- Gen.listOfN(n, Gen.choose(1, 9))
      y  <- Gen.listOfN(n, Gen.choose(1, 9))
      zs <- Gen.listOfN(k, Gen.listOfN(n, Gen.choose(1, 9)))
    yield (x.toVector, y.toVector, zs.map(_.toVector).toVector)

  private def checkAssociation(method: AssociationMethod) =
    forAll(genAssociation) { (xw, yw, zw) =>
      val (x, y) = (mass(xw), mass(yw))
      val set    = get(PredictorSet.of(ids.zip(zw.map(mass))))
      val rank   =
        (c: Vector[Q]) => if method == AssociationMethod.Spearman then Exact.ranks(c) else c
      val oracle =
        Exact.partial((Vector(x, y) ++ set.entries.map(_._2)).map(s => rank(exact(s))))
      PartialAssociation.of(x, y, set, method) match
        case Right(result) =>
          assertEquals(result.method, method)
          assertEquals(result.covariates, ids.take(zw.size))
          assertEquals(result.cells, xw.size)
          oracle match
            case Some((sign, squared)) =>
              val expected = sign * math.sqrt(squared.toDouble)
              val actual   = result.estimate.getOrElse(fail("defined exactly, undefined here"))
              assert(oracleTolerance.approxEquals(actual, expected), s"$actual != $expected")
              assert(math.abs(actual) <= 1.0)
              val swapped = get(PartialAssociation.of(y, x, set, method)).estimate.get
              assert(oracleTolerance.approxEquals(swapped, actual))
              val reordered =
                get(
                  PartialAssociation.of(x, y, get(PredictorSet.of(set.entries.reverse)), method)
                )
              assert(oracleTolerance.approxEquals(reordered.estimate.get, actual))
            case None => assertEquals(result.estimate, None)
        // Ranking can make covariates collinear; the exact covariance is then singular too.
        case Left(
              DecompositionError.Solve(_, Intercept.Include, _: LeastSquaresError.RankDeficient)
            ) =>
          assertEquals(oracle, None)
        case Left(other) => fail(s"$other")
    }

  property("partial Pearson equals the exact inverse-covariance partial correlation") {
    checkAssociation(AssociationMethod.Pearson)
  }

  property("partial Spearman equals the exact average-rank inverse-covariance value") {
    checkAssociation(AssociationMethod.Spearman)
  }

  test("a surface the covariates explain has no partial association") {
    val z1  = mass(Vector(1, 2, 3, 4, 5, 6))
    val z2  = mass(Vector(6, 1, 1, 2, 5, 3))
    val y   = mass(Vector(2, 7, 1, 8, 2, 8))
    val set = get(PredictorSet.of(Vector(ids(0) -> z1, ids(1) -> z2)))
    // x = 0.4 z1 + 0.6 z2 lies exactly in the span of the intercept and covariates.
    val x = get(
      Surface.mass(
        z1.grid,
        IArray.tabulate(6)(i => 0.4 * z1.values(i) + 0.6 * z2.values(i)),
        Provenance.raw(ContentHash.ofString("explained"))
      )
    )
    assertEquals(
      get(PartialAssociation.of(x, y, set, AssociationMethod.Pearson)).estimate,
      None
    )
    val uniform = mass(Vector.fill(6)(1))
    assertEquals(
      get(PartialAssociation.of(uniform, y, set, AssociationMethod.Spearman)).estimate,
      None
    )
    val defined = get(
      PartialAssociation.of(
        y,
        z1,
        get(PredictorSet.of(Vector(ids(1) -> z2))),
        AssociationMethod.Pearson
      )
    )
    assert(defined.estimate.isDefined)
    assertNotEquals(
      defined.provenance.digest,
      get(
        PartialAssociation.of(
          y,
          z1,
          get(PredictorSet.of(Vector(ids(1) -> z2))),
          AssociationMethod.Spearman
        )
      ).provenance.digest
    )
  }

  test("Spearman ranks share the mean rank of a tie") {
    val x               = mass(Vector(3, 1, 3, 2, 0, 3))
    val y               = mass(Vector(1, 2, 3, 4, 5, 6))
    val z               = mass(Vector(5, 1, 4, 1, 2, 6))
    val set             = get(PredictorSet.of(Vector(ids(0) -> z)))
    val (sign, squared) = Exact
      .partial(Vector(x, y, z).map(s => Exact.ranks(exact(s))))
      .getOrElse(fail("singular"))
    val r = get(PartialAssociation.of(x, y, set, AssociationMethod.Spearman)).estimate.get
    assert(oracleTolerance.approxEquals(r, sign * math.sqrt(squared.toDouble)))
    assertEquals(
      Exact.ranks(exact(x)),
      Vector(10, 4, 10, 6, 2, 10).map(v => Q.int(v) / Q.int(2))
    )
  }

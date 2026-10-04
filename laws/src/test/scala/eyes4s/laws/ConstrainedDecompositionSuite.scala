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

  /** The active count lies between the oracle's material and positive
    * counts. The oracle solves the rounded inputs exactly, so it can carry a
    * positive weight below the absolute tolerance whose gradient is below the
    * dual tolerance; the solver rightly leaves that predictor inactive.
    */
  private def activeMatches(active: Int, oracle: Vector[Q]): Unit =
    val material = oracle.count(q => q.toDouble > oracleTolerance.absolute)
    assert(
      material <= active && active <= oracle.count(_.signum > 0),
      s"active=$active oracle=${oracle.map(_.toDouble)}"
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

  /** Near-collinear predictors: a shared integer profile plus small private
    * noise, with a small private cell each for full column rank. Their
    * unconstrained coefficients are often negative, so constraints bind.
    */
  private val genCompeting: Gen[Design] =
    for
      p      <- Gen.choose(2, 4)
      n      <- Gen.choose(p + 3, 12)
      shared <- Gen.listOfN(n - p, Gen.choose(1, 9))
      noise  <- Gen.listOfN(p, Gen.listOfN(n - p, Gen.choose(0, 2)))
      own    <- Gen.listOfN(p, Gen.choose(1, 3))
      y      <- Gen.listOfN(n, Gen.choose(0, 9))
      lift   <- Gen.choose(0, n - 1)
    yield Design(
      Vector.tabulate(p) { k =>
        Vector.tabulate(p)(i => if i == k then own(k) else 0) ++
          shared.zip(noise(k)).map((s, e) => 3 * s + e)
      },
      y.toVector.updated(lift, y(lift) + 1)
    )

  /** A decoy close to the sum of two components that, with noise, make up the
    * response, plus an unrelated fourth predictor; the order is shuffled. The
    * decoy is the best single predictor, so it enters first and is often
    * stepped out once both components are in: Lawson-Hanson interpolates.
    */
  private val genDecoy: Gen[Design] =
    for
      m     <- Gen.choose(4, 8)
      b     <- Gen.listOfN(m, Gen.choose(0, 9))
      c     <- Gen.listOfN(m, Gen.choose(0, 9))
      off   <- Gen.listOfN(m, Gen.choose(0, 4))
      other <- Gen.listOfN(m, Gen.choose(0, 9))
      own   <- Gen.listOfN(4, Gen.choose(1, 2))
      order <- Gen.listOfN(4, Gen.choose(0, 1000))
      ownY  <- Gen.listOfN(4, Gen.choose(0, 2))
      noise <- Gen.listOfN(m, Gen.choose(0, 2))
      lift  <- Gen.choose(0, m + 3)
    yield
      val decoy  = b.lazyZip(c).lazyZip(off).map(_ + _ + _)
      val shared = Vector(decoy, b, c, other).map(_.toVector)
      val cols   = Vector.tabulate(4)(k =>
        Vector.tabulate(4)(i => if i == k then own(k) else 0) ++ shared(k)
      )
      val y = ownY.toVector ++ b.lazyZip(c).lazyZip(noise).map(_ + _ + _)
      Design(cols.indices.sortBy(order).map(cols).toVector, y.updated(lift, y(lift) + 1))

  private val genAnyDesign: Gen[Design] =
    Gen.frequency(1 -> genDesign, 1 -> genCompeting, 2 -> genDecoy)

  property("NNLS equals the exhaustive exact KKT solution and is an Intensity") {
    forAll(genAnyDesign) { d =>
      val fit    = get(Template.decomposeNonNegative(d.y, d.set))
      val oracle = Exact.nonNegative(d.set.entries.map(e => exact(e._2)), exact(d.y))
      assertEquals(fit.coefficients.map(_._1), ids.take(d.predictors.size))
      fit.coefficients.map(_._2).zip(oracle).foreach((x, q) => near(x, q))
      // Exactly zero where the oracle is zero: no rounding leak either side.
      assert(fit.coefficients.forall(_._2 >= 0.0))
      fit.coefficients
        .zip(oracle)
        .foreach((c, q) => if q.signum == 0 then assertEquals(c._2, 0.0))
      activeMatches(fit.diagnostics.active, oracle)
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
    forAll(genAnyDesign, Gen.oneOf(Intercept.Exclude, Intercept.Include)) { (d, intercept) =>
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
      activeMatches(fit.diagnostics.active, oracle)
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
    forAll(genAnyDesign) { d =>
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
    forAll(genAnyDesign) { d =>
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

  test("generated designs make constraints bind and take interpolation steps") {
    // Without this floor the oracle properties can pass on designs whose
    // unconstrained solution is already feasible, testing no active-set step.
    val designs = Gen
      .listOfN(400, genAnyDesign)
      .apply(Gen.Parameters.default, org.scalacheck.rng.Seed(20261003L))
      .getOrElse(fail("generator"))
    val fits = designs.map(d => d -> get(Template.decomposeNonNegative(d.y, d.set)).diagnostics)
    val binding = fits.count((d, f) => f.active < d.predictors.size)
    // Each entry is one iteration; any more are interpolation steps.
    val stepped = fits.count((_, f) => f.iterations > f.active)
    assert(binding >= 400 / 5, s"binding=$binding of 400")
    assert(stepped >= 400 / 40, s"stepped=$stepped of 400")
  }

  test("NNLS steps back to the first zero, keeping a coefficient that recovers") {
    // a = (2,1,2,2), b = (1,3,2,0), c = (2,0,2,3), y = (4,4,4,0). b enters
    // (x_b = 12/7), then c ((156, 40)/101), then a. The three-column solution
    // (112, -12, -72)/17 makes b and c non-positive; c reaches zero first, at
    // alpha = 85/994, leaving x = (40/71, 96/71, 0), so b stays active and the
    // two-column solve gives (64, 132)/101 with c's gradient -72/101. That is
    // three entries and one interpolation step. A full step to the solution
    // would drop b as well and need a fifth iteration to readmit it.
    val rows = Vector(
      Vector(2.0, 1.0, 2.0),
      Vector(1.0, 3.0, 0.0),
      Vector(2.0, 2.0, 2.0),
      Vector(2.0, 0.0, 3.0)
    )
    val y      = Vector(4.0, 4.0, 4.0, 0.0)
    val fit    = get(ConstrainedLeastSquares.nonNegative(rows, y))
    val oracle = Exact.nonNegative(
      Vector.tabulate(3)(j => rows.map(r => Q.of(r(j)))),
      y.map(Q.of)
    )
    assertEquals(oracle, Vector(Q.int(64) / Q.int(101), Q.int(132) / Q.int(101), Q.zero))
    near(fit.coefficients(0), oracle(0))
    near(fit.coefficients(1), oracle(1))
    assertEquals(fit.coefficients(2), 0.0)
    assertEquals(fit.active, 2)
    assertEquals(fit.iterations, 4)
    assertEquals(fit.dualViolation, 0.0)
  }

  test("a set-aside column left above the dual tolerance fails as Stalled") {
    // Each response is an exact non-negative combination of the columns, so
    // every excluded gradient is zero and its computed value is rounding.
    // The smallest positive tolerance must honour that rounding, yet a column
    // admitted on it gets a coefficient that is not positive: the fit is then
    // a Stalled failure, never a Right outside the tolerance. Which designs
    // stall depends on platform rounding; the contract and its reach do not.
    val tiny    = get(RelativeDualTolerance.of(Double.MinPositiveValue))
    val designs = Gen
      .listOfN(
        120,
        for
          p    <- Gen.choose(2, 3)
          n    <- Gen.choose(p + 1, p + 2)
          rows <- Gen.listOfN(n, Gen.listOfN(p, Gen.choose(0, 4)))
          w    <- Gen.listOfN(p, Gen.choose(0, 2))
        yield (rows.map(_.map(_.toDouble).toVector).toVector, w)
      )
      .apply(Gen.Parameters.default, org.scalacheck.rng.Seed(20261004L))
      .getOrElse(fail("generator"))
    val stalls = designs.flatMap { (rows, w) =>
      val y = rows.map(r => r.zip(w).map(_ * _).sum)
      ConstrainedLeastSquares.nonNegative(rows, y, dualTolerance = tiny) match
        case Right(fit) =>
          assert(fit.dualViolation <= tiny.value, s"${fit.dualViolation}")
          None
        case Left(LeastSquaresError.Stalled(column, violation, tolerance)) =>
          assertEquals(tolerance, tiny.value)
          assert(violation > tolerance && violation < 1e-12, s"$violation")
          assert(column >= 0 && column < rows.head.size)
          // The default tolerance is above the rounding and returns a fit.
          assert(ConstrainedLeastSquares.nonNegative(rows, y).isRight)
          Some(column)
        case Left(_: LeastSquaresError.RankDeficient) => None
        case Left(other)                              => fail(s"$other")
    }
    assert(stalls.nonEmpty, "no design reached Stalled")
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
    // The least-squares column indexes the whole design; the message names the predictor.
    assert(
      Template
        .decomposeMixture(a, get(PredictorSet.of(Vector(ids(0) -> y))), Intercept.Include)
        .left
        .exists(_.message.endsWith(" Design column=1 is predictor=a."))
    )
    assert(
      Template
        .decomposeNonNegative(y, get(PredictorSet.of(Vector(ids(0) -> a, ids(1) -> a))))
        .left
        .exists(_.message.endsWith(" Design column=1 is predictor=b."))
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

  test(
    "partial association is defined, and extreme, when y lies in the span of x and the covariates"
  ) {
    // y has x's ordering, so its ranks equal x's: the whole rank covariance is
    // singular, yet the residuals of x and y on the covariate are identical.
    val x          = mass(Vector(1, 4, 2, 8, 5, 7))
    val y          = mass(Vector(2, 9, 3, 30, 10, 20))
    val z          = mass(Vector(5, 1, 4, 1, 2, 6))
    val set        = get(PredictorSet.of(Vector(ids(0) -> z)))
    val exactRanks = Vector(x, y, z).map(s => Exact.ranks(exact(s)))
    assertEquals(exactRanks(0), exactRanks(1))
    assertEquals(Exact.partial(exactRanks), Some((1, Q.one)))
    val r = get(PartialAssociation.of(x, y, set, AssociationMethod.Spearman)).estimate
    assert(r.exists(v => oracleTolerance.approxEquals(v, 1.0)), s"$r")
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

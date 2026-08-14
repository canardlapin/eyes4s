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
import eyes4s.surface.{EdgePolicy, Smoother}

class TransportLiftSuite extends munit.FunSuite:

  val screen = Frame.screen("display", 100, 100).toOption.get
  val grid   = Grid.square(screen, 20).toOption.get
  val clock  = ClockId("tracker")

  private def spikeAt(x: Double, y: Double): Mass[Px] =
    val target = grid.indexOf(Pt[Px](x, y)).get
    Surface
      .intensity(
        grid,
        IArray.tabulate(grid.size)(i => if i == target then 1.0 else 0.0),
        Provenance.raw(ContentHash.empty)
      )
      .flatMap(_.normalised)
      .toOption
      .get

  val near = spikeAt(50, 50)
  val mid  = spikeAt(60, 50)
  val far  = spikeAt(95, 50)

  // -------------------------------------------------------------------------
  // Transport sees distance where cell-wise measures cannot
  // -------------------------------------------------------------------------

  test("transport distinguishes a small displacement from a large one") {
    val w = Transport.slicedWasserstein[Px]()
    val a = w.compare(near, mid).toOption.get.value
    val b = w.compare(near, far).toOption.get.value
    assert(b > a, clue((a, b)))
  }

  test("cell-wise measures cannot make that distinction at all") {
    // The point of having transport. Three disjoint spikes: total variation
    // says all three pairs are maximally different, because it never learns
    // that cells have positions.
    val tv = Distribution.totalVariation[Px]
    val a  = tv.compare(near, mid).toOption.get.value
    val b  = tv.compare(near, far).toOption.get.value
    assertEqualsDouble(a, b, 1e-12)
    assertEqualsDouble(a, 1.0, 1e-12)
  }

  test("sliced Wasserstein satisfies the metric axioms") {
    val w                           = Transport.slicedWasserstein[Px]()
    def d(x: Mass[Px], y: Mass[Px]) = w.compare(x, y).toOption.get.value

    assertEqualsDouble(d(near, near), 0.0, 1e-12)
    assertEqualsDouble(d(near, far), d(far, near), 1e-12)
    assert(
      d(near, far) <= d(near, mid) + d(mid, far) + 1e-9,
      clue((d(near, far), d(near, mid), d(mid, far)))
    )
  }

  test("distance grows with displacement, roughly in proportion") {
    val w  = Transport.slicedWasserstein[Px](directions = 32)
    val d1 = w.compare(spikeAt(50, 50), spikeAt(60, 50)).toOption.get.value
    val d2 = w.compare(spikeAt(50, 50), spikeAt(70, 50)).toOption.get.value
    assert(d2 > d1, clue((d1, d2)))
    assert(d2 < 3.0 * d1, clue((d1, d2)))
  }

  test("more directions do not change the answer much") {
    val a = Transport.slicedWasserstein[Px](8).compare(near, far).toOption.get.value
    val b = Transport.slicedWasserstein[Px](64).compare(near, far).toOption.get.value
    assert(math.abs(a - b) / b < 0.1, clue((a, b)))
  }

  test("the result does not depend on a seed, because there is none") {
    val w = Transport.slicedWasserstein[Px](16)
    assertEquals(w.compare(near, far), w.compare(near, far))
  }

  test("zero directions is refused") {
    assertEquals(
      Transport.slicedWasserstein[Px](0).compare(near, far),
      Left(CompareError.NonPositiveDirections("sliced Wasserstein-1", 0))
    )
  }

  // -------------------------------------------------------------------------
  // Sinkhorn, and the bias it does not hide
  // -------------------------------------------------------------------------

  /** A diffuse distribution, which is where the entropic bias is visible. */
  val blob = Smoother
    .gaussian(Sigma.px(12.0).toOption.get, EdgePolicy.Renormalise)
    .density(PointMeasure.uniform(screen, IArray(Pt[Px](50, 50))).toOption.get, grid)
    .toOption
    .get

  test("Sinkhorn self-distance is NOT zero on a diffuse map, and that is the documented cost") {
    // The claim in C-3 made concrete. Entropic regularisation biases toward a
    // spread-out plan and the bias survives on identical inputs. A caller
    // treating a similarity matrix's diagonal as a reference point would be
    // systematically wrong in a way that looks reasonable.
    val s    = Transport.sinkhorn[Px](epsilon = 50.0, iterations = 50)
    val self = s.compare(blob, blob).toOption.get.value
    assert(self > 0.0, clue(self))
  }

  test("a single-cell distribution is the exception, and it is a degenerate one") {
    // With all mass on one cell the marginals admit exactly one transport plan,
    // so entropy has no freedom to act and the self-distance really is zero.
    // Worth pinning: the bias is a property of having somewhere to spread TO,
    // not an unconditional property of the method.
    val s = Transport.sinkhorn[Px](epsilon = 50.0, iterations = 50)
    assertEqualsDouble(s.compare(near, near).toOption.get.value, 0.0, 1e-12)
  }

  test("a larger regularisation biases the self-distance further from zero") {
    def self(eps: Double) =
      Transport
        .sinkhorn[Px](epsilon = eps, iterations = 50)
        .compare(blob, blob)
        .toOption
        .get
        .value
    assert(self(200.0) > self(50.0), clue((self(50.0), self(200.0))))
  }

  test("Sinkhorn still orders displacements correctly") {
    val s = Transport.sinkhorn[Px](epsilon = 50.0, iterations = 50)
    val a = s.compare(near, mid).toOption.get.value
    val b = s.compare(near, far).toOption.get.value
    assert(b > a, clue((a, b)))
  }

  test("Sinkhorn is symmetric") {
    val s = Transport.sinkhorn[Px](epsilon = 50.0, iterations = 30)
    assertEqualsDouble(
      s.compare(near, far).toOption.get.value,
      s.compare(far, near).toOption.get.value,
      1e-9
    )
  }

  test("Sinkhorn configuration failures retain their parameter values") {
    assertEquals(
      Transport.sinkhorn[Px](epsilon = 0.0).compare(near, far),
      Left(CompareError.NonPositiveRegularisation("Sinkhorn", 0.0))
    )
    assertEquals(
      Transport.sinkhorn[Px](iterations = 0).compare(near, far),
      Left(CompareError.NonPositiveIterations("Sinkhorn", 0))
    )
    assertEquals(
      Transport.sinkhorn[Px](maxCells = 0).compare(near, far),
      Left(CompareError.NonPositiveCellLimit("Sinkhorn", 0))
    )
  }

  test("Sinkhorn is not offered as a metric") {
    val errs = scala.compiletime.testing.typeCheckErrors("""
      import eyes4s.compare.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val m: Metric[Mass[Px]] = Transport.sinkhorn[Px]()
    """)
    assert(errs.nonEmpty, "an entropic approximation was accepted as a metric")
  }

  test("a grid too large for a quadratic cost matrix is refused, with the alternative named") {
    val big  = Grid.square(screen, 128).toOption.get
    val flat = Surface
      .intensity(big, IArray.fill(big.size)(1.0), Provenance.raw(ContentHash.empty))
      .flatMap(_.normalised)
      .toOption
      .get
    val r = Transport.sinkhorn[Px]().compare(flat, flat)
    assert(r.isLeft)
    r.left.foreach(e => assert(clue(e.message).contains("sliced Wasserstein")))
  }

  // -------------------------------------------------------------------------
  // The lift
  // -------------------------------------------------------------------------

  private def fix(fromMs: Long, toMs: Long, x: Double, y: Double) =
    Event.Fixation[Px](
      Interval.of(clock, Instant.millis(fromMs), Instant.millis(toMs)).toOption.get,
      Pt[Px](x, y),
      1.0,
      10
    )

  private def path(pts: (Long, Long, Double, Double)*) =
    Scanpath.of(screen, clock, IArray.from(pts.map(fix))).toOption.get

  val smoother = Smoother.gaussian(Sigma.px(6.0).toOption.get, EdgePolicy.Renormalise)

  val left  = path((0, 100, 20, 50), (150, 250, 25, 55))
  val left2 = path((0, 100, 22, 52), (150, 250, 24, 53))
  val right = path((0, 100, 80, 50), (150, 250, 85, 55))

  test("a path comparison falls out of a map comparison") {
    val lifted = Lift.viaSmoothing(Distribution.totalVariation[Px], smoother, grid)
    val alike  = lifted.compare(left, left2).toOption.get.value
    val apart  = lifted.compare(left, right).toOption.get.value
    assert(apart > alike, clue((alike, apart)))
  }

  test("the lift preserves symmetry, so the result is still valid for an unordered pair") {
    val lifted: SymmetricCompare[Scanpath[Px], MeasureDistance] =
      Lift.viaSmoothingSymmetric(Distribution.totalVariation[Px], smoother, grid)
    assertEqualsDouble(
      lifted.compare(left, right).toOption.get.value,
      lifted.compare(right, left).toOption.get.value,
      1e-12
    )
  }

  test("the lift works with any inner measure, not a fixed list of names") {
    val viaTv  = Lift.viaSmoothing(Distribution.totalVariation[Px], smoother, grid)
    val viaOt  = Lift.viaSmoothing(Transport.slicedWasserstein[Px](), smoother, grid)
    val viaCos = Lift.viaSmoothing(Distribution.cosine[Px], smoother, grid)
    assert(viaTv.compare(left, right).isRight)
    assert(viaOt.compare(left, right).isRight)
    assert(viaCos.compare(left, right).isRight)
  }

  test("the weighting choice reaches through the lift") {
    val byTime =
      Lift.viaSmoothing(Distribution.totalVariation[Px], smoother, grid, Weight.Duration)
    val byCount =
      Lift.viaSmoothing(Distribution.totalVariation[Px], smoother, grid, Weight.Uniform)
    val uneven = path((0, 20, 20, 50), (100, 1100, 80, 50))
    val a      = byTime.compare(uneven, left).toOption.get.value
    val b      = byCount.compare(uneven, left).toOption.get.value
    assert(math.abs(a - b) > 1e-6, clue((a, b)))
  }

  test("the lift records the bandwidth in its own description") {
    val lifted = Lift.viaSmoothing(Distribution.cosine[Px], smoother, grid)
    assert(clue(lifted.info.summary).contains("sigma=6"))
    assert(lifted.info.name.contains("smoothed"))
  }

  // -------------------------------------------------------------------------
  // Map versus points -- the slot eyesim has no way to express
  // -------------------------------------------------------------------------

  private def observedAt(ps: (Double, Double)*): PointMeasure[Px] =
    PointMeasure
      .uniform(screen, IArray.from(ps.map((x, y) => Pt[Px](x, y))))
      .toOption
      .get

  test("NSS is positive when the model predicts where the eye went") {
    val model = Smoother
      .gaussian(Sigma.px(10.0).toOption.get, EdgePolicy.Renormalise)
      .density(observedAt((30.0, 30.0)), grid)
      .toOption
      .get
    val onTarget  = observedAt((30.0, 30.0), (32.0, 28.0))
    val elsewhere = observedAt((90.0, 90.0), (85.0, 95.0))

    val hit  = Saliency.nss[Px].compare(model, onTarget).toOption.get.value
    val miss = Saliency.nss[Px].compare(model, elsewhere).toOption.get.value
    assert(hit > 0.0, clue(hit))
    assert(miss < hit, clue((hit, miss)))
    assertEquals(Saliency.nss[Px].scale, MeasureScale.UnboundedSimilarity)
  }

  test("NSS against a constant map is undefined, not zero") {
    val flat = Surface
      .intensity(grid, IArray.fill(grid.size)(1.0), Provenance.raw(ContentHash.empty))
      .flatMap(_.normalised)
      .toOption
      .get
    assertEquals(
      Saliency.nss[Px].compare(flat, observedAt((50.0, 50.0))),
      Left(CompareError.ConstantInput("NSS", CompareOperand.Model))
    )
  }

  test("NSS with no observed positions is undefined") {
    assertEquals(
      Saliency.nss[Px].compare(near, PointMeasure.empty(screen)),
      Left(CompareError.EmptyInput("NSS", CompareOperand.Observed, 0.0))
    )
  }

  test("the map and the points must share a frame") {
    val other     = Frame.screen("other", 100, 100).toOption.get
    val elsewhere = PointMeasure.uniform(other, IArray(Pt[Px](50, 50))).toOption.get
    assert(Saliency.nss[Px].compare(near, elsewhere).isLeft)
  }

  test("NSS is an integral against a transformed map, which is the whole claim") {
    // Computing it by hand through the occupancy primitive must agree with the
    // measure. If the duality in the architecture is real, this is arithmetic.
    val model = Smoother
      .gaussian(Sigma.px(10.0).toOption.get, EdgePolicy.Renormalise)
      .density(observedAt((40.0, 40.0)), grid)
      .toOption
      .get
    val obs = observedAt((40.0, 40.0), (60.0, 60.0))

    val n      = model.size
    val mean   = model.sum / n
    val sd     = math.sqrt((0 until n).map(i => math.pow(model.at(i) - mean, 2)).sum / n)
    val byHand =
      obs.integrate(p => model.sampleAt(p).map(v => (v - mean) / sd).getOrElse(0.0)) / obs.total

    assertEqualsDouble(Saliency.nss[Px].compare(model, obs).toOption.get.value, byHand, 1e-12)
  }

end TransportLiftSuite

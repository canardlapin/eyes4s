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

package eyes4s.kernel

import eyes4s.kernel.Unit2D.Px

import scala.compiletime.testing.typeCheckErrors

class OccupancySuite extends munit.FunSuite:

  val screen = Frame.screen("display", 100, 100).toOption.get
  val grid   = Grid.square(screen, 10).toOption.get

  val positions = IArray(Pt[Px](5, 5), Pt[Px](15, 5), Pt[Px](95, 95))
  val durations = IArray(100.0, 300.0, 600.0)
  val measure   = PointMeasure.of(screen, positions, durations).toOption.get

  private def uniformMass(g: Grid[Px]): Mass[Px] =
    Surface
      .intensity(g, IArray.fill(g.size)(1.0), Provenance.raw(ContentHash.empty))
      .flatMap(_.normalised)
      .toOption
      .get

  // -------------------------------------------------------------------------
  // PointMeasure
  // -------------------------------------------------------------------------

  test("weights must be finite and non-negative") {
    assert(PointMeasure.of(screen, positions, IArray(1.0, -1.0, 1.0)).isLeft)
    assert(PointMeasure.of(screen, positions, IArray(1.0, Double.NaN, 1.0)).isLeft)
    assert(PointMeasure.of(screen, positions, IArray(1.0, 2.0)).isLeft)
  }

  test("integrate is the primitive the others are built from") {
    assertEqualsDouble(measure.total, 1000.0, 1e-9)
    // Count of positions = integral of the constant one under uniform weights.
    val u = PointMeasure.uniform(screen, positions).toOption.get
    assertEqualsDouble(u.integrate(_ => 1.0), 3.0, 1e-9)
    // Weighted mean x = integral of x, over total.
    assertEqualsDouble(
      measure.integrate(_.x) / measure.total,
      (500.0 + 4500.0 + 57000.0) / 1000.0,
      1e-9
    )
  }

  test("dwell time is the integral of an indicator") {
    val leftHalf = Region.rect(Pt[Px](0, 0), Pt[Px](50, 100)).toOption.get
    assertEqualsDouble(measure.massIn(leftHalf), 400.0, 1e-9)
    // Which is exactly what integrate against the indicator gives.
    assertEqualsDouble(
      measure.integrate(p => if leftHalf.contains(p) then 1.0 else 0.0),
      measure.massIn(leftHalf),
      1e-12
    )
  }

  test("binning preserves total mass for positions inside the frame") {
    val bins = measure.binned(grid).toOption.get
    assertEquals(bins.length, grid.size)
    assertEqualsDouble(bins.sum, 1000.0, 1e-9)
    assertEqualsDouble(bins(grid.indexOf(Pt[Px](5, 5)).get), 100.0, 1e-9)
  }

  test("positions outside the frame contribute nowhere, rather than piling on the edge") {
    val stray = PointMeasure
      .of(screen, IArray(Pt[Px](5, 5), Pt[Px](500, 500)), IArray(10.0, 90.0))
      .toOption
      .get
    val bins = stray.binned(grid).toOption.get
    assertEqualsDouble(bins.sum, 10.0, 1e-9)
    assertEqualsDouble(stray.withinFrame.total, 10.0, 1e-9)
  }

  test("binning refuses a grid over a different frame") {
    val other = Frame.screen("other", 100, 100).toOption.get
    val g2    = Grid.square(other, 10).toOption.get
    assert(measure.binned(g2).isLeft)
  }

  test("normalising an empty measure is an error, not a division") {
    assert(PointMeasure.empty(screen).normalised.isLeft)
    assertEqualsDouble(measure.normalised.toOption.get.total, 1.0, 1e-12)
  }

  // -------------------------------------------------------------------------
  // The type split
  // -------------------------------------------------------------------------

  test("entropy on a signed surface does not compile") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val s: Signed[Px] = ???
      s.entropy()
    """)
    assert(errs.nonEmpty, "entropy was available on a signed map")
  }

  test("normalised is unavailable on a Mass, which is already normalised") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val m: Mass[Px] = ???
      m.normalised
    """)
    assert(errs.nonEmpty, "a Mass offered to normalise itself again")
  }

  test("a negative value cannot become an Intensity") {
    val vs = IArray.tabulate(grid.size)(i => if i == 7 then -1.0 else 1.0)
    val r  = Surface.intensity(grid, vs, Provenance.raw(ContentHash.empty))
    assert(r.isLeft)
    r.left.foreach { e =>
      assert(clue(e.message).contains("Signed"))
    }
  }

  test("normalised is the single gate from an estimate to a distribution") {
    val raw = Surface
      .intensity(
        grid,
        IArray.tabulate(grid.size)(i => i.toDouble),
        Provenance.raw(ContentHash.empty)
      )
      .toOption
      .get
    val m = raw.normalised.toOption.get
    assertEqualsDouble(m.sum, 1.0, 1e-12)
    // And the passage is recorded.
    assert(clue(m.provenance.render).contains("normalise"))
  }

  test("an estimate with no mass cannot be normalised") {
    val flat = Surface
      .intensity(grid, IArray.fill(grid.size)(0.0), Provenance.raw(ContentHash.empty))
      .toOption
      .get
    assert(flat.normalised.isLeft)
  }

  // -------------------------------------------------------------------------
  // Mass operations
  // -------------------------------------------------------------------------

  test("difference of two masses is signed, sums to zero, and is typed Signed") {
    val a = uniformMass(grid)
    val b = Surface
      .intensity(
        grid,
        IArray.tabulate(grid.size)(i => (i + 1).toDouble),
        Provenance.raw(ContentHash.empty)
      )
      .flatMap(_.normalised)
      .toOption
      .get
    val d: Signed[Px] = a.difference(b).toOption.get
    assertEqualsDouble(d.sum, 0.0, 1e-12)
    assert(
      d.values.exists(_ < 0.0),
      "a difference of distinct masses must go negative somewhere"
    )
  }

  test("operations across grids are refused") {
    val g2 = Grid.square(screen, 5).toOption.get
    val a  = uniformMass(grid)
    val b  = uniformMass(g2)
    assertEquals(a.difference(b), Left(SurfaceError.GridMismatch(grid.id, g2.id)))
  }

  test("log ratio is named for what it is, and is zero for identical masses") {
    val a = uniformMass(grid)
    val r = a.logRatio(a).toOption.get
    assert(r.values.forall(v => math.abs(v) < 1e-12))
  }

  test("entropy is maximal for a uniform mass and zero for a point mass") {
    val u = uniformMass(grid)
    assertEqualsDouble(u.entropy().value, math.log(grid.size.toDouble), 1e-12)
    assertEqualsDouble(u.relativeEntropy(), 1.0, 1e-12)

    val spike = Surface
      .intensity(
        grid,
        IArray.tabulate(grid.size)(i => if i == 0 then 1.0 else 0.0),
        Provenance.raw(ContentHash.empty)
      )
      .flatMap(_.normalised)
      .toOption
      .get
    assertEqualsDouble(spike.entropy().value, 0.0, 1e-12)
    assertEqualsDouble(spike.relativeEntropy(), 0.0, 1e-12)
  }

  test("entropy states its base") {
    val u = uniformMass(grid)
    assertEquals(u.entropy(LogBase.Two).base, LogBase.Two)
    assertEqualsDouble(u.entropy(LogBase.Two).value, math.log(100.0) / math.log(2.0), 1e-12)
  }

  test("relative entropy is comparable across resolutions where raw entropy is not") {
    val coarse = uniformMass(Grid.square(screen, 4).toOption.get)
    val fine   = uniformMass(Grid.square(screen, 40).toOption.get)
    assert(coarse.entropy().value < fine.entropy().value, "raw entropy grows with cell count")
    assertEqualsDouble(coarse.relativeEntropy(), fine.relativeEntropy(), 1e-12)
  }

  test("the mean of masses is a mass") {
    val a = uniformMass(grid)
    val b = Surface
      .intensity(
        grid,
        IArray.tabulate(grid.size)(i => i.toDouble),
        Provenance.raw(ContentHash.empty)
      )
      .flatMap(_.normalised)
      .toOption
      .get
    val m = Mass.mean(Seq(a, b)).toOption.get
    assertEqualsDouble(m.sum, 1.0, 1e-9)
    (0 until grid.size).foreach { i =>
      assertEqualsDouble(m.at(i), (a.at(i) + b.at(i)) / 2.0, 1e-12, clue(i))
    }
  }

  test("mean of an empty collection is an error, not a NaN") {
    assert(Mass.mean(Seq.empty[Mass[Px]]).isLeft)
    assert(Mass.weightedMean(Seq.empty[(Double, Mass[Px])]).isLeft)
  }

  test("weighted mean with equal weights equals the plain mean") {
    val a  = uniformMass(grid)
    val b  = uniformMass(Grid.square(screen, 10).toOption.get)
    val m1 = Mass.mean(Seq(a, b)).toOption.get
    val m2 = Mass.weightedMean(Seq((3.0, a), (3.0, b))).toOption.get
    (0 until grid.size).foreach(i => assertEqualsDouble(m2.at(i), m1.at(i), 1e-12))
  }

  // -------------------------------------------------------------------------
  // Module
  // -------------------------------------------------------------------------

  test("signed surfaces form a module over the grid") {
    val M = grid.signedModule
    val a = Surface
      .signed(
        grid,
        IArray.tabulate(grid.size)(i => i.toDouble - 50.0),
        Provenance.raw(ContentHash.empty)
      )
      .toOption
      .get

    assert((0 until grid.size).forall(i => M.plus(a, M.zero).at(i) == a.at(i)))
    assert((0 until grid.size).forall(i => math.abs(M.plus(a, M.negate(a)).at(i)) < 1e-12))
    assert(
      (0 until grid.size).forall(i => math.abs(M.scale(2.0, a).at(i) - 2.0 * a.at(i)) < 1e-12)
    )
    assertEquals(M.zero.size, grid.size)
  }

  test(
    "the module's zero is dimensioned by its grid, which a global instance could not supply"
  ) {
    assertEquals(grid.signedModule.zero.size, 100)
    assertEquals(Grid.square(screen, 3).toOption.get.signedModule.zero.size, 9)
  }

  // -------------------------------------------------------------------------
  // Provenance and content hashing
  // -------------------------------------------------------------------------

  test("identical data hashes identically; different data does not") {
    assertEquals(ContentHash.of(IArray(1.0, 2.0, 3.0)), ContentHash.of(IArray(1.0, 2.0, 3.0)))
    assertNotEquals(
      ContentHash.of(IArray(1.0, 2.0, 3.0)),
      ContentHash.of(IArray(1.0, 2.0, 4.0))
    )
    // Length participates, so a prefix does not collide with the whole.
    assertNotEquals(ContentHash.of(IArray(1.0, 2.0)), ContentHash.of(IArray(1.0, 2.0, 0.0)))
  }

  test("the two zeros hash alike, since they are numerically equal") {
    assertEquals(ContentHash.of(IArray(0.0)), ContentHash.of(IArray(-0.0)))
  }

  test("provenance records the derivation, and the digest covers it") {
    val raw   = Provenance.raw(ContentHash.of(IArray(1.0)))
    val once  = raw.andThen(Provenance.Step.num("smooth", "sigma", 1.0))
    val twice = once.andThen(Provenance.Step.text("normalise", "of", "surface"))
    assertNotEquals(raw.digest, once.digest)
    assertNotEquals(once.digest, twice.digest)
    assert(clue(twice.render).contains("smooth(sigma=1) -> normalise(of=surface)"))
  }

  test("same parameters over different inputs give different digests") {
    // The whole reason provenance carries a content hash: a parameter record
    // alone cannot distinguish two datasets analysed identically.
    val step = Provenance.Step.num("smooth", "sigma", 1.0)
    val a    = Provenance.raw(ContentHash.of(IArray(1.0, 2.0))).andThen(step)
    val b    = Provenance.raw(ContentHash.of(IArray(3.0, 4.0))).andThen(step)
    assertNotEquals(a.digest, b.digest)
  }

end OccupancySuite

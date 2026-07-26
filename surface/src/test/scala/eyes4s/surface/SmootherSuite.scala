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
import eyes4s.kernel.Unit2D.{Deg, Px}

import scala.compiletime.testing.typeCheckErrors

class SmootherSuite extends munit.FunSuite:

  val screen = Frame.screen("display", 100, 100).toOption.get
  val grid   = Grid.square(screen, 50).toOption.get

  private def measureAt(ps: (Double, Double, Double)*): PointMeasure[Px] =
    PointMeasure
      .of(
        screen,
        IArray.from(ps.map((x, y, _) => Pt[Px](x, y))),
        IArray.from(ps.map((_, _, w) => w))
      )
      .toOption
      .get

  val centre = measureAt((50.0, 50.0, 1.0))
  val sigma  = Sigma.px(4.0).toOption.get

  // -------------------------------------------------------------------------
  // Mass conservation, and the edge policy that decides it
  // -------------------------------------------------------------------------

  test("a kernel well inside the grid conserves mass under either policy") {
    val t = Smoother.gaussian(sigma, EdgePolicy.Truncate).smooth(centre, grid).toOption.get
    val r = Smoother.gaussian(sigma, EdgePolicy.Renormalise).smooth(centre, grid).toOption.get
    assertEqualsDouble(t.sum, 1.0, 1e-6)
    assertEqualsDouble(r.sum, 1.0, 1e-6)
  }

  test("at the edge the two policies genuinely disagree, which is why it is required") {
    // A point in the corner puts most of its kernel off the grid.
    val corner = measureAt((0.5, 0.5, 1.0))
    val t      = Smoother.gaussian(sigma, EdgePolicy.Truncate).smooth(corner, grid).toOption.get
    val r = Smoother.gaussian(sigma, EdgePolicy.Renormalise).smooth(corner, grid).toOption.get

    assert(t.sum < 0.5, clue(t.sum))     // more than half the mass has left
    assertEqualsDouble(r.sum, 1.0, 1e-6) // ...or been redistributed
    assert(r.sum - t.sum > 0.5, clue((t.sum, r.sum)))
  }

  test("the estimate is centred where the mass is") {
    val m    = Smoother.gaussian(sigma, EdgePolicy.Truncate).smooth(centre, grid).toOption.get
    val peak = (0 until grid.size).maxBy(m.at)
    val p    = grid.cellCentre(peak)
    assertEqualsDouble(p.x, 50.0, grid.cellWidth)
    assertEqualsDouble(p.y, 50.0, grid.cellHeight)
  }

  test("a wider kernel spreads the same mass further") {
    val narrow = Smoother
      .gaussian(Sigma.px(2.0).toOption.get, EdgePolicy.Truncate)
      .smooth(centre, grid)
      .toOption
      .get
    val wide = Smoother
      .gaussian(Sigma.px(10.0).toOption.get, EdgePolicy.Truncate)
      .smooth(centre, grid)
      .toOption
      .get
    assert(narrow.values.max > wide.values.max, "a narrow kernel has a taller peak")
    assert(narrow.values.count(_ > 1e-6) < wide.values.count(_ > 1e-6))
  }

  test("smoothing is additive: two points give the sum of their kernels") {
    val a    = measureAt((30.0, 50.0, 1.0))
    val b    = measureAt((70.0, 50.0, 1.0))
    val both = measureAt((30.0, 50.0, 1.0), (70.0, 50.0, 1.0))
    val s    = Smoother.gaussian(sigma, EdgePolicy.Truncate)

    val sa    = s.smooth(a, grid).toOption.get
    val sb    = s.smooth(b, grid).toOption.get
    val sboth = s.smooth(both, grid).toOption.get
    (0 until grid.size).foreach { i =>
      assertEqualsDouble(sboth.at(i), sa.at(i) + sb.at(i), 1e-9, clue(i))
    }
  }

  test("weights carry through: a heavier point contributes proportionally more") {
    val s   = Smoother.gaussian(sigma, EdgePolicy.Truncate)
    val one = s.smooth(measureAt((50.0, 50.0, 1.0)), grid).toOption.get
    val ten = s.smooth(measureAt((50.0, 50.0, 10.0)), grid).toOption.get
    (0 until grid.size).foreach(i => assertEqualsDouble(ten.at(i), one.at(i) * 10.0, 1e-9))
  }

  // -------------------------------------------------------------------------
  // Refusals
  // -------------------------------------------------------------------------

  test("a measure with no mass is refused, not silently flattened") {
    val empty = PointMeasure.empty(screen)
    assertEquals(
      Smoother.gaussian(sigma, EdgePolicy.Truncate).smooth(empty, grid),
      Left(EstimateError.NoMass)
    )
  }

  test("a grid over another frame is refused") {
    val other = Frame.screen("other", 100, 100).toOption.get
    val g2    = Grid.square(other, 50).toOption.get
    assert(Smoother.gaussian(sigma, EdgePolicy.Truncate).smooth(centre, g2).isLeft)
  }

  test("a kernel narrower than the grid can express is refused, with a reason") {
    val coarse = Grid.square(screen, 4).toOption.get // 25-unit cells
    val r      = Smoother.gaussian(sigma, EdgePolicy.Truncate).smooth(centre, coarse)
    assert(r.isLeft)
    r.left.foreach { e =>
      assert(clue(e.message).contains("histogram"))
    }
  }

  test("a degree bandwidth cannot smooth a pixel measure") {
    val errs = typeCheckErrors("""
      import eyes4s.surface.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.{Px, Deg}
      val s: Sigma[Deg] = Sigma.deg(1.0).toOption.get
      val m: PointMeasure[Px] = ???
      val g: Grid[Px] = ???
      Smoother.gaussian(s, EdgePolicy.Truncate).smooth(m, g)
    """)
    assert(errs.nonEmpty, "a degree bandwidth reached a pixel grid")
  }

  // -------------------------------------------------------------------------
  // density
  // -------------------------------------------------------------------------

  test("density normalises, and records that it did") {
    val d = Smoother.gaussian(sigma, EdgePolicy.Truncate).density(centre, grid).toOption.get
    assertEqualsDouble(d.sum, 1.0, 1e-12)
    assert(clue(d.provenance.render).contains("smooth"))
    assert(d.provenance.render.contains("normalise"))
  }

  test("provenance records the bandwidth and the edge policy") {
    val d = Smoother.gaussian(sigma, EdgePolicy.Renormalise).density(centre, grid).toOption.get
    assert(clue(d.provenance.render).contains("sigma=4"))
    assert(d.provenance.render.contains("Renormalise"))
  }

  test("a numeric parameter renders identically on every platform") {
    // Double.toString gives "4.0" on the JVM and "4" under Scala.js. Provenance
    // is HASHED as well as shown, so a rendering difference would make a cache
    // key non-portable -- defeating the guarantee ContentHash exists for. This
    // suite runs on both platforms, which is the only reason it was noticed.
    assertEquals(Provenance.Param.Num(4.0).render, "4")
    assertEquals(Provenance.Param.Num(4.5).render, "4.5")
    assertEquals(Provenance.Param.Num(-0.25).render, "-0.25")
    assertEquals(Provenance.Param.Num(1000.0).render, "1000")
  }

  test("the digest depends on the value, not on how it renders") {
    val a = Provenance.Step.num("smooth", "sigma", 4.0)
    val b = Provenance.Step.num("smooth", "sigma", 4.0)
    val c = Provenance.Step.num("smooth", "sigma", 4.5)
    assertEquals(a.digest, b.digest)
    assertNotEquals(a.digest, c.digest)
  }

  test("two bandwidths give different digests, so a cache cannot confuse them") {
    val a = Smoother
      .gaussian(Sigma.px(4.0).toOption.get, EdgePolicy.Truncate)
      .density(centre, grid)
      .toOption
      .get
    val b = Smoother
      .gaussian(Sigma.px(8.0).toOption.get, EdgePolicy.Truncate)
      .density(centre, grid)
      .toOption
      .get
    assertNotEquals(a.provenance.digest, b.provenance.digest)
  }

  test("the edge policy is part of the digest too") {
    val t = Smoother.gaussian(sigma, EdgePolicy.Truncate).density(centre, grid).toOption.get
    val r = Smoother.gaussian(sigma, EdgePolicy.Renormalise).density(centre, grid).toOption.get
    assertNotEquals(t.provenance.digest, r.provenance.digest)
  }

  // -------------------------------------------------------------------------
  // Bandwidth rules
  // -------------------------------------------------------------------------

  test("bandwidth rules need at least two points") {
    assert(Bandwidth.silverman(measureAt((1.0, 1.0, 1.0))).isLeft)
  }

  test("a cloud degenerate in one axis still yields a bandwidth") {
    // Gaze confined to a horizontal line is unusual data, not invalid data.
    val line = measureAt((10.0, 50.0, 1.0), (50.0, 50.0, 1.0), (90.0, 50.0, 1.0))
    assert(Bandwidth.silverman(line).isRight, "a degenerate axis must not veto the rule")
  }

  test("a cloud degenerate in both axes has no bandwidth") {
    val point = measureAt((50.0, 50.0, 1.0), (50.0, 50.0, 1.0), (50.0, 50.0, 1.0))
    assert(Bandwidth.silverman(point).isLeft)
  }

  test("a more concentrated cloud gets a narrower bandwidth") {
    val tight = measureAt((49.0, 49.0, 1.0), (50.0, 50.0, 1.0), (51.0, 51.0, 1.0))
    val loose = measureAt((10.0, 10.0, 1.0), (50.0, 50.0, 1.0), (90.0, 90.0, 1.0))
    val a     = Bandwidth.silverman(tight).toOption.get
    val b     = Bandwidth.silverman(loose).toOption.get
    assert(a.value < b.value, clue((a.value, b.value)))
  }

  test("Scott's rule is wider than Silverman's on the same data") {
    val m = measureAt((10.0, 10.0, 1.0), (50.0, 50.0, 1.0), (90.0, 90.0, 1.0))
    assert(Bandwidth.scott(m).toOption.get.value > Bandwidth.silverman(m).toOption.get.value)
  }

  test("the conventional foveal bandwidth is one degree, and typed as such") {
    val f: Sigma[Deg] = Bandwidth.foveal.toOption.get
    assertEqualsDouble(f.value, 1.0, 1e-12)
  }

  // -------------------------------------------------------------------------
  // Pyramid
  // -------------------------------------------------------------------------

  val sigmas = Vector(2.0, 4.0, 8.0).map(v => Sigma.px(v).toOption.get)

  test("a pyramid holds its scales with its surfaces, in ascending order") {
    val p = Pyramid.of(centre, grid, sigmas.reverse, EdgePolicy.Truncate).toOption.get
    assertEquals(p.size, 3)
    assertEquals(p.scales.map(_.value), Vector(2.0, 4.0, 8.0))
    assert(p.at(Sigma.px(4.0).toOption.get).isDefined)
  }

  test("every level is a normalised mass") {
    val p = Pyramid.of(centre, grid, sigmas, EdgePolicy.Truncate).toOption.get
    p.surfaces.foreach(s => assertEqualsDouble(s.sum, 1.0, 1e-12))
  }

  test("the finest scale has the tallest peak") {
    val p = Pyramid.of(centre, grid, sigmas, EdgePolicy.Truncate).toOption.get
    assert(p.finest.values.max > p.coarsest.values.max)
  }

  test("duplicate scales are rejected rather than deduplicated") {
    val dup = Vector(2.0, 2.0, 4.0).map(v => Sigma.px(v).toOption.get)
    assert(Pyramid.of(centre, grid, dup, EdgePolicy.Truncate).isLeft)
  }

  test("common scales are found by value, not position") {
    val a = Pyramid.of(centre, grid, sigmas, EdgePolicy.Truncate).toOption.get
    val b = Pyramid
      .of(
        centre,
        grid,
        Vector(4.0, 8.0, 16.0).map(v => Sigma.px(v).toOption.get),
        EdgePolicy.Truncate
      )
      .toOption
      .get
    assertEquals(a.commonScales(b).map(_.value), Vector(4.0, 8.0))
  }

  test("a scale and its surface cannot be separated") {
    // The failure eyesim has: a parallel vector of bandwidths that its own
    // operations filter out of step with the surfaces they describe.
    val p = Pyramid.of(centre, grid, sigmas, EdgePolicy.Truncate).toOption.get
    p.levels.foreach { (s, mass) =>
      assertEquals(p.at(s).map(_.sum), Some(mass.sum))
    }
  }

end SmootherSuite

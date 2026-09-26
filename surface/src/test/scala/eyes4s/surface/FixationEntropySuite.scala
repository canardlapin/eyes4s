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

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** Lattice membership and every typed refusal of the fixation-entropy route. */
class FixationEntropySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  private val frame = get(Frame.screen("entropy-units", 40, 20))
  private val clock = ClockId("entropy-units")
  private def path(points: (Double, Double)*): Scanpath[Px] = pathIn(frame, points*)
  private def pathIn(f: Frame[Px], points: (Double, Double)*): Scanpath[Px] =
    val fixations = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 500L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    get(Scanpath.of(f, clock, IArray.from(fixations)))

  private def lattice(edge: LatticeUpperEdge) =
    get(OccupancyLattice.overFrame(frame, 4, 2, edge))
  private val grid  = get(Grid.over(frame, 4, 2))
  private val sigma = get(Sigma.px(10))

  test("cells are half-open except a closed upper edge, which is a named choice") {
    val closed = lattice(LatticeUpperEdge.Closed)
    val open   = lattice(LatticeUpperEdge.Open)
    assertEquals(closed.cellOf(Pt[Px](10, 10)), Some(5))
    assertEquals(closed.cellOf(Pt[Px](9.999, 9.999)), Some(0))
    assertEquals(closed.cellOf(Pt[Px](40, 20)), Some(7))
    assertEquals(open.cellOf(Pt[Px](40, 20)), None)
    assertEquals(open.cellOf(Pt[Px](39.999, 19.999)), Some(7))
    assertEquals(closed.cellOf(Pt[Px](-0.001, 5)), None)
    assertEquals(closed.clampedCellOf(Pt[Px](-5, 25)), 4)
    assertEquals(closed.clampedCellOf(Pt[Px](55, -1)), 3)
  }

  test("a lattice needs cells and a padded range needs a valid padding") {
    assertEquals(
      OccupancyLattice.overFrame(frame, 0, 3, LatticeUpperEdge.Closed).map(_.size),
      Left(FixationEntropyError.DegenerateLattice(0, 3))
    )
    val p = path((5, 5), (15, 15))
    Vector(-0.1, Double.NaN, Double.PositiveInfinity).foreach { bad =>
      OccupancyLattice.paddedRange(p, 2, 2, bad, LatticeUpperEdge.Closed) match
        case Left(FixationEntropyError.InvalidPadding(v)) =>
          assert(v.equals(bad), s"$v is not $bad")
        case other => fail(s"expected InvalidPadding($bad), got $other")
    }
  }

  test("a zero span is widened to one frame unit before padding, as eyesim does") {
    val one     = path((12, 7))
    val padded  = get(OccupancyLattice.paddedRange(one, 3, 3, 0.05, LatticeUpperEdge.Closed))
    val b       = padded.bounds
    val epsilon = 1e-12
    assertEqualsDouble(b.xMin, 11.95, epsilon)
    assertEqualsDouble(b.xMax, 12.05, epsilon)
    assertEqualsDouble(b.yMin, 6.95, epsilon)
    assertEqualsDouble(b.yMax, 7.05, epsilon)
  }

  test("a lattice or grid in another frame is refused with both frame names") {
    val other = get(Frame.screen("elsewhere", 40, 20))
    val p     = pathIn(other, (5, 5))
    val occ   = FixationEntropy.occupancy(
      p,
      lattice(LatticeUpperEdge.Closed),
      Weight.Uniform,
      OutsideLattice.Refuse,
      LogBase.E
    )
    occ match
      case Left(e @ FixationEntropyError.FrameMismatch(from, to, _)) =>
        assertEquals((from, to), (FrameId("elsewhere"), FrameId("entropy-units")))
        assert(e.message.contains("elsewhere") && e.message.contains("entropy-units"))
      case other => fail(s"expected a frame mismatch, got $other")
    assert(
      FixationEntropy
        .density(
          p,
          grid,
          DensityBandwidth.Fixed(sigma),
          EdgePolicy.Truncate,
          Weight.Uniform,
          LogBase.E
        )
        .left
        .exists(_.isInstanceOf[FixationEntropyError.FrameMismatch])
    )
  }

  test(
    "no occupancy left on the lattice or grid names the fixations and how many fell outside"
  ) {
    val outside = path((50, 5), (60, 30))
    assertEquals(
      FixationEntropy
        .occupancy(
          outside,
          lattice(LatticeUpperEdge.Closed),
          Weight.Uniform,
          OutsideLattice.Exclude,
          LogBase.E
        )
        .map(_.entropy),
      Left(FixationEntropyError.NoOccupancy(2, 2))
    )
    assertEquals(
      FixationEntropy
        .density(
          outside,
          grid,
          DensityBandwidth.Fixed(sigma),
          EdgePolicy.Truncate,
          Weight.Uniform,
          LogBase.E
        )
        .map(_.entropy),
      Left(FixationEntropyError.NoOccupancy(2, 2))
    )
  }

  test("an estimate failure names its scale") {
    val tiny = get(Sigma.px(0.5)) // below a fifth of a 10-pixel cell
    FixationEntropy.density(
      path((5, 5)),
      grid,
      DensityBandwidth.Fixed(tiny),
      EdgePolicy.Truncate,
      Weight.Uniform,
      LogBase.E
    ) match
      case Left(FixationEntropyError.Estimate(s, EstimateError.DegenerateBandwidth(_, _))) =>
        assertEquals(s, 0.5)
      case other => fail(s"expected an estimate failure, got $other")
  }

  test("scales must be present, distinct and on one grid") {
    val p                           = path((5, 5), (25, 15))
    def run(sigmas: Vector[Double]) = FixationEntropy
      .multiscale(
        p,
        grid,
        sigmas.map(s => get(Sigma.px(s))),
        EdgePolicy.Truncate,
        Weight.Uniform,
        LogBase.E
      )
      .map(_.scales)
    assertEquals(run(Vector.empty), Left(FixationEntropyError.NoScales))
    assertEquals(run(Vector(10, 5, 10)), Left(FixationEntropyError.DuplicateScale(10.0)))
    assertEquals(run(Vector(10, 5)).map(_.map(_.value)), Right(Vector(5.0, 10.0)))
    assertEquals(
      MultiscaleEntropy.of(Vector.empty[(Sigma[Px], Mass[Px])], LogBase.E).map(_.scales),
      Left(FixationEntropyError.NoScales)
    )
    val m = get(
      Smoother
        .gaussian(sigma, EdgePolicy.Truncate)
        .density(get(p.occupancy(Weight.Uniform)), grid)
    )
    val fine = get(Grid.over(frame, 8, 4))
    val m2   = get(
      Smoother
        .gaussian(sigma, EdgePolicy.Truncate)
        .density(get(p.occupancy(Weight.Uniform)), fine)
    )
    MultiscaleEntropy.of(Vector(get(Sigma.px(1)) -> m, get(Sigma.px(2)) -> m2), LogBase.E) match
      case Left(FixationEntropyError.ScaleGrid(s, SurfaceError.GridMismatch(_, _))) =>
        assertEquals(s, 2.0)
      case other => fail(s"expected a grid mismatch at sigma 2, got $other")
  }

  test(
    "a weighted reduction needs one finite non-negative weight per scale, with positive total"
  ) {
    val p      = path((5, 5), (25, 15))
    val result = get(
      FixationEntropy.multiscale(
        p,
        grid,
        Vector(get(Sigma.px(5)), get(Sigma.px(10))),
        EdgePolicy.Truncate,
        Weight.Uniform,
        LogBase.E
      )
    )
    def reduce(ws: (Double, Double)*) = result.reduce(
      EntropyQuantity.Raw,
      ScaleReduction.Weighted(ws.toVector.map((s, w) => get(Sigma.px(s)) -> w))
    )
    assertEquals(reduce(5.0 -> 1.0), Left(FixationEntropyError.MissingScaleWeight(10.0)))
    assertEquals(
      reduce(5.0 -> 1.0, 7.0 -> 1.0),
      Left(FixationEntropyError.UnknownScaleWeight(7.0))
    )
    assertEquals(reduce(5.0 -> 1.0, 5.0 -> 2.0), Left(FixationEntropyError.DuplicateScale(5.0)))
    assertEquals(
      reduce(5.0 -> -1.0, 10.0 -> 1.0),
      Left(FixationEntropyError.InvalidScaleWeight(5.0, -1.0))
    )
    assertEquals(
      reduce(5.0 -> 0.0, 10.0 -> 0.0),
      Left(FixationEntropyError.DegenerateScaleWeights(0.0))
    )
    val levels = result.values(EntropyQuantity.Raw).map(_._2)
    assertEqualsDouble(get(reduce(5.0 -> 0.0, 10.0 -> 2.0)), levels(1), 1e-15)
    assertEqualsDouble(
      get(result.reduce(EntropyQuantity.Raw, ScaleReduction.Mean())),
      levels.sum / 2,
      1e-15
    )
  }

  test("a pyramid's levels give the same entropies as the multiscale route") {
    val p       = path((5, 5), (25, 15), (33, 3))
    val sigmas  = Vector(get(Sigma.px(5)), get(Sigma.px(10)))
    val pyramid =
      get(Pyramid.of(get(p.occupancy(Weight.Duration)), grid, sigmas, EdgePolicy.Renormalise))
    val direct = get(
      FixationEntropy.multiscale(
        p,
        grid,
        sigmas,
        EdgePolicy.Renormalise,
        Weight.Duration,
        LogBase.Two
      )
    )
    assertEquals(MultiscaleEntropy.ofPyramid(pyramid, LogBase.Two).levels, direct.levels)
  }

  test("results carry the lattice, weights, policies, grid and base that produced them") {
    val p       = path((5, 5), (25, 15), (45, 3))
    val closed  = lattice(LatticeUpperEdge.Closed)
    val counted = get(
      FixationEntropy.occupancy(p, closed, Weight.Duration, OutsideLattice.Exclude, LogBase.Two)
    )
    assert(counted.lattice eq closed)
    assertEquals(counted.weight, Weight.Duration)
    assertEquals(counted.outsidePolicy, OutsideLattice.Exclude)
    assertEquals(counted.outside, Vector(2))
    assertEquals((closed.frame.id, closed.nx, closed.ny, closed.size), (frame.id, 4, 2, 8))
    assertEquals(closed.upperEdge, LatticeUpperEdge.Closed)
    assert(closed.render.contains("4x2") && closed.render.contains("Closed"), closed.render)
    val scales = get(
      FixationEntropy.multiscale(
        p,
        grid,
        Vector(sigma),
        EdgePolicy.Truncate,
        Weight.Uniform,
        LogBase.Two
      )
    )
    assertEquals((scales.grid, scales.base), (grid, LogBase.Two))
    assertEquals(DensityBandwidth.Fixed(sigma).sigma, sigma)
    assertEquals(
      DensityBandwidth.IqrSuggested[Px](IqrBandwidthClamp.Unclamped).clamp,
      IqrBandwidthClamp.Unclamped
    )
    assertEquals(
      ScaleReduction.Weighted(Vector(sigma -> 1.0)).weights.map(_._2),
      Vector(1.0)
    )
    assertEquals(
      EntropyQuantity.values.toVector,
      Vector(EntropyQuantity.Raw, EntropyQuantity.Relative)
    )
    assertEquals(OutsideLattice.values.length, 3)
    assertEquals(LatticeUpperEdge.values.length, 2)
  }

  test("every error message names its operands") {
    Vector(
      FixationEntropyError.DegenerateLattice(0, 7)                -> Vector("0", "7"),
      FixationEntropyError.InvalidPadding(-0.25)                  -> Vector("-0.25"),
      FixationEntropyError.FixationOutsideLattice(3, 70.5, 12.25) -> Vector(
        "3",
        "70.5",
        "12.25"
      ),
      FixationEntropyError.NoOccupancy(6, 4)                   -> Vector("6", "4"),
      FixationEntropyError.Estimate(0.5, EstimateError.NoMass) -> Vector("0.5"),
      FixationEntropyError.DuplicateScale(12.5)                -> Vector("12.5"),
      FixationEntropyError.MissingScaleWeight(6.25)            -> Vector("6.25"),
      FixationEntropyError.UnknownScaleWeight(7.5)             -> Vector("7.5"),
      FixationEntropyError.InvalidScaleWeight(6.25, -2.5)      -> Vector("6.25", "-2.5"),
      FixationEntropyError.DegenerateScaleWeights(Double.NaN)  -> Vector("NaN"),
      FixationEntropyError.Occupancy(SurfaceError.NegativeWeight(2, -1.5)) -> Vector(
        "2",
        "-1.5"
      )
    ).foreach { (error, operands) =>
      operands.foreach(o => assert(error.message.contains(o), s"${error.message} lacks $o"))
    }
  }

end FixationEntropySuite

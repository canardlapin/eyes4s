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
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.surface.*
import scala.compiletime.testing.typeCheckErrors

class MapComparisonSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val MapTolerance                      = 1e-12
  private def grid(n: Int) = get(Grid.over(get(Frame.screen(s"map-$n", n, 1)), n, 1))
  private def mass(values: Vector[Double], g: Grid[Px]): Mass[Px] =
    get(
      Surface.mass(g, IArray.from(values), Provenance.raw(ContentHash.of(IArray.from(values))))
    )
  private def near(a: Double, b: Double): Unit = assertEqualsDouble(a, b, MapTolerance)

  MapReference.cases.foreach { c =>
    test(
      s"${c.name}: every supported method agrees with the independent oracle and pinned vector/density dispatch"
    ) {
      val g = grid(c.a.size); val a = mass(c.a, g); val b = mass(c.b, g)
      c.expected.foreach { (name, expected) =>
        val method = get(MapSimilarityMethod.fromReference(name)).instance[Px]
        near(get(method.compare(a, b)).value, expected)
        near(get(method.compare(b, a)).value, expected)
        assert(method.info.summary.nonEmpty)
      }
    }
  }

  test("Fisher endpoint policies are explicit and legacy behavior is retained") {
    val a = mass(Vector(.1, .2, .3, .4), grid(4))
    near(get(Distribution.fisherZMachineEpsilon[Px].compare(a, a)).value, 18.36840028483855)
    near(
      get(Distribution.fisherZ[Px].compare(a, a)).value,
      0.5 * math.log((1 + .999999999999) / (1 - .999999999999))
    )
    assert(get(Distribution.fisherZ[Px].compare(a, a)).value < 15)
    assert(MapSimilarityMethod.fromReference("emd").isLeft)
    assert(MapSimilarityMethod.fromReference("sinkhorn").isLeft)
    assert(MapSimilarityMethod.fromReference("unknown").isLeft)
  }

  test("constant, one-cell, invalid mass and nominal geometry policies remain explicit") {
    val g = grid(4); val a = mass(Vector.fill(4)(.25), g);
    val b = mass(Vector(.1, .2, .3, .4), g)
    Vector(
      MapSimilarityMethod.Pearson,
      MapSimilarityMethod.Spearman,
      MapSimilarityMethod.DistanceCorrelation,
      MapSimilarityMethod.FisherZMachineEpsilon
    ).foreach { m =>
      assert(m.instance[Px].compare(a, a).isLeft)
      assert(m.instance[Px].compare(a, b).isLeft)
      assert(
        m.instance[Px].compare(mass(Vector(1.0), grid(1)), mass(Vector(1.0), grid(1))).isLeft
      )
    }
    Vector(
      MapSimilarityMethod.Cosine,
      MapSimilarityMethod.ExtendedJaccard,
      MapSimilarityMethod.L1Similarity
    ).foreach { m =>
      near(get(m.instance[Px].compare(a, a)).value, 1.0)
    }
    assert(
      Surface.mass(g, IArray(0.0, 0.0, 0.0, 0.0), Provenance.raw(ContentHash.empty)).isLeft
    )
    assert(
      Surface.mass(g, IArray(-1.0, 0.0, 1.0, 1.0), Provenance.raw(ContentHash.empty)).isLeft
    )
    assert(Surface.mass(g, IArray.empty, Provenance.raw(ContentHash.empty)).isLeft)
    val foreign = get(Grid.over(get(Frame.screen("foreign", 4, 1)), 4, 1))
    MapSimilarityMethod.values.foreach(m =>
      assert(m.instance[Px].compare(b, mass(Vector(.1, .2, .3, .4), foreign)).isLeft)
    )
    assert(typeCheckErrors("""
      def compare(s: eyes4s.kernel.Signed[eyes4s.kernel.Unit2D.Px]) = eyes4s.compare.Distribution.spearman[eyes4s.kernel.Unit2D.Px].compare(s,s)
    """).nonEmpty)
    assert(typeCheckErrors("""
      val metric: eyes4s.compare.Metric[eyes4s.kernel.Mass[eyes4s.kernel.Unit2D.Px]] = eyes4s.compare.Distribution.extendedJaccard[eyes4s.kernel.Unit2D.Px]
    """).nonEmpty)
  }

  test(
    "supplied scales retain labels and agree with named reference values independent of input order"
  ) {
    val g     = grid(4); val p            = mass(Vector(.1, .2, .3, .4), g);
    val q     = mass(Vector(.4, .3, .2, .1), g)
    val one   = get(Sigma.px(1)); val two = get(Sigma.px(2))
    val left  = get(MapScales.of(Vector(two -> p, one -> q)));
    val right = get(MapScales.of(Vector(one -> p, two -> p)))
    assertEquals(left.levels.map(_._1.value), Vector(1.0, 2.0))
    MapReference.orderedScales.foreach { (method, expected) =>
      val result =
        MapComparison.scales(left, right, get(MapSimilarityMethod.fromReference(method)))
      assertEquals(result.requested, 2); assertEquals(result.contributing, 2)
      result.rows.foreach { (sigma, value) =>
        near(get(value).value, expected.find(_._1 == sigma.value).get._2)
      }
      near(get(result.mean).value, expected.map(_._2).sum / 2)
    }
  }

  test("missing, duplicate and failed scales never disappear from means or denominators") {
    val g       = grid(4); val p            = mass(Vector(.1, .2, .3, .4), g);
    val uniform = mass(Vector.fill(4)(.25), g)
    val one     = get(Sigma.px(1)); val two = get(Sigma.px(2)); val three = get(Sigma.px(3))
    assert(MapScales.of(Vector.empty[(Sigma[Px], Mass[Px])]).isLeft)
    assert(MapScales.of(Vector(one -> p, one -> p)).isLeft)
    val a          = get(MapScales.of(Vector(two -> p, one -> p)));
    val b          = get(MapScales.of(Vector(three -> p, two -> p)))
    val mismatched = MapComparison.scales(a, b, MapSimilarityMethod.Cosine)
    assertEquals(mismatched.rows.map(_._1.value), Vector(1.0, 2.0, 3.0))
    assertEquals(mismatched.requested, 3); assertEquals(mismatched.contributing, 1)
    assert(mismatched.mean.isLeft)
    assertEquals(mismatched.rows.head._2, Left(MapScaleFailure.MissingRight))
    assertEquals(mismatched.rows.last._2, Left(MapScaleFailure.MissingLeft))
    val partial = MapComparison.scales(
      get(MapScales.of(Vector(one -> uniform, two -> p))),
      a,
      MapSimilarityMethod.Pearson
    )
    assertEquals(partial.requested, 2); assertEquals(partial.contributing, 1);
    assert(partial.mean.isLeft)
    assert(
      typeCheckErrors(
        """new eyes4s.compare.MapScales[eyes4s.kernel.Unit2D.Px](Vector.empty)"""
      ).nonEmpty
    )
  }

  test(
    "direct map comparison agrees with the public symmetric smoothing lift for every instance"
  ) {
    val frame = get(Frame.screen("lift", 5, 3)); val g = get(Grid.over(frame, 5, 3));
    val clock = ClockId("lift")
    def path(xs: Vector[Double]) = get(
      Scanpath.of(
        frame,
        clock,
        IArray.from(xs.zipWithIndex.map { (x, i) =>
          get(
            Event.Fixation.of(
              get(Interval.of(clock, Instant.millis(i * 100L), Instant.millis(i * 100L + 50))),
              Pt[Px](x, Vector(.5, 2.5, 1.5)(i)),
              0.0,
              DispersionMethod.RmsRadius,
              1
            )
          )
        })
      )
    )
    val a        = path(Vector(.5, 2.5, 4.5)); val b = path(Vector(4.5, 3.5, 1.5))
    val smoother = Smoother.gaussian(get(Sigma.px(1)), EdgePolicy.Renormalise)
    val ma       = get(smoother.density(get(a.occupancy()), g));
    val mb       = get(smoother.density(get(b.occupancy()), g))
    MapSimilarityMethod.values.foreach { method =>
      val direct = method.instance[Px].compare(ma, mb)
      val lifted = Lift.viaSmoothingSymmetric(method.instance[Px], smoother, g).compare(a, b)
      assertEquals(lifted, direct)
    }
  }

  test(
    "exact small transport oracle, pinned emd output, sliced W1 and squared-cost Sinkhorn are distinct"
  ) {
    val frame = get(Frame.screen("transport", 2, 2)); val g = get(Grid.over(frame, 2, 2))
    val a     = mass(Vector(1, 0, 0, 0), g); val b          = mass(Vector(0, 0, 0, 1), g)
    val exact = math.sqrt(2.0)
    near(MapReference.emdDiracSimilarity, 1 / (1 + exact))
    val sliced =
      get(Transport.slicedWasserstein[Px](get(ProjectionDirections.of(4))).compare(a, b)).value
    near(sliced, (2 + math.sqrt(2)) / 4)
    val sinkhorn = get(Transport.sinkhorn[Px]().compare(a, b)).value
    near(sinkhorn, 2.0) // unique plan, squared Euclidean cost
    assert(math.abs(sliced - exact) > 0.5)
    assert(math.abs(sinkhorn - exact) > 0.5)
    near(MapReference.emdSplitSimilarity, 0.5) // every positive source-target distance is one
  }

  test(
    "non-square generated scales agree with independent two-dimensional Gaussian and cosine oracles"
  ) {
    val frame = get(Frame.screen("pyramid-oracle", 5, 3)); val g = get(Grid.over(frame, 5, 3))
    val cells = Vector((0, 0), (2, 2), (4, 1)); val weights      = Vector(5.0, 3.0, 1.0)
    def measure(ws: Vector[Double]) = get(
      PointMeasure.of(
        frame,
        IArray.from(cells.map((x, y) => Pt[Px](x + .5, y + .5))),
        IArray.from(ws)
      )
    )
    val sigmas = Vector(get(Sigma.px(2)), get(Sigma.px(1)))
    def oracle(s: Double, ws: Vector[Double]): Vector[Double] =
      val radius                    = math.ceil(3 * s).toInt
      def k(x: Int, y: Int): Double =
        if math.abs(x) > radius || math.abs(y) > radius then 0.0
        else math.exp(-0.5 * (x * x + y * y) / (s * s))
      val raw = (0 until 15).map { i =>
        cells
          .zip(ws)
          .map { case ((x, y), w) =>
            w * k(i % 5 - x, i / 5 - y) / (0 until 15).map(j => k(j % 5 - x, j / 5 - y)).sum
          }
          .sum
      }.toVector
      raw.map(_ / raw.sum)
    val a = get(Pyramid.of(measure(weights), g, sigmas, EdgePolicy.Renormalise))
    val b = get(Pyramid.of(measure(weights.reverse), g, sigmas.reverse, EdgePolicy.Renormalise))
    val result = MapComparison.scales(
      get(MapScales.of(a.levels)),
      get(MapScales.of(b.levels)),
      MapSimilarityMethod.Cosine
    )
    result.rows.foreach { (sigma, value) =>
      val x = oracle(sigma.value, weights); val y = oracle(sigma.value, weights.reverse)
      get(a.at(sigma).toRight("missing scale")).values.toVector
        .zip(x)
        .foreach((u, v) => near(u, v))
      get(b.at(sigma).toRight("missing scale")).values.toVector
        .zip(y)
        .foreach((u, v) => near(u, v))
      near(
        get(value).value,
        x.zip(y).map(_ * _).sum / math.sqrt(x.map(v => v * v).sum * y.map(v => v * v).sum)
      )
    }
    assertEquals(result.requested, 2); assertEquals(result.contributing, 2)
  }

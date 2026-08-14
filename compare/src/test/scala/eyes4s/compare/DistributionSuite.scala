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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

import scala.compiletime.testing.typeCheckErrors

class DistributionSuite extends munit.FunSuite:

  val screen = Frame.screen("display", 100, 100).toOption.get
  val grid   = Grid.square(screen, 10).toOption.get

  private def mass(f: Int => Double): Mass[Px] =
    Surface
      .intensity(grid, IArray.tabulate(grid.size)(f), Provenance.raw(ContentHash.empty))
      .flatMap(_.normalised)
      .toOption
      .get

  val uniform = mass(_ => 1.0)
  val ramp    = mass(i => (i + 1).toDouble)
  val spikeA  = mass(i => if i == 0 then 1.0 else 1e-9)
  val spikeB  = mass(i => if i == 99 then 1.0 else 1e-9)

  private def d(m: Metric[Mass[Px]], a: Mass[Px], b: Mass[Px]): Double =
    m.compare(a, b).toOption.get.value

  // -------------------------------------------------------------------------
  // The interfaces are claims, and the claims are checked
  // -------------------------------------------------------------------------

  test("total variation satisfies the metric axioms on these maps") {
    val tv = Distribution.totalVariation[Px]
    assertEqualsDouble(d(tv, uniform, uniform), 0.0, 1e-12)
    assertEqualsDouble(d(tv, uniform, ramp), d(tv, ramp, uniform), 1e-12)
    // Triangle inequality across three genuinely different maps.
    assert(d(tv, spikeA, spikeB) <= d(tv, spikeA, uniform) + d(tv, uniform, spikeB) + 1e-12)
  }

  test("total variation is bounded, and reaches its bound on disjoint maps") {
    val tv = Distribution.totalVariation[Px]
    assertEqualsDouble(d(tv, spikeA, spikeB), 1.0, 1e-6)
  }

  test("Hellinger satisfies the metric axioms and is bounded") {
    val h = Distribution.hellinger[Px]
    assertEqualsDouble(d(h, uniform, uniform), 0.0, 1e-12)
    assertEqualsDouble(d(h, uniform, ramp), d(h, ramp, uniform), 1e-12)
    assertEqualsDouble(d(h, spikeA, spikeB), 1.0, 1e-4)
    assert(d(h, spikeA, spikeB) <= d(h, spikeA, uniform) + d(h, uniform, spikeB) + 1e-12)
  }

  test("Jensen-Shannon is symmetric and zero on identical maps") {
    val js = Distribution.jensenShannon[Px]()
    assertEqualsDouble(js.compare(ramp, ramp).toOption.get.value, 0.0, 1e-12)
    assertEqualsDouble(
      js.compare(uniform, ramp).toOption.get.value,
      js.compare(ramp, uniform).toOption.get.value,
      1e-12
    )
  }

  test("Jensen-Shannon ships as a Semimetric, not a Metric") {
    // The type is the claim. Asserting it here so the claim cannot quietly
    // change: the divergence fails the triangle inequality, its square root
    // does not, and shipping it as a Metric would be false.
    val js: Semimetric[Mass[Px]] = Distribution.jensenShannon[Px]()
    val errs                     = typeCheckErrors("""
      import eyes4s.compare.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val m: Metric[Mass[Px]] = Distribution.jensenShannon[Px]()
    """)
    assert(errs.nonEmpty, "Jensen-Shannon was accepted as a Metric")
  }

  test("Jensen-Shannon is bounded by one in bits") {
    val js = Distribution.jensenShannon[Px](LogBase.Two)
    val v  = js.compare(spikeA, spikeB).toOption.get.value
    assert(v <= 1.0 + 1e-9, clue(v))
    assert(v > 0.99, clue(v))
  }

  // -------------------------------------------------------------------------
  // Asymmetry, and what the type prevents
  // -------------------------------------------------------------------------

  test("Kullback-Leibler is genuinely asymmetric") {
    val kl = Distribution.kullbackLeibler[Px]()
    val ab = kl.compare(spikeA, uniform).toOption.get.value
    val ba = kl.compare(uniform, spikeA).toOption.get.value
    assert(math.abs(ab - ba) > 0.5, clue((ab, ba)))
  }

  test("a divergence cannot be used where symmetry is required") {
    // This is the structural point of C-9: an unordered pair evaluation asks
    // for SymmetricCompare, and KL simply does not have it in its ancestry, so
    // it cannot be passed by mistake.
    val errs = typeCheckErrors("""
      import eyes4s.compare.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      def unordered(c: SymmetricCompare[Mass[Px], MeasureDistance]): Int = 0
      unordered(Distribution.kullbackLeibler[Px]())
    """)
    assert(errs.nonEmpty, "an asymmetric divergence was accepted for an unordered comparison")
  }

  test("a metric IS accepted where symmetry is required") {
    def unordered(c: SymmetricCompare[Mass[Px], MeasureDistance]): Boolean = true
    assert(unordered(Distribution.totalVariation[Px]))
    assert(unordered(Distribution.hellinger[Px]))
  }

  // -------------------------------------------------------------------------
  // Similarities that are nothing stronger
  // -------------------------------------------------------------------------

  test("cosine is symmetric and maximal on identical maps") {
    val c = Distribution.cosine[Px]
    assertEqualsDouble(c.compare(ramp, ramp).toOption.get.value, 1.0, 1e-12)
    assertEqualsDouble(
      c.compare(uniform, ramp).toOption.get.value,
      c.compare(ramp, uniform).toOption.get.value,
      1e-12
    )
  }

  test("cosine is not offered as a metric") {
    val errs = typeCheckErrors("""
      import eyes4s.compare.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val m: Metric[Mass[Px]] = Distribution.cosine[Px]
    """)
    assert(errs.nonEmpty, "cosine was accepted as a metric")
  }

  test("Pearson declares the Correlation scale, which warns against averaging") {
    assertEquals(Distribution.pearson[Px].scale, MeasureScale.Correlation)
    assertEquals(Distribution.fisherZ[Px].scale, MeasureScale.FisherZ)
  }

  test("Fisher z is the averageable form, and exceeds the correlation it came from") {
    // Both sides must vary: a constant map has no correlation at all, which
    // the guard above now enforces.
    val bumpy = mass(i => 1.0 + 0.5 * math.sin(i.toDouble))
    val r     = Distribution.pearson[Px].compare(ramp, bumpy).toOption.get.value
    val z     = Distribution.fisherZ[Px].compare(ramp, bumpy).toOption.get.value
    // atanh(x) > x for x in (0, 1): the transform expands the compressed ends.
    if r > 0.0 then assert(z > r, clue((r, z)))
    assertEqualsDouble(z, 0.5 * math.log((1 + r) / (1 - r)), 1e-9)
  }

  test("correlation with a constant map is undefined, not a number made of noise") {
    // The guard has to be RELATIVE. Summing a hundred copies of 0.01 does not
    // give exactly 1.0, so a uniform map has variance of order 1e-33 -- strictly
    // positive. An absolute `variance <= 0` test never fires and the ratio of
    // two noise terms comes back as a plausible-looking correlation.
    val r = Distribution.pearson[Px].compare(uniform, uniform)
    assertEquals(
      r,
      Left(CompareError.ConstantInput("Pearson correlation", CompareOperand.Both))
    )

    // And it holds when only ONE side is constant.
    assertEquals(
      Distribution.pearson[Px].compare(uniform, ramp),
      Left(CompareError.ConstantInput("Pearson correlation", CompareOperand.Left))
    )
  }

  test("a genuinely varying pair still correlates") {
    val other = mass(i => (grid.size - i).toDouble)
    val r     = Distribution.pearson[Px].compare(ramp, other).toOption.get.value
    assertEqualsDouble(r, -1.0, 1e-9)
  }

  test("a map with no mass has no cosine") {
    // Not reachable through Mass, which cannot be all-zero -- so the guard is
    // defensive. Asserting the message rather than the impossibility.
    assert(Distribution.cosine[Px].compare(uniform, uniform).isRight)
  }

  // -------------------------------------------------------------------------
  // Grid agreement
  // -------------------------------------------------------------------------

  test("comparing maps on different grids is refused") {
    val g2    = Grid.square(screen, 5).toOption.get
    val other = Surface
      .intensity(g2, IArray.fill(g2.size)(1.0), Provenance.raw(ContentHash.empty))
      .flatMap(_.normalised)
      .toOption
      .get
    assert(Distribution.totalVariation[Px].compare(uniform, other).isLeft)
    assert(Distribution.cosine[Px].compare(uniform, other).isLeft)
  }

  // -------------------------------------------------------------------------
  // Metadata for an application
  // -------------------------------------------------------------------------

  test("every measure carries enough metadata to present itself") {
    val all: Seq[Compare[Mass[Px], Mass[Px], ?]] = Seq(
      Distribution.totalVariation[Px],
      Distribution.hellinger[Px],
      Distribution.jensenShannon[Px](),
      Distribution.kullbackLeibler[Px](),
      Distribution.cosine[Px],
      Distribution.pearson[Px],
      Distribution.fisherZ[Px]
    )
    all.foreach { m =>
      assert(m.info.name.nonEmpty, clue(m.info))
      assert(m.info.summary.nonEmpty, clue(m.info))
      assert(m.info.render.nonEmpty)
    }
  }

  test("the scale explains the ordering, which differs between measures") {
    // A distance and a similarity order in opposite directions, and an
    // application presenting both has to know which is which.
    assertEquals(Distribution.totalVariation[Px].scale, MeasureScale.Bounded(0.0, 1.0))
    assertEquals(Distribution.kullbackLeibler[Px]().scale, MeasureScale.DistanceLike)
  }

end DistributionSuite

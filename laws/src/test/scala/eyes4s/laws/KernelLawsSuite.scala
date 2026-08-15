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

import eyes4s.kernel.*

import cats.kernel.laws.discipline.{CommutativeGroupTests, OrderTests}

/** Runs the published rule sets against the kernel's own instances.
  *
  * This suite is deliberately thin: it is the suite a downstream author copies
  * when they write their own `Warp` or their own `Smoother`, so it should show
  * nothing but `checkAll` calls and the tolerance being passed.
  */
class KernelLawsSuite extends munit.DisciplineSuite:

  import Generators.given

  // Warp: a partial category, its negative half, inverses, and the projection.
  checkAll("Warp.partialCategory", WarpLaws.category(Tolerance.exactish))
  checkAll("Warp.frameCheck", WarpLaws.compositionRefusesMismatch)
  checkAll("Warp.inverse", WarpLaws.inverses(Tolerance.exactish))
  checkAll("Warp.tangent", WarpLaws.tangentRoundTrip(Tolerance.roundTrip))

  // Occupancy. Both suites take the grid they operate over, so coverage is the
  // caller's choice rather than a hidden constant, and the module laws are
  // stated against the specific module being tested -- its zero is dimensioned
  // by that grid.
  private val frame = Frame.unitSquare("laws-frame").toOption.get
  private val grid  = Grid.over(frame, 12, 9).toOption.get

  checkAll("Region.booleanAlgebra", RegionLaws.lattice(grid))
  checkAll("Surface.module", SurfaceLaws.module(grid))
  checkAll("Surface.mass", SurfaceLaws.mass(grid))
  checkAll("Measure.integrate", SurfaceLaws.measure(grid))

  // Machine: composition, observationally.
  checkAll(
    "Machine.category",
    MachineLaws.category(Generators.genIntInput, Generators.genIntMachine)
  )
  checkAll(
    "Machine.deterministic",
    MachineLaws.deterministic(Generators.genIntInput, Generators.genIntMachine)
  )

  // Measures. Each is audited under the interface it CLAIMS: a metric gets the
  // triangle inequality, a semimetric does not, a divergence is not asked for
  // symmetry, and every SymmetricCompare gets the one law C-9 depends on.
  import eyes4s.compare.*
  import eyes4s.kernel.Unit2D.Norm

  private val massGen = Generators.genMass(grid)

  /** Two masses the caller KNOWS are different: disjoint corners of the grid.
    * The separation law needs a ground truth the generator cannot supply.
    */
  private val distinctPair =
    val spike = (target: Int) =>
      Surface
        .intensity(
          grid,
          IArray.tabulate(grid.size)(i => if i == target then 1.0 else 0.0),
          Provenance.raw(ContentHash.empty)
        )
        .flatMap(_.normalised)
        .toOption
        .get
    (spike(0), spike(grid.size - 1))

  /** Distinct masses inside exact KL's domain: both have full support. */
  private val klDistinctPair =
    val dense = (target: Int) =>
      Surface
        .intensity(
          grid,
          IArray.tabulate(grid.size)(i => if i == target then 2.0 else 1.0),
          Provenance.raw(ContentHash.empty)
        )
        .flatMap(_.normalised)
        .toOption
        .get
    (dense(0), dense(grid.size - 1))

  checkAll(
    "TotalVariation.metric",
    MeasureLaws.metric(Distribution.totalVariation[Norm], massGen, distinctPair)
  )
  checkAll(
    "Hellinger.metric",
    MeasureLaws.metric(Distribution.hellinger[Norm], massGen, distinctPair)
  )
  List(1, 2, 8, 16).foreach { count =>
    val directions = ProjectionDirections.of(count).toOption.get
    checkAll(
      s"SlicedWasserstein.$count-directions.semimetric",
      MeasureLaws.semimetric(
        Transport.slicedWasserstein[Norm](directions),
        massGen
      )
    )
  }
  checkAll(
    "JensenShannon.semimetric",
    MeasureLaws.semimetric(Distribution.jensenShannon[Norm](), massGen)
  )
  checkAll(
    "KullbackLeibler.divergence",
    MeasureLaws.divergence(Distribution.kullbackLeibler[Norm](), massGen, klDistinctPair)
  )
  checkAll(
    "Cosine.symmetry",
    MeasureLaws.symmetry[Mass[Norm], Similarity](
      Distribution.cosine[Norm],
      massGen,
      (a, b) => math.abs(a.value - b.value) < 1e-9
    )
  )
  checkAll("TotalVariation.info", MeasureLaws.described(Distribution.totalVariation[Norm]))
  checkAll("Sinkhorn.info", MeasureLaws.described(Transport.sinkhorn[Norm]()))

  // Time: the algebraic structure claimed in Span's scaladoc, tested by the
  // standard cats bundles rather than by hand-rolled assertions.
  checkAll("Span.commutativeGroup", CommutativeGroupTests[Span].commutativeGroup)
  checkAll("Span.order", OrderTests[Span].order)
  checkAll("Instant.order", OrderTests[Instant].order)
  checkAll("Angle.order", OrderTests[Angle].order)

end KernelLawsSuite

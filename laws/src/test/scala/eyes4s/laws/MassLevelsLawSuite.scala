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
import eyes4s.kernel.Unit2D.Norm

import org.scalacheck.Test

/** The public laws run against the shipped selector and deliberate mutants.
  *
  * ==Mutation receipt==
  *
  * | mutant              | injected fault                         | killed by |
  * |---------------------|----------------------------------------|-----------|
  * | ascending-threshold | accumulates from the least dense cells | coverage and threshold monotonicity |
  * | tie-cell-dropped    | removes one cell from a tied threshold | a threshold includes all and only cells at or above it |
  *
  * Fixed seeds make this receipt reproducible. A kill is a ScalaCheck
  * `Test.Failed` result only: an exception or exhausted generator is not
  * evidence that a law discriminates.
  */
class MassLevelsLawSuite extends munit.DisciplineSuite:

  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val frame     = get(Frame.unitSquare("mass-level-laws"))
  private val grid      = get(Grid.over(frame, 4, 4))
  private val tolerance = Tolerance.exactish
  private val masses    = Generators.genMass(grid)
  private val tiedMass  =
    get(
      Surface
        .intensity(grid, IArray.fill(grid.size)(1.0), Provenance.raw(ContentHash.empty))
        .flatMap(_.normalised)
    )

  private val shipped: MassLevelsLaws.Selector[Norm] = (mass, coverages) =>
    MassLevels
      .of(mass, coverages)
      .map(
        _.map(level =>
          MassLevelsLaws
            .Observed(level.coverage, level.threshold, level.coveredMass, level.cells)
        )
      )

  checkAll("MassLevels.hierarchy", MassLevelsLaws.hierarchy(shipped, masses, tolerance))

  private val mutationParameters =
    Test.Parameters.default.withMinSuccessfulTests(100).withInitialSeed(0x4d4153534cL)

  private def ascending(
      mass: Mass[Norm],
      coverages: Vector[Double]
  ): Either[MassLevelError, Vector[MassLevelsLaws.Observed]] =
    Right(coverages.map { coverage =>
      val values = mass.values.toVector.sorted
      val chosen = values
        .scanLeft(0.0)(_ + _)
        .tail
        .zip(values)
        .collectFirst {
          case (total, threshold) if total >= coverage => (threshold, total)
        }
        .getOrElse((values.last, values.sum))
      val selected = mass.values.iterator.filter(_ >= chosen._1).toVector
      MassLevelsLaws.Observed(coverage, chosen._1, selected.sum, selected.length)
    })

  private def dropsTies(mass: Mass[Norm], coverages: Vector[Double]) =
    shipped(mass, coverages).map(_.map { level =>
      val cells = math.max(0, level.cells - 1)
      level.copy(cells = cells)
    })

  private def outcomes(
      selector: MassLevelsLaws.Selector[Norm],
      generator: org.scalacheck.Gen[Mass[Norm]] = masses
  ): Vector[String] =
    val rules = MassLevelsLaws.hierarchy(selector, generator, tolerance).all.properties
    rules.collect {
      case (name, prop) if genuinelyFalsified(prop) => name.stripPrefix("massLevels.hierarchy.")
    }.toVector

  private def genuinelyFalsified(prop: org.scalacheck.Prop): Boolean =
    Test.check(mutationParameters, prop).status match
      case Test.Failed(_, _) => true
      case _                 => false

  private def survives(
      selector: MassLevelsLaws.Selector[Norm],
      generator: org.scalacheck.Gen[Mass[Norm]] = masses
  ): Boolean =
    MassLevelsLaws
      .hierarchy(selector, generator, tolerance)
      .all
      .properties
      .forall((_, prop) => Test.check(mutationParameters, prop).passed)

  test("published laws pass for the selector and kill the named mutants") {
    assert(survives(shipped))
    assertEquals(
      outcomes(ascending),
      Vector(
        "every returned region covers its request",
        "thresholds do not rise with requested coverage"
      )
    )
    assertEquals(
      outcomes(dropsTies, org.scalacheck.Gen.const(tiedMass)),
      Vector("a threshold includes all and only cells at or above it")
    )
  }

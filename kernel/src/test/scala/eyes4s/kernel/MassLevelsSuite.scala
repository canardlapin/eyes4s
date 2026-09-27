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

class MassLevelsSuite extends munit.FunSuite:

  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val frame = get(Frame.screen("mass-levels", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val raw   = Provenance.raw(ContentHash.empty)

  private def mass(values: Double*): Mass[Px] =
    get(Surface.mass(grid, IArray.from(values), raw))

  test("selects exact binary-rational levels by an independent hand oracle") {
    val selected = get(MassLevels.of(mass(0.5, 0.25, 0.125, 0.125), Vector(0.5, 0.75, 1.0)))
    assertEquals(selected.map(_.threshold), Vector(0.5, 0.25, 0.125))
    assertEquals(selected.map(_.coveredMass), Vector(0.5, 0.75, 1.0))
    assertEquals(selected.map(_.cells), Vector(1, 2, 4))
  }

  test("a threshold includes every exact tie") {
    val selected = get(MassLevels.of(mass(0.4, 0.3, 0.3, 0.0), Vector(0.6))).head
    assertEquals(selected.threshold, 0.3)
    assertEquals(selected.coveredMass, 1.0)
    assertEquals(selected.cells, 3)
  }

  test("zero-valued cells do not lower an already reached threshold") {
    val selected = get(MassLevels.of(mass(1.0, 0.0, 0.0, 0.0), Vector(1.0))).head
    assertEquals(selected.threshold, 1.0)
    assertEquals(selected.cells, 1)
  }

  test("preserves requested order and duplicates") {
    val selected = get(MassLevels.of(mass(0.5, 0.25, 0.125, 0.125), Vector(0.75, 0.5, 0.75)))
    assertEquals(selected.map(_.coverage), Vector(0.75, 0.5, 0.75))
    assertEquals(selected.map(_.threshold), Vector(0.25, 0.5, 0.25))
  }

  test("empty requests have an empty result") {
    assertEquals(MassLevels.of(mass(0.25, 0.25, 0.25, 0.25), Vector.empty), Right(Vector.empty))
  }

  test("coverage must be finite and in the unit interval excluding zero") {
    Vector(0.0, -0.1, 1.1, Double.NaN, Double.PositiveInfinity).foreach { coverage =>
      MassLevels.of(mass(0.25, 0.25, 0.25, 0.25), Vector(coverage)) match
        case Left(MassLevelError.InvalidCoverage(index, actual)) =>
          assertEquals(index, 0)
          if coverage.isNaN then assert(actual.isNaN)
          else assertEquals(actual, coverage)
        case other => fail(s"expected invalid coverage, got $other")
    }
  }

  test("does not renormalise a mass whose accepted total is short of a request") {
    val short = get(Surface.mass(grid, IArray(0.4, 0.2, 0.2, 0.0), raw, tolerance = 0.25))
    assertEquals(
      MassLevels.of(short, Vector(0.9)),
      Left(MassLevelError.InsufficientTotal(0, 0.9, 0.8))
    )
  }

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

package eyes4s.design

/** The iteration cap is a safety net that Lawson-Hanson does not reach on a
  * full-rank design, so it is lowered here to reach `NotConverged`.
  */
class ConstrainedLeastSquaresLimitSuite extends munit.FunSuite:
  // The step-back design of ConstrainedDecompositionSuite: b, c and a enter,
  // then one interpolation step removes c.
  private val rows = Vector(
    Vector(2.0, 1.0, 2.0),
    Vector(1.0, 3.0, 0.0),
    Vector(2.0, 2.0, 2.0),
    Vector(2.0, 0.0, 3.0)
  )
  private val y = Vector(4.0, 4.0, 4.0, 0.0)

  private def within(limit: Int) =
    ConstrainedLeastSquares.run(
      rows,
      y,
      RelativeRankTolerance.default,
      RelativeDualTolerance.default,
      sumToOne = false,
      Some(limit)
    )

  test("the limit stops the active set as NotConverged with the violation left") {
    // After b and c, a still has gradient 56/101 against norms sqrt(13) and sqrt(48).
    within(2) match
      case Left(LeastSquaresError.NotConverged(2, 2, violation)) =>
        val expected = 56.0 / 101.0 / math.sqrt(13.0 * 48.0)
        assert(math.abs(violation - expected) <= 1e-12, s"$violation != $expected")
      case other => fail(s"$other")
    assert(within(3) match
      case Left(LeastSquaresError.NotConverged(3, 3, _)) => true
      case _                                             => false)
    assertEquals(within(4).map(_.iterations), Right(4))
    assertEquals(
      within(4).map(_.coefficients),
      ConstrainedLeastSquares.nonNegative(rows, y).map(_.coefficients)
    )
  }

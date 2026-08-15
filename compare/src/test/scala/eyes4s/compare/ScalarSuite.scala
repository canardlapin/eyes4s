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

import scala.compiletime.testing.typeCheckErrors

class ScalarSuite extends munit.FunSuite:

  test("MeasureDistance admits exactly finite non-negative values") {
    assertEquals(MeasureDistance.of(0.0), Right(MeasureDistance.zero))
    assertEquals(MeasureDistance.of(2.5).map(_.value), Right(2.5))
    assertEquals(
      MeasureDistance.of(-0.1),
      Left(ComparisonValueError.NegativeMeasureDistance(-0.1))
    )
    assert(
      MeasureDistance.of(Double.NaN) match
        case Left(ComparisonValueError.NonFiniteMeasureDistance(value)) => value.isNaN
        case _                                                          => false
    )
    assertEquals(
      MeasureDistance.of(Double.PositiveInfinity),
      Left(ComparisonValueError.NonFiniteMeasureDistance(Double.PositiveInfinity))
    )
  }

  test("generic Similarity owns finiteness but not a contextual scale") {
    assertEquals(Similarity.of(-2.0).map(_.value), Right(-2.0))
    assertEquals(Similarity.of(4.0).map(_.value), Right(4.0))
    assert(
      Similarity.of(Double.NaN) match
        case Left(ComparisonValueError.NonFiniteSimilarity(value)) => value.isNaN
        case _                                                     => false
    )
  }

  test("unit similarities and MultiMatch scores enforce every component bound") {
    assertEquals(Similarity01.of("shape", 0.0).map(_.value), Right(0.0))
    assertEquals(Similarity01.of("shape", 1.0).map(_.value), Right(1.0))
    assertEquals(
      Similarity01.of("shape", 1.01),
      Left(ComparisonValueError.InvalidUnitSimilarity("shape", 1.01))
    )
    assertEquals(
      MultiMatchScore.of(0.1, 0.2, -0.1, 0.4, 0.5),
      Left(ComparisonValueError.InvalidUnitSimilarity("length", -0.1))
    )
    assert(
      MultiMatchScore.of(0.1, Double.NaN, 0.3, 0.4, 0.5) match
        case Left(ComparisonValueError.InvalidUnitSimilarity("direction", value)) => value.isNaN
        case _                                                                    => false
    )
  }

  test("MultiMatchScore retains named access and structural equality") {
    val first  = MultiMatchScore.of(0.1, 0.2, 0.3, 0.4, 0.5).toOption.get
    val second = MultiMatchScore.of(0.1, 0.2, 0.3, 0.4, 0.5).toOption.get
    assertEquals(first, second)
    assertEqualsDouble(first.shape, 0.1, 0.0)
    assertEqualsDouble(first.mean, 0.3, 1e-12)
  }

  test("raw and product-constructor escape hatches are unavailable") {
    val errors = typeCheckErrors("""
      import eyes4s.compare.*
      val distance: MeasureDistance = -1.0
      val similarity: Similarity = Double.NaN
      val unit: Similarity01 = 2.0
      val score = MultiMatchScore(0.0, 0.0, 0.0, 0.0, 2.0)
      val valid = MultiMatchScore.of(0.0, 0.0, 0.0, 0.0, 1.0).toOption.get
      valid.copy(duration = 2.0)
    """)
    assert(errors.length >= 5, clue(errors.map(_.message)))
  }

end ScalarSuite

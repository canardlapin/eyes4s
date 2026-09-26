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

package consumer

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import cats.kernel.Order

class PublicCompareSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  test("comparison conveniences preserve metric meaning score units and structural identity") {
    val frame      = get(Frame.screen("comparison-consumer", 2, 1))
    val grid       = get(Grid.over(frame, 2, 1))
    val provenance = Provenance.raw(ContentHash.of(IArray(1.0, 0.0)))
    val left       = get(Surface.mass(grid, IArray(1.0, 0.0), provenance))
    val right      = get(Surface.mass(grid, IArray(0.0, 1.0), provenance))
    val distance   = get(Distribution.totalVariation[Px].distance(left, right))
    assertEquals(distance.value, 1.0); assert(distance.render.contains("1"))
    assert(summon[Order[MeasureDistance]].gt(distance, MeasureDistance.zero))
    val similarity = get(Similarity.of(0.5))
    assert(summon[Order[Similarity]].gt(similarity, Similarity.zero));
    assert(similarity.render.contains("0.5"))
    val score = get(MultiMatchScore.of(0.1, 0.2, 0.3, 0.4, 0.5))
    assertEquals(score.hashCode, get(MultiMatchScore.of(0.1, 0.2, 0.3, 0.4, 0.5)).hashCode)
    assertEquals(score.toString, score.render); assert(score.render.contains("shape=0.100"))
    val alignment =
      AlignmentPath(Vector(AlignmentStep.Match(0, 0), AlignmentStep.SkipRight(1)), 2.5)
    assertEquals(Alignment.dtw.name, "dtw")
    assertEquals(Alignment.needlemanWunsch(1).name, "Needleman-Wunsch")
    assertEquals(alignment.length, 2); assert(alignment.render.contains("2 steps"))
    assert(alignment.render.contains("2.5"))
  }

  test("standard CRQA configuration exposes the independently expected recurrence matrix") {
    val frame  = get(Frame.screen("recurrence", 10, 10)); val clock = ClockId("recurrence")
    val events = Vector(1.0, 5.0).zipWithIndex.map { (x, i) =>
      get(
        Event.Fixation.of(
          get(Interval.lasting(clock, Instant.millis(i * 10L), Span.millis(5))),
          Pt[Px](x, 1),
          0.0,
          DispersionMethod.RmsRadius,
          1
        )
      )
    }
    val path   = get(Scanpath.of(frame, clock, IArray.from(events)))
    val config = CrqaConfig.standard(get(Distance.px(0)))
    val result = get(Crqa.analyse(path, path, config))
    assertEquals(result.matrix.toRows, Vector(Vector(true, false), Vector(false, true)))
    assert(result.matrix.render.contains("2"))
  }

  test("comparison failures name the failed parameters sources and component") {
    val examples = Vector(
      ComparisonConfigurationError.InvalidIterationCount("algorithm-X", -17).message -> Vector(
        "algorithm-X",
        "-17"
      ),
      CrqaParameterError.NonPositiveEmbeddingDimension(-19).message   -> Vector("-19"),
      FixationComparisonError.Parameter("threshold-X", -0.25).message -> Vector(
        "threshold-X",
        "-0.25"
      ),
      MapComparisonError.UnsupportedMethod("unknown-X").message        -> Vector("unknown-X"),
      MapScaleFailure.MissingLeft.message                              -> Vector("left"),
      MapScaleFailure.MissingRight.message                             -> Vector("right"),
      OverlapFailure.NonfiniteDistance(1.25, 2.25, 3.25, 4.25).message -> Vector(
        "(1.25,2.25)",
        "(3.25,4.25)"
      )
    )
    examples.foreach { (message, operands) =>
      operands.foreach(o => assert(message.contains(o), message))
    }
    val underlying = CompareError.TooShort("left-X", 1, 3)
    assertEquals(MapScaleFailure.Comparison(underlying).message, underlying.message)
  }

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

import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*

class PublicDetectSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("algorithm metadata is checked and hash collections retain equal values") {
    val doi = get(Doi.from("10.1000/example"))
    assertEquals(doi.url, "https://doi.org/10.1000/example")
    def citation = get(Citation.of(Vector("Author"), 2026, "A method", doi))
    assertEquals(Set(citation, citation).size, 1)
    assert(Citation.of(Vector.empty, 2026, "A method", doi).isLeft)
    assert(
      Citation.of(Vector("Author"), -7, "A method", doi).left.exists(_.message.contains("-7"))
    )
    assert(Citation.of(Vector("Author"), 2026, " ", doi).isLeft)
    assertEquals(
      Set(get(AlgorithmVersion.of(1, 2, 3)), get(AlgorithmVersion.of(1, 2, 3))).size,
      1
    )
    val card    = AlgorithmCards.ivt
    val rebuilt = get(
      AlgorithmCard.of(
        card.id,
        card.version,
        card.name,
        card.citations,
        card.assumptions,
        card.deviations,
        card.execution,
        card.references
      )
    )
    assertEquals(card.hashCode, rebuilt.hashCode)
    assertEquals(Set(card, rebuilt).size, 1)
  }

  test("threshold and gap conveniences retain units and failures identify operands") {
    val threshold = get(EkThresholds.of(1.25, 2.5))
    assertEquals(threshold.hashCode, get(EkThresholds.of(1.25, 2.5)).hashCode)
    assertEquals(threshold.toString, "EkThresholds(1.25,2.5)")
    assert(threshold.render.contains("deg/s")); assert(threshold.render.contains("1.25"))
    assertEquals(MaximumMergeGap.none.span, Span.zero)
    assertEquals(MissingPadding.none.span, Span.zero)
    val failure = EkEstimationError.InsufficientVelocities(2, 5).message
    assert(failure.contains("2") && failure.contains("5"))
    val nested = TimeError.ReversedWindow(19, 13)
    val merged = MergeError.Interval(RecordingRef("source-X"), 13, 19, nested).message
    assert(merged.contains("source-X") && merged.contains("13") && merged.contains("19"))
    assert(merged.contains(nested.message))
  }

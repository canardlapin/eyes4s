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

import scala.compiletime.testing.typeCheckErrors

class TimelineSuite extends munit.FunSuite:

  private val tracker = ClockId("tracker")

  test("construction orders timestamps and retains simultaneous input order") {
    val timeline = Timeline
      .of(
        tracker,
        Vector(
          Mark(Instant.millis(20), "late"),
          Mark(Instant.millis(10), "first-simultaneous"),
          Mark(Instant.millis(10), "second-simultaneous"),
          Mark(Instant.millis(0), "early")
        )
      )
      .fold(error => fail(error.message), identity)

    assertEquals(timeline.clock, tracker)
    assertEquals(
      timeline.marks.map(_.value),
      Vector("early", "first-simultaneous", "second-simultaneous", "late")
    )
    assertEquals(
      timeline.marks.map(_.at.toMicros),
      Vector(0L, 10000L, 10000L, 20000L)
    )
  }

  test("map preserves clock, timestamps, and cardinality") {
    val timeline = Timeline
      .of(tracker, Vector(Mark(Instant.millis(2), 1), Mark(Instant.millis(1), 2)))
      .fold(error => fail(error.message), identity)
    val mapped = timeline.map(_.toString)

    assertEquals(mapped.clock, tracker)
    assertEquals(mapped.marks.map(_.at), timeline.marks.map(_.at))
    assertEquals(mapped.marks.map(_.value), Vector("2", "1"))
    assertEquals(mapped.size, timeline.size)
  }

  test("planned and observed timelines are statically distinct") {
    val errors = typeCheckErrors("""
      val planned: eyes4s.kernel.PlannedTimeline[Int] = ???
      val observed: eyes4s.kernel.ObservedTimeline[Int] = planned
    """)

    assert(errors.nonEmpty)
  }

  test("planned and observed wrappers preserve the same neutral value without promotion") {
    val neutral = Timeline
      .of(tracker, Vector(Mark(Instant.millis(1), "trigger")))
      .fold(error => fail(error.message), identity)
    val planned  = PlannedTimeline.from(neutral)
    val observed = ObservedTimeline.from(neutral)

    assertEquals(planned.timeline, neutral)
    assertEquals(observed.timeline, neutral)
    assertEquals(planned.clock, observed.clock)
    assertEquals(planned.marks, observed.marks)
  }

  test("blank clock identity is rejected before a timeline is constructed") {
    assertEquals(
      Timeline.empty[String](ClockId("  ")),
      Left(TimelineError.BlankClock("  "))
    )
  }

  test("nonnegative time and count quantities enforce their advertised invariants") {
    assertEquals(
      NonNegativeSpan.of(Span.micros(-1L)),
      Left(TimeQuantityError.NegativeNonNegativeSpan(Span.micros(-1L)))
    )
    assertEquals(
      PositiveSpan.of(Span.zero),
      Left(TimeQuantityError.NonPositiveSpan(Span.zero))
    )
    assertEquals(
      NonNegativeLong.of(-1L),
      Left(TimeQuantityError.NegativeNonNegativeLong(-1L))
    )
    val maximum = NonNegativeLong
      .of(Long.MaxValue)
      .fold(error => fail(error.message), identity)
    assertEquals(
      maximum.increment,
      Left(TimeQuantityError.NonNegativeLongOverflow(Long.MaxValue, 1L))
    )
    assertEquals(
      NonNegativeSpan.of(Span.millis(2)).map(_.toMicros),
      Right(2000L)
    )
    assertEquals(
      PositiveSpan.of(Span.micros(1L)).map(_.toMicros),
      Right(1L)
    )
  }

end TimelineSuite

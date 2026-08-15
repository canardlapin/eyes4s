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

package eyes4s.core

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

import scala.compiletime.testing.typeCheckErrors

class EventSuite extends munit.FunSuite:

  private val clock = ClockId("events")
  private val span  =
    Interval.of(clock, Instant.millis(10), Instant.millis(20)).toOption.get
  private val empty =
    Interval.of(clock, Instant.millis(10), Instant.millis(10)).toOption.get

  test("a fixation records a typed dispersion statistic and positive support") {
    val fixation = Event.Fixation
      .of(span, Pt[Deg](1.0, 2.0), 0.25, DispersionMethod.RmsRadius, 10)
      .toOption
      .get

    assertEquals(fixation.dispersion.map(_.method), Some(DispersionMethod.RmsRadius))
    assertEquals(fixation.dispersion.map(_.value), Some(0.25))
    assertEquals(fixation.sampleCount, 10)
    assertEquals(
      Event.Fixation.of(span, Pt[Deg](1.0, 2.0), 0.25, DispersionMethod.RmsRadius, 10),
      Right(fixation)
    )
  }

  test("fixation construction rejects every incoherent summary operand") {
    assertEquals(
      Event.Fixation.of(empty, Pt[Deg](1.0, 2.0), 0.25, DispersionMethod.RmsRadius, 10),
      Left(CoreError.OfEvent(EventError.EmptySpan("fixation", empty)))
    )
    assert(
      Event.Fixation.of(
        span,
        Pt[Deg](Double.NaN, 2.0),
        0.25,
        DispersionMethod.RmsRadius,
        10
      ) match
        case Left(
              CoreError.OfEvent(EventError.NonFinitePoint("fixation", "centre", `span`, x, 2.0))
            ) =>
          x.isNaN
        case _ => false
    )
    assertEquals(
      Event.Fixation.of(span, Pt[Deg](1.0, 2.0), -0.1, DispersionMethod.RmsRadius, 10),
      Left(CoreError.OfEvent(EventError.InvalidDispersion(-0.1, DispersionMethod.RmsRadius)))
    )
    assertEquals(
      Event.Fixation.of(span, Pt[Deg](1.0, 2.0), 0.1, DispersionMethod.RmsRadius, 0),
      Left(CoreError.OfEvent(EventError.NonPositiveSampleCount(0)))
    )
  }

  test("saccades, blinks, and pursuits reject empty or non-finite summaries") {
    assertEquals(
      Event.Saccade.of(empty, Pt[Deg](0.0, 0.0), Pt[Deg](1.0, 1.0), None),
      Left(CoreError.OfEvent(EventError.EmptySpan("saccade", empty)))
    )
    assert(
      Event.Saccade
        .of(span, Pt[Deg](0.0, 0.0), Pt[Deg](Double.PositiveInfinity, 1.0), None) match
        case Left(
              CoreError.OfEvent(
                EventError.NonFinitePoint("saccade", "destination", `span`, x, 1.0)
              )
            ) =>
          x.isPosInfinity
        case _ => false
    )
    assertEquals(
      Event.Blink.of[Deg](empty),
      Left(CoreError.OfEvent(EventError.EmptySpan("blink", empty)))
    )
    assertEquals(
      Event.Pursuit.of[Deg](span, IArray.empty),
      Left(CoreError.OfEvent(EventError.EmptyPursuit(span)))
    )
    assert(
      Event.Pursuit.of(span, IArray(Pt[Deg](0.0, 0.0), Pt[Deg](1.0, Double.NaN))) match
        case Left(CoreError.OfEvent(EventError.NonFinitePursuitPoint(`span`, 1, 1.0, y))) =>
          y.isNaN
        case _ => false
    )
  }

  test("every event variant has a coherent public inhabitant") {
    val from = Pt[Deg](0.0, 0.0)
    val to   = Pt[Deg](1.0, 1.0)

    assert(Event.Saccade.of(span, from, to, None).isRight)
    assert(Event.Blink.of[Deg](span).isRight)
    assertEquals(
      Event.Pursuit.of(span, IArray(from, to)).map(_.path.toVector),
      Right(Vector(from, to))
    )
  }

  test("event product constructors and copy escape hatches are unavailable") {
    val errors = typeCheckErrors("""
      import eyes4s.core.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Deg
      val span = Interval.of(ClockId("c"), Instant.millis(0), Instant.millis(1)).toOption.get
      val invalid = Event.Fixation[Deg](span, Pt[Deg](0.0, 0.0), -1.0, 0)
      val valid = Event.Fixation
        .of(span, Pt[Deg](0.0, 0.0), 0.1, DispersionMethod.RmsRadius, 1)
        .toOption.get
      valid.copy(sampleCount = -1)
    """)
    assert(errors.length >= 2, clue(errors.map(_.message)))
  }

end EventSuite

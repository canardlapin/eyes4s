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
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class PublicCoreSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                             = get(Frame.screen("core-consumer", 20, 20))
  private val clock                             = ClockId("core-consumer-clock")
  private val span = get(Interval.lasting(clock, Instant.epoch, Span.seconds(2)))

  test("event convenience measures retain displacement speed support and absent locations") {
    val saccade = get(Event.Saccade.of(span, Pt[Px](1, 1), Pt[Px](4, 5), None))
    assertEquals(saccade.displacement, Vec2[Px](3, 4))
    assertEquals(saccade.amplitude, 5.0)
    assertEqualsDouble(saccade.direction.toRadians, math.atan2(4, 3), 1e-12)
    assertEquals(saccade.meanVelocity.map(_.value), Some(2.5))
    assertEquals(saccade.location, None)
    val fixation =
      get(Event.Fixation.of(span, Pt[Px](2, 3), 0.5, DispersionMethod.RmsRadius, 3))
    assert(fixation.isIn(Region.fromBounds(frame.bounds)))
    assert(!fixation.isIn(Region.empty[Px]))
    assertEquals(fixation.location, Some(Pt[Px](2, 3)))
    assertEquals(get(Event.Blink.of[Px](span)).location, None)
    val dispersion = get(Dispersion.of[Px](0.5, DispersionMethod.RmsRadius))
    assertEquals(
      dispersion.hashCode,
      get(Dispersion.of[Px](0.5, DispersionMethod.RmsRadius)).hashCode
    )
    val scanpath = get(Scanpath.fromEvents(frame, clock, Vector(fixation)))
    assert(scanpath.render.contains("core-consumer"))
    assert(Scanpath.fromEvents(frame, ClockId("wrong"), Vector(fixation)).isLeft)
    assert(Scanpath.fromEvents[Px](frame, clock, Vector(saccade)).isLeft)
    assertEquals(get(SampleRange.of(2, 5)).hashCode, get(SampleRange.of(2, 5)).hashCode)
  }

  test("recording renderers distinguish fixed irregular and paired observations") {
    val samples = IArray(
      Sample(Instant.epoch, Gaze.Tracked(Pt[Px](1, 1), None)),
      Sample(Instant.millis(1), Gaze.Tracked(Pt[Px](2, 2), None))
    )
    val rate  = Rate.Fixed(get(Hz(1000)))
    val fixed = get(Recording.of(frame, clock, rate, Eye.Left, None, samples))
    assert(fixed.render.contains("core-consumer"));
    assert(fixed.samplingEvidence.render.contains("fixed"))
    val irregular = get(Recording.of(frame, clock, Rate.Irregular, Eye.Left, None, samples))
    assertEquals(irregular.samplingEvidence.render, "irregular-timestamps")
    val binocular = get(
      BinocularRecording.of(
        frame,
        clock,
        rate,
        None,
        IArray(Instant.epoch, Instant.millis(1)),
        IArray.from(samples.map(_.gaze)),
        IArray.from(samples.map(_.gaze))
      )
    )
    assert(binocular.render.contains("2 samples"));
    assert(binocular.render.contains("core-consumer"))
    assertEquals(get(Viewing.millimetres(600, 500, 300)).distance.toMm, 600.0)
  }

  test("support failures retain the policy and exact time operands") {
    val fixed = get(EdgeSupport.fixed(Span.millis(2)))
    assert(fixed.render.contains("2.0ms"))
    assert(EdgeSupport.fixed(Span.micros(-2)).left.exists(_.message.contains("-0.002ms")))
    assert(
      MaximumSupportGap.atMost(Span.micros(-3)).left.exists(_.message.contains("-0.003ms"))
    )
    assert(TemporalSupport.fixed(Span.zero).left.exists(_.message.contains("period=0.0ms")))
    val policy     = get(TemporalSupport.fixed(Span.millis(1)))
    val underlying = SurfaceError.NegativeWeight(7, -2.0)
    val error      = OccupancyError.Measure(policy, underlying)
    assert(error.message.contains(policy.render));
    assert(error.message.contains(underlying.message))
  }

  test("error widening preserves successful values and the exact lower-layer error") {
    import CoreError.*
    val surface: Either[SurfaceError, Int] = Left(SurfaceError.NegativeValue(3, -4.0))
    assertEquals(
      surface.widenSurface,
      Left(CoreError.OfSurface(SurfaceError.NegativeValue(3, -4.0)))
    )
    assertEquals((Right(2): Either[SurfaceError, Int]).widenSurface, Right(2))
    val event: Either[EventError, Int] = Left(EventError.NonPositiveSampleCount(0))
    assertEquals(
      event.widenEvent,
      Left(CoreError.OfEvent(EventError.NonPositiveSampleCount(0)))
    )
    assertEquals((Right(5): Either[EventError, Int]).widenEvent, Right(5))
  }

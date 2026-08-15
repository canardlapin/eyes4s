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
import eyes4s.kernel.Unit2D.Px

class DetectionSupportSuite extends munit.FunSuite:

  private val source    = RecordingRef("support-source")
  private val frame     = Frame.angular("support-frame", 20.0, 20.0).toOption.get
  private val clock     = ClockId("support-clock")
  private val recording = Recording
    .of(
      frame,
      clock,
      Rate.Fixed(Hz(1000.0).toOption.get),
      Eye.Left,
      None,
      IArray.tabulate(6) { index =>
        Sample(
          Instant.millis(index.toLong),
          Gaze.Tracked(Pt[Deg](index.toDouble, 0.0), None)
        )
      }
    )
    .toOption
    .get
  private val span =
    Interval.of(clock, Instant.millis(1), Instant.millis(3)).toOption.get
  private val fixation = Event.Fixation
    .of(span, Pt[Deg](0.0, 0.0), 0.1, DispersionMethod.RmsRadius, 2)
    .toOption
    .get

  test("SampleRange is non-empty, non-negative, and half-open") {
    val range = SampleRange.of(2, 5).toOption.get
    assertEquals(range.length, 3)
    assert(range.contains(2))
    assert(!range.contains(5))
    assertEquals(
      SampleRange.of(-1, 2),
      Left(DetectionSupportError.InvalidSampleRange(-1, 2))
    )
    assertEquals(
      SampleRange.of(2, 2),
      Left(DetectionSupportError.InvalidSampleRange(2, 2))
    )
  }

  test("EventSeries retains matching ordered source support") {
    val range  = SampleRange.of(1, 3).toOption.get
    val series = EventSeries.of(recording, source, Vector(fixation), Vector(range))
    assertEquals(series.map(_.support), Right(Vector(range)))
    assertEquals(series.map(_.recording), Right(recording))
    assertEquals(
      series.map(_.events.head.asInstanceOf[Event.Fixation[Deg]].centre),
      Right(Pt[Deg](1.5, 0.0))
    )
    assertEquals(
      series.map(_.events.head.asInstanceOf[Event.Fixation[Deg]].sampleCount),
      Right(2)
    )
    assertEquals(
      series.map(_.events.head.asInstanceOf[Event.Fixation[Deg]].dispersionStatus),
      Right(
        DispersionStatus.Available(
          Dispersion.of[Deg](0.5, DispersionMethod.RmsRadius).toOption.get,
          SummaryEvidence.SourceSupported(source, range)
        )
      )
    )
    assertEquals(series.toOption.flatMap(_.supported(-1)), None)
    assertEquals(series.toOption.flatMap(_.supported(1)), None)
  }

  test("EventSeries rejects count, clock, and overlap defects with source operands") {
    assertEquals(
      EventSeries.of(recording, source, Vector(fixation), Vector.empty),
      Left(DetectionSupportError.EventSupportCountMismatch(source, 1, 0))
    )

    val otherClock = ClockId("other")
    val otherSpan  =
      Interval.of(otherClock, Instant.millis(1), Instant.millis(3)).toOption.get
    val other = Event.Blink.of[Deg](otherSpan).toOption.get
    assertEquals(
      EventSeries.of(
        recording,
        source,
        Vector(other),
        Vector(SampleRange.of(1, 3).toOption.get)
      ),
      Left(DetectionSupportError.EventClockMismatch(source, 0, clock, otherClock))
    )

    val later = Event.Blink
      .of[Deg](Interval.of(clock, Instant.millis(3), Instant.millis(5)).toOption.get)
      .toOption
      .get
    val left  = SampleRange.of(1, 4).toOption.get
    val right = SampleRange.of(3, 5).toOption.get
    assertEquals(
      EventSeries.of(recording, source, Vector(fixation, later), Vector(left, right)),
      Left(DetectionSupportError.OverlappingSampleRanges(source, 1, left, right))
    )
  }

  test("EventSeries rejects support beyond the source recording") {
    val outside = SampleRange.of(4, 7).toOption.get
    assertEquals(
      EventSeries.of(recording, source, Vector(fixation), Vector(outside)),
      Left(DetectionSupportError.SampleRangeOutsideRecording(source, 0, outside, 6))
    )
  }

  test("EventSeries rejects a declared range that does not match the event timestamps") {
    val declared = SampleRange.of(2, 4).toOption.get
    val derived  = SampleRange.of(1, 3).toOption.get
    assertEquals(
      EventSeries.of(recording, source, Vector(fixation), Vector(declared)),
      Left(
        DetectionSupportError.EventSampleRangeMismatch(
          source,
          0,
          fixation.span,
          declared,
          derived
        )
      )
    )
  }

  test("a unit-changing warp recomputes dispersion from source samples") {
    val pixels  = Frame.screen("source-pixels", 1000, 1000).toOption.get
    val degrees = Frame.angular("target-degrees", 100.0, 100.0).toOption.get
    val samples = IArray(
      Sample(Instant.millis(0), Gaze.Tracked(Pt[Px](100.0, 500.0), None)),
      Sample(Instant.millis(1), Gaze.Tracked(Pt[Px](300.0, 500.0), None))
    )
    val sourceRecording = Recording
      .of(
        pixels,
        clock,
        Rate.Fixed(Hz(1000.0).toOption.get),
        Eye.Left,
        None,
        samples
      )
      .toOption
      .get
    val eventSpan = Interval.of(clock, Instant.millis(0), Instant.millis(2)).toOption.get
    val declared  = Event.Fixation
      .of(
        eventSpan,
        Pt[Px](200.0, 500.0),
        dispersion = 999.0,
        method = DispersionMethod.RmsRadius,
        sampleCount = 2
      )
      .toOption
      .get
    val range  = SampleRange.of(0, 2).toOption.get
    val series =
      EventSeries.of(sourceRecording, source, Vector(declared), Vector(range)).toOption.get
    val supported = series.events.head.asInstanceOf[Event.Fixation[Px]]
    assertEqualsDouble(supported.centre.x, 200.0, 1e-12)
    assertEqualsDouble(supported.dispersion.get.value, 100.0, 1e-12)
    assertEquals(supported.sampleCount, 2)
    val transform = Warp.rescale(pixels, degrees).toOption.get
    val moved     = series.warp(transform).toOption.get
    val fixation  = moved.events.head.asInstanceOf[Event.Fixation[Deg]]

    assertEqualsDouble(fixation.centre.x, -30.0, 1e-12)
    assertEqualsDouble(fixation.dispersion.get.value, 10.0, 1e-12)
    assertNotEquals(fixation.dispersion.get.value, declared.dispersion.get.value)
    assertEquals(
      fixation.dispersionStatus,
      DispersionStatus.Available(
        fixation.dispersion.get,
        SummaryEvidence.Recomputed(
          source,
          range,
          pixels.id,
          degrees.id,
          SpatialTransform.of(transform)
        )
      )
    )
    assertEquals(moved.support, series.support)
    assertEquals(
      moved.recording.samples.map(_.origin).toVector,
      Vector.fill(2)(SampleOrigin.Projected)
    )
    assertEquals(
      moved.recording.samples.map(_.lineage.toVector).toVector,
      Vector.fill(2)(Vector(SampleOrigin.Measured, SampleOrigin.Projected))
    )
    assertEquals(moved.lineage.frame, pixels.id)
    assertEquals(moved.lineage.contentHash, sourceRecording.contentHash)
    assertEquals(
      moved.lineage.samples.map(_.toVector),
      Vector.fill(2)(Vector(SampleOrigin.Measured, SampleOrigin.Projected))
    )

    val roundTrip = moved.warp(transform.inverse.get).toOption.get
    val restored  = roundTrip.events.head.asInstanceOf[Event.Fixation[Px]]
    assertEqualsDouble(restored.dispersion.get.value, 100.0, 1e-9)
    assertEquals(restored.sampleCount, 2)
  }

  test("a warped measured peak velocity is explicitly marked for re-estimation") {
    val pixels  = Frame.screen("saccade-pixels", 1000, 1000).toOption.get
    val degrees = Frame.angular("saccade-degrees", 100.0, 100.0).toOption.get
    val input   = Recording
      .of(
        pixels,
        clock,
        Rate.Fixed(Hz(1000.0).toOption.get),
        Eye.Left,
        None,
        IArray(
          Sample(Instant.millis(0), Gaze.Tracked(Pt[Px](100.0, 500.0), None)),
          Sample(Instant.millis(1), Gaze.Tracked(Pt[Px](300.0, 500.0), None))
        )
      )
      .toOption
      .get
    val eventSpan = Interval.of(clock, Instant.millis(0), Instant.millis(2)).toOption.get
    val velocity  = Velocity.perSecond[Px](1000.0).toOption.get
    val saccade   = Event.Saccade
      .of(eventSpan, Pt[Px](100.0, 500.0), Pt[Px](300.0, 500.0), Some(velocity))
      .toOption
      .get
    val range     = SampleRange.of(0, 2).toOption.get
    val transform = Warp.rescale(pixels, degrees).toOption.get
    val moved     = EventSeries
      .of(input, source, Vector(saccade), Vector(range))
      .toOption
      .get
      .warp(transform)
      .toOption
      .get
      .events
      .head
      .asInstanceOf[Event.Saccade[Deg]]

    assertEquals(moved.peakVelocity, None)
    assertEquals(
      moved.peakVelocityStatus,
      PeakVelocityStatus.Unavailable(
        PeakVelocityUnavailable.RequiresReestimation(
          source,
          range,
          pixels.id,
          degrees.id
        )
      )
    )
  }

end DetectionSupportSuite

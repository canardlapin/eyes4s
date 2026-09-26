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

package eyes4s.detect

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class DetectionSupportSearchSuite extends munit.ScalaCheckSuite:
  private val source   = RecordingRef("support-search")
  private val detector = DetectorRef("support-search", "1")
  private val frame    = Frame.angular("support-search", 20.0, 20.0).toOption.get
  private val clock    = ClockId("support-search")

  private def recording(times: Vector[Long]): Recording[Deg] =
    Recording
      .of(
        frame,
        clock,
        Rate.Irregular,
        Eye.Left,
        None,
        IArray.from(
          times.map(t => Sample(Instant.micros(t), Gaze.Tracked(Pt[Deg](0.0, 0.0), None)))
        )
      )
      .toOption
      .get

  private def event(from: Long, until: Long, domain: ClockId = clock): Event[Deg] =
    Event.Blink
      .of[Deg](Interval.of(domain, Instant.micros(from), Instant.micros(until)).toOption.get)
      .toOption
      .get

  // The previous exhaustive scan is deliberately retained only as a test oracle.
  private def scan(
      input: Recording[Deg],
      events: Vector[Event[Deg]]
  ): Either[DetectionResultError, Vector[SampleRange]] =
    events.zipWithIndex.foldLeft[Either[DetectionResultError, Vector[SampleRange]]](
      Right(Vector.empty)
    ) { case (acc, (e, i)) =>
      acc.flatMap { ranges =>
        val found = input.samples.indices.filter(j => e.span.contains(input.samples(j).t))
        if e.span.onset.toMicros < input.extent.onset.toMicros ||
          e.span.offset.toMicros > input.extent.offset.toMicros || found.isEmpty
        then
          Left(
            DetectionResultError
              .EventOutsideRecording(source, detector, i, e.span, input.extent)
          )
        else Right(ranges :+ SampleRange.of(found.head, found.last + 1).toOption.get)
      }
    }

  private val generated = for
    size   <- Gen.choose(2, 120)
    gaps   <- Gen.listOfN(size, Gen.choose(1L, 5000L))
    origin <- Gen.choose(-1000000L, 1000000L)
    times      = gaps.scanLeft(origin)(_ + _).toVector
    boundaries = times.flatMap(t => Vector(t - 1L, t, t + 1L))
    pairs <- Gen.listOf(Gen.zip(Gen.oneOf(boundaries), Gen.oneOf(boundaries)))
    events = pairs.map { case (a, b) => event(math.min(a, b), math.max(a, b) + 1L) }.toVector
  yield (recording(times), events)

  property("ordered timestamp searches retain the exhaustive oracle's ranges and first error") {
    forAll(generated) { case (input, events) =>
      assertEquals(Detection.supportFor(source, input, detector, events), scan(input, events))
    }
  }

  property("EventSeries independently validates generated non-overlapping source ranges") {
    forAll(generated) { case (input, _) =>
      val events = input.samples.toVector
        .sliding(2)
        .map { pair =>
          event(pair.head.t.toMicros, pair.last.t.toMicros)
        }
        .toVector
      val expected = scan(input, events).toOption.get
      val series   = EventSeries.of(input, source, events, expected).toOption.get
      assertEquals(series.support, expected)
      assertEquals(series.events, events)
    }
  }

  test("half-open boundaries, gaps, last sample and large signed timestamps") {
    Vector(-9007199254740995L, 0L, 9007199254740995L).foreach { base =>
      val input = recording(Vector(base, base + 10L, base + 30L))
      val spans = Vector(
        event(base, base + 10L),
        event(base + 10L, base + 30L),
        event(base + 30L, base + 31L)
      )
      assertEquals(
        Detection.supportFor(source, input, detector, spans),
        Right(Vector(0 -> 1, 1 -> 2, 2 -> 3).map { case (a, b) =>
          SampleRange.of(a, b).toOption.get
        })
      )
      val gap = event(base + 11L, base + 29L)
      assertEquals(
        Detection.supportFor(source, input, detector, Vector(gap)),
        scan(input, Vector(gap))
      )
      assertEquals(
        EventSeries.of(input, source, Vector(gap), Vector(SampleRange.of(1, 2).toOption.get)),
        Left(DetectionSupportError.EventSpanHasNoSamples(source, 0, gap.span))
      )
    }
  }

  test("clock and declared-support errors are retained by EventSeries") {
    val input   = recording(Vector(0L, 10L, 30L))
    val foreign = event(0L, 10L, ClockId("foreign"))
    assertEquals(
      EventSeries.of(input, source, Vector(foreign), Vector(SampleRange.of(0, 1).toOption.get)),
      Left(DetectionSupportError.EventClockMismatch(source, 0, clock, ClockId("foreign")))
    )
    val e     = event(0L, 10L)
    val wrong = SampleRange.of(0, 2).toOption.get
    assertEquals(
      EventSeries.of(input, source, Vector(e), Vector(wrong)),
      Left(
        DetectionSupportError.EventSampleRangeMismatch(
          source,
          0,
          e.span,
          wrong,
          SampleRange.of(0, 1).toOption.get
        )
      )
    )
  }

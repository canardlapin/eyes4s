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

class DetectionAssemblySuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), value => value)
  private val source                        = RecordingRef("assembly")
  private val detector                      = DetectorRef("assembly", "1")
  private val identity                      = DetectorIdentity.Custom(detector)
  private val frame                         = get(Frame.angular("assembly", 20.0, 20.0))
  private val clock                         = ClockId("assembly")
  private val input                         = get(
    Recording.of(
      frame,
      clock,
      Rate.Fixed(get(Hz(1000.0))),
      Eye.Left,
      None,
      IArray.tabulate(97) { i =>
        val gaze: Gaze[Deg] = i % 13 match
          case 1 => Gaze.Lost()
          case 2 => Gaze.Blink()
          case 3 => Gaze.OffScreen(Pt[Deg](30.0, 0.0))
          case _ => Gaze.Tracked(Pt[Deg](math.sin(i.toDouble), math.cos(i.toDouble)), None)
        Sample(Instant.millis(i.toLong), gaze)
      }
    )
  )
  private def span(from: Int, until: Int, domain: ClockId = clock) =
    get(Interval.of(domain, Instant.millis(from.toLong), Instant.millis(until.toLong)))
  private def fixation(from: Int, until: Int, method: DispersionMethod): Event[Deg] =
    get(Event.Fixation.of(span(from, until), Pt[Deg](0, 0), 0.0, method, 1))
  private def machine(
      events: Vector[Event[Deg]]
  ): Machine[Sample[Deg], DetectionEmission[Deg]] =
    Machine(new Detector[Unit, Sample[Deg], DetectionEmission[Deg]]:
      def init: Unit                                                                     = ()
      def step(state: Unit, sample: Sample[Deg]): (Unit, Vector[DetectionEmission[Deg]]) =
        ((), Vector.empty)
      def flush(state: Unit): Vector[DetectionEmission[Deg]] = events.map(Right(_)))
  private def view(result: Either[DetectionResultError, DetectionResult[Deg]]) = result.map {
    r =>
      (
        r.labels.toVector,
        r.eventSeries.events,
        r.eventSeries.support,
        r.eventSeries.lineage,
        r.report,
        r.provenance.render
      )
  }
  private def drain(events: Vector[Event[Deg]], policy: GapPolicy, feed: Int, assembly: Int) =
    var cursor = Detection.steppedCustom(source, input, detector, policy, machine(events))
    var result: Option[Either[DetectionResultError, DetectionResult[Deg]]] = None
    var phases = Set.empty[AssemblyPhase]
    var fed    = 0
    while result.isEmpty do
      assertEquals(cursor.total, input.size)
      val phase = cursor.assemblyPhase
      phases ++= phase
      val page = cursor.advance(feed, assembly)
      page match
        case DetectionPage.More(units, next) =>
          assert(units > 0 && units <= phase.fold(feed)(_ => assembly))
          if phase.isEmpty then fed += units
          // Replaying a retained cursor must have identical continuation and outcome.
          cursor.advance(feed, assembly) match
            case DetectionPage.More(again, replay) =>
              assertEquals(again, units)
              assertEquals(replay.assemblyPhase, next.assemblyPhase)
              assertEquals(
                view(DetectionCursor.complete(replay, 17)),
                view(DetectionCursor.complete(next, 19))
              )
            case _ => fail("replay changed page kind")
          cursor = next
        case DetectionPage.Done(units, value) =>
          assert(units > 0 && units <= phase.fold(feed)(_ => assembly))
          result = Some(value)
    assertEquals(fed, input.size)
    (result.get, phases)

  test(
    "assembly matches the pre-change exhaustive labels, gaps, reports and hash across independent quanta"
  ) {
    val events = Vector(
      fixation(4, 36, DispersionMethod.RmsRadius),
      fixation(45, 83, DispersionMethod.BoundingBoxDiagonal)
    )
    val policy   = GapPolicy.Bridge(get(InterpolationGap.of(Span.millis(10))))
    val expected = DetectionAssemblyOracle.assembleEmissions(
      source,
      input,
      identity,
      policy,
      input.representedSupport,
      events.map(Right(_)),
      Vector.empty
    )
    assertEquals(input.contentHash, DetectionAssemblyOracle.contentHash(input))
    for feed <- Vector(1, 17, 1000); assembly <- Vector(1, 11, 4096) do
      val (actual, phases) = drain(events, policy, feed, assembly)
      assertEquals(view(actual), view(expected))
      assertEquals(phases, AssemblyPhase.values.toSet)
  }

  test(
    "first structural failure and legacy last forbidden gap within the first failing event are unchanged"
  ) {
    val valid = fixation(0, 40, DispersionMethod.RmsRadius)
    val cases = Vector(
      Vector.empty,
      Vector(valid),
      Vector(valid, fixation(20, 60, DispersionMethod.RmsRadius)),
      Vector(valid, get(Event.Blink.of[Deg](span(45, 60, ClockId("foreign"))))),
      Vector(fixation(0, 100, DispersionMethod.RmsRadius)),
      Vector(fixation(1, 4, DispersionMethod.RmsRadius))
    )
    cases.foreach { events =>
      val expected = DetectionAssemblyOracle.assembleEmissions(
        source,
        input,
        identity,
        GapPolicy.Break,
        input.representedSupport,
        events.map(Right(_)),
        Vector.empty
      )
      for feed <- Vector(1, 31); assembly <- Vector(1, 13) do
        assertEquals(view(drain(events, GapPolicy.Break, feed, assembly)._1), view(expected))
    }
  }

  test("every fixation dispersion retains the original direct numerical formula") {
    def median(values: Vector[Double]): Double =
      val sorted = values.sorted
      val mid    = sorted.size / 2
      if sorted.size % 2 == 1 then sorted(mid) else (sorted(mid - 1) + sorted(mid)) / 2.0
    for until <- Vector(81, 82); method <- DispersionMethod.values do
      val points = (4 until until).flatMap { i =>
        val sample = input.samples(i)
        if sample.isUsable then sample.position else None
      }.toVector
      val centre = Pt[Deg](points.map(_.x).sum / points.size, points.map(_.y).sum / points.size)
      val expected = method match
        case DispersionMethod.RmsRadius =>
          math.sqrt(points.map(p => math.pow(centre.distanceTo(p), 2.0)).sum / points.size)
        case DispersionMethod.BoundingBoxWidth    => points.map(_.x).max - points.map(_.x).min
        case DispersionMethod.BoundingBoxDiagonal =>
          math.hypot(
            points.map(_.x).max - points.map(_.x).min,
            points.map(_.y).max - points.map(_.y).min
          )
        case DispersionMethod.MedianAbsoluteDeviation =>
          val mid = Pt[Deg](median(points.map(_.x)), median(points.map(_.y)))
          median(points.map(_.distanceTo(mid)))
      val event = fixation(4, until, method)
      val work  = EventSeries.assembly(
        input,
        source,
        Vector(event),
        Vector(get(SampleRange.of(4, until)))
      )
      var cursor = work
      while cursor.phase.nonEmpty do
        val (units, next) = cursor.advance(1)
        assertEquals(units, 1)
        cursor = next
      val actual = get(cursor.complete).events.head.asInstanceOf[Event.Fixation[Deg]]
      assertEquals(actual.centre, centre)
      assertEquals(actual.dispersion.get.value, expected)
      assertEquals(actual.sampleCount, points.size)
  }

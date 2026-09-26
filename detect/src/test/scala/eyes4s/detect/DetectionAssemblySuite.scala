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

class DetectionAssemblySuite extends munit.ScalaCheckSuite:
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
  private def view(result: Either[DetectionResultError, DetectionResult[Deg]]) =
    result.map(DetectionAssemblyOracle.viewOf)
  private def drain(
      events: Vector[Event[Deg]],
      policy: GapPolicy,
      feed: Int,
      assembly: Int,
      recording: Recording[Deg] = input
  ) =
    var cursor = Detection.steppedCustom(source, recording, detector, policy, machine(events))
    var result: Option[Either[DetectionResultError, DetectionResult[Deg]]] = None
    var phases = Set.empty[AssemblyPhase]
    var fed    = 0
    while result.isEmpty do
      assertEquals(cursor.total, recording.size)
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
    assertEquals(fed, recording.size)
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
      assertEquals(view(actual), expected)
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
        assertEquals(view(drain(events, GapPolicy.Break, feed, assembly)._1), expected)
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

  // Randomized comparison against the independent 131970d reference in
  // DetectionAssemblyOracle. Coordinates come mostly from a small set (including
  // -0.0 and 0.0) so medians meet ties and even counts; unusable samples make gaps
  // that Break rejects and Bridge admits up to its limit.
  private val coordinate = Gen.frequency(
    8 -> Gen.oneOf(-1.0, -0.0, 0.0, 0.5, 1.0, 2.5),
    2 -> Gen.choose(-5.0, 5.0)
  )
  private val gazeGen: Gen[Gaze[Deg]] = Gen.frequency(
    6 -> (for x <- coordinate; y <- coordinate yield Gaze.Tracked(Pt[Deg](x, y), None)),
    1 -> Gen.const[Gaze[Deg]](Gaze.Lost()),
    1 -> Gen.const[Gaze[Deg]](Gaze.Blink()),
    1 -> Gen.const[Gaze[Deg]](Gaze.OffScreen(Pt[Deg](30.0, 0.0)))
  )
  private def recordingOf(gazes: Seq[Gaze[Deg]]): Recording[Deg] = get(
    Recording.of(
      frame,
      clock,
      Rate.Fixed(get(Hz(1000.0))),
      Eye.Left,
      None,
      IArray.from(gazes.zipWithIndex.map((g, i) => Sample(Instant.millis(i.toLong), g)))
    )
  )
  private val recordingGen: Gen[Recording[Deg]] = for
    size  <- Gen.choose(1, 40)
    gazes <- Gen.listOfN(size, gazeGen)
  yield recordingOf(gazes)
  private def eventGen(from: Int, until: Int): Gen[Event[Deg]] = Gen.frequency(
    4 -> Gen.oneOf(DispersionMethod.values.toSeq).map(fixation(from, until, _)),
    1 -> Gen.const(get(Event.Fixation.withoutDispersion(span(from, until), Pt[Deg](0, 0), 1))),
    1 -> Gen.const(get(Event.Blink.of[Deg](span(from, until))))
  )

  /** Mostly disjoint in-recording spans; sometimes overlapping or outside spans. */
  private def spansGen(size: Int): Gen[Vector[(Int, Int)]] = Gen.frequency(
    5 -> Gen
      .choose(0, 4)
      .flatMap(k =>
        Gen.listOfN(2 * k, Gen.choose(0, size)).map { cuts =>
          cuts.distinct.sorted.grouped(2).collect { case Seq(a, b) => (a, b) }.toVector
        }
      ),
    1 -> Gen
      .choose(0, 3)
      .flatMap(k =>
        Gen
          .listOfN(
            k,
            for a <- Gen.choose(0, size + 1); length <- Gen.choose(1, 5) yield (a, a + length)
          )
          .map(_.sortBy(_._1).toVector)
      )
  )
  private def eventsGen(size: Int): Gen[Vector[Event[Deg]]] = spansGen(size).flatMap(
    _.foldLeft(Gen.const(Vector.empty[Event[Deg]])) { case (acc, (from, until)) =>
      acc.flatMap(built => eventGen(from, until).map(built :+ _))
    }
  )
  private val policyGen: Gen[GapPolicy] = Gen.frequency(
    1 -> Gen.const(GapPolicy.Break),
    2 -> Gen
      .oneOf(1L, 2L, 5L)
      .map(m => GapPolicy.Bridge(get(InterpolationGap.of(Span.millis(m)))))
  )

  /** The linear-scan support of each event, sometimes shifted or truncated. */
  private def supportGen(recording: Recording[Deg], events: Vector[Event[Deg]]) =
    events
      .foldLeft(Gen.const(Vector.empty[SampleRange])) { (acc, event) =>
        val covered =
          (0 until recording.size).filter(i => event.span.contains(recording.samples(i).t))
        val from  = covered.headOption.getOrElse(0)
        val until = covered.lastOption.fold(1)(_ + 1)
        acc.flatMap(built =>
          Gen
            .frequency(8 -> Gen.const(0), 1 -> Gen.const(-1), 1 -> Gen.const(1))
            .map(shift =>
              built :+ SampleRange
                .of(from, until + shift)
                .getOrElse(get(SampleRange.of(from, until)))
            )
        )
      }
      .flatMap(ranges => Gen.frequency(19 -> Gen.const(ranges), 1 -> Gen.const(ranges.drop(1))))

  property(
    "randomized assembly equals the 131970d reference in labels, events, gaps and reports"
  ) {
    val cases = for
      recording <- recordingGen
      events    <- eventsGen(recording.size)
      policy    <- policyGen
      feed      <- Gen.oneOf(1, 3, 1000)
      assembly  <- Gen.oneOf(1, 3, 4096)
    yield (recording, events, policy, feed, assembly)
    forAll(cases) { case (recording, events, policy, feed, assembly) =>
      val expected = DetectionAssemblyOracle.assembleEmissions(
        source,
        recording,
        identity,
        policy,
        recording.representedSupport,
        events.map(Right(_)),
        Vector.empty
      )
      assertEquals(view(drain(events, policy, feed, assembly, recording)._1), expected)
    }
  }

  property("EventSeries.of equals the linear-scan reference for arbitrary declared support") {
    val cases = for
      recording <- recordingGen
      events    <- eventsGen(recording.size)
      support   <- supportGen(recording, events)
    yield (recording, events, support)
    forAll(cases) { case (recording, events, support) =>
      assertEquals(
        EventSeries
          .of(recording, source, events, support)
          .map(series => (series.events.map(DetectionAssemblyOracle.viewOf), series.lineage)),
        DetectionAssemblyOracle.referenceSeries(recording, source, events, support)
      )
    }
  }

  test("median absolute deviation meets tied and even-count medians like the reference") {
    val tied = Vector((0.0, 0.0), (0.0, 0.0), (1.0, 1.0), (1.0, 1.0), (-0.0, 2.5), (0.5, 0.0))
    val recording = recordingOf(tied.map((x, y) => Gaze.Tracked(Pt[Deg](x, y), None)))
    for until <- Vector(4, 5, 6) do
      val event = fixation(0, until, DispersionMethod.MedianAbsoluteDeviation)
      val range = get(SampleRange.of(0, until))
      assertEquals(
        EventSeries
          .of(recording, source, Vector(event), Vector(range))
          .map(series => (series.events.map(DetectionAssemblyOracle.viewOf), series.lineage)),
        DetectionAssemblyOracle.referenceSeries(recording, source, Vector(event), Vector(range))
      )
  }

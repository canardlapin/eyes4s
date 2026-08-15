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

class DetectionResultSuite extends munit.FunSuite:

  private val source   = RecordingRef("participant-01/trial-02")
  private val detector = DetectorRef("fixture-detector", "1.0.0")
  private val frame    = Frame.angular("detection-result", 20.0, 20.0).toOption.get
  private val clock    = ClockId("detection-result")
  private val rate     = Rate.Fixed(Hz(1000.0).toOption.get)

  private def tracked(index: Int, origin: SampleOrigin = SampleOrigin.Measured) =
    Sample(
      Instant.millis(index.toLong),
      Gaze.Tracked(Pt[Deg](index.toDouble / 10.0, 0.0), None),
      origin
    )

  private def at(index: Int, x: Double): Sample[Deg] =
    Sample(Instant.millis(index.toLong), Gaze.Tracked(Pt[Deg](x, 0.0), None))

  private def recording(samples: Vector[Sample[Deg]]): Recording[Deg] =
    Recording.of(frame, clock, rate, Eye.Left, None, IArray.from(samples)).toOption.get

  private def span(from: Int, until: Int): Interval =
    Interval.of(clock, Instant.millis(from.toLong), Instant.millis(until.toLong)).toOption.get

  private def fixation(from: Int, until: Int): Event.Fixation[Deg] =
    Event.Fixation
      .of(
        span(from, until),
        Pt[Deg](0.0, 0.0),
        0.1,
        DispersionMethod.RmsRadius,
        math.max(1, until - from)
      )
      .toOption
      .get

  private def machine(
      emissions: Vector[DetectionEmission[Deg]]
  ): Machine[Sample[Deg], DetectionEmission[Deg]] =
    Machine(
      new Detector[Unit, Sample[Deg], DetectionEmission[Deg]]:
        def init: Unit = ()
        def step(
            state: Unit,
            sample: Sample[Deg]
        ): (Unit, Vector[DetectionEmission[Deg]])              = ((), Vector.empty)
        def flush(state: Unit): Vector[DetectionEmission[Deg]] = emissions
    )

  test("a result retains source ranges, exhaustive labels, reports, and provenance") {
    val input  = recording((0 until 10).map(tracked(_)).toVector)
    val result = Detection
      .runCustom(
        source,
        input,
        detector,
        GapPolicy.Break,
        machine(Vector(Right(fixation(2, 6))))
      )
      .toOption
      .get

    assertEquals(result.eventSeries.support, Vector(SampleRange.of(2, 6).toOption.get))
    assertEquals(result.eventSeries.recording, input)
    val supportedFixation =
      result.eventSeries.events.head.asInstanceOf[Event.Fixation[Deg]]
    assertEqualsDouble(supportedFixation.centre.x, 0.35, 1e-12)
    assertEquals(supportedFixation.sampleCount, 4)
    assertEqualsDouble(supportedFixation.dispersion.get.value, math.sqrt(0.0125), 1e-12)
    assertEquals(
      supportedFixation.dispersionStatus,
      DispersionStatus.Available(
        supportedFixation.dispersion.get,
        SummaryEvidence.SourceSupported(source, SampleRange.of(2, 6).toOption.get)
      )
    )
    assertEquals(
      result.labels.toVector,
      Vector.fill(2)(SampleClass.Unclassified) ++
        Vector.fill(4)(SampleClass.Fixation) ++
        Vector.fill(4)(SampleClass.Unclassified)
    )
    assertEquals(
      result.report.unclassifiedRanges,
      Vector(SampleRange.of(0, 2).toOption.get, SampleRange.of(6, 10).toOption.get)
    )
    assertEquals(result.report.unclassifiedSamples, 6)
    assertEquals(result.identity, DetectorIdentity.Custom(detector))
    assertEquals(result.report.temporalSupport, input.representedSupport.policy)
    assertEquals(result.report.policyCensoredTime, input.representedSupport.censoredTime)
    assertEquals(result.labels.get(-1), None)
    assertEquals(result.labels.get(result.labels.size), None)
    assertEquals(result.report.warnings.length, 2)
    assert(result.report.warnings.forall(_.message.contains(source.value)))
    assert(result.report.warnings.forall(_.message.contains(detector.render)))
    assertEquals(
      result.report.classDurations.map(_._2.toMicros).sum,
      input.duration.toMicros
    )
    assertEquals(result.provenance.inputs, input.contentHash)
    assert(result.provenance.render.contains(detector.name))
  }

  test("native invalid states remain labelled even without detected events") {
    val input = recording(
      Vector(
        tracked(0),
        Sample(Instant.millis(1), Gaze.Blink[Deg]()),
        Sample(Instant.millis(2), Gaze.Lost[Deg]()),
        Sample(Instant.millis(3), Gaze.OffScreen(Pt[Deg](20.0, 0.0))),
        tracked(4)
      )
    )
    val result = Detection
      .runCustom(source, input, detector, GapPolicy.Break, machine(Vector.empty))
      .toOption
      .get
    assertEquals(
      result.labels.toVector,
      Vector(
        SampleClass.Unclassified,
        SampleClass.Blink,
        SampleClass.Missing,
        SampleClass.OffSurface,
        SampleClass.Unclassified
      )
    )
  }

  test("duration accounting uses the explicitly selected temporal support") {
    val input = Recording
      .of(
        frame,
        clock,
        Rate.Irregular,
        Eye.Left,
        None,
        IArray(tracked(0), tracked(1), tracked(4))
      )
      .toOption
      .get
    val policy = TemporalSupport.ForwardHold(
      MaximumSupportGap.Unlimited,
      EdgeSupport.Censored
    )
    val result = Detection
      .runCustom(
        source,
        input,
        detector,
        GapPolicy.Break,
        policy,
        machine(Vector.empty),
        Vector.empty
      )
      .toOption
      .get

    assertEquals(result.report.temporalSupport, policy)
    assertEquals(result.report.policyCensoredTime, Span.zero)
    assertEquals(
      result.report.classDurations.map(_._2.toMicros).sum,
      input.representedSupport(policy).assignedTime.toMicros
    )
    assertNotEquals(
      result.report.classDurations.map(_._2.toMicros).sum,
      input.representedSupport.assignedTime.toMicros
    )
  }

  test("every event variant has a distinct sample label") {
    val input   = recording((0 until 8).map(tracked(_)).toVector)
    val saccade = Event.Saccade
      .of(span(2, 4), Pt[Deg](0.0, 0.0), Pt[Deg](1.0, 0.0), None)
      .toOption
      .get
    val pursuit = Event.Pursuit
      .of(span(4, 6), IArray(Pt[Deg](1.0, 0.0), Pt[Deg](2.0, 0.0)))
      .toOption
      .get
    val blink  = Event.Blink.of[Deg](span(6, 8)).toOption.get
    val result = Detection
      .runCustom(
        source,
        input,
        detector,
        GapPolicy.Break,
        machine(Vector(Right(fixation(0, 2)), Right(saccade), Right(pursuit), Right(blink)))
      )
      .toOption
      .get
    assertEquals(
      result.labels.toVector,
      Vector(
        SampleClass.Fixation,
        SampleClass.Fixation,
        SampleClass.Saccade,
        SampleClass.Saccade,
        SampleClass.Pursuit,
        SampleClass.Pursuit,
        SampleClass.Blink,
        SampleClass.Blink
      )
    )
  }

  test("Break rejects an event spanning invalid source support") {
    val input = recording(
      Vector(
        tracked(0),
        tracked(1),
        Sample(Instant.millis(2), Gaze.Lost[Deg]()),
        tracked(3),
        tracked(4)
      )
    )
    val result = Detection.runCustom(
      source,
      input,
      detector,
      GapPolicy.Break,
      machine(Vector(Right(fixation(0, 5))))
    )
    assertEquals(
      result,
      Left(
        DetectionResultError.GapPolicyViolation(
          source,
          detector,
          0,
          SampleRange.of(2, 3).toOption.get,
          Span.millis(1),
          GapPolicy.Break
        )
      )
    )
  }

  test("Bridge accepts only invalid runs within its validated duration") {
    val input = recording(
      Vector(
        tracked(0),
        tracked(1),
        Sample(Instant.millis(2), Gaze.Lost[Deg]()),
        tracked(3),
        tracked(4)
      )
    )
    val eventMachine = machine(Vector(Right(fixation(0, 5))))
    val oneMs        = GapPolicy.Bridge(InterpolationGap.of(Span.millis(1)).toOption.get)
    val none         = GapPolicy.Bridge(InterpolationGap.none)

    val accepted =
      Detection.runCustom(source, input, detector, oneMs, eventMachine).toOption.get
    assertEquals(accepted.report.bridgedGaps, Vector(SampleRange.of(2, 3).toOption.get))
    assert(
      Detection.runCustom(source, input, detector, none, eventMachine) match
        case Left(DetectionResultError.GapPolicyViolation(_, _, 0, _, duration, `none`)) =>
          duration == Span.millis(1)
        case _ => false
    )
  }

  test("UseInterpolatedOnly accepts explicitly interpolated usable support") {
    val input = recording(
      Vector(
        tracked(0),
        tracked(1),
        tracked(2, SampleOrigin.Interpolated),
        tracked(3),
        tracked(4)
      )
    )
    val result = Detection
      .runCustom(
        source,
        input,
        detector,
        GapPolicy.UseInterpolatedOnly,
        machine(Vector(Right(fixation(0, 5))))
      )
      .toOption
      .get
    assertEquals(result.labels.toVector, Vector.fill(5)(SampleClass.Fixation))
    assertEquals(result.report.bridgedGaps, Vector.empty)
  }

  test("emission, outside-support, and overlap failures name source and detector") {
    val input      = recording((0 until 10).map(tracked(_)).toVector)
    val eventError = DetectionFailure.EventSummary(
      CoreError.OfEvent(EventError.NonPositiveSampleCount(0))
    )
    assertEquals(
      Detection
        .runCustom(source, input, detector, GapPolicy.Break, machine(Vector(Left(eventError)))),
      Left(DetectionResultError.DetectorEmissionFailed(source, detector, 0, eventError))
    )

    val outside = Event.Blink
      .of[Deg](
        Interval.of(clock, Instant.millis(20), Instant.millis(21)).toOption.get
      )
      .toOption
      .get
    assert(
      Detection.runCustom(
        source,
        input,
        detector,
        GapPolicy.Break,
        machine(Vector(Right(outside)))
      ) match
        case Left(DetectionResultError.EventOutsideRecording(`source`, `detector`, 0, _, _)) =>
          true
        case _ => false
    )

    val partial = Event.Blink
      .of[Deg](
        Interval.of(clock, Instant.millis(9), Instant.millis(11)).toOption.get
      )
      .toOption
      .get
    assert(
      Detection.runCustom(
        source,
        input,
        detector,
        GapPolicy.Break,
        machine(Vector(Right(partial)))
      ) match
        case Left(DetectionResultError.EventOutsideRecording(`source`, `detector`, 0, _, _)) =>
          true
        case _ => false
    )

    val overlapping = Vector[DetectionEmission[Deg]](
      Right(fixation(1, 5)),
      Right(fixation(4, 8))
    )
    assert(
      Detection.runCustom(source, input, detector, GapPolicy.Break, machine(overlapping)) match
        case Left(DetectionResultError.SourceSupport(`source`, `detector`, _)) => true
        case _                                                                 => false
    )
  }

  test("a real detector keeps a discarded short run visible as unclassified support") {
    val first     = (0 until 60).map(index => at(index, 0.0)).toVector
    val twitch    = (0 until 5).map(index => at(60 + index, index.toDouble)).toVector
    val last      = (0 until 60).map(index => at(65 + index, 5.0)).toVector
    val input     = recording(first ++ twitch ++ last)
    val threshold = IvtThreshold
      .of(Velocity.degPerSecond(30.0).toOption.get)
      .toOption
      .get
    val minimum = MinimumEventDuration.of(Span.millis(20)).toOption.get
    val result  = Detection
      .run(
        source,
        input,
        Detectors.ivt(threshold, minimum, clock),
        GapPolicy.Break
      )
      .toOption
      .get

    assert(result.eventSeries.events.collect { case _: Event.Saccade[Deg] => () }.isEmpty)
    assert(result.report.unclassifiedSamples > 0)
    assertEquals(result.labels.size, input.size)
    assertEquals(
      result.report.classDurations.map(_._2.toMicros).sum,
      input.duration.toMicros
    )
  }

  test(
    "I-VT invalid support is excluded while labels and duration still partition the source"
  ) {
    val first     = (0 until 40).map(index => at(index, 0.0)).toVector
    val gap       = Sample(Instant.millis(40), Gaze.Lost[Deg]())
    val last      = (41 until 81).map(index => at(index, 0.0)).toVector
    val input     = recording(first ++ Vector(gap) ++ last)
    val threshold = IvtThreshold
      .of(Velocity.degPerSecond(30.0).toOption.get)
      .toOption
      .get
    val minimum = MinimumEventDuration.of(Span.millis(20)).toOption.get
    val result  = Detection
      .run(
        source,
        input,
        Detectors.ivt(threshold, minimum, clock),
        GapPolicy.Break
      )
      .toOption
      .get

    assertEquals(
      result.identity,
      DetectorIdentity.Algorithm(Detectors.ivt(threshold, minimum, clock).card)
    )

    assertEquals(
      result.eventSeries.support,
      Vector(SampleRange.of(0, 40).toOption.get, SampleRange.of(41, 81).toOption.get)
    )
    assertEquals(
      result.labels.toVector,
      Vector.fill(40)(SampleClass.Fixation) ++
        Vector(SampleClass.Missing) ++
        Vector.fill(40)(SampleClass.Fixation)
    )
    assertEquals(result.report.unclassifiedSamples, 0)
    assertEquals(
      result.report.classDurations.map(_._2.toMicros).sum,
      input.duration.toMicros
    )
  }

end DetectionResultSuite

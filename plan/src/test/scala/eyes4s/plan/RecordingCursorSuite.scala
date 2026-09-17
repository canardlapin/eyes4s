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

package eyes4s.plan

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}

/** The recording cursor against `RecordingPlan.run`: one scientific path,
  * whatever the sample chunking.
  */
class RecordingCursorSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  private val display       = get(Frame.screen("cursor-display", 1000, 1000))
  private val trackerClock  = ClockId("cursor-tracker")
  private val analysisClock = ClockId("cursor-analysis")
  private val source        = RecordingRef("cursor-recording")
  private val viewing       = get(Viewing.millimetres(600.0, 500.0, 500.0))

  /** 40 samples at 100 Hz: a fixation at (400, 500) over samples 0-14, a
    * two-sample saccade, a fixation at (600, 500) from sample 17 to the end,
    * with samples 25-26 lost so that interpolation has a gap to bridge. The
    * first fixation spans every chunk cut below, and the last one is only
    * emitted by the flush.
    */
  private def gaze(i: Int): Gaze[Px] =
    if i == 25 || i == 26 then Gaze.Lost()
    else if i < 15 then Gaze.Tracked(Pt[Px](400.0, 500.0), None)
    else if i == 15 then Gaze.Tracked(Pt[Px](480.0, 500.0), None)
    else if i == 16 then Gaze.Tracked(Pt[Px](560.0, 500.0), None)
    else Gaze.Tracked(Pt[Px](600.0, 500.0), None)

  private val recording = get(
    Recording.of(
      display,
      trackerClock,
      Rate.Fixed(get(Hz(100.0))),
      Eye.Left,
      None,
      IArray.from((0 until 40).map(i => Sample(Instant.millis(i.toLong * 10L), gaze(i))))
    )
  )
  private val marks = Vector(
    get(SyncMark.of("start", Instant.millis(0), Instant.millis(0))),
    get(SyncMark.of("end", Instant.millis(390), Instant.millis(390)))
  )
  private val area = get(
    RecordingArea.of("centre", "Central area", get(Bounds.of[Px](300.0, 300.0, 700.0, 700.0)))
  )
  private val ivtId     = get(DefinitionId.of("eyes4s.ivt", 1))
  private val ivt       = RecordingMethod.ivt(ivtId)
  private val ivtParams = IvtParameters(
    get(IvtThreshold.of(get(Velocity.perSecond[Deg](30)))),
    get(MinimumEventDuration.of(Span.micros(20000)))
  )

  private def plan[P](
      viewing: Option[Viewing] = Some(viewing),
      syncMarks: Vector[SyncMark] = marks,
      residualLimit: Option[SyncResidualLimit] = None,
      method: RecordingMethod[P] = ivt,
      parameters: P = ivtParams
  ) = get(
    RecordingPlan.of(
      ArtifactRef.of(recording.contentHash),
      source,
      display,
      trackerClock,
      analysisClock,
      FrameId("cursor-angular"),
      viewing,
      SyncFitMode.OffsetOnly,
      syncMarks,
      residualLimit,
      get(InterpolationGap.of(Span.micros(30000))),
      Vector(area),
      method,
      parameters
    )
  )

  private def quanta(samples: Int) =
    WorkQuanta(PairQuantum.default, ComparisonQuantum.default, get(SampleQuantum.of(samples)))

  private def steps[P](
      cursor: RecordingCursor[P],
      quanta: WorkQuanta
  ): (Vector[(RecordingStage, Int)], RecordingAnalysis[P]) =
    val trace = Vector.newBuilder[(RecordingStage, Int)]
    @annotation.tailrec
    def loop(cursor: RecordingCursor[P]): RecordingAnalysis[P] =
      val stage = cursor.stage
      get(cursor.advance(quanta)) match
        case WorkStep.More(_, units, next) =>
          trace += stage -> units
          loop(next)
        case WorkStep.Done(units, result) =>
          trace += stage -> units
          result
    val result = loop(cursor)
    trace.result() -> result

  private def assertSameAnalysis[P](
      observed: RecordingAnalysis[P],
      expected: RecordingAnalysis[P],
      clue: Any
  ): Unit =
    assertEquals(observed.description, expected.description, clue)
    assertEquals(observed.angular.samples.toVector, expected.angular.samples.toVector, clue)
    assertEquals(observed.prepared.samples.toVector, expected.prepared.samples.toVector, clue)
    assertEquals(observed.prepared.contentHash, expected.prepared.contentHash, clue)
    assertEquals(observed.detection.identity, expected.detection.identity, clue)
    assertEquals(observed.detection.labels.toVector, expected.detection.labels.toVector, clue)
    assertEquals(
      observed.detection.eventSeries.events,
      expected.detection.eventSeries.events,
      clue
    )
    assertEquals(
      observed.detection.eventSeries.support,
      expected.detection.eventSeries.support,
      clue
    )
    assertEquals(observed.detection.report, expected.detection.report, clue)
    assertEquals(observed.detection.provenance, expected.detection.provenance, clue)
    assertEquals(observed.assignment.toVector, expected.assignment.toVector, clue)
    assertEquals(observed.assignment.report, expected.assignment.report, clue)
    assertEquals(observed.assignment.policy, expected.assignment.policy, clue)

  test(
    "the cursor at every sample quantum is plan.run, chunk-spanning events and flush included"
  ) {
    val p        = plan()
    val expected = get(p.run(recording))
    val events   = expected.detection.eventSeries.events
    assert(events.size >= 2, clue(events))
    // Flush at end: the last event closes at or after the recording's last sample.
    assert(
      events.last.span.offset.toMicros >= expected.prepared.last.t.toMicros,
      clue((events.last.span, expected.prepared.last.t))
    )
    Vector(1, 4, 7, 40, 4096).foreach { samples =>
      val q               = quanta(samples)
      val (trace, result) = steps(get(p.work(recording)), q)
      assertSameAnalysis(result, expected, samples)
      val stages = trace.map(_._1)
      assertEquals(stages.head, RecordingStage.Synchronizing)
      assertEquals(stages(1), RecordingStage.Warping)
      assertEquals(stages.last, RecordingStage.Assigning)
      val chunks        = math.ceil(40.0 / samples).toInt
      val interpolating = trace.collect { case (RecordingStage.Interpolating(at), u) =>
        at -> u
      }
      val detecting = trace.collect { case (RecordingStage.Detecting(at), u) => at -> u }
      assertEquals(interpolating.size, chunks, samples)
      assertEquals(detecting.size, chunks, samples)
      assertEquals(interpolating.map(_._2).sum, 40, samples)
      assertEquals(detecting.map(_._2).sum, 40, samples)
      assertEquals(interpolating.map(_._1), interpolating.scanLeft(0)(_ + _._2).init, samples)
      assertEquals(trace.map(_._2).take(2), Vector(1, 1))
      assertEquals(trace.last._2, 1)
      if samples < 15 then
        // The first fixation covers samples 0-14 and therefore spans a cut.
        val first = expected.detection.eventSeries.support.head
        assert(first.from / samples != (first.until - 1) / samples, clue((samples, first)))
    }
  }

  test(
    "the machine is stepped once per sample and flushed once; an abandoned cursor never flushes"
  ) {
    var steps    = 0
    var flushes  = 0
    val counting = new RecordingMethod[IvtParameters](
      ivtId,
      ivt.parameters,
      (p, c) =>
        val inner = Detectors.ivt(p.threshold, p.minimumDuration, c)
        EventDetector.of[Deg](
          AlgorithmCards.ivt,
          Machine(new Detector[inner.machine.S, Sample[Deg], DetectionEmission[Deg]]:
            def init: inner.machine.S                    = inner.machine.detector.init
            def step(s: inner.machine.S, i: Sample[Deg]) =
              steps += 1
              inner.machine.detector.step(s, i)
            def flush(s: inner.machine.S): Vector[DetectionEmission[Deg]] =
              flushes += 1
              inner.machine.detector.flush(s)),
          inner.configuration
        )
      ,
      Some(RecordingMethodDescriptor.ivt(ivtId))
    )
    val p           = plan(method = counting)
    val expected    = get(plan().run(recording))
    val (_, result) = this.steps(get(p.work(recording)), quanta(4))
    assertEquals((steps, flushes), (40, 1))
    assertSameAnalysis(result, expected, "counting detector")
    steps = 0
    flushes = 0
    // Advance into detection and stop: samples were fed, nothing was flushed.
    var cursor = get(p.work(recording))
    (1 to 15).foreach { _ =>
      get(cursor.advance(quanta(4))) match
        case WorkStep.More(_, _, next) => cursor = next
        case WorkStep.Done(_, _)       => fail("finished early")
    }
    assertEquals(cursor.stage, RecordingStage.Detecting(12))
    assertEquals((steps, flushes), (12, 0))
  }

  test("prerequisites refuse before any step; a synchronization failure is the first step's") {
    val missing = plan(viewing = None)
    assertEquals(
      missing.work(recording).left.map(_.getClass),
      Left(RecordingPlanError.MissingViewing(source).getClass)
    )
    assertEquals(missing.work(recording).swap.toOption, missing.run(recording).swap.toOption)
    val other = get(
      Recording.of(
        display,
        trackerClock,
        Rate.Fixed(get(Hz(100.0))),
        Eye.Left,
        None,
        IArray.from((0 until 3).map(i => Sample(Instant.millis(i.toLong * 10L), gaze(0))))
      )
    )
    plan().work(other) match
      case Left(RecordingPlanError.Input(PlanError.ArtifactMismatch(_, _))) => ()
      case other => fail(s"expected an artifact mismatch, got $other")
    val invalid = plan(
      syncMarks = Vector(
        marks(0),
        get(SyncMark.of("end", Instant.millis(390), Instant.millis(400)))
      ),
      residualLimit = Some(get(SyncResidualLimit.of(Span.micros(1000))))
    )
    val cursor = get(invalid.work(recording))
    assertEquals(cursor.stage, RecordingStage.Synchronizing)
    cursor.advance(quanta(4)) match
      case Left(error @ RecordingPlanError.Synchronization(_)) =>
        assertEquals(invalid.run(recording).swap.toOption, Some(error))
      case other => fail(s"expected a synchronization failure, got $other")
  }

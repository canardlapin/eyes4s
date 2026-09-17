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

package eyes4s.fs2

import cats.effect.{Deferred, IO, Ref}
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*

/** The shared runner over the recording cursor, in the shape of
  * [[StudyExecutionSuite]]: every cancellation is positioned with a barrier
  * under `TestControl` and checked against the pure step sequence.
  */
class RecordingExecutionSuite extends munit.CatsEffectSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  private val display       = get(Frame.screen("execution-display", 1000, 1000))
  private val trackerClock  = ClockId("execution-tracker")
  private val analysisClock = ClockId("execution-analysis")
  private val source        = RecordingRef("execution-recording")
  private val viewing       = get(Viewing.millimetres(600.0, 500.0, 500.0))

  /** 40 samples at 100 Hz: a fixation over samples 0-14, a two-sample
    * saccade, a fixation from sample 17 to the end, two lost samples inside it.
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
      FrameId("execution-angular"),
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

  private val work = plan()

  /** A detector wrapping the shipped IVT machine with instrumented steps. */
  private def instrumented(
      onStep: Int => Unit,
      onFlush: () => Unit
  ): RecordingMethod[IvtParameters] =
    new RecordingMethod[IvtParameters](
      ivtId,
      ivt.parameters,
      (p, c) =>
        val inner = Detectors.ivt(p.threshold, p.minimumDuration, c)
        EventDetector.of[Deg](
          AlgorithmCards.ivt,
          Machine(new Detector[(inner.machine.S, Int), Sample[Deg], DetectionEmission[Deg]]:
            def init: (inner.machine.S, Int) = (inner.machine.detector.init, 0)
            def step(s: (inner.machine.S, Int), i: Sample[Deg]) =
              onStep(s._2)
              val (next, out) = inner.machine.detector.step(s._1, i)
              ((next, s._2 + 1), out)
            def flush(s: (inner.machine.S, Int)): Vector[DetectionEmission[Deg]] =
              onFlush()
              inner.machine.detector.flush(s._1)),
          inner.configuration
        )
      ,
      Some(RecordingMethodDescriptor.ivt(ivtId))
    )

  private def quanta(samples: Int) =
    WorkQuanta(PairQuantum.default, ComparisonQuantum.default, get(SampleQuantum.of(samples)))
  private val four    = quanta(4)
  private val quantas = Vector(quanta(1), four, quanta(7), quanta(4096))

  private type Analysis = RecordingAnalysis[IvtParameters]
  private type Outcome  = RecordingOutcome[IvtParameters]
  private type Event    = RecordingEvent[IvtParameters]

  private val runner = RecordingExecution[IO]

  private final class Broken extends RuntimeException("broken detector")

  // -------------------------------------------------------------------------
  // Pure oracle
  // -------------------------------------------------------------------------

  private def pure(
      plan: RecordingPlan[IvtParameters],
      quanta: WorkQuanta
  ): (Vector[(RecordingStage, Int)], Analysis) =
    val steps = Vector.newBuilder[(RecordingStage, Int)]
    @annotation.tailrec
    def loop(cursor: RecordingCursor[IvtParameters]): Analysis =
      val stage = cursor.stage
      get(cursor.advance(quanta)) match
        case WorkStep.More(_, units, next) =>
          steps += stage -> units
          loop(next)
        case WorkStep.Done(units, result) =>
          steps += stage -> units
          result
    val result = loop(get(plan.work(recording)))
    steps.result() -> result

  private def progressOf(events: Vector[Event]): Vector[RecordingProgress] =
    events.collect { case RunEvent.Advanced(progress) => progress }

  private def finished(events: Vector[Event]): Outcome =
    events.collect { case RunEvent.Finished(outcome) => outcome } match
      case Vector(outcome) => outcome
      case other           => fail(s"expected exactly one terminal event, got $other")

  private def completed(outcome: Outcome): (RecordingProgress, Analysis) = outcome match
    case RunOutcome.Completed(_, last, result) => (last, result)
    case other                                 => fail(s"expected Completed, got $other")

  private def assertSameAnalysis(observed: Analysis, expected: Analysis, clue: Any): Unit =
    assertEquals(observed.description, expected.description, clue)
    assertEquals(observed.angular.samples.toVector, expected.angular.samples.toVector, clue)
    assertEquals(observed.prepared.samples.toVector, expected.prepared.samples.toVector, clue)
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

  private def cancelAfter(
      plan: RecordingPlan[IvtParameters],
      quanta: WorkQuanta,
      completedSteps: Int
  ): IO[(Outcome, Vector[RecordingProgress], Int)] =
    TestControl.executeEmbed {
      for
        started <- Ref[IO].of(0)
        reached <- Deferred[IO, Unit]
        parked  <- Deferred[IO, Unit]
        between = started.updateAndGet(_ + 1).flatMap { n =>
          if n == completedSteps + 1 then reached.complete(()) >> parked.get else IO.unit
        }
        result <- runner.start(plan, recording, quanta, between).use { run =>
          for
            _        <- reached.get
            _        <- run.cancel
            outcome  <- run.outcome
            observed <- run.progress.compile.toVector
            begun    <- started.get
          yield (outcome, observed, begun)
        }
      yield result
    }

  // -------------------------------------------------------------------------
  // Determinism and parity with the pure runner
  // -------------------------------------------------------------------------

  test(
    "events at every sample quantum replay the pure cursor's stages and plan.run's analysis"
  ) {
    val expected = get(work.run(recording))
    quantas.traverse_ { q =>
      val (steps, pureResult) = pure(work, q)
      assertSameAnalysis(pureResult, expected, q.samples.value)
      for
        first  <- runner.events(work, recording, q).compile.toVector
        second <- runner.events(work, recording, q).compile.toVector
      yield
        assertEquals(progressOf(first), progressOf(second), q.samples.value)
        val progress = progressOf(first)
        assertEquals(progress.map(p => (p.stage, p.stepUnits)), steps, q.samples.value)
        assertEquals(progress.map(_.step), (1L to steps.size.toLong).toVector)
        assertEquals(progress.map(_.totalUnits), steps.scanLeft(0L)(_ + _._2).tail)
        assert(progress.forall(_.run == RecordingRunId.of(work, q)))
        val (last, result) = completed(finished(first))
        assertEquals(first.last, RunEvent.Finished(finished(first)))
        assertEquals(last, progress.last)
        assertSameAnalysis(result, expected, q.samples.value)
    } >> IO {
      val (fine, _)   = pure(work, quanta(1))
      val (coarse, _) = pure(work, quanta(4096))
      assert(fine.size > coarse.size, clue((fine.size, coarse.size)))
      assertEquals(coarse.size, 5)
    }
  }

  test("a run id is a function of the plan revision, input and sample quantum") {
    val id = RecordingRunId.of(work, four)
    assertEquals(id, RecordingRunId.of(work, four))
    assertEquals(id.input, work.input.digest)
    assertEquals(id.method, ivtId)
    assertEquals(id.parameters, work.description)
    assertEquals(id.sampleQuantum, 4)
    assertNotEquals(id, RecordingRunId.of(work, quanta(7)))
    // Pair and comparison quanta cut no recording step.
    assertEquals(
      id,
      RecordingRunId.of(
        work,
        WorkQuanta(
          get(PairQuantum.of(1)),
          get(ComparisonQuantum.of(1)),
          get(SampleQuantum.of(4))
        )
      )
    )
    val revised = plan(parameters =
      ivtParams.copy(threshold = get(IvtThreshold.of(get(Velocity.perSecond[Deg](40)))))
    )
    assertNotEquals(id, RecordingRunId.of(revised, four))
    runner.start(work, recording, four).use(run => run.outcome.map(o => (run.id, o.run))).map {
      case (runId, outcomeId) => assertEquals((runId, outcomeId), (id, id))
    }
  }

  test("segments run in scientific order with exact totals that the run meets") {
    runner.events(work, recording, four).compile.toVector.map { events =>
      val progress = progressOf(events)
      assertEquals(
        progress.map(_.segment).distinct,
        Vector(
          RecordingSegment.Synchronizing,
          RecordingSegment.Warping,
          RecordingSegment.Interpolating,
          RecordingSegment.Detecting,
          RecordingSegment.Assigning
        )
      )
      progress.zip(progress.tail).foreach { case (before, after) =>
        assert(after.totalUnits >= before.totalUnits, clue((before, after)))
        assertEquals(after.totalUnits - before.totalUnits, after.stepUnits.toLong)
        if before.segment == after.segment then
          assertEquals(after.segmentUnits - before.segmentUnits, after.stepUnits.toLong)
        else assertEquals(after.segmentUnits, after.stepUnits.toLong)
      }
      val totals = progress.map(p => p.segment -> p.segmentTotal).distinct.toMap
      val last   = progress.groupBy(_.segment).view.mapValues(_.last.segmentUnits).toMap
      assertEquals(
        totals,
        Map(
          RecordingSegment.Synchronizing -> SegmentTotal.Exact(1L),
          RecordingSegment.Warping       -> SegmentTotal.Exact(1L),
          RecordingSegment.Interpolating -> SegmentTotal.Exact(40L),
          RecordingSegment.Detecting     -> SegmentTotal.Exact(40L),
          RecordingSegment.Assigning     -> SegmentTotal.Exact(1L)
        )
      )
      totals.foreach {
        case (segment, SegmentTotal.Exact(units)) => assertEquals(last(segment), units)
        case other                                => fail(s"not exact: $other")
      }
      assertEquals(
        progress.filter(_.segment == RecordingSegment.Detecting).map(_.stage),
        (0 until 40 by 4).map(RecordingStage.Detecting(_)).toVector
      )
      assertEquals(progress.last.totalUnits, 83L)
    }
  }

  // -------------------------------------------------------------------------
  // The run handle: commit, observers, disposal
  // -------------------------------------------------------------------------

  test("start commits the pure analysis once and cancelling afterwards changes nothing") {
    val expected = get(work.run(recording))
    quantas.traverse_ { q =>
      val (steps, _) = pure(work, q)
      for
        events <- runner.events(work, recording, q).compile.toVector
        result <- TestControl.executeEmbed {
          runner.start(work, recording, q).use { run =>
            for
              observed <- run.progress.compile.toVector
              first    <- run.outcome
              again    <- run.outcome
              _        <- run.cancel
              third    <- run.outcome
              late     <- run.progress.compile.toVector
            yield (observed, first, again, third, late)
          }
        }
      yield
        val (observed, first, again, third, late) = result
        val (last, committed)                     = completed(first)
        assertSameAnalysis(committed, expected, q.samples.value)
        assert(first eq again, "outcome settles once")
        assert(first eq third, "cancelling a completed run changes nothing")
        assertEquals(last, progressOf(events).last)
        assertEquals(last.step, steps.size.toLong)
        assertEquals(observed.last, last)
        assert(observed.zip(observed.tail).forall(_.step < _.step))
        assertEquals(late, Vector(last))
    }
  }

  // -------------------------------------------------------------------------
  // Cancellation lands between steps and never publishes a result
  // -------------------------------------------------------------------------

  test("cancelling before any work settles Cancelled with no progress") {
    val id = RecordingRunId.of(work, four)
    TestControl
      .executeEmbed(runner.start(work, recording, four).use(run => run.cancel >> run.outcome))
      .map(outcome => assertEquals(outcome, RunOutcome.Cancelled(id, None)))
      >> cancelAfter(work, four, 0).map { case (outcome, observed, begun) =>
        assertEquals(outcome, RunOutcome.Cancelled(id, None))
        assertEquals(observed, Vector.empty)
        assertEquals(begun, 1)
      }
  }

  test("cancelling inside preprocessing, detection and before commit lands between steps") {
    val (steps, _) = pure(work, four)
    val stages     = steps.map(_._1)
    val positions  = Vector(
      "preprocessing" -> 4,
      "detection"     -> 15,
      "before commit" -> (steps.size - 1)
    )
    runner.events(work, recording, four).compile.toVector.flatMap { events =>
      val progress = progressOf(events)
      positions.traverse_ { case (where, completedSteps) =>
        cancelAfter(work, four, completedSteps).map { case (outcome, observed, begun) =>
          val last = progress(completedSteps - 1)
          assertEquals(outcome, RunOutcome.Cancelled(last.run, Some(last)), where)
          assertEquals(begun, completedSteps + 1, where)
          assertEquals(observed, Vector(last), where)
          assertEquals(progress(completedSteps).stage, stages(completedSteps), where)
          outcome match
            case RunOutcome.Completed(_, _, _) => fail(s"$where published a result")
            case _                             => ()
        }
      }
    } >> IO {
      assertEquals(stages(4), RecordingStage.Interpolating(8))
      assertEquals(stages(15), RecordingStage.Detecting(12))
      assertEquals(stages(positions(2)._2), RecordingStage.Assigning)
    }
  }

  test("a run cancelled inside detection fed samples but flushed nothing") {
    var fed      = 0
    var flushes  = 0
    val counting = plan(method = instrumented(_ => fed += 1, () => flushes += 1))
    cancelAfter(counting, four, 15).flatMap { case (outcome, _, _) =>
      IO {
        assertEquals(outcome.progress.map(_.stage), Some(RecordingStage.Detecting(8)))
        assertEquals((fed, flushes), (12, 0))
        fed = 0
        flushes = 0
      } >> runner.events(counting, recording, four).compile.toVector.map { events =>
        completed(finished(events))
        assertEquals((fed, flushes), (40, 1))
      }
    }
  }

  test("interrupting the pull stream ends it between steps without a terminal event") {
    runner.events(work, recording, four).take(3).compile.toVector.map { events =>
      assertEquals(
        events.map(_.isInstanceOf[RunEvent.Advanced[?, ?, ?, ?, ?]]),
        Vector.fill(3)(true)
      )
      assertEquals(progressOf(events).map(_.step), Vector(1L, 2L, 3L))
    }
  }

  // -------------------------------------------------------------------------
  // Failure is a typed outcome; a defect is raised
  // -------------------------------------------------------------------------

  test("a defect thrown inside a detector step is raised from the outcome") {
    val broken = plan(method = instrumented(fed => if fed == 4 then throw new Broken, () => ()))
    for
      pulled <- runner.events(broken, recording, four).compile.toVector.attempt
      handle <- TestControl.executeEmbed {
        runner.start(broken, recording, four).use { run =>
          run.outcome.attempt.product(run.progress.compile.toVector)
        }
      }
    yield
      assert(pulled.left.exists(_.isInstanceOf[Broken]), clue(pulled))
      val (outcome, observed) = handle
      assert(outcome.left.exists(_.isInstanceOf[Broken]), clue(outcome))
      // The first detection chunk committed; the second threw on its first sample.
      assertEquals(
        observed.lastOption.map(p => (p.step, p.stage)),
        Some((13L, RecordingStage.Detecting(0)))
      )
  }

  test(
    "missing viewing geometry and invalid synchronization settle Failed with the plan's error"
  ) {
    val missing = plan(viewing = None)
    val invalid = plan(
      syncMarks =
        Vector(marks(0), get(SyncMark.of("end", Instant.millis(390), Instant.millis(400)))),
      residualLimit = Some(get(SyncResidualLimit.of(Span.micros(1000))))
    )
    Vector(missing, invalid).traverse_ { p =>
      val error = p.run(recording).swap.getOrElse(fail("expected a failure"))
      val id    = RecordingRunId.of(p, four)
      for
        events <- runner.events(p, recording, four).compile.toVector
        result <- TestControl.executeEmbed {
          runner
            .start(p, recording, four)
            .use(run => run.outcome.product(run.progress.compile.toVector))
        }
      yield
        assertEquals(events, Vector(RunEvent.Finished(RunOutcome.Failed(id, error, None))))
        assertEquals(result._1, RunOutcome.Failed(id, error, None))
        assertEquals(result._2, Vector.empty)
    } >> IO {
      assert(
        missing.run(recording).swap.exists(_.isInstanceOf[RecordingPlanError.MissingViewing])
      )
      assert(
        invalid.run(recording).swap.exists(_.isInstanceOf[RecordingPlanError.Synchronization])
      )
    }
  }

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
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy

/** The shared runner over the temporal cursor, in the shape of
  * [[StudyExecutionSuite]], plus the composition law: a temporal run's step
  * sequence is its cells' study sequences, each stamped with its cell.
  */
class TemporalExecutionSuite extends munit.CatsEffectSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  /** The existing Gaussian oracle contract: absolute, for the designated fixture. */
  private val OracleTolerance = 1e-12

  private val frame = get(Frame.screen("temporal-execution", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val a     = StudyKey("p1", "a", "recall")
  private val b     = StudyKey("p1", "b", "recall")
  private val ar    = StudyKey("p1", "a", "encode")
  private val br    = StudyKey("p1", "b", "encode")
  private val lone  = StudyKey("p2", "c", "recall")

  private def clock(key: StudyKey): ClockId =
    ClockId(key.participant + "/" + key.stimulus + "/" + key.phase)
  private def interval(key: StudyKey, from: Long, until: Long) =
    get(Interval.of(clock(key), Instant.micros(from), Instant.micros(until)))

  /** Fixations of 1000 microseconds at onsets 0, 1000, 2000: the second one
    * straddles the 1500 boundary between the two windows.
    */
  private def trial(key: StudyKey, points: (Double, Double)*) =
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          interval(key, i * 1000L, i * 1000L + 1000L),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock(key), IArray.from(fixes))))

  private val rows = Vector(
    trial(a, (0.5, 0.5), (1.5, 0.5), (0.5, 1.5)),
    trial(b, (0.5, 1.5), (1.5, 1.5)),
    trial(ar, (0.5, 0.5), (0.5, 0.5), (1.5, 1.5)),
    trial(br, (1.5, 0.5), (0.5, 1.5)),
    trial(lone, (0.5, 0.5), (0.5, 0.5))
  )
  private val study = StudyInput(Trials(rows))

  private def epoch(key: StudyKey, anchor: Long = 0L, until: Long = 3000L) =
    key -> TrialEpoch(
      Instant.micros(anchor),
      get(ObservedCoverage.of(clock(key), Vector(interval(key, 0L, until))))
    )

  /** Every trial but `lone` has an epoch, `br` overflows, `b` observed half. */
  private val edges = get(
    TemporalStudyInput.of(
      study,
      Vector(
        epoch(a),
        epoch(b, until = 1500L),
        epoch(ar),
        epoch(br, anchor = Long.MaxValue - 100L)
      )
    )
  )
  private val clean = get(TemporalStudyInput.of(study, rows.map(t => epoch(t.key))))

  private val cosine = StudyMethod.cosine[Px](DefinitionId.cosine)
  private def base(
      input: TemporalStudyInput[StudyKey, Px],
      method: StudyMethod[Unit, Px, Similarity, SignedDifference] = cosine
  ) = get(
    StudyPlan.of(
      input.study.reference,
      StudyKey.layout(DefinitionId.studyLayout),
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned()),
      FailurePolicy.RequireAll,
      method,
      ()
    )
  )
  private def window(name: String, from: Long, until: Long) =
    get(StudyWindow.of(name, get(Window.of(Span.micros(from), Span.micros(until)))))
  private val windows     = Vector(window("early", 0, 1500), window("late", 1500, 3000))
  private val repetitions = Vector(
    get(RepetitionContrast.withinParticipant("recall", "recall", "encode")),
    get(RepetitionContrast.withinParticipant("reversed", "encode", "recall"))
  )
  private def plan(
      input: TemporalStudyInput[StudyKey, Px],
      method: StudyMethod[Unit, Px, Similarity, SignedDifference] = cosine
  ) =
    get(
      TemporalStudyPlan.of(
        base(input, method),
        input.reference,
        windows,
        repetitions,
        FixationBoundary.ClipDuration
      )
    )

  private type Prepared =
    PreparedTemporalStudy[StudyKey, Px, Unit, Similarity, SignedDifference]
  private type Result  = TemporalStudyResult[StudyKey, Px, Unit, Similarity, SignedDifference]
  private type Outcome = TemporalOutcome[StudyKey, Px, Unit, Similarity, SignedDifference]
  private type Event   = TemporalEvent[StudyKey, Px, Unit, Similarity, SignedDifference]

  private val work  = get(plan(clean).prepare(clean))
  private val edged = get(plan(edges).prepare(edges))

  private val finest = WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))
  private val quanta = Vector(
    finest,
    WorkQuanta(get(PairQuantum.of(2)), get(ComparisonQuantum.of(3))),
    WorkQuanta.default
  )

  private val runner  = TemporalExecution[IO]
  private val studies = StudyExecution[IO]

  private final class Broken extends RuntimeException("broken comparison")

  // -------------------------------------------------------------------------
  // Pure oracle
  // -------------------------------------------------------------------------

  private def pure(work: Prepared, quanta: WorkQuanta): (Vector[(TemporalStage, Int)], Result) =
    val steps = Vector.newBuilder[(TemporalStage, Int)]
    @annotation.tailrec
    def loop(cursor: TemporalCursor[StudyKey, Px, Unit, Similarity, SignedDifference]): Result =
      val stage = cursor.stage
      get(cursor.advance(quanta)) match
        case WorkStep.More(_, units, next) =>
          steps += stage -> units
          loop(next)
        case WorkStep.Done(units, result) =>
          steps += stage -> units
          result
    val result = loop(get(work.work()))
    steps.result() -> result

  private def progressOf(events: Vector[Event]): Vector[TemporalProgress] =
    events.collect { case RunEvent.Advanced(progress) => progress }

  private def finished(events: Vector[Event]): Outcome =
    events.collect { case RunEvent.Finished(outcome) => outcome } match
      case Vector(outcome) => outcome
      case other           => fail(s"expected exactly one terminal event, got $other")

  private def completed(outcome: Outcome): (TemporalProgress, Result) = outcome match
    case RunOutcome.Completed(_, last, result) => (last, result)
    case other                                 => fail(s"expected Completed, got $other")

  private def assertSameStudy(
      observed: StudyResult[StudyKey, Px, Similarity, SignedDifference],
      expected: StudyResult[StudyKey, Px, Similarity, SignedDifference],
      clue: Any
  ): Unit =
    assertEquals(observed.input, expected.input, clue)
    assertEquals(observed.description, expected.description, clue)
    assertEquals(observed.scales.size, expected.scales.size, clue)
    observed.scales.zip(expected.scales).foreach { case (o, e) =>
      assertEquals(o.estimate, e.estimate, clue)
      assertEquals(o.excludedPhases, e.excludedPhases, clue)
      assertEquals(o.estimation.map(_._1), e.estimation.map(_._1), clue)
      o.estimation.zip(e.estimation).foreach {
        case ((_, Right(x)), (_, Right(y))) =>
          assertEquals(x.values.toVector, y.values.toVector, clue)
          assertEquals(x.provenance, y.provenance, clue)
        case ((_, Left(x)), (_, Left(y))) => assertEquals(x, y, clue)
        case (x, y)                       => fail(s"estimation outcome differs: $x versus $y")
      }
      (o.contrast, e.contrast) match
        case (Right(x), Right(y)) =>
          assertEquals(x.rows.map(_.key), y.rows.map(_.key), clue)
          assertEquals(x.rows.map(_.difference), y.rows.map(_.difference), clue)
          assertEquals(x.rows.map(_.matched), y.rows.map(_.matched), clue)
          assertEquals(x.rows.map(_.control), y.rows.map(_.control), clue)
          Vector(x.matched -> y.matched, x.control -> y.control).foreach { case (m, n) =>
            assertEquals(m.entries, n.entries, clue)
            assertEquals(m.diagnostics, n.diagnostics, clue)
            assertEquals(m.provenance, n.provenance, clue)
            assertEquals(m.evaluation.name, n.evaluation.name, clue)
            assertEquals(
              m.evaluation.specification
                .map(s => (s.method, s.revision, s.parameters, s.components)),
              n.evaluation.specification
                .map(s => (s.method, s.revision, s.parameters, s.components)),
              clue
            )
            assertEquals(m.source.rows: Any, n.source.rows: Any, clue)
            assertEquals(m.source.diagnostics: Any, n.source.diagnostics: Any, clue)
            assertEquals(m.source.provenance, n.source.provenance, clue)
          }
        case (Left(x), Left(y)) => assertEquals(x, y, clue)
        case (x, y)             => fail(s"contrast outcome differs: $x versus $y")
    }

  private def ledger(cell: TemporalCell[StudyKey, Px, Unit, Similarity, SignedDifference]) =
    cell.occupancy.map { case (key, value) =>
      key -> value.map(o =>
        (
          o.interval,
          o.boundary,
          o.observedMicros,
          o.missingMicros,
          o.fixationTimes,
          o.measure.provenance
        )
      )
    }

  private def assertSameResult(observed: Result, expected: Result, clue: Any): Unit =
    assertEquals(observed.description, expected.description, clue)
    assertEquals(
      observed.cells.map(c => (c.repetition.name, c.window.name)),
      expected.cells.map(c => (c.repetition.name, c.window.name)),
      clue
    )
    observed.cells.zip(expected.cells).foreach { case (o, e) =>
      assertEquals(ledger(o), ledger(e), clue)
      assertSameStudy(o.result, e.result, clue)
    }

  private def cancelAfter(
      work: Prepared,
      quanta: WorkQuanta,
      completedSteps: Int
  ): IO[(Outcome, Vector[TemporalProgress], Int)] =
    TestControl.executeEmbed {
      for
        started <- Ref[IO].of(0)
        reached <- Deferred[IO, Unit]
        parked  <- Deferred[IO, Unit]
        between = started.updateAndGet(_ + 1).flatMap { n =>
          if n == completedSteps + 1 then reached.complete(()) >> parked.get else IO.unit
        }
        result <- runner.start(work, quanta = quanta, between = between).use { run =>
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

  test("events at every quantum replay the pure cursor's stages and plan.run's result") {
    Vector(work -> clean, edged -> edges).traverse_ { case (prepared, input) =>
      val expected = get(plan(input).run(input))
      quanta.traverse_ { q =>
        val (steps, pureResult) = pure(prepared, q)
        assertSameResult(pureResult, expected, q)
        for
          first  <- runner.events(prepared, quanta = q).compile.toVector
          second <- runner.events(prepared, quanta = q).compile.toVector
        yield
          assertEquals(progressOf(first), progressOf(second), q)
          val progress = progressOf(first)
          assertEquals(progress.map(p => (p.stage, p.stepUnits)), steps, q)
          assertEquals(progress.map(_.step), (1L to steps.size.toLong).toVector)
          assertEquals(progress.map(_.totalUnits), steps.scanLeft(0L)(_ + _._2).tail)
          assert(progress.forall(_.run == TemporalRunId.of(prepared, q)))
          val (last, result) = completed(finished(first))
          assertEquals(first.last, RunEvent.Finished(finished(first)))
          assertEquals(last, progress.last)
          assertSameResult(result, expected, q)
      }
    } >> IO {
      // The edge fixture's facts survive as typed data, not as run failures.
      val early = get(plan(edges).run(edges)).cells.head.occupancy.toMap
      assert(early(lone).left.exists(_.isInstanceOf[TemporalStudyError.MissingEpoch]))
      assert(early(br).left.exists(_.isInstanceOf[TemporalStudyError.AnchorOverflow]))
      assertEquals(get(early(a)).fixationTimes.map(_.retainedMicros), Vector(1000L, 500L, 0L))
      val late = get(plan(edges).run(edges)).cells(1).occupancy.toMap
      assertEquals(get(late(b)).missingMicros, 1500L)
    }
  }

  test("a run id is a function of the temporal input, plan revision and quanta") {
    val id = TemporalRunId.of(work, finest)
    assertEquals(id, TemporalRunId.of(work, finest))
    assertEquals(id.input, clean.reference.digest)
    assertEquals((id.layout, id.method), (DefinitionId.studyLayout, DefinitionId.cosine))
    assertEquals(id.parameters, work.description)
    assertEquals((id.pairQuantum, id.comparisonQuantum), (1, 1))
    assertNotEquals(id, TemporalRunId.of(work, WorkQuanta.default))
    // Same fixation data, different anchors and coverage: a different input.
    assertNotEquals(id, TemporalRunId.of(edged, finest))
    assertEquals(
      id,
      TemporalRunId.of(work, finest.copy(samples = get(SampleQuantum.of(1))))
    )
    runner.start(work, quanta = finest).use(run => run.outcome.map(o => (run.id, o.run))).map {
      case (runId, outcomeId) => assertEquals((runId, outcomeId), (id, id))
    }
  }

  test(
    "segments are per-cell with exact preparation totals and the repetition's study totals"
  ) {
    runner.events(work, quanta = finest).compile.toVector.map { events =>
      val progress = progressOf(events)
      val cells    = for r <- repetitions.indices; w <- windows.indices yield (r, w)
      assertEquals(
        progress.map(_.segment).distinct,
        cells.toVector.flatMap { case (r, w) =>
          TemporalSegment.Preparing(r, w) +: Vector(
            StudySegment.Estimating(0),
            StudySegment.Comparing(0, StudyDesign.Matched),
            StudySegment.Reducing(0, StudyDesign.Matched),
            StudySegment.Comparing(0, StudyDesign.Control),
            StudySegment.Reducing(0, StudyDesign.Control),
            StudySegment.Contrasting(0)
          ).map(TemporalSegment.Studying(r, w, _))
        }
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
      cells.foreach { case (r, w) =>
        assertEquals(
          totals(TemporalSegment.Preparing(r, w)),
          SegmentTotal.Exact(rows.size.toLong)
        )
        assertEquals(last(TemporalSegment.Preparing(r, w)), rows.size.toLong)
        val prepared = work.repetitions(r).prepared
        Vector(
          StudySegment.Estimating(0),
          StudySegment.Comparing(0, StudyDesign.Matched),
          StudySegment.Reducing(0, StudyDesign.Matched),
          StudySegment.Comparing(0, StudyDesign.Control),
          StudySegment.Reducing(0, StudyDesign.Control),
          StudySegment.Contrasting(0)
        ).foreach { inner =>
          val segment = TemporalSegment.Studying(r, w, inner)
          assertEquals(totals(segment), StudyExecution.total(prepared, inner), segment)
          totals(segment) match
            case SegmentTotal.Exact(units)  => assertEquals(last(segment), units, segment)
            case SegmentTotal.AtMost(units) => assert(last(segment) <= units, clue(segment))
            case SegmentTotal.Unknown       => ()
        }
      }
      // The reversed repetition has a different focal set, hence its own totals.
      assertNotEquals(
        totals(TemporalSegment.Studying(0, 0, StudySegment.Contrasting(0))),
        totals(TemporalSegment.Studying(1, 0, StudySegment.Contrasting(0)))
      )
    }
  }

  test("a temporal run is its cells' study sequences concatenated with the cell stamped") {
    quanta.traverse_ { q =>
      for
        temporal <- runner.events(work, quanta = q).compile.toVector
        studies  <- work.repetitions.traverse(r =>
          studies.events(r.prepared, quanta = q).compile.toVector
        )
      yield
        val progress = progressOf(temporal)
        val expected = for
          r <- repetitions.indices.toVector
          w <- windows.indices.toVector
        yield
          val inner = studies(r).collect { case StudyEvent.Advanced(p) => p }
          rows.indices.toVector.map(t =>
            (
              TemporalStage.Preparing(r, w, t),
              1,
              TemporalSegment.Preparing(r, w),
              SegmentTotal.Exact(rows.size.toLong)
            )
          ) ++ inner.map(p =>
            (
              TemporalStage.Studying(r, w, p.stage),
              p.stepUnits,
              TemporalSegment.Studying(r, w, p.segment),
              p.segmentTotal
            )
          )
        assertEquals(
          progress.map(p => (p.stage, p.stepUnits, p.segment, p.segmentTotal)),
          expected.flatten,
          q
        )
        // Segment counts restart at each cell exactly as the study run counts them.
        val perCell = progress.groupBy(p =>
          (p.stage, p.segment) match
            case (TemporalStage.Preparing(r, w, _), _) => (r, w)
            case (TemporalStage.Studying(r, w, _), _)  => (r, w)
        )
        perCell.foreach { case ((r, w), steps) =>
          val inner = studies(r).collect { case StudyEvent.Advanced(p) => p }
          assertEquals(
            steps.collect {
              case p if p.stage.isInstanceOf[TemporalStage.Studying] => p.segmentUnits
            },
            inner.map(_.segmentUnits),
            (r, w, q)
          )
        }
    }
  }

  // -------------------------------------------------------------------------
  // The run handle: commit, observers, disposal
  // -------------------------------------------------------------------------

  test("start commits the pure result once and cancelling afterwards changes nothing") {
    val expected = get(plan(clean).run(clean))
    quanta.traverse_ { q =>
      val (steps, _) = pure(work, q)
      for
        events <- runner.events(work, quanta = q).compile.toVector
        result <- TestControl.executeEmbed {
          runner.start(work, quanta = q).use { run =>
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
        assertSameResult(committed, expected, q)
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
  // Cancellation lands between steps and never publishes a partial study
  // -------------------------------------------------------------------------

  test("cancelling before any work settles Cancelled with no progress") {
    val id = TemporalRunId.of(work, finest)
    TestControl
      .executeEmbed(runner.start(work, quanta = finest).use(run => run.cancel >> run.outcome))
      .map(outcome => assertEquals(outcome, RunOutcome.Cancelled(id, None)))
      >> cancelAfter(work, finest, 0).map { case (outcome, observed, begun) =>
        assertEquals(outcome, RunOutcome.Cancelled(id, None))
        assertEquals(observed, Vector.empty)
        assertEquals(begun, 1)
      }
  }

  test(
    "cancelling inside occupancy, estimation, comparison, between cells and before commit lands between steps"
  ) {
    val (steps, _)                           = pure(work, finest)
    val stages                               = steps.map(_._1)
    def firstOf(p: TemporalStage => Boolean) = stages.indexWhere(p)
    val positions                            = Vector(
      "occupancy"  -> 2,
      "estimation" -> (firstOf {
        case TemporalStage.Studying(0, 0, StudyStage.Estimating(_, _)) => true
        case _                                                         => false
      } + 1),
      "comparison" -> (firstOf {
        case TemporalStage.Studying(0, 0, StudyStage.Comparing(_, _)) => true
        case _                                                        => false
      } + 1),
      "between cells" -> firstOf {
        case TemporalStage.Preparing(0, 1, _) => true
        case _                                => false
      },
      "between repetitions" -> firstOf {
        case TemporalStage.Preparing(1, 0, _) => true
        case _                                => false
      },
      "before commit" -> (steps.size - 1)
    )
    runner.events(work, quanta = finest).compile.toVector.flatMap { events =>
      val progress = progressOf(events)
      positions.traverse_ { case (where, completedSteps) =>
        assert(completedSteps >= 1 && completedSteps < steps.size, clue(where))
        cancelAfter(work, finest, completedSteps).map { case (outcome, observed, begun) =>
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
      assertEquals(stages(2), TemporalStage.Preparing(0, 0, 2))
      // Parked before the second trial's estimation: the first one completed.
      assertEquals(
        stages(positions(1)._2),
        TemporalStage.Studying(0, 0, StudyStage.Estimating(0, 1))
      )
      assertEquals(
        stages(positions(2)._2),
        TemporalStage.Studying(0, 0, StudyStage.Comparing(0, StudyDesign.Matched))
      )
      // A cell boundary: the step before is the previous cell's finishing contrast step.
      assertEquals(stages(positions(3)._2), TemporalStage.Preparing(0, 1, 0))
      assertEquals(
        stages(positions(3)._2 - 1),
        TemporalStage.Studying(0, 0, StudyStage.Contrasting(0))
      )
      assertEquals(stages(positions(4)._2), TemporalStage.Preparing(1, 0, 0))
      assertEquals(
        stages(positions(5)._2),
        TemporalStage.Studying(1, 1, StudyStage.Contrasting(0))
      )
    }
  }

  test("interrupting the pull stream ends it between steps without a terminal event") {
    runner.events(work, quanta = finest).take(3).compile.toVector.map { events =>
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

  test("a defect thrown inside a cell's comparison is raised from the outcome") {
    val throwing = new StudyMethod[Unit, Px, Similarity, SignedDifference](
      DefinitionId.cosine,
      "throwing cosine",
      _ => Vector.empty,
      _ =>
        val inner = Distribution.cosine[Px]
        new Compare[Mass[Px], Mass[Px], Similarity]:
          def info: MeasureInfo                                                   = inner.info
          def compare(x: Mass[Px], y: Mass[Px]): Either[CompareError, Similarity] =
            throw new Broken
      ,
      Some(MethodDescriptor.cosine[Px](DefinitionId.cosine))
    )
    val broken = get(plan(clean, throwing).prepare(clean))
    for
      pulled <- runner.events(broken, quanta = finest).compile.toVector.attempt
      handle <- TestControl.executeEmbed {
        runner.start(broken, quanta = finest).use { run =>
          run.outcome.attempt.product(run.progress.compile.toVector)
        }
      }
    yield
      assert(pulled.left.exists(_.isInstanceOf[Broken]), clue(pulled))
      val (outcome, observed) = handle
      assert(outcome.left.exists(_.isInstanceOf[Broken]), clue(outcome))
      // Every trial was prepared and estimated and the first schedule page was
      // visited before the first whole comparison threw, inside cell (0, 0).
      assertEquals(
        observed.lastOption.map(p => (p.step, p.stage)),
        Some(
          (
            2L * rows.size + 1L,
            TemporalStage.Studying(0, 0, StudyStage.Comparing(0, StudyDesign.Matched))
          )
        )
      )
  }

  test("a refused comparison budget settles Failed after the first cell's preparation") {
    val budget = get(ComparisonBudget.of(3))
    val error  = TemporalStudyError.Input(
      PlanError.ComparisonWork(ComparisonWorkError.WorkBudget("cosine", 4, 3))
    )
    val id = TemporalRunId.of(work, finest)
    for
      events <- runner.events(work, budget, finest).compile.toVector
      result <- TestControl.executeEmbed {
        runner.start(work, budget, finest).use { run =>
          run.outcome.product(run.progress.compile.toVector)
        }
      }
    yield
      val progress = progressOf(events)
      assertEquals(progress.size, rows.size - 1)
      assertEquals(
        progress.map(_.stage),
        rows.indices.init.map(TemporalStage.Preparing(0, 0, _)).toVector
      )
      assertEquals(finished(events), RunOutcome.Failed(id, error, progress.lastOption))
      assertEquals(result._1, RunOutcome.Failed(id, error, progress.lastOption))
      assertEquals(result._2, progress.lastOption.toVector)
      assert(progress.nonEmpty)
  }

  test("a missing or wrong input is refused by preparation, never by a run") {
    val p = plan(clean)
    assert(p.prerequisites(None).nonEmpty)
    p.prepare(edges) match
      case Left(TemporalStudyError.Input(PlanError.ArtifactMismatch(_, _))) => ()
      case other => fail(s"expected an artifact mismatch, got $other")
    IO.unit
  }

  // -------------------------------------------------------------------------
  // The designated temporal fixture: exact ledgers and contrast targets
  // -------------------------------------------------------------------------

  test(
    "the runner reproduces the exact temporal ledgers and contrast targets of the designated fixture"
  ) {
    val oracleFrame                  = get(Frame.screen("temporal", 2, 2))
    val oracleGrid                   = get(Grid.over(oracleFrame, 2, 2))
    def key(label: String): StudyKey =
      val pieces = label.split("/")
      StudyKey(pieces(0), pieces(1), pieces(2))
    def label(k: StudyKey): String = s"${k.participant}/${k.stimulus}/${k.phase}"
    def oracleClock(k: StudyKey)   =
      ClockId(s"fixation-trial:${KeyDigest[StudyKey].digest(k).render}")
    def oracleInterval(k: StudyKey, start: Long, end: Long) =
      get(Interval.of(oracleClock(k), Instant.micros(start), Instant.micros(end)))
    val trials = TemporalFixtures.csv.linesIterator
      .drop(1)
      .map(_.split(",").toVector)
      .toVector
      .groupBy(row => StudyKey(row(0), row(1), row(2)))
      .toVector
      .sortBy(_._1)
      .map { case (k, rows) =>
        val fixes = rows
          .sortBy(_(3).toInt)
          .map(row =>
            get(
              Event.Fixation.withoutDispersion(
                oracleInterval(k, row(6).toLong, row(6).toLong + row(7).toLong),
                Pt[Px](row(4).toDouble, row(5).toDouble),
                row(8).toInt
              )
            )
          )
        Trial(k, (), get(Scanpath.of(oracleFrame, oracleClock(k), IArray.from(fixes))))
      }
    val oracleStudy = StudyInput(Trials(trials))
    val epochs      = TemporalFixtures.coverage.toVector.map { case (name, spans) =>
      val k = key(name)
      k -> TrialEpoch(
        Instant.micros(0),
        get(
          ObservedCoverage.of(
            oracleClock(k),
            spans.map { case (a, b) => oracleInterval(k, a, b) }
          )
        )
      )
    }
    val input      = get(TemporalStudyInput.of(oracleStudy, epochs))
    val oracleBase = get(
      StudyPlan.cosine(
        oracleStudy.reference,
        oracleGrid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(
          StudyEstimate.Binned(),
          StudyEstimate.Gaussian(get(Sigma.px(1)), EdgePolicy.Truncate)
        ),
        FailurePolicy.RequireAll
      )
    )
    val oracleWindows = TemporalFixtures.windows.map { case (name, a, b) =>
      get(StudyWindow.of(name, get(Window.of(Span.micros(a), Span.micros(b)))))
    }
    val oracleRepeats = TemporalFixtures.repetitions.map { case (name, f, r) =>
      get(RepetitionContrast.withinParticipant(name, f, r))
    }
    val oracle = get(
      TemporalStudyPlan
        .of(
          oracleBase,
          input.reference,
          oracleWindows,
          oracleRepeats,
          FixationBoundary.ClipDuration
        )
    )
    val prepared = get(oracle.prepare(input))
    val direct   = get(oracle.run(input))
    runner
      .events(
        prepared,
        quanta = WorkQuanta(get(PairQuantum.of(3)), get(ComparisonQuantum.of(2)))
      )
      .compile
      .toVector
      .map { events =>
        val (_, result) = completed(finished(events))
        assertSameResult(result, direct, "designated fixture")
        assertEquals(result.cells.size, 8)
        val actual = result.cells
          .flatMap(cell =>
            cell.result.scales.flatMap(scale =>
              get(scale.contrast).rows.map(row =>
                (cell.repetition.name, label(row.key), cell.window.name, scale.estimate) ->
                  row.difference.toOption.map(_.value)
              )
            )
          )
          .toMap
        assertEquals(actual.size, 96)
        TemporalFixtures.targets.foreach { t =>
          val scale = t.sigma.fold[StudyEstimate[Px]](StudyEstimate.Binned())(s =>
            StudyEstimate.Gaussian(get(Sigma.px(s)), EdgePolicy.Truncate)
          )
          val value = actual((t.repetition, t.key, t.window, scale))
          (value, t.difference) match
            case (Some(a), Some(b)) =>
              assert(math.abs(a - b) <= OracleTolerance, s"${t.key}/${t.window}: $a != $b")
            case _ => assertEquals(value, t.difference)
        }
        result.cells.foreach { cell =>
          cell.occupancy.foreach { case (k, outcome) =>
            val (_, _, retained, observed, missing) = TemporalFixtures.ledgers
              .find(t => t._1 == label(k) && t._2 == cell.window.name)
              .getOrElse(fail(label(k)))
            val value = get(outcome)
            assertEquals(value.fixationTimes.map(_.retainedMicros), retained)
            assertEquals(value.observedMicros, observed)
            assertEquals(value.missingMicros, missing)
          }
        }
      }
  }

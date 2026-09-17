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

/** The FS2 runner against the pure cursor and the pure runner.
  *
  * Every cancellation test positions the cancel with a barrier in the
  * runner's between-step hook under `TestControl`, never with a sleep, and
  * asserts the outcome's last progress against the pure event sequence, so
  * "landed between steps" is an exact step number, not a timing claim.
  */
class StudyExecutionSuite extends munit.CatsEffectSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  private val frame = get(Frame.screen("execution", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val a     = StudyKey("p1", "a", "recall")
  private val b     = StudyKey("p1", "b", "recall")
  private val ar    = StudyKey("p1", "a", "encode")
  private val br    = StudyKey("p1", "b", "encode")

  private def trial(key: StudyKey, points: (Double, Double)*) =
    val clock = ClockId(key.participant + "/" + key.stimulus + "/" + key.phase)
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 2000L), Instant.micros(i * 2000L + 1000L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))

  private val input = StudyInput(
    Trials(
      Vector(
        trial(a, (0.5, 0.5), (1.5, 0.5)),
        trial(b, (0.5, 1.5)),
        trial(ar, (0.5, 0.5)),
        trial(br, (1.5, 0.5), (0.5, 1.5)),
        trial(StudyKey("p2", "c", "excluded"), (0.5, 0.5))
      )
    )
  )

  private val cosine = StudyMethod.cosine[Px](DefinitionId.cosine)

  private def plan(
      source: StudyInput[StudyKey, Px] = input,
      scales: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned()),
      method: StudyMethod[Unit, Px, Similarity, SignedDifference] = cosine
  ) =
    get(
      StudyPlan.of(
        source.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        grid,
        "recall",
        "encode",
        Weight.Duration,
        scales,
        FailurePolicy.RequireAll,
        method,
        ()
      )
    )

  private val work = get(plan().prepare(input))

  private val finest = WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))
  private val quanta = Vector(
    finest,
    WorkQuanta(get(PairQuantum.of(2)), get(ComparisonQuantum.of(3))),
    WorkQuanta.default
  )

  private type Result   = StudyResult[StudyKey, Px, Similarity, SignedDifference]
  private type Outcome  = StudyOutcome[StudyKey, Px, Similarity, SignedDifference]
  private type Event    = StudyEvent[StudyKey, Px, Similarity, SignedDifference]
  private type Prepared = PreparedStudy[StudyKey, Px, Unit, Similarity, SignedDifference]

  private val runner = StudyExecution[IO]

  /** A defect, distinct from every typed failure. */
  private final class Broken extends RuntimeException("broken comparison")

  // -------------------------------------------------------------------------
  // Pure oracle
  // -------------------------------------------------------------------------

  /** Stage and units of every cursor step, the `Done` step included, plus the result. */
  private def pure(
      work: Prepared,
      quanta: WorkQuanta
  ): (Vector[(StudyStage, Int)], Result) =
    val steps = Vector.newBuilder[(StudyStage, Int)]
    @annotation.tailrec
    def loop(cursor: StudyCursor[StudyKey, Px, Similarity, SignedDifference]): Result =
      val stage = cursor.stage
      get(cursor.advance(quanta)) match
        case StudyStep.More(_, units, next) =>
          steps += stage -> units
          loop(next)
        case StudyStep.Done(units, result) =>
          steps += stage -> units
          result
    val result = loop(get(work.work()))
    steps.result() -> result

  private def progressOf(events: Vector[Event]): Vector[StudyProgress] =
    events.collect { case StudyEvent.Advanced(progress) => progress }

  private def finished(events: Vector[Event]): Outcome =
    events.collect { case StudyEvent.Finished(outcome) => outcome } match
      case Vector(outcome) => outcome
      case other           => fail(s"expected exactly one terminal event, got $other")

  private def completed(outcome: Outcome): (StudyProgress, Result) = outcome match
    case StudyOutcome.Completed(_, last, result) => (last, result)
    case other                                   => fail(s"expected Completed, got $other")

  private def isSubsequence[A](sub: Vector[A], of: Vector[A]): Boolean =
    sub
      .foldLeft(Option(0)) { (from, a) =>
        from.flatMap(i => Some(of.indexOf(a, i)).filter(_ >= 0).map(_ + 1))
      }
      .isDefined

  private def assertSameResult(observed: Result, expected: Result, clue: Any): Unit =
    assertEquals(observed.input, expected.input)
    assertEquals(observed.description, expected.description)
    assertEquals(observed.scales.size, expected.scales.size, clue)
    observed.scales.zip(expected.scales).foreach { case (o, e) =>
      assertEquals(o.estimate, e.estimate)
      assertEquals(o.excludedPhases, e.excludedPhases)
      assertEquals(o.estimation.map(_._1), e.estimation.map(_._1))
      o.estimation.zip(e.estimation).foreach {
        case ((_, Right(x)), (_, Right(y))) =>
          assertEquals(x.values.toVector, y.values.toVector)
          assertEquals(x.provenance, y.provenance)
        case ((_, Left(x)), (_, Left(y))) => assertEquals(x, y)
        case (x, y)                       => fail(s"estimation outcome differs: $x versus $y")
      }
      (o.contrast, e.contrast) match
        case (Right(x), Right(y)) =>
          assertEquals(x.rows.map(_.key), y.rows.map(_.key))
          assertEquals(x.rows.map(_.difference), y.rows.map(_.difference))
          assertEquals(x.rows.map(_.matched), y.rows.map(_.matched))
          assertEquals(x.rows.map(_.control), y.rows.map(_.control))
          Vector(x.matched -> y.matched, x.control -> y.control).foreach { case (m, n) =>
            assertEquals(m.entries, n.entries)
            assertEquals(m.diagnostics, n.diagnostics)
            assertEquals(m.provenance, n.provenance)
            assertEquals(m.evaluation.name, n.evaluation.name)
            assertEquals(m.evaluation.scale, n.evaluation.scale)
            assertEquals(
              m.evaluation.specification
                .map(s => (s.method, s.revision, s.parameters, s.components)),
              n.evaluation.specification
                .map(s => (s.method, s.revision, s.parameters, s.components))
            )
            assertEquals(m.source.rows: Any, n.source.rows: Any)
            assertEquals(m.source.diagnostics: Any, n.source.diagnostics: Any)
            assertEquals(m.source.provenance, n.source.provenance)
          }
        case (Left(x), Left(y)) => assertEquals(x, y)
        case (x, y)             => fail(s"contrast outcome differs: $x versus $y")
    }

  /** Run under `TestControl`, cancelling once `completedSteps` steps have
    * committed and the runner is parked at the barrier before the next one.
    * Returns the outcome, what a late observer sees, and how many steps the
    * runner began.
    */
  private def cancelAfter(
      work: Prepared,
      quanta: WorkQuanta,
      completedSteps: Int
  ): IO[(Outcome, Vector[StudyProgress], Int)] =
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
    val expected = get(work.run)
    quanta.traverse_ { q =>
      val (steps, pureResult) = pure(work, q)
      assertSameResult(pureResult, expected, q)
      for
        first  <- runner.events(work, quanta = q).compile.toVector
        second <- runner.events(work, quanta = q).compile.toVector
      yield
        // Same quanta, same event sequence: progress values are compared exactly.
        assertEquals(progressOf(first), progressOf(second), q)
        val progress = progressOf(first)
        assertEquals(progress.map(p => (p.stage, p.stepUnits)), steps, q)
        assertEquals(progress.map(_.step), (1L to steps.size.toLong).toVector)
        assertEquals(progress.map(_.totalUnits), steps.scanLeft(0L)(_ + _._2).tail)
        assert(progress.forall(_.run == StudyRunId.of(work, q)))
        val (last, result) = completed(finished(first))
        assertEquals(first.last, StudyEvent.Finished(finished(first)))
        assertEquals(last, progress.last)
        assertSameResult(result, expected, q)
        assertSameResult(result, pureResult, q)
    } >> IO {
      val (finestSteps, _) = pure(work, finest)
      val (coarseSteps, _) = pure(work, WorkQuanta.default)
      assert(finestSteps.size > coarseSteps.size, clue((finestSteps.size, coarseSteps.size)))
    }
  }

  test("a run id is a function of the input, plan revision and quanta") {
    val id = StudyRunId.of(work, finest)
    assertEquals(id, StudyRunId.of(work, finest))
    assertEquals(id.input, work.inputReference.digest)
    assertEquals((id.layout, id.method), (work.layoutId, work.methodId))
    assertEquals(id.parameters, work.description)
    assertEquals((id.pairQuantum, id.comparisonQuantum), (1, 1))
    assertNotEquals(id, StudyRunId.of(work, WorkQuanta.default))
    val revised = get(
      plan(scales = Vector(StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)))
        .prepare(input)
    )
    assertNotEquals(id, StudyRunId.of(revised, finest))
    val reversed = StudyInput(Trials(input.trials.rows.reverse))
    assertNotEquals(id, StudyRunId.of(get(plan(reversed).prepare(reversed)), finest))
    runner.start(work, quanta = finest).use(run => run.outcome.map(o => (run.id, o.run))).map {
      case (runId, outcomeId) => assertEquals((runId, outcomeId), (id, id))
    }
  }

  test("segments run in scientific order with monotone counts and typed totals") {
    val two = get(
      plan(scales =
        Vector(
          StudyEstimate.Binned(),
          StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)
        )
      ).prepare(input)
    )
    val cells = grid.size
    runner.events(two, quanta = finest).compile.toVector.map { events =>
      val progress = progressOf(events)
      assertSameResult(completed(finished(events))._2, get(two.run), "two scales")
      val perScale = (scale: Int) =>
        Vector(
          StudySegment.Estimating(scale),
          StudySegment.Comparing(scale, StudyDesign.Matched),
          StudySegment.Reducing(scale, StudyDesign.Matched),
          StudySegment.Comparing(scale, StudyDesign.Control),
          StudySegment.Reducing(scale, StudyDesign.Control),
          StudySegment.Contrasting(scale)
        )
      assertEquals(progress.map(_.segment).distinct, perScale(0) ++ perScale(1))
      // Trials of one scale count toward one estimating segment.
      assertEquals(
        progress.filter(_.segment == StudySegment.Estimating(1)).map(_.stage),
        input.trials.rows.indices.map(StudyStage.Estimating(1, _)).toVector
      )
      progress.zip(progress.tail).foreach { case (before, after) =>
        assert(after.totalUnits >= before.totalUnits, clue((before, after)))
        assertEquals(after.totalUnits - before.totalUnits, after.stepUnits.toLong)
        if before.segment == after.segment then
          assertEquals(after.segmentUnits - before.segmentUnits, after.stepUnits.toLong)
        else assertEquals(after.segmentUnits, after.stepUnits.toLong)
      }
      val bySegment = progress.groupBy(_.segment).view.mapValues(_.last).toMap
      val totals    = progress.map(p => p.segment -> p.segmentTotal).distinct.toMap
      assertEquals(totals.size, bySegment.size)
      (0 to 1).foreach { scale =>
        val trials = input.trials.rows.size.toLong
        assertEquals(totals(StudySegment.Estimating(scale)), SegmentTotal.Exact(trials))
        assertEquals(bySegment(StudySegment.Estimating(scale)).segmentUnits, trials)
        Vector(
          StudyDesign.Matched -> two.matched.candidatePairCount,
          StudyDesign.Control -> two.controls.candidatePairCount
        ).foreach { case (design, candidates) =>
          val comparing = StudySegment.Comparing(scale, design)
          val bound     =
            candidates * (2 + cells) + two.focalIndices.size + two.referenceIndices.size
          assertEquals(totals(comparing), SegmentTotal.AtMost(bound))
          val units = bySegment(comparing).segmentUnits
          assert(units >= 1 && units <= bound, clue((comparing, units)))
          assertEquals(totals(StudySegment.Reducing(scale, design)), SegmentTotal.Unknown)
        }
        val focal = two.focalIndices.size.toLong
        assertEquals(totals(StudySegment.Contrasting(scale)), SegmentTotal.AtMost(focal))
        assert(bySegment(StudySegment.Contrasting(scale)).segmentUnits <= focal)
      }
    }
  }

  test("a synchronous method bounds comparison by whole pairs") {
    val synchronous = new StudyMethod[Unit, Px, Similarity, SignedDifference](
      DefinitionId.cosine,
      "synchronous cosine",
      _ => Vector.empty,
      _ => Distribution.cosine[Px]: Compare[Mass[Px], Mass[Px], Similarity],
      Some(MethodDescriptor.cosine[Px](DefinitionId.cosine))
    )
    val whole = get(plan(method = synchronous).prepare(input))
    assertEquals(whole.capability, ExecutionCapability.SynchronousWholeOperation)
    runner.events(whole, quanta = finest).compile.toVector.map { events =>
      val totals = progressOf(events).map(p => p.segment -> p.segmentTotal).toMap
      assertEquals(
        totals(StudySegment.Comparing(0, StudyDesign.Matched)),
        SegmentTotal.AtMost(
          2 * whole.matched.candidatePairCount + whole.focalIndices.size + whole.referenceIndices.size
        )
      )
      assertSameResult(completed(finished(events))._2, get(whole.run), "synchronous")
    }
  }

  test("a fully matched schedule and one without reference trials stay within their bounds") {
    // One focal, one reference on the same stimulus: every candidate is selected,
    // so the paging visit, the begin unit and the cells saturate
    // candidates * (2 + cells) and the unmatched-reference report pass exceeds it.
    val full =
      StudyInput(Trials(Vector(trial(a, (0.5, 0.5), (1.5, 0.5)), trial(ar, (0.5, 0.5)))))
    val fullWork = get(plan(full).prepare(full))
    // One focal and no reference: the schedule charges one unit per focal key.
    val alone     = StudyInput(Trials(Vector(trial(a, (0.5, 0.5)))))
    val aloneWork = get(plan(alone).prepare(alone))
    val cells     = grid.size
    Vector(fullWork -> "fully matched", aloneWork -> "no reference").traverse_ {
      case (prepared, where) =>
        runner.events(prepared, quanta = finest).compile.toVector.map { events =>
          val progress = progressOf(events)
          assertSameResult(completed(finished(events))._2, get(prepared.run), where)
          Vector(
            StudyDesign.Matched -> prepared.matched.candidatePairCount,
            StudyDesign.Control -> prepared.controls.candidatePairCount
          ).foreach { case (design, candidates) =>
            val segment  = StudySegment.Comparing(0, design)
            val realized = progress.filter(_.segment == segment).map(_.segmentUnits).max
            val bound    =
              candidates * (2 + cells) + prepared.focalIndices.size + prepared.referenceIndices.size
            assertEquals(
              progress.find(_.segment == segment).map(_.segmentTotal),
              Some(SegmentTotal.AtMost(bound)),
              (where, design)
            )
            assert(realized <= bound, clue((where, design, realized, bound)))
          }
        }
    } >> runner.events(fullWork, quanta = finest).compile.toVector.map { events =>
      // The matched design of the fully matched fixture: 1 visit + 1 report + 1 begin + 4 cells.
      val matched =
        progressOf(events).filter(_.segment == StudySegment.Comparing(0, StudyDesign.Matched))
      assertEquals(matched.map(_.segmentUnits).max, 7L)
      assert(
        7L > fullWork.matched.candidatePairCount * (2 + cells),
        "the pair-only bound is too small"
      )
    } >> runner.events(aloneWork, quanta = finest).compile.toVector.map { events =>
      val matched =
        progressOf(events).filter(_.segment == StudySegment.Comparing(0, StudyDesign.Matched))
      assertEquals(aloneWork.matched.candidatePairCount, 0L)
      assertEquals(matched.map(_.segmentUnits).max, 1L)
    }
  }

  // -------------------------------------------------------------------------
  // The run handle: commit, observers, disposal
  // -------------------------------------------------------------------------

  test("start commits the pure result once and cancelling afterwards changes nothing") {
    val expected = get(work.run)
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
        assert(isSubsequence(observed, progressOf(events)), clue(observed))
        assertEquals(observed.last, last)
        assert(observed.zip(observed.tail).forall(_.step < _.step))
        // A late observer receives the terminal step and ends.
        assertEquals(late, Vector(last))
    }
  }

  test("a detached, a slow and a disposed observer never stall the run") {
    val (steps, _) = pure(work, finest)
    TestControl
      .executeEmbed {
        for
          detached <- runner.start(work, quanta = finest).use(_.outcome)
          gate     <- Deferred[IO, Unit]
          slow     <- runner.start(work, quanta = finest).use { run =>
            for
              observer <- run.progress.evalTap(_ => gate.get).compile.toVector.start
              outcome  <- run.outcome
              _        <- gate.complete(())
              observed <- observer.joinWithNever
            yield (outcome, observed)
          }
          disposed <- runner.start(work, quanta = finest).use { run =>
            run.progress.take(1).compile.toVector.product(run.outcome)
          }
        yield (detached, slow, disposed)
      }
      .map { case (detached, (slowOutcome, slowSeen), (disposedSeen, disposedOutcome)) =>
        val (last, _) = completed(detached)
        assertEquals(last.step, steps.size.toLong)
        // The single slot coalesces: a blocked observer misses steps, keeps order,
        // and still receives the terminal one.
        assertEquals(completed(slowOutcome)._1, last)
        assert(slowSeen.size < steps.size, clue(slowSeen.size))
        assert(slowSeen.zip(slowSeen.tail).forall(_.step < _.step))
        assertEquals(slowSeen.lastOption, Some(last))
        assertEquals(disposedSeen.size, 1)
        assertEquals(completed(disposedOutcome)._1, last)
      }
  }

  // -------------------------------------------------------------------------
  // Cancellation lands between steps and never publishes a result
  // -------------------------------------------------------------------------

  test("cancelling before any work settles Cancelled with no progress") {
    val id = StudyRunId.of(work, finest)
    TestControl
      .executeEmbed(runner.start(work, quanta = finest).use(run => run.cancel >> run.outcome))
      .map(outcome => assertEquals(outcome, StudyOutcome.Cancelled(id, None)))
      >> cancelAfter(work, finest, 0).map { case (outcome, observed, begun) =>
        assertEquals(outcome, StudyOutcome.Cancelled(id, None))
        assertEquals(observed, Vector.empty)
        assertEquals(begun, 1)
      }
  }

  test(
    "cancelling during estimation, comparison, reduction and before commit lands between steps"
  ) {
    val (steps, _)                        = pure(work, finest)
    val stages                            = steps.map(_._1)
    def firstOf(p: StudyStage => Boolean) = stages.indexWhere(p)
    val positions                         = Vector(
      "estimation"    -> 2,
      "comparison"    -> (firstOf(_.isInstanceOf[StudyStage.Comparing]) + 1),
      "reduction"     -> (firstOf(_.isInstanceOf[StudyStage.Reducing]) + 1),
      "before commit" -> (steps.size - 1)
    )
    runner.events(work, quanta = finest).compile.toVector.flatMap { events =>
      val progress = progressOf(events)
      positions.traverse_ { case (where, completedSteps) =>
        assert(completedSteps >= 1 && completedSteps < steps.size, clue(where))
        cancelAfter(work, finest, completedSteps).map { case (outcome, observed, begun) =>
          val last = progress(completedSteps - 1)
          assertEquals(outcome, StudyOutcome.Cancelled(last.run, Some(last)), where)
          assertEquals(begun, completedSteps + 1, where)
          assertEquals(observed, Vector(last), where)
          // The step the cancellation interrupted was never started: its stage is
          // the pure sequence's next one, and no later progress exists.
          assertEquals(progress(completedSteps).stage, stages(completedSteps), where)
          outcome match
            case StudyOutcome.Completed(_, _, _) => fail(s"$where published a result")
            case _                               => ()
        }
      }
    } >> IO {
      // The positions exercise the stages the ticket names.
      assertEquals(stages(2), StudyStage.Estimating(0, 2))
      assertEquals(stages(positions(1)._2), StudyStage.Comparing(0, StudyDesign.Matched))
      assertEquals(stages(positions(2)._2), StudyStage.Reducing(0, StudyDesign.Matched))
      assertEquals(stages(positions(3)._2), StudyStage.Contrasting(0))
    }
  }

  test("interrupting the pull stream ends it between steps without a terminal event") {
    runner.events(work, quanta = finest).take(3).compile.toVector.map { events =>
      assertEquals(
        events.map(_.isInstanceOf[StudyEvent.Advanced[?, ?, ?, ?]]),
        Vector.fill(3)(true)
      )
      assertEquals(progressOf(events).map(_.step), Vector(1L, 2L, 3L))
    }
  }

  // -------------------------------------------------------------------------
  // Failure is a typed outcome; a defect is raised
  // -------------------------------------------------------------------------

  test(
    "a defect thrown inside a step is raised from the outcome, not hidden as a hang or a cancel"
  ) {
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
    val broken = get(plan(method = throwing).prepare(input))
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
      // Every trial was estimated and the first schedule page was visited before
      // the first whole comparison threw; observers end on that committed step.
      assertEquals(
        observed.lastOption.map(p => (p.step, p.stage)),
        Some((input.trials.rows.size + 1L, StudyStage.Comparing(0, StudyDesign.Matched)))
      )
  }

  test("a refused comparison budget settles Failed with the PlanError and no progress") {
    val budget = get(ComparisonBudget.of(3))
    val error  = PlanError.ComparisonWork(ComparisonWorkError.WorkBudget("cosine", 4, 3))
    val id     = StudyRunId.of(work, finest)
    for
      events <- runner.events(work, budget, finest).compile.toVector
      result <- TestControl.executeEmbed {
        runner.start(work, budget, finest).use { run =>
          run.outcome.product(run.progress.compile.toVector)
        }
      }
    yield
      assertEquals(events, Vector(StudyEvent.Finished(StudyOutcome.Failed(id, error, None))))
      assertEquals(result._1, StudyOutcome.Failed(id, error, None))
      assertEquals(result._2, Vector.empty)
  }

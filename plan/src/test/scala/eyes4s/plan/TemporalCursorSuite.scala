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
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** The temporal cursor against `TemporalStudyPlan.run`, and its composition
  * from per-cell study cursors.
  */
class TemporalCursorSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  private val frame = get(Frame.screen("temporal-cursor", 2, 2))
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

  /** Fixations of 1000 microseconds at onsets 0, 1000, 2000, ...: the second
    * one straddles the 1500 boundary between the windows below.
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
    trial(lone, (0.5, 0.5))
  )
  private val study = StudyInput(Trials(rows))

  private def epoch(key: StudyKey, anchor: Long = 0L, until: Long = 3000L) =
    key -> TrialEpoch(
      Instant.micros(anchor),
      get(ObservedCoverage.of(clock(key), Vector(interval(key, 0L, until))))
    )

  /** Every trial but `lone` has an epoch; `br` is anchored where any window
    * past 100 microseconds overflows; `b` observed only its first half.
    */
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
  private def base(input: TemporalStudyInput[StudyKey, Px]) = get(
    StudyPlan.of(
      input.study.reference,
      StudyKey.layout(DefinitionId.studyLayout),
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned()),
      FailurePolicy.RequireAll,
      cosine,
      ()
    )
  )
  private def window(name: String, from: Long, until: Long) =
    get(StudyWindow.of(name, get(Window.of(Span.micros(from), Span.micros(until)))))
  private val windows =
    Vector(window("early", 0, 1500), window("late", 1500, 3000), window("empty", 5000, 6000))
  private val repetitions = Vector(
    get(RepetitionContrast.withinParticipant("recall", "recall", "encode")),
    get(RepetitionContrast.withinParticipant("reversed", "encode", "recall"))
  )
  private def plan(input: TemporalStudyInput[StudyKey, Px]) =
    get(
      TemporalStudyPlan.of(
        base(input),
        input.reference,
        windows,
        repetitions,
        FixationBoundary.ClipDuration
      )
    )

  private type Result = TemporalStudyResult[StudyKey, Px, Unit, Similarity, SignedDifference]
  private type Cursor = TemporalCursor[StudyKey, Px, Unit, Similarity, SignedDifference]

  private val finest = WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))
  private val quanta =
    Vector(
      finest,
      WorkQuanta(get(PairQuantum.of(2)), get(ComparisonQuantum.of(3))),
      WorkQuanta.default
    )

  private def steps(
      cursor: Cursor,
      quanta: WorkQuanta
  ): (Vector[(TemporalStage, Int)], Result) =
    val trace = Vector.newBuilder[(TemporalStage, Int)]
    @annotation.tailrec
    def loop(cursor: Cursor): Result =
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
            assertEquals(m.source.rows: Any, n.source.rows: Any, clue)
            assertEquals(m.source.provenance, n.source.provenance, clue)
            assertEquals(
              m.evaluation.specification.map(s => (s.method, s.revision, s.parameters)),
              n.evaluation.specification.map(s => (s.method, s.revision, s.parameters)),
              clue
            )
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

  test(
    "the cursor at every quantum is plan.run over straddlers, empty windows, missing epochs and overflow"
  ) {
    Vector(edges -> "edges", clean -> "clean").foreach { case (input, where) =>
      val p        = plan(input)
      val expected = get(p.run(input))
      assertEquals(
        expected.cells.map(c => (c.repetition.name, c.window.name)),
        for r <- repetitions; w <- windows yield (r.name, w.name)
      )
      val work = get(p.prepare(input))
      quanta.foreach { q =>
        val (_, result) = steps(get(work.work()), q)
        assertSameResult(result, expected, (where, q))
        val (again, _) = steps(get(work.work()), q)
        assertEquals(steps(get(work.work()), q)._1, again, (where, q))
      }
    }
    // The edge fixture retains its data-level facts as typed failures per trial.
    val early = get(plan(edges).run(edges)).cells.head
    assertEquals((early.repetition.name, early.window.name), ("recall", "early"))
    val byKey = early.occupancy.toMap
    assert(
      byKey(lone).left.exists(_.isInstanceOf[TemporalStudyError.MissingEpoch]),
      clue(byKey(lone))
    )
    assert(
      byKey(br).left.exists(_.isInstanceOf[TemporalStudyError.AnchorOverflow]),
      clue(byKey(br))
    )
    // `a`'s second fixation straddles 1500 and is clipped to its first half.
    assertEquals(get(byKey(a)).fixationTimes.map(_.retainedMicros), Vector(1000L, 500L, 0L))
    assertEquals(get(byKey(b)).missingMicros, 0L)
    val late = get(plan(edges).run(edges)).cells(1)
    assertEquals(late.window.name, "late")
    // `b` observed only [0, 1500): the late window is entirely missing coverage.
    assertEquals(get(late.occupancy.toMap.apply(b)).missingMicros, 1500L)
    val empty = get(plan(edges).run(edges)).cells(2)
    assertEquals(empty.window.name, "empty")
    assertEquals(get(empty.occupancy.toMap.apply(a)).retainedMicros, 0L)
  }

  test("the step sequence is every cell's preparation then its study cursor's steps, stamped") {
    val p    = plan(clean)
    val work = get(p.prepare(clean))
    quanta.foreach { q =>
      val (trace, _) = steps(get(work.work()), q)
      val expected   = for
        (repetition, r) <- work.repetitions.zipWithIndex
        w               <- windows.indices
      yield
        val occupancy = rows.indices.map(t => TemporalWork.occupancy(work, w, t)).toVector
        val cursor    =
          get(TemporalWork.study(work, ComparisonBudget.default, repetition, w, occupancy))
        val inner = Vector.newBuilder[(StudyStage, Int)]
        @annotation.tailrec
        def loop(cursor: StudyCursor[StudyKey, Px, Similarity, SignedDifference]): Unit =
          val stage = cursor.stage
          get(cursor.advance(q)) match
            case StudyStep.More(_, units, next) =>
              inner += stage -> units
              loop(next)
            case StudyStep.Done(units, _) => inner += stage -> units
        loop(cursor)
        rows.indices.map(t => TemporalStage.Preparing(r, w, t) -> 1).toVector ++
          inner.result().map { case (stage, units) =>
            TemporalStage.Studying(r, w, stage) -> units
          }
      assertEquals(trace, expected.flatten, q)
    }
  }

  test(
    "preparation refuses a wrong or missing input before any step; a budget refusal follows the first cell's preparation"
  ) {
    val p = plan(clean)
    assertEquals(
      p.prepare(edges).swap.map(_.getClass).toOption,
      p.run(edges).swap.map(_.getClass).toOption
    )
    p.prepare(edges) match
      case Left(TemporalStudyError.Input(PlanError.ArtifactMismatch(_, _))) => ()
      case other => fail(s"expected an artifact mismatch, got $other")
    val work   = get(p.prepare(clean))
    val budget = get(ComparisonBudget.of(3))
    var cursor = get(work.work(budget))
    rows.indices.init.foreach { t =>
      assertEquals(cursor.stage, TemporalStage.Preparing(0, 0, t))
      cursor = get(cursor.advance(finest)) match
        case WorkStep.More(_, 1, next) => next
        case other                     => fail(s"unexpected $other")
    }
    assertEquals(cursor.stage, TemporalStage.Preparing(0, 0, rows.size - 1))
    assertEquals(
      cursor.advance(finest).swap.toOption,
      Some(
        TemporalStudyError.Input(
          PlanError.ComparisonWork(ComparisonWorkError.WorkBudget("cosine", 4, 3))
        )
      )
    )
  }

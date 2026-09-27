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
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy

class CountedStudyExecutionSuite extends munit.CatsEffectSuite:
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

  private val finest = WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))

  private type Progress = RunProgress[String, StudyRunStage, StudyRunSegment]

  private def checkMeters(progress: Vector[Progress], expected: Vector[Long]): Unit =
    val running = progress.collect {
      case p @ RunProgress(_, _, StudyRunStage.Running(_, _), _, _, _, _, _) => p
    }
    val meters = running.map(_.stage).collect { case StudyRunStage.Running(_, meter) => meter }
    assertEquals(meters.map(_.kind).distinct.toSet, StageKind.values.toSet)
    StageKind.values.toVector.zip(CountUnit.values.toVector).zip(expected).foreach {
      case ((kind, unit), count) =>
        val same = meters.filter(_.kind == kind)
        assert(same.forall(_.unit == unit))
        assertEquals(same.map(_.done), same.map(_.done).sorted)
        assertEquals(same.last.done, count, clue(kind))
        val total = if kind == StageKind.Contrasting then SegmentTotal.AtMost(count)
        else SegmentTotal.Exact(count)
        assert(same.forall(_.total == total), clue((kind, same.map(_.total))))
    }

  test(
    "one submission counts then executes with committed global meters including final Done"
  ) {
    val two = get(
      plan(scales =
        Vector(
          StudyEstimate.Binned(),
          StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)
        )
      ).prepare(input)
    )
    Execution[IO]
      .events(StudyExecution.submissionWithId("counted", two, quanta = finest))
      .compile
      .toVector
      .map { events =>
        val progress            = events.collect { case RunEvent.Advanced(p) => p }
        val (counting, running) = progress.span(_.segment == StudyRunSegment.Counting)
        assert(counting.nonEmpty)
        assert(running.nonEmpty)
        assert(running.forall(_.segment != StudyRunSegment.Counting))
        assert(counting.forall(_.segmentTotal == SegmentTotal.Counting))
        assert(counting.forall(_.stepUnits <= 1))
        assertEquals(events.count { case RunEvent.Finished(_) => true; case _ => false }, 1)
        assertEquals(progress.map(_.step), (1L to progress.size.toLong).toVector)
        checkMeters(progress, Vector(10L, 8L, 8L, 4L))
        val comparisons = running.collect {
          case p @ RunProgress(
                _,
                _,
                StudyRunStage.Running(StudyStage.Comparing(_, _), _),
                _,
                _,
                _,
                _,
                _
              ) =>
            p
        }
        assert(
          comparisons.zip(comparisons.drop(1)).exists { case (before, after) =>
            (before.stage, after.stage) match
              case (StudyRunStage.Running(_, a), StudyRunStage.Running(_, b)) =>
                before.segment == after.segment && a.done == b.done && after.totalUnits > before.totalUnits
              case _ => false
          },
          "comparison microsteps must not increment the completed-pair meter"
        )
        events.last match
          case RunEvent.Finished(RunOutcome.Completed("counted", last, result)) =>
            assertEquals(last, progress.last)
            assertEquals(result.scales.size, 2)
            last.stage match
              case StudyRunStage.Running(StudyStage.Contrasting(1), meter) =>
                assertEquals(meter.done, 4L)
              case other => fail(s"unexpected terminal stage $other")
          case other => fail(s"unexpected final event $other")
      }
  }

  test("empty schedules traverse counting and finish with zero object meters") {
    val empty    = StudyInput(Trials(Vector.empty[Trial[StudyKey, Unit, Scanpath[Px]]]))
    val prepared = get(plan(empty).prepare(empty))
    Execution[IO]
      .events(StudyExecution.submissionWithId("empty", prepared, quanta = finest))
      .compile
      .toVector
      .map { events =>
        val progress = events.collect { case RunEvent.Advanced(p) => p }
        assert(progress.exists(_.segment == StudyRunSegment.Counting))
        val meters =
          progress.map(_.stage).collect { case StudyRunStage.Running(_, meter) => meter }
        assert(meters.nonEmpty)
        assert(meters.forall(_.done == 0L))
        assert(
          meters.forall(m =>
            m.total == SegmentTotal.Exact(0L) || m.total == SegmentTotal.AtMost(0L)
          )
        )
        events.last match
          case RunEvent.Finished(RunOutcome.Completed(_, last, result)) =>
            assertEquals(last, progress.last)
            assertEquals(result.scales.head.estimation.size, 0)
          case other => fail(s"unexpected empty outcome $other")
      }
  }

  test(
    "cancellation at the counting barrier never starts scientific work or produces a result"
  ) {
    TestControl.executeEmbed {
      var starts   = 0
      val observed = new StudyMethod[Unit, Px, Similarity, SignedDifference](
        DefinitionId.cosine,
        "observed",
        _ => Vector.empty,
        MethodExecution.Bounded(_ => { starts += 1; Distribution.cosine[Px] }),
        None
      )
      val prepared = get(plan(method = observed).prepare(input))
      for
        calls   <- Ref.of[IO, Int](0)
        reached <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        between = calls.getAndUpdate(_ + 1).flatMap { n =>
          if n == 1 then reached.complete(()).void >> release.get else IO.unit
        }
        _ <- Execution[IO]
          .start(StudyExecution.submissionWithId("cancel", prepared, quanta = finest), between)
          .use { run =>
            reached.get >> run.cancel >> run.outcome.map {
              case RunOutcome.Cancelled("cancel", Some(last)) =>
                assertEquals(last.step, 1L)
                assertEquals(last.segment, StudyRunSegment.Counting)
                assertEquals(last.stepUnits, 1)
                assertEquals(starts, 0)
              case other => fail(s"unexpected cancellation outcome $other")
            }
          }
      yield ()
    }
  }

  test(
    "cancellation during diagnostic assembly returns counting progress and starts no science"
  ) {
    TestControl.executeEmbed {
      var starts   = 0
      val observed = new StudyMethod[Unit, Px, Similarity, SignedDifference](
        DefinitionId.cosine,
        "observed",
        _ => Vector.empty,
        MethodExecution.Bounded(_ => { starts += 1; Distribution.cosine[Px] }),
        None
      )
      val source = StudyInput(
        Trials(
          (0 until 20).reverse.toVector.map(i =>
            trial(StudyKey("p", s"item-$i", "recall"), (0.5, 0.5))
          )
        )
      )
      val configured = get(
        StudyPlan.configure(
          source.reference,
          StudyKey.layout(DefinitionId.studyLayout),
          StudyGeometry.WholeFrame(grid),
          "recall",
          "encode",
          Weight.Duration,
          Vector(StudyScale.Native(StudyEstimate.Binned[Px]())),
          None,
          FailurePolicy.RequireAll,
          observed,
          (),
          StudyPairing.default.copy(unmatched = UnmatchedFocalPolicy.Refuse)
        )
      )
      val prepared = get(configured.prepare(source))
      @annotation.tailrec
      def barrier(cursor: CountCursor[StudyKey], steps: Int): (Int, Long) =
        get(cursor.advance(finest)) match
          case WorkStep.More(_, units, next) =>
            if units > 0 && next.visited == cursor.visited then (steps + 1, next.visited)
            else barrier(next, steps + 1)
          case WorkStep.Done(_, _) => fail("no bounded diagnostic page")
      val (afterDiagnostic, visited) = barrier(get(prepared.countWork), 0)
      for
        calls   <- Ref.of[IO, Int](0)
        reached <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        between = calls.getAndUpdate(_ + 1).flatMap { n =>
          if n == afterDiagnostic then reached.complete(()).void >> release.get else IO.unit
        }
        _ <- Execution[IO]
          .start(
            StudyExecution.submissionWithId("diagnostic", prepared, quanta = finest),
            between
          )
          .use { run =>
            reached.get >> run.cancel >> run.outcome.map {
              case RunOutcome.Cancelled("diagnostic", Some(last)) =>
                assertEquals(last.step, afterDiagnostic.toLong)
                assertEquals(last.stage, StudyRunStage.Counting(StudyDesign.Matched, visited))
                assertEquals(starts, 0)
              case other => fail(s"unexpected diagnostic cancellation $other")
            }
          }
      yield ()
    }
  }

  test("duplicate focal keys and failed maps retain attempted object counts") {
    val source   = StudyInput(Trials(input.trials.rows :+ trial(a, (3.5, 3.5))))
    val prepared = get(plan(source).prepare(source))
    Execution[IO]
      .events(StudyExecution.submissionWithId("duplicate", prepared, quanta = finest))
      .compile
      .toVector
      .map { events =>
        val progress = events.collect { case RunEvent.Advanced(p) => p }
        checkMeters(progress, Vector(6L, 2L, 4L, 2L))
        events.last match
          case RunEvent.Finished(RunOutcome.Completed(_, _, result)) =>
            assert(result.scales.head.estimation.exists(_._2.isLeft))
            assertEquals(result.scales.head.analyses.matched.entries.size, 2)
          case other => fail(s"unexpected duplicate-key outcome $other")
      }
  }

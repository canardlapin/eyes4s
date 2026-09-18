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
import cats.effect.kernel.{Outcome, Resource}
import cats.effect.testkit.TestControl
import cats.syntax.all.*
import eyes4s.design.WorkQuanta
import eyes4s.plan.{SegmentTotal, Stepwise, WorkStep}

import _root_.fs2.{Chunk, Pull, Stream}
import _root_.fs2.concurrent.SignallingRef

/** The runner-side half of the execution contract, stated over a synthetic
  * stepwise cursor so the laws are about the runner alone: `events` ends in
  * exactly one terminal event; a cancellation that lands before the deciding
  * step settles `Cancelled`, which carries no result; cancellation is
  * observed only between steps, so the steps the cursor took are exactly the
  * steps the outcome reports; the first commit wins; a defect thrown by the
  * work surfaces from both entry points. The pure half (determinism, cut
  * invariance, totals) is published in `eyes4s-laws`; the family suites
  * check both halves on the shipped fixtures.
  *
  * Each law is a function of a runner. The shipped [[Execution]] must satisfy
  * every law; so must a control re-implementation with no fault; and each
  * mutant runner, the control with one injected fault, must be observed to
  * fail the law that names it.
  *
  * ==Mutation execution receipts==
  *
  * Observed on both JVM and Scala.js (`fs2ModuleJVM/test`, `fs2ModuleJS/test`).
  *
  * {{{
  * | mutant           | injected fault                                              | killed by                                                    |
  * |------------------|-------------------------------------------------------------|--------------------------------------------------------------|
  * | double-terminal  | the pull stream emits Finished twice                        | single terminal event                                        |
  * | eager-commit     | the runner peeks the next step and commits Done a step early| Cancelled never carries a result, cancellation between steps |
  * | cancelable-step  | the between hook runs after advance, outside uncancelable   | cancellation is observed only between steps                  |
  * | last-commit-wins | settling after Completed overwrites the outcome             | first commit wins, a defect surfaces from both paths         |
  * | swallowed-defect | a thrown defect settles Cancelled instead of raising        | a defect surfaces from both paths                            |
  * }}}
  *
  * last-commit-wins also fails the defect law for the same reason: the
  * resource release settles `Cancelled` after the defect was committed, and
  * under this fault the later settlement replaces the raised defect.
  */
class ExecutionConformanceSuite extends munit.CatsEffectSuite:

  /** A defect, distinct from every typed failure. */
  private final class Broken extends RuntimeException("broken step")

  /** What a synthetic step may do besides advancing. */
  private enum Fault derives CanEqual:
    case None
    case Defect(at: Int)
    case Failure(at: Int)

  /** `steps` steps of one unit each; stage is the step index, the result is
    * the step count. Faults fire when the step at `at` is taken.
    */
  private final case class Tick(index: Int, steps: Int, fault: Fault)

  /** The stepwise instance with its advances counted, so a law can compare
    * the steps the cursor took against the steps the outcome reports.
    */
  private final class Counting:
    var advances                                   = 0
    val stepwise: Stepwise[Tick, Int, String, Int] = new Stepwise[Tick, Int, String, Int]:
      def stage(tick: Tick): Int = tick.index
      def advance(tick: Tick, quanta: WorkQuanta): Either[String, WorkStep[Int, Tick, Int]] =
        advances += 1
        tick.fault match
          case Fault.Defect(at) if at == tick.index  => throw new Broken
          case Fault.Failure(at) if at == tick.index => Left(s"failed at ${tick.index}")
          case _                                     =>
            if tick.index + 1 < tick.steps then
              Right(WorkStep.More(tick.index, 1, tick.copy(index = tick.index + 1)))
            else Right(WorkStep.Done(1, tick.steps))

  private type Work    = Submission[String, Tick, Int, Int, String, Int]
  private type Event   = RunEvent[String, Int, Int, String, Int]
  private type Outcome = RunOutcome[String, Int, Int, String, Int]
  private type Handle  = Run[IO, String, Int, Int, String, Int]

  /** Three stages per segment, every segment bounded by its three units. */
  private def submission(counting: Counting, steps: Int, fault: Fault = Fault.None): Work =
    new Submission[String, Tick, Int, Int, String, Int](
      "tick",
      WorkQuanta.default,
      () => Right(Tick(0, steps, fault)),
      (stage: Int) => stage / 3,
      (_, _) => SegmentTotal.AtMost(3L)
    )(using counting.stepwise)

  // -------------------------------------------------------------------------
  // The runner surface the laws are stated over
  // -------------------------------------------------------------------------

  private trait Runner:
    def events(work: Work): Stream[IO, Event]
    def start(work: Work, between: IO[Unit]): Resource[IO, Handle]

  private object Shipped extends Runner:
    private val execution                                          = Execution[IO]
    def events(work: Work): Stream[IO, Event]                      = execution.events(work)
    def start(work: Work, between: IO[Unit]): Resource[IO, Handle] =
      execution.start(work, between)

  private enum RunnerFault derives CanEqual:
    case None, DoubleTerminal, EagerCommit, CancelableStep, LastCommitWins, SwallowedDefect

  /** The runner's bookkeeping, re-stated here because the shipped one is private. */
  private final case class Book(
      cursor: Tick,
      step: Long,
      segment: Option[Int],
      segmentUnits: Long,
      totalUnits: Long,
      last: Option[RunProgress[String, Int, Int]]
  )

  private def bookStep(work: Work, book: Book): (Vector[Event], Option[Book]) =
    def record(stage: Int, units: Int): (RunProgress[String, Int, Int], Book) =
      val segment      = work.segment(stage)
      val segmentUnits =
        if book.segment.contains(segment) then book.segmentUnits + units else units.toLong
      val progress = RunProgress(
        work.id,
        book.step + 1,
        stage,
        units,
        segment,
        segmentUnits,
        work.total(segment, book.cursor),
        book.totalUnits + units
      )
      progress -> book.copy(
        step = progress.step,
        segment = Some(segment),
        segmentUnits = segmentUnits,
        totalUnits = progress.totalUnits,
        last = Some(progress)
      )
    work.stepwise.advance(book.cursor, work.quanta) match
      case Left(error) =>
        (Vector(RunEvent.Finished(RunOutcome.Failed(work.id, error, book.last))), None)
      case Right(WorkStep.More(stage, units, next)) =>
        val (progress, advanced) = record(stage, units)
        (Vector(RunEvent.Advanced(progress)), Some(advanced.copy(cursor = next)))
      case Right(WorkStep.Done(units, result)) =>
        val (progress, _) = record(work.stepwise.stage(book.cursor), units)
        (
          Vector(
            RunEvent.Advanced(progress),
            RunEvent.Finished(RunOutcome.Completed(work.id, progress, result))
          ),
          None
        )

  private final case class Slot(last: Option[RunProgress[String, Int, Int]], terminal: Boolean)

  /** A re-implementation of the shipped runner with one switchable fault;
    * with `RunnerFault.None` it is the control and must pass every law.
    */
  private final class Mutant(fault: RunnerFault) extends Runner:
    def events(work: Work): Stream[IO, Event] =
      def go(book: Book): Pull[IO, Event, Unit] =
        Pull.eval(IO.cede >> IO.uncancelable(_ => IO(bookStep(work, book)))).flatMap {
          case (events, next) =>
            val terminal = events.collect { case f @ RunEvent.Finished(_) => f }
            val output   =
              if fault == RunnerFault.DoubleTerminal then events ++ terminal else events
            Pull.output(Chunk.from(output)) >> next.fold(Pull.done)(go)
        }
      Pull
        .eval(IO(work.begin()))
        .flatMap {
          case Left(error) =>
            Pull.output1(RunEvent.Finished(RunOutcome.Failed(work.id, error, None)))
          case Right(cursor) => go(Book(cursor, 0L, None, 0L, 0L, None))
        }
        .stream

    def start(work: Work, between: IO[Unit]): Resource[IO, Handle] =
      Resource
        .eval(
          (
            Deferred[IO, Either[Throwable, Outcome]],
            Ref[IO].of(Option.empty[Either[Throwable, Outcome]]),
            SignallingRef[IO].of(Slot(None, false))
          ).tupled
        )
        .flatMap { case (deferred, overwritten, signal) =>
          def commit(outcome: Either[Throwable, Outcome]): IO[Unit] =
            if fault == RunnerFault.LastCommitWins then
              overwritten.set(Some(outcome)) >> deferred.complete(outcome).void
            else deferred.complete(outcome).void

          val read: IO[Outcome] =
            if fault == RunnerFault.LastCommitWins then
              deferred.get >> overwritten.get.map(_.get).rethrow
            else deferred.get.rethrow

          def publish(event: Event): IO[Unit] = event match
            case RunEvent.Advanced(progress) => signal.set(Slot(Some(progress), false))
            case RunEvent.Finished(outcome)  =>
              commit(Right(outcome)) >> signal.update(_.copy(terminal = true))

          def loop(book: Book): IO[Unit] = fault match
            case RunnerFault.CancelableStep =>
              IO(bookStep(work, book)).flatMap { case (events, next) =>
                between >> events.traverse_(publish) >> next.fold(IO.unit)(loop)
              }
            case RunnerFault.EagerCommit =>
              between >> IO
                .uncancelable(_ =>
                  IO(bookStep(work, book)).flatMap { case (events, next) =>
                    events.traverse_(publish) >> next
                      .traverse { advanced =>
                        IO(bookStep(work, advanced)).flatMap {
                          case (peeked, None) =>
                            peeked.traverse_(publish).as(Option.empty[Book])
                          case _ => IO.pure(Some(advanced))
                        }
                      }
                      .map(_.flatten)
                  }
                )
                .flatMap(_.fold(IO.unit)(loop))
            case _ =>
              between >> IO
                .uncancelable(_ =>
                  IO(bookStep(work, book)).flatMap { case (events, next) =>
                    events.traverse_(publish).as(next)
                  }
                )
                .flatMap(_.fold(IO.unit)(loop))

          val settle: IO[Unit] =
            signal.get
              .flatMap(slot => commit(Right(RunOutcome.Cancelled(work.id, slot.last)))) >>
              signal.update(_.copy(terminal = true))

          val body: IO[Unit] =
            IO.uncancelable(_ => IO(work.begin()))
              .flatMap {
                case Left(error) =>
                  publish(RunEvent.Finished(RunOutcome.Failed(work.id, error, None)))
                case Right(cursor) => loop(Book(cursor, 0L, None, 0L, 0L, None))
              }
              .guaranteeCase {
                case Outcome.Canceled()      => settle
                case Outcome.Errored(defect) =>
                  if fault == RunnerFault.SwallowedDefect then settle
                  else commit(Left(defect)) >> signal.update(_.copy(terminal = true))
                case Outcome.Succeeded(_) => IO.unit
              }

          val progress =
            signal.discrete
              .takeThrough(!_.terminal)
              .map(_.last)
              .unNone
              .filterWithPrevious((previous, next) => next.step > previous.step)

          Resource
            .make(body.start)(fiber => fiber.cancel >> settle)
            .map(fiber =>
              new Run(work.id, progress, fiber.join >> settle >> read, fiber.cancel >> settle)
            )
        }

  // -------------------------------------------------------------------------
  // The laws
  // -------------------------------------------------------------------------

  private val steps = 7

  /** Cancel while the runner is parked at its between-step hook before step
    * `completed + 1`, or as soon as the run settles on its own, and return
    * the outcome with the advances the cursor took.
    */
  private def cancelAt(runner: Runner, completed: Int): IO[(Outcome, Int)] =
    TestControl.executeEmbed {
      for
        started <- Ref[IO].of(0)
        reached <- Deferred[IO, Unit]
        parked  <- Deferred[IO, Unit]
        between = started.updateAndGet(_ + 1).flatMap { n =>
          if n == completed + 1 then reached.complete(()) >> parked.get else IO.unit
        }
        counting = new Counting
        result <- runner.start(submission(counting, steps), between).use { run =>
          reached.get.race(run.outcome.void) >> run.cancel >> run.outcome
            .map(o => (o, counting.advances))
        }
      yield result
    }

  private def singleTerminalEvent(runner: Runner): IO[Unit] =
    Vector(Fault.None -> steps, Fault.Failure(3) -> 3).traverse_ { (fault, advanced) =>
      runner.events(submission(new Counting, steps, fault)).compile.toVector.map { events =>
        val finished = events.collect { case RunEvent.Finished(outcome) => outcome }
        assertEquals(finished.size, 1, s"$fault: ${events.size} events")
        assertEquals(events.last, RunEvent.Finished(finished.head), s"$fault")
        assertEquals(events.count(_.isInstanceOf[RunEvent.Advanced[?, ?, ?, ?, ?]]), advanced)
        (fault, finished.head) match
          case (Fault.None, RunOutcome.Completed(_, last, result)) =>
            assertEquals((last.step, result), (steps.toLong, steps))
          case (Fault.Failure(at), RunOutcome.Failed(_, error, last)) =>
            assertEquals((error, last.map(_.step)), (s"failed at $at", Some(at.toLong)))
          case other => fail(s"unexpected terminal $other")
      }
    }

  private def cancelledNeverCarriesResult(runner: Runner): IO[Unit] =
    (0 until steps).toVector.traverse_ { completed =>
      cancelAt(runner, completed).map { (outcome, _) =>
        outcome match
          case RunOutcome.Cancelled(_, last) =>
            assertEquals(last.map(_.step), Option.when(completed > 0)(completed.toLong))
          case other => fail(s"cancelling before step ${completed + 1} settled $other")
      }
    }

  private def cancellationBetweenSteps(runner: Runner): IO[Unit] =
    (0 until steps).toVector.traverse_ { completed =>
      cancelAt(runner, completed).map { (outcome, advances) =>
        assertEquals(advances, completed, s"advances taken before step ${completed + 1}")
        assertEquals(
          outcome.progress.map(_.step).getOrElse(0L),
          advances.toLong,
          "every advance taken is a step the outcome reports"
        )
      }
    }

  private def firstCommitWins(runner: Runner): IO[Unit] =
    TestControl
      .executeEmbed {
        runner.start(submission(new Counting, steps), IO.cede).use { run =>
          for
            first  <- run.outcome
            _      <- run.cancel
            second <- run.outcome
            third  <- run.outcome
          yield (first, second, third)
        }
      }
      .map { (first, second, third) =>
        assert(first eq second, s"cancelling after completion changed $first to $second")
        assert(second eq third, "the outcome settles once")
        first match
          case RunOutcome.Completed(_, last, result) =>
            assertEquals((last.step, result), (steps.toLong, steps))
          case other => fail(s"a run left alone settled $other")
      }

  private def defectSurfaces(runner: Runner): IO[Unit] =
    for
      pulled <- runner
        .events(submission(new Counting, steps, Fault.Defect(4)))
        .compile
        .toVector
        .attempt
      handle <- TestControl.executeEmbed {
        runner
          .start(submission(new Counting, steps, Fault.Defect(4)), IO.cede)
          .use(_.outcome.attempt)
      }
    yield
      assert(pulled.left.exists(_.isInstanceOf[Broken]), s"events: $pulled")
      assert(handle.left.exists(_.isInstanceOf[Broken]), s"start: $handle")

  private val laws: Vector[(String, Runner => IO[Unit])] = Vector(
    "single terminal event"                       -> singleTerminalEvent,
    "Cancelled never carries a result"            -> cancelledNeverCarriesResult,
    "cancellation is observed only between steps" -> cancellationBetweenSteps,
    "first commit wins"                           -> firstCommitWins,
    "a defect surfaces from both paths"           -> defectSurfaces
  )

  laws.foreach { (name, law) =>
    test(s"the shipped runner: $name")(law(Shipped))
    test(s"the control re-implementation: $name")(law(new Mutant(RunnerFault.None)))
  }

  private val kills: Vector[(RunnerFault, Set[String])] = Vector(
    RunnerFault.DoubleTerminal -> Set("single terminal event"),
    RunnerFault.EagerCommit    -> Set(
      "Cancelled never carries a result",
      "cancellation is observed only between steps"
    ),
    RunnerFault.CancelableStep -> Set("cancellation is observed only between steps"),
    RunnerFault.LastCommitWins -> Set("first commit wins", "a defect surfaces from both paths"),
    RunnerFault.SwallowedDefect -> Set("a defect surfaces from both paths")
  )

  test("the segment vocabulary moved to eyes4s-plan stays usable under both wildcard imports") {
    // The fs2 names are aliases of the plan enums, so importing both packages
    // must not make a reference ambiguous, and fs2-only imports keep working.
    assertNoDiff(
      compileErrors(
        """{
          import eyes4s.fs2.*
          import eyes4s.plan.*
          val s: StudySegment = StudySegment.Estimating(0)
          val r: RecordingSegment = RecordingSegment.Detecting
          val t: TemporalSegment = TemporalSegment.Preparing(0, 1)
          val n: SegmentTotal = SegmentTotal.Exact(1L)
          (s, r, t, n) match
            case (StudySegment.Estimating(_), _, TemporalSegment.Preparing(_, _), SegmentTotal.Exact(_)) => ()
            case _ => ()
        }"""
      ),
      ""
    )
    val fromFs2: eyes4s.fs2.StudySegment =
      eyes4s.plan.StudySegment.Reducing(0, eyes4s.plan.StudyDesign.Matched)
    assertEquals(fromFs2, eyes4s.fs2.StudySegment.Reducing(0, eyes4s.plan.StudyDesign.Matched))
  }

  test("every runner mutant fails exactly the laws the receipts name") {
    kills.traverse_ { (fault, killers) =>
      laws
        .traverse { (name, law) =>
          law(new Mutant(fault)).attempt.map(result => name -> result.isLeft)
        }
        .map { observed =>
          val failed = observed.collect { case (name, true) => name }.toSet
          assertEquals(failed, killers, s"$fault")
        }
    }
  }

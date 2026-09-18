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

import cats.effect.kernel.{Concurrent, Deferred, Outcome, Resource}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import eyes4s.design.WorkQuanta
import eyes4s.plan.{Stepwise, WorkStep}

import _root_.fs2.{Chunk, Pull, Stream}
import _root_.fs2.concurrent.SignallingRef

/** The segment-total vocabulary is [[eyes4s.plan.SegmentTotal]], stated in
  * the pure plan module so the published execution laws can check it; these
  * aliases keep `SegmentTotal.Exact`, `AtMost` and `Unknown` available here.
  */
type SegmentTotal = eyes4s.plan.SegmentTotal
val SegmentTotal: eyes4s.plan.SegmentTotal.type = eyes4s.plan.SegmentTotal

/** Telemetry for one completed step. `stepUnits` is the step's own count as
  * the cursor reported it; `segmentUnits` accumulates over the current
  * segment and `totalUnits` over the run. Both are monotone. A zero-unit
  * step is a decided comparison, not a stall.
  */
final case class RunProgress[Id, Stage, Segment](
    run: Id,
    step: Long,
    stage: Stage,
    stepUnits: Int,
    segment: Segment,
    segmentUnits: Long,
    segmentTotal: SegmentTotal,
    totalUnits: Long
) derives CanEqual

/** The one authoritative end of a run. A result exists only inside
  * `Completed`; `Cancelled` and `Failed` carry the last committed progress,
  * which is `None` when no step completed.
  */
enum RunOutcome[Id, Stage, Segment, E, R]:
  case Completed(run: Id, last: RunProgress[Id, Stage, Segment], result: R)
  case Cancelled(run: Id, last: Option[RunProgress[Id, Stage, Segment]])
  case Failed(run: Id, error: E, last: Option[RunProgress[Id, Stage, Segment]])

  def run: Id

  def progress: Option[RunProgress[Id, Stage, Segment]] = this match
    case Completed(_, last, _) => Some(last)
    case Cancelled(_, last)    => last
    case Failed(_, _, last)    => last

/** One element of the pull-driven interpretation, see [[Execution.Runner.events]]. */
enum RunEvent[Id, Stage, Segment, E, R]:
  case Advanced(progress: RunProgress[Id, Stage, Segment])
  case Finished(outcome: RunOutcome[Id, Stage, Segment, E, R])

/** A running submission. `outcome` is authoritative and settles exactly
  * once; `progress` is telemetry that may end without the terminal step.
  */
final class Run[F[_], Id, Stage, Segment, E, R] private[fs2] (
    val id: Id,
    val progress: Stream[F, RunProgress[Id, Stage, Segment]],
    val outcome: F[RunOutcome[Id, Stage, Segment, E, R]],
    val cancel: F[Unit]
)

/** Everything the runner needs from one plan family: the deterministic id of
  * this submission, the quanta that cut its steps, how to begin its cursor,
  * which counted segment each stage belongs to and what each segment's total
  * is, plus the [[Stepwise]] evidence that the cursor is step-shaped. The
  * family objects build these; see `StudyExecution.submission`.
  *
  * `total` is asked once per segment, as the segment begins, with the cursor
  * about to take its first step, so a family can state a total only that
  * cursor knows (a reduction's, once its scores are realised) and the runner
  * reports one total for the whole segment.
  */
final class Submission[Id, C, Stage, Segment, E, R](
    val id: Id,
    val quanta: WorkQuanta,
    val begin: () => Either[E, C],
    val segment: Stage => Segment,
    val total: (Segment, C) => SegmentTotal
)(using val stepwise: Stepwise[C, Stage, E, R])

/** Interpret any stepwise cursor under Cats Effect: the one runner every plan
  * family shares. A family's wrapper (`StudyExecution`, `RecordingExecution`,
  * `TemporalExecution`) only builds the [[Submission]].
  *
  * Both entry points drive the same cursor the family's pure `run` drives, so
  * a completed result is the pure runner's result. Each step is one
  * `advance(quanta)` followed by its bookkeeping, evaluated as a single
  * uncancelable region; the fiber cedes before every step, so cancellation
  * lands between steps and never inside one.
  *
  * Scheduling policy, thread dispatch and widgets belong to the consumer.
  */
object Execution:

  /** Fix the effect type once: `Execution[IO].start(submission)`. */
  def apply[F[_]](using F: Concurrent[F]): Runner[F] = new Runner[F]

  final class Runner[F[_]](using F: Concurrent[F]):

    /** The deterministic step sequence as a stream. Pulling performs the work:
      * every `Advanced` is one step, and the stream ends with exactly one
      * `Finished` carrying `Completed` or `Failed`. Interrupting the stream
      * ends it between steps without a terminal event; there is no commit
      * point on this path, so an interrupted pull observes nothing
      * authoritative. Use [[start]] when the outcome must be observable after
      * cancellation.
      */
    def events[Id, C, Stage, Segment, E, R](
        work: Submission[Id, C, Stage, Segment, E, R]
    ): Stream[F, RunEvent[Id, Stage, Segment, E, R]] =
      val interpreter = new Interpreter(work)
      def go(state: interpreter.State): Pull[F, RunEvent[Id, Stage, Segment, E, R], Unit] =
        Pull
          .eval(F.cede >> F.uncancelable(_ => F.catchNonFatal(interpreter.step(state))))
          .flatMap { case (events, next) =>
            Pull.output(Chunk.from(events)) >> next.fold(Pull.done)(go)
          }
      Pull
        .eval(F.catchNonFatal(interpreter.begin))
        .flatMap {
          case Left(failed) => Pull.output1(RunEvent.Finished(failed))
          case Right(state) => go(state)
        }
        .stream

    /** Run a submission on its own fiber with an authoritative outcome.
      *
      * `outcome` settles exactly once, at the single commit point: `Completed`
      * or `Failed` is committed inside the step that decides it; releasing
      * the resource or calling `cancel` cancels the fiber, and a cancellation
      * that lands before that commit settles `Cancelled` with the last
      * completed step. The first commit wins; a cancellation arriving after
      * `Completed` changes nothing. A defect thrown by the work is raised
      * from `outcome`. `outcome` waits for the fiber to terminate, so it is
      * settled even when cancellation won before the fiber ever ran.
      *
      * `progress` coalesces: one slot holds the latest completed step, so an
      * observer never slows or blocks the run, and an observer that keeps up
      * sees every step; a slow observer sees the latest step when it resumes;
      * every observer, including one that subscribes late, receives the last
      * step and then ends once the run has settled. Observers dispose by
      * ending their stream.
      *
      * `between` runs before each step, outside the uncancelable region; the
      * default `cede` is the cooperative yield. A barrier here is how tests
      * cancel at an exact step.
      */
    def start[Id, C, Stage, Segment, E, R](
        work: Submission[Id, C, Stage, Segment, E, R],
        between: F[Unit] = F.cede
    ): Resource[F, Run[F, Id, Stage, Segment, E, R]] =
      type Progress = RunProgress[Id, Stage, Segment]
      type Outcome  = RunOutcome[Id, Stage, Segment, E, R]
      val interpreter = new Interpreter(work)
      val id          = work.id
      Resource
        .eval(
          (
            Deferred[F, Either[Throwable, Outcome]],
            SignallingRef[F].of(Published[Progress](None, false))
          ).tupled
        )
        .flatMap { case (outcome, signal) =>
          def publish(event: RunEvent[Id, Stage, Segment, E, R]): F[Unit] = event match
            case RunEvent.Advanced(progress) => signal.set(Published(Some(progress), false))
            case RunEvent.Finished(result)   =>
              outcome.complete(Right(result)) >> signal.update(_.settle)

          def loop(state: interpreter.State): F[Unit] =
            between >> F
              .uncancelable(_ =>
                F.catchNonFatal(interpreter.step(state)).flatMap { case (events, next) =>
                  events.traverse_(publish).as(next)
                }
              )
              .flatMap(_.fold(F.unit)(loop))

          // Commit `Cancelled` unless a commit already won. The fiber's own
          // finalizer runs this, and so does the handle after the fiber has
          // terminated, because a fiber cancelled before it ever ran executes
          // no finalizer at all.
          val settle: F[Unit] =
            signal.get.flatMap(published =>
              outcome.complete(Right(RunOutcome.Cancelled(id, published.last)))
            ) >> signal.update(_.settle)

          val body: F[Unit] =
            F.uncancelable(_ => F.catchNonFatal(interpreter.begin))
              .flatMap {
                case Left(failed) => publish(RunEvent.Finished(failed))
                case Right(state) => loop(state)
              }
              .guaranteeCase {
                case Outcome.Canceled()      => settle
                case Outcome.Errored(defect) =>
                  outcome.complete(Left(defect)) >> signal.update(_.settle)
                case Outcome.Succeeded(_) => F.unit
              }

          val progress: Stream[F, Progress] =
            signal.discrete
              .takeThrough(published => !published.terminal)
              .map(_.last)
              .unNone
              .filterWithPrevious((previous, next) => next.step > previous.step)

          Resource
            .make(F.start(body))(fiber => fiber.cancel >> settle)
            .map(fiber =>
              new Run(
                id,
                progress,
                fiber.join >> settle >> outcome.get.rethrow,
                fiber.cancel >> settle
              )
            )
        }

  /** The observer slot: the latest completed step and whether the run has settled. */
  private final case class Published[P](last: Option[P], terminal: Boolean):
    def settle: Published[P] = copy(terminal = true)

  /** Pure bookkeeping shared by both entry points. */
  private final class Interpreter[Id, C, Stage, Segment, E, R](
      work: Submission[Id, C, Stage, Segment, E, R]
  ):
    type Progress = RunProgress[Id, Stage, Segment]
    type Outcome  = RunOutcome[Id, Stage, Segment, E, R]
    type Event    = RunEvent[Id, Stage, Segment, E, R]

    final case class State(
        cursor: C,
        step: Long,
        segment: Option[Segment],
        segmentUnits: Long,
        segmentTotal: SegmentTotal,
        totalUnits: Long,
        last: Option[Progress]
    )

    def begin: Either[Outcome, State] =
      work.begin() match
        case Left(error)   => Left(RunOutcome.Failed(work.id, error, None))
        case Right(cursor) =>
          Right(State(cursor, 0L, None, 0L, SegmentTotal.Unknown, 0L, None))

    def step(state: State): (Vector[Event], Option[State]) =
      work.stepwise.advance(state.cursor, work.quanta) match
        case Left(error) =>
          (Vector(RunEvent.Finished(RunOutcome.Failed(work.id, error, state.last))), None)
        case Right(WorkStep.More(stage, units, next)) =>
          val (progress, advanced) = record(state, stage, units)
          (Vector(RunEvent.Advanced(progress)), Some(advanced.copy(cursor = next)))
        case Right(WorkStep.Done(units, result)) =>
          val (progress, _) = record(state, work.stepwise.stage(state.cursor), units)
          (
            Vector(
              RunEvent.Advanced(progress),
              RunEvent.Finished(RunOutcome.Completed(work.id, progress, result))
            ),
            None
          )

    private def record(state: State, stage: Stage, units: Int): (Progress, State) =
      val segment                      = work.segment(stage)
      val (segmentUnits, segmentTotal) =
        if state.segment.contains(segment) then (state.segmentUnits + units, state.segmentTotal)
        else (units.toLong, work.total(segment, state.cursor))
      val progress = RunProgress(
        work.id,
        state.step + 1,
        stage,
        units,
        segment,
        segmentUnits,
        segmentTotal,
        state.totalUnits + units
      )
      (
        progress,
        state.copy(
          step = progress.step,
          segment = Some(segment),
          segmentUnits = segmentUnits,
          segmentTotal = segmentTotal,
          totalUnits = progress.totalUnits,
          last = Some(progress)
        )
      )

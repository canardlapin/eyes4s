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
import eyes4s.compare.ComparisonBudget
import eyes4s.design.WorkQuanta
import eyes4s.kernel.{Provenance, Unit2D}
import eyes4s.plan.*

import _root_.fs2.{Chunk, Pull, Stream}
import _root_.fs2.concurrent.SignallingRef

/** The identity of one submission: the exact prepared input and plan revision
  * plus the quanta that cut its steps. It is a function of those values, so
  * two submissions of the same prepared study at the same quanta share an id
  * and their event sequences are identical. An id names the work; a consumer
  * that schedules the same work twice attaches its own job identity.
  */
final case class StudyRunId(
    input: String,
    layout: DefinitionId,
    method: DefinitionId,
    parameters: Vector[(String, Vector[Provenance.Param])],
    pairQuantum: Int,
    comparisonQuantum: Int
) derives CanEqual

object StudyRunId:
  def of[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D],
      quanta: WorkQuanta
  ): StudyRunId =
    StudyRunId(
      work.inputReference.digest,
      work.layoutId,
      work.methodId,
      work.description,
      quanta.pairs.value,
      quanta.comparison.value
    )

/** The counted span of a run: a [[StudyStage]] with the trial index erased,
  * so every trial's estimation counts toward one `Estimating(scale)` total.
  */
enum StudySegment derives CanEqual:
  case Estimating(scale: Int)
  case Comparing(scale: Int, design: StudyDesign)
  case Reducing(scale: Int, design: StudyDesign)
  case Contrasting(scale: Int)

object StudySegment:
  def of(stage: StudyStage): StudySegment = stage match
    case StudyStage.Estimating(scale, _)     => Estimating(scale)
    case StudyStage.Comparing(scale, design) => Comparing(scale, design)
    case StudyStage.Reducing(scale, design)  => Reducing(scale, design)
    case StudyStage.Contrasting(scale)       => Contrasting(scale)

/** What is known about a segment's total units before it runs. `Exact` is
  * the count the segment finishes at; `AtMost` is a bound the segment cannot
  * exceed but may finish under; `Unknown` is stated rather than estimated.
  */
enum SegmentTotal derives CanEqual:
  case Exact(units: Long)
  case AtMost(units: Long)
  case Unknown

/** Telemetry for one completed step. `stepUnits` is the step's own count as
  * the cursor reported it; `segmentUnits` accumulates over the current
  * segment and `totalUnits` over the run. Both are monotone. A zero-unit
  * step is a decided comparison, not a stall.
  */
final case class StudyProgress(
    run: StudyRunId,
    step: Long,
    stage: StudyStage,
    stepUnits: Int,
    segment: StudySegment,
    segmentUnits: Long,
    segmentTotal: SegmentTotal,
    totalUnits: Long
) derives CanEqual

/** The one authoritative end of a run. A result exists only inside
  * `Completed`; `Cancelled` and `Failed` carry the last committed progress,
  * which is `None` when no step completed.
  */
enum StudyOutcome[K, U <: Unit2D, S, D]:
  case Completed(run: StudyRunId, last: StudyProgress, result: StudyResult[K, U, S, D])
  case Cancelled(run: StudyRunId, last: Option[StudyProgress])
  case Failed(run: StudyRunId, error: PlanError, last: Option[StudyProgress])

  def run: StudyRunId

  def progress: Option[StudyProgress] = this match
    case Completed(_, last, _) => Some(last)
    case Cancelled(_, last)    => last
    case Failed(_, _, last)    => last

/** One element of the pull-driven interpretation, see [[StudyExecution.events]]. */
enum StudyEvent[K, U <: Unit2D, S, D]:
  case Advanced(progress: StudyProgress)
  case Finished(outcome: StudyOutcome[K, U, S, D])

/** A running submission. `outcome` is authoritative and settles exactly
  * once; `progress` is telemetry that may end without the terminal step.
  */
final class StudyRun[F[_], K, U <: Unit2D, S, D] private[fs2] (
    val id: StudyRunId,
    val progress: Stream[F, StudyProgress],
    val outcome: F[StudyOutcome[K, U, S, D]],
    val cancel: F[Unit]
)

/** Interpret a prepared study's bounded steps under Cats Effect.
  *
  * Both entry points drive the same [[StudyCursor]] that `PreparedStudy.run`
  * drives, so a completed result is the pure runner's result. Each step is
  * one `cursor.advance(quanta)` followed by its bookkeeping, evaluated as a
  * single uncancelable region; the fiber cedes before every step, so
  * cancellation lands between steps and never inside one. Cancellation of
  * one trial's estimation therefore waits for that trial (X2 is deferred).
  *
  * Scheduling policy, thread dispatch and widgets belong to the consumer.
  */
object StudyExecution:

  /** Fix the effect type once: `StudyExecution[IO].start(work)`. */
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
    def events[K, U <: Unit2D, P, S, D](
        work: PreparedStudy[K, U, P, S, D],
        budget: ComparisonBudget = ComparisonBudget.default,
        quanta: WorkQuanta = WorkQuanta.default
    ): Stream[F, StudyEvent[K, U, S, D]] =
      val interpreter = new Interpreter[K, U, S, D](work, quanta)
      def go(state: interpreter.State): Pull[F, StudyEvent[K, U, S, D], Unit] =
        Pull
          .eval(F.cede >> F.uncancelable(_ => F.unit.map(_ => interpreter.step(state))))
          .flatMap { case (events, next) =>
            Pull.output(Chunk.from(events)) >> next.fold(Pull.done)(go)
          }
      Pull
        .eval(F.unit.map(_ => interpreter.begin(budget)))
        .flatMap {
          case Left(failed) => Pull.output1(StudyEvent.Finished(failed))
          case Right(state) => go(state)
        }
        .stream

    /** Run a prepared study on its own fiber with an authoritative outcome.
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
    def start[K, U <: Unit2D, P, S, D](
        work: PreparedStudy[K, U, P, S, D],
        budget: ComparisonBudget = ComparisonBudget.default,
        quanta: WorkQuanta = WorkQuanta.default,
        between: F[Unit] = F.cede
    ): Resource[F, StudyRun[F, K, U, S, D]] =
      val interpreter = new Interpreter[K, U, S, D](work, quanta)
      val id          = interpreter.id
      Resource
        .eval(
          (
            Deferred[F, Either[Throwable, StudyOutcome[K, U, S, D]]],
            SignallingRef[F].of(Published.empty)
          ).tupled
        )
        .flatMap { case (outcome, signal) =>
          def publish(event: StudyEvent[K, U, S, D]): F[Unit] = event match
            case StudyEvent.Advanced(progress) => signal.set(Published(Some(progress), false))
            case StudyEvent.Finished(result)   =>
              outcome.complete(Right(result)) >> signal.update(_.settle)

          def loop(state: interpreter.State): F[Unit] =
            between >> F
              .uncancelable(_ =>
                F.unit.map(_ => interpreter.step(state)).flatMap { case (events, next) =>
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
              outcome.complete(Right(StudyOutcome.Cancelled(id, published.last)))
            ) >> signal.update(_.settle)

          val body: F[Unit] =
            F.uncancelable(_ => F.unit.map(_ => interpreter.begin(budget)))
              .flatMap {
                case Left(failed) => publish(StudyEvent.Finished(failed))
                case Right(state) => loop(state)
              }
              .guaranteeCase {
                case Outcome.Canceled()      => settle
                case Outcome.Errored(defect) =>
                  outcome.complete(Left(defect)) >> signal.update(_.settle)
                case Outcome.Succeeded(_) => F.unit
              }

          val progress: Stream[F, StudyProgress] =
            signal.discrete
              .takeThrough(published => !published.terminal)
              .map(_.last)
              .unNone
              .filterWithPrevious((previous, next) => next.step > previous.step)

          Resource
            .make(F.start(body))(fiber => fiber.cancel >> settle)
            .map(fiber =>
              new StudyRun(
                id,
                progress,
                fiber.join >> settle >> outcome.get.rethrow,
                fiber.cancel >> settle
              )
            )
        }

  /** The observer slot: the latest completed step and whether the run has settled. */
  private final case class Published(last: Option[StudyProgress], terminal: Boolean):
    def settle: Published = copy(terminal = true)

  private object Published:
    val empty: Published = Published(None, false)

  /** Pure bookkeeping shared by both entry points. */
  private final class Interpreter[K, U <: Unit2D, S, D](
      work: PreparedStudy[K, U, ?, S, D],
      quanta: WorkQuanta
  ):
    val id: StudyRunId = StudyRunId.of(work, quanta)

    final case class State(
        cursor: StudyCursor[K, U, S, D],
        step: Long,
        segment: Option[StudySegment],
        segmentUnits: Long,
        totalUnits: Long,
        last: Option[StudyProgress]
    )

    def begin(budget: ComparisonBudget): Either[StudyOutcome[K, U, S, D], State] =
      work.work(budget) match
        case Left(error)   => Left(StudyOutcome.Failed(id, error, None))
        case Right(cursor) => Right(State(cursor, 0L, None, 0L, 0L, None))

    def step(state: State): (Vector[StudyEvent[K, U, S, D]], Option[State]) =
      state.cursor.advance(quanta) match
        case Left(error) =>
          (Vector(StudyEvent.Finished(StudyOutcome.Failed(id, error, state.last))), None)
        case Right(StudyStep.More(stage, units, next)) =>
          val (progress, advanced) = record(state, stage, units)
          (Vector(StudyEvent.Advanced(progress)), Some(advanced.copy(cursor = next)))
        case Right(StudyStep.Done(units, result)) =>
          val (progress, _) = record(state, state.cursor.stage, units)
          (
            Vector(
              StudyEvent.Advanced(progress),
              StudyEvent.Finished(StudyOutcome.Completed(id, progress, result))
            ),
            None
          )

    private def record(state: State, stage: StudyStage, units: Int): (StudyProgress, State) =
      val segment      = StudySegment.of(stage)
      val segmentUnits =
        if state.segment.contains(segment) then state.segmentUnits + units else units.toLong
      val progress = StudyProgress(
        id,
        state.step + 1,
        stage,
        units,
        segment,
        segmentUnits,
        total(segment),
        state.totalUnits + units
      )
      (
        progress,
        state.copy(
          step = progress.step,
          segment = Some(segment),
          segmentUnits = segmentUnits,
          totalUnits = progress.totalUnits,
          last = Some(progress)
        )
      )

    /** Totals the prepared study can state before the segment runs. Estimation
      * is one unit per trial. A comparison segment visits each candidate pair
      * once while paging, charges one unit to begin or wholly evaluate each
      * selected pair, and for a bounded method at most one unit per grid cell
      * inside it. Reduction charges per realized score, which preparation does
      * not enumerate. A contrast visits at most every focal key.
      */
    private def total(segment: StudySegment): SegmentTotal = segment match
      case StudySegment.Estimating(_) => SegmentTotal.Exact(work.input.trials.rows.size.toLong)
      case StudySegment.Comparing(_, design) =>
        val candidates = BigInt(design match
          case StudyDesign.Matched => work.matched.candidatePairCount
          case StudyDesign.Control => work.controls.candidatePairCount)
        val perPair = work.capability match
          case ExecutionCapability.BoundedComparison         => BigInt(2) + work.grid.size
          case ExecutionCapability.SynchronousWholeOperation => BigInt(2)
        val bound = candidates * perPair
        if bound.isValidLong then SegmentTotal.AtMost(bound.toLong) else SegmentTotal.Unknown
      case StudySegment.Reducing(_, _) => SegmentTotal.Unknown
      case StudySegment.Contrasting(_) => SegmentTotal.AtMost(work.focalIndices.size.toLong)

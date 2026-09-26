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

import cats.syntax.all.*
import eyes4s.compare.ComparisonBudget
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*

/** The scientific stage a temporal step advanced. Indices address the plan's
  * repetition and window lists; `Preparing` resolves one trial's epoch,
  * window and observed occupancy, `Studying` wraps the cell's study stage.
  */
enum TemporalStage derives CanEqual:
  case Preparing(repetition: Int, window: Int, trial: Int)
  case Studying(repetition: Int, window: Int, stage: StudyStage)

/** One repetition's prepared study: the same [[PreparedStudy]] every window of
  * that repetition executes, so the schedule and its totals are stated once.
  */
final class PreparedRepetition[K, U <: Unit2D, P, S, D] private[plan] (
    val contrast: RepetitionContrast,
    val study: StudyPlan[K, U, P, S, D],
    val prepared: PreparedStudy[K, U, P, S, D]
)

/** A temporal plan bound to its input with every repetition prepared. Like
  * [[PreparedStudy]], preparation builds schedules and checks identities but
  * estimates nothing; `run` and `work` share one [[TemporalCursor]].
  */
final class PreparedTemporalStudy[K, U <: Unit2D, P, S, D] private[plan] (
    private[plan] val plan: TemporalStudyPlan[K, U, P, S, D],
    val input: TemporalStudyInput[K, U],
    val repetitions: Vector[PreparedRepetition[K, U, P, S, D]]
):
  val description: Vector[(String, Vector[Provenance.Param])] = plan.description
  val inputReference: ArtifactRef[TemporalStudyInput[K, U]]   = input.reference
  val layoutId: DefinitionId                                  = plan.base.layout.id
  val methodId: DefinitionId                                  = plan.base.method.id
  def windows: Vector[StudyWindow]                            = plan.windows
  def boundary: FixationBoundary                              = plan.boundary

  /** Trials per cell: the exact total of every `Preparing` segment. */
  def trials: Int = input.study.trials.rows.size

  /** Execute every cell in plan order: repetitions outer, windows inner. */
  def run: Either[TemporalStudyError, TemporalStudyResult[K, U, P, S, D]] =
    work().flatMap(TemporalWork.complete(_))

  /** Resumable execution; `budget` bounds each cell's comparisons as
    * `PreparedStudy.work` does.
    */
  def work(
      budget: ComparisonBudget = ComparisonBudget.default
  ): Either[TemporalStudyError, TemporalCursor[K, U, P, S, D]] =
    TemporalWork.begin(this, budget)

private[plan] sealed trait TemporalPhase[K, U <: Unit2D, S, D]
private[plan] object TemporalPhase:
  final case class Preparing[K, U <: Unit2D, S, D](
      trial: Int,
      occupancy: Vector[(K, Either[TemporalStudyError, WindowOccupancy[U]])]
  ) extends TemporalPhase[K, U, S, D]
  final case class Studying[K, U <: Unit2D, S, D](
      occupancy: Vector[(K, Either[TemporalStudyError, WindowOccupancy[U]])],
      cursor: StudyCursor[K, U, S, D]
  ) extends TemporalPhase[K, U, S, D]

/** An immutable position inside temporal execution: the cell (repetition,
  * window) being worked on, its phase, and the completed cells.
  *
  * Per cell, in plan order: one trial's epoch, window and occupancy per
  * `Preparing` step, then the cell's [[StudyCursor]], created from the
  * repetition's prepared study with the window's occupancy and provenance
  * context, advanced step for step. The inner cursor's `Done` is this
  * cursor's `More` into the next cell, with the units and stage of the step
  * that finished the cell, so the step sequence of a temporal run is the
  * concatenation of its cells' study sequences, each stamped with its cell.
  * Driving the cursor to completion is `TemporalStudyPlan.run`.
  */
final class TemporalCursor[K, U <: Unit2D, P, S, D] private[plan] (
    private val work: PreparedTemporalStudy[K, U, P, S, D],
    private val budget: ComparisonBudget,
    private val repetition: Int,
    private val window: Int,
    private val phase: TemporalPhase[K, U, S, D],
    private val completed: Vector[TemporalCell[K, U, P, S, D]]
):
  import TemporalPhase.*

  /** The stage the next `advance` will work on. */
  def stage: TemporalStage = phase match
    case Preparing(trial, _) => TemporalStage.Preparing(repetition, window, trial)
    case Studying(_, cursor) => TemporalStage.Studying(repetition, window, cursor.stage)

  /** The cell's study cursor's [[StudyCursor.reductionUnits]]; `None` while preparing. */
  def reductionUnits: Option[Long] = phase match
    case Studying(_, cursor) => cursor.reductionUnits
    case _                   => None

  def advance(quanta: WorkQuanta): Either[
    TemporalStudyError,
    WorkStep[TemporalStage, TemporalCursor[K, U, P, S, D], TemporalStudyResult[K, U, P, S, D]]
  ] =
    phase match
      case Preparing(trial, occupancy) =>
        val resolved = occupancy :+ TemporalWork.occupancy(work, window, trial)
        if trial + 1 < work.trials then
          Right(WorkStep.More(stage, 1, copy(phase = Preparing(trial + 1, resolved))))
        else
          TemporalWork
            .study(work, budget, current, window, resolved)
            .map(cursor => WorkStep.More(stage, 1, copy(phase = Studying(resolved, cursor))))

      case Studying(occupancy, cursor) =>
        cursor.advance(quanta).left.map(TemporalStudyError.Input.apply).flatMap {
          case StudyStep.More(inner, units, next) =>
            Right(
              WorkStep.More(
                TemporalStage.Studying(repetition, window, inner),
                units,
                copy(phase = Studying(occupancy, next))
              )
            )
          case StudyStep.Done(units, result) =>
            val cells = completed :+ new TemporalCell(
              current.contrast,
              work.windows(window),
              current.study,
              occupancy,
              result
            )
            val finished = TemporalStage.Studying(repetition, window, cursor.stage)
            if window + 1 < work.windows.size then
              TemporalWork
                .enter(work, budget, repetition, window + 1, cells)
                .map(next => WorkStep.More(finished, units, next))
            else if repetition + 1 < work.repetitions.size then
              TemporalWork
                .enter(work, budget, repetition + 1, 0, cells)
                .map(next => WorkStep.More(finished, units, next))
            else
              Right(
                WorkStep.Done(
                  units,
                  new TemporalStudyResult(work.plan, cells)
                )
              )
        }

  private def current: PreparedRepetition[K, U, P, S, D] = work.repetitions(repetition)

  private def copy(phase: TemporalPhase[K, U, S, D]): TemporalCursor[K, U, P, S, D] =
    new TemporalCursor(work, budget, repetition, window, phase, completed)

object TemporalWork:

  /** Drive a temporal cursor to completion with fixed quanta. */
  def complete[K, U <: Unit2D, P, S, D](
      cursor: TemporalCursor[K, U, P, S, D],
      quanta: WorkQuanta = WorkQuanta.default
  ): Either[TemporalStudyError, TemporalStudyResult[K, U, P, S, D]] =
    Stepwise.complete(cursor, quanta)

  private[plan] def prepare[K, U <: Unit2D, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      available: TemporalStudyInput[K, U],
      budget: PairScheduleBudget
  ): Either[TemporalStudyError, PreparedTemporalStudy[K, U, P, S, D]] =
    plan.prerequisites(Some(available)).headOption match
      case Some(error) => Left(error)
      case None        =>
        plan.repetitions
          .traverse { repetition =>
            for
              study    <- plan.repetitionPlan(repetition)
              prepared <- study
                .prepare(available.study, budget)
                .left
                .map(TemporalStudyError.Input.apply)
            yield new PreparedRepetition(repetition, study, prepared)
          }
          .map(repetitions => new PreparedTemporalStudy(plan, available, repetitions))

  private[plan] def begin[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      budget: ComparisonBudget
  ): Either[TemporalStudyError, TemporalCursor[K, U, P, S, D]] =
    enter(work, budget, 0, 0, Vector.empty)

  /** The first cursor of a cell: preparing its first trial, or its study
    * cursor directly when the input has no trials.
    */
  private[plan] def enter[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      budget: ComparisonBudget,
      repetition: Int,
      window: Int,
      completed: Vector[TemporalCell[K, U, P, S, D]]
  ): Either[TemporalStudyError, TemporalCursor[K, U, P, S, D]] =
    if work.trials > 0 then
      Right(
        new TemporalCursor(
          work,
          budget,
          repetition,
          window,
          TemporalPhase.Preparing(0, Vector.empty),
          completed
        )
      )
    else
      study(work, budget, work.repetitions(repetition), window, Vector.empty).map(cursor =>
        new TemporalCursor(
          work,
          budget,
          repetition,
          window,
          TemporalPhase.Studying(Vector.empty, cursor),
          completed
        )
      )

  /** One trial's measured anchor, resolved window and observed occupancy,
    * over the fixations the base plan's initial-fixation policy keeps.
    */
  private[plan] def occupancy[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      window: Int,
      trial: Int
  ): (K, Either[TemporalStudyError, WindowOccupancy[U]]) =
    val row      = work.input.study.trials.rows(trial)
    val resolved = for
      epoch <- work.input.epochs
        .get(row.key)
        .toRight(
          TemporalStudyError.MissingEpoch(work.plan.base.layout.digest.digest(row.key).render)
        )
      interval <- work.windows(window).resolve(epoch)
      outcome = work.plan.base.initialFixationRule.select(row.value)
      kept <- outcome.kept.toRight(
        TemporalStudyError.Input(
          PlanError.InitialFixations(
            InitialFixationError.NoFixationKept(
              outcome.tally.dropped,
              outcome.tally.droppedDuration.toMicros
            )
          )
        )
      )
      value <- WindowOccupancy(kept, interval, epoch.coverage, work.boundary).left
        .map(TemporalStudyError.Occupancy.apply)
    yield value
    row.key -> resolved

  /** The cell's study cursor over its resolved occupancy and provenance context. */
  private[plan] def study[K, U <: Unit2D, P, S, D](
      work: PreparedTemporalStudy[K, U, P, S, D],
      budget: ComparisonBudget,
      repetition: PreparedRepetition[K, U, P, S, D],
      window: Int,
      occupancy: Vector[(K, Either[TemporalStudyError, WindowOccupancy[U]])]
  ): Either[TemporalStudyError, StudyCursor[K, U, S, D]] =
    val byKey   = occupancy.toMap
    val context = work.plan.provenanceContext(repetition.contrast, work.windows(window))
    repetition.prepared
      .work(
        budget,
        (key, _) =>
          byKey
            .get(key)
            .toRight(
              TemporalStudyError.MissingEpoch(work.plan.base.layout.digest.digest(key).render)
            )
            .flatten
            .left
            .map(StudyFailure.Temporal(key, _))
            .map(_.measure),
        context
      )
      .left
      .map(TemporalStudyError.Input.apply)

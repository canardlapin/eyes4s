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

import eyes4s.compare.ComparisonBudget
import eyes4s.design.WorkQuanta
import eyes4s.kernel.Unit2D

/** Counting is preparation telemetry, separate from the four scientific stages.
  * Every running stage carries its completed-object meter as a required value.
  */
enum StudyRunStage:
  case Counting(design: StudyDesign, visited: Long)
  case Running(stage: StudyStage, meter: StageMeter)

enum StudyRunSegment derives CanEqual:
  case Counting
  case Running(segment: StudySegment)

object StudyRunSegment:
  def of(stage: StudyRunStage): StudyRunSegment = stage match
    case StudyRunStage.Counting(_, _)    => Counting
    case StudyRunStage.Running(stage, _) => Running(StudySegment.of(stage))

/** Scientific refusal and invalid telemetry retain their complete typed operands. */
enum StudyRunError:
  case Plan(underlying: PlanError)
  case Meter(underlying: StageMeterError)
  case UnexpectedCompletion(design: StudyDesign, visited: Long)
  def message: String = this match
    case Plan(underlying)                      => underlying.message
    case Meter(underlying)                     => underlying.message
    case UnexpectedCompletion(design, visited) =>
      s"A scientific result cannot complete counting design=$design after visited=$visited."

/** One cursor crosses counting into execution without a second run or recount.
  * Preparation/source indexing happens before this cursor. A scientific estimation
  * remains one whole trial; stepping does not promise bounded estimation latency.
  */
final class CountedStudyCursor[K, U <: Unit2D, P, S, D] private (
    private val work: PreparedStudy[K, U, P, S, D],
    private val budget: ComparisonBudget,
    private val phase: CountedStudyPhase[K, U, S, D],
    val stage: StudyRunStage
):
  import CountedStudyPhase.*
  import CountedStudyCursor.*

  /** Work-unit total, separate from the scientific object meter. */
  def total(segment: StudyRunSegment): SegmentTotal = (segment, phase) match
    case (StudyRunSegment.Counting, _)                          => SegmentTotal.Counting
    case (StudyRunSegment.Running(segment), Running(cursor, _)) =>
      StudySegment.total(work, segment, cursor)
    case _ => SegmentTotal.Unknown

  def advance(quanta: WorkQuanta): Either[StudyRunError, WorkStep[
    StudyRunStage,
    CountedStudyCursor[K, U, P, S, D],
    StudyResult[K, U, S, D]
  ]] = phase match
    case Counting(cursor) =>
      cursor.advance(quanta).left.map(StudyRunError.Plan(_)).flatMap {
        case WorkStep.More(design, units, next) =>
          next.pairingRefusal.toLeft(()).left.map(StudyRunError.Plan(_)).map { _ =>
            WorkStep.More(
              StudyRunStage.Counting(design, next.visited),
              units,
              new CountedStudyCursor(
                work,
                budget,
                Counting(next),
                StudyRunStage.Counting(next.stage, next.visited)
              )
            )
          }
        case WorkStep.Done(units, counts) =>
          work.countedWork(counts, budget).left.map(StudyRunError.Plan(_)).flatMap { next =>
            runningStage(next.stage, Totals.of(next), counts).map { snapshot =>
              WorkStep.More(
                StudyRunStage.Counting(cursor.stage, cursor.visited + units),
                units,
                new CountedStudyCursor(work, budget, Running(next, counts), snapshot)
              )
            }
          }
      }
    case Running(cursor, counts) =>
      cursor.advance(quanta).left.map(StudyRunError.Plan(_)).flatMap {
        case StudyStep.More(executed, units, next) =>
          val completed = Totals.of(next)
          for
            after    <- runningStage(executed, completed, counts)
            snapshot <- runningStage(next.stage, completed, counts)
          yield WorkStep.More(
            after,
            units,
            new CountedStudyCursor(work, budget, Running(next, counts), snapshot)
          )
        case StudyStep.Done(units, result) => Right(WorkStep.Done(units, result))
      }

  /** Committed-step telemetry, including the result of the terminal step.
    * A runner must use this projection rather than the next-step snapshot.
    */
  def completedStage(
      step: WorkStep[
        StudyRunStage,
        CountedStudyCursor[K, U, P, S, D],
        StudyResult[K, U, S, D]
      ]
  ): Either[StudyRunError, StudyRunStage] = step match
    case WorkStep.More(completed, _, _) => Right(completed)
    case WorkStep.Done(_, result)       =>
      phase match
        case Running(cursor, counts) => runningStage(cursor.stage, Totals.of(result), counts)
        case Counting(cursor)        =>
          Left(StudyRunError.UnexpectedCompletion(cursor.stage, cursor.visited))

private[plan] enum CountedStudyPhase[K, U <: Unit2D, S, D]:
  case Counting(cursor: CountCursor[K])
  case Running(cursor: StudyCursor[K, U, S, D], counts: StudyCounts[K])

object CountedStudyCursor:
  def of[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D],
      budget: ComparisonBudget = ComparisonBudget.default
  ): Either[StudyRunError, CountedStudyCursor[K, U, P, S, D]] =
    work.countWork.left.map(StudyRunError.Plan(_)).map { cursor =>
      new CountedStudyCursor(
        work,
        budget,
        CountedStudyPhase.Counting(cursor),
        StudyRunStage.Counting(cursor.stage, cursor.visited)
      )
    }

  private final case class Totals(maps: Long, pairs: Long, keys: Long, rows: Long)
  private object Totals:
    def of[K, U <: Unit2D, S, D](cursor: StudyCursor[K, U, S, D]): Totals =
      Totals(
        cursor.completedMaps,
        cursor.completedPairs,
        cursor.completedReductionKeys,
        cursor.completedContrastRows
      )
    def of[K, U <: Unit2D, S, D](result: StudyResult[K, U, S, D]): Totals =
      result.scales.foldLeft(Totals(0L, 0L, 0L, 0L)) { (acc, scale) =>
        Totals(
          acc.maps + scale.estimation.size,
          acc.pairs + scale.analyses.matchedSource.rows.size + scale.analyses.controlSource.rows.size,
          acc.keys + scale.analyses.matched.entries.size + scale.analyses.control.entries.size,
          acc.rows + scale.contrast.toOption.fold(0L)(_.rows.size.toLong)
        )
      }

  private def runningStage[K](
      stage: StudyStage,
      completed: Totals,
      counts: StudyCounts[K]
  ): Either[StudyRunError, StudyRunStage] =
    val (kind, unit, done, total) = stage match
      case StudyStage.Estimating(_, _) =>
        (
          StageKind.Estimating,
          CountUnit.Maps,
          completed.maps,
          SegmentTotal.Exact(counts.totalMaps)
        )
      case StudyStage.Comparing(_, _) =>
        (
          StageKind.Comparing,
          CountUnit.Pairs,
          completed.pairs,
          SegmentTotal.Exact(counts.totalPairs)
        )
      case StudyStage.Reducing(_, _) =>
        (
          StageKind.Reducing,
          CountUnit.Keys,
          completed.keys,
          SegmentTotal.Exact(counts.totalReductionKeys)
        )
      case StudyStage.Contrasting(_) =>
        (
          StageKind.Contrasting,
          CountUnit.Rows,
          completed.rows,
          SegmentTotal.AtMost(counts.totalContrastRows)
        )
    StageMeter
      .of(kind, unit, done, total)
      .left
      .map(StudyRunError.Meter(_))
      .map(StudyRunStage.Running(stage, _))

  given [K, U <: Unit2D, P, S, D]: Stepwise[
    CountedStudyCursor[K, U, P, S, D],
    StudyRunStage,
    StudyRunError,
    StudyResult[K, U, S, D]
  ] with
    def stage(cursor: CountedStudyCursor[K, U, P, S, D]): StudyRunStage = cursor.stage
    def advance(cursor: CountedStudyCursor[K, U, P, S, D], quanta: WorkQuanta): Either[
      StudyRunError,
      WorkStep[StudyRunStage, CountedStudyCursor[K, U, P, S, D], StudyResult[K, U, S, D]]
    ] = cursor.advance(quanta)

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

import eyes4s.design.WorkQuanta
import eyes4s.kernel.Unit2D

/** One step of bounded work over a cursor of type `C`: the stage the step
  * worked on with its own unit count and the next immutable cursor, or the
  * complete result with the units of the step that finished it.
  */
enum WorkStep[+Stage, +C, +R]:
  case More(stage: Stage, workUnits: Int, next: C)
  case Done(workUnits: Int, result: R)

/** The step shape every bounded plan family exposes and the one contract a
  * runner interprets: a pure, immutable cursor that names the stage its next
  * `advance` works on and advances by one bounded step to `More` or `Done`, or
  * fails with a typed error. [[StudyCursor]] satisfies it as it is;
  * [[RecordingCursor]] and [[TemporalCursor]] are written to it. Instances live
  * in this companion.
  */
trait Stepwise[C, Stage, E, R]:
  def stage(cursor: C): Stage
  def advance(cursor: C, quanta: WorkQuanta): Either[E, WorkStep[Stage, C, R]]

object Stepwise:
  def apply[C, Stage, E, R](using s: Stepwise[C, Stage, E, R]): Stepwise[C, Stage, E, R] = s

  /** Drive any stepwise cursor to completion with fixed quanta. */
  def complete[C, Stage, E, R](cursor: C, quanta: WorkQuanta)(using
      s: Stepwise[C, Stage, E, R]
  ): Either[E, R] =
    @annotation.tailrec
    def loop(cursor: C): Either[E, R] = s.advance(cursor, quanta) match
      case Left(error)                      => Left(error)
      case Right(WorkStep.More(_, _, next)) => loop(next)
      case Right(WorkStep.Done(_, result))  => Right(result)
    loop(cursor)

  given study[K, U <: Unit2D, S, D]
      : Stepwise[StudyCursor[K, U, S, D], StudyStage, PlanError, StudyResult[K, U, S, D]] with
    def stage(cursor: StudyCursor[K, U, S, D]): StudyStage = cursor.stage
    def advance(
        cursor: StudyCursor[K, U, S, D],
        quanta: WorkQuanta
    ): Either[
      PlanError,
      WorkStep[StudyStage, StudyCursor[K, U, S, D], StudyResult[K, U, S, D]]
    ] =
      cursor.advance(quanta).map {
        case StudyStep.More(stage, units, next) => WorkStep.More(stage, units, next)
        case StudyStep.Done(units, result)      => WorkStep.Done(units, result)
      }

  given recording[P]
      : Stepwise[RecordingCursor[P], RecordingStage, RecordingPlanError, RecordingAnalysis[P]]
  with
    def stage(cursor: RecordingCursor[P]): RecordingStage = cursor.stage
    def advance(cursor: RecordingCursor[P], quanta: WorkQuanta): Either[
      RecordingPlanError,
      WorkStep[RecordingStage, RecordingCursor[P], RecordingAnalysis[P]]
    ] = cursor.advance(quanta)

  given temporal[K, U <: Unit2D, P, S, D]: Stepwise[
    TemporalCursor[K, U, P, S, D],
    TemporalStage,
    TemporalStudyError,
    TemporalStudyResult[K, U, P, S, D]
  ] with
    def stage(cursor: TemporalCursor[K, U, P, S, D]): TemporalStage = cursor.stage
    def advance(cursor: TemporalCursor[K, U, P, S, D], quanta: WorkQuanta): Either[
      TemporalStudyError,
      WorkStep[TemporalStage, TemporalCursor[K, U, P, S, D], TemporalStudyResult[K, U, P, S, D]]
    ] = cursor.advance(quanta)

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

package eyes4s.studio.core.real

import cats.syntax.all.*
import eyes4s.compare.Similarity
import eyes4s.design.SignedDifference
import eyes4s.fs2.{RunOutcome, RunProgress}
import eyes4s.kernel.Unit2D
import eyes4s.plan.{
  CountUnit as CoreUnit,
  Diagnostic,
  StageKind as CoreKind,
  StageMeter as CoreMeter,
  StudyCounts,
  StudyResult,
  StudyRunError,
  StudyRunSegment,
  StudyRunStage,
  TrialKey as CoreKey
}
import eyes4s.studio.core.backend.*

/** eyes4s-fs2's run of a prepared study, read as the studio job protocol
  * (S3.7 slice 4). Every count is eyes4s's: a report's meter is the UI-D
  * `StageMeter` eyes4s commits with each step, unchanged, and its run totals
  * are the latest map and pair meters eyes4s has reported against the exact
  * totals of the revision's `StudyCounts`.
  */
object RealExecution:
  /** The run's identity in eyes4s: the studio job and run it serves. */
  type Id       = (JobId, RunId)
  type Result   = StudyResult[CoreKey, Unit2D.Px, Similarity, SignedDifference]
  type Progress = RunProgress[Id, StudyRunStage, StudyRunSegment]
  type Outcome  = RunOutcome[Id, StudyRunStage, StudyRunSegment, StudyRunError, Result]

  /** The latest run-wide map and pair counts eyes4s's meters reported. A
    * meter counts one unit, so the other is carried from its last report.
    */
  final case class Carried(maps: Long, pairs: Long) derives CanEqual

  object Carried:
    val none: Carried = Carried(0L, 0L)

  /** Why eyes4s's run could not be read as the protocol states it: a defect
    * of eyes4s or of this mapping, never a property of the data.
    */
  final case class Defect(message: String) derives CanEqual

  /** A committed step as a job report, with the counts it carries forward.
    * Counting is preparation telemetry with no protocol stage, so a counting
    * step reports nothing and the job stays queued.
    */
  def report(
      job: JobId,
      run: RunId,
      progress: Progress,
      carried: Carried,
      counts: StudyCounts[CoreKey]
  ): Either[Defect, Option[(JobProgress, Carried)]] =
    (progress.segment, progress.stage) match
      case (StudyRunSegment.Running(segment), StudyRunStage.Running(_, meter)) =>
        val next = meter.unit match
          case CoreUnit.Maps  => carried.copy(maps = meter.done)
          case CoreUnit.Pairs => carried.copy(pairs = meter.done)
          case _              => carried
        val made =
          for
            m <- studioMeter(meter)
            t <- RunTotals.of(
              next.maps,
              ProgressTotal.Exact(counts.totalMaps),
              next.pairs,
              ProgressTotal.Exact(counts.totalPairs)
            )
            p <- JobProgress.of(job, run, progress.step, Segment.of(segment), m, t)
          yield Some((p, next))
        made.leftMap(e => Defect(e.message))
      case (StudyRunSegment.Counting, StudyRunStage.Counting(_, _)) => Right(None)
      case (segment, stage)                                         =>
        Left(Defect(s"eyes4s reported stage $stage under segment $segment"))

  /** The settled job: its outcome, the run's state and, when it completed,
    * eyes4s's result. `last` is the job's latest report, the fallback when
    * eyes4s settles before any scientific step.
    */
  def settle(
      job: JobId,
      run: RunId,
      outcome: Either[Defect, Outcome],
      carried: Carried,
      counts: StudyCounts[CoreKey]
  ): (JobOutcome, RunState, Option[Result]) =
    def lastOf(p: Option[Progress]): Either[Defect, Option[JobProgress]] =
      p.flatTraverse(report(job, run, _, carried, counts).map(_.map(_._1)))
    def failed(d: Defect) =
      (JobOutcome.Failed(job, run, Vector(defect(run, d)), None), RunState.Failed, None)
    outcome
      .flatMap {
        case RunOutcome.Completed(_, last, result) =>
          // The completed run's maps and pairs are the rows eyes4s's result
          // holds; they must be the totals its counts stated.
          val done = Carried(
            result.scales.map(_.estimation.size.toLong).sum,
            result.scales
              .map(s =>
                s.analyses.matchedSource.rows.size.toLong + s.analyses.controlSource.rows.size
              )
              .sum
          )
          if done.maps != counts.totalMaps || done.pairs != counts.totalPairs then
            Left(
              Defect(
                s"eyes4s completed ${done.maps} maps and ${done.pairs} pairs; " +
                  s"its counts stated ${counts.totalMaps} and ${counts.totalPairs}"
              )
            )
          else
            report(job, run, last, done, counts).flatMap {
              case Some((p, _)) =>
                Right((JobOutcome.Completed(job, run, p), RunState.Completed, Some(result)))
              case None => Left(Defect("eyes4s completed the run while counting"))
            }
        case RunOutcome.Cancelled(_, last) =>
          lastOf(last).map(p =>
            (JobOutcome.Cancelled(job, run, p), RunState.Cancelled(p.map(_.segment.kind)), None)
          )
        case RunOutcome.Failed(_, error, last) =>
          lastOf(last).map(p =>
            (JobOutcome.Failed(job, run, diagnostics(error), p), RunState.Failed, None)
          )
      }
      .fold(failed, identity)

  /** eyes4s's diagnostic of a refused run (family `study-run`: a plan
    * refusal, an invalid meter or an unexpected completion), with eyes4s's
    * operands.
    */
  def diagnostics(error: StudyRunError): Vector[StudioDiagnostic] =
    Vector(StudioDiagnostic.of(Diagnostic.of(error), (k: Nothing) => k))

  /** A host defect: an exception the run raised, or telemetry this mapping
    * cannot read. The run produced nothing the backend can hold.
    */
  def defect(run: RunId, d: Defect): StudioDiagnostic =
    BackendError.Unavailable(DiagnosticLocus.Run(run)).diagnostic.copy(message = d.message)

  private def studioMeter(m: CoreMeter): Either[ProgressError, StageMeter] =
    val kind = m.kind match
      case CoreKind.Estimating  => StageKind.Estimating
      case CoreKind.Comparing   => StageKind.Comparing
      case CoreKind.Reducing    => StageKind.Reducing
      case CoreKind.Contrasting => StageKind.Contrasting
    val unit = m.unit match
      case CoreUnit.Maps  => CountUnit.Maps
      case CoreUnit.Pairs => CountUnit.Pairs
      case CoreUnit.Keys  => CountUnit.Keys
      case CoreUnit.Rows  => CountUnit.Rows
    StageMeter.of(kind, unit, m.done, ProgressTotal.of(m.total))

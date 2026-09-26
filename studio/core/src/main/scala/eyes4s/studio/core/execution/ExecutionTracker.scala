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

package eyes4s.studio.core.execution

import eyes4s.studio.core.backend.*

/** The execution service's state, as a pure value: every tracked job and the
  * stamp currently requested. [[ExecutionService]] holds one in a `Ref` and
  * publishes the events each transition returns; the properties of S3.1 are
  * stated and tested here.
  *
  * The rules:
  *   - `requested` is the stamp of the latest submission or [[require]]. A
  *     completion is `Succeeded` only if its job's stamp equals `requested`
  *     when the completion arrives; otherwise it is `Superseded` and never
  *     produces a [[RunReady]] (E2E-07).
  *   - Progress never moves back: a report that does not follow the job's
  *     last one ([[ExecutionProgress.follows]]) is dropped, so steps rise and
  *     done units are monotone per run.
  *   - A terminal phase absorbs every later event.
  *   - A cancel request moves a live job to `Cancelling`; later telemetry
  *     updates its last progress but never makes it `Running` again.
  */
final case class ExecutionTracker private (
    jobs: Vector[ExecutionJob],
    requested: Option[RunStamp]
) derives CanEqual:
  import ExecutionTracker.Step

  def job(id: JobId): Either[ExecutionError, ExecutionJob] =
    jobs.find(_.id == id).toRight(ExecutionError.UnknownJob(id, jobs.map(_.id)))

  private def put(j: ExecutionJob): ExecutionTracker =
    copy(jobs = jobs.map(o => if o.id == j.id then j else o))

  private def changed(j: ExecutionJob): Step[ExecutionJob] =
    (put(j), Vector(ExecutionEvent.Changed(j)), j)

  /** Track a job the backend accepted for `stamp`, which becomes the
    * requested stamp. A backend status that has already settled is observed
    * at once.
    */
  def track(status: JobStatus, stamp: RunStamp): Either[ExecutionError, Step[ExecutionJob]] =
    if jobs.exists(_.id == status.job) then Left(ExecutionError.AlreadyTracked(status.job))
    else
      val queued  = ExecutionJob(status.job, status.run, stamp, JobPhase.Queued)
      val tracked = ExecutionTracker(jobs :+ queued, Some(stamp))
      val first   = Vector(ExecutionEvent.Changed(queued))
      def follow(event: JobEvent): Step[ExecutionJob] =
        tracked
          .observe(status.job, event)
          .fold(_ => (tracked, first, queued), (t, events, j) => (t, first ++ events, j))
      Right(status.state match
        case JobState.Queued      => (tracked, first, queued)
        case JobState.Running(p)  => follow(JobEvent.Advanced(p))
        case JobState.Finished(o) => follow(JobEvent.Finished(o)))

  /** The document now wants results for `stamp` (a draft saved as a new
    * revision, a dataset re-admitted, a plan re-bound). Running jobs of any
    * other stamp will settle as `Superseded`.
    */
  def require(stamp: RunStamp): ExecutionTracker = copy(requested = Some(stamp))

  /** A cancel request: a live job becomes `Cancelling`; a settled one is
    * returned as it is.
    */
  def cancelling(id: JobId): Either[ExecutionError, Step[ExecutionJob]] =
    job(id).map { j =>
      j.phase match
        case JobPhase.Queued | JobPhase.Running(_) =>
          changed(j.copy(phase = JobPhase.Cancelling(j.phase.lastReport)))
        case _ => (this, Vector.empty, j)
    }

  /** Fold one backend event of job `id` into the state. */
  def observe(id: JobId, event: JobEvent): Either[ExecutionError, Step[ExecutionJob]] =
    job(id).map { j =>
      if j.phase.isTerminal then (this, Vector.empty, j)
      else
        event match
          case JobEvent.Advanced(p) =>
            val report = ExecutionProgress(p)
            if j.phase.lastReport.exists(!report.follows(_)) then (this, Vector.empty, j)
            else
              j.phase match
                case JobPhase.Cancelling(_) =>
                  changed(j.copy(phase = JobPhase.Cancelling(Some(report))))
                case _ => changed(j.copy(phase = JobPhase.Running(report)))
          case JobEvent.Finished(outcome) => settle(j, outcome)
    }

  /** The job's progress stream ended before it settled and the backend holds
    * no outcome: it fails with `diagnostic`.
    */
  def lose(
      id: JobId,
      diagnostic: StudioDiagnostic
  ): Either[ExecutionError, Step[ExecutionJob]] =
    job(id).map { j =>
      if j.phase.isTerminal then (this, Vector.empty, j)
      else changed(j.copy(phase = JobPhase.Failed(Vector(diagnostic), j.phase.lastReport)))
    }

  /** The later of the job's last report and the outcome's. */
  private def latest(j: ExecutionJob, reported: Option[JobProgress]) =
    (j.phase.lastReport, reported.map(ExecutionProgress(_))) match
      case (Some(a), Some(b)) => Some(if b.follows(a) then b else a)
      case (a, b)             => a.orElse(b)

  private def settle(j: ExecutionJob, outcome: JobOutcome): Step[ExecutionJob] =
    outcome match
      case JobOutcome.Completed(_, _, last) =>
        val progress = latest(j, Some(last)).getOrElse(ExecutionProgress(last))
        if requested.contains(j.stamp) then
          val (t, events, done) = changed(j.copy(phase = JobPhase.Succeeded(progress)))
          (t, events :+ ExecutionEvent.Ready(RunReady(done.id, done.run, done.stamp)), done)
        else changed(j.copy(phase = JobPhase.Superseded(requested, progress)))
      case JobOutcome.Cancelled(_, _, last) =>
        changed(j.copy(phase = JobPhase.Cancelled(latest(j, last))))
      case JobOutcome.Failed(_, _, diagnostics, last) =>
        val stated = if diagnostics.isEmpty then Vector(ExecutionDiagnostics.unexplained(j.id))
        else diagnostics
        changed(j.copy(phase = JobPhase.Failed(stated, latest(j, last))))

object ExecutionTracker:
  /** A transition's next state, the events it publishes in order, and the
    * job it concerned.
    */
  type Step[A] = (ExecutionTracker, Vector[ExecutionEvent], A)

  val empty: ExecutionTracker = ExecutionTracker(Vector.empty, None)

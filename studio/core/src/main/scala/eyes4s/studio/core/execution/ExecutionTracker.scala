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

/** The user's latest intent: the stamp to run or show results for, and the
  * generation that recorded it. Every submission and every requirement
  * change takes a new generation.
  */
final case class Requested(stamp: RunStamp, generation: Long) derives CanEqual

/** The execution service's state, as a pure value: the tracked jobs and the
  * latest intent. [[ExecutionService]] holds one in a `Ref` and publishes the
  * events each transition returns; the properties of S3.1 are stated and
  * tested here.
  *
  * The rules:
  *   - An intent ([[intend]] before a submission, [[require]] otherwise) is
  *     recorded before the backend is asked, with a new generation. A job
  *     the backend then accepts is tracked for its intent's generation: if a
  *     newer intent was recorded meanwhile the job is `Superseded` from the
  *     start, and tracking never changes the requested stamp.
  *   - A completion is `Succeeded` only if the job was accepted, was not
  *     being cancelled, and its stamp is still the requested one; otherwise it
  *     is `Superseded` (or `Cancelled`) and never produces a [[RunReady]]
  *     (E2E-07).
  *   - Progress never moves back: a report that does not follow the job's
  *     last one ([[ExecutionProgress.follows]]) is dropped, so steps rise and
  *     done units are monotone per run.
  *   - A terminal phase absorbs every later event.
  *   - A cancel request moves a live job to `Cancelling`; later telemetry
  *     updates its last progress but never makes it `Running` again, and a
  *     completion then settles it as `Cancelled`.
  *   - Every live job is kept; of the settled ones, only the latest
  *     [[ExecutionTracker.RetainedPerRevision]] of each analysis revision.
  */
final case class ExecutionTracker private (
    jobs: Vector[ExecutionJob],
    requested: Option[Requested],
    generation: Long
) derives CanEqual:
  import ExecutionTracker.{RetainedPerRevision, Step}

  def job(id: JobId): Either[ExecutionError, ExecutionJob] =
    jobs.find(_.id == id).toRight(ExecutionError.UnknownJob(id, jobs.map(_.id)))

  def requestedStamp: Option[RunStamp] = requested.map(_.stamp)

  /** Drop settled jobs beyond the latest few of each revision. */
  private def evicted: ExecutionTracker =
    val kept = jobs
      .filter(_.phase.isTerminal)
      .groupBy(_.stamp.revision)
      .values
      .flatMap(_.takeRight(RetainedPerRevision).map(_.id))
      .toSet
    copy(jobs = jobs.filter(j => !j.phase.isTerminal || kept.contains(j.id)))

  private def put(j: ExecutionJob): ExecutionTracker =
    copy(jobs = jobs.map(o => if o.id == j.id then j else o)).evicted

  private def changed(j: ExecutionJob): Step[ExecutionJob] =
    (put(j), Vector(ExecutionEvent.Changed(j)), j)

  /** Record the intent to submit `stamp`, before the backend is asked. The
    * returned generation goes to [[track]] with the backend's answer.
    */
  def intend(stamp: RunStamp): (ExecutionTracker, Long) =
    val next = generation + 1
    (copy(requested = Some(Requested(stamp, next)), generation = next), next)

  /** The document now wants results for `stamp` (a draft saved as a new
    * revision, a dataset re-admitted, a plan re-bound). Jobs of any other
    * stamp will settle as `Superseded`, and submissions still awaiting the
    * backend are superseded when they arrive.
    */
  def require(stamp: RunStamp): ExecutionTracker = intend(stamp)._1

  /** Track a job the backend accepted for the intent of `generation`. If a
    * newer intent was recorded since, the job is `Superseded` at once (the
    * caller cancels it). A backend status that has already settled is
    * observed at once. The requested stamp is never changed here.
    */
  def track(
      status: JobStatus,
      stamp: RunStamp,
      generation: Long
  ): Either[ExecutionError, Step[ExecutionJob]] =
    if jobs.exists(_.id == status.job) then Left(ExecutionError.AlreadyTracked(status.job))
    else if !requested.exists(_.generation == generation) then
      val late = ExecutionJob(
        status.job,
        status.run,
        stamp,
        JobPhase.Superseded(requestedStamp, None)
      )
      Right((copy(jobs = jobs :+ late).evicted, Vector(ExecutionEvent.Changed(late)), late))
    else
      val queued  = ExecutionJob(status.job, status.run, stamp, JobPhase.Queued)
      val tracked = copy(jobs = jobs :+ queued)
      val first   = Vector(ExecutionEvent.Changed(queued))
      def follow(event: JobEvent): Step[ExecutionJob] =
        tracked
          .observe(status.job, event)
          .fold(_ => (tracked, first, queued), (t, events, j) => (t, first ++ events, j))
      Right(status.state match
        case JobState.Queued      => (tracked, first, queued)
        case JobState.Running(p)  => follow(JobEvent.Advanced(p))
        case JobState.Finished(o) => follow(JobEvent.Finished(o)))

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
        j.phase match
          case JobPhase.Cancelling(_) =>
            changed(j.copy(phase = JobPhase.Cancelled(Some(progress))))
          case _ if requestedStamp.contains(j.stamp) =>
            val (t, events, done) = changed(j.copy(phase = JobPhase.Succeeded(progress)))
            (t, events :+ ExecutionEvent.Ready(RunReady(done.id, done.run, done.stamp)), done)
          case _ => changed(j.copy(phase = JobPhase.Superseded(requestedStamp, Some(progress))))
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

  /** Settled jobs kept per analysis revision; live jobs are always kept. */
  val RetainedPerRevision: Int = 10

  val empty: ExecutionTracker = ExecutionTracker(Vector.empty, None, 0L)

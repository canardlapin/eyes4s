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

package eyes4s.studio.app.jobs

import eyes4s.studio.core.backend.{JobId, RunId, StudioDiagnostic}
import eyes4s.studio.core.execution.{
  ExecutionError,
  ExecutionEvent,
  ExecutionJob,
  JobPhase,
  RunReady,
  RunShelf,
  RunStamp
}

/** What the jobs chip and the status bar's job slot show. */
enum JobHeadline derives CanEqual:
  case Idle
  case Active(job: ExecutionJob)

  /** The newest job failed; `diagnostics` are its stable-coded causes. */
  case Failed(job: ExecutionJob, diagnostics: Vector[StudioDiagnostic])

  /** A finished run waiting for the user to choose Show ("Run 8 ready — Show"). */
  case Ready(notice: RunReady)

/** The presentation layer's projection of the execution service (S3.1):
  * its jobs, keyed by [[JobId]], and the [[RunShelf]] the app keeps. It adds
  * no state of its own: jobs arrive as [[ExecutionEvent]]s or a snapshot, and
  * the shelf changes only through its own `receive`, `require`, `show` and
  * `dismiss`.
  */
final case class JobBoard private (jobs: Vector[ExecutionJob], shelf: RunShelf)
    derives CanEqual:

  /** Jobs newest first by job id. */
  private def newestFirst: Vector[ExecutionJob] = jobs.sortBy(-_.id.number)

  def job(id: JobId): Option[ExecutionJob] = jobs.find(_.id == id)

  /** The newest job that has not settled. */
  def active: Option[ExecutionJob] = newestFirst.find(!_.phase.isTerminal)

  /** The ready notice (`RunShelf.pending`). */
  def ready: Option[RunReady] = shelf.pending

  /** The newest active job; else the ready notice; else the newest job when
    * it failed; else idle.
    */
  def headline: JobHeadline =
    active
      .map(JobHeadline.Active(_))
      .orElse(ready.map(JobHeadline.Ready(_)))
      .orElse(newestFirst.headOption.collect {
        case j @ ExecutionJob(_, _, _, JobPhase.Failed(diagnostics, _)) =>
          JobHeadline.Failed(j, diagnostics)
      })
      .getOrElse(JobHeadline.Idle)

  /** Fold one service event: a job's new state, or a ready notice. */
  def receive(event: ExecutionEvent): JobBoard = event match
    case ExecutionEvent.Changed(job) => copy(jobs = jobs.filterNot(_.id == job.id) :+ job)
    case ExecutionEvent.Ready(_)     => copy(shelf = shelf.receive(event))
    case _: ExecutionEvent.ArtifactsStored | _: ExecutionEvent.ArtifactsRefused => this

  /** Replace the jobs with a snapshot of the service's. */
  def withJobs(snapshot: Vector[ExecutionJob]): JobBoard = JobBoard.from(snapshot, shelf)

  def require(stamp: RunStamp): JobBoard = copy(shelf = shelf.require(stamp))

  /** Show `run`: only the pending run can be shown (S8.8). */
  def show(run: RunId): Either[ExecutionError, JobBoard] =
    shelf.show(run).map(s => copy(shelf = s))

  def dismiss(run: RunId): JobBoard = copy(shelf = shelf.dismiss(run))

object JobBoard:

  /** The board of `jobs` (the last state of each job id wins) and `shelf`. */
  def from(jobs: Vector[ExecutionJob], shelf: RunShelf): JobBoard =
    JobBoard(jobs.reverse.distinctBy(_.id).reverse, shelf)

  /** No jobs; the shelf of a document showing `shown`. */
  def empty(shown: Option[RunId]): JobBoard = JobBoard(Vector.empty, RunShelf.of(shown))

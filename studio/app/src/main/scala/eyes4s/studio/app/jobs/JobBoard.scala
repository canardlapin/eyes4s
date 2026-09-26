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

import eyes4s.studio.core.backend.{AnalysisRevision, RunId, StageKind}

// The job seam of the presentation layer. The execution service (S3.1)
// projects its jobs and its shelf of finished-but-not-shown runs into a
// [[JobBoard]] and dispatches it as `Intent.JobsChanged`; the app never
// runs, polls or cancels a job itself (it emits `AppEffect.CancelJob`).

/** A pair total as the execution service knows it. Only an exact total is a
  * number the user may read as "how many"; a total still being counted
  * renders as "counting…", never as a number.
  */
enum MeterTotal derives CanEqual:
  case Exact(pairs: Long)
  case AtMost(pairs: Long)
  case Counting

/** Why a pair meter was refused; each case names its operands. */
enum MeterError derives CanEqual:
  case NegativeDone(done: Long)
  case NegativeTotal(total: Long)
  case BeyondTotal(done: Long, total: Long)

  def message: String = this match
    case NegativeDone(d)   => s"Completed pairs $d is negative."
    case NegativeTotal(t)  => s"Pair total $t is negative."
    case BeyondTotal(d, t) => s"Completed pairs $d exceed the total $t."

/** How far a running job has got: its stage and pairs compared over every
  * scale (the jobs chip reads this).
  */
final case class PairMeter private (stage: StageKind, done: Long, total: MeterTotal)
    derives CanEqual:

  /** done / total, only when the total is exact and positive. */
  def fraction: Option[Double] = total match
    case MeterTotal.Exact(t) if t > 0 => Some(done.toDouble / t.toDouble)
    case _                            => None

object PairMeter:
  def of(stage: StageKind, done: Long, total: MeterTotal): Either[MeterError, PairMeter] =
    val bound = total match
      case MeterTotal.Exact(t)  => Some(t)
      case MeterTotal.AtMost(t) => Some(t)
      case MeterTotal.Counting  => None
    if done < 0 then Left(MeterError.NegativeDone(done))
    else
      bound match
        case Some(t) if t < 0    => Left(MeterError.NegativeTotal(t))
        case Some(t) if done > t => Left(MeterError.BeyondTotal(done, t))
        case _                   => Right(new PairMeter(stage, done, total))

/** A job's lifecycle, as the execution service reports it. */
enum JobPhase derives CanEqual:
  case Queued, Running, Cancelling, Succeeded

  /** `diagnostics` is the number of diagnostics the failure carries. */
  case Failed(diagnostics: Int)
  case Cancelled

  /** A newer run of a later revision completed first; this one's result will
    * never become current.
    */
  case Superseded

  def isActive: Boolean = this match
    case Queued | Running | Cancelling => true
    case _                             => false

/** One job, by the run it produces. */
final case class JobSummary(
    run: RunId,
    revision: AnalysisRevision,
    phase: JobPhase,
    meter: Option[PairMeter]
) derives CanEqual

/** What the jobs chip and the status bar's job slot show. */
enum JobHeadline derives CanEqual:
  case Idle
  case Active(job: JobSummary)
  case Failed(job: JobSummary, diagnostics: Int)

  /** A finished run waiting for the user to choose Show ("Run 8 ready — Show"). */
  case Ready(run: RunId)

/** Every job the session knows, and `ready`: finished runs that have not
  * replaced the shown run (the execution service's shelf). Results never
  * swap under the user; Show is an intent.
  */
final case class JobBoard(jobs: Vector[JobSummary], ready: Vector[RunId]) derives CanEqual:

  /** The newest active job; else the newest ready run; else the newest job
    * when it failed; else idle.
    */
  def headline: JobHeadline =
    val newest = jobs.sortBy(_.run.number)
    newest
      .findLast(_.phase.isActive)
      .map(JobHeadline.Active(_))
      .orElse(ready.lastOption.map(JobHeadline.Ready(_)))
      .orElse(newest.lastOption.collect { case j @ JobSummary(_, _, JobPhase.Failed(n), _) =>
        JobHeadline.Failed(j, n)
      })
      .getOrElse(JobHeadline.Idle)

  def active: Option[JobSummary] = jobs.sortBy(_.run.number).findLast(_.phase.isActive)

  def isReady(run: RunId): Boolean = ready.contains(run)

  /** The board once `run` has been shown: it leaves the shelf. */
  def shown(run: RunId): JobBoard = copy(ready = ready.filterNot(_ == run))

object JobBoard:
  val empty: JobBoard = JobBoard(Vector.empty, Vector.empty)

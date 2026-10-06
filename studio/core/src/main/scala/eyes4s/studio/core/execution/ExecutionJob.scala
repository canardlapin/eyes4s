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
import eyes4s.studio.core.artifacts.NativeBindingFacts
import eyes4s.studio.core.backend.ProtocolCodecs.portableLong
import io.circe.Codec

// ---------------------------------------------------------------------------
// Progress, as the Jobs chip reads it
// ---------------------------------------------------------------------------

/** What is known of a total while a stage runs. `Counting` is the honest
  * state before the total is known and is shown as "counting…" (DESIGN_SPEC
  * preamble); it is never rendered as a number.
  */
enum MeterTotal derives CanEqual, Codec.AsObject:
  case Exact(units: Long)
  case AtMost(units: Long)
  case Counting

object MeterTotal:
  /** Unresolved totals retain the existing counting presentation. */
  def of(total: ProgressTotal): MeterTotal = total match
    case ProgressTotal.Exact(units)                     => Exact(units)
    case ProgressTotal.AtMost(units)                    => AtMost(units)
    case ProgressTotal.Unknown | ProgressTotal.Counting => Counting

/** A count and its total in one unit. */
final case class Meter(unit: CountUnit, done: Long, total: MeterTotal)
    derives CanEqual,
      Codec.AsObject

/** One progress report of a job, read for display: the stage, the stage's own
  * meter and the run-wide pairs the chip shows ("Run 8 · Comparing · 21,400 /
  * 44,845 pairs"). `report` is the backend's value, unchanged, for the
  * freshness grammar (`SessionFacts.progress`).
  */
final case class ExecutionProgress(report: JobProgress) derives CanEqual, Codec.AsObject:
  def step: Long        = report.step
  def stage: StageKind  = report.segment.kind
  def segment: Segment  = report.segment
  def stageMeter: Meter =
    Meter(report.meter.unit, report.meter.done, MeterTotal.of(report.meter.total))
  def pairs: Meter =
    Meter(
      CountUnit.Pairs,
      report.totals.completedPairs,
      MeterTotal.of(report.totals.totalPairs)
    )
  def maps: Meter =
    Meter(CountUnit.Maps, report.totals.completedMaps, MeterTotal.of(report.totals.totalMaps))

  /** Run-wide units completed so far; never decreases along a job. */
  def completedUnits: Long = report.totals.completedMaps + report.totals.completedPairs

  /** Whether this report may follow `previous`: a later step that has not
    * lost units. A report that fails this is dropped, so progress never
    * moves back.
    */
  def follows(previous: ExecutionProgress): Boolean =
    step > previous.step && completedUnits >= previous.completedUnits

// ---------------------------------------------------------------------------
// Jobs
// ---------------------------------------------------------------------------

/** Where a job is. The last five cases are terminal and absorb every later
  * report. Only `Succeeded` makes a run available to Show; `Superseded` is a
  * completion whose stamp was no longer the one requested when it arrived.
  */
enum JobPhase derives CanEqual, Codec.AsObject:
  case Queued
  case Running(progress: ExecutionProgress)

  /** Cancel was requested; the backend has not yet settled the job. */
  case Cancelling(last: Option[ExecutionProgress])
  case Succeeded(last: ExecutionProgress)

  /** `diagnostics` is never empty and carries stable codes (E2E-13). */
  case Failed(diagnostics: Vector[StudioDiagnostic], last: Option[ExecutionProgress])
  case Cancelled(last: Option[ExecutionProgress])

  /** `current` is what was requested when the job was superseded: when its
    * completion arrived, or, with no progress, when the backend accepted a
    * submission that a newer intent had already replaced.
    */
  case Superseded(current: Option[RunStamp], last: Option[ExecutionProgress])

  def isTerminal: Boolean = this match
    case Queued | Running(_) | Cancelling(_) => false
    case _                                   => true

  /** The latest progress report the job made, if any. */
  def lastReport: Option[ExecutionProgress] = this match
    case Queued              => None
    case Running(p)          => Some(p)
    case Cancelling(last)    => last
    case Succeeded(last)     => Some(last)
    case Failed(_, last)     => last
    case Cancelled(last)     => last
    case Superseded(_, last) => last

/** One backend job as the service tracks it: the run it produces and the
  * stamp it was submitted with.
  */
final case class ExecutionJob(id: JobId, run: RunId, stamp: RunStamp, phase: JobPhase)
    derives CanEqual,
      Codec.AsObject

/** A completed run the user may now choose to Show ("Run 8 ready — Show").
  * Completion never promotes it by itself (S8.8).
  */
final case class RunReady(job: JobId, run: RunId, stamp: RunStamp)
    derives CanEqual,
      Codec.AsObject:
  def label: String = s"${run.label.capitalize} ready"

/** What the service publishes, in the order it happened. */
enum ExecutionEvent derives CanEqual, Codec.AsObject:
  /** A job's phase or progress changed. */
  case Changed(job: ExecutionJob)

  /** Published right after the `Changed` that made the job `Succeeded`. */
  case Ready(notice: RunReady)

  /** Verified files were stored after successful computation. Payload bytes
    * stay in the artifact transport, not the progress stream.
    */
  case ArtifactsStored(job: JobId, facts: NativeBindingFacts)

  /** Artifact delivery failed; computation remains successfully completed. */
  case ArtifactsRefused(job: JobId, run: RunId, diagnostic: StudioDiagnostic)

// ---------------------------------------------------------------------------
// Refusals and diagnostics
// ---------------------------------------------------------------------------

/** Why the execution service refused a request. Every case names its
  * operands; `code` is a stable identity in the `studio-execution` family.
  */
enum ExecutionError derives CanEqual:
  case Backend(error: BackendError)
  case UnknownJob(job: JobId, tracked: Vector[JobId])
  case AlreadyTracked(job: JobId)

  /** The backend started a job on another revision or dataset than the stamp
    * requested; the job was cancelled.
    */
  case StampMismatch(
      requested: RunStamp,
      job: JobId,
      revision: AnalysisRevision,
      dataset: DatasetRevision
  )
  case UnsavedRevision(revision: AnalysisRevision, saved: Vector[AnalysisRevision])
  case NotReady(run: RunId, pending: Option[RunId])

  def code: String = this match
    case Backend(e)                => e.code
    case UnknownJob(_, _)          => "studio-execution.unknown-job"
    case AlreadyTracked(_)         => "studio-execution.already-tracked"
    case StampMismatch(_, _, _, _) => "studio-execution.stamp-mismatch"
    case UnsavedRevision(_, _)     => "studio-execution.unsaved-revision"
    case NotReady(_, _)            => "studio-execution.not-ready"

  def message: String = this match
    case Backend(e)               => e.message
    case UnknownJob(job, tracked) =>
      s"Job ${job.number} is not tracked; the service tracks ${tracked.map(_.number).mkString(", ")}."
    case AlreadyTracked(job)                      => s"Job ${job.number} is already tracked."
    case StampMismatch(requested, job, rev, data) =>
      s"Job ${job.number} runs ${rev.label} on data ${data.label}, not ${requested.label}."
    case UnsavedRevision(revision, saved) =>
      s"${revision.label.capitalize} is not saved; the document has ${saved.map(_.label).mkString(", ")}."
    case NotReady(run, pending) =>
      s"${run.label.capitalize} is not ready to show; ${pending.fold("no run is ready")(r =>
          s"${r.label} is"
        )}."

/** The service's own diagnostics for a failed job, in the `studio-execution`
  * family. A backend's diagnostics keep their own codes.
  */
object ExecutionDiagnostics:

  private def host(code: String, job: JobId, message: String) =
    StudioDiagnostic(
      code,
      DiagnosticLevel.Error,
      DiagnosticOrigin.Host,
      Vector(DiagnosticLocus.Job(job)),
      message
    )

  /** The backend reported a failure without saying why. */
  def unexplained(job: JobId): StudioDiagnostic =
    host(
      "studio-execution.unexplained-failure",
      job,
      s"Job ${job.number} failed and the backend gave no diagnostic."
    )

  /** The job's progress stream ended, or broke, before the job settled. */
  def lost(job: JobId, reason: String): StudioDiagnostic =
    host(
      "studio-execution.lost-job",
      job,
      s"Job ${job.number} stopped reporting before it settled: $reason"
    )

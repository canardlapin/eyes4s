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

package eyes4s.studio.core.backend

import cats.Functor
import cats.syntax.functor.*
import fs2.Stream
import io.circe.Codec

/** Why the backend refused a request. Every case names its operands; `code` is
  * a stable identity in the `studio-backend` family.
  */
enum BackendError derives CanEqual, Codec.AsObject:
  case UnknownDataset(dataset: DatasetRevision, known: Vector[DatasetRevision])
  case UnknownRevision(revision: AnalysisRevision, known: Vector[AnalysisRevision])
  case UnknownRun(run: RunId, known: Vector[RunId])
  case UnknownJob(job: JobId, known: Vector[JobId])

  /** The backend knows the subject but holds no data for this request. */
  case Unavailable(subject: String, reason: String)

  /** The run exists but has no result to serve. */
  case NoResult(run: RunId, state: RunState)

  case UnknownReference(run: RunId, address: ResultAddress)
  case AlreadyRunning(revision: AnalysisRevision, job: JobId)

  def code: String = this match
    case UnknownDataset(_, _)   => "studio-backend.unknown-dataset"
    case UnknownRevision(_, _)  => "studio-backend.unknown-revision"
    case UnknownRun(_, _)       => "studio-backend.unknown-run"
    case UnknownJob(_, _)       => "studio-backend.unknown-job"
    case Unavailable(_, _)      => "studio-backend.unavailable"
    case NoResult(_, _)         => "studio-backend.no-result"
    case UnknownReference(_, _) => "studio-backend.unknown-reference"
    case AlreadyRunning(_, _)   => "studio-backend.already-running"

  def message: String = this match
    case UnknownDataset(d, known) =>
      s"No dataset ${d.label}; the backend has ${known.map(_.label).mkString(", ")}."
    case UnknownRevision(r, known) =>
      s"No analysis ${r.label}; the backend has ${known.map(_.label).mkString(", ")}."
    case UnknownRun(r, known) =>
      s"No ${r.label}; the backend has ${known.map(_.label).mkString(", ")}."
    case UnknownJob(j, known) =>
      s"No job ${j.number}; the backend has ${known.map(_.number).mkString(", ")}."
    case Unavailable(subject, reason) => s"$subject is unavailable: $reason."
    case NoResult(r, state)           => s"${r.label} has no result (state $state)."
    case UnknownReference(r, a)       => s"${r.label} has no result item $a."
    case AlreadyRunning(r, j)         =>
      s"Cannot submit ${r.label}: job ${j.number} is still running."

  def diagnostic: StudioDiagnostic =
    StudioDiagnostic(code, DiagnosticLevel.Error, Vector.empty, message)

/** Everything Eyes Studio asks of eyes4s (DESIGN_SPEC section 13, S3.0): the
  * admission summary and ledger, preview paging, jobs with progress, results,
  * inspection and provenance.
  *
  * Requests and responses are the serializable values of this package, so the
  * same protocol serves an in-process backend and an IPC sidecar
  * ([[BackendRequest]], [[StudyBackend.serve]]). `FakeStudyBackend` serves the
  * mock study; the real backend over eyes4s arrives in S3.7. Both pass
  * `BackendConformanceSuite`.
  *
  * Refusals are values. An effect fails only on a defect of the backend itself.
  */
trait StudyBackend[F[_]]:

  def admission(dataset: DatasetRevision): F[Either[BackendError, AdmissionSummary]]

  /** Every inventory trial's disposition, in inventory order. */
  def ledger(dataset: DatasetRevision, page: PageRequest): F[Either[BackendError, LedgerPage]]

  def preview(revision: AnalysisRevision): F[Either[BackendError, PreviewSummary]]

  /** The resolved design's queries, in focal (source) order. */
  def previewRows(
      revision: AnalysisRevision,
      page: PageRequest
  ): F[Either[BackendError, PreviewPage]]

  /** Every run, oldest first. */
  def runs: F[Vector[RunSummary]]

  /** Start a run of `revision` on its dataset. */
  def submit(revision: AnalysisRevision): F[Either[BackendError, JobStatus]]

  def jobs: F[Vector[JobStatus]]

  def job(id: JobId): F[Either[BackendError, JobStatus]]

  /** The job's progress, ending with exactly one `Finished`. A finished job's
    * stream is that single event.
    */
  def events(id: JobId): F[Either[BackendError, Stream[F, JobEvent]]]

  /** Request cancellation; returns the outcome once the job has settled. A
    * finished job's outcome is returned unchanged.
    */
  def cancel(id: JobId): F[Either[BackendError, JobOutcome]]

  /** The job's outcome, once it has settled. */
  def outcome(id: JobId): F[Either[BackendError, JobOutcome]]

  def result(run: RunId): F[Either[BackendError, ResultSummary]]

  /** Every requested query of the run, in focal (source) order. */
  def queries(run: RunId, page: PageRequest): F[Either[BackendError, QueryPage]]

  def inspect(run: RunId, address: ResultAddress): F[Either[BackendError, Inspection]]

  def provenance(run: RunId, address: ResultAddress): F[Either[BackendError, Provenance]]

/** A request of the [[StudyBackend]] protocol, one case per method; progress
  * streams travel separately as [[JobEvent]]s.
  */
enum BackendRequest derives CanEqual, Codec.AsObject:
  case Admission(dataset: DatasetRevision)
  case Ledger(dataset: DatasetRevision, page: PageRequest)
  case Preview(revision: AnalysisRevision)
  case PreviewRows(revision: AnalysisRevision, page: PageRequest)
  case Runs
  case Submit(revision: AnalysisRevision)
  case Jobs
  case Job(id: JobId)
  case Cancel(id: JobId)
  case Outcome(id: JobId)
  case Result(run: RunId)
  case Queries(run: RunId, page: PageRequest)
  case Inspect(run: RunId, address: ResultAddress)
  case ProvenanceOf(run: RunId, address: ResultAddress)

/** A response of the [[StudyBackend]] protocol. */
enum BackendResponse derives CanEqual, Codec.AsObject:
  case Refused(error: BackendError)
  case Admission(summary: AdmissionSummary)
  case Ledger(page: LedgerPage)
  case Preview(summary: PreviewSummary)
  case PreviewRows(page: PreviewPage)
  case Runs(runs: Vector[RunSummary])
  case Job(status: JobStatus)
  case Jobs(jobs: Vector[JobStatus])
  case Outcome(outcome: JobOutcome)
  case Result(summary: ResultSummary)
  case Queries(page: QueryPage)
  case Inspected(inspection: Inspection)
  case ProvenanceOf(provenance: Provenance)

object StudyBackend:

  /** Answer one protocol request: the server side of any transport. */
  def serve[F[_]: Functor](backend: StudyBackend[F])(
      request: BackendRequest
  ): F[BackendResponse] =
    import BackendRequest as Q
    import BackendResponse as A
    def answer[V](result: F[Either[BackendError, V]])(wrap: V => BackendResponse) =
      result.map(_.fold(A.Refused(_), wrap))
    request match
      case Q.Admission(d)       => answer(backend.admission(d))(A.Admission(_))
      case Q.Ledger(d, p)       => answer(backend.ledger(d, p))(A.Ledger(_))
      case Q.Preview(r)         => answer(backend.preview(r))(A.Preview(_))
      case Q.PreviewRows(r, p)  => answer(backend.previewRows(r, p))(A.PreviewRows(_))
      case Q.Runs               => backend.runs.map(A.Runs(_))
      case Q.Submit(r)          => answer(backend.submit(r))(A.Job(_))
      case Q.Jobs               => backend.jobs.map(A.Jobs(_))
      case Q.Job(j)             => answer(backend.job(j))(A.Job(_))
      case Q.Cancel(j)          => answer(backend.cancel(j))(A.Outcome(_))
      case Q.Outcome(j)         => answer(backend.outcome(j))(A.Outcome(_))
      case Q.Result(r)          => answer(backend.result(r))(A.Result(_))
      case Q.Queries(r, p)      => answer(backend.queries(r, p))(A.Queries(_))
      case Q.Inspect(r, a)      => answer(backend.inspect(r, a))(A.Inspected(_))
      case Q.ProvenanceOf(r, a) => answer(backend.provenance(r, a))(A.ProvenanceOf(_))

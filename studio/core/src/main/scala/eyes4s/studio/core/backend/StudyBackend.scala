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

import ProtocolCodecs.{byteDigest, portableLong}
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.document.SourcePath

import cats.Functor
import cats.syntax.functor.*
import eyes4s.studio.core.execution.RunStamp
import eyes4s.studio.core.preview.*
import fs2.Stream
import io.circe.{Codec, Decoder, Encoder}

/** Why the backend refused a request. Every case names its operands; `code` is
  * a stable identity in the `studio-backend` family.
  */
enum BackendError derives CanEqual, Codec.AsObject:
  case UnknownDataset(dataset: DatasetRevision, known: Vector[DatasetRevision])
  case UnknownRevision(revision: AnalysisRevision, known: Vector[AnalysisRevision])
  case UnknownRun(run: RunId, known: Vector[RunId])
  case UnknownJob(job: JobId, known: Vector[JobId])
  case UnknownPreview(preview: PreviewId, known: Vector[PreviewId])
  case PreviewNotReady(
      preview: PreviewId,
      completedParticipants: ParticipantCount,
      totalParticipants: ParticipantCount
  )
  case StalePreview(preview: PreviewId, captured: RunStamp, current: RunStamp)
  case TamperedPreview(supplied: PreviewReady, retained: PreviewReady)

  /** The backend knows the subject but holds no data for it. */
  case Unavailable(subject: DiagnosticLocus)

  /** The run exists but has no result to serve. */
  case NoResult(run: RunId, state: RunState)

  case UnknownReference(run: RunId, address: ResultAddress)
  case AlreadyRunning(revision: AnalysisRevision, job: JobId)
  case UnsupportedVersion(requested: ProtocolVersion, supported: ProtocolVersion)

  /** A request line that names its id but is not a request (protocol 1.1);
    * `excerpt` is its start, at most `WireFormat.ExcerptLength` characters.
    */
  case Malformed(excerpt: String, reason: String)

  /** A `Subscribe` reusing the id of a subscription still live on its
    * connection (protocol 1.1).
    */
  case DuplicateSubscription(request: RequestId)

  /** eyes4s refused `dataset`'s trial inventory, so it cannot be admitted
    * (S5.4): each issue names its records, trial and columns.
    */
  case InventoryRefused(dataset: DatasetRevision, issues: Vector[InventoryIssue])

  /** `trial` is not a trial of `dataset`'s inventory (protocol 1.6, S6.2). */
  case UnknownTrial(dataset: DatasetRevision, trial: TrialKey)

  /** A trial view the backend cannot give (protocol 1.6, S6.2): a value
    * that would not be valid, a trial its study fails, or a step eyes4s
    * refused. `error` names the trial and what failed.
    */
  case TrialViewRefused(error: TrialViewError)

  /** A page of `revision`'s source records the backend cannot give
    * (protocol 1.7, S6.4): a range outside the file, a value that would not
    * be valid, or a step eyes4s refused.
    */
  case SourceRecordsRefused(revision: AnalysisRevision, error: SourceRecordsError)

  /** `run` has no scale index `scale`; it computes `scales` (protocol 1.9). */
  case UnknownScale(run: RunId, scale: Int, scales: Vector[String])

  /** Exact source bytes differed from the revision that names them. */
  case SourceDigestMismatch(
      dataset: DatasetRevision,
      source: SourcePath,
      recorded: ByteDigest,
      read: ByteDigest
  )

  /** eyes4s refused a source outside the inventory error family. */
  case AdmissionRefused(dataset: DatasetRevision, source: String, reason: String)

  /** A recomputation differed from the run’s recorded canonical result. */
  case ResultDigestMismatch(run: RunId, recorded: ByteDigest, recomputed: ByteDigest)
  case RunDatasetMismatch(
      run: RunId,
      revision: AnalysisRevision,
      recorded: DatasetRevision,
      current: DatasetRevision
  )

  def code: String = this match
    case SourceDigestMismatch(_, _, _, _) => "studio-backend.source-digest-mismatch"
    case AdmissionRefused(_, _, _)        => "studio-backend.admission-refused"
    case ResultDigestMismatch(_, _, _)    => "studio-backend.result-digest-mismatch"
    case RunDatasetMismatch(_, _, _, _)   => "studio-backend.run-dataset-mismatch"
    case UnknownDataset(_, _)             => "studio-backend.unknown-dataset"
    case UnknownRevision(_, _)            => "studio-backend.unknown-revision"
    case UnknownRun(_, _)                 => "studio-backend.unknown-run"
    case UnknownJob(_, _)                 => "studio-backend.unknown-job"
    case UnknownPreview(_, _)             => "studio-backend.unknown-preview"
    case UnknownTrial(_, _)               => "studio-backend.unknown-trial"
    case TrialViewRefused(_)              => "studio-backend.trial-view-refused"
    case SourceRecordsRefused(_, _)       => "studio-backend.source-records-refused"
    case UnknownScale(_, _, _)            => "studio-backend.unknown-scale"
    case PreviewNotReady(_, _, _)         => "studio-backend.preview-not-ready"
    case StalePreview(_, _, _)            => "studio-backend.stale-preview"
    case TamperedPreview(_, _)            => "studio-backend.tampered-preview"
    case Unavailable(_)                   => "studio-backend.unavailable"
    case NoResult(_, _)                   => "studio-backend.no-result"
    case UnknownReference(_, _)           => "studio-backend.unknown-reference"
    case AlreadyRunning(_, _)             => "studio-backend.already-running"
    case UnsupportedVersion(_, _)         => "studio-backend.unsupported-version"
    case Malformed(_, _)                  => "studio-backend.malformed-request"
    case DuplicateSubscription(_)         => "studio-backend.duplicate-subscription"
    case InventoryRefused(_, _)           => "studio-backend.inventory-refused"

  def message: String = this match
    case SourceDigestMismatch(d, s, recorded, read) =>
      s"${d.label} source ${s.value}: recorded sha256 ${recorded.hex}, read sha256 ${read.hex}."
    case AdmissionRefused(d, source, reason)     => s"${d.label} source $source: $reason"
    case ResultDigestMismatch(r, recorded, made) =>
      s"${r.label} result: recorded sha256 ${recorded.hex}, recomputed sha256 ${made.hex}."
    case RunDatasetMismatch(r, revision, recorded, current) =>
      s"${r.label} was computed on data ${recorded.label}; ${revision.label} is on data ${current.label}."
    case UnknownDataset(d, known) =>
      s"No dataset ${d.label}; the backend has ${known.map(_.label).mkString(", ")}."
    case UnknownRevision(r, known) =>
      s"No analysis ${r.label}; the backend has ${known.map(_.label).mkString(", ")}."
    case UnknownRun(r, known) =>
      s"No ${r.label}; the backend has ${known.map(_.label).mkString(", ")}."
    case UnknownJob(j, known) =>
      s"No job ${j.number}; the backend has ${known.map(_.number).mkString(", ")}."
    case UnknownPreview(p, known) =>
      s"No preview ${p.value}; the backend has ${known.map(_.value).mkString(", ")}."
    case PreviewNotReady(p, done, total) =>
      s"Preview ${p.value} has counted ${done.value} of ${total.value} participants."
    case StalePreview(p, captured, current) =>
      s"Preview ${p.value} captured ${captured.label}, but the current input is ${current.label}."
    case TamperedPreview(supplied, retained) =>
      s"Preview receipt ${supplied.id.value} does not match retained preview ${retained.id.value}."
    case Unavailable(subject)   => s"The backend holds no data for ${subject.render}."
    case NoResult(r, state)     => s"${r.label} has no result (it is ${state.label})."
    case UnknownReference(r, a) => s"${r.label} has no result item: ${a.render}."
    case AlreadyRunning(r, j)   =>
      s"Cannot submit ${r.label}: job ${j.number} is still running."
    case UnsupportedVersion(requested, supported) =>
      s"Protocol ${requested.render} is not supported; this backend speaks ${supported.render}."
    case Malformed(excerpt, reason)     => s"Not a request ($reason): $excerpt"
    case DuplicateSubscription(request) =>
      s"Request ${request.value} is already a live subscription on this connection."
    case InventoryRefused(d, issues) =>
      s"The trial inventory of ${d.label} is refused: ${issues.map(_.message).mkString(" ")}"
    case UnknownTrial(d, t)         => s"${t.label} is not a trial of dataset ${d.label}."
    case UnknownScale(r, i, scales) =>
      s"${r.label} has no scale $i; it computes ${scales.size} (${scales.mkString(", ")})."
    case TrialViewRefused(e)        => e.message
    case SourceRecordsRefused(r, e) => s"${r.label}: ${e.message}"

  def diagnostic: StudioDiagnostic =
    val subject = this match
      case SourceDigestMismatch(d, s, _, _) =>
        Vector(DiagnosticLocus.Dataset(d), DiagnosticLocus.Artifact(s.value))
      case AdmissionRefused(d, source, _) =>
        Vector(DiagnosticLocus.Dataset(d), DiagnosticLocus.Artifact(source))
      case ResultDigestMismatch(r, _, _)                      => Vector(DiagnosticLocus.Run(r))
      case RunDatasetMismatch(r, revision, recorded, current) =>
        Vector(
          DiagnosticLocus.Run(r),
          DiagnosticLocus.Revision(revision),
          DiagnosticLocus.Dataset(recorded),
          DiagnosticLocus.Dataset(current)
        )
      case UnknownDataset(d, _)        => Vector(DiagnosticLocus.Dataset(d))
      case UnknownRevision(r, _)       => Vector(DiagnosticLocus.Revision(r))
      case UnknownRun(r, _)            => Vector(DiagnosticLocus.Run(r))
      case UnknownJob(j, _)            => Vector(DiagnosticLocus.Job(j))
      case UnknownPreview(_, _)        => Vector.empty
      case PreviewNotReady(_, _, _)    => Vector.empty
      case StalePreview(_, _, current) => Vector(DiagnosticLocus.Revision(current.revision))
      case TamperedPreview(_, _)       => Vector.empty
      case Unavailable(s)              => Vector(s)
      case NoResult(r, _)              => Vector(DiagnosticLocus.Run(r))
      case UnknownReference(r, a) => Vector(DiagnosticLocus.Run(r), DiagnosticLocus.Address(a))
      case AlreadyRunning(r, j)   => Vector(DiagnosticLocus.Revision(r), DiagnosticLocus.Job(j))
      case UnsupportedVersion(_, _) => Vector.empty
      case Malformed(_, _)          => Vector.empty
      case DuplicateSubscription(_) => Vector.empty
      case InventoryRefused(d, is)  => DiagnosticLocus.Dataset(d) +: is.flatMap(_.loci)
      case UnknownTrial(d, t)  => Vector(DiagnosticLocus.Dataset(d), DiagnosticLocus.Trial(t))
      case TrialViewRefused(e) => Vector(DiagnosticLocus.Trial(e.trial))
      case SourceRecordsRefused(r, _) => Vector(DiagnosticLocus.Revision(r))
      case UnknownScale(r, i, _) => Vector(DiagnosticLocus.Run(r), DiagnosticLocus.Scale(i))
    StudioDiagnostic(code, DiagnosticLevel.Error, DiagnosticOrigin.Host, subject, message)

/** Everything Eyes Studio asks of eyes4s (DESIGN_SPEC section 13, S3.0): the
  * admission summary and ledger, preview paging, jobs with progress, results,
  * inspection and provenance.
  *
  * Requests and responses are the serializable values of this package, so the
  * same protocol serves an in-process backend and an IPC sidecar
  * ([[Envelope]], [[StudyBackend.handle]]). No method blocks on a job: a job's
  * end arrives as the last frame of [[subscribe]] or by polling [[outcome]].
  * `FakeStudyBackend` serves the mock study; the real backend over eyes4s
  * arrives in S3.7. Both pass `BackendConformanceSuite`.
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

  /** Start a backend-owned preview and count at most `budget` participant
    * pages. Stopping consumption stops this bounded page; pages already
    * completed remain counted.
    */
  def previewCounting(
      revision: AnalysisRevision,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]]

  /** Continue one retained preview for at most `budget` participant pages. */
  def continuePreview(
      preview: PreviewId,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]]

  /** Submit exactly the prepared study retained for a ready receipt. */
  def submitPreview(ready: PreviewReady): F[Either[BackendError, JobStatus]]

  /** Every run, oldest first. */
  def runs: F[Vector[RunSummary]]

  /** Start a run of `revision` on its dataset. */
  def submit(revision: AnalysisRevision): F[Either[BackendError, JobStatus]]

  def jobs: F[Vector[JobStatus]]

  def job(id: JobId): F[Either[BackendError, JobStatus]]

  /** The job's progress from now on, ending with exactly one `Finished`. A
    * finished job's stream is that single event.
    */
  def subscribe(id: JobId): F[Either[BackendError, Stream[F, JobEvent]]]

  /** Request cancellation and return the job's status; cancellation settles
    * as a `Finished(Cancelled)` event unless the job has already ended.
    */
  def cancel(id: JobId): F[Either[BackendError, JobStatus]]

  /** The job's outcome if it has settled; never waits. */
  def outcome(id: JobId): F[Either[BackendError, Option[JobOutcome]]]

  def result(run: RunId): F[Either[BackendError, ResultSummary]]

  /** Every requested query of the run, in focal (source) order. */
  def queries(run: RunId, page: PageRequest): F[Either[BackendError, QueryPage]]

  def inspect(run: RunId, address: ResultAddress): F[Either[BackendError, Inspection]]

  def provenance(run: RunId, address: ResultAddress): F[Either[BackendError, Provenance]]

  /** Every pair row of `run` at scale index `scale`, a page at a time, in
    * focal order with each query's matched pair first (eyes4s `PairScores`,
    * protocol 1.9; the export bundle's comparisons.csv, S9.5).
    */
  def pairRows(run: RunId, scale: Int, page: PageRequest): F[Either[BackendError, PairRowPage]]

  /** The admitted fixations of `trial` under `revision`, in scanpath order,
    * each placed against the map by the revision's study (protocol 1.6,
    * S6.2). A trial without an admitted scanpath is `Unavailable`.
    */
  def trialFixations(
      revision: AnalysisRevision,
      trial: TrialKey
  ): F[Either[BackendError, TrialFixations]]

  /** eyes4s's preview density of `trial` under `revision` at σ 2°, with the
    * backend's isoline levels: a preview, not a result of any run (protocol
    * 1.6, S6.2).
    */
  def trialPreview(
      revision: AnalysisRevision,
      trial: TrialKey
  ): F[Either[BackendError, TrialPreview]]

  /** Records `from` to `from + count - 1` (at most [[SourceRecordPage.Limit]])
    * of the fixation file of `revision`'s dataset, in file order, each as the
    * file states it, in image pixels and degrees, and placed by the
    * revision's study (protocol 1.7, S6.4).
    */
  def sourceRecords(
      revision: AnalysisRevision,
      from: Int,
      count: Int
  ): F[Either[BackendError, SourceRecordPage]]

/** A request of the [[StudyBackend]] protocol, one case per method. */
enum BackendRequest derives CanEqual, Codec.AsObject:
  case Admission(dataset: DatasetRevision)
  case Ledger(dataset: DatasetRevision, page: PageRequest)
  case Preview(revision: AnalysisRevision)
  case PreviewRows(revision: AnalysisRevision, page: PageRequest)
  case PreviewCounting(revision: AnalysisRevision, budget: PreviewBudget)
  case ContinuePreview(preview: PreviewId, budget: PreviewBudget)
  case SubmitPreview(ready: PreviewReady)
  case Runs
  case Submit(revision: AnalysisRevision)
  case Jobs
  case Job(id: JobId)

  /** Answered by a stream of [[ServerFrame.Event]] frames. */
  case Subscribe(id: JobId)
  case Cancel(id: JobId)
  case Outcome(id: JobId)
  case Result(run: RunId)
  case Queries(run: RunId, page: PageRequest)
  case Inspect(run: RunId, address: ResultAddress)
  case ProvenanceOf(run: RunId, address: ResultAddress)

  /** End the subscription opened by request `subscription` on this
    * connection (protocol 1.1). Answered by [[BackendResponse.Unsubscribed]]
    * after the subscription's last frame.
    */
  case Unsubscribe(subscription: RequestId)

  /** Protocol 1.6. */
  case TrialFixationsOf(revision: AnalysisRevision, trial: TrialKey)

  /** Protocol 1.6. */
  case TrialPreviewOf(revision: AnalysisRevision, trial: TrialKey)

  /** Protocol 1.7. */
  case SourceRecordsOf(revision: AnalysisRevision, from: Int, count: Int)

  /** Protocol 1.9. */
  case PairRowsOf(run: RunId, scale: Int, page: PageRequest)

/** A response of the [[StudyBackend]] protocol. */
enum BackendResponse derives CanEqual, Codec.AsObject:
  case Refused(error: BackendError)
  case Admission(summary: AdmissionSummary)
  case Ledger(page: LedgerPage)
  case Preview(summary: PreviewSummary)
  case PreviewRows(page: PreviewPage)

  /** The following frames are the bounded preview stream. */
  case PreviewAccepted
  case Runs(runs: Vector[RunSummary])
  case Job(status: JobStatus)
  case Jobs(jobs: Vector[JobStatus])
  case Outcome(job: JobId, outcome: Option[JobOutcome])
  case Result(summary: ResultSummary)
  case Queries(page: QueryPage)
  case Inspected(inspection: Inspection)
  case ProvenanceOf(provenance: Provenance)

  /** No frame of `subscription` follows; `active` says whether it was still
    * running when the request arrived.
    */
  case Unsubscribed(subscription: RequestId, active: Boolean)

  /** Protocol 1.6. */
  case TrialFixationsOf(fixations: TrialFixations)

  /** Protocol 1.6. */
  case TrialPreviewOf(preview: TrialPreview)

  /** Protocol 1.7. */
  case SourceRecordsOf(page: SourceRecordPage)

  /** Protocol 1.9. */
  case PairRowsOf(page: PairRowPage)

/** A frame from backend to client: the one response to a request, or one
  * event of a subscription.
  */
enum ServerFrame derives CanEqual, Codec.AsObject:
  case Response(response: BackendResponse)
  case Event(event: JobEvent)
  case Preview(event: PreviewEvent)

/** The protocol's version. Client and backend are deployed together and speak
  * exactly one version: every minor so far has changed what an older body
  * decoder can read (1.2's `Counting`, 1.3's inventory join, 1.8's required
  * diagnostic fields), so a minor is no promise of compatibility. Both ends
  * read a frame's version before its body and refuse any other version,
  * naming both ([[BackendError.UnsupportedVersion]] from the server,
  * [[TransportError.Incompatible]] at the client), never as malformed (bead
  * bd-01M3JH3492J21SKMYYNZM93118).
  */
final case class ProtocolVersion(major: Int, minor: Int) derives CanEqual, Codec.AsObject:
  def render: String = s"$major.$minor"

  /** Whether a peer speaking this version can be served: only the current one. */
  def isCurrent: Boolean = this == ProtocolVersion.Current

object ProtocolVersion:
  /** 1.1 added `Unsubscribe`, `Unsubscribed`, `Malformed` and
    * `DuplicateSubscription` (S0.9). 1.2 added `ProgressTotal.Counting`. 1.3
    * replaced the admission summary's inventory counts with [[InventoryJoin]]
    * and added `InventoryRefused` (S5.4). 1.4 uses decimal strings for Long
    * values outside the safe JSON integer range. 1.5 adds the resolved
    * design's query counts to preview candidates and counts (S7.5). 1.6
    * adds a trial's admitted fixations and its preview map (S6.2). 1.7 adds
    * a revision's fixation-file records as pages (`sourceRecords`, S6.4).
    * 1.8 adds a diagnostic's affected trials, finding class and remedy
    * (S3.5). 1.9 adds a run's pair rows as pages (`pairRows`, S9.5) and
    * `UnknownScale`, and refuses any other version before reading a frame's
    * body. 1.10 adds the `TrialFailed` map placement, with its window
    * tally: a fixation in the window of a trial the study fails (eyes4s
    * UI-G G3); `InWindow` keeps its wire name `InMap`. Deploy client and
    * backend together.
    */
  val Current: ProtocolVersion = ProtocolVersion(1, 10)

/** A client's correlation id; every frame answering a request carries it. */
final case class RequestId(value: Long) derives CanEqual

object RequestId:
  given Codec[RequestId] = ProtocolCodecs.wrapper(RequestId(_), _.value)

/** One message on a transport, in either direction. */
final case class Envelope[A](version: ProtocolVersion, id: RequestId, body: A) derives CanEqual

object Envelope:
  def apply[A](id: RequestId, body: A): Envelope[A] =
    Envelope(ProtocolVersion.Current, id, body)

  given [A: Encoder]: Encoder.AsObject[Envelope[A]] =
    Encoder.forProduct3("version", "id", "body")(e => (e.version, e.id, e.body))

  given [A: Decoder]: Decoder[Envelope[A]] =
    Decoder.forProduct3("version", "id", "body")(Envelope.apply[A])

object StudyBackend:

  /** Answer one enveloped request: the server side of any transport. Every
    * frame carries the request's id and the current version.
    */
  def handle[F[_]: Functor](
      backend: StudyBackend[F]
  )(request: Envelope[BackendRequest]): Stream[F, Envelope[ServerFrame]] =
    val frames =
      if !request.version.isCurrent then
        Stream.emit(
          ServerFrame.Response(
            BackendResponse.Refused(
              BackendError.UnsupportedVersion(request.version, ProtocolVersion.Current)
            )
          )
        )
      else serve(backend)(request.body)
    frames.map(Envelope(request.id, _))

  /** Answer one request: a single response, or a subscription's events. */
  def serve[F[_]: Functor](backend: StudyBackend[F])(
      request: BackendRequest
  ): Stream[F, ServerFrame] =
    import BackendRequest as Q
    import BackendResponse as A
    def answer[V](result: F[Either[BackendError, V]])(wrap: V => BackendResponse) =
      Stream.eval(result.map(r => ServerFrame.Response(r.fold(A.Refused(_), wrap))))
    def always(result: F[BackendResponse]) = Stream.eval(result.map(ServerFrame.Response(_)))
    request match
      case Q.Admission(d)          => answer(backend.admission(d))(A.Admission(_))
      case Q.Ledger(d, p)          => answer(backend.ledger(d, p))(A.Ledger(_))
      case Q.Preview(r)            => answer(backend.preview(r))(A.Preview(_))
      case Q.PreviewRows(r, p)     => answer(backend.previewRows(r, p))(A.PreviewRows(_))
      case Q.PreviewCounting(r, b) =>
        previewFrames(backend.previewCounting(r, b))
      case Q.ContinuePreview(p, b) =>
        previewFrames(backend.continuePreview(p, b))
      case Q.SubmitPreview(r)       => answer(backend.submitPreview(r))(A.Job(_))
      case Q.Runs                   => always(backend.runs.map(A.Runs(_)))
      case Q.Submit(r)              => answer(backend.submit(r))(A.Job(_))
      case Q.Jobs                   => always(backend.jobs.map(A.Jobs(_)))
      case Q.Job(j)                 => answer(backend.job(j))(A.Job(_))
      case Q.Cancel(j)              => answer(backend.cancel(j))(A.Job(_))
      case Q.Outcome(j)             => answer(backend.outcome(j))(A.Outcome(j, _))
      case Q.Result(r)              => answer(backend.result(r))(A.Result(_))
      case Q.Queries(r, p)          => answer(backend.queries(r, p))(A.Queries(_))
      case Q.Inspect(r, a)          => answer(backend.inspect(r, a))(A.Inspected(_))
      case Q.ProvenanceOf(r, a)     => answer(backend.provenance(r, a))(A.ProvenanceOf(_))
      case Q.TrialFixationsOf(r, t) =>
        answer(backend.trialFixations(r, t))(A.TrialFixationsOf(_))
      case Q.TrialPreviewOf(r, t)     => answer(backend.trialPreview(r, t))(A.TrialPreviewOf(_))
      case Q.SourceRecordsOf(r, f, n) =>
        answer(backend.sourceRecords(r, f, n))(A.SourceRecordsOf(_))
      case Q.PairRowsOf(r, s, p) => answer(backend.pairRows(r, s, p))(A.PairRowsOf(_))
      // In process a subscription is ended by dropping its stream; only a
      // connection (SidecarServer) holds subscriptions to end.
      case Q.Unsubscribe(id) => Stream.emit(ServerFrame.Response(A.Unsubscribed(id, false)))
      case Q.Subscribe(j)    =>
        Stream.eval(backend.subscribe(j)).flatMap {
          case Left(e)       => Stream.emit(ServerFrame.Response(A.Refused(e)))
          case Right(events) => events.map(ServerFrame.Event(_))
        }

  private def previewFrames[F[_]](
      events: Stream[F, Either[BackendError, PreviewEvent]]
  ): Stream[F, ServerFrame] =
    events.pull.uncons1.flatMap {
      case None                   => fs2.Pull.done
      case Some((Left(error), _)) =>
        fs2.Pull.output1(ServerFrame.Response(BackendResponse.Refused(error)))
      case Some((Right(event), tail)) =>
        fs2.Pull.output1(ServerFrame.Response(BackendResponse.PreviewAccepted)) >>
          fs2.Pull.output1(ServerFrame.Preview(event)) >>
          tail
            .takeThrough(_.isRight)
            .map {
              case Left(error)  => ServerFrame.Response(BackendResponse.Refused(error))
              case Right(value) => ServerFrame.Preview(value)
            }
            .pull
            .echo
    }.stream

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

import cats.effect.kernel.{Concurrent, Deferred, Ref}
import cats.kernel.Eq
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.fs2.{Execution, StudyExecution}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{
  DatasetRevisionSpec,
  Recipe,
  RunLifecycle,
  Source,
  StudioDocument
}
import eyes4s.studio.core.preview.*
import fs2.Stream
import fs2.concurrent.SignallingRef
import java.nio.charset.StandardCharsets

/** What the host gives the real backend for a dataset revision: the exact
  * bytes of each of its source files and the stimulus registry (S2.10). The
  * backend reads no files itself (studio-core links for Scala.js); the
  * desktop host reads them, and a test serves fixtures/studio-golden.
  */
trait DatasetSources[F[_]]:
  /** The bytes of `source`, or `None` when the host does not hold them. */
  def bytes(dataset: DatasetRevisionSpec, source: Source): F[Option[IArray[Byte]]]

  /** The stimulus registry of `dataset`, or `None` when the host has none. */
  def assets(dataset: DatasetRevisionSpec): F[Option[AssetRegistry]]

/** The [[StudyBackend]] over eyes4s (ticket S3.7, docs/studio/plan/S3.7-slices.md).
  *
  * Slice 1 serves admission and the admission ledger: every count is
  * eyes4s's ([[RealAdmission]]). Every other operation answers a typed
  * refusal until its slice lands: an unknown dataset, revision, run or job
  * is refused as on the fake, and a known revision whose operation is not
  * yet served is `Unavailable` for that revision. The real backend never
  * serves a number it did not get from eyes4s.
  *
  * A dataset revision is admitted once, on first request, and its result is
  * kept: a dataset revision is immutable, so its admission cannot change.
  *
  * Slice 4 runs a revision's prepared study on eyes4s-fs2's runner
  * ([[RealExecution]]): one job at a time, each on its own fiber, its
  * progress and outcome eyes4s's.
  */
final class RealStudyBackend[F[_]] private (
    datasets: Map[DatasetRevision, DatasetRevisionSpec],
    revisions: Map[AnalysisRevision, (DatasetRevision, Recipe)],
    sources: DatasetSources[F],
    admitted: Ref[F, Map[DatasetRevision, AdmittedDataset]],
    prepared: Ref[F, Map[AnalysisRevision, RealPrepared]],
    state: SignallingRef[F, RealStudyBackend.Jobs[F]]
)(using F: Concurrent[F])
    extends StudyBackend[F]:
  import RealStudyBackend.*

  // ------------------------------------------------------------------ admission

  private def knownDatasets: Vector[DatasetRevision] = datasets.keys.toVector.sortBy(_.number)

  /** Only a successful admission is kept: a refusal (for example, bytes the
    * host does not hold yet) is answered again on the next request.
    */
  private def admission0(d: DatasetRevision): F[Either[BackendError, AdmittedDataset]] =
    datasets.get(d) match
      case None       => F.pure(Left(BackendError.UnknownDataset(d, knownDatasets)))
      case Some(spec) =>
        admitted.get.flatMap(_.get(d) match
          case Some(done) => F.pure(Right(done))
          case None       =>
            admit(spec).flatTap {
              case Right(done) => admitted.update(_.updated(d, done))
              case Left(_)     => F.unit
            })

  /** Until protocol 1.11 adds the typed `SourceDigestMismatch`, a source the
    * host cannot give, or whose bytes are not the ones the revision
    * recorded, is `Unavailable` naming the file and both digests.
    */
  private def admit(spec: DatasetRevisionSpec): F[Either[BackendError, AdmittedDataset]] =
    def unavailable(what: String) = BackendError.Unavailable(DiagnosticLocus.Artifact(what))
    def text(role: String, source: Option[Source]): F[Either[BackendError, String]] =
      source match
        case None =>
          F.pure(Left(BackendError.Unavailable(DiagnosticLocus.Field(s"$role source"))))
        case Some(s) =>
          sources.bytes(spec, s).map {
            case None    => Left(unavailable(s"${s.path.value}: the host holds no bytes"))
            case Some(b) =>
              val read = ByteDigest.sha256(b)
              if read == s.bytes then
                Right(new String(IArray.genericWrapArray(b).toArray, StandardCharsets.UTF_8))
              else
                Left(
                  unavailable(
                    s"${s.path.value}: recorded sha256 ${s.bytes.hex}, read sha256 ${read.hex}"
                  )
                )
          }
    (
      text("fixations", spec.sources.fixations),
      text("trials", spec.sources.trials),
      sources.assets(spec)
    ).mapN { (fixations, trials, assets) =>
      for
        f <- fixations
        t <- trials
        a <- assets.toRight(
          BackendError.Unavailable(DiagnosticLocus.Field("stimulus registry"))
        )
        result <- RealAdmission.admit(spec, f, t, a)
      yield result
    }

  def admission(dataset: DatasetRevision): F[Either[BackendError, AdmissionSummary]] =
    admission0(dataset).map(_.map(_.summary))

  def ledger(dataset: DatasetRevision, page: PageRequest): F[Either[BackendError, LedgerPage]] =
    admission0(dataset).map(_.map { done =>
      val entries = done.ledger.slice(page.offset, page.offset + page.size)
      LedgerPage(dataset, PageInfo.of(page, done.ledger.size, entries.size), entries)
    })

  // ------------------------------------------------------------------ not yet served

  private def knownRevisions: Vector[AnalysisRevision] =
    revisions.keys.toVector.sortBy(_.number)

  /** A known revision's operation that a later slice serves. */
  private def notYet[A](r: AnalysisRevision): F[Either[BackendError, A]] =
    F.pure(
      Left(
        if revisions.contains(r) then BackendError.Unavailable(DiagnosticLocus.Revision(r))
        else BackendError.UnknownRevision(r, knownRevisions)
      )
    )

  /** No run has a result yet: results are slice 5. */
  private def noRun[A](run: RunId): F[Either[BackendError, A]] =
    state.get.map(s =>
      Left(
        if s.runs.exists(_.run == run) then BackendError.Unavailable(DiagnosticLocus.Run(run))
        else BackendError.UnknownRun(run, s.runs.map(_.run))
      )
    )

  // ------------------------------------------------------------------ the prepared study

  /** The revision's study, configured from its recipe over its dataset's
    * admitted input and prepared once; a revision is immutable.
    */
  private def prepare(r: AnalysisRevision): F[Either[BackendError, RealPrepared]] =
    revisions.get(r) match
      case None              => F.pure(Left(BackendError.UnknownRevision(r, knownRevisions)))
      case Some((d, recipe)) =>
        prepared.get.flatMap(_.get(r) match
          case Some(done) => F.pure(Right(done))
          case None       =>
            admission0(d)
              .map(_.flatMap(RealPrepared.of(r, d, recipe, _)))
              .flatTap {
                case Right(done) => prepared.update(_.updated(r, done))
                case Left(_)     => F.unit
              })

  def preview(revision: AnalysisRevision): F[Either[BackendError, PreviewSummary]] =
    prepare(revision).map(_.map(_.summary))

  def previewRows(
      revision: AnalysisRevision,
      page: PageRequest
  ): F[Either[BackendError, PreviewPage]] = notYet(revision)

  def previewCounting(
      revision: AnalysisRevision,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]] =
    Stream.eval(notYet[PreviewEvent](revision))

  def continuePreview(
      preview: PreviewId,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]] =
    Stream.emit(Left(BackendError.UnknownPreview(preview, Vector.empty)))

  def submitPreview(ready: PreviewReady): F[Either[BackendError, JobStatus]] =
    F.pure(Left(BackendError.UnknownPreview(ready.id, Vector.empty)))

  // ------------------------------------------------------------------ jobs

  def runs: F[Vector[RunSummary]] = state.get.map(_.runs)

  def jobs: F[Vector[JobStatus]] = state.get.map(_.jobs)

  private def known[A](id: JobId)(f: (Jobs[F], JobStatus) => A): F[Either[BackendError, A]] =
    state.get.map(s =>
      s.job(id).map(f(s, _)).toRight(BackendError.UnknownJob(id, s.jobs.map(_.job)))
    )

  def job(id: JobId): F[Either[BackendError, JobStatus]] = known(id)((_, j) => j)

  def outcome(id: JobId): F[Either[BackendError, Option[JobOutcome]]] =
    known(id)((_, j) => finished(j.state))

  /** The job's state from now on: a report per change, then its one
    * `Finished`. A slow reader sees the latest report.
    */
  def subscribe(id: JobId): F[Either[BackendError, Stream[F, JobEvent]]] =
    known(id)((_, _) =>
      state.discrete
        .map(_.job(id).map(_.state))
        .unNone
        .changes(using Eq.fromUniversalEquals)
        .collect {
          case JobState.Running(p)  => JobEvent.Advanced(p)
          case JobState.Finished(o) => JobEvent.Finished(o)
        }
        .takeThrough {
          case JobEvent.Finished(_) => false
          case JobEvent.Advanced(_) => true
        }
    )

  /** Cancel through eyes4s's runner and wait until the job has settled, so
    * the returned status is final. A finished job is returned unchanged.
    */
  def cancel(id: JobId): F[Either[BackendError, JobStatus]] =
    known(id)((s, j) => (s.handles.get(id), j)).flatMap {
      case Left(e)                  => F.pure(Left(e))
      case Right((None, j))         => F.pure(Right(j))
      case Right((Some(handle), _)) =>
        handle.get.flatten >> state.discrete
          .map(_.job(id))
          .unNone
          .find(j => finished(j.state).isDefined)
          .compile
          .lastOrError
          .map(Right(_))
    }

  def submit(revision: AnalysisRevision): F[Either[BackendError, JobStatus]] =
    prepare(revision).flatMap {
      case Left(e)     => F.pure(Left(e))
      case Right(work) => start(work)
    }

  /** Reserve the job and its run, then start eyes4s's runner on its own
    * fiber. One job runs at a time.
    */
  private def start(work: RealPrepared): F[Either[BackendError, JobStatus]] =
    Deferred[F, F[Unit]].flatMap { handle =>
      state
        .modify { s =>
          s.jobs.find(j => finished(j.state).isEmpty) match
            case Some(active) =>
              (s, Left(BackendError.AlreadyRunning(work.revision, active.job)))
            case None =>
              val status = JobStatus(
                JobId(s.jobs.size + 1),
                RunId(s.nextRun),
                work.revision,
                work.dataset,
                JobState.Queued
              )
              val run = RunSummary(
                status.run,
                work.revision,
                work.dataset,
                RunState.Running(status.job)
              )
              (
                s.copy(
                  jobs = s.jobs :+ status,
                  runs = s.runs :+ run,
                  nextRun = s.nextRun + 1,
                  handles = s.handles.updated(status.job, handle)
                ),
                Right(status)
              )
        }
        .flatTap {
          case Right(status) => launch(work, status, handle)
          case Left(_)       => F.unit
        }
    }

  /** The runner's progress is folded into the job by one observer fiber; its
    * authoritative outcome settles the job on another, then releases the run.
    */
  private def launch(
      work: RealPrepared,
      status: JobStatus,
      handle: Deferred[F, F[Unit]]
  ): F[Unit] =
    val (job, run) = (status.job, status.run)
    val submission = StudyExecution.submissionWithId((job, run), work.work)
    F.uncancelable { _ =>
      Execution[F].start(submission).allocated.flatMap { case (running, release) =>
        val observe = running.progress
          .evalMap(p =>
            state.update(s =>
              s.live(job).fold(s) { carried =>
                RealExecution.report(job, run, p, carried, work.counts) match
                  case Right(Some((report, next))) =>
                    s.update(job, JobState.Running(report), next)
                  case Right(None) => s
                  case Left(d)     => s.copy(defects = s.defects.updated(job, d))
              }
            )
          )
          .compile
          .drain
        val settle = running.outcome.attempt.flatMap { outcome =>
          state.update { s =>
            s.live(job).fold(s) { carried =>
              val read = s.defects.get(job) match
                case Some(d) => Left(d)
                case None    =>
                  outcome.leftMap(e => RealExecution.Defect(s"eyes4s raised ${e.getMessage}"))
              val (end, runState, result) =
                RealExecution.settle(job, run, read, carried, work.counts)
              s.settle(job, run, end, runState, result)
            }
          }
        } >> release
        handle.complete(running.cancel) >> F.start(observe) >> F.start(settle).void
      }
    }

  def result(run: RunId): F[Either[BackendError, ResultSummary]] = noRun(run)

  def queries(run: RunId, page: PageRequest): F[Either[BackendError, QueryPage]] = noRun(run)

  def inspect(run: RunId, address: ResultAddress): F[Either[BackendError, Inspection]] =
    noRun(run)

  def provenance(run: RunId, address: ResultAddress): F[Either[BackendError, Provenance]] =
    noRun(run)

  def trialFixations(
      revision: AnalysisRevision,
      trial: TrialKey
  ): F[Either[BackendError, TrialFixations]] = notYet(revision)

  def trialPreview(
      revision: AnalysisRevision,
      trial: TrialKey
  ): F[Either[BackendError, TrialPreview]] = notYet(revision)

  def sourceRecords(
      revision: AnalysisRevision,
      from: Int,
      count: Int
  ): F[Either[BackendError, SourceRecordPage]] = notYet(revision)

object RealStudyBackend:

  /** The real backend over `document`'s dataset revisions and analysis
    * revisions (its saved revisions and its draft), with `sources` for their
    * files.
    */
  def create[F[_]: Concurrent](
      document: StudioDocument,
      sources: DatasetSources[F]
  ): F[RealStudyBackend[F]] =
    (
      Ref.of[F, Map[DatasetRevision, AdmittedDataset]](Map.empty),
      Ref.of[F, Map[AnalysisRevision, RealPrepared]](Map.empty),
      SignallingRef[F].of(Jobs.of[F](document))
    ).mapN { (admitted, prepared, state) =>
      val saved = document.analyses.map(a => a.id -> (a.dataset, a.recipe))
      // A draft's recipe is its changes applied to its base's recipe.
      val draft = document.draft.flatMap(d =>
        document
          .analysis(d.base)
          .map(base => d.id -> (d.dataset.getOrElse(base.dataset), d.recipe(base.recipe)))
      )
      new RealStudyBackend(
        document.datasets.map(d => d.id -> d).toMap,
        (saved ++ draft).toMap,
        sources,
        admitted,
        prepared,
        state
      )
    }

  private def finished(s: JobState): Option[JobOutcome] = s match
    case JobState.Finished(o) => Some(o)
    case _                    => None

  /** The backend's runs and jobs. `handles` cancel a job's eyes4s run;
    * `carried` and `defects` are a live job's telemetry; `results` are the
    * completed runs' eyes4s results (served from slice 5).
    */
  final case class Jobs[F[_]](
      runs: Vector[RunSummary],
      jobs: Vector[JobStatus],
      nextRun: Int,
      handles: Map[JobId, Deferred[F, F[Unit]]],
      carried: Map[JobId, RealExecution.Carried],
      defects: Map[JobId, RealExecution.Defect],
      results: Map[RunId, RealExecution.Result]
  ):
    def job(id: JobId): Option[JobStatus] = jobs.find(_.job == id)

    /** A job still running, with its carried counts. */
    def live(id: JobId): Option[RealExecution.Carried] =
      job(id)
        .filter(j => finished(j.state).isEmpty)
        .map(_ => carried.getOrElse(id, RealExecution.Carried.none))

    def update(id: JobId, next: JobState, counts: RealExecution.Carried): Jobs[F] =
      copy(
        jobs = jobs.map(j => if j.job == id then j.copy(state = next) else j),
        carried = carried.updated(id, counts)
      )

    def settle(
        id: JobId,
        run: RunId,
        outcome: JobOutcome,
        state: RunState,
        result: Option[RealExecution.Result]
    ): Jobs[F] =
      copy(
        jobs =
          jobs.map(j => if j.job == id then j.copy(state = JobState.Finished(outcome)) else j),
        runs = runs.map(r => if r.run == run then r.copy(state = state) else r),
        carried = carried - id,
        defects = defects - id,
        results = result.fold(results)(results.updated(run, _))
      )

  object Jobs:
    /** The document's runs as this backend reports them. The shown run is
      * current; another completed run is stale once a later dataset revision
      * is admitted, and completed otherwise. A run the document records as
      * running had a job of an earlier backend, which this one does not have
      * (a job id means nothing after a restart), so it is not listed; its
      * number is never reused.
      */
    def of[F[_]](document: StudioDocument): Jobs[F] =
      val latest = document.latestAdmitted.map(_.id)
      val runs   = document.runs.flatMap { r =>
        val state = r.state match
          case RunLifecycle.Completed if document.presentation.shownRun.contains(r.id) =>
            Some(RunState.Current)
          case RunLifecycle.Completed if !latest.contains(r.dataset) => Some(RunState.Stale)
          case RunLifecycle.Completed                                => Some(RunState.Completed)
          case RunLifecycle.Cancelled(at) => Some(RunState.Cancelled(at))
          case RunLifecycle.Failed        => Some(RunState.Failed)
          case RunLifecycle.Running       => None
        state.map(RunSummary(r.id, r.analysis, r.dataset, _))
      }
      Jobs(
        runs,
        Vector.empty,
        document.runs.map(_.id.number).maxOption.getOrElse(0) + 1,
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty
      )

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
import eyes4s.fs2.{Execution, RunOutcome, StudyExecution}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{
  DatasetRevisionSpec,
  Recipe,
  CoreBinding,
  RunLifecycle,
  RunRef,
  Source,
  StudioDocument
}
import eyes4s.studio.core.engine.StudioBuild
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
    state: SignallingRef[F, RealStudyBackend.Jobs[F]],
    documentRuns: Map[RunId, RunRef],
    inspected: Ref[F, Map[RunId, RealResults]],
    trialViews: Ref[F, Map[AnalysisRevision, RealTrialViews]]
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
      case Right(work) => start(work, Purpose.NewRun)
    }

  /** Reserve the job (and, for a new run, its run), then start eyes4s's
    * runner on its own fiber. One job runs at a time.
    */
  private def start(work: RealPrepared, purpose: Purpose): F[Either[BackendError, JobStatus]] =
    Deferred[F, F[Unit]].flatMap { handle =>
      state
        .modify { s =>
          s.jobs.find(j => finished(j.state).isEmpty) match
            case Some(active) =>
              (s, Left(BackendError.AlreadyRunning(work.revision, active.job)))
            case None =>
              val job               = JobId(s.jobs.size + 1)
              val (run, runs, next) = purpose match
                case Purpose.NewRun =>
                  val id = RunId(s.nextRun)
                  val r  = RunSummary(id, work.revision, work.dataset, RunState.Running(job))
                  (id, s.runs :+ r, s.nextRun + 1)
                // A recomputation serves an existing run, whose state it leaves alone.
                case Purpose.Recompute(ref) => (ref.id, s.runs, s.nextRun)
              val status = JobStatus(job, run, work.revision, work.dataset, JobState.Queued)
              (
                s.copy(
                  jobs = s.jobs :+ status,
                  runs = runs,
                  nextRun = next,
                  handles = s.handles.updated(job, handle),
                  purposes = s.purposes.updated(job, purpose)
                ),
                Right(status)
              )
        }
        .flatTap {
          case Right(status) => launch(work, status, handle, purpose)
          case Left(_)       => F.unit
        }
    }

  /** The runner's progress is folded into the job by one observer fiber; its
    * authoritative outcome settles the job on another, then releases the run.
    */
  private def launch(
      work: RealPrepared,
      status: JobStatus,
      handle: Deferred[F, F[Unit]],
      purpose: Purpose
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
          // A recomputed result is checked against the run's recorded
          // archive digest, when it has one, outside the state's lock.
          val mismatch = (purpose, outcome) match
            case (Purpose.Recompute(ref), Right(RunOutcome.Completed(_, _, result))) =>
              verify(ref, work, result)
            case _ => None
          state.update { s =>
            s.live(job).fold(s) { carried =>
              val read = s.defects.get(job) match
                case Some(d) => Left(d)
                case None    =>
                  outcome.leftMap(e => RealExecution.Defect(s"eyes4s raised ${e.getMessage}"))
              val (end, runState, result) =
                RealExecution.settle(job, run, read, carried, work.counts)
              (purpose, mismatch) match
                case (Purpose.NewRun, _) =>
                  val held = result.map(RealRun(work, _, RunOrigin.Computed))
                  s.settle(job, run, end, Some(runState), held)
                case (Purpose.Recompute(_), Some(refused)) =>
                  val failed =
                    JobOutcome.Failed(job, run, Vector(refused.diagnostic), end.progress)
                  s.settle(job, run, failed, None, None).refuse(run, refused)
                case (Purpose.Recompute(_), None) =>
                  val origin = RunOrigin.Recomputed(StudioBuild.eyes4sBaseVersion)
                  s.settle(job, run, end, None, result.map(RealRun(work, _, origin)))
            }
          }
        } >> release
        handle.complete(running.cancel) >> F.start(observe) >> F.start(settle).void
      }
    }

  // ------------------------------------------------------------------ results

  /** Until protocol 1.11's typed digest mismatch, a recomputed result whose
    * digest is not the one the run recorded is `Unavailable` naming the run
    * and both digests.
    */
  private def verify(
      ref: RunRef,
      work: RealPrepared,
      result: RealExecution.Result
  ): Option[BackendError] =
    ref.archive match
      case CoreBinding.Unbound()     => None
      case CoreBinding.Bound(digest) =>
        val recorded = digest.sha256.hex
        val made     = work.digest(result)
        Option.when(made != Right(recorded))(
          BackendError.Unavailable(
            DiagnosticLocus.Artifact(
              s"${ref.id.label} result: recorded sha256 $recorded, recomputed " +
                made.fold(e => s"no digest (${e.message})", d => s"sha256 $d")
            )
          )
        )

  /** The eyes4s result of `run`. A completed run of the document that this
    * backend did not compute is recomputed on first request (S3.7 slice 5,
    * option (a)): its revision's prepared study runs as an ordinary job, whose
    * progress the job surface shows, and until it completes the run is
    * `Unavailable`. The result is kept, marked recomputed with the eyes4s
    * release that produced it.
    */
  private[real] def held(run: RunId): F[Either[BackendError, RealRun]] =
    state.get.flatMap { s =>
      (s.results.get(run), s.refused.get(run), s.runs.find(_.run == run)) match
        case (Some(done), _, _)    => F.pure(Right(done))
        case (_, Some(refused), _) => F.pure(Left(refused))
        case (_, _, None) => F.pure(Left(BackendError.UnknownRun(run, s.runs.map(_.run))))
        case (_, _, Some(summary)) =>
          val pending = Left(BackendError.Unavailable(DiagnosticLocus.Run(run)))
          documentRuns.get(run).filter(_.state == RunLifecycle.Completed) match
            case None => F.pure(Left(BackendError.NoResult(run, summary.state)))
            case Some(_) if s.jobs.exists(j => j.run == run && finished(j.state).isEmpty) =>
              F.pure(pending)
            case Some(ref) =>
              prepare(ref.analysis).flatMap {
                case Left(e) => F.pure(Left(e))
                // Another job running defers the recomputation to a later request.
                case Right(work) => start(work, Purpose.Recompute(ref)).as(pending)
              }
    }

  /** The run's result as eyes4s inspects it, built once per run. */
  private def results(run: RunId): F[Either[BackendError, RealResults]] =
    held(run).flatMap {
      case Left(e)     => F.pure(Left(e))
      case Right(done) =>
        inspected.get.flatMap(_.get(run) match
          case Some(r) => F.pure(Right(r))
          case None    =>
            F.pure(RealResults.of(run, done)).flatTap {
              case Right(r) => inspected.update(_.updated(run, r))
              case Left(_)  => F.unit
            })
    }

  /** Until the summary's scalars move to `report(run, spec, scale)`
    * (bd-01M44P3SF52WPXQXFXSMXC5R8B), the real backend serves no
    * `ResultSummary`: its grouped and single-scale fields have no source.
    */
  def result(run: RunId): F[Either[BackendError, ResultSummary]] = noRun(run)

  def pairRows(
      run: RunId,
      scale: Int,
      page: PageRequest
  ): F[Either[BackendError, PairRowPage]] =
    results(run).map(_.flatMap(_.pairRows(scale, page)))

  def queries(run: RunId, page: PageRequest): F[Either[BackendError, QueryPage]] = noRun(run)

  /** An item eyes4s holds, as eyes4s holds it. A no-match query's contrast
    * row is eyes4s's failed row (missing operands) until protocol 1.11
    * reports it as NoMatch with eyes4s's unmatched reason; a query that was
    * not admitted has no item and is an unknown reference.
    */
  def inspect(run: RunId, address: ResultAddress): F[Either[BackendError, Inspection]] =
    results(run).map(_.flatMap(_.inspect(address)))

  def provenance(run: RunId, address: ResultAddress): F[Either[BackendError, Provenance]] =
    noRun(run)

  /** The revision's trial views, built once per revision. */
  private def views(r: AnalysisRevision): F[Either[BackendError, RealTrialViews]] =
    prepare(r).flatMap {
      case Left(e)     => F.pure(Left(e))
      case Right(work) =>
        trialViews.get.flatMap(_.get(r) match
          case Some(v) => F.pure(Right(v))
          case None    =>
            F.pure(RealTrialViews.of(work)).flatTap {
              case Right(v) => trialViews.update(_.updated(r, v))
              case Left(_)  => F.unit
            })
    }

  def trialFixations(
      revision: AnalysisRevision,
      trial: TrialKey
  ): F[Either[BackendError, TrialFixations]] =
    views(revision).map(_.flatMap(_.fixations(trial)))

  def trialPreview(
      revision: AnalysisRevision,
      trial: TrialKey
  ): F[Either[BackendError, TrialPreview]] = notYet(revision)

  def sourceRecords(
      revision: AnalysisRevision,
      from: Int,
      count: Int
  ): F[Either[BackendError, SourceRecordPage]] =
    views(revision).map(_.flatMap(_.sourceRecords(from, count)))

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
      SignallingRef[F].of(Jobs.of[F](document)),
      Ref.of[F, Map[RunId, RealResults]](Map.empty),
      Ref.of[F, Map[AnalysisRevision, RealTrialViews]](Map.empty)
    ).mapN { (admitted, prepared, state, inspected, trialViews) =>
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
        state,
        document.runs.map(r => r.id -> r).toMap,
        inspected,
        trialViews
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
      purposes: Map[JobId, Purpose],
      carried: Map[JobId, RealExecution.Carried],
      defects: Map[JobId, RealExecution.Defect],
      results: Map[RunId, RealRun],
      refused: Map[RunId, BackendError]
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

    /** Settle a job; `state` is the run's new state, `None` to leave it. */
    def settle(
        id: JobId,
        run: RunId,
        outcome: JobOutcome,
        state: Option[RunState],
        result: Option[RealRun]
    ): Jobs[F] =
      copy(
        jobs =
          jobs.map(j => if j.job == id then j.copy(state = JobState.Finished(outcome)) else j),
        runs = runs.map(r => if r.run == run then state.fold(r)(n => r.copy(state = n)) else r),
        carried = carried - id,
        defects = defects - id,
        results = result.fold(results)(results.updated(run, _))
      )

    /** A run whose result is refused for good (a recorded digest mismatch). */
    def refuse(run: RunId, error: BackendError): Jobs[F] =
      copy(refused = refused.updated(run, error))

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
        Map.empty,
        Map.empty,
        Map.empty
      )

  /** Why a job runs: a new run, or an existing run of the document
    * recomputed for its result.
    */
  enum Purpose derives CanEqual:
    case NewRun
    case Recompute(run: RunRef)

  /** Where a held result came from. A recomputed result names the eyes4s
    * release that produced it, so it never passes for the original
    * computation (provenance shows it, slice 6).
    */
  enum RunOrigin derives CanEqual:
    case Computed
    case Recomputed(eyes4sVersion: String)

  /** A run's eyes4s result with the prepared study that produced it. */
  final case class RealRun(
      prepared: RealPrepared,
      result: RealExecution.Result,
      origin: RunOrigin
  )

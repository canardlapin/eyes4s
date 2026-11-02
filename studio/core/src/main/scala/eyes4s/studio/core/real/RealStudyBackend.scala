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

import cats.effect.kernel.{Concurrent, Deferred, Ref, Resource}
import cats.effect.std.{Mutex, Supervisor}
import cats.kernel.Eq
import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, CanonicalDigest, ReportSources}
import eyes4s.fs2.{Execution, RunOutcome, StudyExecution}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.artifacts.{
  NativeArtifactSource,
  NativeArtifactBudget,
  NativeArtifactError,
  NativeCompletion,
  LocationNativePackage,
  AdmittedLocations
}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{
  DatasetRevisionSpec,
  LocationDatasetRevision,
  LocationAnalysisRevision,
  AnalysisDefinition,
  DatasetDefinition,
  AnalysisRevisionSpec,
  CoreBinding,
  RunLifecycle,
  RunRef,
  Source,
  SemanticIdentity,
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

  def locationBytes(
      @scala.annotation.unused dataset: LocationDatasetRevision,
      @scala.annotation.unused source: Source
  )(using
      F: cats.Applicative[F]
  ): F[Option[IArray[Byte]]] =
    F.pure(None)

  /** The stimulus registry of `dataset`, or `None` when the host has none. */
  def assets(dataset: DatasetRevisionSpec): F[Option[AssetRegistry]]

/** The [[StudyBackend]] over eyes4s (ticket S3.7, docs/studio/plan/S3.7-slices.md).
  *
  * Admission, previews, execution, retained results, reports and trial views
  * project eyes4s values into the Studio protocol. Unsupported representation
  * choices are explicit refusals; an unknown dataset, revision, run or job
  * is refused as on the fixture backend.
  *
  * A dataset revision is admitted once, on first request, and its result is
  * kept: a dataset revision is immutable, so its admission cannot change.
  *
  * Slice 4 runs a revision's prepared study on eyes4s-fs2's runner
  * ([[RealExecution]]): one job at a time, each on its own fiber, its
  * progress and outcome eyes4s's.
  */
final class RealStudyBackend[F[_]] private (
    private[real] val registry: Ref[F, RealRegistry],
    sources: DatasetSources[F],
    admitted: Ref[F, Map[DatasetRevision, AdmittedDataset]],
    locationAdmission: Ref[F, Option[AdmittedLocations]],
    locationAdmissionMutex: Mutex[F],
    prepared: Ref[F, Map[AnalysisRevision, RealPrepared]],
    previews: RealPreview[F],
    state: SignallingRef[F, RealStudyBackend.Jobs[F]],
    inspected: Ref[F, Map[RunId, RealResults]],
    trialViews: Ref[F, Map[AnalysisRevision, RealTrialViews]],
    initialReporting: Map[
      eyes4s.studio.core.document.ReportingId,
      eyes4s.studio.core.document.ReportingSpec
    ],
    navigationState: Ref[F, RealNavigator.State],
    archiveSource: Option[NativeArtifactSource[F]],
    restoring: Ref[F, Map[RunId, Deferred[F, Either[BackendError, Boolean]]]],
    supervisor: Supervisor[F],
    synchronization: Mutex[F],
    reports: RealReportGate[F]
)(using F: Concurrent[F])
    extends StudyBackend[F]
    with eyes4s.studio.core.artifacts.NativeArtifactProvider[F]:
  import RealStudyBackend.*

  lazy val navigator: RealNavigator[F] =
    new RealNavigator(initialReporting, held, prepare, navigationState, reports)

  /** Synchronize the authoritative document before host effects and reads.
    * Existing results/jobs/cursors keep their original scientific snapshots.
    */
  def synchronize(document: StudioDocument): F[Either[BackendError, Unit]] =
    synchronization.lock.surround {
      registry.get.flatMap { previous =>
        previous.synchronize(document) match
          case Left(error)   => F.pure(Left(error))
          case Right(change) =>
            val enriched = change.next.revisions.toVector.collect {
              case (revision, (_, recipe))
                  if (change.next.planBindings
                    .get(revision)
                    .exists(_.isInstanceOf[CoreBinding.Bound[?]]) &&
                    previous.planBindings
                      .get(revision) != change.next.planBindings.get(revision)) ||
                    (recipe.input.nonEmpty && previous.revisions
                      .get(revision)
                      .flatMap(_._2.input) != recipe.input) =>
                revision
            }
            val archives = change.next.documentRuns.values.toVector.filter(ref =>
              ref.archive match
                case CoreBinding.Bound(_) =>
                  previous.documentRuns.get(ref.id).forall(_.archive != ref.archive)
                case _ => false
            )
            val checkedArchives = state.get.map { current =>
              archives.traverse_ { ref =>
                current.results
                  .get(ref.id)
                  .toRight(
                    BackendError.RegistryRefused(
                      DiagnosticLocus.Run(ref.id),
                      "A new archive binding requires this run's retained native result."
                    )
                  )
                  .flatMap {
                    case done: RealRun     => verify(ref, done.prepared, done.result).toLeft(())
                    case done: LocationRun =>
                      LocationNativePackage
                        .create(ref.id, done.admitted, done.analysis, done.result)
                        .leftMap(e =>
                          BackendError.RegistryRefused(DiagnosticLocus.Run(ref.id), e.message)
                        )
                        .flatMap(p =>
                          Either.cond(
                            ref.archive == CoreBinding.Bound(p.facts.result),
                            (),
                            BackendError.RegistryRefused(
                              DiagnosticLocus.Run(ref.id),
                              "Spatial archive binding differs from the retained result."
                            )
                          )
                        )
                  }
              }
            }
            enriched
              .traverse(revision => configured(change.next, revision).map(_.void))
              .map(_.sequence)
              .flatMap {
                case Left(error) => F.pure(Left(error))
                case Right(_)    => checkedArchives
              }
              .flatMap {
                case Left(error) => F.pure(Left(error))
                case Right(_)    =>
                  admitted.update(_ -- change.datasets) >>
                    prepared.update(_ -- change.revisions) >>
                    trialViews.update(_ -- change.revisions) >>
                    previews.invalidate(change.revisions) >>
                    navigator.synchronizeReporting(document.reporting) >>
                    registry.set(change.next) >>
                    state
                      .update { current =>
                        val declared = Jobs.of[F](document).runs
                        val known    = current.runs.map(_.run).toSet
                        val active   =
                          current.jobs.filter(j => finished(j.state).isEmpty).map(_.run).toSet
                        val updated = current.runs.map(r =>
                          if active(r.run) then r
                          else declared.find(_.run == r.run).getOrElse(r)
                        )
                        val occupied = document.runs
                          .filterNot(r => change.next.reservations(r.id))
                          .map(_.id.number)
                        current
                          .copy(
                            runs = updated ++ declared.filterNot(r => known(r.run)),
                            nextRun =
                              math.max(current.nextRun, occupied.maxOption.fold(1)(_ + 1))
                          )
                          .releaseArchivedLocations(if archiveSource.nonEmpty then
                            archives.map(_.id).toSet
                          else Set.empty)
                      }
                      .as(Right(()))
              }
      }
    }

  // ------------------------------------------------------------------ admission

  def verify(
      dataset: DatasetRevision,
      content: CanonicalDigest[DatasetRevisionSpec]
  ): F[Either[BackendError, AdmissionSummary]] =
    registry.get.flatMap { snapshot =>
      snapshot.datasets.get(dataset) match
        case None => F.pure(Left(BackendError.UnknownDataset(dataset, snapshot.knownDatasets)))
        case Some(spec) =>
          DatasetRevisionSpec.contentDigest(spec) match
            case Left(error) =>
              F.pure(
                Left(
                  BackendError.Unavailable(
                    DiagnosticLocus.Artifact(s"${dataset.label} content: ${error.message}")
                  )
                )
              )
            case Right(held) if held != content =>
              F.pure(Left(BackendError.ContentMismatch(dataset, content, held)))
            case Right(_) => admission(dataset)
    }

  def recipeForm(request: RecipeEditRequest): F[Either[BackendError, RecipeFormResult]] =
    admission0(request.dataset).map(
      _.flatMap(admitted =>
        RealRecipeForm.serve(
          request,
          admitted.screen,
          admitted.input,
          admitted.evidence.inventory
        )
      )
    )

  def placement(spec: DatasetRevisionSpec): F[Either[BackendError, PlacementPreview]] =
    spec.sources.fixations match
      case None =>
        F.pure(Left(BackendError.PlacementRefused(spec.id, "it has no fixation source")))
      case Some(source) =>
        sources.bytes(spec, source).map {
          case None =>
            Left(
              BackendError
                .PlacementRefused(spec.id, s"${source.path.value}: the host holds no bytes")
            )
          case Some(bytes) =>
            val read = ByteDigest.sha256(bytes)
            if read != source.bytes then
              Left(BackendError.SourceDigestMismatch(spec.id, source.path, source.bytes, read))
            else RealPlacement.place(spec, bytes)
        }

  /** Only a successful admission for this exact live declaration is cached. */
  private def admission0(d: DatasetRevision): F[Either[BackendError, AdmittedDataset]] =
    registry.get.flatMap { snapshot =>
      snapshot.datasets.get(d) match
        case None       => F.pure(Left(BackendError.UnknownDataset(d, snapshot.knownDatasets)))
        case Some(spec) =>
          admitted.get.flatMap(_.get(d).filter(_.spec == spec) match
            case Some(done) => F.pure(Right(done))
            case None       =>
              admit(spec).flatTap {
                case Right(done) => admitted.update(_.updated(d, done))
                case Left(_)     => F.unit
              })
    }

  /** Verify the exact source bytes before admission. */
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
                  BackendError.SourceDigestMismatch(spec.id, s.path, s.bytes, read)
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
        a <- assets match
          case Some(registry)                                     => Right(registry)
          case None if spec.inventory.flatMap(_.displays).isEmpty =>
            AssetRegistry
              .fromRows(spec, Vector.empty, Vector.empty)
              .left
              .map(error =>
                BackendError.RegistryRefused(DiagnosticLocus.Dataset(spec.id), error.message)
              )
          case None =>
            Left(BackendError.Unavailable(DiagnosticLocus.Field("stimulus registry")))
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

  // ------------------------------------------------------------------ the prepared study

  /** Cached work must match the live recipe and admitted declaration. Old
    * work is retained in its job/result/cursor snapshot, never rewritten.
    */
  private def prepare(r: AnalysisRevision): F[Either[BackendError, RealPrepared]] =
    registry.get.flatMap { snapshot =>
      snapshot.revisions.get(r) match
        case None => F.pure(Left(BackendError.UnknownRevision(r, snapshot.knownRevisions)))
        case Some((d, recipe)) =>
          prepared.get.flatMap(
            _.get(r).filter(done =>
              done.dataset == d && done.recipe == recipe &&
                snapshot.datasets.get(d).contains(done.admitted.spec)
            ) match
              case Some(done) => F.pure(Right(done))
              case None       =>
                admission0(d)
                  .map(
                    _.flatMap(admission =>
                      RealPrepared
                        .configure(r, d, recipe, admission)
                        .flatMap(c => checkBinding(c, snapshot))
                        .flatMap(c =>
                          c.work.counts
                            .leftMap(RealPrepared.refused(r, "counts"))
                            .flatMap(RealPrepared.fromCounts(c, _))
                        )
                    )
                  )
                  .flatTap {
                    case Right(done) => prepared.update(_.updated(r, done))
                    case Left(_)     => F.unit
                  }
          )
    }

  def preview(revision: AnalysisRevision): F[Either[BackendError, PreviewSummary]] =
    prepare(revision)
      .flatTap {
        case Right(work) => previews.remember(work)
        case Left(_)     => F.unit
      }
      .map(_.map(_.summary))

  private def configuration(
      revision: AnalysisRevision
  ): F[Either[BackendError, RealConfigured]] =
    registry.get.flatMap(snapshot => configured(snapshot, revision))

  private def configured(
      snapshot: RealRegistry,
      revision: AnalysisRevision
  ): F[Either[BackendError, RealConfigured]] =
    snapshot.revisions.get(revision) match
      case None => F.pure(Left(BackendError.UnknownRevision(revision, snapshot.knownRevisions)))
      case Some((dataset, recipe)) =>
        snapshot.datasets.get(dataset) match
          case None =>
            F.pure(Left(BackendError.UnknownDataset(dataset, snapshot.knownDatasets)))
          case Some(spec) =>
            admitted.get
              .flatMap(_.get(dataset).filter(_.spec == spec) match
                case Some(ready) => F.pure(Right(ready))
                case None        => admit(spec))
              .map(
                _.flatMap(admission =>
                  RealPrepared
                    .configure(revision, dataset, recipe, admission)
                    .flatMap(c => checkBinding(c, snapshot))
                )
              )

  /** Saved source and plan bindings must describe the native preparation. */
  private def checkBinding(
      configured: RealConfigured,
      snapshot: RealRegistry
  ): Either[BackendError, RealConfigured] =
    val source = SemanticIdentity.fromCore(configured.admitted.evidence.source.records)
    val input  = configured.recipe.input match
      case Some(recorded) if recorded != source =>
        Left(
          BackendError.RegistryRefused(
            DiagnosticLocus.Revision(configured.revision),
            s"Source records recorded ${recorded.value}, prepared ${source.value} for ${configured.dataset.label}."
          )
        )
      case _ => Right(())
    input.flatMap { _ =>
      snapshot.planBindings.get(configured.revision) match
        case Some(CoreBinding.Bound(recorded)) =>
          RealPreview.stamp(configured).flatMap { stamp =>
            stamp.plan match
              case CoreBinding.Bound(prepared) if prepared.sha256 == recorded.sha256 =>
                Right(configured)
              case found =>
                Left(
                  BackendError.RegistryRefused(
                    DiagnosticLocus.Revision(configured.revision),
                    s"Plan digest recorded ${recorded.sha256.hex}, prepared ${found.render}."
                  )
                )
          }
        case _ => Right(configured)
    }

  def previewRows(
      revision: AnalysisRevision,
      page: PageRequest
  ): F[Either[BackendError, PreviewPage]] =
    registry.get.flatMap(snapshot =>
      if snapshot.revisions.contains(revision) then previews.rows(revision, page)
      else F.pure(Left(BackendError.UnknownRevision(revision, snapshot.knownRevisions)))
    )

  def previewCounting(
      revision: AnalysisRevision,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]] =
    Stream.eval(configuration(revision)).flatMap {
      case Left(error)       => Stream.emit(Left(error))
      case Right(configured) => previews.begin(configured, budget)
    }

  def continuePreview(
      preview: PreviewId,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]] = previews.continue(preview, budget)

  def submitPreview(ready: PreviewReady): F[Either[BackendError, JobStatus]] =
    synchronization.lock.surround {
      previews
        .accept(ready)(revision => configuration(revision).map(_.flatMap(RealPreview.stamp)))
        .flatMap {
          case Left(error) => F.pure(Left(error))
          case Right(work) =>
            prepared.update(_.updated(work.revision, work)) >> start(work, Purpose.NewRun)
        }
    }

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
    synchronization.lock.surround(submit0(revision))

  override def submitStamped(
      stamp: eyes4s.studio.core.execution.ExecutionStamp
  )(using cats.Monad[F]): F[Either[BackendError, JobStatus]] =
    synchronization.lock.surround {
      registry.get.flatMap { snapshot =>
        val agrees = (snapshot.document.analysisDefinition(stamp.revision), stamp) match
          case (
                Some(value: LocationAnalysisRevision),
                requested: eyes4s.studio.core.execution.LocationRunStamp
              ) =>
            eyes4s.studio.core.execution
              .LocationRunStamp(value.id, value.dataset, value.plan, value.recipe.input)
              .agreesWithDeclarations(requested)
          case (
                Some(value: AnalysisRevisionSpec),
                requested: eyes4s.studio.core.execution.RunStamp
              ) =>
            value.dataset == requested.dataset
          case (None, requested: eyes4s.studio.core.execution.RunStamp) =>
            snapshot.revisions.get(requested.revision).exists(_._1 == requested.dataset)
          case _ => false
        if agrees then submit0(stamp.revision)
        else
          F.pure(
            Left(
              BackendError.RegistryRefused(
                DiagnosticLocus.Revision(stamp.revision),
                "Requested scientific kind or identities differ from the saved analysis."
              )
            )
          )
      }
    }

  private def submit0(revision: AnalysisRevision): F[Either[BackendError, JobStatus]] =
    registry.get.flatMap { snapshot =>
      snapshot.document.analysisDefinition(revision) match
        case Some(value: LocationAnalysisRevision) =>
          prepareLocations(snapshot, value).flatMap {
            case Left(error) => F.pure(Left(error))
            case Right(work) =>
              reserve(
                value.id,
                value.dataset,
                Purpose.NewRun,
                (status, handle) => launchLocations(work, value, status, handle)
              )
          }
        case _ =>
          prepare(revision).flatMap {
            case Left(e)     => F.pure(Left(e))
            case Right(work) => start(work, Purpose.NewRun)
          }
    }

  /** Reserve the job (and, for a new run, its run), then start eyes4s's
    * runner on its own fiber. One job runs at a time.
    */
  private def start(work: RealPrepared, purpose: Purpose): F[Either[BackendError, JobStatus]] =
    work.executionRefusal match
      case Some(error) => F.pure(Left(error))
      case None        => startChecked(work, purpose)

  private def startChecked(
      work: RealPrepared,
      purpose: Purpose
  ): F[Either[BackendError, JobStatus]] =
    reserve(
      work.revision,
      work.dataset,
      purpose,
      (status, handle) => launch(work, status, handle, purpose)
    )

  private def reserve(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      purpose: Purpose,
      launchReserved: (JobStatus, Deferred[F, F[Unit]]) => F[Unit]
  ): F[Either[BackendError, JobStatus]] =
    registry.get.flatMap { snapshot =>
      Deferred[F, F[Unit]].flatMap { handle =>
        state
          .modify { s =>
            s.jobs.find(j => finished(j.state).isEmpty) match
              case Some(active) =>
                (s, Left(BackendError.AlreadyRunning(revision, active.job)))
              case None =>
                val reserved = purpose match
                  case Purpose.NewRun =>
                    snapshot.reserved(
                      revision,
                      dataset,
                      s.jobs.map(_.run).toSet ++ s.results.keySet
                    )
                  case Purpose.Recompute(_) => Right(None)
                reserved match
                  case Left(error)      => (s, Left(error))
                  case Right(requested) =>
                    val job               = JobId(s.jobs.size + 1)
                    val (run, runs, next) = purpose match
                      case Purpose.NewRun =>
                        val id = requested.getOrElse(RunId(s.nextRun))
                        val r  =
                          RunSummary(id, revision, dataset, RunState.Running(job))
                        (
                          id,
                          s.runs.filterNot(_.run == id) :+ r,
                          math.max(s.nextRun, id.number + 1)
                        )
                      case Purpose.Recompute(ref) => (ref.id, s.runs, s.nextRun)
                    val status =
                      JobStatus(job, run, revision, dataset, JobState.Queued)
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
            case Right(status) => launchReserved(status, handle)
            case Left(_)       => F.unit
          }
      }
    }

  def locationAdmission(dataset: DatasetRevision): F[Either[BackendError, AdmittedLocations]] =
    registry.get.flatMap(snapshot =>
      snapshot.document.datasetDefinition(dataset) match
        case Some(value: LocationDatasetRevision) => readLocations(value)
        case _ => F.pure(Left(BackendError.UnknownDataset(dataset, snapshot.knownDatasets)))
    )

  private def readLocations(
      dataset: LocationDatasetRevision
  ): F[Either[BackendError, AdmittedLocations]] =
    locationAdmissionMutex.lock.surround {
      locationAdmission.get.flatMap {
        case Some(value) if value.dataset == dataset => F.pure(Right(value))
        case _                                       =>
          dataset.sourceFiles
            .traverse(source =>
              sources
                .locationBytes(dataset, source)
                .map(data =>
                  data
                    .toRight(
                      BackendError.AdmissionRefused(
                        dataset.id,
                        source.path.value,
                        "Source bytes are unavailable."
                      )
                    )
                    .map(value => source.role -> IArray.genericWrapArray(value).toIndexedSeq)
                )
            )
            .map(
              _.sequence.flatMap(files =>
                LocationNativePackage
                  .admit(dataset, files)
                  .leftMap(e =>
                    BackendError.AdmissionRefused(dataset.id, "spatial bundle", e.message)
                  )
              )
            )
            .flatTap(_.traverse_(value => locationAdmission.set(Some(value))))
      }
    }

  /** Keep one immutable source closure for the currently used spatial dataset.
    * Its key is the whole revision, including every source digest and profile.
    */
  private def sharedLocationAdmission(value: AdmittedLocations): F[AdmittedLocations] =
    locationAdmissionMutex.lock.surround(
      locationAdmission.modify {
        case Some(cached) if cached.dataset == value.dataset => Some(cached) -> cached
        case _                                               => Some(value)  -> value
      }
    )

  private def prepareLocations(
      snapshot: RealRegistry,
      revision: LocationAnalysisRevision
  ): F[Either[BackendError, AdmittedLocations]] =
    snapshot.document.datasetDefinition(revision.dataset) match
      case Some(dataset: LocationDatasetRevision) =>
        readLocations(dataset).map(
          _.flatMap(admitted =>
            Either.cond(
              revision.recipe.profile == dataset.profile &&
                revision.recipe.input == CoreBinding.Bound(admitted.inputCanonical) &&
                revision.plan == CoreBinding.Bound(
                  admitted.planCanonical
                ) && admitted.plan.queries.nonEmpty,
              admitted,
              BackendError.RegistryRefused(
                DiagnosticLocus.Revision(revision.id),
                "Spatial input, profile or plan differs from the admitted sources, or no query is executable."
              )
            )
          )
        )
      case _ =>
        F.pure(Left(BackendError.UnknownDataset(revision.dataset, snapshot.knownDatasets)))

  private def launchLocations(
      work: AdmittedLocations,
      revision: LocationAnalysisRevision,
      status: JobStatus,
      handle: Deferred[F, F[Unit]]
  ): F[Unit] =
    val job                                                                   = status.job
    val run                                                                   = status.run
    def report(cursor: eyes4s.plan.LocationExecution): F[LocationJobProgress] =
      F.fromEither(
        LocationJobProgress
          .of(
            job,
            run,
            cursor.completedMaps.toLong + cursor.completedQueries.toLong,
            cursor.completedMaps.toLong,
            cursor.totalMaps.toLong,
            cursor.completedQueries.toLong,
            cursor.totalQueries.toLong
          )
          .leftMap(e => new IllegalStateException(e.toString))
      )
    def last(current: Jobs[F]): Option[JobProgressValue] = current
      .job(job)
      .flatMap(_.state match
        case JobState.Running(progress) => Some(progress)
        case _                          => None)
    val compute = F.tailRecM(work.plan.execution) { cursor =>
      report(cursor)
        .flatMap(progress =>
          state
            .update { current =>
              if current
                  .live(job)
                  .isDefined && (progress.step % 128 == 0 || cursor.complete || (cursor.completedMaps == cursor.totalMaps && cursor.completedQueries == 0))
              then
                current.copy(jobs =
                  current.jobs.map(j =>
                    if j.job == job then j.copy(state = JobState.Running(progress)) else j
                  )
                )
              else current
            }
            .as(progress)
        )
        .flatMap { progress =>
          cursor.result match
            case Some(result) =>
              state
                .update { current =>
                  if current.live(job).isEmpty then current
                  else
                    current.settle(
                      job,
                      run,
                      JobOutcome.Completed(job, run, progress),
                      Some(RunState.Completed),
                      Some(LocationRun(work, revision, result, RunOrigin.Computed))
                    )
                }
                .as(Right(()))
            case None => F.cede >> F.catchNonFatal(cursor.step).map(Left(_))
        }
    }
    val guarded = F
      .onCancel(
        compute,
        state.update { current =>
          if current.live(job).isEmpty then current
          else
            current.settle(
              job,
              run,
              JobOutcome.Cancelled(job, run, last(current)),
              Some(RunState.Cancelled(None)),
              None
            )
        }
      )
      .handleErrorWith(error =>
        state.update { current =>
          val diagnostic = NativeArtifactError
            .Defect(run, "spatial execution", error.getClass.getName)
            .diagnostic
          if current.live(job).isEmpty then current
          else
            current.settle(
              job,
              run,
              JobOutcome.Failed(job, run, Vector(diagnostic), last(current)),
              Some(RunState.Failed),
              None
            )
        }
      )
    F.uncancelable(_ =>
      supervisor.supervise(guarded).flatMap(fiber => handle.complete(fiber.cancel).void)
    )

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
        // Telemetry this mapping cannot read, or a throw while folding it,
        // is a defect: it is recorded and the run cancelled, so the settle
        // fiber ends the job.
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
          .handleErrorWith(e =>
            state.update(s =>
              s.copy(defects = s.defects.updated(job, RealExecution.Defect(s"telemetry: $e")))
            ) >> running.cancel
          )
        // The job always settles: a throw from eyes4s's outcome, from the
        // digest check or from settling itself settles it as a defect, which
        // frees the backend for the next submission. Releasing the run
        // (also on the supervisor's shutdown) cancels it.
        val settle = F.guarantee(
          running.outcome.attempt
            .flatMap(outcome =>
              F.catchNonFatal(decide(work, job, run, purpose, outcome)).flatMap(state.update)
            )
            .handleErrorWith(e =>
              state.update(s =>
                s.live(job).fold(s) { _ =>
                  val end = JobOutcome.Failed(
                    job,
                    run,
                    Vector(RealExecution.defect(run, RealExecution.Defect(s"settling: $e"))),
                    None
                  )
                  s.settle(
                    job,
                    run,
                    end,
                    Option.when(purpose == Purpose.NewRun)(RunState.Failed),
                    None
                  )
                }
              )
            ),
          release
        )
        handle.complete(running.cancel) >> supervisor.supervise(observe) >>
          supervisor.supervise(settle).void
      }
    }

  /** The state after a job's eyes4s outcome: a new run's result is kept as
    * computed; a recomputed one is checked against the run's recorded digest
    * and kept marked with the eyes4s release, and a recomputation that fails
    * is refused for good, since its inputs cannot change in this backend.
    */
  private def decide(
      work: RealPrepared,
      job: JobId,
      run: RunId,
      purpose: Purpose,
      outcome: Either[Throwable, RealExecution.Outcome]
  ): Jobs[F] => Jobs[F] =
    val mismatch = (purpose, outcome) match
      case (Purpose.Recompute(ref), Right(RunOutcome.Completed(_, _, result))) =>
        verify(ref, work, result)
      case _ => None
    s =>
      s.live(job).fold(s) { carried =>
        val read = s.defects.get(job) match
          case Some(d) => Left(d)
          case None    => outcome.leftMap(e => RealExecution.Defect(s"eyes4s raised $e"))
        val (end, runState, result) = RealExecution.settle(job, run, read, carried, work.counts)
        (purpose, mismatch, end) match
          case (Purpose.NewRun, _, _) =>
            s.settle(
              job,
              run,
              end,
              Some(runState),
              result.map(RealRun(work, _, RunOrigin.Computed))
            )
          case (Purpose.Recompute(_), Some(refused), _) =>
            val failed = JobOutcome.Failed(job, run, Vector(refused.diagnostic), end.progress)
            s.settle(job, run, failed, None, None).refuse(run, refused)
          case (Purpose.Recompute(_), None, JobOutcome.Failed(_, _, diagnostics, _)) =>
            val refused = BackendError.Unavailable(
              DiagnosticLocus.Artifact(
                s"${run.label} recomputation failed: " +
                  diagnostics.map(d => s"${d.code}: ${d.message}").mkString("; ")
              )
            )
            s.settle(job, run, end, None, None).refuse(run, refused)
          case (Purpose.Recompute(_), None, _) =>
            val origin = RunOrigin.Recomputed(StudioBuild.eyes4sBaseVersion)
            s.settle(job, run, end, None, result.map(RealRun(work, _, origin)))
      }

  // ------------------------------------------------------------------ results

  /** Verify the recomputation against the recorded canonical digest. */
  private def verify(
      ref: RunRef,
      work: RealPrepared,
      result: RealExecution.Result
  ): Option[BackendError] =
    ref.archive match
      case CoreBinding.Unbound()     => None
      case CoreBinding.Bound(digest) =>
        work.digest(result) match
          case Right(made) if made == digest.sha256.hex => None
          case Right(made)                              =>
            ByteDigest.parse(made) match
              case Right(value) =>
                Some(BackendError.ResultDigestMismatch(ref.id, digest.sha256, value))
              case Left(error) =>
                Some(
                  BackendError.Unavailable(
                    DiagnosticLocus.Artifact(s"${ref.id.label} result: ${error.message}")
                  )
                )
          case Left(error) =>
            Some(
              BackendError.Unavailable(
                DiagnosticLocus.Artifact(s"${ref.id.label} result: ${error.message}")
              )
            )

  /** Serve a retained completion or restore its verified archive. A result read
    * never admits sources or starts scientific execution; absent storage is an
    * explicit refusal, and the user can submit a new run through the normal job path.
    */
  private[real] def held(run: RunId): F[Either[BackendError, RealRun]] =
    heldCompletion(run).map(_.flatMap {
      case timed: RealRun => Right(timed)
      case _: LocationRun =>
        Left(
          BackendError.Unavailable(
            DiagnosticLocus.Artifact(s"${run.label} is spatial; use the spatial result view.")
          )
        )
    })

  private[real] def heldCompletion(run: RunId): F[Either[BackendError, RetainedRun]] =
    registry.get.flatMap(snapshot =>
      state.get.flatMap { s =>
        (s.results.get(run), s.refused.get(run), s.runs.find(_.run == run)) match
          case (Some(done), _, _)    => F.pure(Right(done))
          case (_, Some(refused), _) => F.pure(Left(refused))
          case (_, _, None) => F.pure(Left(BackendError.UnknownRun(run, s.runs.map(_.run))))
          case (_, _, Some(summary)) =>
            s.jobs.find(j => j.run == run && finished(j.state).isEmpty) match
              case Some(active) => F.pure(Left(BackendError.ResultPending(run, active.job)))
              case None         =>
                snapshot.documentRuns.get(run).filter(_.state == RunLifecycle.Completed) match
                  case None      => F.pure(Left(BackendError.NoResult(run, summary.state)))
                  case Some(ref) =>
                    snapshot.revisions.get(ref.analysis) match
                      case Some((dataset, _)) if dataset != ref.dataset =>
                        F.pure(
                          Left(
                            BackendError.RunDatasetMismatch(
                              run,
                              ref.analysis,
                              ref.dataset,
                              dataset
                            )
                          )
                        )
                      case _ => restoreStored(ref)

      }
    )

  /** Coalesced host archive lookup (bead q-native-archive-readback). Only local read wrappers wait on its
    * signal; no scientific execution job represents storage access.
    */
  private def restoreStored(ref: RunRef): F[Either[BackendError, RetainedRun]] =
    archiveSource match
      case None         => F.pure(Left(BackendError.ResultArchiveUnavailable(ref.id)))
      case Some(source) =>
        Deferred[F, Either[BackendError, Boolean]].flatMap { candidate =>
          F.uncancelable { poll =>
            restoring
              .modify { current =>
                current.get(ref.id) match
                  case Some(signal) => current                            -> (signal, false)
                  case None         => current.updated(ref.id, candidate) -> (candidate, true)
              }
              .flatMap { (signal, owner) =>
                val launch = if owner then
                  supervisor
                    .supervise(restore(ref, source, signal))
                    .void
                    .handleErrorWith(_ =>
                      signal.complete(Left(BackendError.ResultReadClosed(ref.id, None))).void
                    )
                else F.unit
                launch >> poll(signal.tryGet.flatMap {
                  case None               => F.pure(Left(BackendError.ResultRestoring(ref.id)))
                  case Some(Left(error))  => F.pure(Left(error))
                  case Some(Right(true))  => heldCompletion(ref.id)
                  case Some(Right(false)) =>
                    F.pure(Left(BackendError.ResultArchiveUnavailable(ref.id)))
                })
              }
          }
        }

  /** Wait only for an already requested archive lookup. Its scientific
    * result or refusal is retained separately; absence wakes normal reads
    * to an explicit unavailable-archive refusal without scientific execution.
    */
  def awaitRestore(run: RunId): F[Either[BackendError, Unit]] =
    restoring.get.flatMap(_.get(run) match
      case Some(signal) => signal.get.map(_.void)
      case None         => F.pure(Left(BackendError.Unavailable(DiagnosticLocus.Run(run)))))

  private def restore(
      ref: RunRef,
      source: NativeArtifactSource[F],
      signal: Deferred[F, Either[BackendError, Boolean]]
  ): F[Unit] =
    def refused(error: NativeArtifactError): BackendError =
      BackendError.ArchiveRestoreRefused(ref.id, error.diagnostic)
    def publish(
        value: Either[BackendError, Option[RetainedRun]],
        expected: Option[(AnalysisDefinition, DatasetDefinition)] = None
    ): F[Unit] =
      synchronization.lock.surround {
        registry.get.flatMap { current =>
          val checked = value.flatMap { result =>
            Either.cond(
              current.documentRuns.get(ref.id).contains(ref) && expected.forall {
                (revision, dataset) =>
                  current.document.analysisDefinition(revision.id).contains(revision) &&
                  current.document.datasetDefinition(dataset.id).contains(dataset)
              },
              result,
              BackendError.RegistryRefused(
                DiagnosticLocus.Run(ref.id),
                "The saved run declaration changed while its archive was being restored."
              )
            )
          }
          val retainPrepared = checked match
            case Right(Some(done: RealRun)) =>
              prepared.update(_.updated(ref.analysis, done.prepared))
            case _ => F.unit
          val forgetEvictedSignals = checked match
            case Right(Some(done: LocationRun)) if Jobs.isRestored(done) =>
              state.get.flatMap(current =>
                restoring.update(_ -- current.results.collect {
                  case (id, value: LocationRun) if id != ref.id && Jobs.isRestored(value) => id
                })
              )
            case _ => F.unit
          retainPrepared >> forgetEvictedSignals >> state.update { state =>
            checked match
              case Right(Some(done: LocationRun)) if Jobs.isRestored(done) =>
                state.retainRestoredLocation(ref.id, done)
              case Right(Some(done)) =>
                state.copy(results = state.results.updated(ref.id, done))
              case Left(error) => state.refuse(ref.id, error)
              case Right(None) => state
          } >> signal.complete(checked.map(_.nonEmpty)).void
        }
      }
    val read = source.loadCompletion(ref, NativeArtifactBudget.Default).flatMap {
      case Left(error)           => publish(Left(refused(error)))
      case Right(None)           => publish(Right(None))
      case Right(Some(artifact)) =>
        registry.get.flatMap { current =>
          val declarations = for
            revision <- current.document
              .analysisDefinition(ref.analysis)
              .toRight(BackendError.UnknownRevision(ref.analysis, current.knownRevisions))
            dataset <- current.document
              .datasetDefinition(ref.dataset)
              .toRight(BackendError.UnknownDataset(ref.dataset, current.knownDatasets))
          yield (revision, dataset)
          val restored = declarations.flatMap((revision, dataset) =>
            (artifact, revision, dataset) match
              case (
                    NativeCompletion.Timed(value),
                    r: AnalysisRevisionSpec,
                    d: DatasetRevisionSpec
                  ) =>
                NativeArtifactRestore.of(value, ref, r, d).map(Some(_))
              case (
                    NativeCompletion.Locations(value),
                    r: LocationAnalysisRevision,
                    d: LocationDatasetRevision
                  ) =>
                Either.cond(
                  value.facts.run == ref.id && value.facts.analysis == r && value.facts.dataset == d &&
                    ref.analysis == r.id && ref.dataset == d.id && ref.archive == CoreBinding
                      .Bound(value.facts.result),
                  Some(
                    LocationRun(
                      value.admitted,
                      r,
                      value.retained.result,
                      RunOrigin.Restored(value.facts.result.sha256)
                    )
                  ),
                  BackendError.RegistryRefused(
                    DiagnosticLocus.Run(ref.id),
                    "Spatial archive identities differ from the saved run."
                  )
                )
              case _ =>
                Left(
                  BackendError.RegistryRefused(
                    DiagnosticLocus.Run(ref.id),
                    "Archive, analysis and dataset scientific kinds differ."
                  )
                )
          )
          restored match
            case Right(Some(value: LocationRun)) =>
              sharedLocationAdmission(value.admitted).flatMap(shared =>
                publish(Right(Some(value.copy(admitted = shared))), declarations.toOption)
              )
            case _ => publish(restored, declarations.toOption)
        }
    }
    F.guarantee(
      read.handleErrorWith(error =>
        publish(
          Left(
            refused(
              NativeArtifactError.Defect(ref.id, "restore archive", error.getClass.getName)
            )
          )
        )
      ),
      signal.complete(Left(BackendError.ResultReadClosed(ref.id, None))).void
    )

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

  /** Local native-factory artifact capability: reads only retained completions. */
  def nativeArtifacts(
      run: RunId,
      budget: eyes4s.studio.core.artifacts.NativeArtifactBudget
  ): F[Either[
    eyes4s.studio.core.artifacts.NativeArtifactError,
    eyes4s.studio.core.artifacts.NativeArtifactPackage
  ]] =
    import eyes4s.studio.core.artifacts.NativeArtifactError
    state.get.flatMap { current =>
      current.results.get(run) match
        case None =>
          F.pure(
            Left(
              NativeArtifactError.NotRetained(
                run,
                current.results.keys.toVector.sortBy(_.number)
              )
            )
          )
        case Some(_: LocationRun) =>
          F.pure(
            Left(
              NativeArtifactError.InvalidFacts(
                run,
                "scientific kind",
                "A spatial run requires nativeCompletion."
              )
            )
          )
        case Some(done: RealRun) =>
          F.catchNonFatal(NativeArtifacts.build(run, done, budget))
            .handleError(e =>
              Left(NativeArtifactError.Defect(run, "export", e.getClass.getName))
            )
    }

  override def nativeCompletion(run: RunId, budget: NativeArtifactBudget)(using
      cats.Functor[F]
  ): F[Either[NativeArtifactError, NativeCompletion]] =
    state.get.flatMap { current =>
      current.results.get(run) match
        case Some(done: LocationRun) =>
          F.catchNonFatal(
            LocationNativePackage
              .create(run, done.admitted, done.analysis, done.result, budget)
              .map(NativeCompletion.Locations.apply)
          ).handleError(e =>
            Left(
              NativeArtifactError.Defect(run, "export spatial completion", e.getClass.getName)
            )
          )
        case _ => nativeArtifacts(run, budget).map(_.map(NativeCompletion.Timed.apply))
    }

  def locationResult(run: RunId): F[Either[BackendError, LocationRun]] =
    heldCompletion(run).map(_.flatMap {
      case value: LocationRun => Right(value)
      case _: RealRun         =>
        Left(
          BackendError.Unavailable(DiagnosticLocus.Artifact(s"${run.label} is a timed study."))
        )
    })

  /** Ungrouped count facts; numerical report cells are served by `report`. */
  def result(run: RunId): F[Either[BackendError, ResultSummary]] =
    results(run).map(_.flatMap(_.summary))

  def report(
      run: RunId,
      reporting: eyes4s.studio.core.document.ReportingSpec,
      scale: Int
  ): F[Either[BackendError, ReportView]] =
    navigator.requestedReport(run, reporting, scale) >>
      held(run)
        .flatMap(done =>
          reports.evaluate(
            done.flatMap(RealReports.evaluateWithNative(run, reporting, scale, _))
          )
        )
        .flatTap {
          case Left(_)            => F.unit
          case Right((_, native)) => navigator.evaluatedReport(run, reporting, scale, native)
        }
        .map(_.map(_._1))

  def mapGrid(run: RunId, scale: Int, trial: TrialKey): F[Either[BackendError, DensityGrid]] =
    results(run).map(_.flatMap(_.mapGrid(scale, trial)))

  def pairRows(
      run: RunId,
      scale: Int,
      page: PageRequest
  ): F[Either[BackendError, PairRowPage]] =
    results(run).map(_.flatMap(_.pairRows(scale, page)))

  def queries(run: RunId, page: PageRequest): F[Either[BackendError, QueryPage]] =
    results(run).map(_.flatMap(_.queries(page)))

  /** An item eyes4s holds, as eyes4s holds it. A no-match query's contrast
    * row is eyes4s's failed row (missing operands) until protocol 1.11
    * reports it as NoMatch with eyes4s's unmatched reason; a query that was
    * not admitted has no item and is an unknown reference.
    */
  def inspect(run: RunId, address: ResultAddress): F[Either[BackendError, Inspection]] =
    results(run).map(_.flatMap(_.inspect(address)))

  def provenance(run: RunId, address: ResultAddress): F[Either[BackendError, Provenance]] =
    results(run).map(_.flatMap(_.provenance(address)))

  /** The revision's trial views, built once per revision. */
  private def views(r: AnalysisRevision): F[Either[BackendError, RealTrialViews]] =
    prepare(r).flatMap {
      case Left(e)     => F.pure(Left(e))
      case Right(work) =>
        trialViews.get.flatMap(_.get(r).filter(_.work eq work) match
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
    views(revision).map(_.flatMap(_.fixations(trial))).flatMap {
      case Left(error) => F.pure(Left(error))
      case Right(view) =>
        navigator.rememberTrial(revision, view.fixations.map(_.ref)).map {
          case Right(_)                                                           => Right(view)
          case Left(eyes4s.studio.core.navigation.NavigationError.Backend(error)) => Left(error)
          case Left(error)                                                        =>
            Left(BackendError.Unavailable(DiagnosticLocus.Artifact(error.message)))
        }
    }

  def trialPreview(
      revision: AnalysisRevision,
      trial: TrialKey
  ): F[Either[BackendError, TrialPreview]] =
    views(revision).map(_.flatMap(_.preview(trial)))

  def sourceRecords(
      revision: AnalysisRevision,
      from: Int,
      count: Int
  ): F[Either[BackendError, SourceRecordPage]] =
    views(revision).flatMap {
      case Left(error)                                           => F.pure(Left(error))
      case Right(view) if view.work.admitted.sourceText.nonEmpty =>
        F.pure(view.sourceRecords(from, count))
      case Right(view) =>
        val dataset = view.work.admitted.spec
        dataset.sources.fixations match
          case None =>
            F.pure(Left(BackendError.Unavailable(DiagnosticLocus.Field("fixations source"))))
          case Some(source) =>
            sources.bytes(dataset, source).map {
              case None =>
                Left(
                  BackendError.Unavailable(
                    DiagnosticLocus.Artifact(
                      s"${source.path.value}: the host holds no exact source bytes"
                    )
                  )
                )
              case Some(bytes) =>
                val read = ByteDigest.sha256(bytes)
                if read != source.bytes then
                  Left(
                    BackendError
                      .SourceDigestMismatch(dataset.id, source.path, source.bytes, read)
                  )
                else
                  view.sourceRecords(
                    new String(IArray.genericWrapArray(bytes).toArray, StandardCharsets.UTF_8),
                    from,
                    count
                  )
            }
    }

object RealStudyBackend:

  /** The real backend over `document`'s dataset revisions and analysis
    * revisions (its saved revisions and its draft), with `sources` for their
    * files. Its jobs run on fibers the resource owns: releasing it cancels
    * every running job.
    */
  def resource[F[_]: Concurrent](
      document: StudioDocument,
      sources: DatasetSources[F],
      archiveSource: Option[NativeArtifactSource[F]] = None
  ): Resource[F, RealStudyBackend[F]] =
    Supervisor[F](await = false).evalMap(supervisor =>
      (
        Ref.of[F, Map[DatasetRevision, AdmittedDataset]](Map.empty),
        Ref.of[F, Option[AdmittedLocations]](None),
        Mutex[F],
        Ref.of[F, Map[AnalysisRevision, RealPrepared]](Map.empty),
        SignallingRef[F].of(Jobs.of[F](document)),
        Ref.of[F, Map[RunId, RealResults]](Map.empty),
        Ref.of[F, Map[AnalysisRevision, RealTrialViews]](Map.empty),
        RealPreview.create[F],
        RealNavigator.empty[F],
        Ref.of[F, RealRegistry](RealRegistry.of(document)),
        Ref.of[F, Map[RunId, Deferred[F, Either[BackendError, Boolean]]]](Map.empty),
        Mutex[F],
        RealReportGate.create[F]
      ).mapN {
        (
            admitted,
            locationAdmission,
            locationAdmissionMutex,
            prepared,
            state,
            inspected,
            trialViews,
            previews,
            navigation,
            registry,
            restoring,
            synchronization,
            reports
        ) =>
          new RealStudyBackend(
            registry,
            sources,
            admitted,
            locationAdmission,
            locationAdmissionMutex,
            prepared,
            previews,
            state,
            inspected,
            trialViews,
            document.reporting.map(spec => spec.id -> spec).toMap,
            navigation,
            archiveSource,
            restoring,
            supervisor,
            synchronization,
            reports
          )
      }
    )

  /** A backend whose fibers are never released: for tests, which end with
    * their JVM. A host uses [[resource]].
    */
  private[real] def create[F[_]: Concurrent](
      document: StudioDocument,
      sources: DatasetSources[F]
  ): F[RealStudyBackend[F]] =
    resource(document, sources).allocated.map(_._1)

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
      results: Map[RunId, RetainedRun],
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
        result: Option[RetainedRun]
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

    /** Cold spatial archives are re-readable, so retain only the most recent one.
      * Computed values stay resident until their native archive is published.
      */
    def retainRestoredLocation(run: RunId, value: LocationRun): Jobs[F] =
      copy(results =
        results
          .collect {
            case (id, retained: LocationRun) if !Jobs.isRestored(retained) => id -> retained
            case (id, retained: RealRun)                                   => id -> retained
          }
          .updated(run, value)
      )

    /** An archive binding was checked against this exact spatial result. It is
      * durable now, so a later read restores its saved bytes rather than holding
      * the completed source closure in this process.
      */
    def releaseArchivedLocations(runs: Set[RunId]): Jobs[F] =
      copy(results = results.filter {
        case (run, _: LocationRun) => !runs(run)
        case _                     => true
      })

  object Jobs:
    private[real] def isRestored(value: LocationRun): Boolean = value.origin match
      case RunOrigin.Restored(_) => true
      case _                     => false

    /** The document's runs as this backend reports them. The shown run is
      * current; another completed run is stale once a later dataset revision
      * is admitted, and completed otherwise. A run the document records as
      * running had a job of an earlier backend, which this one does not have
      * (a job id means nothing after a restart), so it is not listed; its
      * number is never reused.
      */
    def of[F[_]](document: StudioDocument): Jobs[F] =
      def latest(dataset: DatasetRevision) = document
        .datasetDefinition(dataset)
        .flatMap(d =>
          document.datasets
            .findLast(candidate => candidate.kind == d.kind && candidate.isAdmitted)
            .map(_.id)
        )
      val runs = document.runs.flatMap { r =>
        val state = r.state match
          case RunLifecycle.Completed if document.presentation.shownRun.contains(r.id) =>
            Some(RunState.Current)
          case RunLifecycle.Completed if !latest(r.dataset).contains(r.dataset) =>
            Some(RunState.Stale)
          case RunLifecycle.Completed     => Some(RunState.Completed)
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
    case Restored(manifest: ByteDigest)

  sealed trait RetainedRun

  final case class LocationRun(
      admitted: AdmittedLocations,
      analysis: LocationAnalysisRevision,
      result: eyes4s.plan.LocationComparisonResult,
      origin: RunOrigin
  ) extends RetainedRun

  /** A run's eyes4s result with the prepared study that produced it. */
  final case class RealRun(
      prepared: RealPrepared,
      result: RealExecution.Result,
      origin: RunOrigin
  ) extends RetainedRun:
    private[real] lazy val reportSource = ReportSources
      .retainedStudy(prepared.plans, prepared.inputs, prepared.results)(
        prepared.plan,
        prepared.admitted.input,
        result,
        Some(prepared.admitted.evidence)
      )

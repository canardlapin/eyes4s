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

import cats.effect.kernel.{Concurrent, Ref}
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{DatasetRevisionSpec, Recipe, Source, StudioDocument}
import eyes4s.studio.core.preview.*
import fs2.Stream
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
  */
final class RealStudyBackend[F[_]] private (
    datasets: Map[DatasetRevision, DatasetRevisionSpec],
    revisions: Map[AnalysisRevision, (DatasetRevision, Recipe)],
    sources: DatasetSources[F],
    admitted: Ref[F, Map[DatasetRevision, Either[BackendError, AdmittedDataset]]],
    prepared: Ref[F, Map[AnalysisRevision, Either[BackendError, RealPrepared]]]
)(using F: Concurrent[F])
    extends StudyBackend[F]:

  // ------------------------------------------------------------------ admission

  private def knownDatasets: Vector[DatasetRevision] = datasets.keys.toVector.sortBy(_.number)

  private def admission0(d: DatasetRevision): F[Either[BackendError, AdmittedDataset]] =
    datasets.get(d) match
      case None       => F.pure(Left(BackendError.UnknownDataset(d, knownDatasets)))
      case Some(spec) =>
        admitted.get.flatMap(_.get(d) match
          case Some(done) => F.pure(done)
          case None       =>
            admit(spec).flatTap(result => admitted.update(_.updated(d, result))))

  private def admit(spec: DatasetRevisionSpec): F[Either[BackendError, AdmittedDataset]] =
    val unavailable = BackendError.Unavailable(DiagnosticLocus.Dataset(spec.id))
    def text(source: Option[Source]): F[Either[BackendError, String]] = source match
      case None    => F.pure(Left(unavailable))
      case Some(s) =>
        sources.bytes(spec, s).map {
          // The host must hand over exactly the bytes the revision recorded.
          case Some(b) if ByteDigest.sha256(b) == s.bytes =>
            Right(new String(IArray.genericWrapArray(b).toArray, StandardCharsets.UTF_8))
          case _ => Left(unavailable)
        }
    (text(spec.sources.fixations), text(spec.sources.trials), sources.assets(spec)).mapN {
      (fixations, trials, assets) =>
        for
          f      <- fixations
          t      <- trials
          a      <- assets.toRight(unavailable)
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

  /** No run exists yet: execution is a later slice. */
  private def noRun[A](run: RunId): F[Either[BackendError, A]] =
    F.pure(Left(BackendError.UnknownRun(run, Vector.empty)))

  /** No job exists yet: execution is a later slice. */
  private def noJob[A](job: JobId): F[Either[BackendError, A]] =
    F.pure(Left(BackendError.UnknownJob(job, Vector.empty)))

  // ------------------------------------------------------------------ the prepared study

  /** The revision's study, configured from its recipe over its dataset's
    * admitted input and prepared once; a revision is immutable.
    */
  private def prepare(r: AnalysisRevision): F[Either[BackendError, RealPrepared]] =
    revisions.get(r) match
      case None              => F.pure(Left(BackendError.UnknownRevision(r, knownRevisions)))
      case Some((d, recipe)) =>
        prepared.get.flatMap(_.get(r) match
          case Some(done) => F.pure(done)
          case None       =>
            admission0(d)
              .map(_.flatMap(RealPrepared.of(r, d, recipe, _)))
              .flatTap(result => prepared.update(_.updated(r, result))))

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

  def runs: F[Vector[RunSummary]] = F.pure(Vector.empty)

  def submit(revision: AnalysisRevision): F[Either[BackendError, JobStatus]] =
    notYet(revision)

  def jobs: F[Vector[JobStatus]] = F.pure(Vector.empty)

  def job(id: JobId): F[Either[BackendError, JobStatus]] = noJob(id)

  def subscribe(id: JobId): F[Either[BackendError, Stream[F, JobEvent]]] = noJob(id)

  def cancel(id: JobId): F[Either[BackendError, JobStatus]] = noJob(id)

  def outcome(id: JobId): F[Either[BackendError, Option[JobOutcome]]] = noJob(id)

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
      Ref.of[F, Map[DatasetRevision, Either[BackendError, AdmittedDataset]]](Map.empty),
      Ref.of[F, Map[AnalysisRevision, Either[BackendError, RealPrepared]]](Map.empty)
    ).mapN { (admitted, prepared) =>
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
        prepared
      )
    }

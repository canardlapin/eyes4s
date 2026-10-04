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

package eyes4s.studio.core.runs

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eyes4s.codec.{ArtifactName, ByteDigest, CodecError}
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.bundle.{
  BundleError,
  BundlePath,
  ProjectBundle,
  ProjectStore,
  StoreError,
  WriterLock
}
import eyes4s.studio.core.document.{
  CoreBinding,
  FigureId,
  ResultArchiveArtifact,
  RunLifecycle,
  RunRef,
  StudioDocument
}

/** Why the run store refused or failed. Every case names what it was
  * applied to; `message` is a default English rendering, never an identity.
  */
enum RunStoreError derives CanEqual:
  case NegativeLength(name: ArtifactName, length: Long)
  case EmptyArchive(run: RunId)
  case DuplicateEntries(run: RunId, names: Vector[ArtifactName])

  /** Only a completed run has an archive. */
  case NotCompleted(run: RunId, state: RunLifecycle)

  /** The run has no stored archive: it was never stored or was pruned. */
  case NoArchive(run: RunId, index: BundlePath)

  /** The index at `path` is another run's. */
  case IndexOfOtherRun(path: BundlePath, expected: RunId, found: RunId)

  /** The stored archive is not the one the document's run binds. */
  case ArchiveMismatch(
      run: RunId,
      document: CoreBinding[ResultArchiveArtifact],
      stored: CoreBinding[ResultArchiveArtifact]
  )
  case UnknownEntry(run: RunId, name: ArtifactName, names: Vector[ArtifactName])
  case MissingEntry(run: RunId, name: ArtifactName, path: BundlePath)
  case EntryLength(run: RunId, name: ArtifactName, expected: Long, found: Long)
  case EntryDigest(run: RunId, name: ArtifactName, expected: ByteDigest, found: ByteDigest)
  case UnknownFigure(figure: FigureId, known: Vector[FigureId])
  case UnknownRun(run: RunId, referrer: String, known: Vector[RunId])
  case NothingToPrune
  case NotInStorage(run: RunId, stored: Vector[RunId])

  /** Pruning the run's archive is refused: it is kept for `reasons`. */
  case KeptRun(run: RunId, reasons: Vector[KeepReason])

  /** The run's stored archive is not the one the confirmed plan showed. */
  case PlanOutdated(run: RunId)
  case Bundle(error: BundleError)
  case Index(target: String, error: CodecError)

  def message: String = this match
    case NegativeLength(name, length) =>
      s"Archive entry $name declares a negative length, $length."
    case EmptyArchive(run)            => s"The archive of ${run.label} has no entries."
    case DuplicateEntries(run, names) =>
      s"The archive of ${run.label} names ${names.mkString(", ")} more than once."
    case NotCompleted(run, state) =>
      s"${run.label} has no archive to store: it is ${RunStoreError.describe(state)}."
    case NoArchive(run, index) => s"${run.label} has no stored archive (no $index)."
    case IndexOfOtherRun(path, expected, found) =>
      s"$path is the archive index of ${found.label}, not of ${expected.label}."
    case ArchiveMismatch(run, document, stored) =>
      s"The stored archive of ${run.label} is ${stored.render}; the document binds ${document.render}."
    case UnknownEntry(run, name, names) =>
      s"The archive of ${run.label} has no entry $name (it has ${names.mkString(", ")})."
    case MissingEntry(run, name, path) =>
      s"Entry $name of ${run.label}'s archive is missing from the bundle ($path)."
    case EntryLength(run, name, expected, found) =>
      s"Entry $name of ${run.label}'s archive has $found bytes; its index lists $expected."
    case EntryDigest(run, name, expected, found) =>
      s"Entry $name of ${run.label}'s archive has SHA-256 ${found.hex}; its index lists ${expected.hex}."
    case UnknownFigure(figure, known) =>
      s"The document has no ${figure.label} (it has ${RunStoreError.listed(known.map(_.label))})."
    case UnknownRun(run, referrer, known) =>
      s"$referrer names ${run.label}, which the document does not have " +
        s"(it has ${RunStoreError.listed(known.map(_.label))})."
    case NothingToPrune            => "No run archive was chosen for pruning."
    case NotInStorage(run, stored) =>
      s"${run.label} has no stored archive to prune (stored: ${RunStoreError.listed(stored.map(_.label))})."
    case KeptRun(run, reasons) =>
      s"The archive of ${run.label} is kept: ${reasons.map(RunStoreError.describe).mkString("; ")}."
    case PlanOutdated(run) =>
      s"The archive of ${run.label} changed after pruning was confirmed; nothing was pruned."
    case Bundle(error)        => error.message
    case Index(target, error) => s"$target: ${error.message}"

object RunStoreError:
  private def listed(items: Vector[String]): String =
    if items.isEmpty then "none" else items.mkString(", ")

  private def describe(state: RunLifecycle): String = state match
    case RunLifecycle.Running      => "running"
    case RunLifecycle.Completed    => "completed"
    case RunLifecycle.Cancelled(_) => "cancelled"
    case RunLifecycle.Failed       => "failed"

  private def describe(reason: KeepReason): String = reason match
    case KeepReason.Figure(figure, source) =>
      val where = source match
        case BindingSource.Current  => "the open document"
        case BindingSource.Undo     => "a document undo or redo returns to"
        case BindingSource.Saved    => "the saved document"
        case BindingSource.Previous => "the previous saved document"
      s"${figure.label} binds it in $where"
    case KeepReason.Shown   => "the document shows it"
    case KeepReason.Running => "it is running"
    case KeepReason.Ready   => "it is ready to show"

/** The run store (ticket S2.6): completed runs' result archives, kept in an
  * `.eyes` bundle under `runs/<id>/archive/` ([[ArchivePaths]]).
  *
  *  - '''Lazy.''' The manifest lists no archive, so opening a project reads
  *    none. [[index]] reads one run's index only; [[entry]] reads one entry;
  *    [[load]] reads a whole archive. Every entry read is checked against
  *    the length and SHA-256 its index lists, and every index against the
  *    archive binding of the document's run.
  *  - '''Figures render from their own run.''' [[figure]] loads the archive
  *    of the run a figure binds, never the latest one.
  *  - '''Retention.''' [[storage]] lists every stored archive with its size,
  *    kept or prunable under a [[RetentionBasis]]; the runs any reachable
  *    document's figures bind are kept. [[prune]] takes only a
  *    [[ConfirmedPrune]], and re-checks it before deleting anything: a run
  *    that has become kept, or an archive that changed, refuses the whole
  *    prune.
  *  - '''Order.''' Writing stores the entries before the index, and pruning
  *    deletes the entries before the index, so an index names only stored
  *    bytes until a prune; an interrupted write or prune leaves an
  *    [[ArchiveRecord.Incomplete]] or a still-prunable archive.
  */
final class RunStore[F[_]: Monad](store: ProjectStore[F]):
  import RunStoreError.*

  private def lift[A](e: Either[BundleError, A]): EitherT[F, RunStoreError, A] =
    EitherT.fromEither[F](e.left.map(Bundle(_)))

  /** Store `archive`: its entries, then its index. Storing the same archive
    * again writes nothing; another archive for the same run is refused as a
    * [[BundleError.Collision]].
    */
  def put(lock: WriterLock, archive: RunArchive): F[Either[RunStoreError, ArchiveRecord]] =
    val run = archive.run
    (for
      indexPath <- lift(ArchivePaths.index(run))
      text      <- EitherT.fromEither[F](ArchiveText.render(archive.index, indexPath.value))
      contents  <- archive.files.distinctBy(_._1.sha256.hex).traverse { (entry, bytes) =>
        lift(ArchivePaths.content(run, entry.sha256)).flatMap(path =>
          EitherT(ProjectBundle.writeImmutable(store, lock, path, bytes))
            .leftMap(Bundle(_))
            .as(path)
        )
      }
      _ <- EitherT(ProjectBundle.writeImmutable(store, lock, indexPath, text)).leftMap(
        Bundle(_)
      )
    yield ArchiveRecord.Stored(
      archive.index,
      text.length.toLong,
      ByteDigest.sha256(text),
      (contents :+ indexPath).sorted
    )).value

  /** The index of `run`'s stored archive. Reads nothing else. */
  def index(run: RunRef): F[Either[RunStoreError, ArchiveIndex]] =
    (for
      path  <- lift(ArchivePaths.index(run.id))
      bytes <- EitherT(store.read(path)).leftMap {
        case StoreError.Missing(_) => NoArchive(run.id, path)
        case other                 => Bundle(BundleError.Store(other))
      }
      index <- EitherT.fromEither[F](ArchiveText.read(path, bytes))
      _ <- EitherT.cond[F](index.run == run.id, (), IndexOfOtherRun(path, run.id, index.run))
      _ <- EitherT.cond[F](
        index.archive == run.archive,
        (),
        ArchiveMismatch(run.id, run.archive, index.archive)
      )
    yield index).value

  /** The bytes of entry `name` in an archive whose index was read. */
  def entry(index: ArchiveIndex, name: ArtifactName): F[Either[RunStoreError, IArray[Byte]]] =
    index.entry(name) match
      case None        => Monad[F].pure(Left(UnknownEntry(index.run, name, index.names)))
      case Some(entry) => read(index.run, entry).value

  /** The bytes of entry `name` of `run`'s archive: its index and that entry
    * only.
    */
  def entry(run: RunRef, name: ArtifactName): F[Either[RunStoreError, IArray[Byte]]] =
    EitherT(index(run)).flatMapF(entry(_, name)).value

  /** `run`'s whole archive. */
  def load(run: RunRef): F[Either[RunStoreError, RunArchive]] =
    (for
      index    <- EitherT(this.index(run))
      contents <- index.entries.traverse(e => read(index.run, e).map(e.name.value -> _))
    yield RunArchive.stored(index, contents.toMap)).value

  /** The archive a figure renders from: that of the run it binds, never the
    * latest.
    */
  def figure(document: StudioDocument, figure: FigureId): F[Either[RunStoreError, RunArchive]] =
    (for
      spec <- EitherT.fromOption[F](
        document.figures.find(_.id == figure),
        UnknownFigure(figure, document.figures.map(_.id))
      )
      run <- EitherT.fromOption[F](
        document.run(spec.run),
        UnknownRun(spec.run, figure.label, document.runs.map(_.id))
      )
      archive <- EitherT(load(run))
    yield archive).value

  /** Every stored archive area, reading only indexes. */
  def records: F[Either[RunStoreError, Vector[ArchiveRecord]]] =
    (for
      listed <- EitherT(store.list).leftMap(e => Bundle(BundleError.Store(e)))
      groups = listed
        .flatMap(p => ArchivePaths.owner(p).map(_ -> p))
        .groupMap(_._1)(_._2)
        .toVector
        .sortBy(_._1.number)
      records <- groups.traverse { (run, paths) =>
        paths.find(ArchivePaths.isIndex) match
          case None =>
            EitherT.rightT[F, RunStoreError](ArchiveRecord.Incomplete(run, paths.sorted))
          case Some(path) =>
            for
              bytes <- EitherT(store.read(path)).leftMap(e => Bundle(BundleError.Store(e)))
              index <- EitherT.fromEither[F](ArchiveText.read(path, bytes))
              _ <- EitherT.cond[F](index.run == run, (), IndexOfOtherRun(path, run, index.run))
            yield ArchiveRecord.Stored(
              index,
              bytes.length.toLong,
              ByteDigest.sha256(bytes),
              paths.sorted
            ): ArchiveRecord
      }
    yield records).value

  /** Every stored archive, kept or prunable under `basis`, with sizes. */
  def storage(basis: RetentionBasis): F[Either[RunStoreError, RunStorage]] =
    EitherT(records).map(RunStorage.classify(_, basis)).value

  /** Prune the confirmed archives. Before deleting anything, every planned
    * archive must still be stored as the plan showed it and still be
    * prunable under `basis`; otherwise nothing is deleted.
    */
  def prune(
      lock: WriterLock,
      confirmed: ConfirmedPrune,
      basis: RetentionBasis
  ): F[Either[RunStoreError, PruneReport]] =
    val plan = confirmed.plan
    (for
      now <- EitherT(storage(basis))
      _   <- EitherT.fromEither[F](plan.records.traverse_ { planned =>
        now.row(planned.run) match
          case Some(RunRetention.Kept(_, reasons)) => Left(KeptRun(planned.run, reasons))
          case Some(RunRetention.Prunable(record)) if record == planned => Right(())
          case _ => Left(PlanOutdated(planned.run))
      })
      deleted <- plan.records.flatTraverse { record =>
        val (indexes, contents) = record.files.partition(ArchivePaths.isIndex)
        (contents ++ indexes).traverse(path =>
          EitherT(store.delete(lock, path)).leftMap(e => Bundle(BundleError.Store(e))).as(path)
        )
      }
    yield PruneReport(plan.runs, plan.bytes, deleted)).value

  private def read(run: RunId, entry: ArchiveEntry): EitherT[F, RunStoreError, IArray[Byte]] =
    for
      path  <- lift(ArchivePaths.content(run, entry.sha256))
      bytes <- EitherT(store.read(path)).leftMap {
        case StoreError.Missing(_) => MissingEntry(run, entry.name, path)
        case other                 => Bundle(BundleError.Store(other))
      }
      _ <- EitherT.cond[F](
        bytes.length.toLong == entry.length,
        (),
        EntryLength(run, entry.name, entry.length, bytes.length.toLong)
      )
      found = ByteDigest.sha256(bytes)
      _ <- EitherT
        .cond[F](found == entry.sha256, (), EntryDigest(run, entry.name, entry.sha256, found))
    yield bytes

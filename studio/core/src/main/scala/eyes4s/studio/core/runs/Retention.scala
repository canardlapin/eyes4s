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

import eyes4s.codec.ByteDigest
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.bundle.BundlePath
import eyes4s.studio.core.command.{History, HistoryStack}
import eyes4s.studio.core.document.{FigureId, StudioDocument}

import scala.annotation.tailrec

/** Which document binds a figure to a run. */
enum BindingSource derives CanEqual:
  /** The open document. */
  case Current

  /** A document that undo or redo can return to. */
  case Undo

  /** The last saved document (`project.json`). */
  case Saved

  /** The retained previous manifest's document, which the session opens
    * when `project.json` is damaged.
    */
  case Previous

/** Why a stored archive is kept. */
enum KeepReason derives CanEqual:
  /** A figure binds the run: figures never follow the latest run, so their
    * run's archive must stay to render them.
    */
  case Figure(figure: FigureId, source: BindingSource)

  /** A document shows the run: the open one, one undo or redo (of a science
    * or a view change) returns to, or a saved one.
    */
  case Shown(source: BindingSource)

  /** The run is still running: its archive may be partly written. */
  case Running

  /** The run is offered by the ready notice ("Run 8 ready — Show"). */
  case Ready

/** Every run a stored archive must be kept for, with each reason (S2.6):
  * the runs that the open document, every document the user can still
  * reach, the saved document and the previous manifest's document bind by
  * a figure or show, the open document's running runs and the ready
  * notice's run.
  *
  * [[RetentionBasis.session]] is the only way to build one, and it takes
  * every source explicitly, so a caller cannot prune under a partial basis
  * by leaving one out.
  */
final case class RetentionBasis private (keeps: Vector[(RunId, KeepReason)]) derives CanEqual:
  def reasons(run: RunId): Vector[KeepReason] = keeps.collect { case (`run`, r) => r }.distinct
  def kept(run: RunId): Boolean               = keeps.exists(_._1 == run)
  def runs: Vector[RunId]                     = keeps.map(_._1).distinct.sortBy(_.number)

object RetentionBasis:
  /** The basis of a writing session:
    *
    *  - `history`: the open document, its running runs, and every document
    *    its undo and redo reach on both the science and the presentation
    *    stack;
    *  - `saved`: the document `project.json` holds, `None` for a project
    *    never saved;
    *  - `previous`: the retained previous manifest's document, `None` when
    *    there is none or it does not open;
    *  - `ready`: the run the ready notice offers, if any.
    */
  def session(
      history: History,
      saved: Option[StudioDocument],
      previous: Option[StudioDocument],
      ready: Option[RunId]
  ): RetentionBasis =
    val current = history.document
    RetentionBasis(
      (binds(current, BindingSource.Current) ++
        current.running.map(_.id -> KeepReason.Running) ++
        reachable(history).flatMap(binds(_, BindingSource.Undo)) ++
        saved.toVector.flatMap(binds(_, BindingSource.Saved)) ++
        previous.toVector.flatMap(binds(_, BindingSource.Previous)) ++
        ready.map(_ -> KeepReason.Ready)).distinct
    )

  /** The runs `document`'s figures bind and the run it shows. */
  private def binds(
      document: StudioDocument,
      source: BindingSource
  ): Vector[(RunId, KeepReason)] =
    document.figures.map(f => f.run -> KeepReason.Figure(f.id, source)) ++
      document.presentation.shownRun.map(_ -> KeepReason.Shown(source))

  /** The documents `history`'s undo and redo reach on either stack,
    * excluding its own. An undo or redo the reducer refuses ends that
    * direction.
    */
  private def reachable(history: History): Vector[StudioDocument] =
    @tailrec
    def walk(
        from: History,
        step: History => Option[History],
        acc: Vector[StudioDocument]
    ): Vector[StudioDocument] =
      step(from) match
        case Some(next) => walk(next, step, acc :+ next.document)
        case None       => acc
    HistoryStack.values.toVector.flatMap { which =>
      def back(h: History) =
        Option.when(h.stack(which).canUndo)(h.undoOn(which).toOption.map(_.history)).flatten
      def forth(h: History) =
        Option.when(h.stack(which).canRedo)(h.redoOn(which).toOption.map(_.history)).flatten
      walk(history, back, Vector.empty) ++ walk(history, forth, Vector.empty)
    }

/** What the store holds for one run's archive. */
enum ArchiveRecord derives CanEqual:
  /** An archive with its index: `paths` are every file in the run's
    * archive area, the index included.
    */
  case Stored(
      index: ArchiveIndex,
      indexLength: Long,
      indexDigest: ByteDigest,
      paths: Vector[BundlePath]
  )

  /** Files in a run's archive area without an index: a write or a prune
    * that did not finish. Their size is not read.
    */
  case Incomplete(owner: RunId, paths: Vector[BundlePath])

  /** A run's archive area whose index could not be read: `error` says why.
    * It is listed, and classified like any other, rather than hiding every
    * other run's archive. Its size is not read.
    */
  case Damaged(owner: RunId, index: BundlePath, error: RunStoreError, paths: Vector[BundlePath])

  def run: RunId = this match
    case Stored(index, _, _, _) => index.run
    case Incomplete(run, _)     => run
    case Damaged(run, _, _, _)  => run

  def files: Vector[BundlePath] = this match
    case Stored(_, _, _, paths)  => paths
    case Incomplete(_, paths)    => paths
    case Damaged(_, _, _, paths) => paths

  /** The bytes the archive occupies: its index and its distinct entries;
    * unknown for an incomplete or damaged one.
    */
  def size: Option[Long] = this match
    case Stored(index, length, _, _) => Some(index.contentBytes + length)
    case Incomplete(_, _)            => None
    case Damaged(_, _, _, _)         => None

/** A stored archive and whether it may be pruned. */
enum RunRetention derives CanEqual:
  case Kept(archive: ArchiveRecord, reasons: Vector[KeepReason])
  case Prunable(archive: ArchiveRecord)

  def record: ArchiveRecord = this match
    case Kept(archive, _)  => archive
    case Prunable(archive) => archive

  def run: RunId = record.run

/** Every stored archive, each kept or prunable, in run order: what the
  * store shows, with sizes.
  */
final case class RunStorage private (rows: Vector[RunRetention]) derives CanEqual:
  def row(run: RunId): Option[RunRetention] = rows.find(_.run == run)

  def kept: Vector[RunRetention.Kept]         = rows.collect { case k: RunRetention.Kept => k }
  def prunable: Vector[RunRetention.Prunable] = rows.collect { case p: RunRetention.Prunable =>
    p
  }

  /** The known size of all stored archives, of the kept ones and of the
    * prunable ones.
    */
  def bytes: Long         = rows.flatMap(_.record.size).sum
  def keptBytes: Long     = kept.flatMap(_.record.size).sum
  def prunableBytes: Long = prunable.flatMap(_.record.size).sum

object RunStorage:
  /** Classify `records` under `basis`: a run the basis keeps is kept, with
    * its reasons; every other is prunable.
    */
  def classify(records: Vector[ArchiveRecord], basis: RetentionBasis): RunStorage =
    RunStorage(records.sortBy(_.run.number).map { record =>
      val reasons = basis.reasons(record.run)
      if reasons.isEmpty then RunRetention.Prunable(record)
      else RunRetention.Kept(record, reasons)
    })

/** Archives chosen for pruning, each prunable when chosen. Pruning needs
  * the user's confirmation ([[PrunePlan.confirm]]), and is re-checked
  * against the store and the retention basis when it runs.
  */
final case class PrunePlan private (records: Vector[ArchiveRecord]) derives CanEqual:
  def runs: Vector[RunId] = records.map(_.run)

  /** The known bytes pruning frees, and whether some archive's size is
    * unknown (an incomplete one).
    */
  def bytes: Long          = records.flatMap(_.size).sum
  def sizeUnknown: Boolean = records.exists(_.size.isEmpty)

  /** The user confirmed this plan. */
  def confirm: ConfirmedPrune = ConfirmedPrune(this)

object PrunePlan:
  /** A plan to prune the archives of `runs`, each stored and prunable in
    * `storage`.
    */
  def of(storage: RunStorage, runs: Vector[RunId]): Either[RunStoreError, PrunePlan] =
    if runs.isEmpty then Left(RunStoreError.NothingToPrune)
    else
      runs.distinct
        .foldLeft[Either[RunStoreError, Vector[ArchiveRecord]]](Right(Vector.empty)) {
          (acc, run) =>
            acc.flatMap(records =>
              storage.row(run) match
                case None => Left(RunStoreError.NotInStorage(run, storage.rows.map(_.run)))
                case Some(RunRetention.Kept(_, reasons)) =>
                  Left(RunStoreError.KeptRun(run, reasons))
                case Some(RunRetention.Prunable(record)) => Right(records :+ record)
            )
        }
        .map(records => PrunePlan(records.sortBy(_.run.number)))

  /** A plan to prune every prunable archive, if there is one. */
  def everything(storage: RunStorage): Either[RunStoreError, PrunePlan] =
    of(storage, storage.prunable.map(_.run))

/** A [[PrunePlan]] the user confirmed: the only thing
  * [[RunStore.prune]] accepts.
  */
final case class ConfirmedPrune private[runs] (plan: PrunePlan) derives CanEqual

/** What a prune removed. */
final case class PruneReport(
    runs: Vector[RunId],
    freedBytes: Long,
    deleted: Vector[BundlePath]
) derives CanEqual

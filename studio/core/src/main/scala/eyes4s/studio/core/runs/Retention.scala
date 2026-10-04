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

  /** The run the open document shows. */
  case Shown

  /** The run is still running: its archive may be partly written. */
  case Running

  /** The run is offered by the ready notice ("Run 8 ready — Show"). */
  case Ready

/** Every run a stored archive must be kept for, with each reason (S2.6).
  * Built from the open document, then widened with every other document
  * the user can still reach: the documents undo and redo return to, the
  * saved document and the previous manifest's. A run a figure binds in any
  * of them is kept.
  */
final case class RetentionBasis private (keeps: Vector[(RunId, KeepReason)]) derives CanEqual:
  def reasons(run: RunId): Vector[KeepReason] = keeps.collect { case (`run`, r) => r }.distinct
  def kept(run: RunId): Boolean               = keeps.exists(_._1 == run)
  def runs: Vector[RunId]                     = keeps.map(_._1).distinct.sortBy(_.number)

  private def adding(more: Vector[(RunId, KeepReason)]): RetentionBasis =
    RetentionBasis((keeps ++ more).distinct)

  /** Keep the runs `document`'s figures bind, as bindings from `source`. */
  def withFigures(document: StudioDocument, source: BindingSource): RetentionBasis =
    adding(document.figures.map(f => f.run -> KeepReason.Figure(f.id, source)))

  /** Keep every run a figure binds in a document `history`'s undo or redo
    * can return to, on its science stack.
    */
  def withUndo(history: History): RetentionBasis =
    RetentionBasis
      .reachable(history)
      .foldLeft(this)((basis, document) => basis.withFigures(document, BindingSource.Undo))

  def withSaved(document: StudioDocument): RetentionBasis =
    withFigures(document, BindingSource.Saved)

  def withPrevious(document: StudioDocument): RetentionBasis =
    withFigures(document, BindingSource.Previous)

  /** Keep the run of the ready notice. */
  def withReady(run: RunId): RetentionBasis = adding(Vector(run -> KeepReason.Ready))

object RetentionBasis:
  /** What `document` keeps: the runs its figures bind, the run it shows and
    * its running runs.
    */
  def of(document: StudioDocument): RetentionBasis =
    RetentionBasis(Vector.empty)
      .withFigures(document, BindingSource.Current)
      .adding(
        document.presentation.shownRun.map(_ -> KeepReason.Shown).toVector ++
          document.running.map(_.id -> KeepReason.Running)
      )

  /** What a history keeps: its document's runs and every figure binding its
    * undo and redo can return to.
    */
  def of(history: History): RetentionBasis = of(history.document).withUndo(history)

  /** The documents `history`'s science undo and redo reach, excluding its
    * own. An undo or redo the reducer refuses ends that direction.
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
    def back(h: History) =
      Option.when(h.stack(HistoryStack.Science).canUndo)(h.undo.toOption.map(_.history)).flatten
    def forth(h: History) =
      Option.when(h.stack(HistoryStack.Science).canRedo)(h.redo.toOption.map(_.history)).flatten
    walk(history, back, Vector.empty) ++ walk(history, forth, Vector.empty)

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

  def run: RunId = this match
    case Stored(index, _, _, _) => index.run
    case Incomplete(run, _)     => run

  def files: Vector[BundlePath] = this match
    case Stored(_, _, _, paths) => paths
    case Incomplete(_, paths)   => paths

  /** The bytes the archive occupies: its index and its distinct entries;
    * unknown for an incomplete one.
    */
  def size: Option[Long] = this match
    case Stored(index, length, _, _) => Some(index.contentBytes + length)
    case Incomplete(_, _)            => None

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

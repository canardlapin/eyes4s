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

package eyes4s.studio.app.runs

import eyes4s.studio.app.text.Format
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.runs.{
  ArchiveRecord,
  BindingSource,
  ConfirmedPrune,
  KeepReason,
  PrunePlan,
  PruneReport,
  RunRetention,
  RunStorage
}

import scala.annotation.tailrec

/** A user action or platform fact the run storage panel's view dispatches. */
enum StorageIntent derives CanEqual:
  /** The platform listed the run store under the session's retention basis. */
  case Loaded(storage: RunStorage)
  case LoadFailed(reason: String)

  /** Choose or unchoose a prunable archive; a kept one cannot be chosen. */
  case Toggle(run: RunId)
  case ChooseAllPrunable

  /** "Prune…": ask for confirmation of the chosen archives. */
  case RequestPrune
  case Confirm
  case Cancel
  case Pruned(report: PruneReport)
  case PruneFailed(reason: String)

/** Work the panel asks the runtime to do, as data. */
enum StorageEffect derives CanEqual:
  /** List the run store again (`RunStore.storage`). */
  case Refresh

  /** Prune exactly the confirmed archives (`RunStore.prune`), which the
    * store re-checks against its retention basis.
    */
  case Prune(confirmed: ConfirmedPrune)

/** One stored archive as the panel shows it. */
final case class StorageRow(
    run: RunId,
    label: String,
    size: String,
    status: String,
    choosable: Boolean,
    chosen: Boolean
) derives CanEqual

/** The "Prune run results?" confirmation. */
final case class PruneDialog(title: String, body: String, confirm: String, cancel: String)
    derives CanEqual

/** What the panel shows. */
final case class StorageView(
    rows: Vector[StorageRow],
    summary: String,
    pruneEnabled: Boolean,
    dialog: Option[PruneDialog],
    notice: Option[String]
) derives CanEqual

/** The run storage panel (S2.6): every stored run archive with its size,
  * whether it is kept and why, and pruning with confirmation. It computes
  * nothing about runs itself: the store classifies archives, a plan is
  * built only from that classification, and only a confirmed plan becomes
  * an effect.
  */
final case class RunStoragePanel private (
    storage: Option[RunStorage],
    chosen: Vector[RunId],
    confirming: Option[PrunePlan],
    busy: Boolean,
    notice: Option[String]
) derives CanEqual:
  import StorageIntent.*

  def update(intent: StorageIntent): (RunStoragePanel, Vector[StorageEffect]) = intent match
    case Loaded(next) =>
      val prunable = next.prunable.map(_.run)
      (copy(storage = Some(next), chosen = chosen.filter(prunable.contains)), Vector.empty)
    case LoadFailed(reason) => (copy(notice = Some(reason)), Vector.empty)
    case Toggle(run)        =>
      val prunable = storage.exists(_.prunable.exists(_.run == run))
      if busy || confirming.isDefined || !prunable then (this, Vector.empty)
      else if chosen.contains(run) then
        (copy(chosen = chosen.filterNot(_ == run)), Vector.empty)
      else (copy(chosen = (chosen :+ run).sortBy(_.number)), Vector.empty)
    case ChooseAllPrunable =>
      if busy || confirming.isDefined then (this, Vector.empty)
      else (copy(chosen = storage.toVector.flatMap(_.prunable.map(_.run))), Vector.empty)
    case RequestPrune =>
      storage match
        case Some(s) if !busy && confirming.isEmpty =>
          PrunePlan.of(s, chosen) match
            case Right(plan) => (copy(confirming = Some(plan), notice = None), Vector.empty)
            case Left(error) => (copy(notice = Some(error.message)), Vector.empty)
        case _ => (this, Vector.empty)
    case Confirm =>
      confirming match
        case Some(plan) =>
          (copy(confirming = None, busy = true), Vector(StorageEffect.Prune(plan.confirm)))
        case None => (this, Vector.empty)
    case Cancel         => (copy(confirming = None), Vector.empty)
    case Pruned(report) =>
      (
        copy(chosen = Vector.empty, busy = false, notice = Some(RunStorageText.pruned(report))),
        Vector(StorageEffect.Refresh)
      )
    case PruneFailed(reason) =>
      (copy(busy = false, notice = Some(reason)), Vector(StorageEffect.Refresh))

  /** The view of the panel over `document`, which names the runs. */
  def view(document: StudioDocument): StorageView =
    val rows = storage.toVector.flatMap(_.rows).map { row =>
      val run = row.run
      StorageRow(
        run,
        document.run(run).fold(s"${run.label} · not in this document")(_.label),
        RunStorageText.size(row.record),
        RunStorageText.status(row),
        !busy && row.isInstanceOf[RunRetention.Prunable],
        chosen.contains(run)
      )
    }
    StorageView(
      rows,
      storage.fold(RunStorageText.Loading)(RunStorageText.summary),
      !busy && confirming.isEmpty && chosen.nonEmpty,
      confirming.map(RunStorageText.dialog),
      notice
    )

object RunStoragePanel:
  /** A panel that has not listed the store yet; it asks to. */
  def start: (RunStoragePanel, Vector[StorageEffect]) =
    (RunStoragePanel(None, Vector.empty, None, false, None), Vector(StorageEffect.Refresh))

/** The run storage panel's English text. */
object RunStorageText:
  val Loading: String = "Reading stored run results…"

  private val Units = Vector("kB", "MB", "GB", "TB", "PB", "EB")

  /** A byte count in decimal units, one decimal place: "812 B", "4.1 kB",
    * "12.4 MB". A value that would round to 1000.0 moves to the next unit.
    */
  def bytes(count: Long): String =
    @tailrec
    def scale(value: Double, unit: Int): (Double, Int) =
      if value >= 999.95 && unit < Units.size - 1 then scale(value / 1000.0, unit + 1)
      else (value, unit)
    if count < 1000L then s"${Format.count(count)} B"
    else
      val (value, unit) = scale(count.toDouble / 1000.0, 0)
      s"${Format.decimal(value, 1)} ${Units(unit)}"

  def size(record: ArchiveRecord): String = record.size.fold("size unknown")(bytes)

  private def reason(r: KeepReason): String = r match
    case KeepReason.Figure(figure, source) =>
      source match
        case BindingSource.Current  => figure.label
        case BindingSource.Undo     => s"${figure.label} (undo)"
        case BindingSource.Saved    => s"${figure.label} (saved)"
        case BindingSource.Previous => s"${figure.label} (previous save)"
    case KeepReason.Shown   => "shown"
    case KeepReason.Running => "running"
    case KeepReason.Ready   => "ready to show"

  def status(row: RunRetention): String =
    val incomplete = row.record match
      case ArchiveRecord.Incomplete(_, _) => "Incomplete · "
      case _                              => ""
    row match
      case RunRetention.Kept(_, reasons) =>
        s"${incomplete}Kept · ${reasons.map(reason).mkString(", ")}"
      case RunRetention.Prunable(_) => s"${incomplete}Prunable"

  def summary(storage: RunStorage): String =
    val n = storage.rows.size
    if n == 0 then "No stored run results"
    else
      val archives = if n == 1 then "1 stored run" else s"${Format.count(n.toLong)} stored runs"
      s"$archives · ${bytes(storage.bytes)} · ${bytes(storage.prunableBytes)} prunable"

  private def runs(ids: Vector[RunId]): String = ids.map(_.label) match
    case Vector(one)  => one
    case init :+ last => s"${init.mkString(", ")} and $last"
    case _            => ""

  def dialog(plan: PrunePlan): PruneDialog =
    val size = (if plan.sizeUnknown then "at least " else "") + bytes(plan.bytes)
    PruneDialog(
      "Prune run results?",
      s"Delete the stored results of ${runs(plan.runs)} ($size). The runs stay in the " +
        "project, but their results will no longer open. Results that figures, the shown " +
        "run or a running run need are kept.",
      "Prune",
      "Cancel"
    )

  def pruned(report: PruneReport): String =
    s"Pruned ${runs(report.runs)}; ${bytes(report.freedBytes)} freed."

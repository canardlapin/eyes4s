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

package eyes4s.studio.app.keys

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.execution.JobPhase
import eyes4s.studio.app.text.MessageId
import eyes4s.studio.core.command.{CommandError, HistoryStack}
import eyes4s.studio.core.document.Perspective

/** A registered command's identity ("perspective.compare", "edit.undo"). */
final case class CommandId private (value: String) derives CanEqual

object CommandId:
  /** Lookup by id text, for scripting (S3.6): only registered ids resolve. */
  def parse(value: String): Option[CommandId] =
    CommandRegistry.all.map(_.id).find(_.value == value)

  private[keys] def declared(value: String): CommandId = new CommandId(value)

/** A user action as data: its id, label, shortcut, and the intent it stands
  * for in a given model. It is enabled exactly when that intent exists
  * (S1.9: the menu bar and keymap are renderings of these).
  */
final class AppCommand private[keys] (
    val id: CommandId,
    val label: MessageId,
    val shortcut: Option[KeyChord],
    resolve: AppModel => Option[Intent],
    why: AppModel => Option[CommandError] = (_: AppModel) => None
):
  /** The intent this command dispatches now; never another Invoke or key. */
  def intent(model: AppModel): Option[Intent] = resolve(model)

  def enabled(model: AppModel): Boolean = resolve(model).isDefined

  /** Why a disabled command is disabled, when the history says so (undo at
    * a Save & run barrier: S2.2's UndoBlocked).
    */
  def reason(model: AppModel): Option[CommandError] =
    if enabled(model) then None else why(model)

  override def toString: String = s"AppCommand(${id.value})"

/** The command registry and keymap (ticket S1.0; rendered by S1.9). */
object CommandRegistry:

  private def always(intent: Intent): AppModel => Option[Intent] = _ => Some(intent)

  private def perspective(p: Perspective, key: Key, label: MessageId) =
    AppCommand(
      CommandId.declared(s"perspective.${p.label.toLowerCase}"),
      label,
      Some(KeyChord.command(key)),
      always(Intent.SwitchPerspective(p))
    )

  val data: AppCommand    = perspective(Perspective.Data, Key.Digit1, MessageId.PerspectiveData)
  val explore: AppCommand =
    perspective(Perspective.Explore, Key.Digit2, MessageId.PerspectiveExplore)
  val analysis: AppCommand =
    perspective(Perspective.Analysis, Key.Digit3, MessageId.PerspectiveAnalysis)
  val compare: AppCommand =
    perspective(Perspective.Compare, Key.Digit4, MessageId.PerspectiveCompare)
  val figures: AppCommand =
    perspective(Perspective.Figures, Key.Digit5, MessageId.PerspectiveFigures)

  val back: AppCommand = AppCommand(
    CommandId.declared("navigate.back"),
    MessageId.Back,
    Some(KeyChord.command(Key.BracketLeft)),
    m => Option.when(m.navigation.canGoBack)(Intent.Back)
  )

  val forward: AppCommand = AppCommand(
    CommandId.declared("navigate.forward"),
    MessageId.Forward,
    Some(KeyChord.command(Key.BracketRight)),
    m => Option.when(m.navigation.canGoForward)(Intent.Forward)
  )

  val undo: AppCommand = AppCommand(
    CommandId.declared("edit.undo"),
    MessageId.CommandUndo,
    Some(KeyChord.command(Key.Z)),
    m => Option.when(m.history.science.canUndo)(Intent.Undo(HistoryStack.Science)),
    m => m.history.undoOn(HistoryStack.Science).left.toOption
  )

  val redo: AppCommand = AppCommand(
    CommandId.declared("edit.redo"),
    MessageId.CommandRedo,
    Some(KeyChord.commandShift(Key.Z)),
    m => Option.when(m.history.science.canRedo)(Intent.Redo(HistoryStack.Science)),
    m => m.history.redoOn(HistoryStack.Science).left.toOption
  )

  val undoView: AppCommand = AppCommand(
    CommandId.declared("view.undo"),
    MessageId.CommandUndoView,
    None,
    m => Option.when(m.history.presentation.canUndo)(Intent.Undo(HistoryStack.Presentation)),
    m => m.history.undoOn(HistoryStack.Presentation).left.toOption
  )

  val redoView: AppCommand = AppCommand(
    CommandId.declared("view.redo"),
    MessageId.CommandRedoView,
    None,
    m => Option.when(m.history.presentation.canRedo)(Intent.Redo(HistoryStack.Presentation)),
    m => m.history.redoOn(HistoryStack.Presentation).left.toOption
  )

  val nextPane: AppCommand = AppCommand(
    CommandId.declared("pane.next"),
    MessageId.CommandNextPane,
    Some(KeyChord.plain(Key.F6)),
    m => Option.when(m.layout.groups.size > 1)(Intent.FocusNextPane)
  )

  val maximize: AppCommand = AppCommand(
    CommandId.declared("pane.maximize"),
    MessageId.CommandMaximize,
    Some(KeyChord.commandShift(Key.Enter)),
    always(Intent.ToggleMaximize)
  )

  /** Cancel the active job, when its run is still running in the document. */
  val cancelRun: AppCommand = AppCommand(
    CommandId.declared("run.cancel"),
    MessageId.CommandCancelRun,
    None,
    m =>
      m.jobs.active
        .filter(_.phase match
          case JobPhase.Queued | JobPhase.Running(_) => true
          case _                                     => false)
        .filter(j => m.document.job(j.run).contains(j.id))
        .map(j => Intent.CancelJob(j.id))
  )

  /** Show the run the shelf holds ready; disabled exactly when it holds none. */
  val showRun: AppCommand = AppCommand(
    CommandId.declared("run.show"),
    MessageId.CommandShowRun,
    None,
    m => m.jobs.ready.map(notice => Intent.ShowRun(notice.run))
  )

  val reviewDraft: AppCommand = AppCommand(
    CommandId.declared("draft.review"),
    MessageId.CommandReviewDraft,
    None,
    m => m.document.draft.map(_ => Intent.ReviewDraft)
  )

  val discardDraft: AppCommand = AppCommand(
    CommandId.declared("draft.discard"),
    MessageId.CommandDiscardDraft,
    None,
    m => m.document.draft.map(_ => Intent.RequestDiscardDraft)
  )

  val importSources: AppCommand = AppCommand(
    CommandId.declared("data.import"),
    MessageId.CommandImport,
    None,
    always(Intent.RequestImport)
  )

  /** Every command, in menu order. */
  val all: Vector[AppCommand] = Vector(
    data,
    explore,
    analysis,
    compare,
    figures,
    back,
    forward,
    undo,
    redo,
    undoView,
    redoView,
    nextPane,
    maximize,
    cancelRun,
    showRun,
    reviewDraft,
    discardDraft,
    importSources
  )

  def find(id: CommandId): Option[AppCommand] = all.find(_.id == id)

  /** Each shortcut and the command it invokes. */
  val keymap: Map[KeyChord, CommandId] =
    all.flatMap(c => c.shortcut.map(_ -> c.id)).toMap

  /** A command's shortcut as the boards print it ("⌘1"), or "". */
  def shortcutText(command: AppCommand): String = command.shortcut.fold("")(_.render)

  def forPerspective(p: Perspective): AppCommand = p match
    case Perspective.Data     => data
    case Perspective.Explore  => explore
    case Perspective.Analysis => analysis
    case Perspective.Compare  => compare
    case Perspective.Figures  => figures

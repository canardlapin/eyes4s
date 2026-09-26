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
import eyes4s.studio.app.text.{MessageId, Messages}
import eyes4s.studio.core.command.{CommandError, HistoryStack}
import eyes4s.studio.core.document.Perspective

/** A registered command's identity ("perspective.compare", "edit.undo"). */
final case class CommandId private (value: String) derives CanEqual

object CommandId:
  /** Lookup by id text, for scripting (S3.6): only registered ids resolve. */
  def parse(value: String): Option[CommandId] =
    CommandRegistry.all.map(_.id).find(_.value == value)

  private[keys] def declared(value: String): CommandId = new CommandId(value)

/** The menu of the system menu bar a command is listed in (S1.9), in bar
  * order. macOS adds the application menu itself.
  */
enum MenuSection derives CanEqual:
  case File, Edit, View, Go, Run, Window

  def title: MessageId = this match
    case File   => MessageId.MenuFile
    case Edit   => MessageId.MenuEdit
    case View   => MessageId.MenuView
    case Go     => MessageId.MenuGo
    case Run    => MessageId.MenuRun
    case Window => MessageId.MenuWindow

/** A user action as data: its id, label, shortcut, and the intent it stands
  * for in a given model. It is enabled exactly when that intent exists
  * (S1.9: the menu bar and keymap are renderings of these).
  */
final class AppCommand private[keys] (
    val id: CommandId,
    val label: MessageId,
    val section: MenuSection,
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
      MenuSection.View,
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
    MessageId.CommandBack,
    MenuSection.Go,
    Some(KeyChord.command(Key.BracketLeft)),
    m => Option.when(m.navigation.canGoBack)(Intent.Back)
  )

  val forward: AppCommand = AppCommand(
    CommandId.declared("navigate.forward"),
    MessageId.CommandForward,
    MenuSection.Go,
    Some(KeyChord.command(Key.BracketRight)),
    m => Option.when(m.navigation.canGoForward)(Intent.Forward)
  )

  val undo: AppCommand = AppCommand(
    CommandId.declared("edit.undo"),
    MessageId.CommandUndo,
    MenuSection.Edit,
    Some(KeyChord.command(Key.Z)),
    m => Option.when(m.history.science.canUndo)(Intent.Undo(HistoryStack.Science)),
    m => m.history.undoOn(HistoryStack.Science).left.toOption
  )

  val redo: AppCommand = AppCommand(
    CommandId.declared("edit.redo"),
    MessageId.CommandRedo,
    MenuSection.Edit,
    Some(KeyChord.commandShift(Key.Z)),
    m => Option.when(m.history.science.canRedo)(Intent.Redo(HistoryStack.Science)),
    m => m.history.redoOn(HistoryStack.Science).left.toOption
  )

  val undoView: AppCommand = AppCommand(
    CommandId.declared("view.undo"),
    MessageId.CommandUndoView,
    MenuSection.Edit,
    None,
    m => Option.when(m.history.presentation.canUndo)(Intent.Undo(HistoryStack.Presentation)),
    m => m.history.undoOn(HistoryStack.Presentation).left.toOption
  )

  val redoView: AppCommand = AppCommand(
    CommandId.declared("view.redo"),
    MessageId.CommandRedoView,
    MenuSection.Edit,
    None,
    m => Option.when(m.history.presentation.canRedo)(Intent.Redo(HistoryStack.Presentation)),
    m => m.history.redoOn(HistoryStack.Presentation).left.toOption
  )

  val nextPane: AppCommand = AppCommand(
    CommandId.declared("pane.next"),
    MessageId.CommandNextPane,
    MenuSection.Window,
    Some(KeyChord.plain(Key.F6)),
    m => Option.when(m.layout.groups.size > 1)(Intent.FocusNextPane)
  )

  val previousPane: AppCommand = AppCommand(
    CommandId.declared("pane.previous"),
    MessageId.CommandPreviousPane,
    MenuSection.Window,
    Some(KeyChord.shift(Key.F6)),
    m => Option.when(m.layout.groups.size > 1)(Intent.FocusPreviousPane)
  )

  val nextTab: AppCommand = AppCommand(
    CommandId.declared("tab.next"),
    MessageId.CommandNextTab,
    MenuSection.Window,
    Some(KeyChord.control(Key.Tab)),
    always(Intent.NextTab)
  )

  val previousTab: AppCommand = AppCommand(
    CommandId.declared("tab.previous"),
    MessageId.CommandPreviousTab,
    MenuSection.Window,
    Some(KeyChord.controlShift(Key.Tab)),
    always(Intent.PreviousTab)
  )

  val maximize: AppCommand = AppCommand(
    CommandId.declared("pane.maximize"),
    MessageId.CommandMaximize,
    MenuSection.Window,
    Some(KeyChord.commandShift(Key.Enter)),
    always(Intent.ToggleMaximize)
  )

  /** Cancel the active job, when its run is still running in the document. */
  val cancelRun: AppCommand = AppCommand(
    CommandId.declared("run.cancel"),
    MessageId.CommandCancelRun,
    MenuSection.Run,
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
    MenuSection.Run,
    None,
    m => m.jobs.ready.map(notice => Intent.ShowRun(notice.run))
  )

  val reviewDraft: AppCommand = AppCommand(
    CommandId.declared("draft.review"),
    MessageId.CommandReviewDraft,
    MenuSection.Run,
    None,
    m => m.document.draft.map(_ => Intent.ReviewDraft)
  )

  val discardDraft: AppCommand = AppCommand(
    CommandId.declared("draft.discard"),
    MessageId.CommandDiscardDraft,
    MenuSection.Run,
    None,
    m => m.document.draft.map(_ => Intent.RequestDiscardDraft)
  )

  val importSources: AppCommand = AppCommand(
    CommandId.declared("data.import"),
    MessageId.CommandImport,
    MenuSection.File,
    None,
    always(Intent.RequestImport)
  )

  // --- The project chip's menu (S1.4) and View (S1.5a) -----------------------

  val renameProject: AppCommand = AppCommand(
    CommandId.declared("project.rename"),
    MessageId.CommandRenameProject,
    MenuSection.File,
    None,
    always(Intent.RequestRename)
  )

  /** Only a saved project has a place to reveal. */
  val revealProject: AppCommand = AppCommand(
    CommandId.declared("project.reveal"),
    MessageId.CommandRevealProject,
    MenuSection.File,
    None,
    m => m.project.map(_ => Intent.RevealProject)
  )

  val projectInfo: AppCommand = AppCommand(
    CommandId.declared("project.info"),
    MessageId.CommandProjectInfo,
    MenuSection.File,
    None,
    always(Intent.ShowProjectInfo)
  )

  val resetPerspective: AppCommand = AppCommand(
    CommandId.declared("view.reset-perspective"),
    MessageId.CommandResetPerspective,
    MenuSection.View,
    None,
    always(Intent.ResetPerspective)
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
    previousPane,
    nextTab,
    previousTab,
    maximize,
    cancelRun,
    showRun,
    reviewDraft,
    discardDraft,
    importSources,
    renameProject,
    revealProject,
    projectInfo,
    resetPerspective
  )

  def find(id: CommandId): Option[AppCommand] = all.find(_.id == id)

  /** Each shortcut and the command it invokes. */
  val keymap: Map[KeyChord, CommandId] =
    all.flatMap(c => c.shortcut.map(_ -> c.id)).toMap

  /** The chords a menu item carries as its accelerator: every command with a
    * shortcut is an item of the menu bar (S1.9).
    */
  lazy val menuAccelerators: Set[KeyChord] = menus.flatMap(_._2).flatMap(_.shortcut).toSet

  /** The chords a window's own key handler dispatches. Where the menu bar is
    * native (macOS), the menu's accelerator is the only path for its chord,
    * so one key press can never dispatch its command twice; elsewhere the
    * menu bar is hidden and the window handles every chord.
    */
  def windowKeymap(nativeMenu: Boolean): Map[KeyChord, CommandId] =
    if nativeMenu then keymap.filterNot((chord, _) => menuAccelerators(chord)) else keymap

  /** A command's shortcut as the boards print it ("⌘1"), or "". */
  def shortcutText(command: AppCommand): String = command.shortcut.fold("")(_.render)

  /** The commands of each menu, in bar order and, within a menu, in
    * registry order. Every command is in exactly one menu.
    */
  def menus: Vector[(MenuSection, Vector[AppCommand])] =
    MenuSection.values.toVector.map(s => s -> all.filter(_.section == s)).filter(_._2.nonEmpty)

  /** docs/studio/SHORTCUTS.md, generated from the registry (S1.9): one row
    * per command, grouped by menu, with its id and shortcut. A test checks
    * the committed file against this text.
    */
  def shortcutTable(messages: Messages = Messages.english): String =
    def cell(s: String) = s.replace("|", "\\|")
    val rows            = menus.flatMap { (section, commands) =>
      commands.map { c =>
        val keys = c.shortcut.fold("—")(k => s"`${cell(k.render)}`")
        s"| ${messages(section.title)} | ${cell(messages(c.label))} | $keys | `${c.id.value}` |"
      }
    }
    (Vector(
      "# Eyes Studio keyboard shortcuts",
      "",
      "Generated from `CommandRegistry` (studio-app, ticket S1.9); do not edit by hand.",
      "`KeymapFxSuite` fails when this file and the registry disagree; regenerate",
      "it with `EYES4S_UPDATE_GOLDENS=1`.",
      "",
      "⌘ is Command on macOS and Control on Linux and Windows. Every command is",
      "also an item of the menu named in the first column. Inside a plot, the",
      "arrow keys move a roving cursor, Enter selects and Esc clears; Tab leaves",
      "the plot (DESIGN_SPEC section 10).",
      "",
      "On macOS the native menu bar's accelerators are the only path for these",
      "chords (the window's key handler skips them, `CommandRegistry.windowKeymap`),",
      "so a key press cannot fire a command twice. An item with a shortcut stays",
      "enabled there, so a disabled command still says why (\"Undo: There is no",
      "edit to undo.\") instead of the menu swallowing the key.",
      "Verified by hand on macOS: pending. The hand check also covers text fields:",
      "while a text field has focus, the native menu sees ⌘Z (and ⌘⇧Z) before the",
      "field. Expected: the field's own undo wins while it has focus; the document's",
      "Undo applies only outside text fields. Pending: confirm which one wins today.",
      "",
      "| Menu | Command | Shortcut | Id |",
      "|---|---|---|---|"
    ) ++ rows).mkString("", "\n", "\n")

  def forPerspective(p: Perspective): AppCommand = p match
    case Perspective.Data     => data
    case Perspective.Explore  => explore
    case Perspective.Analysis => analysis
    case Perspective.Compare  => compare
    case Perspective.Figures  => figures

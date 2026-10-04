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

package eyes4s.studio.app.vm

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.keys.{AppCommand, CommandId, CommandRegistry, KeyChord, MenuSection}
import eyes4s.studio.app.layout.{PaneId, PaneKind, StudioLayouts}
import eyes4s.studio.app.text.{MessageId, Messages}

/** A menu: its title and its items, in order. */
final case class MenuVM(title: String, items: Vector[ActionVM]) derives CanEqual

/** One item of the system menu bar: a registered command, its shortcut
  * (the menu shows it as its accelerator), and what it dispatches now.
  */
final case class MenuItemVM(
    command: CommandId,
    label: String,
    shortcut: Option[KeyChord],
    enabled: Boolean,
    intent: Intent,
    submenu: Option[String] = None,
    checked: Option[Boolean] = None
) derives CanEqual

/** One menu of the system menu bar. */
final case class BarMenuVM(section: MenuSection, title: String, items: Vector[MenuItemVM])
    derives CanEqual

/** The shell's menus and window title (tickets S1.4 and S1.5a): pure
  * projections of the model, like [[Shell]].
  */
object Menus:

  /** A command as a menu item: enabled exactly when it resolves; a disabled
    * item carries its Invoke, which explains itself with a notice.
    */
  def item(command: AppCommand, model: AppModel, messages: Messages): ActionVM =
    ActionVM(
      messages(command.label),
      command.enabled(model),
      command.intent(model).getOrElse(Intent.Invoke(command.id))
    )

  /** The project chip's menu: Rename…, Reveal in Finder, Project info. */
  def project(model: AppModel, messages: Messages = Messages.english): Vector[ActionVM] =
    Vector(
      CommandRegistry.renameProject,
      CommandRegistry.revealProject,
      CommandRegistry.projectInfo
    ).map(item(_, model, messages))

  /** The macOS system menu bar (S1.9): every registered command, in its
    * menu, generated from the [[CommandRegistry]]. A disabled item carries
    * its Invoke, which explains itself with a notice.
    */
  def bar(model: AppModel, messages: Messages = Messages.english): Vector[BarMenuVM] =
    CommandRegistry.menus.map { (section, commands) =>
      BarMenuVM(
        section,
        messages(section.title),
        commands.map { c =>
          val a = item(c, model, messages)
          MenuItemVM(
            c.id,
            a.label,
            c.shortcut,
            a.enabled,
            a.intent,
            c.submenu.map(messages(_)),
            c.checked(model)
          )
        }
      )
    }

  /** The studio's items in a dock tab's context menu (S1.9, through
    * scaladock's tab-menu hook, after the dock's own items): a plot's
    * "Show table" (its keyboard twin, DESIGN_SPEC section 3), a table's
    * "Show plot", and Reset perspective. A pane outside the current layout
    * gets none.
    */
  def tab(
      model: AppModel,
      pane: PaneId,
      messages: Messages = Messages.english
  ): Vector[ActionVM] =
    val layout = model.layout
    layout.pane(pane).toVector.flatMap { decl =>
      val sibling = StudioLayouts.twin(layout, pane).map { t =>
        val label =
          if decl.kind == PaneKind.Plot then MessageId.TabShowTable else MessageId.TabShowPlot
        ActionVM(messages(label), true, Intent.FocusPane(t.id))
      }
      sibling.toVector :+ item(CommandRegistry.resetPerspective, model, messages)
    }

  /** View › Reset perspective. S1.9 adds the rest of the menu bar. */
  def view(model: AppModel, messages: Messages = Messages.english): MenuVM =
    MenuVM(
      messages(MessageId.MenuView),
      Vector(item(CommandRegistry.resetPerspective, model, messages))
    )

  /** The native window's title: "memory-study.eyes", marked while edited
    * ("memory-study.eyes — Edited"), since JavaFX has no document-edited dot.
    */
  def windowTitle(model: AppModel, messages: Messages = Messages.english): String =
    val window = Shell.project(model, messages).window
    if window.edited then messages(MessageId.WindowEdited, window.title) else window.title

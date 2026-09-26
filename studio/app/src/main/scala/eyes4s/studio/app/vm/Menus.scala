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
import eyes4s.studio.app.keys.{AppCommand, CommandRegistry}
import eyes4s.studio.app.text.{MessageId, Messages}

/** A menu: its title and its items, in order. */
final case class MenuVM(title: String, items: Vector[ActionVM]) derives CanEqual

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

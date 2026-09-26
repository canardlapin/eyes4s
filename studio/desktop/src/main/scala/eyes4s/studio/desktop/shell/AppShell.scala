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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.keys.CommandRegistry
import eyes4s.studio.app.text.Messages
import eyes4s.studio.app.vm.{Menus, Shell}
import eyes4s.studio.desktop.dock.PerspectiveHost
import javafx.scene.control.{Menu, MenuBar, MenuItem}
import javafx.scene.input.KeyEvent
import javafx.scene.layout.{Priority, StackPane, VBox}

/** The window's content (tickets S1.4, S1.5a): app bar, context strip,
  * draft banner, the perspective host's dock, and the status bar, top to
  * bottom (DESIGN_SPEC section 3). It renders the shell view-models of each
  * model and dispatches the intents they carry; keys go through the app's
  * keymap.
  */
final class AppShell(host: PerspectiveHost, dispatch: Intent => Unit, messages: Messages):

  val appBar: AppBar             = AppBar(dispatch)
  val contextStrip: ContextStrip = ContextStrip(dispatch)
  val banner: DraftBanner        = DraftBanner(dispatch)
  val statusBar: StatusBar       = StatusBar(dispatch)

  /** The dock area: grows to fill the window. */
  val dockArea: StackPane = StackPane(host.dock.view)
  dockArea.getStyleClass.add("dock-area")
  VBox.setVgrow(dockArea, Priority.ALWAYS)

  /** View › Reset perspective. The system menu bar on macOS; elsewhere S1.9
    * decides where the menu bar goes, so it is kept out of the layout.
    */
  val menuBar: MenuBar = MenuBar()
  menuBar.setUseSystemMenuBar(true)
  if !AppShell.systemMenuBar then
    menuBar.setManaged(false)
    menuBar.setVisible(false)

  val root: VBox =
    VBox(menuBar, appBar.node, contextStrip.node, banner.node, dockArea, statusBar.node)
  root.getStyleClass.add("studio-shell")

  root.addEventHandler(
    KeyEvent.KEY_PRESSED,
    (e: KeyEvent) =>
      ShellKeys.chord(e).filter(CommandRegistry.keymap.contains).foreach { c =>
        dispatch(Intent.KeyPressed(c))
        e.consume()
      }
  )

  def render(model: AppModel): Unit =
    val vm = Shell.project(model, messages)
    appBar.render(vm.appBar, Menus.project(model, messages))
    contextStrip.render(vm.context)
    banner.render(vm.banner)
    statusBar.render(vm.status)
    val view = Menus.view(model, messages)
    val menu = Menu(view.title)
    menu.getItems.setAll(view.items.map { a =>
      val item = MenuItem(a.label)
      item.setMnemonicParsing(false)
      item.setDisable(!a.enabled)
      item.setOnAction(_ => dispatch(a.intent))
      item
    }*)
    menuBar.getMenus.setAll(menu)
    host.sync(model)

object AppShell:
  /** Whether JavaFX draws the menu bar in the platform's own place. */
  val systemMenuBar: Boolean =
    sys.props.get("os.name").exists(_.toLowerCase.contains("mac"))

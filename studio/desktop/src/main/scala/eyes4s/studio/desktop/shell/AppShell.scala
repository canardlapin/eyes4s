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
import eyes4s.studio.app.vm.{BarMenuVM, MenuItemVM, Menus, Shell}
import eyes4s.studio.desktop.dock.PerspectiveHost
import javafx.scene.control.{
  Menu,
  MenuBar,
  MenuItem,
  RadioMenuItem,
  SeparatorMenuItem,
  ToggleGroup
}
import javafx.scene.input.KeyEvent
import javafx.scene.layout.{Priority, StackPane, VBox}

/** The window's content (tickets S1.4–S1.9): app bar, context strip,
  * notice, confirmation, draft banner, the perspective host's dock, and the
  * status bar, top to bottom (DESIGN_SPEC section 3). It renders the shell
  * view-models of each model and dispatches the intents they carry; keys go
  * through the app's keymap. `model` is read when a tab's context menu opens.
  */
final class AppShell(
    host: PerspectiveHost,
    dispatch: Intent => Unit,
    messages: Messages,
    model: () => AppModel,
    /** Whether the menu bar is the platform's native one (macOS). A test
      * of the window's own key path passes `false`, as on Linux.
      */
    val nativeMenu: Boolean = AppShell.systemMenuBar
):

  val appBar: AppBar             = AppBar(dispatch)
  val contextStrip: ContextStrip = ContextStrip(dispatch)
  val banner: DraftBanner        = DraftBanner(dispatch)
  val statusBar: StatusBar       = StatusBar(dispatch)
  val notice: NoticeBar          = NoticeBar(dispatch, messages)
  val confirmation: ConfirmBar   = ConfirmBar(dispatch)

  /** The dock area: grows to fill the window. */
  val dockArea: StackPane = StackPane(host.dock.view)
  dockArea.getStyleClass.add("dock-area")
  VBox.setVgrow(dockArea, Priority.ALWAYS)

  /** The menu bar generated from the command registry (S1.9): the system
    * menu bar on macOS. Elsewhere it is kept out of the layout, and the
    * keymap below carries the shortcuts.
    */
  val menuBar: MenuBar = MenuBar()
  menuBar.setUseSystemMenuBar(nativeMenu)
  menuBar.setFocusTraversable(false)
  if !nativeMenu then
    menuBar.setManaged(false)
    menuBar.setVisible(false)

  val root: VBox =
    VBox(
      menuBar,
      appBar.node,
      contextStrip.node,
      notice.node,
      confirmation.node,
      banner.node,
      dockArea,
      statusBar.node
    )
  root.getStyleClass.add("studio-shell")

  // The keymap: a registered chord that no focused control consumed is the
  // app's; it is consumed here, so the scene's menu accelerators (which run
  // after the handlers) never dispatch it a second time.
  /** The chords this window handles itself now (S1.9): none that an enabled
    * native menu item carries.
    */
  def windowKeys: Map[eyes4s.studio.app.keys.KeyChord, eyes4s.studio.app.keys.CommandId] =
    CommandRegistry.windowKeymap(nativeMenu, model())

  root.addEventHandler(
    KeyEvent.KEY_PRESSED,
    (e: KeyEvent) =>
      ShellKeys.chords(e).find(windowKeys.contains).foreach { c =>
        dispatch(Intent.KeyPressed(c))
        e.consume()
      }
  )

  /** A tab's context menu (S1.9, scaladock's `setTabMenu` hook): the dock's
    * own items, then the studio's for the model as it is when it opens.
    */
  def tabMenu(pane: scaladock.PaneId, defaults: Vector[MenuItem]): Vector[MenuItem] =
    val studio = host
      .studioPane(pane)
      .toVector
      .flatMap(p => Menus.tab(model(), p, messages))
      .map(a => AppShell.item(a.label, a.enabled, () => dispatch(a.intent)))
    if studio.isEmpty then defaults else (defaults :+ SeparatorMenuItem()) ++ studio

  host.dock.setTabMenu((context, defaults) => tabMenu(context.pane, defaults))

  def render(model: AppModel): Unit =
    val vm = Shell.project(model, messages)
    appBar.render(vm.appBar, Menus.project(model, messages))
    contextStrip.render(vm.context)
    banner.render(vm.banner)
    statusBar.render(vm.status)
    notice.render(vm.notice)
    confirmation.render(vm.confirmation)
    renderMenus(Menus.bar(model, messages))
    host.sync(model)

  private def menuItem(i: MenuItemVM): MenuItem =
    val item = i.checked match
      case Some(on) =>
        val r = RadioMenuItem(i.label)
        r.setSelected(on)
        r.setMnemonicParsing(false)
        r.setDisable(!i.enabled)
        r.setOnAction(_ => dispatch(i.intent))
        r
      case None => AppShell.item(i.label, i.enabled, () => dispatch(i.intent))
    item.setId(i.command.value)
    i.shortcut.map(ShellKeys.combination).foreach(item.setAccelerator)
    item

  private var shownMenus: Vector[BarMenuVM] = Vector.empty

  /** The menus as drawn, by title, for tests. */
  def menus: Vector[Menu] =
    import scala.jdk.CollectionConverters.*
    menuBar.getMenus.asScala.toVector

  /** Each menu is rebuilt only when its view-model changes, so an open menu
    * is not replaced under the pointer on every update.
    */
  private def renderMenus(bar: Vector[BarMenuVM]): Unit =
    if shownMenus != bar then
      val before = shownMenus
      shownMenus = bar
      if before.map(_.section) != bar.map(_.section) then
        menuBar.getMenus.setAll(bar.map(m => Menu(m.title))*): Unit
      bar.zip(menus).zipWithIndex.foreach { case ((vm, menu), i) =>
        if !before.lift(i).contains(vm) then
          menu.setText(vm.title)
          menu.setMnemonicParsing(false)
          menu.getItems.setAll(
            AppShell
              .grouped(vm.items)
              .map {
                case (None, items)        => items.map(menuItem)
                case (Some(title), items) =>
                  // A submenu of choices (View › Appearance): radio items, the
                  // current one selected.
                  val sub   = Menu(title)
                  val group = ToggleGroup()
                  sub.setMnemonicParsing(false)
                  sub.getItems.setAll(items.map(menuItem)*)
                  sub.getItems.forEach {
                    case r: RadioMenuItem => r.setToggleGroup(group)
                    case _                => ()
                  }
                  Vector(sub)
              }
              .flatten*
          ): Unit
      }

object AppShell:

  /** A menu's items, runs of one submenu together, in order. */
  def grouped(items: Vector[MenuItemVM]): Vector[(Option[String], Vector[MenuItemVM])] =
    items.foldLeft(Vector.empty[(Option[String], Vector[MenuItemVM])]) { (acc, i) =>
      acc.lastOption match
        case Some((sub, run)) if sub == i.submenu => acc.init :+ (sub  -> (run :+ i))
        case _                                    => acc :+ (i.submenu -> Vector(i))
    }

  /** Whether JavaFX draws the menu bar in the platform's own place. */
  val systemMenuBar: Boolean =
    sys.props.get("os.name").exists(_.toLowerCase.contains("mac"))

  private def item(label: String, enabled: Boolean, action: () => Unit): MenuItem =
    val item = MenuItem(label)
    item.setMnemonicParsing(false)
    item.setDisable(!enabled)
    item.setOnAction(_ => action())
    item

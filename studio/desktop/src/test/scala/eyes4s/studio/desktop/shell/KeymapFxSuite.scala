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

import eyes4s.studio.app.{AppModel, Intent, PlatformDialog, StoryModels}
import eyes4s.studio.app.keys.{CommandRegistry, Key, KeyChord, Modifier}
import eyes4s.studio.app.layout.{PaneId as StudioPaneId, StudioLayouts}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, Modifiers}
import javafx.event.Event
import javafx.scene.control.{ContextMenu, SeparatorMenuItem}
import javafx.scene.input.{ContextMenuEvent, KeyCode, KeyEvent}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The menu bar, command registry and keymap (ticket S1.9): the system menu
  * bar is generated from the registry; every chord is its command; a
  * keyboard-only user reaches every pane and every command; the tab context
  * menu carries the studio's items; docs/studio/SHORTCUTS.md is the
  * registry's table (regenerate with `EYES4S_UPDATE_GOLDENS=1`).
  */
class KeymapFxSuite extends ShellFxSuite:

  private val buildRoot: Path = Paths.get(
    String(
      getClass.getClassLoader
        .getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
        .readAllBytes,
      UTF_8
    ).trim
  )

  private def model(w: StudioWindow): AppModel = runOnFx(w.runtime.model)

  private def modifiers(chord: KeyChord): Modifiers =
    val m = chord.modifiers
    Modifiers(
      shift = m.contains(Modifier.Shift),
      control = m.contains(Modifier.Control) || (m.contains(Modifier.Command) && !isMac),
      alt = m.contains(Modifier.Option),
      meta = m.contains(Modifier.Command) && isMac
    )

  private def press(fx: FxStage, chord: KeyChord): Unit =
    fx.robot.press(ShellKeys.code(chord.key), modifiers(chord))

  // --- The menu bar ------------------------------------------------------------

  fxStage.test("the menu bar is the registry: every command once, its accelerator and state") {
    fx =>
      val w     = boot(fx, StoryModels.t2Compare)
      val menus = runOnFx(w.shell.menus)
      assertEquals(
        runOnFx(menus.map(_.getText.stripPrefix("_"))),
        Vector("File", "Edit", "View", "Go", "Run", "Window", "Help")
      )
      // A submenu (View › Appearance) lists its commands in place.
      def flat(i: javafx.scene.control.MenuItem): Vector[javafx.scene.control.MenuItem] =
        i match
          case m: javafx.scene.control.Menu => m.getItems.asScala.toVector.flatMap(flat)
          case other                        => Vector(other)
      val items = runOnFx(menus.flatMap(_.getItems.asScala).flatMap(flat))
      assertEquals(items.map(_.getId), CommandRegistry.menus.flatMap(_._2.map(_.id.value)))
      items.foreach { item =>
        val c = CommandRegistry.all.find(_.id.value == item.getId).get
        assertEquals(
          Option(item.getAccelerator),
          c.shortcut.map(ShellKeys.combination),
          item.getId
        )
        assertEquals(!item.isDisable, c.enabled(model(w)), item.getId)
        assert(!item.getText.contains("{") && item.getText.nonEmpty, item.getText)
      }
      // A menu item is its command: View › Compare, Go › Back.
      def fire(id: String): Unit =
        runOnFx(items.find(_.getId == id).get.fire())
        fx.awaitLayout()
      fire("perspective.explore")
      assertEquals(model(w).perspective, Perspective.Explore)
      fire("navigate.back")
      assertEquals(model(w).perspective, Perspective.Compare)
      // The menus follow the model: Forward is enabled now.
      val forward = runOnFx(
        w.shell.menus.flatMap(_.getItems.asScala).find(_.getId == "navigate.forward").get
      )
      assert(runOnFx(!forward.isDisable))
  }

  fxStage.test("every chord is its command, dispatched exactly once") { fx =>
    val w            = boot(fx, StoryModels.t2Compare)
    val dockCommands = Set(CommandRegistry.nextTab.id, CommandRegistry.previousTab.id)
    CommandRegistry.all.filter(_.shortcut.isDefined).foreach { c =>
      val before = model(w)
      val pure   = AppModel.update(before, Intent.Invoke(c.id))._1
      press(fx, c.shortcut.get)
      val after = model(w)
      if dockCommands(c.id) then
        assertNotEquals(after.focusedPane, before.focusedPane, c.id.value)
      else
        assertEquals(after.location, pure.location, c.id.value)
        assertEquals(after.navigation, pure.navigation, c.id.value)
        assertEquals(after.focusedPane, pure.focusedPane, c.id.value)
        assertEquals(after.isMaximized, pure.isMaximized, c.id.value)
        assertEquals(after.document, pure.document, c.id.value)
    }
  }

  fxStage.test(
    "native menu bar: the window leaves accelerated chords to the menu; one press, one step"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Explore, nativeMenu = true)
    assert(w.shell.nativeMenu)
    // Structural: the window's own keymap holds no chord an enabled menu
    // item carries.
    val m             = model(w)
    val enabledChords = CommandRegistry.all
      .filter(_.enabled(m))
      .flatMap(_.shortcut)
      .toSet
      .intersect(CommandRegistry.menuAccelerators)
    assertEquals(runOnFx(w.shell.windowKeys.keySet.intersect(enabledChords)), Set.empty)
    val start   = model(w)
    val oneBack = AppModel.update(start, Intent.Back)._1.location
    // ⌘[ once: whichever path takes it (the native menu on macOS, a scene
    // accelerator elsewhere), Back happens at most once.
    press(fx, CommandRegistry.back.shortcut.get)
    assert(Set(start.location, oneBack).contains(model(w).location), model(w).location)
    // The menu item is the path: firing it goes back exactly one step.
    dispatch(fx, w, Intent.Navigate(start.location))
    val here = model(w)
    val item = runOnFx(
      w.shell.menus.flatMap(_.getItems.asScala).find(_.getId == "navigate.back").get
    )
    runOnFx(item.fire())
    fx.awaitLayout()
    assertEquals(model(w).location, AppModel.update(here, Intent.Back)._1.location)
  }

  /** Press `chord` at the window with no menu accelerator able to act (the
    * menus are emptied), and report whether the window's handler consumed it
    * (a scene-level probe after the root never sees a consumed event) and
    * the model after.
    */
  private def pressAtWindow(
      fx: FxStage,
      w: StudioWindow,
      chord: KeyChord
  ): (Boolean, AppModel) =
    runOnFx(w.shell.menuBar.getMenus.clear())
    val code    = ShellKeys.code(chord.key)
    val reached = java.util.concurrent.atomic.AtomicBoolean(false)
    val probe: javafx.event.EventHandler[javafx.scene.input.KeyEvent] =
      e => if e.getCode == code then reached.set(true)
    runOnFx(fx.scene.addEventHandler(javafx.scene.input.KeyEvent.KEY_PRESSED, probe))
    try
      press(fx, chord)
      (!reached.get, model(w))
    finally runOnFx(fx.scene.removeEventHandler(javafx.scene.input.KeyEvent.KEY_PRESSED, probe))

  private val back = CommandRegistry.back.shortcut.get
  private val undo = CommandRegistry.undo.shortcut.get

  fxStage.test(
    "the window's key handler leaves an enabled menu item's chord alone only with a native menu"
  ) { fx =>
    val native = boot(fx, StoryModels.t2Explore, nativeMenu = true)
    val start  = model(native)
    assert(CommandRegistry.back.enabled(start))
    val (consumed, after) = pressAtWindow(fx, native, back)
    assert(!consumed, "with a native menu the window consumed ⌘[")
    assertEquals(after.location, start.location, "with a native menu the window dispatched ⌘[")
    runOnFx(native.close())
    opened.clear()
    val own              = boot(fx, StoryModels.t2Explore, nativeMenu = false)
    val (took, ownAfter) = pressAtWindow(fx, own, back)
    assert(took, "without a native menu the window did not consume ⌘[")
    assertEquals(ownAfter.location, AppModel.update(start, Intent.Back)._1.location)
  }

  private def undoItem(w: StudioWindow) =
    val combo = ShellKeys.combination(undo)
    runOnFx(w.shell.menus.flatMap(_.getItems.asScala).find(_.getAccelerator == combo))
      .getOrElse(fail("no menu item carries ⌘Z"))

  fxStage.test(
    "native menu, nothing to undo: Undo is greyed out and ⌘Z at the window says why"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Compare, nativeMenu = true)
    assert(!CommandRegistry.undo.enabled(model(w)), "the fixture has something to undo")
    // The menu renders the enabled predicate: the ⌘Z item is disabled.
    assert(runOnFx(undoItem(w).isDisable), "Undo is not greyed out with nothing to undo")
    // ⌘Z reaching the window is the window's: KeyPressed, the Unavailable
    // notice, exactly as the Linux keymap answers.
    val (consumed, after) = pressAtWindow(fx, w, undo)
    assert(consumed, "the window did not take ⌘Z for a disabled Undo")
    val linux = AppModel.update(StoryModels.t2Compare, Intent.KeyPressed(undo))._1.notice
    after.notice match
      case Some(n @ eyes4s.studio.app.Notice.Unavailable(id, _)) =>
        assertEquals(id, CommandRegistry.undo.id)
        assertEquals(Some(n), linux)
        assertEquals(n.message, "Undo: There is no edit to undo.")
      case other => fail(s"expected Unavailable for Undo, got $other")
    assertEquals(after.document, StoryModels.t2Compare.document)
  }

  fxStage.test(
    "native menu, something to undo: ⌘Z at the window is neither consumed nor dispatched"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Compare, nativeMenu = true)
    dispatch(fx, w, Intent.RequestDiscardDraft)
    dispatch(fx, w, Intent.Confirm)
    val before = model(w)
    assert(CommandRegistry.undo.enabled(before))
    assert(runOnFx(!undoItem(w).isDisable), "Undo is greyed out with an edit to undo")
    val (consumed, after) = pressAtWindow(fx, w, undo)
    assert(!consumed, "the window consumed ⌘Z that the enabled Undo item owns")
    assertEquals(after.document, before.document, "the window dispatched ⌘Z")
    assertEquals(after.notice, before.notice)
  }

  fxStage.test("⌘Z and ⌘⇧Z undo and redo a science edit") { fx =>
    val w = boot(fx, StoryModels.t2Compare)
    dispatch(fx, w, Intent.RequestDiscardDraft)
    dispatch(fx, w, Intent.Confirm)
    assertEquals(model(w).document.draft, None)
    press(fx, CommandRegistry.undo.shortcut.get)
    assert(model(w).document.draft.isDefined)
    press(fx, CommandRegistry.redo.shortcut.get)
    assertEquals(model(w).document.draft, None)
  }

  // --- Keyboard-only reach -------------------------------------------------------

  /** Visit every pane of the shown layout with F6 and ⌃⇥ alone; keyboard
    * focus lands in each pane.
    */
  private def tour(fx: FxStage, w: StudioWindow): Set[String] =
    val layout = model(w).layout
    val seen   = scala.collection.mutable.Set.empty[String]
    layout.groups.indices.foreach { _ =>
      val group = model(w).layout.groupOf(model(w).focusedPane).get
      group.panes.toVector.indices.foreach { _ =>
        val pane = model(w).focusedPane
        seen += pane.value
        val node = runOnFx(w.host.node(pane))
        assert(
          node.exists(n => runOnFx(fx.scene.getFocusOwner eq n)),
          s"focus is not in ${pane.value}"
        )
        val tabs = group.panes.toVector.map(_.id)
        val next = tabs((tabs.indexOf(pane) + 1) % tabs.size)
        press(fx, KeyChord.control(Key.Tab))
        assertEquals(model(w).focusedPane, next, s"Ctrl+Tab from ${pane.value}")
      }
      press(fx, KeyChord.plain(Key.F6))
    }
    seen.toSet

  fxStage.test("keyboard only: ⌘1–5, F6 and ⌃⇥ reach every pane of every layout") { fx =>
    val w       = boot(fx, StoryModels.t2Compare)
    val reached = scala.collection.mutable.Set.empty[String]
    Vector(Key.Digit1, Key.Digit2, Key.Digit3, Key.Digit4, Key.Digit5).foreach { k =>
      press(fx, KeyChord.command(k))
      reached ++= tour(fx, w)
    }
    // Compare's summary layout: Tab to its crumb, Space.
    press(fx, KeyChord.command(Key.Digit4))
    val summary = runOnFx(w.shell.contextStrip.crumbs.head)
    var steps   = 0
    while !runOnFx(fx.scene.getFocusOwner eq summary) && steps < 40 do
      fx.robot.press(KeyCode.TAB)
      steps += 1
    assert(runOnFx(fx.scene.getFocusOwner eq summary), "Tab never reached the Summary crumb")
    fx.robot.press(KeyCode.SPACE)
    assertEquals(runOnFx(w.host.active), Some("compare.summary"))
    reached ++= tour(fx, w)
    val declared = StudioLayouts.spec.all
      .filterNot(_ == StudioLayouts.dataFirstRun) // shown only before any dataset
      .flatMap(_.panes.map(_.id.value))
      .toSet
    assertEquals(declared -- reached, Set.empty[String])
  }

  // --- The tab context menu --------------------------------------------------------

  private def pane(id: String): StudioPaneId =
    StudioPaneId.of(id).fold(e => fail(e.message), identity)

  fxStage.test(
    "a tab's context menu: the dock's items, then Show table and Reset perspective"
  ) { fx =>
    val w      = boot(fx, StoryModels.t2Compare)
    val studio =
      runOnFx(w.shell.tabMenu(w.host.dockId(pane("compare.query-trial")), Vector.empty))
    assertEquals(
      runOnFx(studio.map(i => Option(i.getText).getOrElse("—"))),
      Vector("—", "Show table", "Reset perspective")
    )
    assert(studio.head.isInstanceOf[SeparatorMenuItem])
    // The hook is installed: a context-menu request on the tab shows it.
    val tab = runOnFx(
      w.root.lookupAll(".dock-tab").asScala.find(_.getAccessibleText == "Query trial")
    ).getOrElse(fail("no Query trial tab"))
    runOnFx {
      val b = tab.getLayoutBounds
      val p = tab.localToScene(b.getCenterX, b.getCenterY)
      val s = tab.localToScreen(b.getCenterX, b.getCenterY)
      Event.fireEvent(
        tab,
        ContextMenuEvent(
          ContextMenuEvent.CONTEXT_MENU_REQUESTED,
          p.getX,
          p.getY,
          s.getX,
          s.getY,
          false,
          null
        )
      )
    }
    fx.awaitLayout()
    val menu = runOnFx(
      javafx.stage.Window.getWindows.asScala.collectFirst {
        case c: ContextMenu if c.isShowing => c
      }
    ).getOrElse(fail("no context menu showed"))
    val items = runOnFx(menu.getItems.asScala.toVector)
    val texts = runOnFx(items.map(i => Option(i.getText).getOrElse("—")))
    assert(
      texts
        .contains("Close") && texts.takeRight(2) == Vector("Show table", "Reset perspective"),
      texts
    )
    runOnFx(items.find(_.getText == "Show table").get.fire())
    runOnFx(menu.hide())
    fx.awaitLayout()
    assertEquals(model(w).focusedPane.value, "compare.query-trial.table")
    assertEquals(
      runOnFx(w.host.dock.state.focused.map(_.value)),
      Some("compare.query-trial.table")
    )
  }

  // --- docs/studio/SHORTCUTS.md ------------------------------------------------------

  test("docs/studio/SHORTCUTS.md is the registry's shortcut table") {
    val file  = buildRoot.resolve("docs/studio/SHORTCUTS.md")
    val table = CommandRegistry.shortcutTable()
    if sys.env.contains("EYES4S_UPDATE_GOLDENS") then
      Files.writeString(file, table, UTF_8): Unit
    assert(Files.exists(file), s"missing $file; run with EYES4S_UPDATE_GOLDENS=1 to write it")
    assertNoDiff(Files.readString(file, UTF_8), table)
  }

  fxStage.test("every command has a keyboard path through the native or in-window menu bar") {
    fx =>
      List("macOS" -> true, "Windows/Linux" -> false).foreach { (platform, native) =>
        val w = boot(fx, StoryModels.t2Compare, nativeMenu = native)
        def flat(item: javafx.scene.control.MenuItem): Vector[javafx.scene.control.MenuItem] =
          item match
            case menu: javafx.scene.control.Menu => menu.getItems.asScala.toVector.flatMap(flat)
            case other                           => Vector(other)
        val items = runOnFx(w.shell.menus.flatMap(_.getItems.asScala).flatMap(flat))
        CommandRegistry.all.foreach { command =>
          assertEquals(
            items.count(_.getId == command.id.value),
            1,
            s"$platform: ${command.id.value}"
          )
        }
        if !native then
          assert(runOnFx(w.shell.menuBar.isVisible && w.shell.menuBar.isManaged))
          assert(runOnFx(w.shell.menuBar.getBoundsInParent.getHeight > 0))
          assert(runOnFx(w.shell.menus.forall(_.isMnemonicParsing)))
        runOnFx(w.close())
      }
  }

  /** Send a key into the open popup, as the OS routes it when a menu is open.
    * The harness's normal robot deliberately targets the main scene.
    */
  private def popupKey(fx: FxStage, code: KeyCode): Unit =
    runOnFx {
      val popup = javafx.stage.Window.getWindows.asScala
        .collectFirst {
          case c: ContextMenu if c.isShowing => c
        }
        .getOrElse(fail("no menu popup"))
      val target = popup.getSkin.getNode
      Event.fireEvent(
        target,
        KeyEvent(
          KeyEvent.KEY_PRESSED,
          KeyEvent.CHAR_UNDEFINED,
          code.getName,
          code,
          false,
          false,
          false,
          false
        )
      )
      Event.fireEvent(
        target,
        KeyEvent(
          KeyEvent.KEY_RELEASED,
          KeyEvent.CHAR_UNDEFINED,
          code.getName,
          code,
          false,
          false,
          false,
          false
        )
      )
    }
    fx.awaitLayout()

  fxStage.test("off macOS: F10 and arrows invoke a command with no chord, once") { fx =>
    val dialogs = Dialogs(None)
    val w       = boot(fx, StoryModels.t2Compare, dialogs = dialogs, nativeMenu = false)
    val focus   = runOnFx(fx.scene.getFocusOwner)
    fx.robot.press(KeyCode.F10)
    fx.robot.press(KeyCode.LEFT) // File wraps to Help.
    fx.robot.press(KeyCode.DOWN)
    assert(runOnFx(w.shell.menus.last.isShowing), "F10 did not enter menu navigation")
    popupKey(fx, KeyCode.ENTER)
    assertEquals(dialogs.asked.toVector, Vector(PlatformDialog.About))
    assertEquals(runOnFx(fx.scene.getFocusOwner), focus)
  }

  fxStage.test(
    "off macOS: bare Alt enters the bar; Alt+H opens Help without firing on Alt release"
  ) { fx =>
    val dialogs = Dialogs(None)
    val w       = boot(fx, StoryModels.t2Compare, dialogs = dialogs, nativeMenu = false)
    fx.robot.press(KeyCode.ALT)
    fx.robot.press(KeyCode.DOWN)
    assert(runOnFx(w.shell.menus.head.isShowing), "bare Alt did not enter menu navigation")
    popupKey(fx, KeyCode.ESCAPE)
    fx.robot.press(KeyCode.H, Modifiers(alt = true))
    assert(runOnFx(w.shell.menus.last.isShowing), "Alt+H did not open Help")
    popupKey(fx, KeyCode.DOWN)
    popupKey(fx, KeyCode.ENTER)
    assertEquals(dialogs.asked.toVector, Vector(PlatformDialog.About))
  }

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

import eyes4s.studio.app.{AppGen, Intent, StoryModels}
import org.scalacheck.Prop.forAll

/** The command registry and keymap as data (ticket S1.0). */
class CommandRegistrySuite extends munit.ScalaCheckSuite:

  test("ids are unique and parse back; shortcuts are unique") {
    val ids = CommandRegistry.all.map(_.id)
    assertEquals(ids.distinct.size, ids.size)
    ids.foreach(id => assertEquals(CommandId.parse(id.value), Some(id)))
    assertEquals(CommandId.parse("no.such.command"), None)
    val shortcuts = CommandRegistry.all.flatMap(_.shortcut)
    assertEquals(shortcuts.distinct.size, shortcuts.size)
  }

  test("the System board's keyboard model: ⌘1–5, ⌘[ ⌘], ⌘Z ⌘⇧Z, F6, ⌘⇧↩") {
    val table = CommandRegistry.all.flatMap(c => c.shortcut.map(s => c.id.value -> s.render))
    assertEquals(
      table,
      Vector(
        "perspective.data"     -> "⌘1",
        "perspective.explore"  -> "⌘2",
        "perspective.analysis" -> "⌘3",
        "perspective.compare"  -> "⌘4",
        "perspective.figures"  -> "⌘5",
        "navigate.back"        -> "⌘[",
        "navigate.forward"     -> "⌘]",
        "edit.undo"            -> "⌘Z",
        "edit.redo"            -> "⌘⇧Z",
        "pane.next"            -> "F6",
        "pane.previous"        -> "⇧F6",
        "tab.next"             -> "⌃⇥",
        "tab.previous"         -> "⌃⇧⇥",
        "pane.maximize"        -> "⌘⇧↩"
      )
    )
  }

  property("a command is enabled exactly when it resolves, never to Invoke or a key") {
    forAll(AppGen.session(10)) { traces =>
      traces.map(_.after).foreach { m =>
        CommandRegistry.all.foreach { c =>
          val resolved = c.intent(m)
          assertEquals(c.enabled(m), resolved.isDefined, c)
          resolved.foreach {
            case Intent.Invoke(_) | Intent.KeyPressed(_) =>
              fail(s"$c resolves to another command")
            case _ => ()
          }
        }
      }
    }
  }

  test("story moments: undo and redo follow the history; Show waits for a ready run") {
    val t2 = StoryModels.t2Compare
    assertEquals(CommandRegistry.undo.enabled(t2), false)
    assertEquals(CommandRegistry.reviewDraft.enabled(t2), true)
    assertEquals(CommandRegistry.showRun.enabled(StoryModels.t3Summary), false)
    assertEquals(CommandRegistry.cancelRun.enabled(StoryModels.t3Summary), true)
    assertEquals(CommandRegistry.cancelRun.enabled(t2), false)
    assertEquals(CommandRegistry.back.enabled(t2), true)
  }

  test("S1.9: every command is in exactly one menu of the bar, in bar order") {
    val listed = CommandRegistry.menus.flatMap(_._2)
    assertEquals(listed.map(_.id.value).sorted, CommandRegistry.all.map(_.id.value).sorted)
    assertEquals(listed.map(_.id).distinct.size, listed.size)
    assertEquals(
      CommandRegistry.menus.map(_._1),
      Vector(
        MenuSection.File,
        MenuSection.Edit,
        MenuSection.View,
        MenuSection.Go,
        MenuSection.Run,
        MenuSection.Window
      )
    )
    // The keyboard model's commands sit where a Mac user looks for them.
    def sectionOf(c: AppCommand) = CommandRegistry.menus.collectFirst {
      case (s, cs) if cs.contains(c) => s
    }
    assertEquals(sectionOf(CommandRegistry.compare), Some(MenuSection.View))
    assertEquals(sectionOf(CommandRegistry.back), Some(MenuSection.Go))
    assertEquals(sectionOf(CommandRegistry.undo), Some(MenuSection.Edit))
    assertEquals(sectionOf(CommandRegistry.nextPane), Some(MenuSection.Window))
    assertEquals(sectionOf(CommandRegistry.maximize), Some(MenuSection.Window))
  }

  test("S1.9: the shortcut table lists every command once, with its shortcut and id") {
    val table = CommandRegistry.shortcutTable()
    val rows  = table.linesIterator.filter(_.startsWith("| ")).drop(1).toVector
    assertEquals(rows.size, CommandRegistry.all.size)
    Vector(
      "| View | Compare | `⌘4` | `perspective.compare` |",
      "| Go | Back | `⌘[` | `navigate.back` |",
      "| Go | Forward | `⌘]` | `navigate.forward` |",
      "| Window | Next pane | `F6` | `pane.next` |",
      "| Window | Previous pane | `⇧F6` | `pane.previous` |",
      "| Window | Maximize the focused group | `⌘⇧↩` | `pane.maximize` |",
      "| Edit | Undo | `⌘Z` | `edit.undo` |",
      "| Edit | Redo | `⌘⇧Z` | `edit.redo` |",
      "| File | Rename… | — | `project.rename` |"
    ).foreach(row => assert(rows.contains(row), s"$row\n$table"))
    assert(table.endsWith("\n") && !table.contains("{"), table)
  }

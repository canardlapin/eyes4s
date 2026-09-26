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

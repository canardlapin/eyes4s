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

package eyes4s.studio.app.project

import eyes4s.studio.app.*
import eyes4s.studio.app.keys.{CommandRegistry, Key, KeyChord}
import eyes4s.studio.core.assets.InputCheck
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.Theme

class ProjectCopyModelSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("Save As keeps edited history and view context, with fresh backend ownership") {
    val original = StoryModels.t2Compare
    val theme    =
      if original.document.presentation.theme == Theme.Light then Theme.Dark else Theme.Light
    val edited = AppModel.update(original, Intent.Dispatch(Command.SetTheme(theme)))._1
    val at     = get(ClockTime.of(12, 34))
    val name   = get(ProjectName.of("saved-copy"))
    val copied = AppModel.savedCopy(edited, name, at)
    val fresh  = AppModel.open(edited.document, Some(name))
    assertEquals(copied.project, Some(name))
    assertEquals(copied.history, edited.history)
    assertEquals(copied.navigation, edited.navigation)
    assertEquals(copied.selection, edited.selection)
    assertEquals(copied.items, edited.items)
    assertEquals(copied.panes, edited.panes)
    assertEquals(copied.appearance, edited.appearance)
    assertEquals(copied.session, fresh.session)
    assertEquals(copied.jobs, fresh.jobs)
    assertEquals(copied.prepared, None)
    assertEquals(copied.inputs, InputCheck.NoProject)
    assertEquals(copied.checks, fresh.checks)
    assertEquals(copied.save.last, Some(at))
    assert(!copied.save.edited)
    assertEquals(get(copied.history.undoView).history.document, original.document)
  }

  test("Save As requests its own chooser and does not ask to discard copied edits") {
    val original = get(AppModel.newProject)
    val chord    = KeyChord.commandShift(Key.S)
    assertEquals(CommandRegistry.saveProjectAs.shortcut, Some(chord))
    val (same, effects) = AppModel.update(original, Intent.KeyPressed(chord))
    assertEquals(same, original)
    assertEquals(effects, Vector(AppEffect.OpenDialog(PlatformDialog.SaveProjectAs)))
    val (started, id) = ProjectLifecycle.empty.begin(ProjectOperation.SaveAs)
    val facts         = ProjectCloseFacts("Untitled", false, true, false)
    assertEquals(started.prepared(id.get, facts)._2, ProjectLifecycleAction.Commit)
  }

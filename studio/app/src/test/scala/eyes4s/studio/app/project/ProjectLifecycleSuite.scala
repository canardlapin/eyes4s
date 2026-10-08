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

class ProjectLifecycleSuite extends munit.FunSuite:
  private val clean = ProjectCloseFacts("project.eyes", true, false, false)
  private val dirty = clean.copy(edited = true)
  private def begun(action: ProjectOperation) =
    val (state, id) = ProjectLifecycle.empty.begin(action)
    (state, id.getOrElse(fail("no operation id")))

  test("clean close commits once; duplicate admission is ignored") {
    val (state, id) = begun(ProjectOperation.Close)
    val (ready, action) = state.prepared(id, clean)
    assertEquals(action, ProjectLifecycleAction.Commit)
    assertEquals(ready.prepared(id, clean)._2, ProjectLifecycleAction.Ignored)
    assert(!ready.finished(id).busy)
    assert(!ready.finished(id).stopped)
  }

  test("dirty or active work needs admission, and cancelling preserves the live project") {
    for facts <- Vector(dirty, clean.copy(activeWork = true), dirty.copy(activeWork = true)) do
      val (state, id) = begun(ProjectOperation.Open)
      val (pending, action) = state.prepared(id, facts)
      assertEquals(action, ProjectLifecycleAction.Ask(facts))
      val (kept, result) = pending.choose(id, ProjectCloseChoice.KeepOpen)
      assertEquals(result, ProjectLifecycleAction.KeepOpen)
      assert(!kept.busy)
      assert(!kept.stopped)
      assertEquals(kept.choose(id, ProjectCloseChoice.CloseWithoutSaving)._2, ProjectLifecycleAction.Ignored)
  }

  test("untitled close makes no Save As promise") {
    val facts = dirty.copy(title = "Untitled", named = false)
    val (state, id) = begun(ProjectOperation.Quit)
    val pending = state.prepared(id, facts)._1
    assertEquals(pending.choose(id, ProjectCloseChoice.SaveAndClose)._2, ProjectLifecycleAction.Ignored)
    val close = pending.choose(id, ProjectCloseChoice.CloseWithoutSaving)._1
    assert(close.finished(id).stopped)
  }

  test("save failure keeps the window; later edits require fresh admission") {
    val (state, id) = begun(ProjectOperation.New)
    val pending = state.prepared(id, dirty)._1
    val (saving, effect) = pending.choose(id, ProjectCloseChoice.SaveAndClose)
    assertEquals(effect, ProjectLifecycleAction.Save)
    assertEquals(saving.saved(id, false, dirty)._2, ProjectLifecycleAction.KeepOpen)
    assertEquals(saving.saved(id, true, dirty)._2, ProjectLifecycleAction.Ask(dirty))
    assertEquals(saving.saved(id, true, clean)._2, ProjectLifecycleAction.Commit)
  }

  test("late chooser/open/save answers cannot own a newer request or a closed host") {
    val (first, id) = begun(ProjectOperation.Open)
    val (next, newer) = first.cancel(id).begin(ProjectOperation.New)
    assert(newer.isDefined && newer != Some(id))
    assertEquals(next.prepared(id, clean)._2, ProjectLifecycleAction.Ignored)
    assertEquals(next.saved(id, true, clean)._2, ProjectLifecycleAction.Ignored)
    assertEquals(next.shutdown.prepared(newer.get, clean)._2, ProjectLifecycleAction.Ignored)
    assertEquals(next.shutdown.begin(ProjectOperation.Open)._2, None)
  }

  test("new work during a save needs fresh admission; already approved work does not loop") {
    import eyes4s.studio.core.backend.{JobId, RunId}
    val first = ProjectWork(RunId(7), Some(JobId(23)))
    val newer = ProjectWork(RunId(7), Some(JobId(24)))
    def saving(facts: ProjectCloseFacts) =
      val (state, id) = begun(ProjectOperation.Quit)
      (state.prepared(id, facts)._1.choose(id, ProjectCloseChoice.SaveAndClose)._1, id)
    val (idleSave, id) = saving(dirty)
    val active = clean.copy(activeWork = true, work = Set(first))
    assertEquals(idleSave.saved(id, true, active)._2, ProjectLifecycleAction.Ask(active))
    val (approved, approvedId) = saving(dirty.copy(activeWork = true, work = Set(first)))
    assertEquals(approved.saved(approvedId, true, active)._2, ProjectLifecycleAction.Commit)
    val newActive = active.copy(work = Set(newer))
    assertEquals(approved.saved(approvedId, true, newActive)._2, ProjectLifecycleAction.Ask(newActive))
    val queued = dirty.copy(activeWork = true, work = Set(ProjectWork(RunId(7), None)))
    val (queuedSave, queuedId) = saving(queued)
    assertEquals(queuedSave.saved(queuedId, true, active)._2, ProjectLifecycleAction.Commit)
  }

  test("File New Open and Close resolve to their concrete lifecycle platform requests") {
    import eyes4s.studio.app.{AppModel, AppEffect, PlatformDialog}
    import eyes4s.studio.app.keys.CommandRegistry
    import eyes4s.studio.app.vm.Menus
    val model = AppModel.newProject.fold(e => fail(e.message), identity)
    Vector(
      CommandRegistry.newProject -> PlatformDialog.NewProject,
      CommandRegistry.openProject -> PlatformDialog.OpenProject,
      CommandRegistry.closeProject -> PlatformDialog.CloseProject
    ).foreach { (command, dialog) =>
      val intent = command.intent(model).getOrElse(fail("disabled lifecycle command"))
      val (same, effects) = AppModel.update(model, intent)
      assertEquals(same, model)
      assertEquals(effects, Vector(AppEffect.OpenDialog(dialog)))
    }
    assertEquals(Menus.bar(model).find(_.title == "File").get.items.take(3).map(_.label),
      Vector("New project…", "Open project…", "Close project"))
  }

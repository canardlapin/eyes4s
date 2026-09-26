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

import eyes4s.studio.app.{AppEffect, AppModel, Intent, PlatformDialog, ProjectName, StoryModels}
import eyes4s.studio.app.StoryModels.ok
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.document.{LayoutBlob, Perspective}

/** The project chip's menu, View › Reset perspective, saved layouts and the
  * window title (tickets S1.4 and S1.5a).
  */
class MenusSuite extends munit.FunSuite:

  private val t2                          = StoryModels.t2Compare
  private val blob                        = LayoutBlob("""{"compare.query":{}}""")
  private val otherBlob                   = LayoutBlob("""{"figures":{}}""")
  private def run(m: AppModel, i: Intent) = AppModel.update(m, i)

  test("the project chip's menu: Rename…, Reveal in Finder, Project info") {
    assertEquals(
      Menus.project(t2).map(a => (a.label, a.enabled)),
      Vector("Rename…" -> true, "Reveal in Finder" -> true, "Project info" -> true)
    )
    // An untitled project has nowhere to reveal.
    assertEquals(Menus.project(StoryModels.firstRun).map(_.enabled), Vector(true, false, true))
    assertEquals(
      Menus.project(StoryModels.firstRun)(1).intent,
      Intent.Invoke(eyes4s.studio.app.keys.CommandRegistry.revealProject.id)
    )
  }

  test("Rename… asks the platform; the answer renames and marks the project edited") {
    assertEquals(
      run(t2, Intent.RequestRename)._2,
      Vector(AppEffect.OpenDialog(PlatformDialog.RenameProject))
    )
    val name            = ok(ProjectName.of("recall-study"))
    val (renamed, done) = run(t2, Intent.RenameProject(name))
    assertEquals(renamed.project, Some(name))
    assertEquals(done, Vector(AppEffect.Persist))
    assertEquals(Menus.windowTitle(renamed), "recall-study.eyes — Edited")
    assertEquals(run(t2, Intent.RenameProject(StoryModels.project)), (t2, Vector.empty))
  }

  test("Reveal and Project info are platform effects") {
    assertEquals(run(t2, Intent.RevealProject)._2, Vector(AppEffect.RevealProject))
    assertEquals(run(StoryModels.firstRun, Intent.RevealProject)._2, Vector.empty)
    assertEquals(
      run(t2, Intent.ShowProjectInfo)._2,
      Vector(AppEffect.OpenDialog(PlatformDialog.ProjectInfo))
    )
  }

  test("the window title is the project's file name, marked while edited") {
    assertEquals(Menus.windowTitle(t2), "memory-study.eyes")
    assertEquals(Menus.windowTitle(StoryModels.firstRun), "Untitled project")
  }

  test("captured layouts save only the perspectives that changed, view-only") {
    val (saved, effects) =
      run(
        t2,
        Intent.LayoutsCaptured(
          Vector(Perspective.Compare -> Some(blob), Perspective.Data -> None)
        )
      )
    assertEquals(AppModel.savedLayout(saved, Perspective.Compare), Some(blob))
    assertEquals(AppModel.savedLayout(saved, Perspective.Data), None)
    assertEquals(saved.document.science, t2.document.science)
    assertEquals(
      effects.collect { case AppEffect.Journal(e) => e },
      Vector(JournalEntry.Apply(Command.SaveLayout(Perspective.Compare, Some(blob))))
    )
    // The same capture again changes nothing and is not refused.
    val again = run(saved, Intent.LayoutsCaptured(Vector(Perspective.Compare -> Some(blob))))
    assertEquals(again, (saved, Vector.empty))
    assertEquals(again._1.notice, None)
    val both = run(
      saved,
      Intent.LayoutsCaptured(
        Vector(Perspective.Compare -> None, Perspective.Figures -> Some(otherBlob))
      )
    )._1
    assertEquals(
      both.document.presentation.layouts.map(_.perspective),
      Vector(Perspective.Figures)
    )
  }

  test("View › Reset perspective clears the saved layout and resets the dock") {
    assertEquals(Menus.view(t2).title, "View")
    assertEquals(Menus.view(t2).items.map(_.label), Vector("Reset perspective"))
    assertEquals(Menus.view(t2).items.map(_.intent), Vector(Intent.ResetPerspective))
    // Nothing saved: only the dock resets, and nothing is refused.
    assertEquals(
      run(t2, Intent.ResetPerspective),
      (t2, Vector(AppEffect.ResetLayouts(Perspective.Compare)))
    )
    val saved = run(t2, Intent.LayoutsCaptured(Vector(Perspective.Compare -> Some(blob))))._1
    val (reset, effects) = run(saved, Intent.ResetPerspective)
    assertEquals(AppModel.savedLayout(reset, Perspective.Compare), None)
    assertEquals(effects.last, AppEffect.ResetLayouts(Perspective.Compare))
    assertEquals(reset.notice, None)
    // Focus and maximize return to the default arrangement's too.
    val focused = run(
      run(saved, Intent.FocusPane(ok(eyes4s.studio.app.layout.PaneId.of("compare.pairs"))))._1,
      Intent.ToggleMaximize
    )._1
    assert(focused.isMaximized)
    val restored = run(focused, Intent.ResetPerspective)._1
    assertEquals(restored.focusedPane, t2.focusedPane)
    assert(!restored.isMaximized)
  }

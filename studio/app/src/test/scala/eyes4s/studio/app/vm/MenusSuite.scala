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

  test("a refused rename is a notice naming the error; nothing changes") {
    val (m, effects) =
      run(t2, Intent.RenameRefused(eyes4s.studio.app.AppError.BlankProjectName))
    assertEquals(effects, Vector.empty)
    assertEquals(m.project, t2.project)
    assertEquals(Shell.project(m).notice.map(_.text), Some("A project name is blank."))
  }

  test("unreadable saved layouts are a notice; the document is untouched") {
    val (m, effects) =
      run(t2, Intent.LayoutsUnreadable(Vector(Perspective.Compare), "not JSON"))
    assertEquals(effects, Vector.empty)
    assertEquals(m.document, t2.document)
    assertEquals(
      Shell.project(m).notice.map(_.text),
      Some(
        "The saved layout of Compare could not be read (not JSON); it shows the default layout."
      )
    )
  }

  test("the dock's own maximize is reported and idempotent; F6 and ⇧F6 cycle groups") {
    val contrast   = ok(eyes4s.studio.app.layout.PaneId.of("compare.contrast"))
    val (on, none) = run(t2, Intent.SetMaximized(Some(contrast)))
    assert(on.isMaximized)
    assertEquals(on.focusedPane, contrast)
    assertEquals(none, Vector.empty)
    assertEquals(run(on, Intent.SetMaximized(Some(contrast)))._1, on)
    assert(!run(on, Intent.SetMaximized(None))._1.isMaximized)
    // A pane the layout does not show changes nothing.
    val elsewhere = ok(eyes4s.studio.app.layout.PaneId.of("figures.page"))
    assertEquals(run(t2, Intent.SetMaximized(Some(elsewhere)))._1, t2)
    val back = run(on, Intent.FocusPane(t2.focusedPane))._1
    val next = run(back, Intent.FocusNextPane)._1
    assert(!next.isMaximized)
    assertNotEquals(next.focusedPane, t2.focusedPane)
    assertEquals(run(next, Intent.FocusPreviousPane)._1.focusedPane, t2.focusedPane)
    val chord = eyes4s.studio.app.keys.KeyChord.shift(eyes4s.studio.app.keys.Key.F6)
    assertEquals(run(next, Intent.KeyPressed(chord))._1.focusedPane, t2.focusedPane)
  }

  test("S1.9: the menu bar is the registry: every command, its shortcut and enablement") {
    import eyes4s.studio.app.keys.CommandRegistry
    for m <- Vector(t2, StoryModels.t3Summary, StoryModels.firstRun) do
      val bar   = Menus.bar(m)
      val items = bar.flatMap(_.items)
      assertEquals(bar.map(_.title), Vector("File", "Edit", "View", "Go", "Run", "Window"))
      assertEquals(items.map(_.command), CommandRegistry.menus.flatMap(_._2.map(_.id)))
      items.foreach { i =>
        val c = CommandRegistry.find(i.command).get
        assertEquals(i.shortcut, c.shortcut, i.label)
        assertEquals(i.enabled, c.enabled(m), i.label)
        assertEquals(i.intent, c.intent(m).getOrElse(Intent.Invoke(c.id)), i.label)
        assert(!i.label.contains("{"), i.label)
      }
    val go = Menus.bar(t2).find(_.title == "Go").get
    assertEquals(
      go.items.map(i => (i.label, i.enabled)),
      Vector("Back" -> true, "Forward" -> false)
    )
    // A disabled item explains itself when invoked anyway.
    val forward = go.items(1)
    val refused = run(t2, forward.intent)._1
    assertEquals(
      Shell.project(refused).notice.map(_.text),
      Some("Forward is not available now.")
    )
  }

  test("S1.9: a tab's context menu offers the plot's table twin and Reset perspective") {
    def pane(id: String) = ok(eyes4s.studio.app.layout.PaneId.of(id))
    val plot             = Menus.tab(t2, pane("compare.query-trial"))
    assertEquals(plot.map(_.label), Vector("Show table", "Reset perspective"))
    assertEquals(plot.head.intent, Intent.FocusPane(pane("compare.query-trial.table")))
    val table = Menus.tab(t2, pane("compare.query-trial.table"))
    assertEquals(table.map(_.label), Vector("Show plot", "Reset perspective"))
    assertEquals(table.head.intent, Intent.FocusPane(pane("compare.query-trial")))
    assertEquals(
      Menus.tab(t2, pane("compare.inspector")).map(_.label),
      Vector("Reset perspective")
    )
    // A pane of another layout gets nothing.
    assertEquals(Menus.tab(t2, pane("figures.page")), Vector.empty)
    // Show table focuses the twin.
    assertEquals(run(t2, plot.head.intent)._1.focusedPane, pane("compare.query-trial.table"))
  }

  test("Discard draft asks in words: the draft and its changes; Keep draft dismisses") {
    val asking = run(t2, Intent.RequestDiscardDraft)._1
    val c      = Shell.confirmation(asking).get
    assertEquals(c.text, "Discard draft rev 5 and its 1 change? Runs are not affected.")
    assertEquals((c.confirm.label, c.confirm.intent), ("Discard draft", Intent.Confirm))
    assertEquals((c.cancel.label, c.cancel.intent), ("Keep draft", Intent.Dismiss))
    assertEquals(Shell.confirmation(run(asking, c.cancel.intent)._1), None)
    val discarded = run(asking, c.confirm.intent)._1
    assertEquals(discarded.document.draft, None)
    assertEquals(Shell.confirmation(discarded), None)
    assertEquals(Shell.confirmation(t2), None)
  }

  test("⌃⇥ and ⌃⇧⇥ are the dock's tab cycling, as effects") {
    import eyes4s.studio.app.keys.{Key, KeyChord}
    import eyes4s.studio.app.DockCommand
    assertEquals(
      run(t2, Intent.KeyPressed(KeyChord.control(Key.Tab))),
      (t2, Vector(AppEffect.Dock(DockCommand.NextTab)))
    )
    assertEquals(
      run(t2, Intent.KeyPressed(KeyChord.controlShift(Key.Tab))),
      (t2, Vector(AppEffect.Dock(DockCommand.PreviousTab)))
    )
  }

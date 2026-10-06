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

import eyes4s.studio.app.{AppModel, Intent, Notice, PlatformDialog, StoryModels}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.LayoutBlob
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.app.layout.{PaneId as StudioPaneId, StudioLayouts}
import eyes4s.studio.app.vm.Menus
import eyes4s.studio.core.backend.{DiagnosticLevel, DiagnosticOrigin, StudioDiagnostic}
import eyes4s.studio.core.document.{Perspective, PresentationState}
import eyes4s.studio.core.execution.JobPhase
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{Modifiers, StudioTheme}
import eyes4s.studio.desktop.runtime.EffectProblem
import io.circe.syntax.*
import javafx.scene.control.{Button, Label, MenuItem}
import javafx.scene.input.KeyCode
import javafx.scene.layout.Region

import scala.jdk.CollectionConverters.*

/** The first real window (tickets S1.4 and S1.5a), booted on the fake
  * backend: the app bar, context strip, draft banner and status bar render
  * the S1.0 view-models; the perspective host shows one retained dock.
  *
  * Snapshots go to `target/studio-snapshots/AppShellFxSuite/<test>/` for a
  * human to compare with docs/studio/design/.
  */
class AppShellFxSuite extends ShellFxSuite:

  // --- Story moment t2: every perspective, drawn from its board's model --------

  /** Board, model, layout, and whether a dark snapshot is taken too. */
  private val boards: Vector[(String, () => AppModel, String, Boolean)] = Vector(
    (
      "Data",
      () => at(StoryModels.t2Compare, Intent.SwitchPerspective(Perspective.Data)),
      "data.verify",
      true
    ),
    ("Explore", () => StoryModels.t2Explore, "explore", false),
    ("Analysis", () => StoryModels.t2Analysis, "analysis", false),
    (
      "Compare summary",
      () => at(StoryModels.t2Compare, Intent.OpenCrumb(0)),
      "compare.summary",
      false
    ),
    ("Compare query", () => StoryModels.t2Compare, "compare.query", true),
    ("Figures", () => StoryModels.t2Figures, "figures", false)
  )

  boards.foreach { (board, model, layout, dark) =>
    fxStage.test(s"t2 · $board: the shell renders the S1.0 view-models; snapshot") { fx =>
      assumeFullStage(fx)
      val w = boot(fx, model())
      assertEquals(runOnFx(w.host.active), Some(layout))
      assertNoDiff(texts(w).mkString("\n"), expected(w).mkString("\n"))
      assertEquals(runOnFx(fx.stage.getTitle), runOnFx(Menus.windowTitle(w.runtime.model)))
      val files = fx.snapshot(StudioTheme.Light)
      assertEquals(files.map(_.getFileName.toString), List("light-1x.png", "light-2x.png"))
      if dark then
        val sheets =
          StudioStyles.stylesheets(Theme.Dark).fold(e => fail(e.message), identity)
        runOnFx(w.root.getStylesheets.setAll(sheets*))
        val darkFiles = fx.snapshot(StudioTheme.Dark)
        assertEquals(
          darkFiles.map(_.getFileName.toString),
          List("dark-1x.png", "dark-2x.png")
        )
        // The texts do not depend on the theme.
        assertNoDiff(texts(w).mkString("\n"), expected(w).mkString("\n"))
    }
  }

  fxStage.test("t2 · Main board: app bar, context strip and status bar read as the board") {
    fx =>
      assumeFullStage(fx)
      val w = boot(fx, StoryModels.t2Compare)
      assertEquals(runOnFx(fx.stage.getTitle), "memory-study.eyes")
      val lines = texts(w)
      val board = Vector(
        "app.name: Eyes Studio",
        "app.project: memory-study",
        "app.perspectives: Data ⌘1 | Explore ⌘2 | Analysis ⌘3 | Compare ⌘4 (current) | Figures ⌘5",
        "app.jobs: No jobs",
        "context.nav: Back (⌘[) | [Forward (⌘])]",
        "context.trail: Summary · by retrieval response › Remembered › P17 › ret_07 · beach-042 › *pair ret_07 × enc_03 · matched*",
        "context.freshness: Analysis rev 4 · run 7 · data r3 · current",
        "context.draft: Draft rev 5 · 1 change · ready",
        "banner.lead: Showing run 7 (analysis rev 4).",
        "banner.detail: Draft rev 5 adds σ 8° and has not been run.",
        "banner.actions: Review in Analysis | Discard draft",
        "status: Selected: P17 › ret_07 × enc_03 (matched) · σ 2° | Prev / Next steps controls · F6 next pane | No jobs | Saved 10:24"
      )
      assertNoDiff(lines.mkString("\n"), board.mkString("\n"))
      val wordmark = runOnFx(w.shell.appBar.wordmark.getFont)
      assertEquals(wordmark.getSize, 13.0)
      assertEquals(wordmark.getFamily, "IBM Plex Sans SmBld")
  }

  // --- Metrics (PARITY_CHECKLIST "Shared shell", ±1 px from node bounds) -------

  fxStage.test(
    "shell metrics: app bar 44, context strip 32, banner 30, tab headers 28, status bar 24"
  ) { fx =>
    assumeFullStage(fx)
    val w                 = boot(fx, StoryModels.t2Compare)
    def height(r: Region) = runOnFx(r.getLayoutBounds.getHeight)
    assertEqualsDouble(height(w.shell.appBar.node), 44, 1)
    assertEqualsDouble(height(w.shell.contextStrip.node), 32, 1)
    assertEqualsDouble(height(w.shell.banner.node), 30, 1)
    assertEqualsDouble(height(w.shell.statusBar.node), 24, 1)
    val headers = runOnFx(
      w.root
        .lookupAll(".dock-header")
        .asScala
        .toVector
        .filter(_.isVisible)
        .map(_.getLayoutBounds.getHeight)
    )
    assert(headers.nonEmpty)
    headers.foreach(h => assertEqualsDouble(h, 28, 1))
    // The rows fill the 900 px window: nothing else takes height.
    val dock = height(w.shell.dockArea)
    assertEqualsDouble(44 + 32 + 30 + dock + 24, 900, 1)
    // The switcher pill: 32 px, radius 7, centred in the bar, clear of its
    // bottom hairline (Main.dc.html).
    val pill                  = w.shell.appBar.perspectives
    val (top, bottom, radius) = runOnFx {
      val b = w.shell.appBar.node.sceneToLocal(pill.localToScene(pill.getLayoutBounds))
      val r = pill.getBackground.getFills.get(0).getRadii.getTopLeftHorizontalRadius
      (b.getMinY, b.getMaxY, r)
    }
    assertEqualsDouble(bottom - top, 32, 1)
    assertEqualsDouble(top, (44 - 32) / 2.0, 1)
    assert(bottom <= 43, s"the pill reaches $bottom, over the hairline at 43")
    assertEqualsDouble(radius, 7, 0)
  }

  // --- Perspectives: ⌘1–5, retained panes, Compare by trail depth ------------

  fxStage.test("⌘1–⌘5 switch perspectives; the toggle group follows the model") { fx =>
    val w    = boot(fx, StoryModels.t2Compare)
    val keys = Vector(
      KeyCode.DIGIT1 -> (Perspective.Data, "data.verify"),
      KeyCode.DIGIT2 -> (Perspective.Explore, "explore"),
      KeyCode.DIGIT3 -> (Perspective.Analysis, "analysis"),
      KeyCode.DIGIT4 -> (Perspective.Compare, "compare.query"),
      KeyCode.DIGIT5 -> (Perspective.Figures, "figures")
    )
    keys.foreach { case (key, (p, layout)) =>
      fx.robot.press(key, shortcut)
      assertEquals(runOnFx(w.runtime.model.perspective), p)
      assertEquals(runOnFx(w.host.active), Some(layout))
      val selected =
        runOnFx(Perspective.values.toVector.filter(w.shell.appBar.switcher(_).isSelected))
      assertEquals(selected, Vector(p))
    }
    // A click on the switcher is the same intent.
    fx.robot.click(runOnFx(w.shell.appBar.switcher(Perspective.Analysis)))
    assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Analysis)
    // Clicking the selected toggle keeps it selected.
    fx.robot.click(runOnFx(w.shell.appBar.switcher(Perspective.Analysis)))
    assert(runOnFx(w.shell.appBar.switcher(Perspective.Analysis).isSelected))
  }

  fxStage.test(
    "switching perspectives retains every pane: one view per pane id, none disposed"
  ) { fx =>
    val w                = boot(fx, StoryModels.t2Compare)
    val panes            = w.host.panes
    def node(id: String) =
      runOnFx(w.host.node(StudioPaneId.of(id).fold(e => fail(e.message), identity)))
    // Every perspective, twice, and Compare's two layouts.
    val tour = Vector(1, 2, 3, 4, 5, 1, 2, 3, 4, 5).map(d => KeyCode.valueOf(s"DIGIT$d"))
    tour.take(5).foreach(fx.robot.press(_, shortcut))
    val first = runOnFx(panes.creations.map(id => id -> panes.node(id)).toMap)
    tour.drop(5).foreach(fx.robot.press(_, shortcut))
    fx.robot.press(KeyCode.DIGIT4, shortcut)
    dispatch(fx, w, Intent.OpenCrumb(0))
    assertEquals(runOnFx(w.host.active), Some("compare.summary"))
    dispatch(fx, w, Intent.Back)
    assertEquals(runOnFx(w.host.active), Some("compare.query"))
    val built = runOnFx(panes.creations)
    assertEquals(built.distinct, built, "a pane was built twice")
    assertEquals(runOnFx(panes.disposals), Vector.empty)
    val declared = StudioLayouts.spec.all
      .filterNot(_ == StudioLayouts.dataFirstRun)
      .flatMap(_.panes.map(_.id.value))
      .distinct
    assertEquals(built.map(_.value).sorted, declared.sorted)
    first.foreach { (id, n) =>
      assert(runOnFx(panes.node(id)).exists(m => n.exists(_ eq m)), s"$id changed identity")
    }
    assert(node("compare.scale-profile").isDefined)
  }

  fxStage.test(
    "Compare shows the summary layout at the Summary crumb and the query layout below it"
  ) { fx =>
    val w       = boot(fx, StoryModels.t2Compare)
    val profile = StudioPaneId.of("compare.scale-profile").fold(e => fail(e.message), identity)
    val shared  = runOnFx(w.host.node(profile))
    assertEquals(runOnFx(w.host.active), Some("compare.query"))
    val crumbs = runOnFx(w.shell.contextStrip.crumbs)
    fx.robot.click(crumbs.head)
    assertEquals(runOnFx(w.host.active), Some("compare.summary"))
    assert(runOnFx(w.host.dock.state.findPane(w.host.dockId(profile))).isDefined)
    assert(runOnFx(w.host.node(profile)).exists(n => shared.exists(_ eq n)))
    // Deeper than the Summary crumb: the query layout again.
    dispatch(
      fx,
      w,
      Intent.Navigate(
        eyes4s.studio.app.nav.Location(Perspective.Compare, StoryModels.queryTrail.take(3))
      )
    )
    assertEquals(runOnFx(w.host.active), Some("compare.query"))
  }

  // --- The jobs chip on the fake backend -------------------------------------

  private val lost = StudioDiagnostic(
    "studio-execution.lost-job",
    DiagnosticLevel.Error,
    DiagnosticOrigin.Host,
    Vector.empty,
    "lost"
  )

  fxStage.test("jobs chip idle at t2: 'No jobs', nothing to open") { fx =>
    val w    = boot(fx, StoryModels.t2Compare)
    val jobs = w.shell.appBar.jobs
    assertEquals(runOnFx(jobs.drawnParts), Vector("No jobs"))
    assert(runOnFx(!jobs.progress.isVisible && !jobs.cancel.isVisible))
    assertEquals(runOnFx(jobs.chip.getAccessibleText), "Jobs: none running")
  }

  fxStage.test(
    "jobs chip running at t3: run 8 adopted from the fake backend; Cancel reaches it"
  ) { fx =>
    assumeFullStage(fx)
    val w    = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    val jobs = w.shell.appBar.jobs
    assertEquals(
      runOnFx(jobs.drawnParts.mkString(" · ")),
      "Run 8 · Comparing · 21,400 / 44,845 pairs"
    )
    assert(runOnFx(jobs.progress.isVisible && jobs.cancel.isVisible))
    assertEqualsDouble(runOnFx(jobs.progress.getProgress), 21400.0 / 44845.0, 1e-9)
    assertEquals(runOnFx(drawn(jobs.cancel)), "Cancel")
    fx.snapshot(StudioTheme.Light)
    fx.robot.click(runOnFx(jobs.cancel))
    eventually(fx, "the cancelled run")(!jobs.text.startsWith("Run 8 · Comparing"))
    // The cancel reached the execution service; only the not-yet-wired
    // journal (S2.4b) is recorded.
    assertEquals(
      runOnFx(w.effects.problems.filterNot(_.isInstanceOf[EffectProblem.NotWired])),
      Vector.empty
    )
    val phases = w.session.jobs.map(_.phase)
    assert(
      phases.forall {
        case JobPhase.Cancelling(_) | JobPhase.Cancelled(_) => true
        case _                                              => false
      } && phases.nonEmpty,
      phases
    )
  }

  fxStage.test("jobs chip failed: 'Run 8 failed · 2 diagnostics' opens the Diagnostics pane") {
    fx =>
      val w    = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      val jobs = w.shell.appBar.jobs
      w.session.await(w.session.fixture.get.fail(StoryMoments.run8Job, Vector(lost, lost)))
      eventually(fx, "the failed chip")(jobs.text == "Run 8 failed · 2 diagnostics")
      assertEquals(runOnFx(jobs.drawnParts.mkString(" · ")), "Run 8 failed · 2 diagnostics")
      assert(runOnFx(!jobs.progress.isVisible && !jobs.cancel.isVisible))
      fx.snapshot(StudioTheme.Light)

      fx.robot.click(runOnFx(jobs.chip))
      assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Analysis)
      assertEquals(runOnFx(w.host.active), Some("analysis"))
      val diagnostics = w.host.dockId(StudioLayouts.diagnostics)
      assertEquals(runOnFx(w.host.dock.state.focused), Some(diagnostics))
      val pane = runOnFx(w.host.node(StudioLayouts.diagnostics))
      assert(runOnFx(pane.exists(n => n.getScene eq fx.scene) && pane.exists(_.isVisible)))
      assertEquals(
        runOnFx(pane.flatMap(_.lookup(".pane-placeholder-title") match
          case l: Label => Some(l.getText)
          case _        => None)),
        Some("Diagnostics")
      )
  }

  // --- Layouts in the presentation state; View › Reset perspective ------------

  fxStage.test("layouts save into the presentation state, reload, and reset") { fx =>
    val a     = boot(fx, StoryModels.t2Compare)
    val pairs =
      a.host.dockId(StudioPaneId.of("compare.pairs").fold(e => fail(e.message), identity))
    def activeOf(w: StudioWindow) = runOnFx(w.host.dock.state.groupOf(pairs).map(_.active))
    val contrast                  = Some(scaladock.PaneId("compare.contrast"))
    assertEquals(activeOf(a), contrast)
    runOnFx(a.host.dock.focus(pairs))
    fx.awaitLayout()
    assertEquals(activeOf(a), Some(pairs))
    runOnFx(a.captureLayouts())
    val presentation = runOnFx(a.runtime.model.document.presentation)
    assertEquals(presentation.layouts.map(_.perspective), Vector(Perspective.Compare))
    // The presentation state round-trips as the S2.3 bundle writes it.
    val json = presentation.asJson
    assertEquals(json.as[PresentationState], Right(presentation))

    // A second window on the saved document shows the saved arrangement.
    val b = boot(fx, runOnFx(a.runtime.model))
    assertEquals(runOnFx(b.host.active), Some("compare.query"))
    assertEquals(activeOf(b), Some(pairs))

    // View › Reset perspective: the default arrangement, nothing saved.
    val reset: MenuItem = runOnFx(
      b.shell.menus
        .find(_.getText == "View")
        .flatMap(_.getItems.asScala.find(_.getId == "view.reset-perspective"))
        .getOrElse(fail("View has no Reset perspective"))
    )
    assertEquals(runOnFx(reset.getText), "Reset perspective")
    runOnFx(reset.fire())
    fx.awaitLayout()
    assertEquals(activeOf(b), contrast)
    assertEquals(runOnFx(b.runtime.model.document.presentation.layouts), Vector.empty)
  }

  // --- The project chip and the window title ---------------------------------

  fxStage.test("project chip menu: Rename… renames and marks the window edited") { fx =>
    val dialogs = Dialogs(Some("recall-study"))
    val w       = boot(fx, StoryModels.t2Compare, dialogs = dialogs)
    val items   = runOnFx(w.shell.appBar.project.getItems.asScala.toVector)
    assertEquals(
      runOnFx(items.map(_.getText)),
      Vector("Rename…", "Reveal in Finder", "Project info")
    )
    runOnFx(items.head.fire())
    fx.awaitLayout()
    assertEquals(dialogs.asked.toVector, Vector(PlatformDialog.RenameProject))
    assertEquals(runOnFx(w.shell.appBar.project.getText), "recall-study")
    assertEquals(runOnFx(fx.stage.getTitle), "recall-study.eyes — Edited")
    runOnFx(items(2).fire())
    assertEquals(dialogs.asked.last, PlatformDialog.ProjectInfo)
  }

  fxStage.test("Rename… with a blank name shows the refusal and keeps the name") { fx =>
    val w = boot(fx, StoryModels.t2Compare, dialogs = Dialogs(Some("   ")))
    runOnFx(w.shell.appBar.project.getItems.get(0).fire())
    fx.awaitLayout()
    assertEquals(runOnFx(w.shell.appBar.project.getText), "memory-study")
    assert(runOnFx(w.shell.notice.node.isVisible))
    assertEquals(runOnFx(drawn(w.shell.notice.text)), "A project name is blank.")
    assertEquals(runOnFx(fx.stage.getTitle), "memory-study.eyes")
    // Dismiss puts it away.
    val dismiss = runOnFx(w.shell.notice.node.getChildren.asScala.collectFirst {
      case b: Button => b
    }.get)
    fx.robot.click(dismiss)
    assert(runOnFx(!w.shell.notice.node.isVisible))
    assertEquals(runOnFx(w.runtime.model.notice), None)
  }

  // --- Maximize and keys the dock would otherwise own ------------------------

  /** The maximize button in the header of the group showing `pane`. */
  private def maximizeButton(w: StudioWindow, pane: String): javafx.scene.Node = runOnFx {
    val node  = w.host.node(StudioPaneId.of(pane).fold(e => fail(e.message), identity)).get
    val group = Iterator
      .iterate(node)(_.getParent)
      .takeWhile(_ != null)
      .find(_.getStyleClass.contains("dock-group"))
      .getOrElse(fail(s"$pane is in no group"))
    group.lookup(".dock-header-button.maximize")
  }

  fxStage.test("the header's maximize reaches the model; ⌘⇧↩ then restores both") { fx =>
    val w      = boot(fx, StoryModels.t2Compare)
    val model  = () => runOnFx(w.runtime.model)
    val docked = () => runOnFx(w.host.dock.state.maximized)
    // Another group than the focused one (a wide one: a narrow header folds
    // its maximize into the tab menu): the model follows focus too.
    fx.robot.click(maximizeButton(w, "compare.contrast"))
    assert(model().isMaximized)
    assertEquals(model().focusedPane.value, "compare.contrast")
    assert(docked().isDefined)
    fx.robot.press(KeyCode.ENTER, shortcut.copy(shift = true))
    assert(!model().isMaximized)
    assertEquals(docked(), None)
    // And the other way round: the key maximizes, the header restores.
    fx.robot.press(KeyCode.ENTER, shortcut.copy(shift = true))
    assert(model().isMaximized && docked().isDefined)
    fx.robot.click(maximizeButton(w, "compare.contrast"))
    assert(!model().isMaximized)
    assertEquals(docked(), None)
  }

  fxStage.test("F6 and ⇧F6 cycle groups; ⌃⇥ and ⌃⇧⇥ cycle tabs through the dock") { fx =>
    val w       = boot(fx, StoryModels.t2Compare)
    def focused = runOnFx(w.runtime.model.focusedPane.value)
    def docked  = runOnFx(w.host.dock.state.focused.map(_.value))
    assertEquals(focused, "compare.query-trial")
    fx.robot.press(KeyCode.F6)
    assertEquals(focused, "compare.reference-trial")
    fx.robot.press(KeyCode.F6, Modifiers(shift = true))
    assertEquals(focused, "compare.query-trial")
    fx.robot.press(KeyCode.TAB, Modifiers(control = true))
    fx.awaitLayout()
    assertEquals(focused, "compare.query-trial.table")
    assertEquals(docked, Some("compare.query-trial.table"))
    fx.robot.press(KeyCode.TAB, Modifiers(control = true, shift = true))
    fx.awaitLayout()
    assertEquals(focused, "compare.query-trial")
    assertEquals(docked, Some("compare.query-trial"))
  }

  // --- Saved layouts: no spurious saves; unreadable ones never block ----------

  fxStage.test("touring every perspective, focusing and maximizing saves no layout") { fx =>
    val w = boot(fx, StoryModels.t2Compare)
    Vector(1, 2, 3, 4, 5).foreach { d =>
      fx.robot.press(KeyCode.valueOf(s"DIGIT$d"), shortcut)
      fx.robot.press(KeyCode.F6)
    }
    fx.robot.press(KeyCode.DIGIT4, shortcut)
    dispatch(fx, w, Intent.OpenCrumb(0))
    fx.robot.press(KeyCode.ENTER, shortcut.copy(shift = true))
    assert(runOnFx(w.host.dock.state.maximized).isDefined)
    val captured = runOnFx(w.host.capture())
    assertEquals(captured.map(_._1), Perspective.values.toVector)
    assertEquals(captured.collect { case (p, Some(_)) => p }, Vector.empty)
    runOnFx(w.captureLayouts())
    assertEquals(runOnFx(w.runtime.model.document.presentation.layouts), Vector.empty)
  }

  private val corrupt: Vector[(String, String)] = Vector(
    "{not json"                   -> "not JSON",
    "{\"compare.query\": 5}"      -> "not a layout",
    "{\"compare.elsewhere\": {}}" -> "an unknown layout"
  )

  corrupt.foreach { (text, what) =>
    fxStage.test(s"a saved layout that is $what: defaults, a notice, and the window opens") {
      fx =>
        val broken = at(
          StoryModels.t2Compare,
          Intent.Dispatch(Command.SaveLayout(Perspective.Compare, Some(LayoutBlob(text))))
        )
        assertEquals(AppModel.savedLayout(broken, Perspective.Compare), Some(LayoutBlob(text)))
        val w = boot(fx, broken)
        assertEquals(runOnFx(w.host.active), Some("compare.query"))
        val pairs =
          w.host.dockId(StudioPaneId.of("compare.pairs").fold(e => fail(e.message), identity))
        assertEquals(
          runOnFx(w.host.dock.state.groupOf(pairs).map(_.active.value)),
          Some("compare.contrast")
        )
        assert(runOnFx(w.shell.notice.node.isVisible))
        val notice = runOnFx(drawn(w.shell.notice.text))
        assert(notice.startsWith("The saved layout of Compare could not be read ("), notice)
        assert(notice.endsWith("); it shows the default layout."), notice)
        runOnFx(w.runtime.model.notice) match
          case Some(Notice.LayoutsReset(ps, _)) => assertEquals(ps, Vector(Perspective.Compare))
          case other                            => fail(s"expected LayoutsReset, got $other")
        // The next capture replaces the unreadable layout with the default.
        runOnFx(w.captureLayouts())
        assertEquals(runOnFx(w.runtime.model.document.presentation.layouts), Vector.empty)
    }
  }

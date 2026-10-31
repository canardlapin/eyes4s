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
  * human to compare with docs/studio/design/, each light one with its
  * board's resting focus ring ([[RestingFocus]]).
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
      // The board's resting focus ring, by keyboard (REVIEW ticket 28).
      RestingFocus.appShell(board).foreach(RestingFocus.show(fx, w, _))
      val files = fx.snapshot(StudioTheme.Light)
      assertEquals(files.map(_.getFileName.toString), List("light-1x.png", "light-2x.png"))
      if dark then
        RestingFocus.clear(fx, w)
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

  fxStage.test("t2 · Main board: toolbar, trail and status bar read as the v4 golden") { fx =>
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t2Compare)
    assertEquals(runOnFx(fx.stage.getTitle), "memory-study.eyes")
    val lines = texts(w)
    val board = Vector(
      "app.project: memory-study",
      "app.perspectives: Data | Analysis | Results (current) | Report",
      "app.state: Run 7 · current | Data r3 | Analysis rev 5 · not run",
      "context.nav: Back (⌘[) | [Forward (⌘])]",
      "context.trail: Summary · by retrieval response / Remembered / P17 / ret_07 · beach-042 / *pair ret_07 × enc_03 · matched*",
      "status: Prev / Next steps controls · F6 next pane | Saved 10:24"
    )
    assertNoDiff(lines.mkString("\n"), board.mkString("\n"))
    val project = runOnFx(w.shell.appBar.project.lookup(".label").asInstanceOf[Label].getFont)
    assertEquals(project.getSize, 13.0)
    assertEquals(project.getFamily, "IBM Plex Sans Medm")
  }

  // --- Metrics (PARITY_CHECKLIST "Shared shell", ±1 px from node bounds) -------

  fxStage.test(
    "shell metrics (v4): toolbar 48, trail 34, tab headers 28, status bar 26"
  ) { fx =>
    assumeFullStage(fx)
    val w                 = boot(fx, StoryModels.t2Compare)
    def height(r: Region) = runOnFx(r.getLayoutBounds.getHeight)
    assertEqualsDouble(height(w.shell.appBar.node), 48, 1)
    assertEqualsDouble(height(w.shell.contextStrip.node), 34, 1)
    assertEqualsDouble(height(w.shell.statusBar.node), 26, 1)
    val headers = runOnFx(
      w.root
        .lookupAll(".pane-header")
        .asScala
        .toVector
        .filter(_.isVisible)
        .map(_.getLayoutBounds.getHeight)
    )
    assert(headers.nonEmpty)
    headers.foreach(h => assertEqualsDouble(h, 36, 1))
    // The menu bar and shell rows fill the 900 px window.
    val dock = height(w.shell.dockArea)
    assertEqualsDouble(height(w.shell.menuBar) + 48 + 34 + dock + 26, 900, 1)
    // The switcher pill: 32 px, radius 8, centred in the toolbar and on the
    // window, clear of its bottom hairline (the v4 golden).
    val pill                          = w.shell.appBar.perspectives
    val (top, bottom, radius, centre) = runOnFx {
      val b = w.shell.appBar.node.sceneToLocal(pill.localToScene(pill.getLayoutBounds))
      val r = pill.getBackground.getFills.get(0).getRadii.getTopLeftHorizontalRadius
      (b.getMinY, b.getMaxY, r, (b.getMinX + b.getMaxX) / 2.0)
    }
    assertEqualsDouble(bottom - top, 32, 1)
    assertEqualsDouble(top, (48 - 32) / 2.0, 1)
    assert(bottom <= 47, s"the pill reaches $bottom, over the hairline at 47")
    assertEqualsDouble(radius, 8, 0)
    assertEqualsDouble(centre, 720, 1)
  }

  // --- Perspectives: ⌘1–5, retained panes, Compare by trail depth ------------

  fxStage.test("⌘1–⌘4 switch perspectives; the toggle group follows the model") { fx =>
    val w    = boot(fx, StoryModels.t2Compare)
    val keys = Vector(
      KeyCode.DIGIT1 -> (Perspective.Data, "data.verify"),
      KeyCode.DIGIT2 -> (Perspective.Analysis, "analysis"),
      KeyCode.DIGIT3 -> (Perspective.Results, "compare.query"),
      KeyCode.DIGIT4 -> (Perspective.Report, "figures")
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
    // Every perspective, twice, and Results' three layouts: the pair, the
    // drill (a record) and the summary.
    val tour = Vector(1, 2, 3, 4, 1, 2, 3, 4).map(d => KeyCode.valueOf(s"DIGIT$d"))
    tour.take(4).foreach(fx.robot.press(_, shortcut))
    val first = runOnFx(panes.creations.map(id => id -> panes.node(id)).toMap)
    tour.drop(4).foreach(fx.robot.press(_, shortcut))
    fx.robot.press(KeyCode.DIGIT3, shortcut)
    assertEquals(runOnFx(w.host.active), Some("compare.query"))
    dispatch(fx, w, Intent.Explain(eyes4s.studio.app.nav.Place.At(StoryModels.record)))
    assertEquals(runOnFx(w.host.active), Some("explore"))
    dispatch(fx, w, Intent.OpenCrumb(0))
    assertEquals(runOnFx(w.host.active), Some("compare.summary"))
    dispatch(fx, w, Intent.Back)
    assertEquals(runOnFx(w.host.active), Some("explore"))
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
    assert(node("compare.participant-plot").isDefined)
  }

  fxStage.test(
    "Results shows the summary layout at the Summary crumb and the pair layout below it"
  ) { fx =>
    val w       = boot(fx, StoryModels.t2Compare)
    val profile =
      StudioPaneId.of("compare.participant-plot").fold(e => fail(e.message), identity)
    assertEquals(runOnFx(w.host.active), Some("compare.query"))
    assert(runOnFx(w.host.dock.state.findPane(w.host.dockId(profile))).isEmpty)
    val crumbs = runOnFx(w.shell.contextStrip.crumbs)
    fx.robot.click(crumbs.head)
    assertEquals(runOnFx(w.host.active), Some("compare.summary"))
    // The slopegraphs are the summary's (overhaul v4).
    assert(runOnFx(w.host.dock.state.findPane(w.host.dockId(profile))).isDefined)
    // Deeper than the Summary crumb: the query layout again.
    dispatch(
      fx,
      w,
      Intent.Navigate(
        eyes4s.studio.app.nav.Location(Perspective.Results, StoryModels.queryTrail.take(3))
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

  fxStage.test(
    "state button idle at t2: three slots, no cancel; the menu lists four sections"
  ) { fx =>
    val w     = boot(fx, StoryModels.t2Compare)
    val state = w.shell.appBar.state
    assertEquals(
      runOnFx(state.drawnSlots),
      Vector("Run 7 · current", "Data r3", "Analysis rev 5 · not run")
    )
    assert(!runOnFx(state.cancel.isVisible))
    assertEquals(
      runOnFx(state.button.getAccessibleText),
      "Study state: Run 7 · current | Data r3 | Analysis rev 5 · not run. Opens the state menu"
    )
    fx.robot.click(runOnFx(state.button))
    eventually(fx, "the state menu")(state.menu.isShowing)
    assertEquals(
      runOnFx(state.drawnSections),
      Vector("Run 7 · current", "Dataset r3 · admitted", "Analysis rev 5 · not run", "Jobs")
    )
    fx.snapshot(StudioTheme.Light)
    runOnFx(state.menu.hide())
    // Closing the menu asks nothing of the model.
    assertEquals(runOnFx(w.runtime.model.pending), None)
  }

  fxStage.test(
    "state button running at t3: run 8 adopted from the fake backend; ✕ asks, Cancel run reaches it"
  ) { fx =>
    assumeFullStage(fx)
    val w     = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    val state = w.shell.appBar.state
    assertEquals(
      runOnFx(state.drawnSlots),
      Vector("Showing run 7", "Data r3", "Run 8 · comparing 21,400 / 44,845")
    )
    assert(runOnFx(state.cancel.isVisible))
    assertEqualsDouble(runOnFx(state.drawnProgress).get, 21400.0 / 44845.0, 1e-9)
    assertEquals(runOnFx(state.cancel.getAccessibleText), "Cancel run 8")
    fx.robot.click(runOnFx(state.cancel))
    eventually(fx, "the inline confirmation")(state.menu.isShowing && state.asking)
    assertEquals(
      runOnFx(state.drawnMenu),
      Vector(
        "menu: Cancel run 8? | 21,400 of 44,845 pairs are done. Run 7 stays shown. | Cancel run / Keep running"
      )
    )
    // Keep running is the default and has the focus: Enter keeps the run.
    val keep = runOnFx(
      state.content.lookupAll(".state-default").asScala.collectFirst { case b: Button => b }.get
    )
    assert(runOnFx((keep.getScene.getFocusOwner eq keep) && keep.isDefaultButton))
    fx.snapshot(StudioTheme.Light)
    val confirm = runOnFx(
      state.content
        .lookupAll(".state-destructive")
        .asScala
        .collectFirst { case b: Button =>
          b
        }
        .get
    )
    // The menu is its own popup window: its buttons are fired, not clicked.
    runOnFx(confirm.fire())
    fx.awaitLayout()
    eventually(fx, "the cancelled run")(
      !state.drawnSlots.exists(_.startsWith("Run 8 · comparing"))
    )
    assert(!runOnFx(state.menu.isShowing))
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

  fxStage.test("Esc at the confirmation keeps the run: the model's question is dismissed") {
    fx =>
      val w     = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      val state = w.shell.appBar.state
      fx.robot.click(runOnFx(state.cancel))
      eventually(fx, "the inline confirmation")(state.menu.isShowing && state.asking)
      runOnFx(
        javafx.event.Event.fireEvent(
          state.content,
          javafx.scene.input.KeyEvent(
            javafx.scene.input.KeyEvent.KEY_PRESSED,
            "",
            "",
            KeyCode.ESCAPE,
            false,
            false,
            false,
            false
          )
        )
      )
      eventually(fx, "the closed menu")(!state.menu.isShowing)
      assertEquals(runOnFx(w.runtime.model.pending), None)
      assert(runOnFx(state.drawnSlots.last.startsWith("Run 8 · comparing")))
  }

  fxStage.test(
    "state button failed: 'Run 8 failed · 2 diagnostics' opens the Diagnostics pane"
  ) { fx =>
    val w     = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    val state = w.shell.appBar.state
    w.session.await(w.session.fixture.get.fail(StoryMoments.run8Job, Vector(lost, lost)))
    eventually(fx, "the failed slot")(
      state.drawnSlots.lastOption.contains("Run 8 failed · 2 diagnostics")
    )
    assert(runOnFx(!state.cancel.isVisible))
    fx.robot.click(runOnFx(state.button))
    eventually(fx, "the state menu")(state.menu.isShowing)
    val open = runOnFx(
      state.content
        .lookupAll(".state-action")
        .asScala
        .collectFirst {
          case b: Button if b.getText == "Run 8 failed · 2 diagnostics" => b
        }
        .get
    )
    runOnFx(open.fire())
    fx.awaitLayout()
    assertEquals(runOnFx(w.runtime.model.perspective), Perspective.Analysis)
    assertEquals(runOnFx(w.host.active), Some("analysis"))
    val diagnostics = w.host.dockId(StudioLayouts.diagnostics)
    assertEquals(runOnFx(w.host.dock.state.focused), Some(diagnostics))
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
    assertEquals(presentation.layouts.map(_.perspective), Vector(Perspective.Results))
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
        .find(_.getText.stripPrefix("_") == "View")
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

  fxStage.test("⌘⇧↩ maximizes the focused group and restores it; the dock follows the model") {
    fx =>
      val w      = boot(fx, StoryModels.t2Compare)
      val model  = () => runOnFx(w.runtime.model)
      val docked = () => runOnFx(w.host.dock.state.maximized)
      // The v4 pane header has no maximize button (the golden's): the key is
      // the way, on the focused group.
      dispatch(fx, w, Intent.FocusPane(StudioPaneId.of("compare.contrast").toOption.get))
      fx.robot.press(KeyCode.ENTER, shortcut.copy(shift = true))
      assert(model().isMaximized)
      assertEquals(model().focusedPane.value, "compare.contrast")
      assert(docked().isDefined)
      fx.robot.press(KeyCode.ENTER, shortcut.copy(shift = true))
      assert(!model().isMaximized)
      assertEquals(docked(), None)
  }

  fxStage.test("F6 and ⇧F6 cycle groups; ⌃⇥ and ⌃⇧⇥ cycle tabs through the dock") { fx =>
    val w       = boot(fx, StoryModels.t2Compare)
    def focused = runOnFx(w.runtime.model.focusedPane.value)
    def docked  = runOnFx(w.host.dock.state.focused.map(_.value))
    assertEquals(focused, "compare.pair")
    fx.robot.press(KeyCode.F6)
    assertEquals(focused, "compare.contrast")
    fx.robot.press(KeyCode.F6, Modifiers(shift = true))
    assertEquals(focused, "compare.pair")
    fx.robot.press(KeyCode.TAB, Modifiers(control = true))
    fx.awaitLayout()
    assertEquals(focused, "compare.pair.table")
    assertEquals(docked, Some("compare.pair.table"))
    fx.robot.press(KeyCode.TAB, Modifiers(control = true, shift = true))
    fx.awaitLayout()
    assertEquals(focused, "compare.pair")
    assertEquals(docked, Some("compare.pair"))
  }

  // --- Saved layouts: no spurious saves; unreadable ones never block ----------

  fxStage.test("touring every perspective, focusing and maximizing saves no layout") { fx =>
    val w = boot(fx, StoryModels.t2Compare)
    Vector(1, 2, 3, 4).foreach { d =>
      fx.robot.press(KeyCode.valueOf(s"DIGIT$d"), shortcut)
      fx.robot.press(KeyCode.F6)
    }
    fx.robot.press(KeyCode.DIGIT3, shortcut)
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
          Intent.Dispatch(Command.SaveLayout(Perspective.Results, Some(LayoutBlob(text))))
        )
        assertEquals(AppModel.savedLayout(broken, Perspective.Results), Some(LayoutBlob(text)))
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
        assert(notice.startsWith("The saved layout of Results could not be read ("), notice)
        assert(notice.endsWith("); it shows the default layout."), notice)
        runOnFx(w.runtime.model.notice) match
          case Some(Notice.LayoutsReset(ps, _)) => assertEquals(ps, Vector(Perspective.Results))
          case other                            => fail(s"expected LayoutsReset, got $other")
        // The next capture replaces the unreadable layout with the default.
        runOnFx(w.captureLayouts())
        assertEquals(runOnFx(w.runtime.model.document.presentation.layouts), Vector.empty)
    }
  }

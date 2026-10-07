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

package eyes4s.studio.desktop.journey

import eyes4s.studio.app.keys.{AppCommand, CommandRegistry, KeyChord, Modifier}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{AppModel, Intent, PlatformDialog, StoryModels}
import eyes4s.studio.core.backend.{PairDesign, RunId}
import eyes4s.studio.app.figures.NewPanel
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, Modifiers}
import eyes4s.studio.desktop.platform.TempDirs
import eyes4s.studio.desktop.shell.ShellKeys
import javafx.scene.control.{
  ComboBox,
  Labeled,
  ListView,
  Menu,
  MenuButton,
  MenuItem,
  RadioButton
}
import javafx.scene.input.KeyCode
import javafx.scene.{Node, Parent}

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** E2E-01 from the keyboard alone (ticket S10.5b): the golden journey in the
  * window, headless on Monocle, driven by key events only (the keymap's
  * chords, Tab, the arrows, Space and Enter), held to the headless route
  * byte for byte as the pointer route is ([[GoldenWindow.heldToHeadless]]).
  *
  * The window runs its own key path (as on Linux and Windows), so a chord is
  * a key event. Named steps that are not key events:
  *  - platform answers: the import wizard's commit (its own window; its
  *    pages are audited in ImportWizardFxSuite), Repair…'s and Export
  *    bundle…'s file choosers, and the fake's `declare` and `complete`;
  *  - commands with no chord and no control in the window, fired as their
  *    menu items as the system menu bar does from the keyboard on macOS
  *    (Ctrl-F2); off macOS the window shows no menu bar (S10.5 K1). The
  *    journey reaches every command it needs from the window, so none is
  *    fired that way ([[viaSystemMenu]] holds what was);
  *  - starting a draft (`StartDraft`): no control starts one (S7.3);
  *  - opening the matched-pair context for figure templates, as the pointer
  *    route opens its crumb/row. Figure creation itself uses keyboard menus.
  */
class KeyboardJourneySuite extends GoldenWindow:
  import StoryModels.{p17enc03, sigma2}

  override val munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(600, "s")

  private val r2   = StoryMoments.r2
  private val r3   = StoryMoments.r3
  private val run6 = RunId(6)

  private def model(w: StudioWindow): AppModel = runOnFx(w.runtime.model)

  // -------------------------------------------------------------------------
  // The keyboard
  // -------------------------------------------------------------------------

  private def modifiers(chord: KeyChord): Modifiers =
    val m = chord.modifiers
    Modifiers(
      shift = m.contains(Modifier.Shift),
      control = m.contains(Modifier.Control) || (m.contains(Modifier.Command) && !isMac),
      alt = m.contains(Modifier.Option),
      meta = m.contains(Modifier.Command) && isMac
    )

  /** The command's chord, pressed. */
  private def chord(fx: FxStage, c: AppCommand): Unit =
    val k = c.shortcut.getOrElse(fail(s"${c.id.value} has no chord"))
    fx.robot.press(ShellKeys.code(k.key), modifiers(k))

  /** Commands reached through the system menu bar, in order (S10.5 K1). */
  private val viaSystemMenu = Vector.newBuilder[String]

  /** A command with no chord, as the system menu bar fires it. */
  private def systemMenu(fx: FxStage, w: StudioWindow, c: AppCommand): Unit =
    assert(c.shortcut.isEmpty, s"${c.id.value} has a chord; press it")
    val item = runOnFx {
      def flat(i: MenuItem): Vector[MenuItem] = i match
        case m: Menu => m.getItems.asScala.toVector.flatMap(flat)
        case other   => Vector(other)
      w.shell.menus.flatMap(_.getItems.asScala).flatMap(flat).find(_.getId == c.id.value)
    }.getOrElse(fail(s"no menu item ${c.id.value}"))
    assert(!runOnFx(item.isDisable), s"${c.id.value} is disabled")
    viaSystemMenu += c.id.value
    runOnFx(item.fire())
    fx.awaitLayout()

  private def owner(fx: FxStage): Option[Node] = runOnFx(Option(fx.scene.getFocusOwner))

  private def name(n: Node): String = runOnFx(Option(n.getAccessibleText).getOrElse(""))

  private def describe(n: Node): String = runOnFx(
    s"${n.getAccessibleRole}: ${Option(n.getAccessibleText).getOrElse(n.getClass.getSimpleName).take(70)}"
  )

  /** Tab until the focus owner is `what`; the cycle walked is shown when it
    * is not found.
    */
  private def tabTo(fx: FxStage, what: String)(found: Node => Boolean): Node =
    val seen = Vector.newBuilder[String]
    var n    = 0
    var at   = owner(fx)
    while !at.exists(a => runOnFx(found(a))) do
      if n >= 120 then
        fail(s"Tab did not reach $what; walked:\n${seen.result().mkString("\n")}")
      at.foreach(a => seen += describe(a))
      fx.robot.press(KeyCode.TAB)
      at = owner(fx)
      n += 1
    at.get

  /** Tab to the stop whose accessible name starts with `prefix`. */
  private def tabToNamed(fx: FxStage, prefix: String): Node =
    tabTo(fx, s"'$prefix'")(n => Option(n.getAccessibleText).exists(_.startsWith(prefix)))

  private def space(fx: FxStage): Unit = fx.robot.press(KeyCode.SPACE)

  /** Tab to the combo box named `prefix`, then step it with ↓ (↑ once past
    * the end) until `wanted` holds of its value's text.
    */
  private def choose(fx: FxStage, prefix: String)(wanted: String => Boolean): Unit =
    val box = tabToNamed(fx, prefix) match
      case c: ComboBox[?] => c
      case other          => fail(s"$prefix is not a combo box: $other")
    def value = runOnFx(Option(box.getValue).fold("")(_.toString))
    var n     = 0
    var down  = true
    while !wanted(value) do
      if n >= 40 then fail(s"$prefix never offered the wanted value; at '$value'")
      val before = value
      fx.robot.press(if down then KeyCode.DOWN else KeyCode.UP)
      if value == before then down = !down
      n += 1

  /** ↓ on the row-cursor stop `stop` until its name says `wanted`. */
  private def cursorTo(fx: FxStage, stop: Node, what: String)(wanted: String => Boolean): Unit =
    var n = 0
    while !wanted(name(stop)) do
      if n >= 200 then fail(s"the row cursor never reached $what; at '${name(stop)}'")
      fx.robot.press(KeyCode.DOWN)
      n += 1

  /** Names and roles in the state the journey is in (A11yChecks): every
    * Tab stop named, and no two alike. These states are not the resting
    * boards A11yTreeSuite audits.
    */
  private def audit(w: StudioWindow, stage: String): Unit =
    import eyes4s.studio.desktop.shell.A11yChecks
    assertEquals(runOnFx(A11yChecks.unlabelled(w.root)), Vector.empty[String], stage)
    assertEquals(runOnFx(A11yChecks.ambiguous(w.root)), Vector.empty[String], stage)

  /** Every shown label's text. */
  private def shownTexts(w: StudioWindow): Vector[String] = runOnFx {
    def all(n: Node): Vector[Node] = n +: (n match
      case p: Parent => p.getChildrenUnmodifiable.asScala.toVector.flatMap(all)
      case _         => Vector.empty)
    all(w.root).collect {
      case l: Labeled
          if Iterator.iterate[Node](l)(_.getParent).takeWhile(_ != null).forall(_.isVisible) =>
        Option(l.getText).getOrElse("")
    }
  }

  // -------------------------------------------------------------------------

  fxStage.test("E2E-01-keyboard: the golden journey from the keyboard alone") { fx =>
    assumeFullStage(fx)
    val route      = headless()
    val base       = Files.createTempDirectory("keyboard-journey")
    val projectDir = base.resolve("journey.eyes")
    val exportDir  = base.resolve("export")
    Files.createDirectories(exportDir)
    try
      val history = eyes4s.studio.app.driver.GoldenJourney.history
      val port    = project(projectDir, history)
      val dialogs = Dialogs(None)
      val w       = boot(
        fx,
        AppModel.open(history, Some(StoryModels.project)),
        StoryMoment.T1,
        dialogs = dialogs,
        project = Some(port),
        chooseFolder = (_, _) => Some(exportDir),
        assetFiles = standIn
      )

      // --- Import and admission ---------------------------------------------

      // File › Import sources… has no chord and no control in the window: the
      // system menu bar. Its wizard's commit, as its window sends it.
      systemMenu(fx, w, CommandRegistry.importSources)
      assertEquals(dialogs.asked.toVector, Vector(PlatformDialog.ImportSources))
      val t1r3   = StoryModels.t1.dataset(r3).get
      val r2Spec = StoryModels.t1.dataset(r2).get
      runOnFx(
        StudioWindow.followImports(() => w.runtime.model, w.runtime.dispatch)(
          Intent.Dispatch(
            Command.ImportSources(
              Some(r2),
              r2Spec.sources,
              r2Spec.mapping,
              r2Spec.units,
              r2Spec.geometry,
              DeclaredAttributes.empty,
              None,
              t1r3.inventory
            )
          )
        )
      )
      fx.awaitLayout()
      // The import took Data to r3 (S10.5 K2).
      assertEquals(model(w).location.trail, Vector(Place.Dataset(r3)))

      // The column-mapping pane: occurrence's role, milliseconds, Apply.
      choose(fx, "Role for occurrence")(_.contains("Occurrence"))
      choose(fx, "Time unit")(_.toLowerCase.contains("millisecond"))
      tabToNamed(fx, "Apply to r3")
      space(fx)
      eventually(fx, "r3's mapping applied")(
        w.runtime.model.document
          .dataset(r3)
          .exists(d =>
            d.mapping == t1r3.mapping && d.units == DeclaredUnits(Some(TimeUnit.Milliseconds))
          )
      )

      audit(w, "after the mapping")
      // Admission: ↓ in the decision's radio group chooses Review exclusions
      // (S10.5 K3), then Admit as r3.
      val radio = tabTo(fx, "the admission choice")(_.isInstanceOf[RadioButton])
      assert(name(radio).startsWith("Require complete"), name(radio))
      fx.robot.press(KeyCode.DOWN)
      assert(owner(fx).exists(name(_).startsWith("Review exclusions")), owner(fx).map(describe))
      tabToNamed(fx, "Admit as r3")
      space(fx)
      eventually(fx, "r3 admitted")(
        w.runtime.model.document.dataset(r3).exists(_.decision.isAdmitted)
      )
      assert(shownTexts(w).exists(_.contains("937")), "the admission pane shows 937 admitted")

      audit(w, "after admission")
      // Repair the two missing images: Repair…, twice.
      eventually(fx, "two missing images")(shownTexts(w).contains("257 of 259 images found"))
      for n <- 1 to 2 do
        tabToNamed(fx, "Repair")
        space(fx)
        eventually(fx, s"$n image(s) repaired")(
          w.runtime.model.document.relinks.of(r3).size == n
        )
      eventually(fx, "every image found")(shownTexts(w).contains("259 of 259 images found"))

      audit(w, "after repair")
      // --- Explore P17 enc_03 -----------------------------------------------

      chord(fx, CommandRegistry.explore)
      tabToNamed(fx, "Filter trials")
      fx.robot.typeText("P17")
      val trials = tabToNamed(fx, "Trials of r3")
      def row    = runOnFx(trials match
        case l: ListView[?] => Option(l.getSelectionModel.getSelectedItem).fold("")(_.toString)
        case _              => "")
      var n = 0
      while !row.contains("enc_03,") do
        if n >= 20 then fail(s"no enc_03 row; at $row")
        fx.robot.press(KeyCode.DOWN)
        n += 1
      fx.robot.press(KeyCode.ENTER)
      assertEquals(
        model(w).location.trail.lastOption,
        Some(Place.At(StudioRef.Trial(p17enc03)))
      )

      audit(w, "Explore, before a run")
      // --- Analysis rev 4 and run 6 -----------------------------------------

      dispatch(
        fx,
        w,
        Intent.Dispatch(Command.StartDraft(StoryMoments.rev3, Some(r3), Vector.empty))
      )
      // The fake learns a draft's revision until S3.7 binds plans.
      w.session
        .await(w.session.fixture.get.declare(StoryMoments.rev4, r3))
        .fold(e => fail(e.toString), identity)
      chord(fx, CommandRegistry.analysis)
      // Tab once the shown design is checked: while Analysis is still
      // answering, Tab can bounce between its panes (S10.5 K5).
      eventually(fx, "Analysis settled")(w.resolvedDesign.state.preview.receipt.isDefined)
      // The context strip's draft chip opens the draft.
      tabToNamed(fx, "Draft rev 4")
      space(fx)
      eventually(fx, "the draft's design is checked")(
        w.resolvedDesign.state.preview.receipt.isDefined
      )
      tabToNamed(fx, "Save & run rev 4")
      space(fx)
      eventually(fx, "run 6's job")(w.runtime.model.jobs.jobs.exists(_.run == run6))
      val job = model(w).jobs.jobs.find(_.run == run6).get.id
      w.session.await(w.session.fixture.get.complete(job)).fold(e => fail(e.toString), identity)
      eventually(fx, "run 6 ready")(w.runtime.model.jobs.ready.exists(_.run == run6))
      audit(w, "run 6 ready")
      // The jobs chip's Show.
      tabToNamed(fx, "Run 6 ready")
      space(fx)
      eventually(fx, "run 6 shown")(
        w.runtime.model.document.presentation.shownRun.contains(run6)
      )

      audit(w, "run 6 shown")
      // --- Compare P17 ret_07, then the summary ------------------------------

      chord(fx, CommandRegistry.compare)
      // Select 2° from the reports, then move the cursor before activating.
      eventually(fx, "the 2° report is available")(
        w.summary.vm.scales.exists(c => c.scale == sigma2 && c.available)
      )
      tabToNamed(fx, "σ ")
      fx.robot.press(KeyCode.RIGHT)
      fx.robot.press(KeyCode.RIGHT)
      eventually(fx, "2° is selected from the keyboard")(
        w.summary.vm.scales.exists(c => c.scale == sigma2 && c.chosen)
      )
      eventually(fx, "participant report rows")(w.summary.participantTable.rowCount == 24)
      val participants = tabToNamed(fx, "Participant summary")
      cursorTo(fx, participants, "P17")(_.contains("P17"))
      fx.robot.press(KeyCode.ENTER)
      eventually(fx, "P17 is selected")(
        w.runtime.model.selection.selected.exists(_.toString.contains("P17"))
      )
      tabToNamed(fx, "Explain P17")
      space(fx)
      // The queries navigator's row cursor: P17, → to open it, ret_07, Enter.
      val queries = tabToNamed(fx, "Queries")
      cursorTo(fx, queries, "P17")(_.contains("Queries, P17:"))
      fx.robot.press(KeyCode.RIGHT)
      cursorTo(fx, queries, "ret_07")(_.contains("ret_07"))
      fx.robot.press(KeyCode.ENTER)
      eventually(fx, "the contrast readout") {
        val t = shownTexts(w)
        t.contains("+0.38") && t.exists(_.contains("0.73")) && t.exists(_.contains("0.35"))
      }
      assert(
        model(w).location.trail.exists(_.toString.contains("ret_07")) ||
          model(w).location.toString.contains(sigma2.toString),
        model(w).location
      )
      audit(w, "the query")
      // The summary: its crumb.
      tabToNamed(fx, "Summary · by retrieval response")
      space(fx)
      eventually(fx, "the summary")(shownTexts(w).exists(_.contains("+0.38")))

      audit(w, "the summary")
      // --- Rev 5, discarded ---------------------------------------------------

      val rev4   = model(w).document.analysis(StoryMoments.rev4).get
      val scales = ok(ScaleSet.of(rev4.recipe.scales.values :+ ok(Sigma.of(8.0))))
      dispatch(
        fx,
        w,
        Intent.Dispatch(
          Command.StartDraft(
            StoryMoments.rev4,
            None,
            Vector(RecipeChange.Scales(rev4.recipe.scales, scales))
          )
        )
      )
      w.session
        .await(w.session.fixture.get.declare(StoryMoments.rev5, r3))
        .fold(e => fail(e.toString), identity)
      // The context strip's draft chip, then rev 5's pair rows in Analysis.
      tabToNamed(fx, "Draft rev 5")
      space(fx)
      eventually(fx, "rev 5's pair rows")(
        shownTexts(w).exists(_.contains("Save & run rev 5 · 44,845 pairs"))
      )
      audit(w, "rev 5 in Analysis")
      // Discard draft from Compare's context strip, then the confirmation's.
      chord(fx, CommandRegistry.compare)
      eventually(fx, "Compare settled")(model(w).perspective == Perspective.Compare)
      tabToNamed(fx, "Discard draft")
      space(fx)
      tabTo(fx, "the confirmation's Discard draft")(n =>
        n.getStyleClass.contains("confirm-action")
      )
      space(fx)
      eventually(fx, "no draft")(w.runtime.model.document.draft.isEmpty)

      audit(w, "after the discard")
      // --- Figures and the export bundle -----------------------------------

      chord(fx, CommandRegistry.figures)
      tabToNamed(fx, "New figure")
      space(fx)
      eventually(fx, "Figure 3")(w.runtime.model.document.figures.exists(_.id.number == 3))
      dispatch(
        fx,
        w,
        Intent.Explain(
          Place.At(
            StudioRef.Pair(run6, sigma2, PairDesign.Matched, StoryModels.p17ret07, p17enc03)
          )
        )
      )
      chord(fx, CommandRegistry.figures)
      def template(menuName: String, kind: NewPanel): Unit =
        val menu = tabToNamed(fx, menuName).asInstanceOf[MenuButton]
        eventually(fx, s"${kind.label} available")(
          runOnFx(menu.getItems.asScala.exists(i => i.getText == kind.label && !i.isDisable))
        )
        val index = runOnFx(
          menu.getItems.asScala.filterNot(_.isDisable).indexWhere(_.getText == kind.label)
        )
        space(fx)
        eventually(fx, s"$menuName menu open")(runOnFx(menu.isShowing))
        (0 to index).foreach(_ => fx.robot.press(KeyCode.DOWN))
        fx.robot.press(KeyCode.ENTER)
      template("Start figure with", NewPanel.EncodingGaze)
      eventually(fx, "Figure 4 begins with A")(
        w.runtime.model.document.figures.lastOption.exists(f =>
          f.id.number == 4 && f.panels.map(_.letter.value) == Vector("A")
        )
      )
      Vector(
        NewPanel.RetrievalGaze,
        NewPanel.DensityMaps,
        NewPanel.ParticipantD,
        NewPanel.ScaleProfile
      ).zipWithIndex.foreach { (kind, i) =>
        template("Add panel", kind)
        eventually(fx, s"${kind.label} appended")(
          w.runtime.model.document.figures.last.panels.size == i + 2
        )
      }
      assertEquals(model(w).document.figures.last.panels, ok(StoryMoments.figure1).panels)
      val figure = model(w).document.figures.last.id
      assertEquals(figure.number, 4)
      // The figure list's row for Figure 4.
      tabToNamed(fx, "Figure 4")
      space(fx)
      eventually(fx, "Figure 4 shown")(
        w.runtime.model.location.trail.contains(Place.Figure(figure))
      )
      // The bundle's project snapshot, then Export bundle….
      tabTo(fx, "the bundle's project snapshot")(n =>
        Option(n.getAccessibleText).exists(t =>
          t.startsWith("project snapshot") && t.endsWith(", not in the bundle")
        )
      )
      space(fx)
      eventually(fx, "Figure 4 is saved")(!w.runtime.model.save.edited)
      tabToNamed(fx, "Export bundle")
      space(fx)
      val bundleDir = exportDir.resolve(s"figure-${figure.number}-bundle")
      eventually(fx, "the bundle is written")(
        Files.isRegularFile(bundleDir.resolve("README.txt"))
      )
      audit(w, "after the export")
      chord(fx, CommandRegistry.compare)

      // Saved: the window's project folder holds the journey.
      eventually(fx, "the project is saved")(!w.runtime.model.save.edited)
      val ui = model(w).document
      runOnFx(w.close())
      opened -= w

      assertEquals(viaSystemMenu.result(), Vector(CommandRegistry.importSources.id.value))
      heldToHeadless(route, ui, projectDir, bundleDir)
    finally TempDirs.remove(base)
  }

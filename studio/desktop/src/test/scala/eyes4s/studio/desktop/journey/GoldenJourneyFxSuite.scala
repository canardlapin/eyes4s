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

import eyes4s.studio.app.driver.GoldenJourney
import eyes4s.studio.app.keys.{CommandId, CommandRegistry}
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.app.{AppModel, Intent, PlatformDialog, StoryModels}
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.platform.TempDirs
import javafx.scene.Node
import javafx.scene.control.{Button, Labeled, Menu, MenuItem, RadioButton, ToggleButton}

import java.nio.file.Files
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** E2E-01-ui (ticket S10.1): the golden journey through the studio window,
  * headless on Monocle, with its own project folder and export folder, then
  * held to the headless route ([[GoldenRoute]]) run in the same JVM: the same
  * document science, the same export bundle byte for byte, and a project
  * folder whose document parts are the headless folder's.
  *
  * Commands go through the window's controls as the user gives them: the
  * menu bar's items (Import sources…, Review draft, Show run), the panes'
  * controls (Review exclusions, Admit as r3, Repair…, Save & run, New
  * figure, the bundle's project snapshot, Export bundle…) and the menu's
  * Discard draft with the confirmation bar's Discard draft. The run completes through the
  * window's own fake backend.
  *
  * Named bypasses, each dispatched as the missing control would dispatch it:
  *  - the import dialog's answer (`ImportSources`) and the mapping pane's
  *    commit (`SetMapping`, `SetUnits`): the dialog is the platform's, and
  *    the mapping pane's own suites drive its controls (S5.2, S5.3);
  *  - navigations (`Navigate`, `Explain`) where the user's gesture is a
  *    crumb, a row or a link;
  *  - starting a draft (`StartDraft`, rev 4 on r3 and rev 5 with σ 8°): no
  *    control starts a draft or edits a recipe yet (S7.3,
  *    bd-01M3DPFSE5NN4B3SSHHYA76B5B);
  *  - the board's Figure 1 panels A–E (`CreateFigure`, Figure 4): New figure
  *    makes Figure 3 with its two default panels, and no control adds a
  *    panel (`AddPanel`) yet (a bead is requested in the S10.1 report);
  *  - the fake's `declare` of a draft's revision, until S3.7 binds plans.
  */
class GoldenJourneyFxSuite extends GoldenWindow:
  import StoryModels.{p17enc03, p17ret07, sigma2}

  override val munitTimeout: Duration = Duration(600, "s")

  private val r2   = StoryMoments.r2
  private val r3   = StoryMoments.r3
  private val rev3 = StoryMoments.rev3
  private val run6 = RunId(6)

  // -------------------------------------------------------------------------
  // The window's controls, as the user reaches them
  // -------------------------------------------------------------------------

  private def menuItem(w: StudioWindow, id: CommandId): MenuItem = runOnFx {
    def flat(i: MenuItem): Vector[MenuItem] = i match
      case m: Menu => m.getItems.asScala.toVector.flatMap(flat)
      case other   => Vector(other)
    w.shell.menus
      .flatMap(_.getItems.asScala)
      .flatMap(flat)
      .find(_.getId == id.value)
      .getOrElse(fail(s"no menu item ${id.value}"))
  }

  private def choose(fx: FxStage, w: StudioWindow, id: CommandId): Unit =
    val item = menuItem(w, id)
    assert(!runOnFx(item.isDisable), s"${id.value} is disabled")
    runOnFx(item.fire())
    fx.awaitLayout()

  private def nodes(root: Node): Vector[Node] = root match
    case p: javafx.scene.Parent =>
      root +: p.getChildrenUnmodifiable.asScala.toVector.flatMap(nodes)
    case other => Vector(other)

  /** The shown, enabled control whose text is `text`, once there is one. */
  private def control[A <: Labeled](fx: FxStage, w: StudioWindow, text: String)(
      pick: PartialFunction[Node, A]
  ): A =
    var found: Option[A] = None
    eventually(fx, s"a control '$text'") {
      found = nodes(w.root)
        .collect(pick)
        .find(c =>
          c.getText != null && c.getText.startsWith(text) && visible(c) && !c.isDisabled
        )
      found.isDefined
    }
    found.get

  private def press(fx: FxStage, w: StudioWindow, text: String): Unit =
    val b = control(fx, w, text) { case b: Button => b }
    runOnFx(b.fire())
    fx.awaitLayout()

  private def shown(w: StudioWindow): Vector[String] = runOnFx(
    nodes(w.root).collect { case l: Labeled if visible(l) => Option(l.getText).getOrElse("") }
  )

  /** Shown: the node and everything enclosing it are visible. */
  private def visible(n: Node): Boolean =
    Iterator.iterate(n)(_.getParent).takeWhile(_ != null).forall(_.isVisible)

  private def model(w: StudioWindow): AppModel = runOnFx(w.runtime.model)

  // -------------------------------------------------------------------------

  fxStage.test("E2E-01-ui: the golden journey in the window equals the headless route") { fx =>
    assumeFullStage(fx)
    val route = headless()

    val base       = Files.createTempDirectory("golden-journey-ui")
    val projectDir = base.resolve("journey.eyes")
    val exportDir  = base.resolve("export")
    Files.createDirectories(exportDir)
    try
      val history = GoldenJourney.history
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

      // Import: File › Import sources…, answered as the dialog answers it.
      choose(fx, w, CommandRegistry.importSources.id)
      assertEquals(dialogs.asked.toVector, Vector(PlatformDialog.ImportSources))
      val t1r3   = StoryModels.t1.dataset(r3).get
      val r2Spec = StoryModels.t1.dataset(r2).get
      dispatch(
        fx,
        w,
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
      // Map columns as the mapping pane commits them.
      dispatch(fx, w, Intent.Dispatch(Command.SetMapping(r3, t1r3.mapping)))
      dispatch(
        fx,
        w,
        Intent.Dispatch(Command.SetUnits(r3, DeclaredUnits(Some(TimeUnit.Milliseconds))))
      )

      // Admit r3 in the admission pane: Review exclusions, then Admit as r3.
      dispatch(
        fx,
        w,
        Intent.Navigate(
          Location(
            Perspective.Data,
            Vector(Place.Dataset(r3), Place.DataView(DataSection.Admission))
          )
        )
      )
      val review = control(fx, w, "Review exclusions") { case r: RadioButton => r }
      runOnFx(review.fire())
      press(fx, w, "Admit as r3")
      eventually(fx, "r3 admitted")(
        w.runtime.model.document.dataset(r3).exists(_.decision.isAdmitted)
      )
      assert(shown(w).exists(_.contains("937")), "the admission pane shows 937 admitted")

      // Repair the two missing images in Data › Sources: Repair…, twice.
      dispatch(
        fx,
        w,
        Intent.Navigate(
          Location(
            Perspective.Data,
            Vector(Place.Dataset(r3), Place.DataView(DataSection.Sources))
          )
        )
      )
      eventually(fx, "two missing images")(
        nodes(w.root).exists {
          case l: Labeled => l.getText == "257 of 259 images found"; case _ => false
        }
      )
      press(fx, w, "Repair")
      eventually(fx, "one image repaired")(w.runtime.model.document.relinks.of(r3).size == 1)
      press(fx, w, "Repair")
      eventually(fx, "both images repaired")(w.runtime.model.document.relinks.of(r3).size == 2)
      eventually(fx, "every image found")(
        nodes(w.root).exists {
          case l: Labeled => l.getText == "259 of 259 images found"; case _ => false
        }
      )

      // Explore P17 enc_03.
      dispatch(fx, w, Intent.Explain(Place.At(StudioRef.Trial(p17enc03))))
      assertEquals(model(w).perspective, Perspective.Explore)

      // Analysis rev 4: a draft on r3, Review draft, then Save & run.
      dispatch(fx, w, Intent.Dispatch(Command.StartDraft(rev3, Some(r3), Vector.empty)))
      // The fake learns a draft's revision from a fake control until S3.7
      // binds plans (as Save & run declares it in the headless journey).
      w.session
        .await(w.session.backend.declare(StoryMoments.rev4, r3))
        .fold(e => fail(e.toString), identity)
      choose(fx, w, CommandRegistry.reviewDraft.id)
      assertEquals(model(w).perspective, Perspective.Analysis)
      eventually(fx, "the design is checked")(w.resolvedDesign.state.preview.receipt.isDefined)
      assert(runOnFx(w.preflight.runButton._2), runOnFx(w.preflight.runButton))
      runOnFx(w.preflight.pressRun())
      eventually(fx, "run 6's job")(w.runtime.model.jobs.jobs.exists(_.run == run6))
      val job = model(w).jobs.jobs.find(_.run == run6).get.id
      w.session.await(w.session.backend.complete(job)).fold(e => fail(e.toString), identity)
      eventually(fx, "run 6 ready")(w.runtime.model.jobs.ready.exists(_.run == run6))
      choose(fx, w, CommandRegistry.showRun.id)
      eventually(fx, "run 6 shown")(
        w.runtime.model.document.presentation.shownRun.contains(run6)
      )

      // Compare P17 ret_07: its readout is fixture.json's, as FIXTURE.md says.
      dispatch(fx, w, Intent.Explain(Place.At(StudioRef.QueryContrast(run6, sigma2, p17ret07))))
      assertEquals(model(w).perspective, Perspective.Compare)
      eventually(fx, "the contrast readout") {
        val t = shown(w)
        t.contains("+0.38") && t.exists(_.contains("0.73")) && t.exists(_.contains("0.35"))
      }
      fx.snapshot(StudioTheme.Light)

      // The summary.
      dispatch(
        fx,
        w,
        Intent.Navigate(
          Location(Perspective.Compare, Vector(Place.Summary(StoryModels.reporting)))
        )
      )
      assertEquals(model(w).perspective, Perspective.Compare)
      // Report rows are served at every scale. Select the journey's 2° explicitly.
      eventually(fx, "the 2° summary report")(
        w.summary.vm.scales.exists(c => c.scale == sigma2 && c.available)
      )
      val twoDegrees = control(fx, w, "σ 2°") {
        case b: ToggleButton if b.getText == "σ 2°" => b
      }
      runOnFx(twoDegrees.fire())
      eventually(fx, "the summary")(shown(w).exists(_.contains("+0.38")))

      // Rev 5 adds σ 8° (StartDraft: a named bypass until S7.3); Review
      // draft shows its 44,845 pair rows; then Discard draft, confirmed.
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
      assertEquals(model(w).document.draft.map(_.id), Some(StoryMoments.rev5))
      w.session
        .await(w.session.backend.declare(StoryMoments.rev5, r3))
        .fold(e => fail(e.toString), identity)
      choose(fx, w, CommandRegistry.reviewDraft.id)
      eventually(fx, "rev 5's pair rows")(
        shown(w).exists(_.contains("Save & run rev 5 · 44,845 pairs"))
      )
      // Edit › Discard draft (in Analysis the draft's actions are the menu's).
      choose(fx, w, CommandRegistry.discardDraft.id)
      val confirm = control(fx, w, "Discard draft") {
        case b: Button if b.getStyleClass.contains("confirm-action") => b
      }
      runOnFx(confirm.fire())
      eventually(fx, "no draft")(w.runtime.model.document.draft.isEmpty)

      // New figure: Figure 3 on the shown run, its two default panels.
      dispatch(fx, w, Intent.Navigate(Location(Perspective.Figures, Vector(Place.Figures))))
      press(fx, w, "New figure")
      eventually(fx, "Figure 3")(w.runtime.model.document.figures.exists(_.id.number == 3))
      assertEquals(
        model(w).document.figures.find(_.id.number == 3).map(f => (f.run, f.panels.size)),
        Some((run6, 2))
      )

      // Figure 4 with the board's panels (CreateFigure: a named bypass until
      // a control adds panels), and its bundle with the project snapshot.
      dispatch(
        fx,
        w,
        Intent.Dispatch(
          Command.CreateFigure(run6, StoryModels.reporting, ok(StoryMoments.figure1).panels)
        )
      )
      val figure = model(w).document.figures.last.id
      assertEquals(figure.number, 4)
      dispatch(
        fx,
        w,
        Intent.Navigate(
          Location(Perspective.Figures, Vector(Place.Figures, Place.Figure(figure)))
        )
      )
      // The bundle's snapshot row (not the appearance's "includes images").
      val row = control(fx, w, "project snapshot") {
        case b: Button
            if Option(b.getAccessibleText).exists(_.endsWith(", not in the bundle")) =>
          b
      }
      runOnFx(row.fire())
      fx.awaitLayout()
      eventually(fx, "the snapshot is in the bundle")(
        nodes(w.root).exists {
          case b: Button =>
            visible(b) && Option(b.getAccessibleText).exists(t =>
              t.startsWith("project snapshot") && t.endsWith(", in the bundle")
            )
          case _ => false
        }
      )
      // The snapshot is the project as last saved: let the figure be saved.
      eventually(fx, "Figure 4 is saved")(!w.runtime.model.save.edited)
      press(fx, w, "Export bundle")
      val bundleDir = exportDir.resolve(s"figure-${figure.number}-bundle")
      eventually(fx, "the bundle is written")(
        Files.isRegularFile(bundleDir.resolve("README.txt"))
      )
      fx.snapshot(StudioTheme.Light)
      // Back to Compare, as the headless route returns: View › Compare (⌘4).
      choose(fx, w, CommandRegistry.compare.id)
      assertEquals(model(w).perspective, Perspective.Compare)

      // Saved: the window's project folder holds the journey.
      eventually(fx, "the project is saved")(!w.runtime.model.save.edited)
      val ui = model(w).document
      runOnFx(w.close())
      opened -= w

      // --- The UI route against the headless route ------------------------

      heldToHeadless(route, ui, projectDir, bundleDir)
    finally TempDirs.remove(base)
  }

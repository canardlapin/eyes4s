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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.instances.future.*
import cats.syntax.all.*
import eyes4s.studio.app.driver.{DriverRecord, GoldenJourney, StudioDriver}
import eyes4s.studio.app.keys.{CommandId, CommandRegistry}
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.app.{AppModel, Intent, PlatformDialog, StoryModels}
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
import eyes4s.studio.desktop.runtime.SessionPort
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.Node
import javafx.scene.control.{Button, Labeled, Menu, MenuItem, RadioButton}

import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext}
import scala.jdk.CollectionConverters.*

/** E2E-01-ui (ticket S10.1): the golden journey through the studio window,
  * headless on Monocle, with its own project folder and export folder, then
  * held to the headless route ([[GoldenRoute]]) run in the same JVM: the same
  * document science, the same export bundle byte for byte, and a project
  * folder whose document parts are the headless folder's.
  *
  * Commands go through the window as the user gives them: the menu bar's
  * items (Import sources…, Review draft, Show run), the panes' controls
  * (Review exclusions, Admit as r3, Save & run, Export bundle…), and the
  * model's intents where the user's gesture is a navigation (Explain, a
  * crumb). The import dialog's answer and the column mapping are dispatched
  * as the dialog and the mapping pane dispatch them (S5.2, S5.3); the run
  * completes through the window's own fake backend.
  */
class GoldenJourneyFxSuite extends ShellFxSuite:
  import StoryModels.{p17enc03, p17ret07, sigma2}

  override val munitTimeout: Duration = Duration(600, "s")

  private given ExecutionContext = ExecutionContext.global

  private val r2   = StoryMoments.r2
  private val r3   = StoryMoments.r3
  private val rev3 = StoryMoments.rev3
  private val run6 = RunId(6)

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  /** The headless route, run to its end: what the UI route is held to. */
  private def headless(): GoldenRoute.Route =
    val sessions = Await.result(
      (HeadlessSession.open(StoryMoment.T1), HeadlessSession.open(StoryMoment.T2)).tupled,
      60.seconds
    )
    val (s, views) = sessions
    try
      val route = GoldenRoute.Route(s, views)
      Await.result(
        route.scenario.run(StudioDriver.open(StoryModels.firstRun)),
        120.seconds
      ) match
        case Left(failure) => fail(s"the headless route: ${failure.message}")
        case Right(end)    =>
          assertEquals(end.records.collect { case r: DriverRecord.Refused => r }, Vector.empty)
          route
    finally Await.result((s.close, views.close).tupled, 30.seconds): Unit

  /** A project at `root` holding the fixture's history, its inputs stored. */
  private def project(root: Path, document: StudioDocument): SessionPort =
    val owner   = ok(LockOwner.of("GoldenJourneyFxSuite"))
    val sources =
      Vector(SourceRole.Fixations -> "fixations.csv", SourceRole.Trials -> "trials.csv")
    (for
      store  <- FileProjectStore.at[IO](root)
      lock   <- store.acquire(owner).map(ok)
      inputs <- sources.traverse { (role, name) =>
        val bytes = IArray.unsafeFromArray(
          Files.readAllBytes(FixtureDoc.root.resolve(s"fixtures/studio-golden/$name"))
        )
        ProjectBundle.importInput(store, lock, InputKind.Source(role), name, bytes).map(ok)
      }
      _       <- store.release(lock)
      session <- ProjectSession.create(store, owner, document, SharingOptions.complete, inputs)
    yield SessionPort.start(ok(session))).unsafeRunSync()

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
          c.getText != null && c.getText.startsWith(text) && c.isVisible && !c.isDisabled
        )
      found.isDefined
    }
    found.get

  private def press(fx: FxStage, w: StudioWindow, text: String): Unit =
    val b = control(fx, w, text) { case b: Button => b }
    runOnFx(b.fire())
    fx.awaitLayout()

  private def shown(w: StudioWindow): Vector[String] = runOnFx(
    nodes(w.root).collect { case l: Labeled if l.isVisible => Option(l.getText).getOrElse("") }
  )

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
        // Repair…'s file chooser answers as the headless route does: the
        // missing file's restored stand-in, another stimulus's bytes.
        assetFiles = (file, done) =>
          done(
            Right(
              Some(
                (
                  ok(
                    eyes4s.studio.core.assets.AssetFile
                      .of(file.value.stripSuffix(".png") + "_restored.png")
                  ),
                  IArray.unsafeFromArray(
                    Files.readAllBytes(
                      FixtureDoc.root.resolve("fixtures/studio-golden/stimuli/beach-042.png")
                    )
                  )
                )
              )
            )
          )
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
      // The participant rows are fixture.json's: P17's mean D, as FIXTURE.md says.
      eventually(fx, "the summary")(shown(w).exists(_.contains("+0.38")))

      // Figure 3 with the board's panels, and its bundle: Export bundle….
      dispatch(
        fx,
        w,
        Intent.Dispatch(
          Command.CreateFigure(run6, StoryModels.reporting, ok(StoryMoments.figure1).panels)
        )
      )
      val figure = model(w).document.figures.last.id
      dispatch(
        fx,
        w,
        Intent.Navigate(
          Location(Perspective.Figures, Vector(Place.Figures, Place.Figure(figure)))
        )
      )
      press(fx, w, "Export bundle")
      val folder = exportDir.resolve(s"figure-${figure.number}-bundle")
      eventually(fx, "the bundle is written")(Files.isRegularFile(folder.resolve("README.txt")))
      fx.snapshot(StudioTheme.Light)

      // Saved: the window's project folder holds the journey.
      eventually(fx, "the project is saved")(!w.runtime.model.save.edited)
      val ui = model(w).document
      runOnFx(w.close())
      opened -= w

      // --- The UI route against the headless route ------------------------

      val closed = route.closed.getOrElse(fail("the headless route did not close"))
      def science(d: StudioDocument) =
        ok(
          ProjectBundle.encode(d, SharingOptions.complete, BundleSamples.inputsFor(d))
        ).parts.toMap.view
          .mapValues(Vector.from(_))
          .toMap
      assertEquals(science(ui).keySet, science(closed).keySet)
      science(ui).foreach((path, bytes) => assertEquals(bytes, science(closed)(path), path))

      // The export bundle, file for file and byte for byte.
      val written = Files
        .list(folder)
        .iterator
        .asScala
        .map(p => p.getFileName.toString -> Files.readAllBytes(p).toVector)
        .toMap
      assertEquals(written.keySet, route.bundle.keySet)
      assert(written.keySet.exists(_.endsWith(".svg")), written.keySet)
      written.foreach((name, bytes) => assertEquals(bytes, route.bundle(name), name))

      // The project folder: reopened, the same document; each document part
      // it lists is the headless folder's file.
      val reopened = (for
        store  <- FileProjectStore.at[IO](projectDir)
        opened <- ProjectBundle.open(store)
      yield opened).unsafeRunSync().fold(e => fail(e.message), identity)
      assertEquals(science(reopened.document), science(closed))
      reopened.manifest.parts.all.foreach { entry =>
        val path = entry.path.value
        val ours = Files.readAllBytes(projectDir.resolve(path)).toVector
        assertEquals(Some(ours), route.savedFolder.get(path), path)
      }
    finally TempDirs.remove(base)
  }

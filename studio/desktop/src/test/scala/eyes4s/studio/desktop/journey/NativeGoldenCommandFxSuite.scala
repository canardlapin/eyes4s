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

import eyes4s.studio.desktop.harness.NativeArchiveRuns
import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import eyes4s.plan.AdmissionDecision as NativeAdmissionDecision
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.figures.BundleItem
import eyes4s.studio.core.backend.{
  Inspection,
  JobStatus,
  ProvenanceStep,
  QueryStatus,
  ResultAddress
}
import eyes4s.studio.core.artifacts.{
  NativeArtifactBudget,
  NativeArtifactPackage,
  NativeArtifactSource
}
import eyes4s.studio.core.document.RunRef
import java.util.concurrent.atomic.AtomicReference
import eyes4s.studio.core.bundle.{LockOwner, ProjectBundle}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{CoreBinding, Perspective, Preset, RunLifecycle}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.core.headless.NativeHeadlessSession
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, SessionPort}
import javafx.scene.Node
import javafx.scene.control.Button
import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/** First analysis from an empty native window through actual admission,
  * preset and Save & run controls. Import-dialog answers, navigation and
  * report/panel configuration are explicit commands where controls are absent.
  */
class NativeGoldenCommandFxSuite extends GoldenWindow:
  override def munitIgnore: Boolean = NativeArchiveRuns.skipped
  import NativeCommandJourneyFixture.*
  override val munitTimeout: Duration = 600.seconds

  private def timed[A](phase: String)(work: => A): A =
    val started = System.nanoTime()
    try work
    finally
      println(
        s"NativeGoldenCommandFxSuite: $phase in ${(System.nanoTime() - started).nanos.toSeconds}s"
      )

  private def until(fx: FxStage, what: String)(condition: => Boolean): Unit =
    val deadline = System.nanoTime() + 180.seconds.toNanos
    while !runOnFx(condition) do
      if System.nanoTime() > deadline then fail(s"Timed out: $what")
      Thread.sleep(25)
      fx.awaitLayout()

  private def nodes(root: Node): Vector[Node] = root match
    case parent: javafx.scene.Parent =>
      root +: parent.getChildrenUnmodifiable.asScala.toVector.flatMap(nodes)
    case _ => Vector(root)

  fxStage.test(
    "first native analysis admits, saves and runs through the window, then reopens identically"
  ) { fx =>
    assumeFullStage(fx)
    val inputs           = load.unsafeRunSync()
    val initial          = get(AppModel.newProject)
    val (folder, remove) =
      TempDirs.resource("eyes4s-native-command-fx-").allocated.unsafeRunSync()
    val projectDir = folder.resolve("native.eyes")
    val exportDir  = folder.resolve("exports")
    Files.createDirectories(exportDir)
    val (port, release) =
      NativeJourneyProject.open(projectDir, initial.document, inputs).allocated.unsafeRunSync()
    var window: Option[StudioWindow]      = None
    var released                          = false
    var primaryFailure: Option[Throwable] = None
    try
      val w = boot(
        fx,
        initial,
        project = Some(port),
        displays = NavigatorDisplays.stored(port),
        nativeSources = Some(DatasetSourceHosts.stored(port)),
        chooseFolder = (_, _) => Some(exportDir)
      )
      window = Some(w)
      assertEquals(w.session.fixture, None)
      assertEquals(runOnFx(w.runtime.model.document.analyses), Vector.empty)
      // The file chooser's answer supplies these actual stored byte identities.
      dispatch(fx, w, Intent.Dispatch(inputs.importCommand))
      dispatch(
        fx,
        w,
        Intent.Navigate(
          Location(
            Perspective.Data,
            Vector(Place.Dataset(dataset), Place.DataView(DataSection.Admission))
          )
        )
      )
      until(fx, "native admission counts") {
        w.admission.state.counts match
          case Loading.Ready(_) => true
          case _                => false
      }
      runOnFx(w.admission.view.choices(NativeAdmissionDecision.ReviewExclusions).fire())
      until(fx, "admission control enabled") { !w.admission.view.admit.isDisabled }
      runOnFx(w.admission.view.admit.fire())
      until(fx, "actual dataset admission") {
        w.runtime.model.document.dataset(dataset).exists(_.decision.isAdmitted)
      }
      assertEquals(runOnFx(w.runtime.model.document.analyses), Vector.empty)

      dispatch(fx, w, Intent.SwitchPerspective(Perspective.Analysis))
      until(fx, "first-analysis preset control") { w.recipe.enabled(Preset.EncodingRetrieval) }
      runOnFx(w.recipe.choose(Preset.EncodingRetrieval))
      until(fx, "initial working recipe") { w.runtime.model.document.draft.exists(_.isInitial) }
      assertEquals(runOnFx(w.runtime.model.document.analyses), Vector.empty)
      assertEquals(runOnFx(w.runtime.model.document.draftRecipe), Some(recipe))
      until(fx, "native bounded preview and run control") {
        w.resolvedDesign.state.preview.receipt.isDefined && w.preflight.runButton._2
      }
      val receipt = runOnFx(w.resolvedDesign.state.preview.receipt.get)
      assertEquals(receipt.stamp.revision, revision)
      assertEquals(receipt.stamp.dataset, dataset)
      runOnFx(w.preflight.pressRun())
      until(fx, "the requested first run ready") {
        w.runtime.model.jobs.ready.exists(_.run == run)
      }
      assertEquals(runOnFx(w.runtime.model.document.runs.map(_.id)), Vector(run))
      assertEquals(
        runOnFx(w.runtime.model.document.run(run).map(_.state)),
        Some(RunLifecycle.Completed)
      )
      assertEquals(runOnFx(w.runtime.model.document.presentation.shownRun), None)
      assert(w.session.await(w.session.backend.jobs).exists(_.run == run))
      until(fx, "verified native artifacts stored and bound") {
        val document = w.runtime.model.document
        document.run(run).exists(_.archive.isInstanceOf[CoreBinding.Bound[?]]) &&
        document
          .analysis(revision)
          .exists(analysis =>
            analysis.plan == receipt.stamp.plan && analysis.recipe.input.nonEmpty
          )
      }
      assertEquals(runOnFx(w.runtime.model.document.presentation.shownRun), None)

      dispatch(fx, w, Intent.Dispatch(Command.PutReporting(reporting)))
      dispatch(fx, w, Intent.ShowRun(run))
      val sigma = get(ScaleIndex.of(scale))
      dispatch(fx, w, Intent.Explain(Place.At(StudioRef.QueryContrast(run, sigma, focus))))
      val inspection = get(
        w.session.await(w.session.reads.inspect(run, ResultAddress.ContrastRow(scale, focus)))
      )
      inspection match
        case _: Inspection.Contrast => ()
        case other                  => fail(s"Native query not scored: $other")
      dispatch(fx, w, Intent.Dispatch(Command.CreateFigure(run, reporting.id, panels)))
      dispatch(fx, w, Intent.Navigate(Location(Perspective.Figures, Vector(Place.Figures))))
      until(fx, "all edits saved") { !w.runtime.model.save.edited }
      val document = runOnFx(w.runtime.model.document)
      val stored   = timed("warm archive verification") {
        get(port.session.loadNativeArtifacts(run).unsafeRunSync())
      }
      assertEquals(stored.facts.run, run)
      assertEquals(stored.facts.stamp, receipt.stamp)
      assertEquals(
        document.analysis(revision).get.plan,
        CoreBinding.Bound(stored.facts.planCanonical)
      )
      assertEquals(document.analysis(revision).get.recipe.input, Some(stored.facts.source))
      assertEquals(document.run(run).get.archive, CoreBinding.Bound(stored.facts.result))
      val captured = NativeCommandJourneyReadback
        .capture(document, NativeCommandJourneyReadback.Port.from(w.session))
        .unsafeRunSync()
      val commanded = timed("independent command oracle") {
        NativeCommandJourneyScenario
          .run(inputs, folder.resolve("commanded.eyes"))
          .unsafeRunSync()
      }
      assertEquals(captured.canonicalScience, commanded.canonicalScience)
      assertEquals(captured.rows, commanded.rows)
      assertEquals(captured.report, commanded.report)
      assertEquals(captured.source, commanded.source)
      assertEquals(captured.sourceRef, commanded.sourceRef)
      assertEquals(captured.exports, commanded.exports)
      assertEquals(captured.summary.run, run)
      assertEquals(captured.rows.size, 480)
      assert(
        captured.rows.exists(q =>
          q.query == focus && q.status.isInstanceOf[QueryStatus.Contributing]
        )
      )
      assert(captured.exports.exists(_._1.endsWith(".svg")))
      assert(captured.exports.exists(_._1 == "participants.csv"))
      until(fx, "all native scientific bundle items available") {
        w.figures.vm.bundle.exists(b => BundleItem.Default.subsetOf(b.written.toSet))
      }
      val exportButton = runOnFx(
        nodes(w.root)
          .collectFirst {
            case button: Button if button.getText == "Export bundle…" && !button.isDisabled =>
              button
          }
          .getOrElse(fail("No enabled export-bundle control"))
      )
      runOnFx(exportButton.fire())
      val written = exportDir.resolve("figure-1-bundle")
      until(fx, "native export bundle written") {
        Files.isRegularFile(written.resolve("README.txt"))
      }
      captured.exports.foreach { (name, bytes) =>
        assertEquals(Files.readAllBytes(written.resolve(name)).toVector, bytes, name)
      }
      runOnFx(w.close())
      opened -= w
      window = None
      release.unsafeRunSync()
      released = true
      val reopened = get((for
        store   <- FileProjectStore.at[IO](projectDir)
        project <- ProjectBundle.open(store)
      yield project).unsafeRunSync())
      assertEquals(reopened.document, document)
      assert(reopened.science.verified)
      val reopenedStore   = FileProjectStore.at[IO](projectDir).unsafeRunSync()
      val reopenedSession = get(
        ProjectSession
          .open(reopenedStore, get(LockOwner.of("native artifact cold verification")))
          .unsafeRunSync()
      ).session
      try
        val loaded = new AtomicReference[Option[NativeArtifactPackage]](None)
        timed("cold backend restoration") {
          Resource
            .make(IO.blocking(SessionPort.start(reopenedSession)))(port =>
              IO.blocking(port.close())
            )
            .use { port =>
              val observedSource = new NativeArtifactSource[IO]:
                def load(ref: RunRef, budget: NativeArtifactBudget) =
                  port.nativeArtifactSource.load(ref, budget).map { result =>
                    result.foreach(loaded.set)
                    result
                  }
              Resource
                .make(
                  IO.fromFuture(
                    IO(
                      NativeHeadlessSession.open(
                        reopened.document,
                        DatasetSourceHosts.stored(port),
                        artifactSource = Some(observedSource)
                      )
                    )
                  )
                )(session => IO.fromFuture(IO(session.close)))
                .use { session =>
                  for
                    restored <- NativeCommandJourneyReadback.capture(
                      reopened.document,
                      NativeCommandJourneyReadback.Port.from(session)
                    )
                    jobs <- session.rawBackend.jobs
                  yield
                    val cold =
                      loaded.get().getOrElse(fail("Cold backend did not load the archive"))
                    assertEquals(cold.facts, stored.facts)
                    assertEquals(cold.archive.index, stored.archive.index)
                    cold.archive.files.zip(stored.archive.files).foreach {
                      case ((entry, bytes), (firstEntry, firstBytes)) =>
                        assertEquals(entry, firstEntry)
                        assertEquals(
                          Vector.from(bytes),
                          Vector.from(firstBytes),
                          entry.name.value
                        )
                    }
                    assertEquals(restored.canonicalScience, captured.canonicalScience)
                    assertEquals(restored.rows, captured.rows)
                    assertEquals(restored.report, captured.report)
                    assertEquals(restored.source, captured.source)
                    assertEquals(restored.exports, captured.exports)
                    assertEquals(jobs, Vector.empty[JobStatus])
                    assert(
                      restored.provenance.trail
                        .contains(ProvenanceStep.Restored(cold.manifestAddress))
                    )
                    assert(
                      !restored.provenance.trail
                        .exists(_.isInstanceOf[ProvenanceStep.Recomputed])
                    )
                }
            }
            .unsafeRunSync()
        }
      finally get(reopenedSession.close.unsafeRunSync())
    catch
      case NonFatal(error) =>
        primaryFailure = Some(error)
        throw error
    finally
      val errors                         = Vector.newBuilder[Throwable]
      def cleanup(action: => Unit): Unit =
        try action
        catch case NonFatal(error) => errors += error: Unit
      cleanup(window.foreach(w => { runOnFx(w.close()); opened -= w }))
      cleanup(if !released then release.unsafeRunSync())
      cleanup(remove.unsafeRunSync())
      val failures = errors.result()
      primaryFailure match
        case Some(error) => failures.foreach(error.addSuppressed)
        case None        =>
          failures.headOption.foreach { error =>
            failures.tail.foreach(error.addSuppressed)
            throw error
          }
  }

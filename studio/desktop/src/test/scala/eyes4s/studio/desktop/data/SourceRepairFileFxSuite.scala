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

package eyes4s.studio.desktop.data

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.codec.ByteDigest
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.assets.{AssetFile, SourceState}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.platform.{Dialogs as HostDialogs, FileRequest, HostPath}
import eyes4s.studio.desktop.platform.{DesktopPlatform, FileProjectStore, TempDirs}
import eyes4s.studio.desktop.runtime.DatasetSourceHosts
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.trial.StimulusSource
import javafx.scene.control.Button
import javafx.scene.layout.StackPane
import eyes4s.studio.desktop.trial.{TrialView, TrialViewStatus}
import eyes4s.studio.desktop.plot.PlotHostStatus
import eyes4s.studio.app.tokens.{Theme, StageVariant}
import eyes4s.studio.viz.plot.DataPoint
import eyes4s.studio.viz.trial.{FrameArt, MarkStyle, TrialSceneInput}
import scala.jdk.CollectionConverters.*
import java.nio.file.Files
import scala.concurrent.duration.*

/** A native window reads and repairs actual on-disk project files through its Sources controls. */
class SourceRepairFileFxSuite extends ShellFxSuite:
  import SourceRepairFileFixture.*
  override val munitTimeout: Duration = 180.seconds

  private final class Choices extends HostDialogs[IO]:
    @volatile var next: Option[HostPath]                       = None
    @volatile var requested                                    = Vector.empty[FileRequest]
    def chooseOpen(request: FileRequest): IO[Option[HostPath]] = IO {
      requested :+= request
      next
    }
    def chooseSave(request: FileRequest): IO[Option[HostPath]] = IO.pure(None)
    def chooseDirectory(title: String): IO[Option[HostPath]]   = IO.pure(None)

  private def sourceButton(w: eyes4s.studio.desktop.StudioWindow): Button = runOnFx {
    w.sources.view.sources
      .lookupAll(".button")
      .asScala
      .collectFirst {
        case b: Button if b.getAccessibleText == "Repair fixations.csv…" => b
      }
      .getOrElse(fail("missing source repair button"))
  }

  fxStage.test(
    "real source Repair cancels safely, restores exact bytes, and creates a pending replacement"
  ) { fx =>
    val (directory, remove) =
      TempDirs.resource("eyes4s-repair-native-fx-").allocated.unsafeRunSync()
    val root            = directory.resolve("native.eyes")
    val (port, release) = open(root).allocated.unsafeRunSync()
    try
      val ready    = working(port, dataset).unsafeRunSync()
      val original = directory.resolve("original.csv")
      val altered  = directory.resolve("replacement.csv")
      Files.write(original, Array.from(fixations))
      Files.write(altered, Array.from(replacement))
      Files.write(address(root, source(ready)), Array.from(replacement))
      val choices  = Choices()
      val platform = DesktopPlatform.create(_ => (), () => None).copy(dialogs = choices)
      val files    = AssetFiles.onPlatform(
        platform,
        path => Right(java.nio.file.Path.of(path.value).getFileName.toString)
      )
      val store = FileProjectStore.at[IO](root).unsafeRunSync()
      val w     = boot(
        fx,
        AppModel.open(ready, None),
        project = Some(port),
        assetFiles = files,
        displays = NavigatorDisplays.stored(port),
        stimuli = StimulusSource.bundle(store),
        nativeSources = Some(DatasetSourceHosts.stored(port))
      )
      eventually(fx, "same-path source detected by the real project check") {
        w.runtime.model.sources
          .of(dataset)
          .exists(_.state == SourceState.Changed(ByteDigest.sha256(replacement)))
      }
      assert(runOnFx(w.runtime.model.runBlock.isDefined))
      runOnFx(w.runtime.dispatch(Intent.SwitchPerspective(Perspective.Analysis)))
      eventually(fx, "Analysis refuses the changed source")(
        w.resolvedDesign.state.blocked.isDefined
      )
      runOnFx(w.runtime.dispatch(Intent.SwitchPerspective(Perspective.Data)))
      assert(!runOnFx(w.preflight.runButton._2))
      val before = runOnFx(w.runtime.model.document)
      choices.next = None
      runOnFx(sourceButton(w).fire())
      eventually(fx, "cancelled file dialog answered")(choices.requested.nonEmpty)
      fx.awaitLayout()
      assertEquals(runOnFx(w.runtime.model.document), before)
      assertEquals(
        Files.readAllBytes(address(root, source(ready))).toVector,
        Array.from(replacement).toVector
      )
      choices.next = Some(get(HostPath.of(original.toString)))
      runOnFx(sourceButton(w).fire())
      eventually(fx, "exact original source restored") {
        w.runtime.model.sources.of(dataset).forall(_.state == SourceState.Present)
      }
      assertEquals(runOnFx(w.runtime.model.document.science), before.science)
      assertEquals(runOnFx(w.runtime.model.runBlock), None)
      Files.write(address(root, source(ready)), Array.from(replacement))
      runOnFx(w.runtime.dispatch(Intent.CheckInputs))
      eventually(fx, "source changed again")(w.runtime.model.runBlock.isDefined)
      choices.next = Some(get(HostPath.of(altered.toString)))
      runOnFx(sourceButton(w).fire())
      eventually(fx, "replacement is a new pending revision") {
        w.runtime.model.document.datasets.size == 2
      }
      val next = runOnFx(w.runtime.model.document.datasets.last)
      assertEquals(next.parent, Some(dataset))
      assertEquals(next.decision, AdmissionDecision.Pending)
      assertEquals(next.sources.fixations.map(_.bytes), Some(ByteDigest.sha256(replacement)))
      assertEquals(runOnFx(w.runtime.model.document.datasets.head), ready.datasets.head)
      assert(runOnFx(w.sources.view.note.getText.contains("must be admitted")))
      runOnFx(w.close())
      opened -= w
      port.session.save.map(get).unsafeRunSync()
    finally
      release.unsafeRunSync()
      remove.unsafeRunSync()
  }

  fxStage.test("a real stored missing image is hatched while a blank display remains plain") {
    fx =>
      val (directory, remove) =
        TempDirs.resource("eyes4s-repair-assets-fx-").allocated.unsafeRunSync()
      val root            = directory.resolve("images.eyes")
      val (port, release) = open(root).allocated.unsafeRunSync()
      try
        val document = port.session.document.unsafeRunSync()
        val spec     = document.datasets.head
        val registry = DatasetSourceHosts
          .stored(port)
          .assets(spec)
          .unsafeRunSync()
          .getOrElse(fail("missing stored registry"))
        val missing = registry.display(trial).getOrElse(fail("missing trial"))
        val plain   = registry.display(blank).getOrElse(fail("blank trial"))
        val store   = FileProjectStore.at[IO](root).unsafeRunSync()
        val view    = runOnFx(TrialView(StimulusSource.bundle(store)))
        try
          fx.show(runOnFx(StackPane(view)))
          def show(display: eyes4s.studio.core.assets.TrialDisplay): (FrameArt, Vector[Int]) =
            runOnFx(
              view.show(
                TrialSceneInput(
                  display,
                  spec.geometry.screen,
                  Vector.empty,
                  MarkStyle.Neutral,
                  Theme.Light,
                  StageVariant.Dark
                )
              )
            )
            fx.awaitLayout()
            eventually(fx, "real file-backed trial frame drawn") {
              (view.status.get, view.plotHost.status.get) match
                case (TrialViewStatus.Shown(scene), PlotHostStatus.Drawn(frame)) =>
                  (frame.plan.scene eq scene.plot.scene) &&
                  frame.surface.logicalWidth == view.plotHost.getWidth &&
                  frame.surface.logicalHeight == view.plotHost.getHeight
                case _ => false
            }
            runOnFx {
              val scene = view.status.get match
                case TrialViewStatus.Shown(scene) => scene
                case other                        => fail(s"not shown: $other")
              val frame = view.plotHost.status.get match
                case PlotHostStatus.Drawn(frame) => frame
                case other                       => fail(s"not drawn: $other")
              val image   = view.plotHost.snapshot(null, null)
              val colours = (464 until 1464 by 16).map { x =>
                val point = get(frame.transform.dataToCanvas(DataPoint(x, 300)))
                image.getPixelReader.getArgb(point.x.toInt, point.y.toInt)
              }.toVector
              (scene.frameArt, colours)
            }
          val (art, hatched)          = show(missing)
          val (blankArt, plainPixels) = show(plain)
          assertEquals(art, FrameArt.Missing(get(AssetFile.of("missing.png"))))
          assertEquals(blankArt, FrameArt.Screen)
          assert(hatched.distinct.size > 1, "missing image must actually render hatch stripes")
          assertEquals(plainPixels.distinct.size, 1, "blank image frame stays plain")
          assertNotEquals(hatched, plainPixels)
          assertEquals(port.session.document.unsafeRunSync().science, document.science)
        finally runOnFx(view.dispose())
      finally
        release.unsafeRunSync()
        remove.unsafeRunSync()
  }

  fxStage.test(
    "Repair image stores verified bytes and relink provenance through file-backed reopen"
  ) { fx =>
    val (directory, remove) =
      TempDirs.resource("eyes4s-repair-image-fx-").allocated.unsafeRunSync()
    val root            = directory.resolve("image-repair.eyes")
    val (port, release) = open(root).allocated.unsafeRunSync()
    val picture         = Files.readAllBytes(
      eyes4s.studio.desktop.trial.GoldenTrials.stimuli.resolve("beach-042.png")
    )
    val chosen = directory.resolve("found.png")
    Files.write(chosen, picture)
    var before: Option[StudioDocument] = None
    try
      val document = port.session.document.unsafeRunSync()
      before = Some(document)
      val choices = Choices()
      choices.next = Some(get(HostPath.of(chosen.toString)))
      val platform = DesktopPlatform.create(_ => (), () => None).copy(dialogs = choices)
      val adapter  = AssetFiles.onPlatform(
        platform,
        path => Right(java.nio.file.Path.of(path.value).getFileName.toString)
      )
      val w = boot(
        fx,
        AppModel.open(document, None),
        project = Some(port),
        assetFiles = adapter,
        displays = NavigatorDisplays.stored(port)
      )
      eventually(fx, "stored missing image repair control")(w.sources.vm.missing.isDefined)
      runOnFx(w.sources.view.repair.fire())
      eventually(fx, "hashed image relink is recorded") {
        w.runtime.model.document.relinks
          .of(dataset)
          .headOption
          .exists(_.asset.file.value == "found.png")
      }
      val repaired = runOnFx(w.runtime.model.document)
      assertEquals(repaired.science, document.science)
      assertEquals(
        repaired.relinks.of(dataset).head.asset.sha256,
        ByteDigest.sha256(IArray.from(picture))
      )
      eventually(fx, "registry sees actual stored repaired image")(w.sources.vm.missing.isEmpty)
      runOnFx(w.close())
      opened -= w
      port.session.save.map(get).unsafeRunSync()
    finally release.unsafeRunSync()
    try
      reopen(root)
        .use { reopened =>
          for
            document <- reopened.session.document
            registry <- DatasetSourceHosts.stored(reopened).assets(document.datasets.head)
          yield
            assertEquals(
              document.science,
              before.getOrElse(fail("no original science")).science
            )
            assertEquals(document.relinks.of(dataset).head.asset.file.value, "found.png")
            val served = get(
              registry
                .getOrElse(fail("reopened registry missing"))
                .withRelinks(document.relinks.of(dataset))
            )
            assert(
              served
                .display(trial)
                .exists(_.state.isInstanceOf[eyes4s.studio.core.assets.DisplayState.Image])
            )
            assertEquals(
              served.display(blank).map(_.state),
              Some(eyes4s.studio.core.assets.DisplayState.Blank)
            )
        }
        .unsafeRunSync()
    finally remove.unsafeRunSync()
  }

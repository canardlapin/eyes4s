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
import eyes4s.studio.app.{AppModel, Intent, PlatformDialog}
import eyes4s.studio.app.keys.CommandRegistry
import eyes4s.studio.core.fixture.ExampleProject
import eyes4s.studio.core.platform.*
import eyes4s.studio.desktop.platform.{BundledExample, FilePresetStore, JvmFileSystem, TempDirs}
import eyes4s.studio.desktop.project.ProjectLifecycleHost
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.stage.Window
import javafx.scene.control.Label
import scala.jdk.CollectionConverters.*

class DataFirstRunFxSuite extends ShellFxSuite:
  private def get[E, A](answer: Either[E, A]): A = answer.fold(e => fail(e.toString), identity)
  private def path(p: java.nio.file.Path): HostPath = get(HostPath.of(p.toString))
  private def shown = Window.getWindows.asScala.filter(_.isShowing).toVector

  fxStage.test(
    "DataEmpty draws its required/recommended inputs, presets, checklist and both themes"
  ) { fx =>
    val w = boot(fx, get(AppModel.newProject))
    runOnFx {
      assert(w.root.lookup("#data-first-run") != null)
      assert(w.root.lookup("#data-first-run-checklist") != null)
      assertEquals(w.firstRun.presets.node.getChildren.asScala.count(_.isFocusTraversable), 1)
      assert(w.firstRun.fixations.getText.contains("fixations.csv"))
      assert(w.firstRun.trials.getText.contains("trials.csv"))
      assertEquals(w.shell.statusBar.saved.getText, "Not saved")
    }
    fx.snapshot(StudioTheme.Light)
    runOnFx(w.runtime.dispatch(Intent.Invoke(CommandRegistry.appearanceDark.id)))
    fx.awaitLayout()
    assertEquals(runOnFx(w.runtime.model.notice), None)
    assert(runOnFx(w.runtime.model.save.edited))
    fx.snapshot(StudioTheme.Dark)
  }

  fxStage.test("E2E02 first run opens a verified independent classpath example copy") { fx =>
    TempDirs
      .resource("eyes4s-first-example-")
      .use { dir =>
        IO.blocking {
          val memory   = InMemoryPlatform.create[IO]().unsafeRunSync()
          val platform = memory.platform.copy(files = JvmFileSystem)
          val host     = runOnFx(
            ProjectLifecycleHost(
              fx.stage,
              platform,
              presets = FilePresetStore(dir.resolve("presets"))
            )
          )
          try
            runOnFx(host.start())
            val target = dir.resolve("Example.eyes")
            memory.answer(Some(path(target))).unsafeRunSync()
            runOnFx(host.window.get.firstRun.example.fire())
            eventually(fx, "example copied and installed")(
              !host.busy && host.window.exists(_.project.isDefined)
            )
            runOnFx {
              assertEquals(
                host.window.get.runtime.model.document.datasets.size,
                ExampleProject.document.toOption.get.datasets.size
              )
              assert(host.window.get.runtime.model.document.datasets.nonEmpty)
              assertEquals(host.window.get.runtime.model.save.edited, false)
            }
            val inputs = java.util.concurrent
              .CompletableFuture[Either[String, Vector[eyes4s.studio.core.bundle.InputEntry]]]()
            runOnFx(host.window.get.project.get.storedInputs(answer => {
              inputs.complete(answer); ()
            }))
            assertEquals(get(inputs.get(10, java.util.concurrent.TimeUnit.SECONDS)).size, 259)
            val source = get(BundledExample.source.unsafeRunSync())
            val opened =
              get(eyes4s.studio.core.bundle.ProjectBundle.open(source).unsafeRunSync())
            assertEquals(opened.document, ExampleProject.document.toOption.get)
            assert(java.nio.file.Files.isRegularFile(target.resolve("project.json")))
            runOnFx(host.request(eyes4s.studio.app.project.ProjectOperation.Close))
            eventually(fx, "example closed to first run")(
              !host.busy && host.window.exists(_.project.isEmpty)
            )
            val recent = runOnFx(
              host.window.get.firstRun.node
                .lookupAll(".button")
                .asScala
                .collectFirst {
                  case button: javafx.scene.control.Button
                      if button.getText == target.toString =>
                    button
                }
                .getOrElse(fail("first-run recent project missing"))
            )
            runOnFx(recent.fire())
            eventually(fx, "first-run recent project reopened")(
              !host.busy && host.window.exists(_.project.isDefined)
            )

          finally runOnFx(host.shutdown())
        }
      }
      .unsafeRunSync()
  }

  fxStage.test(
    "canceling first import leaves untitled untouched; dropped tables retain both roles"
  ) { fx =>
    TempDirs
      .resource("eyes4s-first-import-")
      .use { dir =>
        IO.blocking {
          val memory = InMemoryPlatform.create[IO]().unsafeRunSync()
          val host   = runOnFx(
            ProjectLifecycleHost(
              fx.stage,
              memory.platform.copy(files = JvmFileSystem),
              presets = FilePresetStore(dir.resolve("presets"))
            )
          )
          try
            runOnFx(host.start())
            val initial = runOnFx(host.window.get)
            memory.answer(None).unsafeRunSync()
            runOnFx(initial.firstRun.fixations.fire())
            eventually(fx, "creation canceled")(!host.busy)
            runOnFx {
              assert(host.window.exists(_ eq initial))
              assert(initial.project.isEmpty)
            }
            val fixations = dir.resolve("fixations.csv")
            val trials    = dir.resolve("trials.csv")
            java.nio.file.Files.writeString(
              fixations,
              "participant,phase,trial,x,y,onset,duration\np1,encoding,t1,10,20,0,100\n"
            )
            java.nio.file.Files
              .writeString(trials, "participant,phase,trial,item\np1,encoding,t1,item1\n")
            memory.answer(Some(path(dir.resolve("Import.eyes")))).unsafeRunSync()
            runOnFx(assert(initial.firstRun.dropFiles(Vector(fixations.toFile, trials.toFile))))
            eventually(fx, "new project import wizard")(
              !host.busy && host.window.exists(_.project.isDefined) && shown.size >= 2
            )
            eventually(fx, "both chosen source names in actual wizard") {
              val text = shown
                .flatMap(w =>
                  Option(w.getScene).toVector.flatMap(
                    _.getRoot.lookupAll(".label").asScala.collect { case l: Label => l.getText }
                  )
                )
                .mkString("\n")

              text.contains("trials.csv") && text.contains("fixations.csv")
            }
            runOnFx {
              assert(host.window.get.runtime.model.document.datasets.isEmpty)
              host.shutdown()
              assertEquals(shown.size, 1)
            }
          finally runOnFx(host.shutdown())
        }
      }
      .unsafeRunSync()
  }

  fxStage.test(
    "ambiguous or unsupported drops refuse the whole set without starting a lifecycle"
  ) { _ =>
    val requests = scala.collection.mutable.ArrayBuffer.empty[PlatformDialog]
    val view     = runOnFx(DataFirstRunView(dialog => { requests += dialog; () }))
    runOnFx {
      assert(!view.dropFiles(Vector(java.io.File("a.csv"), java.io.File("b.csv"))))
      assert(!view.dropFiles(Vector(java.io.File("fixations.csv"), java.io.File("notes.pdf"))))
      assert(!view.dropFiles(Vector.empty))
      assert(requests.isEmpty)
      assert(view.notice.getText.nonEmpty)
    }
  }

  fxStage.test(
    "the first-run stimulus folder stores validated image bytes under the filename"
  ) { fx =>
    TempDirs
      .resource("eyes4s-first-images-")
      .use { dir =>
        IO.blocking {
          val memory = InMemoryPlatform.create[IO]().unsafeRunSync()
          val host   = runOnFx(
            ProjectLifecycleHost(
              fx.stage,
              memory.platform.copy(files = JvmFileSystem),
              presets = FilePresetStore(dir.resolve("presets"))
            )
          )
          val folder = java.nio.file.Files.createDirectories(dir.resolve("images"))
          val image  =
            java.awt.image.BufferedImage(2, 2, java.awt.image.BufferedImage.TYPE_INT_RGB)
          assert(javax.imageio.ImageIO.write(image, "png", folder.resolve("tiny.png").toFile))
          try
            runOnFx(host.start())
            memory.answer(Some(path(dir.resolve("Images.eyes")))).unsafeRunSync()
            memory.answer(Some(path(folder))).unsafeRunSync()
            runOnFx(host.window.get.firstRun.images.fire())
            eventually(fx, "image folder stored") {
              !host.busy && shown
                .flatMap(w =>
                  Option(w.getScene).toVector.flatMap(
                    _.getRoot.lookupAll(".label").asScala.collect { case l: Label => l.getText }
                  )
                )
                .exists(_.contains("Stored 1 stimulus images"))
            }
            val inputs = java.util.concurrent
              .CompletableFuture[Either[String, Vector[eyes4s.studio.core.bundle.InputEntry]]]()
            runOnFx(host.window.get.project.get.storedInputs(answer => {
              inputs.complete(answer); ()
            }))
            val entries = get(inputs.get(10, java.util.concurrent.TimeUnit.SECONDS))
            assertEquals(entries.map(_.name), Vector(Some("tiny.png")))
            assertEquals(
              entries.map(_.kind),
              Vector(eyes4s.studio.core.bundle.InputKind.StimulusImage)
            )
            assert(runOnFx(host.window.get.runtime.model.document.datasets.isEmpty))
          finally runOnFx(host.shutdown())
        }
      }
      .unsafeRunSync()
  }

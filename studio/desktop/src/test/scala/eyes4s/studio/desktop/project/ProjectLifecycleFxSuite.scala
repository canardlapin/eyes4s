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

package eyes4s.studio.desktop.project

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.{AppModel, Intent, ProjectName}
import eyes4s.studio.app.keys.{CommandId, CommandRegistry}
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.Theme
import eyes4s.studio.core.platform.*
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.platform.{
  FilePresetStore,
  FileProjectStore,
  JvmFileSystem,
  TempDirs
}
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.event.Event
import javafx.scene.control.{Button, Menu, MenuItem}
import javafx.stage.{Window, WindowEvent}
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Real bundle bytes and actual File menu commands; only chooser answers are scripted. */
class ProjectLifecycleFxSuite extends ShellFxSuite:
  override val munitTimeout: Duration           = 180.seconds
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def path(value: Path): HostPath       = get(HostPath.of(value.toString))

  private def invoke(host: ProjectLifecycleHost, id: CommandId): Unit = runOnFx {
    val window = host.window.getOrElse(fail("no current window"))
    def flatten(item: MenuItem): Vector[MenuItem] = item match
      case menu: Menu => menu.getItems.asScala.toVector.flatMap(flatten)
      case item       => Vector(item)
    val item = window.shell.menus
      .flatMap(_.getItems.asScala)
      .flatMap(flatten)
      .find(_.getId == id.value)
      .getOrElse(fail(s"no File menu command ${id.value}"))
    assert(!item.isDisable)
    item.fire()
  }

  private def press(fx: FxStage, text: String): Unit =
    def button = runOnFx(
      Window.getWindows.asScala.toVector
        .flatMap(w =>
          Option(w.getScene).toVector.flatMap(_.getRoot.lookupAll(".button").asScala.collect {
            case b: Button if b.getText == text && b.isVisible => b
          })
        )
        .headOption
    )
    eventually(fx, s"dialog button $text")(button.isDefined)
    runOnFx(button.get.fire())

  private def create(directory: Path): Unit =
    val store   = FileProjectStore.at[IO](directory).unsafeRunSync()
    val session = get(
      ProjectSession
        .create[IO](
          store,
          get(LockOwner.of("lifecycle fixture")),
          get(AppModel.newProject).document,
          SharingOptions.complete,
          Vector.empty
        )
        .unsafeRunSync()
    )
    get(session.close.unsafeRunSync())

  private def usable(directory: Path): Boolean =
    val store = FileProjectStore.at[IO](directory).unsafeRunSync()
    ProjectSession
      .open[IO](store, get(LockOwner.of("lifecycle lock probe")))
      .unsafeRunSync() match
      case Left(_)       => false
      case Right(opened) => get(opened.session.close.unsafeRunSync()); true

  private def setup(
      fx: FxStage,
      directory: Path,
      files: FileSystem[IO] = JvmFileSystem
  ): (InMemoryPlatform[IO], ProjectLifecycleHost) =
    val memory   = InMemoryPlatform.create[IO]().unsafeRunSync()
    val platform = memory.platform.copy(files = files)
    val host     = runOnFx(
      ProjectLifecycleHost(
        fx.stage,
        platform,
        presets = FilePresetStore(directory.resolve("presets"))
      )
    )
    runOnFx(host.start())
    (memory, host)

  fxStage.test(
    "File New/Close/Open create and reopen a real native bundle, with current titles"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-")
      .use { directory =>
        IO.blocking {
          val project          = directory.resolve("study.eyes")
          val (platform, host) = setup(fx, directory)
          try
            assertEquals(runOnFx(fx.stage.getTitle), "Untitled project")
            assert(runOnFx(host.window.exists(_.session.fixture.isEmpty)))
            platform.answer(Some(path(project))).unsafeRunSync()
            invoke(host, CommandRegistry.newProject.id)
            eventually(fx, "named native project opened")(
              !host.busy && host.window
                .exists(_.runtime.model.project.exists(_.value == "study"))
            )
            val first = runOnFx(host.window.get)
            assert(Files.isRegularFile(project.resolve(ProjectStore.ManifestName)))
            assertEquals(runOnFx(fx.stage.getTitle), "study.eyes")
            runOnFx(
              first.runtime.dispatch(
                Intent.Dispatch(eyes4s.studio.core.command.Command.SetTheme(Theme.Dark))
              )
            )
            eventually(fx, "theme saved")(!first.runtime.model.save.edited)
            invoke(host, CommandRegistry.closeProject.id)
            eventually(fx, "project closed to untitled")(
              !host.busy && host.window.exists(_.runtime.model.project.isEmpty)
            )
            assert(runOnFx(first.isClosed))
            assert(usable(project), "Close must release the writer before returning")
            platform.answer(Some(path(project))).unsafeRunSync()
            invoke(host, CommandRegistry.openProject.id)
            eventually(fx, "saved project reopened")(
              !host.busy && host.window
                .exists(_.runtime.model.project.exists(_.value == "study"))
            )
            assertEquals(
              runOnFx(host.window.get.runtime.model.document.presentation.theme),
              Theme.Dark
            )
            assertEquals(runOnFx(fx.stage.getTitle), "study.eyes")
          finally runOnFx(host.shutdown())
          assert(usable(project))
        }
      }
      .unsafeRunSync()
  }

  fxStage.test("cancelled choosers and refused open preserve the current window and title") {
    fx =>
      TempDirs
        .resource("eyes4s-lifecycle-cancel-")
        .use { directory =>
          IO.blocking {
            val (platform, host) = setup(fx, directory)
            val original         = runOnFx(host.window.get)
            try
              for command <- Vector(CommandRegistry.newProject, CommandRegistry.openProject) do
                platform.answer(None).unsafeRunSync()
                invoke(host, command.id)
                eventually(fx, "chooser cancelled")(!host.busy)
                assertEquals(runOnFx(host.window), Some(original))
                assert(!runOnFx(original.isClosed))
              platform.answer(Some(path(directory.resolve("missing.eyes")))).unsafeRunSync()
              invoke(host, CommandRegistry.openProject.id)
              press(fx, "OK")
              assertEquals(runOnFx(host.window), Some(original))
              assertEquals(runOnFx(fx.stage.getTitle), "Untitled project")
              assert(!runOnFx(original.isClosed))
            finally runOnFx(host.shutdown())
          }
        }
        .unsafeRunSync()
  }

  fxStage.test(
    "dirty File Close and native close request both keep resources on cancellation"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-unsaved-")
      .use { directory =>
        IO.blocking {
          val (_, host) = setup(fx, directory)
          val original  = runOnFx(host.window.get)
          try
            runOnFx(
              original.runtime.dispatch(Intent.RenameProject(get(ProjectName.of("unsaved"))))
            )
            assert(runOnFx(fx.stage.getTitle.endsWith(" — Edited")))
            invoke(host, CommandRegistry.closeProject.id)
            press(fx, "Keep open")
            assertEquals(runOnFx(host.window), Some(original))
            assert(!runOnFx(original.isClosed))
            runOnFx(
              Event.fireEvent(fx.stage, WindowEvent(fx.stage, WindowEvent.WINDOW_CLOSE_REQUEST))
            )
            press(fx, "Keep open")
            assertEquals(runOnFx(host.window), Some(original))
            assert(!runOnFx(original.isClosed))
            assert(runOnFx(fx.stage.isShowing))
            invoke(host, CommandRegistry.closeProject.id)
            press(fx, "Close without saving")
            eventually(fx, "untitled replacement after admitted close")(
              !host.busy && host.window.exists(_.runtime.model.project.isEmpty)
            )
            assert(runOnFx(original.isClosed))
          finally runOnFx(host.shutdown())
        }
      }
      .unsafeRunSync()
  }

  fxStage.test("cancelled replacement releases only the staged real project's writer") { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-staged-")
      .use { directory =>
        IO.blocking {
          val target = directory.resolve("target.eyes")
          create(target)
          val (platform, host) = setup(fx, directory)
          val original         = runOnFx(host.window.get)
          try
            runOnFx(
              original.runtime.dispatch(Intent.RenameProject(get(ProjectName.of("unsaved"))))
            )
            platform.answer(Some(path(target))).unsafeRunSync()
            invoke(host, CommandRegistry.openProject.id)
            press(fx, "Keep open")
            assertEquals(runOnFx(host.window), Some(original))
            assert(!runOnFx(original.isClosed))
            val deadline = System.nanoTime() + 10.seconds.toNanos
            while !usable(target) && System.nanoTime() < deadline do Thread.sleep(10)
            assert(usable(target), "cancelled candidate must release its native writer")
          finally runOnFx(host.shutdown())
        }
      }
      .unsafeRunSync()
  }

  fxStage.test(
    "acquisition finishing after shutdown releases its project even without a queued FX completion"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-late-")
      .use { directory =>
        IO.blocking {
          val target = directory.resolve("late.eyes")
          create(target)
          val entered  = Deferred[IO, Unit].unsafeRunSync()
          val finish   = Deferred[IO, Unit].unsafeRunSync()
          val releases = AtomicInteger(0)
          val files    = new FileSystem[IO]:
            export JvmFileSystem.{child, read, readStream, write, list}
            def project(p: HostPath): IO[Either[PlatformError, ProjectStore[IO]]] =
              JvmFileSystem
                .project(p)
                .map(_.map { delegate =>
                  new ProjectStore[IO]:
                    export delegate.{
                      read,
                      list,
                      write,
                      delete,
                      swapManifest,
                      acquire,
                      readSidecar,
                      appendSidecar,
                      replaceSidecar,
                      removeSidecar
                    }
                    def readManifest = delegate.readManifest
                      .flatTap(_ => entered.complete(()).void >> finish.get)
                    def release(lock: WriterLock) =
                      IO(releases.incrementAndGet()).flatMap(_ => delegate.release(lock))
                })
          val (platform, host) = setup(fx, directory, files)
          try
            platform.answer(Some(path(target))).unsafeRunSync()
            invoke(host, CommandRegistry.openProject.id)
            entered.get.timeout(10.seconds).unsafeRunSync()
            runOnFx(host.shutdown())
            finish.complete(()).unsafeRunSync()
            val deadline = System.nanoTime() + 10.seconds.toNanos
            while releases.get == 0 && System.nanoTime() < deadline do Thread.sleep(10)
            assertEquals(releases.get, 1)
            assert(usable(target))
            assertEquals(runOnFx(host.window), None)
          finally
            finish.complete(()).unsafeRunSync()
            runOnFx(host.shutdown())
        }
      }
      .unsafeRunSync()
  }

  fxStage.test(
    "active-work close admission can be cancelled without disposing the native window"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-active-")
      .use { directory =>
        IO.blocking {
          val (_, host) = setup(fx, directory)
          val original  = runOnFx(host.window.get)
          try
            val work =
              eyes4s.studio.app.StoryModels.t3Summary.jobs.jobs.filterNot(_.phase.isTerminal)
            assert(work.nonEmpty)
            runOnFx(original.runtime.dispatch(Intent.JobsChanged(work)))
            invoke(host, CommandRegistry.closeProject.id)
            press(fx, "Keep open")
            assertEquals(runOnFx(host.window), Some(original))
            assert(!runOnFx(original.isClosed))
            assertEquals(runOnFx(original.runtime.model.jobs.jobs), work)
          finally runOnFx(host.shutdown())
        }
      }
      .unsafeRunSync()
  }

  fxStage.test("real autosave recovery is unchanged on cancel and restored through File Open") {
    fx =>
      TempDirs
        .resource("eyes4s-lifecycle-recovery-")
        .use { directory =>
          IO.blocking {
            val target = directory.resolve("recovery.eyes")
            create(target)
            val store  = FileProjectStore.at[IO](target).unsafeRunSync()
            val writer = get(
              ProjectSession.open[IO](store, get(LockOwner.of("recovery edit"))).unsafeRunSync()
            ).session
            get(
              writer
                .perform(
                  eyes4s.studio.core.command.JournalEntry
                    .Apply(eyes4s.studio.core.command.Command.SetTheme(Theme.Dark))
                )
                .unsafeRunSync()
            )
            get(writer.close.unsafeRunSync())
            val before = get(store.readSidecar(Sidecar.Journal).unsafeRunSync()).toVector
            val (platform, host) = setup(fx, directory)
            val original         = runOnFx(host.window.get)
            try
              platform.answer(Some(path(target))).unsafeRunSync()
              invoke(host, CommandRegistry.openProject.id)
              press(fx, "Cancel")
              eventually(fx, "recovery open cancelled")(!host.busy)
              assertEquals(runOnFx(host.window), Some(original))
              assertEquals(
                get(store.readSidecar(Sidecar.Journal).unsafeRunSync()).toVector,
                before
              )
              val deadline = System.nanoTime() + 10.seconds.toNanos
              while !usable(target) && System.nanoTime() < deadline do Thread.sleep(10)
              platform.answer(Some(path(target))).unsafeRunSync()
              invoke(host, CommandRegistry.openProject.id)
              press(fx, "Restore unsaved changes")
              eventually(fx, "recovered real project current")(
                !host.busy && host.window
                  .exists(_.runtime.model.project.exists(_.value == "recovery"))
              )
              assertEquals(
                runOnFx(host.window.get.runtime.model.document.presentation.theme),
                Theme.Dark
              )
              assert(runOnFx(original.isClosed))
            finally runOnFx(host.shutdown())
          }
        }
        .unsafeRunSync()
  }

  fxStage.test(
    "untitled persistence reports missing location and Import directs native project creation"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-location-")
      .use { directory =>
        IO.blocking {
          val (_, host) = setup(fx, directory)
          try
            val window = runOnFx(host.window.get)
            runOnFx(
              window.runtime.dispatch(Intent.RenameProject(get(ProjectName.of("unsaved"))))
            )
            eventually(fx, "no save location explained") {
              eyes4s.studio.app.vm.Shell
                .project(window.runtime.model)
                .notice
                .exists(_.text.contains("no project save location"))
            }
            invoke(host, CommandRegistry.importSources.id)
            press(fx, "OK")
            assertEquals(runOnFx(window.runtime.model.document.datasets), Vector.empty)
            assert(!runOnFx(window.isClosed))
          finally runOnFx(host.shutdown())
        }
      }
      .unsafeRunSync()
  }

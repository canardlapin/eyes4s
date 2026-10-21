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
import eyes4s.studio.core.preferences.{PreferencesStore, RecentProjects, UserPreferences}
import eyes4s.studio.desktop.platform.{
  DesktopPreferences,
  FilePresetStore,
  FileProjectStore,
  JvmFileSystem,
  TempDirs
}
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.harness.StudioTheme
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
      files: FileSystem[IO] = JvmFileSystem,
      preferences: Option[DesktopPreferences] = None,
      initialRecent: RecentProjects = RecentProjects.empty
  ): (InMemoryPlatform[IO], ProjectLifecycleHost) =
    val memory   = InMemoryPlatform.create[IO]().unsafeRunSync()
    val platform = memory.platform.copy(files = files)
    val host     = runOnFx(
      ProjectLifecycleHost(
        fx.stage,
        platform,
        presets = FilePresetStore(directory.resolve("presets")),
        preferences = preferences,
        initialRecent = initialRecent
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
          val sceneStyles      = runOnFx(fx.scene.getStylesheets.asScala.toVector)
          try
            assert(runOnFx(fx.stage.getScene eq fx.scene))
            assertEquals(runOnFx(fx.stage.getTitle), "Untitled project")
            assert(runOnFx(host.window.exists(_.session.fixture.isEmpty)))
            platform.answer(Some(path(project))).unsafeRunSync()
            invoke(host, CommandRegistry.newProject.id)
            eventually(fx, "named native project opened")(
              !host.busy && host.window
                .exists(_.runtime.model.project.exists(_.value == "study"))
            )
            val first = runOnFx(host.window.get)
            assert(runOnFx(fx.stage.getScene eq fx.scene))
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
            assert(runOnFx(fx.stage.getScene eq fx.scene))
            assertEquals(runOnFx(fx.scene.getStylesheets.asScala.toVector), sceneStyles)
            assertEquals(runOnFx(host.window.get.themes.theme), Some(Theme.Dark))
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

  fxStage.test("named active-work admission saves and closes through the actual dialog") { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-save-close-")
      .use { directory =>
        IO.blocking {
          val target = directory.resolve("saved.eyes")
          create(target)
          val (platform, host) = setup(fx, directory)
          try
            platform.answer(Some(path(target))).unsafeRunSync()
            invoke(host, CommandRegistry.openProject.id)
            eventually(fx, "named project ready for save admission")(
              !host.busy && host.window.exists(_.runtime.model.project.isDefined)
            )
            val original = runOnFx(host.window.get)
            val work     =
              eyes4s.studio.app.StoryModels.t3Summary.jobs.jobs.filterNot(_.phase.isTerminal)
            runOnFx(original.runtime.dispatch(Intent.JobsChanged(work)))
            invoke(host, CommandRegistry.closeProject.id)
            press(fx, "Save and close")
            eventually(fx, "named save and close completed")(
              !host.busy && host.window.exists(_.runtime.model.project.isEmpty)
            )
            assert(runOnFx(original.isClosed))
            assert(usable(target), "save admission must drain the port and release the writer")
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

  fxStage.test(
    "Save As switches to a real copy, retains undo, and releases only the source writer"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-copy-")
      .use { directory =>
        IO.blocking {
          val source = directory.resolve("source.eyes")
          val target = directory.resolve("copy.eyes")
          create(source)
          val (platform, host) = setup(fx, directory)
          try
            platform.answer(Some(path(source))).unsafeRunSync()
            invoke(host, CommandRegistry.openProject.id)
            eventually(fx, "source opened")(
              !host.busy && host.window.exists(_.runtime.model.project.isDefined)
            )
            val old     = runOnFx(host.window.get)
            val initial = runOnFx(old.runtime.model.document.presentation.theme)
            val theme   = if initial == Theme.Light then Theme.Dark else Theme.Light
            runOnFx(
              old.runtime
                .dispatch(Intent.Dispatch(eyes4s.studio.core.command.Command.SetTheme(theme)))
            )
            eventually(fx, "source edit saved")(!old.runtime.model.save.edited)
            val history  = runOnFx(old.runtime.model.history)
            val manifest =
              Files.readAllBytes(source.resolve(ProjectStore.ManifestName)).toVector
            platform.answer(Some(path(target))).unsafeRunSync()
            invoke(host, CommandRegistry.saveProjectAs.id)
            eventually(fx, "copy becomes current")(
              !host.busy && host.window
                .exists(_.runtime.model.project.exists(_.value == "copy"))
            )
            val copied = runOnFx(host.window.get)
            assert(runOnFx(old.isClosed))
            assert(runOnFx(fx.stage.getScene eq fx.scene))
            assertEquals(runOnFx(fx.stage.getTitle), "copy.eyes")
            assertEquals(runOnFx(copied.runtime.model.history), history)
            assert(!runOnFx(copied.runtime.model.save.edited))
            assertEquals(
              Files.readAllBytes(source.resolve(ProjectStore.ManifestName)).toVector,
              manifest
            )
            assert(usable(source))
            assert(!usable(target), "the current copy keeps its writer")
            fx.snapshot(StudioTheme.Dark): Unit
            runOnFx(
              copied.runtime
                .dispatch(Intent.Undo(eyes4s.studio.core.command.HistoryStack.Presentation))
            )
            eventually(fx, "copied undo saved")(
              !copied.runtime.model.save.edited && copied.runtime.model.document.presentation.theme == initial
            )
            platform.answer(None).unsafeRunSync()
            invoke(host, CommandRegistry.saveProjectAs.id)
            eventually(fx, "cancelled chooser")(!host.busy)
            assert(runOnFx(host.window.contains(copied)))
          finally runOnFx(host.shutdown())
          assert(usable(target))
        }
      }
      .unsafeRunSync()
  }

  fxStage.test(
    "Save As gives an edited untitled project its first bundle without losing undo"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-first-copy-")
      .use { directory =>
        IO.blocking {
          val target           = directory.resolve("first.eyes")
          val (platform, host) = setup(fx, directory)
          try
            val old     = runOnFx(host.window.get)
            val initial = runOnFx(old.runtime.model.document.presentation.theme)
            val theme   = if initial == Theme.Light then Theme.Dark else Theme.Light
            runOnFx(
              old.runtime
                .dispatch(Intent.Dispatch(eyes4s.studio.core.command.Command.SetTheme(theme)))
            )
            assert(runOnFx(old.runtime.model.save.edited))
            val history = runOnFx(old.runtime.model.history)
            platform.answer(Some(path(target))).unsafeRunSync()
            invoke(host, CommandRegistry.saveProjectAs.id)
            eventually(fx, "untitled saved as first")(
              !host.busy && host.window
                .exists(_.runtime.model.project.exists(_.value == "first"))
            )
            val copied = runOnFx(host.window.get)
            assertEquals(runOnFx(copied.runtime.model.history), history)
            assert(!runOnFx(copied.runtime.model.save.edited))
            runOnFx(
              copied.runtime
                .dispatch(Intent.Undo(eyes4s.studio.core.command.HistoryStack.Presentation))
            )
            eventually(fx, "first bundle undo saved")(
              !copied.runtime.model.save.edited && copied.runtime.model.document.presentation.theme == initial
            )
          finally runOnFx(host.shutdown())
          val store  = FileProjectStore.at[IO](target).unsafeRunSync()
          val opened = get(
            ProjectSession.open[IO](store, get(LockOwner.of("cold copy check"))).unsafeRunSync()
          )
          try
            assertEquals(
              opened.session.document.unsafeRunSync().presentation.theme,
              Theme.Light
            )
          finally get(opened.session.close.unsafeRunSync())
        }
      }
      .unsafeRunSync()
  }

  fxStage.test("edits during Save As keep the source even when the document ends unchanged") {
    fx =>
      TempDirs
        .resource("eyes4s-lifecycle-copy-race-")
        .use { directory =>
          IO.blocking {
            val source = directory.resolve("source.eyes")
            val target = directory.resolve("copy.eyes")
            create(source)
            val entered = Deferred[IO, Unit].unsafeRunSync()
            val finish  = Deferred[IO, Unit].unsafeRunSync()
            val files   = new FileSystem[IO]:
              export JvmFileSystem.{child, read, readStream, write, list}
              def project(p: HostPath): IO[Either[PlatformError, ProjectStore[IO]]] =
                JvmFileSystem
                  .project(p)
                  .map(
                    _.map { delegate =>
                      if p != path(target) then delegate
                      else
                        new ProjectStore[IO]:
                          export delegate.{
                            read,
                            list,
                            write,
                            delete,
                            swapManifest,
                            acquire,
                            release,
                            readSidecar,
                            appendSidecar,
                            replaceSidecar,
                            removeSidecar
                          }
                          def readManifest = delegate.readManifest
                            .flatTap(_ => entered.complete(()).void >> finish.get)
                    }
                  )
            val (platform, host) = setup(fx, directory, files)
            try
              platform.answer(Some(path(source))).unsafeRunSync()
              invoke(host, CommandRegistry.openProject.id)
              eventually(fx, "race source opened")(
                !host.busy && host.window.exists(_.runtime.model.project.isDefined)
              )
              val old    = runOnFx(host.window.get)
              val before = runOnFx(old.runtime.model.history)
              platform.answer(Some(path(target))).unsafeRunSync()
              invoke(host, CommandRegistry.saveProjectAs.id)
              entered.get.timeout(10.seconds).unsafeRunSync()
              runOnFx {
                val theme = before.document.presentation.theme
                val other = if theme == Theme.Light then Theme.Dark else Theme.Light
                old.runtime.dispatch(
                  Intent.Dispatch(eyes4s.studio.core.command.Command.SetTheme(other))
                )
                old.runtime.dispatch(
                  Intent.Dispatch(eyes4s.studio.core.command.Command.SetTheme(theme))
                )
              }
              assertEquals(runOnFx(old.runtime.model.document), before.document)
              assert(runOnFx(old.runtime.model.history != before))
              finish.complete(()).unsafeRunSync()
              press(fx, "OK")
              eventually(fx, "copy rejected and writer released")(!host.busy && usable(target))
              assert(runOnFx(host.window.contains(old)))
              assert(!runOnFx(old.isClosed))
              assert(!usable(source))
            finally
              finish.complete(()).unsafeRunSync()
              runOnFx(host.shutdown())
          }
        }
        .unsafeRunSync()
  }

  fxStage.test("Save As refuses a nonempty destination without replacing the live project") {
    fx =>
      TempDirs
        .resource("eyes4s-lifecycle-copy-nonempty-")
        .use { directory =>
          IO.blocking {
            val target = directory.resolve("occupied.eyes")
            Files.createDirectories(target)
            val sentinel = target.resolve("keep.txt")
            Files.writeString(sentinel, "existing content")
            val (platform, host) = setup(fx, directory)
            try
              val old = runOnFx(host.window.get)
              platform.answer(Some(path(target))).unsafeRunSync()
              invoke(host, CommandRegistry.saveProjectAs.id)
              press(fx, "OK")
              assert(!runOnFx(host.busy))
              assert(runOnFx(host.window.contains(old)))
              assert(!runOnFx(old.isClosed))
              assertEquals(Files.readString(sentinel), "existing content")
              assert(!Files.exists(target.resolve(ProjectStore.ManifestName)))
            finally runOnFx(host.shutdown())
          }
        }
        .unsafeRunSync()
  }

  private def chooseRecent(host: ProjectLifecycleHost, target: Path): Unit = runOnFx {
    val menu = host.window.get.shell.menus
      .flatMap(_.getItems.asScala)
      .collectFirst {
        case m: Menu if m.getId == CommandRegistry.openRecent.id.value => m
      }
      .getOrElse(fail("no recent submenu"))
    val item = menu.getItems.asScala
      .find(_.getText == target.toString)
      .getOrElse(fail("recent path missing"))
    assert(!item.isDisable)
    item.fire()
  }

  private def preferenceStore(directory: Path): PreferencesStore[IO] =
    PreferencesStore[IO](
      JvmFileSystem,
      path(directory.resolve("preferences.json")),
      IO.pure(1L)
    )

  fxStage.test(
    "Open Recent persists successful Open and Save As in order, survives close and cold boot"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-recent-")
      .use { directory =>
        IO.blocking {
          val first  = directory.resolve("first.eyes")
          val copied = directory.resolve("copy.eyes")
          create(first)
          val store            = preferenceStore(directory)
          val (platform, host) =
            setup(fx, directory, preferences = Some(DesktopPreferences(store)))
          try
            platform.answer(Some(path(first))).unsafeRunSync()
            invoke(host, CommandRegistry.openProject.id)
            eventually(fx, "first recent project")(
              !host.busy && host.window
                .exists(_.runtime.model.recent.paths == Vector(path(first)))
            )
            platform.answer(Some(path(copied))).unsafeRunSync()
            invoke(host, CommandRegistry.saveProjectAs.id)
            eventually(fx, "copy is most recent")(
              !host.busy && host.window
                .exists(_.runtime.model.recent.paths == Vector(path(copied), path(first)))
            )
            invoke(host, CommandRegistry.closeProject.id)
            eventually(fx, "close retains recent menu")(
              !host.busy && host.window.exists(_.runtime.model.project.isEmpty)
            )
            chooseRecent(host, first)
            eventually(fx, "recent item reopens without a chooser")(
              !host.busy && host.window
                .exists(_.runtime.model.project.exists(_.value == "first"))
            )
            assertEquals(
              runOnFx(host.window.get.runtime.model.recent.paths),
              Vector(path(first), path(copied))
            )
            assert(!usable(first))
            assert(usable(copied))
          finally runOnFx(host.shutdown())
          val recent = store.load.unsafeRunSync()._1.recent
          assertEquals(recent.paths, Vector(path(first), path(copied)))
          val (_, cold) = setup(
            fx,
            directory,
            preferences = Some(DesktopPreferences(store)),
            initialRecent = recent
          )
          try
            assertEquals(runOnFx(cold.window.get.runtime.model.recent), recent)
            chooseRecent(cold, copied)
            eventually(fx, "cold recent selection opens")(
              !cold.busy && cold.window
                .exists(_.runtime.model.project.exists(_.value == "copy"))
            )
          finally runOnFx(cold.shutdown())
          assertEquals(
            store.load.unsafeRunSync()._1.recent.paths,
            Vector(path(copied), path(first))
          )
        }
      }
      .unsafeRunSync()
  }

  fxStage.test(
    "only definitely missing recent projects are forgotten; failed opens keep the live writer"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-recent-missing-")
      .use { directory =>
        IO.blocking {
          val live    = directory.resolve("live.eyes")
          val missing = directory.resolve("missing.eyes")
          val broken  = directory.resolve("broken.eyes")
          create(live)
          Files.createDirectories(broken)
          val manifest = broken.resolve(ProjectStore.ManifestName)
          Files.writeString(manifest, "broken manifest evidence")
          val initial = RecentProjects.of(Vector(path(missing), path(broken), path(live)))
          val store   = preferenceStore(directory)
          get(store.save(UserPreferences.defaults.withRecent(initial)).unsafeRunSync())
          val (platform, host) = setup(
            fx,
            directory,
            preferences = Some(DesktopPreferences(store)),
            initialRecent = initial
          )
          try
            platform.answer(Some(path(live))).unsafeRunSync()
            invoke(host, CommandRegistry.openProject.id)
            eventually(fx, "live opened before refusal")(
              !host.busy && host.window
                .exists(_.runtime.model.project.exists(_.value == "live"))
            )
            val old = runOnFx(host.window.get)
            chooseRecent(host, missing)
            press(fx, "OK")
            assert(runOnFx(host.window.contains(old)))
            assert(
              !Files.exists(missing),
              "a missing recent entry must not create a phantom directory"
            )
            assert(!runOnFx(old.runtime.model.recent.paths.contains(path(missing))))
            chooseRecent(host, broken)
            press(fx, "OK")
            assert(runOnFx(host.window.contains(old)))
            assert(runOnFx(old.runtime.model.recent.paths.contains(path(broken))))
            assertEquals(Files.readString(manifest), "broken manifest evidence")
            assert(!usable(live))
          finally runOnFx(host.shutdown())
          assertEquals(
            store.load.unsafeRunSync()._1.recent.paths,
            Vector(path(live), path(broken))
          )
        }
      }
      .unsafeRunSync()
  }

  fxStage.test(
    "cancelling admission of a recent project preserves untitled edits and recent order"
  ) { fx =>
    TempDirs
      .resource("eyes4s-lifecycle-recent-cancel-")
      .use { directory =>
        IO.blocking {
          val first = directory.resolve("first.eyes")
          val other = directory.resolve("other.eyes")
          create(first)
          create(other)
          val initial = RecentProjects.of(Vector(path(other), path(first)))
          val store   = preferenceStore(directory)
          get(store.save(UserPreferences.defaults.withRecent(initial)).unsafeRunSync())
          val (_, host) = setup(
            fx,
            directory,
            preferences = Some(DesktopPreferences(store)),
            initialRecent = initial
          )
          try
            val old = runOnFx(host.window.get)
            runOnFx(
              old.runtime.dispatch(
                Intent.Dispatch(eyes4s.studio.core.command.Command.SetTheme(Theme.Dark))
              )
            )
            assert(runOnFx(old.runtime.model.save.edited))
            chooseRecent(host, first)
            press(fx, "Keep open")
            eventually(fx, "recent candidate released")(!host.busy && usable(first))
            assert(runOnFx(host.window.contains(old)))
            assert(runOnFx(old.runtime.model.save.edited))
            assertEquals(runOnFx(old.runtime.model.recent), initial)
          finally runOnFx(host.shutdown())
          assertEquals(store.load.unsafeRunSync()._1.recent, initial)
        }
      }
      .unsafeRunSync()
  }

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

package eyes4s.studio.desktop.platform

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import eyes4s.studio.app.tokens.FontFace
import eyes4s.studio.core.bundle.ProjectStore
import eyes4s.studio.core.platform.*
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.application.Platform as FxPlatform
import javafx.scene.input.{Clipboard as FxClipboard, ClipboardContent}
import fs2.{Chunk, Stream}
import javafx.stage.{DirectoryChooser, FileChooser, Window}

import java.nio.file.{Files, InvalidPathException, NoSuchFileException, Path, Paths}
import java.util.prefs.{BackingStoreException, Preferences as JPreferences}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** The JVM platform services (ticket S0.9, DESIGN_SPEC section 13): the
  * desktop's implementations of studio-core's platform interfaces. Each passes
  * `PlatformConformance`; docs/studio/PORTING.md lists what another host
  * implements instead.
  */
object DesktopPlatform:

  /** The services of this JVM.
    *
    * @param show the application's `HostServices.showDocument`
    * @param owner the window dialogs are modal to, when there is one
    * @param preferences the node per-user preferences are kept under
    */
  def create(
      show: String => Unit,
      owner: () => Option[Window],
      preferences: JPreferences = JPreferences.userRoot.node("eyes4s/studio")
  ): Platform[IO] =
    Platform(
      capabilities,
      JvmFileSystem,
      JavaFxDialogs(owner),
      JavaFxClipboard,
      JavaFxFonts,
      Scheduler.temporal[IO](JvmZone.offsetMinutes),
      DesktopExternalOpen(show),
      JvmPreferences(preferences)
    )

  /** Native naming metadata; portable import code never interprets opaque host paths. */
  def fileName(path: HostPath): Either[PlatformError, String] =
    try
      Option(Paths.get(path.value).getFileName)
        .map(_.toString)
        .toRight(PlatformError.Unreadable(path, "a root has no file name"))
    catch case e: InvalidPathException => Left(PlatformError.InvalidPath(path, e.getReason))

  /** Preserve the legacy per-user preset directory at the native location edge. */
  def importPresetDirectory: Path =
    Paths.get(sys.props.getOrElse("user.home", ".")).resolve(".eyes4s-studio/import-presets")

  /** macOS has the one native menu bar; elsewhere the menu is in the window. */
  val capabilities: HostCapabilities = HostCapabilities(
    nativeMenuBar = sys.props.getOrElse("os.name", "").toLowerCase.contains("mac"),
    multipleWindows = true,
    localFiles = true,
    clipboardRead = true
  )

  /** Run `body` on the FX application thread. Where the IO runs is checked
    * when it runs, in the same step as `body`, never when it is built: an IO
    * built on the FX thread may run on any pool.
    */
  private[platform] def onFx[A](body: => A): IO[A] =
    IO.delay(
      if FxPlatform.isFxApplicationThread then Some(Right(body))
      else None
    ).flatMap {
      case Some(result) => IO.fromEither(result)
      case None         =>
        IO.async_[A] { done =>
          FxPlatform.runLater(() =>
            done(try Right(body)
            catch case NonFatal(e) => Left(e))
          )
        }
    }

/** The JVM's UTC offset, in minutes, at an epoch millisecond. */
object JvmZone:
  def offsetMinutes(epochMillis: Long): Int =
    java.time.ZoneId
      .systemDefault()
      .getRules
      .getOffset(java.time.Instant.ofEpochMilli(epochMillis))
      .getTotalSeconds / 60

/** Host paths are the JVM's file-system paths; bundles are [[FileProjectStore]]s.
  * Every conversion from a host path is checked, and every failure of the
  * file system is a [[PlatformError]] naming the path.
  */
object JvmFileSystem extends FileSystem[IO]:

  /** The JVM path of a host path, made absolute against the working
    * directory. Every operation works on the absolute path, so `child` of a
    * relative directory returns an absolute path.
    */
  private def local(path: HostPath): Either[PlatformError, Path] =
    try Right(Paths.get(path.value).toAbsolutePath)
    catch case e: InvalidPathException => Left(PlatformError.InvalidPath(path, e.getReason))

  private def host(path: Path): Either[PlatformError, HostPath] = HostPath.of(path.toString)

  /** `body` on the blocking pool with the path converted; file-system
    * failures become `failed(reason)`, or `Missing` for a missing path.
    */
  private def at[A](path: HostPath)(failed: String => PlatformError)(
      body: Path => Either[PlatformError, A]
  ): IO[Either[PlatformError, A]] =
    local(path).fold(
      e => IO.pure(Left(e)),
      p =>
        IO.blocking(body(p)).recover {
          case _: NoSuchFileException => Left(PlatformError.Missing(path))
          case NonFatal(e)            => Left(failed(e.toString))
        }
    )

  def child(directory: HostPath, name: String): Either[PlatformError, HostPath] =
    if !HostPath.validName(name) then Left(PlatformError.InvalidName(directory, name))
    else
      local(directory).flatMap { dir =>
        try host(dir.resolve(name))
        catch case e: InvalidPathException => Left(PlatformError.InvalidName(directory, name))
      }

  def read(path: HostPath): IO[Either[PlatformError, IArray[Byte]]] =
    at(path)(PlatformError.Unreadable(path, _)) { p =>
      if Files.isDirectory(p) then Left(PlatformError.Unreadable(path, "a directory"))
      else Right(IArray.unsafeFromArray(Files.readAllBytes(p)))
    }

  def readStream(path: HostPath, chunkSize: Int): IO[Either[PlatformError, Stream[IO, Byte]]] =
    at(path)(PlatformError.Unreadable(path, _)) { p =>
      if Files.isDirectory(p) then Left(PlatformError.Unreadable(path, "a directory"))
      else if !Files.isReadable(p) then
        if Files.exists(p) then Left(PlatformError.Unreadable(path, "not readable"))
        else Left(PlatformError.Missing(path))
      else
        val size = chunkSize.max(1)
        Right(
          Stream
            .resource(Resource.fromAutoCloseable(IO.blocking(Files.newInputStream(p))))
            .flatMap { in =>
              Stream
                .repeatEval(IO.blocking {
                  val buffer = new Array[Byte](size)
                  val n      = in.read(buffer)
                  if n < 0 then None else Some(Chunk.array(buffer, 0, n))
                })
                .unNoneTerminate
                .unchunks
            }
            .adaptError { case NonFatal(e) =>
              PlatformFailure(PlatformError.Unreadable(path, e.toString))
            }
        )
    }

  /** Written beside the target and moved into place, so a reader never sees
    * a partial file.
    */
  def write(path: HostPath, bytes: IArray[Byte]): IO[Either[PlatformError, Unit]] =
    at(path)(PlatformError.Unwritable(path, _)) { target =>
      Option(target.getParent) match
        case _ if Files.isDirectory(target) =>
          Left(PlatformError.Unwritable(path, "a directory"))
        case None =>
          Left(PlatformError.Unwritable(path, "a root has no parent directory"))
        case Some(parent) =>
          Files.createDirectories(parent)
          val staged = Files.createTempFile(parent, ".eyes4s-", ".tmp")
          try
            Files.write(staged, IArray.genericWrapArray(bytes).toArray)
            Files.move(
              staged,
              target,
              java.nio.file.StandardCopyOption.REPLACE_EXISTING,
              java.nio.file.StandardCopyOption.ATOMIC_MOVE
            )
          finally Files.deleteIfExists(staged): Unit
          Right(())
    }

  def list(directory: HostPath): IO[Either[PlatformError, Vector[HostPath]]] =
    at(directory)(PlatformError.Unreadable(directory, _)) { dir =>
      if Files.exists(dir) && !Files.isDirectory(dir) then
        Left(PlatformError.Unreadable(directory, "not a directory"))
      else
        val stream = Files.list(dir)
        try stream.iterator().asScala.toVector.sortBy(_.toString).traverse(host)
        finally stream.close()
    }

  def project(path: HostPath): IO[Either[PlatformError, ProjectStore[IO]]] =
    local(path).fold(
      e => IO.pure(Left(e)),
      p =>
        FileProjectStore
          .at[IO](p)
          .map(_.asRight[PlatformError])
          .recover { case NonFatal(e) => Left(PlatformError.Unreadable(path, e.toString)) }
    )

/** JavaFX file choosers, modal to `owner`. */
final class JavaFxDialogs(owner: () => Option[Window]) extends Dialogs[IO]:

  private def choose(request: FileRequest, save: Boolean): IO[Option[HostPath]] =
    DesktopPlatform
      .onFx {
        val chooser = JavaFxDialogs.chooser(request)
        val window  = owner().orNull
        Option(if save then chooser.showSaveDialog(window) else chooser.showOpenDialog(window))
      }
      .map(_.flatMap(f => HostPath.of(f.getPath).toOption))

  def chooseOpen(request: FileRequest): IO[Option[HostPath]] = choose(request, save = false)
  def chooseSave(request: FileRequest): IO[Option[HostPath]] = choose(request, save = true)

  /** A `DirectoryChooser`: a project bundle is a directory, which a
    * `FileChooser` cannot pick.
    */
  def chooseDirectory(title: String): IO[Option[HostPath]] =
    DesktopPlatform
      .onFx(Option(JavaFxDialogs.directoryChooser(title).showDialog(owner().orNull)))
      .map(_.flatMap(f => HostPath.of(f.getPath).toOption))

object JavaFxDialogs:
  /** The chooser a directory request is shown in. */
  def directoryChooser(title: String): DirectoryChooser =
    val chooser = DirectoryChooser()
    chooser.setTitle(title)
    chooser

  /** The chooser a request is shown in: its title, one filter per kind, and
    * the suggested name.
    */
  def chooser(request: FileRequest): FileChooser =
    val chooser = FileChooser()
    chooser.setTitle(request.title)
    request.kinds.foreach { k =>
      chooser.getExtensionFilters.add(
        FileChooser.ExtensionFilter(k.description, k.extensions.map(e => s"*.$e").asJava)
      )
    }
    request.suggestedName.foreach(chooser.setInitialFileName)
    chooser

/** The system clipboard's text, read and written on the FX thread. */
object JavaFxClipboard extends Clipboard[IO]:
  def readText: IO[Either[PlatformError, Option[String]]] =
    DesktopPlatform.onFx {
      val clipboard = FxClipboard.getSystemClipboard
      Right(if clipboard.hasString then Option(clipboard.getString) else None)
    }

  def writeText(text: String): IO[Either[PlatformError, Unit]] =
    DesktopPlatform.onFx {
      val content = ClipboardContent()
      content.putString(text)
      if FxClipboard.getSystemClipboard.setContent(content) then Right(())
      else Left(PlatformError.Unsupported("clipboard", "writing"))
    }

/** Registers studio's bundled faces through [[StudioFonts]]; any other
  * resource is refused.
  */
object JavaFxFonts extends Fonts[IO]:

  /** Every bundled face, as the platform interface names it. */
  val bundled: Vector[FontRequest] =
    FontFace.values.toVector.flatMap(f =>
      FontRequest.of(f.javaFxFamily, StudioFonts.resource(f)).toOption
    )

  def register(font: FontRequest): IO[Either[PlatformError, Unit]] =
    FontFace.values.find(StudioFonts.resource(_) == font.resource) match
      case None =>
        IO.pure(Left(PlatformError.FontRejected(font, "not a bundled face")))
      case Some(face) if face.javaFxFamily != font.family =>
        IO.pure(
          Left(PlatformError.FontRejected(font, s"the bundled face is ${face.javaFxFamily}"))
        )
      case Some(face) =>
        IO.blocking(StudioFonts.load(face))
          .map(
            _.left.map(p => PlatformError.FontRejected(font, p.message)).map(_ => ())
          )

/** Opens URLs through `show` (the application's `HostServices.showDocument`)
  * and reveals a path by showing the directory that holds it.
  */
final class DesktopExternalOpen(show: String => Unit) extends ExternalOpen[IO]:
  def open(target: ExternalTarget): IO[Either[PlatformError, Unit]] =
    IO.blocking {
      target match
        case ExternalTarget.Url(url)     => show(url.value)
        case ExternalTarget.Reveal(path) => show(DesktopExternalOpen.revealed(path))
    }.attempt
      .map(_.left.map(e => PlatformError.OpenFailed(target, e.toString)))

object DesktopExternalOpen:
  /** The `file:` URI shown for a reveal: a directory itself, a file's parent. */
  def revealed(path: HostPath): String =
    val p      = Paths.get(path.value).toAbsolutePath
    val parent = Option(p.getParent)
    (if Files.isDirectory(p) then p else parent.getOrElse(p)).toUri.toString

/** Preferences under one `java.util.prefs` node, flushed on every change. */
final class JvmPreferences(node: JPreferences) extends Preferences[IO]:

  private def guarded[A](key: PreferenceKey)(body: => A): IO[Either[PlatformError, A]] =
    IO.blocking(body).map(_.asRight[PlatformError]).recover {
      case e @ (_: BackingStoreException | _: IllegalArgumentException |
          _: IllegalStateException) =>
        Left(PlatformError.PreferenceFailed(key, e.toString))
    }

  def get(key: PreferenceKey): IO[Either[PlatformError, Option[String]]] =
    guarded(key)(Option(node.get(key.value, null)))

  def put(key: PreferenceKey, value: String): IO[Either[PlatformError, Unit]] =
    guarded(key) { node.put(key.value, value); node.flush() }

  def remove(key: PreferenceKey): IO[Either[PlatformError, Unit]] =
    guarded(key) { node.remove(key.value); node.flush() }

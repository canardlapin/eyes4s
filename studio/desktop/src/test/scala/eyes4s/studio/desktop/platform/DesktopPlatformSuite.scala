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
import eyes4s.studio.core.platform.*
import eyes4s.studio.desktop.harness.StudioFxSuite
import javafx.application.Platform
import javafx.scene.input.{Clipboard as FxClipboard, ClipboardContent}

import java.nio.file.Files
import java.util.UUID
import java.util.prefs.Preferences as JPreferences
import scala.jdk.CollectionConverters.*

/** The JVM platform services pass [[PlatformConformance]] (S0.9): files in a
  * temporary directory, a throwaway preferences node, the system clipboard on
  * the FX thread, the bundled fonts and the effect-timer scheduler on real
  * time. File choosers are interactive, so only their configuration is
  * checked.
  */
class DesktopPlatformSuite extends PlatformConformance:

  override def beforeAll(): Unit =
    super.beforeAll()
    StudioFxSuite.startToolkit()

  private val preferences: Resource[IO, JPreferences] =
    Resource.make(
      IO.blocking(JPreferences.userRoot.node(s"eyes4s-test/${UUID.randomUUID}"))
    )(node => IO.blocking { node.removeNode(); JPreferences.userRoot.flush() })

  /** The developer's clipboard, every format of it, put back afterwards. */
  private val clipboardKept: Resource[IO, Unit] =
    Resource
      .make(
        DesktopPlatform.onFx {
          val clipboard = FxClipboard.getSystemClipboard
          clipboard.getContentTypes.asScala.toVector
            .flatMap(f => Option(clipboard.getContent(f)).map(f -> _))
        }
      )(saved =>
        DesktopPlatform.onFx {
          val content = ClipboardContent()
          saved.foreach((format, value) => content.put(format, value))
          FxClipboard.getSystemClipboard.setContent(content): Unit
        }
      )
      .map(_ => ())

  def subject: Resource[IO, PlatformConformance.Subject] =
    for
      _     <- clipboardKept
      dir   <- TempDirs.resource("eyes4s-platform")
      prefs <- preferences
      shown <- Resource.eval(IO(java.util.concurrent.ConcurrentLinkedQueue[String]()))
    yield PlatformConformance.Subject(
      DesktopPlatform.create(s => shown.add(s): Unit, () => None, prefs),
      right(HostPath.of(dir.toString)),
      right(HostPath.of(dir.getRoot.toString)),
      JavaFxFonts.bundled,
      IO(shown.asScala.toVector),
      {
        case ExternalTarget.Url(url)     => url.value
        case ExternalTarget.Reveal(path) => DesktopExternalOpen.revealed(path)
      },
      IO.sleep
    )

  test("every bundled face registers and a foreign or misnamed face is refused, named") {
    val foreign  = right(FontRequest.of("Comic", "fonts/Comic.ttf"))
    val misnamed = right(FontRequest.of("Wrong", JavaFxFonts.bundled.head.resource))
    for
      a <- JavaFxFonts.register(foreign)
      b <- JavaFxFonts.register(misnamed)
    yield
      assertEquals(JavaFxFonts.bundled.size, eyes4s.studio.app.tokens.FontFace.values.length)
      assert(a.left.exists(_.message.contains("not a bundled face")), a)
      assert(b.left.exists(_.message.contains("Wrong")), b)
  }

  test("a reveal shows the directory holding a file, or the directory itself") {
    TempDirs.resource("eyes4s-reveal").use { dir =>
      IO.blocking {
        val file = Files.writeString(dir.resolve("a.csv"), "x")
        assertEquals(
          DesktopExternalOpen.revealed(right(HostPath.of(file.toString))),
          dir.toUri.toString
        )
        assertEquals(
          DesktopExternalOpen.revealed(right(HostPath.of(dir.toString))),
          dir.toUri.toString
        )
      }
    }
  }

  test(
    "a file chooser carries the request's title, one filter per kind and the suggested name"
  ) {
    val csv  = right(FileKind.of("Fixation CSV", Vector("csv", "tsv")))
    val asc  = right(FileKind.of("EyeLink ASC", Vector("asc")))
    val made = DesktopPlatform.onFx(
      JavaFxDialogs.chooser(FileRequest("Import sources", Vector(csv, asc), Some("study.csv")))
    )
    made.map { chooser =>
      assertEquals(chooser.getTitle, "Import sources")
      assertEquals(
        chooser.getExtensionFilters.asScala.toVector.map(f =>
          f.getDescription -> f.getExtensions.asScala.toVector
        ),
        Vector("Fixation CSV" -> Vector("*.csv", "*.tsv"), "EyeLink ASC" -> Vector("*.asc"))
      )
      assertEquals(chooser.getInitialFileName, "study.csv")
    }
  }

  test("an FX-bound IO built on the FX thread runs on the compute pool") {
    // Built inside runLater, run afterwards on munit's compute pool: the
    // thread check must happen when the IO runs, not when it is built.
    val read = StudioFxSuite.runOnFx(JavaFxClipboard.readText)
    val onFx = StudioFxSuite.runOnFx(DesktopPlatform.onFx(Platform.isFxApplicationThread))
    clipboardKept.surround {
      for
        _      <- ok(JavaFxClipboard.writeText("built on FX"))
        text   <- read
        onFxTh <- onFx
        caller <- IO(Platform.isFxApplicationThread)
      yield
        assertEquals(text, Right(Some("built on FX")))
        assert(onFxTh)
        assert(!caller)
    }
  }

  test("a directory chooser carries the request's title") {
    DesktopPlatform
      .onFx(JavaFxDialogs.directoryChooser("Open project").getTitle)
      .map(assertEquals(_, "Open project"))
  }

  test("a file without read permission streams as Unreadable, not a defect") {
    TempDirs.resource("eyes4s-stream").use { dir =>
      val local = dir.resolve("locked.asc")
      val file  = right(HostPath.of(local.toString))
      for
        _      <- ok(JvmFileSystem.write(file, IArray.empty[Byte]))
        closed <- IO.blocking(local.toFile.setReadable(false))
        denied <- IO.blocking(!Files.isReadable(local))
        result <- JvmFileSystem.readStream(file, 8)
        _      <- IO.blocking(local.toFile.setReadable(true))
      yield
        // A superuser may read anything; the refusal is checked where it applies.
        if closed && denied then
          assertEquals(
            result.map(_ => ()),
            Left(PlatformError.Unreadable(file, "not readable"))
          )
    }
  }

  test("the JVM zone offset is the one java.time reports") {
    IO {
      val now = System.currentTimeMillis()
      assertEquals(
        JvmZone.offsetMinutes(now) * 60,
        java.time.ZoneId.systemDefault.getRules
          .getOffset(java.time.Instant.ofEpochMilli(now))
          .getTotalSeconds
      )
    }
  }

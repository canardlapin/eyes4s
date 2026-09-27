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

  def subject: Resource[IO, PlatformConformance.Subject] =
    for
      dir   <- TempDirs.resource("eyes4s-platform")
      prefs <- preferences
      shown <- Resource.eval(IO(java.util.concurrent.ConcurrentLinkedQueue[String]()))
    yield PlatformConformance.Subject(
      DesktopPlatform.create(s => shown.add(s): Unit, () => None, prefs),
      right(HostPath.of(dir.toString)),
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

  test("listing a file is refused as unreadable, not a directory") {
    TempDirs.resource("eyes4s-list").use { dir =>
      val file = right(HostPath.of(dir.resolve("a.csv").toString))
      for
        _      <- ok(JvmFileSystem.write(file, IArray.empty[Byte]))
        listed <- JvmFileSystem.list(file)
        left   <- IO.blocking(
          Files.list(dir).iterator().asScala.map(_.getFileName.toString).toVector
        )
      yield
        assertEquals(listed, Left(PlatformError.Unreadable(file, "not a directory")))
        assertEquals(left, Vector("a.csv"))
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

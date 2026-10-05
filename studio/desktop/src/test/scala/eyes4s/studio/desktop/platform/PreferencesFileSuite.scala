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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.core.document.StageAppearance
import eyes4s.studio.core.platform.HostPath
import eyes4s.studio.core.preferences.{
  AppearanceChoice,
  PreferencesProblem,
  PreferencesStore,
  RecentProjects,
  UserPreferences
}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

/** The desktop's preferences file (ticket S2.8): where each platform keeps
  * it, and the store on the JVM file system: a round trip through a real
  * file, and a corrupt file read as the defaults, logged and kept.
  */
class PreferencesFileSuite extends munit.FunSuite:

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private val env = Map("APPDATA" -> "/roaming", "XDG_CONFIG_HOME" -> "/xdg")

  test("macOS: Application Support") {
    assertEquals(
      right(PreferencesLocation.of("Mac OS X", "/Users/me", env.get)).value,
      Paths
        .get("/Users/me", "Library", "Application Support", "Eyes Studio", "preferences.json")
        .toString
    )
  }

  test("Windows: APPDATA, else the roaming profile under home") {
    assertEquals(
      right(PreferencesLocation.of("Windows 11", "/home/me", env.get)).value,
      Paths.get("/roaming", "Eyes Studio", "preferences.json").toString
    )
    assertEquals(
      right(PreferencesLocation.of("Windows 11", "/home/me", _ => None)).value,
      Paths.get("/home/me", "AppData", "Roaming", "Eyes Studio", "preferences.json").toString
    )
  }

  test("elsewhere: XDG_CONFIG_HOME, else ~/.config") {
    assertEquals(
      right(PreferencesLocation.of("Linux", "/home/me", env.get)).value,
      Paths.get("/xdg", "eyes4s-studio", "preferences.json").toString
    )
    assertEquals(
      right(
        PreferencesLocation.of("Linux", "/home/me", Map("XDG_CONFIG_HOME" -> " ").get)
      ).value,
      Paths.get("/home/me", ".config", "eyes4s-studio", "preferences.json").toString
    )
  }

  private def withDirectory[A](f: Path => A): A =
    val dir = Files.createTempDirectory("eyes4s-prefs-")
    try f(dir)
    finally
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))

  private def storeIn(dir: Path): (Path, PreferencesStore[IO]) =
    val file = dir.resolve("Eyes Studio").resolve("preferences.json")
    (file, PreferencesStore[IO](JvmFileSystem, right(HostPath.of(file.toString)), IO.pure(1L)))

  test("round trip through a real file, its directory created") {
    withDirectory { dir =>
      val (file, store) = storeIn(dir)
      val p             = UserPreferences.defaults
        .withAppearance(AppearanceChoice.System)
        .withStage(StageAppearance.Light)
        .withRecent(
          RecentProjects.empty.opened(right(HostPath.of(dir.resolve("a.eyes").toString)))
        )
      assertEquals(store.load.unsafeRunSync(), (UserPreferences.defaults, Vector.empty))
      assertEquals(store.save(p).unsafeRunSync(), Right(None))
      assert(Files.isRegularFile(file))
      assertEquals(store.load.unsafeRunSync(), (p, Vector.empty))
    }
  }

  test("a corrupt file: defaults and a logged problem; a save copies it aside first") {
    withDirectory { dir =>
      val (file, store) = storeIn(dir)
      Files.createDirectories(file.getParent)
      Files.writeString(file, """{"version": 1, "preferences": """, UTF_8)
      val (prefs, problems) = store.load.unsafeRunSync()
      assertEquals(prefs, UserPreferences.defaults)
      problems match
        case Vector(p @ PreferencesProblem.Corrupt(_, _)) =>
          assert(p.message.startsWith(s"Preferences $file are unusable"), p.message)
        case other => fail(s"expected one Corrupt problem, got $other")
      val copy = Paths.get(file.toString + ".corrupt-1")
      assert(!Files.exists(copy))
      assertEquals(
        store.save(UserPreferences.defaults).unsafeRunSync().map(_.map(_.value)),
        Right(Some(copy.toString))
      )
      assertEquals(Files.readString(copy, UTF_8), """{"version": 1, "preferences": """)
      assertEquals(store.load.unsafeRunSync(), (UserPreferences.defaults, Vector.empty))
    }
  }

  test("the log names the home directory as ~") {
    assertEquals(
      PreferencesLocation.redact("Preferences /Users/me/Library/x are unusable", "/Users/me"),
      "Preferences ~/Library/x are unusable"
    )
  }

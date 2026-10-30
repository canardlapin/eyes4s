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
import cats.syntax.all.*
import eyes4s.studio.core.document.{LayoutBlob, Perspective, SavedLayout, StageAppearance}
import eyes4s.studio.core.platform.HostPath
import eyes4s.studio.core.preferences.{
  AppearanceChoice,
  PreferencesProblem,
  PreferencesStore,
  UserPreferences
}
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

class DesktopPreferencesSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def host(path: Path): HostPath        = get(HostPath.of(path.toString))
  private def store(path: Path): PreferencesStore[IO] =
    PreferencesStore[IO](JvmFileSystem, host(path), IO.pure(1L))

  test(
    "independent concurrent writers preserve recent paths, appearance, stage, layout and export directory"
  ) {
    TempDirs
      .resource("eyes4s-preference-patches-")
      .use { dir =>
        val file       = dir.resolve("preferences.json")
        val paths      = Vector.tabulate(10)(i => host(dir.resolve(s"project-$i.eyes")))
        val exportPath = host(dir.resolve("exports"))
        val layout     = SavedLayout(Perspective.Results, LayoutBlob("{\"fixture\":true}"))
        val changes: Vector[UserPreferences => UserPreferences] =
          paths.map(path =>
            (p: UserPreferences) => p.withRecent(p.recent.opened(path))
          ) ++ Vector[UserPreferences => UserPreferences](
            _.withAppearance(AppearanceChoice.Dark),
            _.withStage(StageAppearance.Light),
            _.withLayout(layout),
            _.withExportDirectory(Some(exportPath))
          )
        for
          results <- changes
            .parTraverse(change => DesktopPreferences(store(file)).update(change))
          loaded <- store(file).load
        yield
          assert(results.forall(_.isRight), results.toString)
          assertEquals(loaded._2, Vector.empty)
          assertEquals(loaded._1.recent.paths.toSet, paths.toSet)
          assertEquals(loaded._1.appearance, AppearanceChoice.Dark)
          assertEquals(loaded._1.stage, StageAppearance.Light)
          assertEquals(loaded._1.layouts, Vector(layout))
          assertEquals(loaded._1.exportDirectory, Some(exportPath))
      }
      .unsafeRunSync()
  }

  test("a corrupt preferences file is retained and reported before a patch is committed") {
    TempDirs
      .resource("eyes4s-preference-corrupt-")
      .use { dir =>
        val file = dir.resolve("preferences.json")
        for
          _       <- IO.blocking(Files.writeString(file, "unusable preferences evidence"))
          written <- DesktopPreferences(store(file)).update(
            _.withAppearance(AppearanceChoice.Dark)
          )
          result = get(written)
          kept   <- IO.blocking(Files.readString(Path.of(result.kept.get.value)))
          loaded <- store(file).load
        yield
          assert(result.problems.exists(_.isInstanceOf[PreferencesProblem.Corrupt]))
          assertEquals(kept, "unusable preferences evidence")
          assertEquals(
            loaded._1,
            UserPreferences.defaults.withAppearance(AppearanceChoice.Dark)
          )
          assertEquals(loaded._2, Vector.empty)
      }
      .unsafeRunSync()
  }

  test(
    "another process's lock prevents a patch; timeout preserves the file and releasing permits a retry"
  ) {
    TempDirs
      .resource("eyes4s-preference-process-")
      .use { dir =>
        IO.blocking {
          val file = dir.resolve("preferences.json")
          get(store(file).save(UserPreferences.defaults).unsafeRunSync())
          val before = Files.readAllBytes(file).toVector
          val source = dir.resolve("PreferenceLockHolder.java")
          Files.writeString(
            source,
            """
        import java.nio.channels.FileChannel;
        import java.nio.file.Path;
        import java.nio.file.StandardOpenOption;
        class PreferenceLockHolder {
          public static void main(String[] args) throws Exception {
            try (var channel = FileChannel.open(Path.of(args[0]), StandardOpenOption.CREATE, StandardOpenOption.WRITE);
                 var lock = channel.lock()) {
              System.out.println("held");
              System.out.flush();
              System.in.read();
            }
          }
        }
        """
          )
          val java  = Path.of(System.getProperty("java.home"), "bin", "java")
          val child = ProcessBuilder(
            java.toString,
            "--source",
            "21",
            source.toString,
            file.toString + ".lock"
          ).redirectErrorStream(true).start()
          try
            val reader = child.inputReader()
            val ready  =
              IO.interruptibleMany(reader.readLine()).timeout(15.seconds).unsafeRunSync()
            assertEquals(ready, "held")
            val answer = DesktopPreferences(store(file), 80.millis)
              .update(_.withAppearance(AppearanceChoice.Dark))
              .unsafeRunSync()
            assert(answer.left.exists(_.message.contains("Timed out")), answer.toString)
            assertEquals(Files.readAllBytes(file).toVector, before)
            child.getOutputStream.close()
            assert(child.waitFor(10, TimeUnit.SECONDS))
            assertEquals(child.exitValue(), 0)
            assert(
              DesktopPreferences(store(file))
                .update(_.withAppearance(AppearanceChoice.Dark))
                .unsafeRunSync()
                .isRight
            )
          finally
            child.getOutputStream.close()
            if child.isAlive then child.destroyForcibly(): Unit
        }
      }
      .unsafeRunSync()
  }

  test(
    "cancelling a waiting patch releases its channel and a later patch can acquire the writer"
  ) {
    TempDirs
      .resource("eyes4s-preference-cancel-")
      .use { dir =>
        val file     = dir.resolve("preferences.json")
        val lockFile = dir.resolve("preferences.json.lock")
        cats.effect.Resource
          .fromAutoCloseable(
            IO.blocking(
              java.nio.channels.FileChannel.open(
                lockFile,
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.WRITE
              )
            )
          )
          .use { channel =>
            for
              lock   <- IO.blocking(channel.lock())
              waiter <- DesktopPreferences(store(file))
                .update(_.withAppearance(AppearanceChoice.Dark))
                .start
              _     <- IO.sleep(50.millis)
              _     <- waiter.cancel.timeout(2.seconds)
              _     <- IO.blocking(lock.release())
              retry <- DesktopPreferences(store(file)).update(
                _.withAppearance(AppearanceChoice.System)
              )
            yield
              assert(retry.isRight, retry.toString)
              assertEquals(get(retry).preferences.appearance, AppearanceChoice.System)
          }
      }
      .unsafeRunSync()
  }

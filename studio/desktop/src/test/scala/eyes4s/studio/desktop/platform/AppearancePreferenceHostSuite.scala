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
import eyes4s.studio.app.appearance.Appearance
import eyes4s.studio.app.{AppEffect, Intent, StoryModels}
import eyes4s.studio.core.platform.HostPath
import eyes4s.studio.core.preferences.{AppearanceChoice, PreferencesStore, UserPreferences}
import eyes4s.studio.desktop.runtime.{EffectPerformer, StudioRuntime}

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/** The appearance preference host's saves (ticket S2.8 review): one at a
  * time, the latest choice winning, and a failed save neither recorded as
  * saved nor lost (the next choice writes it). Runs without JavaFX: the
  * saves' completions are queued and drained by the test, as the JavaFX
  * thread would run them.
  */
class AppearancePreferenceHostSuite extends munit.FunSuite:

  private val none = new EffectPerformer:
    def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = ()

  private def withDirectory[A](f: Path => A): A =
    val dir = Files.createTempDirectory("eyes4s-host-")
    try f(dir)
    finally
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))

  /** A host on `file`, its completions queued for [[drain]]. */
  private final class Harness(file: Path, initial: UserPreferences):
    val completions = ConcurrentLinkedQueue[() => Unit]()
    val logged      = ConcurrentLinkedQueue[String]()
    val store       = PreferencesStore[IO](
      JvmFileSystem,
      HostPath.of(file.toString).fold(e => fail(e.message), identity),
      IO.pure(1L)
    )
    val host = AppearancePreferenceHost(
      Some(store),
      initial,
      logged.add(_): Unit,
      completions.add(_): Unit
    )
    val runtime = StudioRuntime(StoryModels.t2Compare, none)
    host.attach(runtime)

    /** Runs completions as they arrive until none is pending for a while. */
    def drain(): Unit =
      var idle = 0
      while idle < 20 do
        Option(completions.poll()) match
          case Some(f) => f(); idle = 0
          case None    => Thread.sleep(25); idle += 1

    def choose(a: Appearance): Unit = runtime.dispatch(Intent.SetAppearance(a))

  test("quick choices are saved one at a time and the latest wins") {
    withDirectory { dir =>
      val file = dir.resolve("preferences.json")
      val h    = Harness(file, UserPreferences.defaults)
      h.choose(Appearance.Dark)
      h.choose(Appearance.System)
      h.choose(Appearance.Dark)
      h.drain()
      assertEquals(h.store.load.unsafeRunSync()._1.appearance, AppearanceChoice.Dark)
      assertEquals(h.host.saved.appearance, AppearanceChoice.Dark)
      assertEquals(h.logged.asScala.toVector, Vector.empty)
    }
  }

  test("a choice back to the saved value during a save is still written") {
    withDirectory { dir =>
      val file = dir.resolve("preferences.json")
      val h    = Harness(file, UserPreferences.defaults)
      h.choose(Appearance.Dark)  // saving Dark
      h.choose(Appearance.Light) // back to what the file held before
      h.drain()
      assertEquals(h.store.load.unsafeRunSync()._1.appearance, AppearanceChoice.Light)
    }
  }

  test("a failed save is logged, not recorded as saved, and the next choice writes it") {
    withDirectory { dir =>
      val file = dir.resolve("preferences.json")
      // The path is a directory: the store refuses to replace what it cannot read.
      Files.createDirectories(file)
      val h = Harness(file, UserPreferences.defaults)
      h.choose(Appearance.Dark)
      h.drain()
      assertEquals(h.host.saved.appearance, AppearanceChoice.Light)
      assertEquals(h.host.current.appearance, AppearanceChoice.Dark)
      assert(h.logged.asScala.exists(_.contains("were not saved")), h.logged.toString)
      Files.delete(file)
      h.choose(Appearance.Dark)
      h.drain()
      assertEquals(h.host.saved.appearance, AppearanceChoice.Dark)
      assertEquals(h.store.load.unsafeRunSync()._1.appearance, AppearanceChoice.Dark)
    }
  }

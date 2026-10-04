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
import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.core.platform.HostPath
import eyes4s.studio.core.preferences.{AppearanceChoice, PreferencesStore, UserPreferences}
import eyes4s.studio.desktop.shell.ShellFxSuite

import java.nio.file.Files

/** The appearance preference in a studio window (tickets S2.8, S1.10): a
  * System preference makes the window follow the platform when it opens,
  * and choosing another appearance saves it to the preferences file.
  */
class AppearancePreferenceFxSuite extends ShellFxSuite:

  fxStage.test("System is applied at boot; a new choice is saved to the file") { fx =>
    val dir   = Files.createTempDirectory("eyes4s-appearance-")
    val file  = dir.resolve("preferences.json")
    val store = PreferencesStore[IO](
      JvmFileSystem,
      HostPath.of(file.toString).fold(e => fail(e.message), identity)
    )
    try
      val w    = boot(fx, StoryModels.t2Compare)
      val host = AppearancePreferenceHost(
        Some(store),
        UserPreferences.defaults.withAppearance(AppearanceChoice.System),
        m => fail(m)
      )
      runOnFx(host.attach(w.runtime))
      eventually(fx, "following the platform")(w.runtime.model.appearance.followSystem)
      // Applying the preference at boot is no change to save.
      assert(!Files.exists(file))
      runOnFx(w.runtime.dispatch(Intent.SetAppearance(Appearance.Dark)))
      eventually(fx, "the saved preference") {
        Files.exists(file) &&
        store.load.unsafeRunSync()._1.appearance == AppearanceChoice.Dark
      }
      assertEquals(runOnFx(host.current.appearance), AppearanceChoice.Dark)
    finally
      Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
  }

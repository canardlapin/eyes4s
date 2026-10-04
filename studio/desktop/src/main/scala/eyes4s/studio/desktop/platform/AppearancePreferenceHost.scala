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
import cats.effect.unsafe.IORuntime
import eyes4s.studio.app.appearance.AppearancePreference
import eyes4s.studio.core.preferences.{PreferencesStore, UserPreferences}
import eyes4s.studio.desktop.runtime.StudioRuntime

/** The appearance preference on the desktop (tickets S2.8, S1.10): applies
  * it when a window opens ([[AppearancePreference.atBoot]]) and saves it,
  * off the JavaFX thread, whenever the appearance shown changes
  * ([[AppearancePreference.changed]]). `store` is `None` when the host has
  * no preferences file; a failed save is logged and the preferences stay in
  * memory. Use on the JavaFX thread.
  */
final class AppearancePreferenceHost(
    store: Option[PreferencesStore[IO]],
    initial: UserPreferences,
    log: String => Unit
)(using IORuntime):
  private var prefs = initial

  /** The preferences as last saved or read. */
  def current: UserPreferences = prefs

  def attach(runtime: StudioRuntime): Unit =
    AppearancePreference.atBoot(prefs, runtime.model).foreach(runtime.dispatch)
    runtime.listen(m => AppearancePreference.changed(prefs, m).foreach(save))

  private def save(p: UserPreferences): Unit =
    prefs = p
    store.foreach(
      _.save(p).unsafeRunAsync {
        case Left(e)         => log(s"Preferences could not be saved: ${e.getMessage}")
        case Right(Left(pr)) => log(pr.message)
        case Right(Right(_)) => ()
      }
    )

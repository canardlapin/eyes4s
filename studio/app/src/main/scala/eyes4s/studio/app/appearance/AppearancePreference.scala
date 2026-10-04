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

package eyes4s.studio.app.appearance

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.preferences.{AppearanceChoice, UserPreferences}

/** The appearance as a user preference (ticket S2.8; S1.10's View ›
  * Appearance). Following the platform's theme is a preference only, never a
  * document field (lead's decision on bead S2.8): a fixed Light or Dark is
  * the document's own theme, which a preference never overrides. So the
  * preference applied at boot is System alone, and the preference saved is
  * whatever the user last chose.
  */
object AppearancePreference:

  def toChoice(a: Appearance): AppearanceChoice = a match
    case Appearance.Light  => AppearanceChoice.Light
    case Appearance.Dark   => AppearanceChoice.Dark
    case Appearance.System => AppearanceChoice.System

  def fromChoice(c: AppearanceChoice): Appearance = c match
    case AppearanceChoice.Light  => Appearance.Light
    case AppearanceChoice.Dark   => Appearance.Dark
    case AppearanceChoice.System => Appearance.System

  /** What to dispatch when a window opens on `m` with `prefs`: follow the
    * platform when the user chose to; a fixed choice leaves the document's
    * theme as it is.
    */
  def atBoot(prefs: UserPreferences, m: AppModel): Vector[Intent] =
    Option
      .when(prefs.appearance == AppearanceChoice.System && !m.appearance.followSystem)(
        Intent.SetAppearance(Appearance.System)
      )
      .toVector

  /** The appearance `m` shows, as a preference. */
  def shown(m: AppModel): AppearanceChoice =
    toChoice(m.appearance.shown(m.document.presentation.theme))

  /** The preferences to save after a model change, when the appearance
    * shown differs from the one `prefs` holds; `None` when nothing changed.
    */
  def changed(prefs: UserPreferences, m: AppModel): Option[UserPreferences] =
    val now = shown(m)
    Option.when(now != prefs.appearance)(prefs.withAppearance(now))

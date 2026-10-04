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
import eyes4s.studio.core.document.Theme
import eyes4s.studio.core.preferences.{AppearanceChoice, UserPreferences}

/** The appearance as a user preference (ticket S2.8; S1.10's View ›
  * Appearance; lead's decision on bead S2.8):
  *
  *  - System follows the platform's theme. It is a preference only, never a
  *    document field, and is applied when a window opens.
  *  - Light or Dark is the theme a NEW project starts with
  *    ([[newProjectTheme]]); a project that opens keeps its own document
  *    theme, so a fixed preference is not applied at boot.
  *
  * The preference changes only when the user chooses an appearance
  * ([[chosen]] on `Intent.SetAppearance`): never when a window opens, a
  * project opens with another theme, or an undo changes the theme shown.
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

  /** The preferences after the user dispatched `intent`: `Some` only for
    * an explicit View › Appearance choice (even of the appearance already
    * preferred, so a host can retry a save that failed).
    */
  def chosen(prefs: UserPreferences, intent: Intent): Option[UserPreferences] =
    intent match
      case Intent.SetAppearance(a) => Some(prefs.withAppearance(toChoice(a)))
      case _                       => None

  /** The theme a new project starts with: the fixed preference, or the
    * platform's theme `system` when the preference is to follow it.
    */
  def newProjectTheme(prefs: UserPreferences, system: Theme): Theme =
    prefs.appearance match
      case AppearanceChoice.Light  => Theme.Light
      case AppearanceChoice.Dark   => Theme.Dark
      case AppearanceChoice.System => system

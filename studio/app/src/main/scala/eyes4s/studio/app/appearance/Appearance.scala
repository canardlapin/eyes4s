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

import eyes4s.studio.core.document.Theme

/** View › Appearance (ticket S1.10): a fixed theme, or the platform's. */
enum Appearance derives CanEqual:
  case Light, Dark, System

/** Whether the studio follows the platform's theme, and the platform's
  * theme as last reported. The document's theme (`presentation.theme`) is the
  * user's last fixed choice; while following the platform the studio shows
  * `system` instead, and records nothing.
  */
final case class AppearanceState(followSystem: Boolean, system: Theme) derives CanEqual:

  /** The appearance the menu shows checked, given the document's theme. */
  def shown(theme: Theme): Appearance =
    if followSystem then Appearance.System
    else
      theme match
        case Theme.Light => Appearance.Light
        case Theme.Dark  => Appearance.Dark

  /** The theme shown, given the document's. */
  def effective(theme: Theme): Theme = if followSystem then system else theme

  /** The theme `appearance` asks for. */
  def themeFor(appearance: Appearance): Theme = appearance match
    case Appearance.Light  => Theme.Light
    case Appearance.Dark   => Theme.Dark
    case Appearance.System => system

object AppearanceState:

  /** A fixed theme, the platform's assumed light until it reports. */
  val initial: AppearanceState = AppearanceState(followSystem = false, Theme.Light)

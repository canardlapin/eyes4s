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

package eyes4s.studio.desktop

import eyes4s.studio.app.tokens.Theme as TokenTheme
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.document.Theme
import javafx.application.{ColorScheme, Platform}
import javafx.beans.value.ChangeListener
import scaladock.fx.{Dock, DockTheme}

/** A window's theme (ticket S1.10): it follows the document's theme, restyling
  * the window, its dock with the theme's generated `DockTheme.Custom` (so
  * every floating dock window follows), and each scene it was given (the
  * import wizard); and it reports the platform's colour scheme for View ›
  * Appearance › System. Use on the JavaFX thread.
  */
final class ThemeHost(root: javafx.scene.Parent, dock: Dock, app: Intent => Unit):
  private var shown: Option[Theme]                 = None
  private var scenes                               = Vector.empty[javafx.scene.Scene]
  private val preferences                          = Platform.getPreferences
  private val schemes: ChangeListener[ColorScheme] = (_, _, s) => app(ThemeHost.reported(s))

  /** The theme the window shows now. */
  def theme: Option[Theme] = shown

  /** Start reporting the platform's scheme: now, then on every change. */
  def start(): Unit =
    preferences.colorSchemeProperty.addListener(schemes)
    app(ThemeHost.reported(preferences.getColorScheme))

  /** Follow the model's theme. */
  def sync(m: AppModel): Unit = show(m.document.presentation.theme)

  /** Style `scene` in the window's theme, now and on every change. */
  def register(scene: javafx.scene.Scene): Unit =
    scenes :+= scene
    shown.foreach(t => ThemeHost.sheets(t).foreach(s => scene.getStylesheets.setAll(s*)))

  private def show(t: Theme): Unit =
    if !shown.contains(t) then
      (ThemeHost.sheets(t), ThemeHost.dockTheme(t)) match
        case (Right(sheets), Right(dockTheme)) =>
          root.getStylesheets.setAll(sheets*)
          scenes.foreach(_.getStylesheets.setAll(sheets*))
          dock.setTheme(dockTheme)
          shown = Some(t)
        case (sheets, dockTheme) =>
          // The resources ship with the studio (TokenFilesSuite): a missing
          // one leaves the window as it is and says which.
          (sheets.left.toOption ++ dockTheme.left.toOption)
            .foreach(m => System.err.println(m.message))

  def dispose(): Unit = preferences.colorSchemeProperty.removeListener(schemes)

object ThemeHost:

  /** The platform's scheme as the document's theme. */
  def reported(scheme: ColorScheme): Intent =
    Intent.SystemTheme(if scheme == ColorScheme.DARK then Theme.Dark else Theme.Light)

  def tokenTheme(t: Theme): TokenTheme = t match
    case Theme.Light => TokenTheme.Light
    case Theme.Dark  => TokenTheme.Dark

  /** The window's stylesheets in `t`. */
  def sheets(t: Theme): Either[MissingStylesheet, List[String]] =
    StudioStyles.stylesheets(tokenTheme(t))

  /** The dock's theme in `t`: the generated sheet that imports the theme's
    * tokens and maps scaladock's variables onto them.
    */
  def dockTheme(t: Theme): Either[MissingStylesheet, DockTheme] =
    val resource = tokens.TokenFiles.dockStylesheetResource(tokenTheme(t))
    Option(getClass.getClassLoader.getResource(resource))
      .toRight(MissingStylesheet(resource))
      .map(url => DockTheme.Custom(url.toExternalForm))

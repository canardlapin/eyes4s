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

import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.desktop.tokens.TokenFiles

/** A studio stylesheet that is not on the classpath. */
final case class MissingStylesheet(resource: String):
  def message: String = s"stylesheet $resource is not on the classpath"

/** The stylesheets a studio scene loads, in order: the theme's tokens
  * (S1.1), the type scale (S1.2), the icons (S1.3) and the shell (S1.4).
  */
object StudioStyles:

  /** The classpath resource of the icon stylesheet. */
  val iconStylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-icons.css"

  /** The classpath resource of the shell stylesheet (S1.4, S1.5a). */
  val shellStylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-shell.css"

  /** The classpath resources of `theme`'s stylesheets. */
  def resources(theme: Theme): List[String] =
    List(
      TokenFiles.stylesheetResource(theme),
      TokenFiles.typeStylesheetResource,
      iconStylesheetResource,
      shellStylesheetResource
    )

  /** The stylesheet URLs of `theme`, for `Scene.getStylesheets`. */
  def stylesheets(theme: Theme): Either[MissingStylesheet, List[String]] =
    resources(theme).foldRight(Right(Nil): Either[MissingStylesheet, List[String]]) {
      (r, acc) =>
        Option(getClass.getClassLoader.getResource(r)) match
          case None      => Left(MissingStylesheet(r))
          case Some(url) => acc.map(url.toExternalForm :: _)
    }

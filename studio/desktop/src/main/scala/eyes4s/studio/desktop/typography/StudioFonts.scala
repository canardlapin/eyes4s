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

package eyes4s.studio.desktop.typography

import eyes4s.studio.app.tokens.{FontFace, TypeFamily, TypeScale}
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.scene.text.Font

/** Why a bundled face did not load. Each case names the face and its resource. */
enum FontLoadProblem derives CanEqual:
  case MissingResource(face: FontFace, resource: String)
  case Rejected(face: FontFace, resource: String)
  case WrongFamily(face: FontFace, resource: String, expected: String, actual: String)

  def message: String = this match
    case MissingResource(f, r)   => s"font $f: resource $r is not on the classpath"
    case Rejected(f, r)          => s"font $f: JavaFX could not read $r"
    case WrongFamily(f, r, e, a) =>
      s"font $f: $r loaded as family '$a', not '$e'; CSS naming '$e' would not find it"

/** The bundled static faces (ticket S1.2): IBM Plex Sans 400/500/600, IBM Plex
  * Mono 400/500 and Source Serif 4 400/600, under `fonts/` next to the
  * stylesheets, with their SIL Open Font Licences and `SOURCES.md`.
  *
  * `Font.loadFont` registers a face with JavaFX, after which CSS finds it by
  * its family (`studio-type.css`). Load them once, before the first scene.
  */
object StudioFonts:

  /** The classpath directory of the fonts, their licences and their sources. */
  val directory: String = s"${TokenFiles.resourceDirectory}/fonts"

  /** The classpath resource of a face. */
  def resource(face: FontFace): String = s"$directory/${face.fileName}"

  /** The classpath resource of a family's licence (SIL OFL 1.1). */
  def licenceResource(family: TypeFamily): String = family match
    case TypeFamily.Sans  => s"$directory/OFL-IBM-Plex-Sans.txt"
    case TypeFamily.Mono  => s"$directory/OFL-IBM-Plex-Mono.txt"
    case TypeFamily.Serif => s"$directory/OFL-Source-Serif-4.txt"

  /** Where each face was downloaded from, with its SHA-256. */
  val sourcesResource: String = s"$directory/SOURCES.md"

  /** Loads one face and checks that JavaFX names it as the stylesheet does. */
  def load(face: FontFace): Either[FontLoadProblem, Font] =
    val name = resource(face)
    Option(getClass.getClassLoader.getResourceAsStream(name)) match
      case None     => Left(FontLoadProblem.MissingResource(face, name))
      case Some(in) =>
        val font =
          try Option(Font.loadFont(in, TypeScale.body.px.toDouble))
          finally in.close()
        font match
          case None => Left(FontLoadProblem.Rejected(face, name))
          case Some(f) if f.getFamily != face.javaFxFamily =>
            Left(FontLoadProblem.WrongFamily(face, name, face.javaFxFamily, f.getFamily))
          case Some(f) => Right(f)

  /** Loads every bundled face; the problems of those that did not load. */
  def loadAll(): List[FontLoadProblem] =
    FontFace.values.toList.flatMap(face => load(face).left.toOption)

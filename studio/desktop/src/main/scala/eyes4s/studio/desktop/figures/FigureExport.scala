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

package eyes4s.studio.desktop.figures

import eyes4s.studio.app.figures.{ExportFormat, PageVM}
import eyes4s.studio.app.tokens.FontFace
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.viz.figure.{EmbeddedFont, FigureSvg}

import java.nio.charset.StandardCharsets.UTF_8

/** The bundled faces as export fonts, under the families the figure's text
  * names (`FontFace.javaFxFamily`), read from the classpath.
  */
object FigureFonts:
  def bundled: Either[String, Vector[EmbeddedFont]] =
    FontFace.values.toVector
      .foldLeft[Either[String, Vector[EmbeddedFont]]](Right(Vector.empty)) { (acc, face) =>
        acc.flatMap { fonts =>
          val resource = StudioFonts.resource(face)
          Option(getClass.getClassLoader.getResourceAsStream(resource)) match
            case None     => Left(s"the font resource $resource is not on the classpath")
            case Some(in) =>
              val bytes =
                try IArray.unsafeFromArray(in.readAllBytes())
                finally in.close()
              EmbeddedFont.of(face.javaFxFamily, bytes).map(fonts :+ _)
        }
      }

/** A figure page written as a file of one format (ticket S9.3). */
object FigureExport:

  /** The bytes of `page` as `format`, with the bundled fonts embedded. */
  def render(format: ExportFormat, page: PageVM): Either[String, IArray[Byte]] =
    FigureFonts.bundled.flatMap { fonts =>
      format match
        case ExportFormat.Svg =>
          FigureSvg
            .render(page, fonts)
            .left
            .map(_.message)
            .map(s => IArray.unsafeFromArray(s.getBytes(UTF_8)))
        case ExportFormat.Pdf | ExportFormat.Png =>
          Left(s"${format.label} export needs the Intaglio PDF and Java2D modules at the pin.")
    }

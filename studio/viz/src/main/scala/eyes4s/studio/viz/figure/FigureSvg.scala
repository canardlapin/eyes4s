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

package eyes4s.studio.viz.figure

import eyes4s.studio.app.figures.PageVM
import intaglio.svg.SvgRenderer

import java.util.Base64

/** A font face's TrueType bytes under the family name the page's text uses
  * (`FontFace.javaFxFamily`), to embed in an export.
  */
final class EmbeddedFont private (val family: String, val bytes: IArray[Byte]):
  override def toString: String = s"EmbeddedFont($family, ${bytes.length} bytes)"

object EmbeddedFont:
  def of(family: String, bytes: IArray[Byte]): Either[String, EmbeddedFont] =
    if family.trim.isEmpty then Left("an embedded font needs a family name")
    else if bytes.isEmpty then Left(s"the font $family has no bytes")
    else Right(new EmbeddedFont(family.trim, bytes))

/** Why an SVG export failed. */
enum FigureSvgError derives CanEqual:
  case Page(error: FigurePageError)
  case Render(details: String)

  /** The SVG names a family no supplied font provides. */
  case MissingFont(family: String, supplied: Vector[String])

  def message: String = this match
    case Page(e)                       => e.message
    case Render(d)                     => s"The SVG renderer refused the page: $d"
    case MissingFont(family, supplied) =>
      s"The figure uses the font $family, which is not among the embedded fonts " +
        s"(${supplied.mkString(", ")})."

/** The figure page as SVG (ticket S9.3): the page's own scene at 96 px per
  * inch, so its size is the page's in millimetres, with every font family
  * its text uses embedded as an `@font-face` data URL, so the file renders
  * the same anywhere.
  */
object FigureSvg:
  val PixelsPerInch: Double = 96.0

  private val familyAttribute = """font-family="([^"]+)"""".r

  /** The families the SVG's text names, in first-use order. */
  def families(svg: String): Vector[String] =
    familyAttribute.findAllMatchIn(svg).map(_.group(1)).toVector.distinct

  def render(page: PageVM, fonts: Vector[EmbeddedFont]): Either[FigureSvgError, String] =
    for
      built <- FigurePage.build(page).left.map(FigureSvgError.Page(_))
      plan  <- built.plan(PixelsPerInch).left.map(FigureSvgError.Page(_))
      doc   <- SvgRenderer
        .render(plan, Some(page.title))
        .left
        .map(e => FigureSvgError.Render(e.message))
      svg <- embed(doc.value, fonts)
    yield svg

  /** `svg` with an `@font-face` for each family it names, from `fonts`. */
  def embed(svg: String, fonts: Vector[EmbeddedFont]): Either[FigureSvgError, String] =
    val used = families(svg)
    used
      .foldLeft[Either[FigureSvgError, Vector[EmbeddedFont]]](Right(Vector.empty)) {
        (acc, family) =>
          acc.flatMap(found =>
            fonts
              .find(_.family.equalsIgnoreCase(family))
              .toRight(FigureSvgError.MissingFont(family, fonts.map(_.family)))
              .map(found :+ _)
          )
      }
      .map { faces =>
        if faces.isEmpty then svg
        else
          val css = faces.map { f =>
            val data = Base64.getEncoder.encodeToString(Array.from(f.bytes))
            s"@font-face{font-family:\"${f.family}\";src:url(data:font/ttf;base64,$data) format(\"truetype\");}"
          }
          val style = s"<defs><style>${css.mkString}</style></defs>"
          val open  = svg.indexOf('>', svg.indexOf("<svg"))
          svg.substring(0, open + 1) + style + svg.substring(open + 1)
      }

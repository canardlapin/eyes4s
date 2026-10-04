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
import cats.syntax.all.*
import eyes4s.studio.core.assets.AssetRef
import eyes4s.studio.desktop.trial.{StimulusSource, Stimuli}
import eyes4s.studio.viz.figure.{EmbeddedFont, FigureGaze, FigurePage, FigureSvg}
import eyes4s.studio.viz.trial.StimulusRaster
import intaglio.Rgba
import intaglio.java2d.{Java2DBackground, Java2DExportOptions, Java2DRenderer}
import intaglio.pdf.{PdfFont, PdfFontCatalog, PdfOptions, PdfRenderer}

import java.nio.charset.StandardCharsets.UTF_8
import scala.util.control.NonFatal

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

/** The figure page as PDF (ticket S9.3): vector shapes and text, every face
  * embedded and subset by PDFBox (intaglio-pdf), the page its journal size.
  */
object FigurePdf:
  /** The page is its size in millimetres rounded up to whole pixels at this
    * density, so it is at most one pixel, 25.4 / 720 ≈ 0.035 mm, larger.
    */
  val PixelsPerInch: Double = 720.0

  def render(
      page: PageVM,
      fonts: Vector[EmbeddedFont],
      rasters: Map[AssetRef, StimulusRaster]
  ): Either[String, Array[Byte]] =
    for
      faces <- fonts
        .traverse(f => PdfFont.fromBytes(f.family, Array.from(f.bytes)))
        .left
        .map(_.message)
      catalog <- faces match
        case first +: rest => PdfFontCatalog.from(first, rest*).left.map(_.message)
        case _             => Left("A PDF needs at least one font to embed.")
      built <- FigurePage.build(page, rasters).left.map(_.message)
      plan  <- built.plan(PixelsPerInch).left.map(_.message)
      doc   <- PdfRenderer
        .render(plan, catalog, PdfOptions(title = Some(page.title)))
        .left
        .map(_.message)
    yield doc.bytes

/** The figure page as PNG (ticket S9.3): drawn by Java2D at print density on
  * white paper, with the bundled faces registered so the text is set in them.
  */
object FigurePng:
  val PixelsPerInch: Double = 300.0

  /** Registers the faces with AWT once; the families each answers to. */
  private lazy val registered: Either[String, Set[String]] =
    // Java2D draws off screen; AWT stays off the display JavaFX owns.
    if System.getProperty("java.awt.headless") == null then
      System.setProperty("java.awt.headless", "true"): Unit
    FigureFonts.bundled.flatMap(_.traverse { f =>
      try
        val font = java.awt.Font.createFont(
          java.awt.Font.TRUETYPE_FONT,
          java.io.ByteArrayInputStream(Array.from(f.bytes))
        )
        java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(font): Unit
        Right(Set(font.getFamily, font.getFontName))
      catch case NonFatal(e) => Left(s"The font ${f.family} could not be read: ${e.getMessage}")
    }.map(_.flatten.toSet))

  /** Why a page whose text names `used` cannot be drawn with the faces AWT
    * answers to (`registered`): the first family it lacks, which Java2D
    * would otherwise draw in a fallback face without a word.
    */
  def unregistered(used: Vector[String], registered: Set[String]): Option[String] =
    used
      .find(f => !registered.contains(f))
      .map(f => s"The figure uses the font $f, which Java2D does not have.")

  def render(
      page: PageVM,
      rasters: Map[AssetRef, StimulusRaster]
  ): Either[String, Array[Byte]] =
    for
      names <- registered
      built <- FigurePage.build(page, rasters).left.map(_.message)
      plan  <- built.plan(PixelsPerInch).left.map(_.message)
      used  <- FigureSvg.families(built).left.map(_.message)
      _     <- unregistered(used, names).toLeft(())
      png   <- Java2DRenderer
        .renderPng(
          plan,
          Java2DExportOptions(background = Java2DBackground.Solid(Rgba.White))
        )
        .left
        .map(_.message)
    yield png

/** A figure page written as a file of one format (ticket S9.3). */
object FigureExport:

  /** The bytes of `page` as `format`, with the bundled fonts embedded and
    * the gaze panels' displays drawn from `rasters`.
    */
  def render(
      format: ExportFormat,
      page: PageVM,
      rasters: Map[AssetRef, StimulusRaster] = Map.empty
  ): Either[String, IArray[Byte]] =
    FigureFonts.bundled.flatMap { fonts =>
      format match
        case ExportFormat.Svg =>
          FigureSvg
            .render(page, fonts, rasters)
            .left
            .map(_.message)
            .map(s => IArray.unsafeFromArray(s.getBytes(UTF_8)))
        case ExportFormat.Pdf =>
          FigurePdf.render(page, fonts, rasters).map(IArray.unsafeFromArray)
        case ExportFormat.Png => FigurePng.render(page, rasters).map(IArray.unsafeFromArray)
    }

  /** The decoded images of the stored assets `page`'s gaze panels display. */
  def rasters(page: PageVM, stimuli: StimulusSource): Map[AssetRef, StimulusRaster] =
    FigureGaze.assets(page).map(a => a -> Stimuli.load(stimuli, a)).toMap

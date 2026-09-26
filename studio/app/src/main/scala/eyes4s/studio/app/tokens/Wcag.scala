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

package eyes4s.studio.app.tokens

/** Why a colour measurement could not be made. */
enum WcagError derives CanEqual:

  /** A background must be opaque: what shows through it is unknown. */
  case TranslucentBackground(background: Colour)

  /** Lightness is defined for an opaque colour only. */
  case TranslucentColour(colour: Colour)

  def message: String = this match
    case TranslucentBackground(b) =>
      s"background ${b.webCss} is translucent; composite it over an opaque colour first"
    case TranslucentColour(c) =>
      s"colour ${c.webCss} is translucent; composite it over an opaque colour first"

/** WCAG 2.2 contrast and CIE lightness for the token checks (ticket S1.1).
  *
  * Contrast follows the WCAG definition of relative luminance and contrast
  * ratio; lightness is CIE L* (D65) of the same luminance. A translucent
  * foreground is composited over its background in sRGB, as a browser or
  * JavaFX draws it.
  */
object Wcag:

  /** SC 1.4.3: body text needs 4.5:1. */
  val TextMinimum: Double = 4.5

  /** SC 1.4.3: large text needs 3:1. */
  val LargeTextMinimum: Double = 3.0

  /** The size, in CSS pixels, from which text counts as large (18pt). */
  val LargeTextPx: Int = 24

  /** SC 1.4.11: graphical objects need 3:1 against what is adjacent. */
  val NonTextMinimum: Double = 3.0

  /** The minimum contrast for text of `sizePx`. */
  def textMinimum(sizePx: Int): Double =
    if sizePx >= LargeTextPx then LargeTextMinimum else TextMinimum

  /** The contrast ratio of `foreground` drawn over `background`, 1 to 21. */
  def contrast(foreground: Colour, background: Colour): Either[WcagError, Double] =
    if !background.isOpaque then Left(WcagError.TranslucentBackground(background))
    else
      val a = luminance(composite(foreground, background))
      val b = luminance(channels(background))
      Right((math.max(a, b) + 0.05) / (math.min(a, b) + 0.05))

  /** WCAG relative luminance of an opaque colour, 0 to 1. */
  def relativeLuminance(colour: Colour): Either[WcagError, Double] =
    if colour.isOpaque then Right(luminance(channels(colour)))
    else Left(WcagError.TranslucentColour(colour))

  /** CIE L* of an opaque colour, 0 (black) to 100 (white). */
  def lightness(colour: Colour): Either[WcagError, Double] =
    relativeLuminance(colour).map { y =>
      val epsilon = 216.0 / 24389.0
      val kappa   = 24389.0 / 27.0
      if y > epsilon then 116.0 * math.cbrt(y) - 16.0 else kappa * y
    }

  private type Rgb = (Double, Double, Double)

  private def channels(c: Colour): Rgb = (c.red / 255.0, c.green / 255.0, c.blue / 255.0)

  private def composite(fg: Colour, bg: Colour): Rgb =
    val a                                 = fg.alphaPercent / 100.0
    val (fr, fgG, fb)                     = channels(fg)
    val (br, bgG, bb)                     = channels(bg)
    def mix(f: Double, b: Double): Double = f * a + b * (1.0 - a)
    (mix(fr, br), mix(fgG, bgG), mix(fb, bb))

  private def linear(c: Double): Double =
    if c <= 0.04045 then c / 12.92 else math.pow((c + 0.055) / 1.055, 2.4)

  private def luminance(rgb: Rgb): Double =
    val (r, g, b) = rgb
    0.2126 * linear(r) + 0.7152 * linear(g) + 0.0722 * linear(b)

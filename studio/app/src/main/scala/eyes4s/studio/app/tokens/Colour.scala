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

import scala.annotation.publicInBinary
import scala.compiletime.error

/** An sRGB colour: three 8-bit channels and a straight (unpremultiplied) alpha
  * in whole percent, `0` transparent to `100` opaque.
  *
  * Colours are built only by [[Colour.srgb]] and [[Colour.srgba]], which reject
  * an out-of-range literal at compile time, so every value is a valid colour
  * and the token source cannot hold a malformed one. Alpha is a whole percent
  * rather than a `Double` so that the CSS the token source generates is exact
  * and stable (`.35`, never `0.35000000000000003`).
  */
final case class Colour private (red: Int, green: Int, blue: Int, alphaPercent: Int)
    derives CanEqual:

  /** True when the colour covers what lies beneath it completely. */
  def isOpaque: Boolean = alphaPercent == Colour.OpaquePercent

  /** The colour channels as `#RRGGBB`, upper case; alpha is not included. */
  def hexRgb: String = f"#$red%02X$green%02X$blue%02X"

  /** The CSS value in the form the design boards write it: `#RRGGBB` when
    * opaque, `rgba(r,g,b,.aa)` otherwise.
    */
  def webCss: String =
    if isOpaque then hexRgb else s"rgba($red,$green,$blue,${Colour.webAlpha(alphaPercent)})"

  /** The value as JavaFX CSS reads it: `#RRGGBB` when opaque,
    * `rgba(r,g,b,0.aa)` otherwise.
    */
  def javaFxCss: String =
    if isOpaque then hexRgb else s"rgba($red,$green,$blue,${Colour.fxAlpha(alphaPercent)})"

object Colour:

  /** The alpha of an opaque colour, in percent. */
  val OpaquePercent: Int = 100

  /** An opaque colour from a 24-bit `0xRRGGBB` literal.
    *
    * The argument must be a constant between `0x000000` and `0xFFFFFF`; any
    * other value is a compile error, not a runtime failure.
    */
  inline def srgb(inline rgb: Int): Colour =
    inline if rgb < 0 || rgb > 0xffffff then
      error("Colour.srgb: the literal must lie in 0x000000..0xFFFFFF")
    else fromChecked(rgb, OpaquePercent)

  /** A colour from a 24-bit `0xRRGGBB` literal and a whole-percent alpha.
    *
    * Both arguments must be constants: the colour in `0x000000..0xFFFFFF`, the
    * alpha in `0..100`. Anything else is a compile error.
    */
  inline def srgba(inline rgb: Int, inline alphaPercent: Int): Colour =
    inline if rgb < 0 || rgb > 0xffffff then
      error("Colour.srgba: the literal must lie in 0x000000..0xFFFFFF")
    else inline if alphaPercent < 0 || alphaPercent > 100 then
      error("Colour.srgba: alpha is a whole percent in 0..100")
    else fromChecked(rgb, alphaPercent)

  // Reached only from the inline constructors above, after their compile-time
  // range checks; public in binary so that the inlined call sites can link it.
  @publicInBinary private[Colour] def fromChecked(rgb: Int, alphaPercent: Int): Colour =
    new Colour((rgb >> 16) & 0xff, (rgb >> 8) & 0xff, rgb & 0xff, alphaPercent)

  // `.35` as the boards write it; `1` and `0` stay whole.
  private def webAlpha(percent: Int): String =
    if percent == 0 then "0"
    else if percent == OpaquePercent then "1"
    else "." + f"$percent%02d".reverse.dropWhile(_ == '0').reverse

  private def fxAlpha(percent: Int): String =
    if percent == 0 || percent == OpaquePercent then webAlpha(percent)
    else "0" + webAlpha(percent)

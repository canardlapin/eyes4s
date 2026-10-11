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

/** The grey a colour prints as: Rec. 709 relative luminance, computed in
  * linear light and encoded back to sRGB (the Figures board's greyscale
  * check, S9.3).
  *
  * Each sRGB channel is decoded to linear light, weighted
  * `Y = 0.2126 R + 0.7152 G + 0.0722 B`, and `Y` is encoded with the sRGB
  * transfer function. A neutral grey maps to itself. A saturation-only
  * desaturation (HSB brightness, roughly the largest channel) would print the
  * figure's matched and control marks as nearly the same grey.
  */
object Greyscale:

  /** One sRGB-encoded channel, 0 to 1, in linear light, 0 to 1. */
  def toLinear(encoded: Double): Double =
    if encoded <= 0.04045 then encoded / 12.92 else math.pow((encoded + 0.055) / 1.055, 2.4)

  /** One linear-light value, 0 to 1, sRGB-encoded, 0 to 1. */
  def toEncoded(linear: Double): Double =
    if linear <= 0.0031308 then 12.92 * linear
    else 1.055 * math.pow(linear, 1.0 / 2.4) - 0.055

  /** The Rec. 709 relative luminance of 8-bit sRGB channels, 0 to 1. */
  def luminance(red: Int, green: Int, blue: Int): Double =
    0.2126 * linear8(red) + 0.7152 * linear8(green) + 0.0722 * linear8(blue)

  /** The 8-bit sRGB grey of 8-bit sRGB channels (0 to 255 each). */
  def grey(red: Int, green: Int, blue: Int): Int =
    encode8(luminance(red, green, blue))

  /** The 8-bit sRGB grey of a colour's channels; its alpha is not used. */
  def grey(colour: Colour): Int = grey(colour.red, colour.green, colour.blue)

  /** A non-premultiplied `0xAARRGGBB` pixel as its grey, alpha kept. */
  def argb(pixel: Int): Int =
    val g = grey((pixel >>> 16) & 0xff, (pixel >>> 8) & 0xff, pixel & 0xff)
    (pixel & 0xff000000) | (g << 16) | (g << 8) | g

  // The linear light of each 8-bit level.
  private val linearLevels: Array[Double] = Array.tabulate(256)(i => toLinear(i / 255.0))

  // The linear light at the boundary between level i and i + 1: encoding then
  // rounding is a count of the boundaries below the value.
  private val boundaries: Array[Double] = Array.tabulate(255)(i => toLinear((i + 0.5) / 255.0))

  private def linear8(level: Int): Double = linearLevels(level & 0xff)

  private def encode8(linear: Double): Int =
    var lo = 0
    var hi = boundaries.length
    while lo < hi do
      val mid = (lo + hi) >>> 1
      if boundaries(mid) <= linear then lo = mid + 1 else hi = mid
    lo

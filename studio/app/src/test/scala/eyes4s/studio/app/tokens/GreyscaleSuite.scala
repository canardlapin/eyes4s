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

/** The greyscale check's conversion: Rec. 709 luminance in linear light
  * (S9.3). Runs on the JVM and on Scala.js.
  */
class GreyscaleSuite extends munit.FunSuite:

  /** Two role marks that must stay apart in print differ by at least this
    * much of the sRGB grey range (13 of 255 levels).
    */
  private val DistinctGreyStep = 0.05

  /** A grey is the exact value rounded to a whole 8-bit level. */
  private val Rounding = 0.5 + 1e-9

  /** Floating-point agreement of the two transfer functions. */
  private val Exact = 1e-12

  private def fraction(level: Int): Double = level / 255.0

  test("a neutral grey is its own grey; black and white are fixed") {
    (0 to 255).foreach(c => assertEquals(Greyscale.grey(c, c, c), c, c))
  }

  test("the transfer functions invert each other") {
    (0 to 255).foreach { c =>
      val v = fraction(c)
      assertEqualsDouble(Greyscale.toEncoded(Greyscale.toLinear(v)), v, Exact, c)
    }
  }

  test("the primaries carry the Rec. 709 weights, in linear light") {
    def expected(y: Double) = math.round(Greyscale.toEncoded(y) * 255).toInt
    assertEquals(Greyscale.grey(255, 0, 0), expected(0.2126))
    assertEquals(Greyscale.grey(0, 255, 0), expected(0.7152))
    assertEquals(Greyscale.grey(0, 0, 255), expected(0.0722))
    // Averaging the encoded channels would give 85 for each.
    assertEquals(Greyscale.grey(255, 0, 0), 127)
    assertEquals(Greyscale.grey(0, 255, 0), 220)
    assertEquals(Greyscale.grey(0, 0, 255), 76)
  }

  test("the figure's matched and control marks print as distinct greys") {
    val matched = Greyscale.grey(Tokens.palette(PaletteToken.FigMatch))
    val control = Greyscale.grey(Tokens.palette(PaletteToken.FigControl))
    val query   = Greyscale.grey(Tokens.palette(PaletteToken.FigQuery))
    val fill    = Greyscale.grey(Tokens.palette(PaletteToken.FigControlFill))
    assert(
      math.abs(fraction(matched) - fraction(control)) >= DistinctGreyStep,
      s"match prints as $matched, control as $control"
    )
    assert(
      math.abs(fraction(matched) - fraction(fill)) >= DistinctGreyStep,
      s"match prints as $matched, control fill as $fill"
    )
    assert(
      math.abs(fraction(query) - fraction(matched)) >= DistinctGreyStep,
      s"query prints as $query, match as $matched"
    )
    // The largest channel, which a saturation-only desaturation keeps, does
    // not separate them: this is the check the conversion replaces.
    def brightest(t: PaletteToken) =
      val c = Tokens.palette(t)
      math.max(c.red, math.max(c.green, c.blue))
    val before = math.abs(brightest(PaletteToken.FigMatch) - brightest(PaletteToken.FigControl))
    assert(fraction(before) < DistinctGreyStep, before)
  }

  test("a figure tile's hatch prints as two distinct greys, stripes darker") {
    // The hatch is legible in the greyscale proof only while its stripes and
    // their ground print apart (slice S27).
    val stripes = Greyscale.grey(Tokens.palette(PaletteToken.FigHatch))
    val ground  = Greyscale.grey(Tokens.palette(PaletteToken.FigHatchGround))
    assert(
      fraction(ground) - fraction(stripes) >= DistinctGreyStep,
      s"stripes print as $stripes, their ground as $ground"
    )
  }

  test("a pixel keeps its alpha and takes its grey in every channel") {
    val c = Tokens.palette(PaletteToken.FigMatch)
    val g = Greyscale.grey(c)
    val p = 0x80000000 | (c.red << 16) | (c.green << 8) | c.blue
    assertEquals(Greyscale.argb(p), 0x80000000 | (g << 16) | (g << 8) | g)
  }

  test("the grey is the sRGB encoding of the luminance, rounded to a level") {
    for
      r <- 0 to 255 by 15
      g <- 0 to 255 by 17
      b <- 0 to 255 by 51
    do
      val exact = Greyscale.toEncoded(Greyscale.luminance(r, g, b)) * 255
      assert(math.abs(Greyscale.grey(r, g, b) - exact) <= Rounding, (r, g, b, exact))
  }

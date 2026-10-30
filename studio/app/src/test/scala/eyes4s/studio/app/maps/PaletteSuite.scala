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

package eyes4s.studio.app.maps

import eyes4s.studio.app.tokens.{Colour, PaletteToken, Tokens}

/** The map palettes (ticket S4.4; the System board's map ramps): the
  * magenta mass ramp, fading in below 10% of its limits (review ticket 8),
  * the BlueRust difference ramp opaque with zero pinned, missing drawn
  * differently from zero, checked colour limits tied to their palette,
  * shared and per-panel limits, and the one global opacity, applied when
  * drawing, which alters no stored value.
  */
class PaletteSuite extends munit.FunSuite:
  import MapSamples.*

  private def packed(c: Colour): Int = (0xff << 24) | (c.red << 16) | (c.green << 8) | c.blue
  private def token(t: PaletteToken): Int  = packed(Tokens.palette(t))
  private def alpha(argb: Int): Int        = (argb >>> 24) & 0xff
  private def luminance(argb: Int): Double =
    0.2126 * ((argb >> 16) & 0xff) + 0.7152 * ((argb >> 8) & 0xff) + 0.0722 * (argb & 0xff)

  private def rgb(argb: Int): Int = argb & 0x00ffffff

  test("the mass ramp runs from ramp-0 to ramp-3 through its stops, darker with mass") {
    def at(v: Double) = MapColours.argb(massStyle, Some(v))
    assertEquals(rgb(at(0.0)), rgb(token(PaletteToken.Ramp0)))
    assertEquals(at(1.0 / 3.0), token(PaletteToken.Ramp1))
    assertEquals(at(2.0 / 3.0), token(PaletteToken.Ramp2))
    assertEquals(at(1.0), token(PaletteToken.Ramp3))
    val ramp = (0 to 30).map(k => at(k / 30.0))
    ramp.zip(ramp.tail).foreach((a, b) => assert(luminance(b) <= luminance(a), s"$a then $b"))
    // Beyond its limits a value takes the end colour.
    assertEquals(at(-1.0), at(0.0))
    assertEquals(at(7.0), at(1.0))
  }

  test("a mass cell's alpha rises linearly from zero to opaque at 25% of its limits") {
    // DESIGN_SPEC section 15: alpha = min(1, t / 0.25), t the fraction along
    // the limits; the colour is the ramp's, whatever the alpha.
    val Floor = 0.25
    Vector((massStyle, 0.0, 1.0), (mass(2.0, 4.0), 2.0, 4.0)).foreach { (style, lo, hi) =>
      def at(t: Double) = MapColours.argb(style, Some(lo + t * (hi - lo)))
      assertEquals(alpha(at(0.0)), 0, style)
      assertEquals(alpha(at(-0.5)), 0, style)
      assertEquals(alpha(at(Floor / 4.0)), 64, style)
      assertEquals(alpha(at(Floor)), 0xff, style)
      // Linear below the floor: steps of 5 in alpha, away from rounding ties.
      (0 to 51).foreach(k => assertEquals(alpha(at(Floor * k / 51.0)), 5 * k, s"$style, k $k"))
      // Opaque from the floor up.
      (25 to 100).foreach(k => assertEquals(alpha(at(k / 100.0)), 0xff, s"$style at $k%"))
      // The floor fades the colour in; it does not change it.
      assertEquals(rgb(at(0.05)), rgb(MapColours.argb(massStyle, Some(0.05))), style)
    }
  }

  test("difference maps stay opaque: zero sits mid-ramp, so the floor never applies") {
    (-30 to 30).map(k => k / 30.0).foreach { v =>
      assertEquals(alpha(MapColours.argb(differenceStyle, Some(v))), 0xff, s"at $v")
    }
  }

  test("missing covers nothing and is told apart from zero, in both palettes") {
    Vector(massStyle, differenceStyle).foreach { style =>
      val none = MapColours.argb(style, None)
      val zero = MapColours.argb(style, Some(0.0))
      assertEquals(none, MapColours.NoCoverage, style)
      assertEquals(alpha(none), 0, style)
      assertNotEquals(zero, none, style)
    }
    // Zero mass fades out under the floor but keeps its ramp colour; the
    // scenes draw a missing cell hatched, never as a ramp colour.
    assertEquals(MapColours.argb(massStyle, Some(0.0)), rgb(token(PaletteToken.Ramp0)))
    assertEquals(MapColours.argb(differenceStyle, Some(0.0)), token(PaletteToken.DivMid))
  }

  test("BlueRust pins zero to its middle colour at any extent, blue below and rust above") {
    Vector(0.01, 0.5, 3.0, 1e6).foreach { e =>
      def at(v: Double) = MapColours.argb(difference(e), Some(v))
      assertEquals(at(0.0), token(PaletteToken.DivMid), s"extent $e")
      assertEquals(at(-e), token(PaletteToken.DivNeg))
      assertEquals(at(e), token(PaletteToken.DivPos))
    }
    def at(v: Double) = MapColours.argb(differenceStyle, Some(v))
    val (neg, pos)    = (at(-0.5), at(0.5))
    assert((neg & 0xff) > ((neg >> 16) & 0xff), "a negative difference is blue")
    assert(((pos >> 16) & 0xff) > (pos & 0xff), "a positive difference is rust")
  }

  test("limits are checked and name their operands; a palette takes only limits of its kind") {
    assertEquals(ColourLimits.sequential(5.0, 1.0), Left(LimitsError.Unordered(5.0, 1.0)))
    assertEquals(ColourLimits.sequential(1.0, 1.0), Left(LimitsError.Unordered(1.0, 1.0)))
    assertEquals(ColourLimits.symmetric(-1.0), Left(LimitsError.NotPositive(-1.0)))
    assertEquals(ColourLimits.symmetric(0.0), Left(LimitsError.NotPositive(0.0)))
    ColourLimits.sequential(Double.NaN, 1.0) match
      case Left(LimitsError.NotFinite("lower", v)) => assert(v.isNaN)
      case other                                   => fail(s"$other")
    assertEquals(
      ColourLimits.sequential(0.0, Double.PositiveInfinity),
      Left(LimitsError.NotFinite("upper", Double.PositiveInfinity))
    )
    ColourLimits.symmetric(Double.NaN) match
      case Left(LimitsError.NotFinite("extent", v)) => assert(v.isNaN)
      case other                                    => fail(s"$other")
    val sym = right(ColourLimits.symmetric(1.0))
    val seq = right(ColourLimits.sequential(0.0, 1.0))
    assertEquals(
      MapStyle.of(MapPalette.Mass, sym),
      Left(LimitsError.PaletteMismatch(MapPalette.Mass, sym))
    )
    assertEquals(
      MapStyle.of(MapPalette.Difference, seq),
      Left(LimitsError.PaletteMismatch(MapPalette.Difference, seq))
    )
    assert(LimitsError.Unordered(5.0, 1.0).message.contains("from 5"))
  }

  test("colour limits span the panels together when shared, each panel alone otherwise") {
    val small  = grid(id("ret_01", 2), bump.map(_.map(_ * 0.5)))
    val large  = grid(id("ret_02", 2), bump.map(_.map(_ * 2.0)))
    val panels = Vector(small, large)
    val shared = ColourLimits.forPanels(MapPalette.Mass, LimitsScope.Shared, panels)
    assertEquals(shared, Vector.fill(2)(mass(0.0, large.cells.flatten.max)))
    val own = ColourLimits.forPanels(MapPalette.Mass, LimitsScope.PerPanel, panels)
    assertEquals(own, panels.map(g => mass(0.0, g.cells.flatten.max)))
    // Shared limits draw equal values in equal colours across panels.
    val v = Some(0.4)
    assertEquals(MapColours.argb(shared(0), v), MapColours.argb(shared(1), v))
    assertNotEquals(MapColours.argb(own(0), v), MapColours.argb(own(1), v))
    // A difference map's limits are symmetric about zero.
    val d = grid(id("ret_03", 2), signed)
    assertEquals(ColourLimits.spanning(MapPalette.Difference, Vector(d)), difference(0.6))
    // An all-zero map gets the unit range rather than a degenerate one.
    val flat = grid(id("ret_04", 2), Vector.fill(Columns * Rows)(Some(0.0)))
    assertEquals(ColourLimits.spanning(MapPalette.Mass, Vector(flat)), massStyle)
  }

  test("the global opacity defaults to 0.85, lies in [0, 1] and scales a pixel's alpha only") {
    assertEquals(MapOpacity.Default.value, 0.85)
    assertEquals(MapOpacity.of(1.01), None)
    assertEquals(MapOpacity.of(-0.1), None)
    assertEquals(MapOpacity.of(Double.NaN), None)
    val pixel = token(PaletteToken.Ramp2)
    assertEquals(MapOpacity.Default.over(pixel), (217 << 24) | (pixel & 0x00ffffff))
    assertEquals(MapOpacity.of(0.0).get.over(pixel) >>> 24, 0)
    assertEquals(MapOpacity.of(1.0).get.over(pixel), pixel)
    assertEquals(MapOpacity.Default.over(MapColours.NoCoverage), MapColours.NoCoverage)
  }

  test("a palette or opacity change alters no stored value: grid and raster are unchanged") {
    val g      = grid(id("ret_07", 2), bump, Vector(0.31, 0.62))
    val before = g.contentHash
    val styles = for
      p <- MapPalette.values.toVector
      s <- LimitsScope.values.toVector
    yield ColourLimits.forPanels(p, s, Vector(g)).head
    val rasters   = styles.map(MapRaster.render(g, _))
    val stored    = rasters.map(_.argb.toVector)
    val opacities = Vector(MapOpacity.Default, MapOpacity.of(0.2).get, MapOpacity.of(1.0).get)
    for
      r <- rasters
      o <- opacities
    do
      val drawn = r.drawn(o)
      // Drawn at the opacity: every cell's alpha is scaled, its colour kept.
      r.argb.indices.foreach { i =>
        val stored = r.argb(i)
        assertEquals(drawn(i) >>> 24, math.round(alpha(stored) * o.value).toInt)
        assertEquals(rgb(drawn(i)), rgb(stored))
      }
    // Drawing at any opacity changed neither the grid nor the stored rasters.
    assertEquals(g.contentHash, before)
    assertEquals(g.cells, bump)
    assertEquals(g.levels, Vector(0.31, 0.62))
    assertEquals(rasters.map(_.argb.toVector), stored)
    // The palettes do draw it differently.
    assertNotEquals(stored.head, stored.last)
    // The hash tells stored values apart.
    assertNotEquals(grid(id("ret_07", 2), bump.updated(100, Some(0.5))).contentHash, before)
    assertNotEquals(grid(id("ret_07", 2), bump.updated(0, None)).contentHash, before)
    assertNotEquals(grid(id("ret_07", 2), bump, Vector(0.31)).contentHash, before)
  }

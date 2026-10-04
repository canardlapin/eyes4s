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
  * magenta mass ramp opaque per cell, the BlueRust difference ramp with zero
  * pinned, missing drawn differently from zero, shared and per-panel colour
  * limits, the one global opacity, and that no palette or opacity change
  * alters a stored value.
  */
class PaletteSuite extends munit.FunSuite:
  import MapSamples.*

  private def packed(c: Colour): Int = (0xff << 24) | (c.red << 16) | (c.green << 8) | c.blue
  private def token(t: PaletteToken): Int  = packed(Tokens.palette(t))
  private def alpha(argb: Int): Int        = (argb >>> 24) & 0xff
  private def luminance(argb: Int): Double =
    0.2126 * ((argb >> 16) & 0xff) + 0.7152 * ((argb >> 8) & 0xff) + 0.0722 * (argb & 0xff)

  private val mass = ColourLimits.Sequential(0.0, 1.0)

  test("the mass ramp runs from ramp-0 to ramp-3 through its stops, opaque, darker with mass") {
    def at(v: Double) = MapColours.argb(MapPalette.Mass, mass, Some(v))
    assertEquals(at(0.0), token(PaletteToken.Ramp0))
    assertEquals(at(1.0 / 3.0), token(PaletteToken.Ramp1))
    assertEquals(at(2.0 / 3.0), token(PaletteToken.Ramp2))
    assertEquals(at(1.0), token(PaletteToken.Ramp3))
    val ramp = (0 to 30).map(k => at(k / 30.0))
    assert(ramp.forall(alpha(_) == 0xff), "every mass cell is opaque")
    ramp.zip(ramp.tail).foreach((a, b) => assert(luminance(b) <= luminance(a), s"$a then $b"))
    // Beyond its limits a value takes the end colour.
    assertEquals(at(-1.0), at(0.0))
    assertEquals(at(7.0), at(1.0))
  }

  test("missing is transparent and drawn differently from zero, in both palettes") {
    MapPalette.values.foreach { p =>
      val limits = ColourLimits.spanning(p, Vector(grid(id("ret_01", 2), difference)))
      val none   = MapColours.argb(p, limits, None)
      val zero   = MapColours.argb(p, limits, Some(0.0))
      assertEquals(none, MapColours.Transparent, p)
      assertEquals(alpha(none), 0, p)
      assertEquals(alpha(zero), 0xff, p)
      assertNotEquals(zero, none, p)
    }
  }

  test("BlueRust pins zero to its middle colour at any extent, blue below and rust above") {
    Vector(0.01, 0.5, 3.0, 1e6).foreach { e =>
      val limits        = ColourLimits.Symmetric(e)
      def at(v: Double) = MapColours.argb(MapPalette.Difference, limits, Some(v))
      assertEquals(at(0.0), token(PaletteToken.DivMid), s"extent $e")
      assertEquals(at(-e), token(PaletteToken.DivNeg))
      assertEquals(at(e), token(PaletteToken.DivPos))
    }
    def at(v: Double) =
      MapColours.argb(MapPalette.Difference, ColourLimits.Symmetric(1.0), Some(v))
    val (neg, pos) = (at(-0.5), at(0.5))
    assert((neg & 0xff) > ((neg >> 16) & 0xff), "a negative difference is blue")
    assert(((pos >> 16) & 0xff) > (pos & 0xff), "a positive difference is rust")
  }

  test("colour limits span the panels together when shared, each panel alone otherwise") {
    val small  = grid(id("ret_01", 2), bump.map(_.map(_ * 0.5)))
    val large  = grid(id("ret_02", 2), bump.map(_.map(_ * 2.0)))
    val panels = Vector(small, large)
    val shared = ColourLimits.forPanels(MapPalette.Mass, LimitsScope.Shared, panels)
    assertEquals(shared.distinct.size, 1)
    shared.head match
      case ColourLimits.Sequential(lo, hi) =>
        assertEquals(lo, 0.0)
        assertEquals(hi, large.cells.flatten.max)
      case other => fail(s"$other")
    val own = ColourLimits.forPanels(MapPalette.Mass, LimitsScope.PerPanel, panels)
    assertEquals(own, panels.map(g => ColourLimits.Sequential(0.0, g.cells.flatten.max)))
    // Shared limits draw equal values in equal colours across panels.
    val v = Some(0.4)
    assertEquals(
      MapColours.argb(MapPalette.Mass, shared(0), v),
      MapColours.argb(MapPalette.Mass, shared(1), v)
    )
    assertNotEquals(
      MapColours.argb(MapPalette.Mass, own(0), v),
      MapColours.argb(MapPalette.Mass, own(1), v)
    )
    // A difference map's limits are symmetric about zero.
    val d = grid(id("ret_03", 2), difference)
    assertEquals(
      ColourLimits.spanning(MapPalette.Difference, Vector(d)),
      ColourLimits.Symmetric(0.6)
    )
    // An empty or all-zero map gets the unit range rather than a degenerate one.
    val flat = grid(id("ret_04", 2), Vector.fill(Columns * Rows)(Some(0.0)))
    assertEquals(
      ColourLimits.spanning(MapPalette.Mass, Vector(flat)),
      ColourLimits.Sequential(0.0, 1.0)
    )
  }

  test("the global opacity defaults to 0.6 and lies in [0, 1]") {
    assertEquals(MapOpacity.Default.value, 0.6)
    assertEquals(MapOpacity.of(0.0).map(_.value), Some(0.0))
    assertEquals(MapOpacity.of(1.0).map(_.value), Some(1.0))
    assertEquals(MapOpacity.of(1.01), None)
    assertEquals(MapOpacity.of(-0.1), None)
    assertEquals(MapOpacity.of(Double.NaN), None)
  }

  test("a palette or opacity change alters no stored value: the grid's hash is unchanged") {
    val g       = grid(id("ret_07", 2), bump, Vector(0.31, 0.62))
    val before  = g.contentHash
    val rasters = for
      p <- MapPalette.values.toVector
      s <- LimitsScope.values.toVector
      o <- Vector(MapOpacity.Default, MapOpacity.of(0.2).get, MapOpacity.of(1.0).get)
    yield
      val limits = ColourLimits.forPanels(p, s, Vector(g)).head
      val raster = MapRaster.render(g, p, limits)
      assertEquals(g.contentHash, before, s"$p $s ${o.value}")
      raster
    assertEquals(g.cells, bump)
    assertEquals(g.levels, Vector(0.31, 0.62))
    // The palettes do draw it differently.
    assertNotEquals(rasters.head.argb.toVector, rasters.last.argb.toVector)
    // The hash tells stored values apart.
    assertNotEquals(grid(id("ret_07", 2), bump.updated(100, Some(0.5))).contentHash, before)
    assertNotEquals(grid(id("ret_07", 2), bump.updated(0, None)).contentHash, before)
    assertNotEquals(grid(id("ret_07", 2), bump, Vector(0.31)).contentHash, before)
  }

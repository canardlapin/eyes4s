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

package eyes4s.studio.viz.trial

import eyes4s.studio.app.maps.*
import eyes4s.studio.app.text.{TrialText, TrialTextId}
import eyes4s.studio.app.tokens.{Colour, PaletteToken, StageVariant, Theme, Tokens}
import eyes4s.studio.core.assets.Display
import eyes4s.studio.core.backend.{RunId, TrialKey}
import eyes4s.studio.core.selection.ScaleIndex
import eyes4s.studio.viz.plot.IntaglioColours
import intaglio.svg.SvgRenderer
import intaglio.{Grob, alpha, blue, green, red, value}

/** The trial view's result map (ticket S4.3b): the raster of the run's
  * grid drawn cell for cell over the image frame at the global opacity, the
  * grid's backend levels contoured over it and cased, and the remembered
  * image as an underlay, disclosed whenever it is shown.
  */
class TrialMapSceneSuite extends munit.FunSuite:
  import TrialSamples.*

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val Columns = 64
  private val Rows    = 48

  private def mapId(trial: TrialKey = ret07) = MapId(RunId(7), trial, right(ScaleIndex.of(2)))

  // Test values, not results: mass rising to the right, missing at one cell.
  private def cells(missing: Int = 100): Vector[Option[Double]] =
    Vector.tabulate(Columns * Rows)(i =>
      if i == missing then None else Some((i % Columns) / 63.0)
    )

  private def grid(map: MapId = mapId(), levels: Vector[Double] = Vector(0.5)): MapGrid =
    right(MapGrid.of(map, Columns, Rows, cells(), levels))

  private val style = ColourLimits.spanning(MapPalette.Mass, Vector(grid()))

  private def withMap(
      m: Option[TrialMap],
      remembered: RememberedImage = RememberedImage.Absent
  ) =
    TrialSceneInput(
      display(ret07, Display.Blank),
      screen,
      ret07Fixations,
      MarkStyle.Role(TrialRole.Query),
      Theme.Light,
      StageVariant.Dark,
      rasters = Map(beach -> StimulusRaster.Loaded(raster)),
      map = m,
      remembered = remembered
    )

  private def grobs(scene: TrialScene): Vector[Grob] =
    def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
    scene.plot.scene.grobs.flatMap(all)

  private def named(scene: TrialScene, n: String): Option[Grob] =
    grobs(scene).find {
      case i: Grob.Image    => i.name.exists(_.value == n)
      case s: Grob.Segments => s.name.exists(_.value == n)
      case t: Grob.Text     => t.name.exists(_.value == n)
      case _                => false
    }

  test("the map layer is the run's grid, cell for cell, at the global opacity") {
    val g      = grid()
    val hash   = g.contentHash
    val raster = MapRaster.render(g, style)
    val scene  = right(TrialScene(withMap(Some(TrialMap(g, raster)))))
    assertEquals(scene.map, Some(g.map))
    val image = named(scene, TrialScene.MapName) match
      case Some(i: Grob.Image) => i.image
      case other               => fail(s"no map image: $other")
    assertEquals((image.width, image.height), (Columns, Rows))
    val drawn = raster.drawn(MapOpacity.Default)
    for
      y <- 0 until Rows
      x <- 0 until Columns
    do
      val p = image.pixelUnsafe(x, y)
      val d = drawn(y * Columns + x)
      assertEquals(
        (p.red, p.green, p.blue, p.alpha),
        ((d >> 16) & 0xff, (d >> 8) & 0xff, d & 0xff, (d >>> 24) & 0xff),
        s"cell ($x, $y)"
      )
    // Every pixel is the grid's value's colour: the missing cell covers nothing.
    assertEquals(image.pixelUnsafe(100 % Columns, 100 / Columns).alpha, 0)
    assertEquals(image.pixelUnsafe(5, 5).alpha, math.round(255 * 0.6).toInt)
    // Drawing changed no stored value.
    assertEquals(g.contentHash, hash)
    assertEquals(raster.argb.toVector, MapRaster.render(g, style).argb.toVector)
  }

  test("a raster of another map, or another trial's map, is refused") {
    val g     = grid()
    val other = grid(MapId(RunId(8), ret07, right(ScaleIndex.of(2))))
    assertEquals(
      TrialScene(withMap(Some(TrialMap(g, MapRaster.render(other, style))))).left.toOption,
      Some(TrialSceneError.MapMismatch(ret07, other.map, g.map))
    )
    val enc = grid(mapId(enc03))
    assertEquals(
      TrialScene(withMap(Some(TrialMap(enc, MapRaster.render(enc, style))))).left.toOption,
      Some(TrialSceneError.MapOfAnotherTrial(ret07, enc.map))
    )
  }

  test("isolines are contoured at the backend's levels over the map, ink cased in white") {
    val g     = grid(levels = Vector(0.5))
    val scene = right(TrialScene(withMap(Some(TrialMap(g, MapRaster.render(g, style))))))
    val lines = named(scene, TrialScene.IsolinesName) match
      case Some(s: Grob.Segments) => s
      case other                  => fail(s"no isolines: $other")
    assertEquals(lines.segments.size, Isolines.of(g).flatMap(_.segments).size)
    assertEquals(
      lines.gp.stroke,
      Some(IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.IsolineInk)))
    )
    assertEquals(
      lines.gp.casing.map(_.color),
      Some(IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.IsolineCase)))
    )
    // Drawn over the map raster, under the marks.
    val order = grobs(scene)
    val at    = (n: String) => order.indexWhere(g => named(scene, n).exists(_ eq g))
    assert(at(TrialScene.MapName) < at(TrialScene.IsolinesName))
    // No levels, no isolines: studio derives none.
    val bare = grid(levels = Vector.empty)
    val none = right(TrialScene(withMap(Some(TrialMap(bare, MapRaster.render(bare, style))))))
    assertEquals(named(none, TrialScene.IsolinesName), None)
  }

  test("the underlay is disclosed whenever it is shown, and said to be hidden otherwise") {
    val shown = right(TrialScene(withMap(None, RememberedImage.Shown(beach))))
    val said  = TrialText(TrialTextId.RememberedShown)
    assertEquals(said, "Reference image — not displayed during this trial")
    assertEquals(shown.disclosure, Some(said))
    assert(named(shown, TrialScene.UnderlayName).isDefined, "no underlay image")
    named(shown, TrialScene.RememberName) match
      case Some(t: Grob.Text) => assertEquals(t.label, said)
      case other              => fail(s"no disclosure: $other")
    // The export carries it: the scene is what figures and SVG are made from.
    val svg = right(SvgRenderer.render(shown.plot.scene)).value
    assert(svg.contains(said), "the SVG export lacks the disclosure")
    // Still loading, the disclosure is there before the image.
    val loading = right(
      TrialScene(withMap(None, RememberedImage.Shown(beach)).copy(rasters = Map.empty))
    )
    assertEquals(loading.disclosure, Some(said))
    assert(named(loading, TrialScene.UnderlayName).isEmpty)
    assert(named(loading, TrialScene.RememberName).isDefined)
    // Hidden: no image, and the caption says it is not shown.
    val hidden = right(TrialScene(withMap(None, RememberedImage.Hidden(beach))))
    assertEquals(hidden.disclosure, Some(TrialText(TrialTextId.RememberedHidden)))
    assert(named(hidden, TrialScene.UnderlayName).isEmpty)
    // Absent: nothing.
    val absent = right(TrialScene(withMap(None)))
    assertEquals((absent.disclosure, named(absent, TrialScene.RememberName)), (None, None))
    // The underlay lies under the map.
    val g    = grid()
    val both = right(
      TrialScene(
        withMap(Some(TrialMap(g, MapRaster.render(g, style))), RememberedImage.Shown(beach))
      )
    )
    val order         = grobs(both)
    def at(n: String) = order.indexWhere(x => named(both, n).exists(_ eq x))
    assert(at(TrialScene.UnderlayName) < at(TrialScene.MapName))
  }

  /** WCAG relative luminance and contrast of sRGB colours. */
  private def luminance(r: Double, g: Double, b: Double): Double =
    def lin(c: Double) = if c <= 0.03928 then c / 12.92 else math.pow((c + 0.055) / 1.055, 2.4)
    0.2126 * lin(r / 255) + 0.7152 * lin(g / 255) + 0.0722 * lin(b / 255)
  private def contrast(a: (Double, Double, Double), b: (Double, Double, Double)): Double =
    val (la, lb) = (luminance(a._1, a._2, a._3), luminance(b._1, b._2, b._3))
    (math.max(la, lb) + 0.05) / (math.min(la, lb) + 0.05)
  private def rgb(c: Colour): (Double, Double, Double) =
    (c.red.toDouble, c.green.toDouble, c.blue.toDouble)
  private def over(
      top: (Double, Double, Double),
      alphaTop: Double,
      under: (Double, Double, Double)
  ) =
    (
      top._1 * alphaTop + under._1 * (1 - alphaTop),
      top._2 * alphaTop + under._2 * (1 - alphaTop),
      top._3 * alphaTop + under._3 * (1 - alphaTop)
    )

  test("isolines are legible over sky, sand and the blank screen, map included") {
    // Sky and sand stand in for photographs; the blank screen is the token.
    val screenTok   = Tokens.palette(PaletteToken.Screen)
    val backgrounds = Vector(
      "sky"          -> (135.0, 190.0, 235.0),
      "sand"         -> (225.0, 198.0, 153.0),
      "blank screen" -> rgb(screenTok)
    )
    val ink    = Tokens.palette(PaletteToken.IsolineInk)
    val casing = Tokens.palette(PaletteToken.IsolineCase)
    // The map at an isoline is drawn at its level's colour, at the global opacity.
    val levelColour = MapColours.argb(style, Some(0.5))
    val mapColour   =
      (
        ((levelColour >> 16) & 0xff).toDouble,
        ((levelColour >> 8) & 0xff).toDouble,
        (levelColour & 0xff).toDouble
      )
    backgrounds.foreach { (where, bg) =>
      Vector(false, true).foreach { mapped =>
        val ground = if mapped then over(mapColour, MapOpacity.Default.value, bg) else bg
        val cased  = over(rgb(casing), casing.alphaPercent / 100.0, ground)
        val line   = over(rgb(ink), ink.alphaPercent / 100.0, cased)
        val c      = contrast(line, cased)
        assert(c >= 4.5, f"$where (map $mapped): ink on its casing $c%.2f")
        // Without a map the cased line also stands off the bare ground at
        // 3:1 (the ink on a light ground, the casing on a dark one).
        if !mapped then
          val off = math.max(contrast(line, ground), contrast(cased, ground))
          assert(off >= 3.0, f"$where: cased line on ground $off%.2f")
      }
    }
  }

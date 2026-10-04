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

  // Test values, not results: an off-centre bump (near column 20, row 12
  // from the top), missing at one cell.
  private def cells(missing: Int = 100): Vector[Option[Double]] =
    Vector.tabulate(Columns * Rows) { i =>
      val (x, y) = (i % Columns, i / Columns)
      if i == missing then None
      else
        Some(math.exp(-(math.pow((x - 20.0) / 8.0, 2) + math.pow((y - 12.0) / 6.0, 2)) / 2.0))
    }

  private def grid(
      map: MapId = mapId(),
      levels: Vector[Double] = Vector(0.5),
      order: RowOrder = RowOrder.TopFirst
  ): MapGrid =
    val stored = order match
      case RowOrder.TopFirst    => cells()
      case RowOrder.BottomFirst => cells().grouped(Columns).toVector.reverse.flatten
    right(MapGrid.of(map, Columns, Rows, order, stored, levels))

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
    for
      y <- 0 until Rows
      x <- 0 until Columns
    do
      val p = image.pixelUnsafe(x, y)
      // The cell's own value, counted from the top, coloured and drawn at 0.6.
      val d = MapOpacity.Default.over(MapColours.argb(style, g.atTop(x, y)))
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

  /** A native coordinate's value. */
  private def nativeValue(e: intaglio.LengthExpr): Double = e match
    case intaglio.LengthExpr.Const(l) => l.value
    case other                        => fail(s"not a native constant: $other")

  test("a map covering a region of the screen is drawn there, not over the image") {
    // A preview over a window that is not the image: here the whole screen.
    val g                  = grid()
    val raster             = MapRaster.render(g, style)
    def image(m: TrialMap) =
      named(right(TrialScene(withMap(Some(m)))), TrialScene.MapName) match
        case Some(i: Grob.Image) => i
        case other               => fail(s"no map image: $other")
    val onImage = image(TrialMap(g, raster))
    val frame   = display(ret07, Display.Blank).placement
    assertEquals(
      onImage.at,
      right(
        intaglio.Point.native(frame.left + frame.width / 2.0, frame.top + frame.height / 2.0)
      )
    )
    val whole  = right(ScreenRect.of(0.0, 0.0, 1920.0, 1080.0))
    val onRect = image(TrialMap(g, raster, covers = MapCoverage.Region(whole)))
    assertEquals(onRect.at, right(intaglio.Point.native(960.0, 540.0)))
    assertEquals(
      onRect.size,
      intaglio.Size.fromExtents(
        right(intaglio.ExtentExpr.native(1920.0)),
        right(intaglio.ExtentExpr.native(1080.0))
      )
    )
    assertNotEquals(onRect.size, onImage.size)
    // The isolines follow the map: every segment lies in the region it covers.
    val inner = right(ScreenRect.of(100.0, 50.0, 612.0, 434.0))
    val lines = named(
      right(TrialScene(withMap(Some(TrialMap(g, raster, covers = MapCoverage.Region(inner)))))),
      TrialScene.IsolinesName
    ) match
      case Some(s: Grob.Segments) => s
      case other                  => fail(s"no isolines: $other")
    val xs = lines.segments.flatMap((a, b) => Vector(a.x, b.x)).map(nativeValue)
    val ys = lines.segments.flatMap((a, b) => Vector(a.y, b.y)).map(nativeValue)
    assert(xs.nonEmpty)
    assert(xs.forall(x => x >= 100.0 && x <= 612.0), xs.take(5))
    assert(ys.forall(y => y >= 50.0 && y <= 434.0), ys.take(5))
  }

  test("a bottom-first grid draws the same picture: map and isolines") {
    def scene(order: RowOrder) =
      val g = grid(order = order)
      right(TrialScene(withMap(Some(TrialMap(g, MapRaster.render(g, style))))))
    def image(s: TrialScene) = named(s, TrialScene.MapName) match
      case Some(i: Grob.Image) => i.image
      case other               => fail(s"$other")
    def lines(s: TrialScene) = named(s, TrialScene.IsolinesName) match
      case Some(l: Grob.Segments) => l.segments
      case other                  => fail(s"$other")
    val (top, bottom) = (scene(RowOrder.TopFirst), scene(RowOrder.BottomFirst))
    assertEquals(image(bottom), image(top))
    assertEquals(lines(bottom), lines(top))
    // The bump is drawn in the top half of the frame.
    assert(image(top).pixelUnsafe(20, 12).alpha > 0)
    assertNotEquals(image(top).pixelUnsafe(20, 12), image(top).pixelUnsafe(20, Rows - 1 - 12))
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

  test("isolines are legible over sky, sand and the blank screen, over every map colour") {
    // Sky and sand stand in for photographs; the blank screen is the token.
    val backgrounds = Vector(
      "sky"          -> (135.0, 190.0, 235.0),
      "sand"         -> (225.0, 198.0, 153.0),
      "blank screen" -> rgb(Tokens.palette(PaletteToken.Screen))
    )
    val ink    = Tokens.palette(PaletteToken.IsolineInk)
    val casing = Tokens.palette(PaletteToken.IsolineCase)
    // Every mass colour from the ramp's lightest to its darkest, at 0.6, and no map.
    val unit = right(ColourLimits.sequential(0.0, 1.0).flatMap(MapStyle.of(MapPalette.Mass, _)))
    val grounds = for
      (where, bg) <- backgrounds
      t           <- (0 to 100).map(k => Some(k / 100.0)) :+ None
    yield
      val ground = t.fold(bg) { v =>
        val c = MapColours.argb(unit, Some(v))
        over(
          (((c >> 16) & 0xff).toDouble, ((c >> 8) & 0xff).toDouble, (c & 0xff).toDouble),
          MapOpacity.Default.value,
          bg
        )
      }
      (s"$where, ${t.fold("no map")(v => f"map at $v%.2f")}", ground)
    val worst = grounds
      .map { (where, ground) =>
        val cased = over(rgb(casing), casing.alphaPercent / 100.0, ground)
        val line  = over(rgb(ink), ink.alphaPercent / 100.0, cased)
        assert(contrast(line, cased) >= 4.5, s"$where: ink on its casing")
        // The cased line stands off the ground around it: the ink does on a
        // light ground, the white casing on a dark one.
        (math.max(contrast(line, ground), contrast(cased, ground)), where)
      }
      .minBy(_._1)
    assert(worst._1 >= 3.0, f"worst: ${worst._2} at ${worst._1}%.2f")
  }

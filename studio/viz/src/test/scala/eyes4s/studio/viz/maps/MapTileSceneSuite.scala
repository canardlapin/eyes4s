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
package eyes4s.studio.viz.maps

import eyes4s.studio.app.maps.*
import eyes4s.studio.app.tokens.{PaletteToken, StageToken, StageVariant, Tokens}
import eyes4s.studio.core.backend.{Phase, RunId, ScreenRegion, TrialKey}
import eyes4s.studio.core.selection.ScaleIndex
import eyes4s.studio.viz.plot.IntaglioColours
import intaglio.svg.SvgRenderer
import intaglio.{Grob, alpha, blue, green, red, value}

/** Panel C's map tile (bd-01M42K7ZYDRR90JXZYNDD2HVXP): the served grid's
  * raster, opaque and cell for cell over the region it covers, on the
  * stage, with the served levels contoured and cased.
  */
class MapTileSceneSuite extends munit.FunSuite:

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val Columns = 16
  private val Rows    = 12
  private val ret07   = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  private val region  = right(ScreenRegion.of(ret07, 448.0, 156.0, 1472.0, 924.0))

  // Test values, not results: a bump near column 5, row 3 from the top.
  private val cells: Vector[Option[Double]] = Vector.tabulate(Columns * Rows) { i =>
    val (x, y) = (i % Columns, i / Columns)
    Some(math.exp(-(math.pow((x - 5.0) / 2.0, 2) + math.pow((y - 3.0) / 2.0, 2)) / 2.0))
  }

  private def grid(levels: Vector[Double] = Vector(0.5, 0.2)): MapGrid =
    right(
      MapGrid.of(
        MapId(RunId(7), ret07, right(ScaleIndex.of(2))),
        Columns,
        Rows,
        RowOrder.TopFirst,
        cells,
        levels
      )
    )

  // A shared upper limit above this map's own peak, as panel C's tiles share one.
  private val style = right(
    MapStyle.of(MapPalette.Mass, right(ColourLimits.sequential(0.0, 2.0)))
  )

  private def scene(g: MapGrid = grid()) =
    right(MapTileScene.of("figures.map.test", g, style, region, StageVariant.Dark))

  private def grobs(g: MapGrid = grid()): Vector[Grob] =
    def all(x: Grob): Vector[Grob] = x +: x.children.flatMap(all)
    scene(g).scene.grobs.flatMap(all)

  private def named(n: String, g: MapGrid = grid()): Option[Grob] = grobs(g).find {
    case i: Grob.Image    => i.name.exists(_.value == n)
    case s: Grob.Segments => s.name.exists(_.value == n)
    case r: Grob.Rect     => r.name.exists(_.value == n)
    case _                => false
  }

  private def nativeValue(e: intaglio.LengthExpr): Double = e match
    case intaglio.LengthExpr.Const(l) => l.value
    case other                        => fail(s"not a native constant: $other")

  test("the tile is the grid's raster in the shared style, opaque, cell for cell") {
    val image = named(MapTileScene.MapName) match
      case Some(i: Grob.Image) => i.image
      case other               => fail(s"no map image: $other")
    assertEquals((image.width, image.height), (Columns, Rows))
    val g = grid()
    for
      y <- 0 until Rows
      x <- 0 until Columns
    do
      val p = image.pixelUnsafe(x, y)
      val d = MapColours.argb(style, g.atTop(x, y))
      assertEquals(
        (p.red, p.green, p.blue, p.alpha),
        ((d >> 16) & 0xff, (d >> 8) & 0xff, d & 0xff, 255),
        s"cell ($x, $y)"
      )
    // Under a limit above its peak, the fullest cell is not the ramp's darkest.
    val peak = MapColours.argb(style, Some(1.0))
    assertNotEquals(peak, MapColours.argb(style, Some(2.0)))
  }

  test("the stage is drawn under the map, over the region the grid covers") {
    named(MapTileScene.StageName) match
      case Some(r: Grob.Rect) =>
        assertEquals(
          r.gp.fill,
          Some(IntaglioColours.staged(StageVariant.Dark, StageToken.Stage))
        )
      case other => fail(s"no stage: $other")
    val order = grobs()
    val at    = (n: String) =>
      order.indexWhere {
        case i: Grob.Image    => i.name.exists(_.value == n)
        case l: Grob.Segments => l.name.exists(_.value == n)
        case r: Grob.Rect     => r.name.exists(_.value == n)
        case _                => false
      }
    assert(at(MapTileScene.StageName) >= 0)
    assert(at(MapTileScene.StageName) < at(MapTileScene.MapName))
    assert(at(MapTileScene.MapName) < at(MapTileScene.IsolinesName))
    val panel = scene().panel
    assertEquals((panel.xDomain.lower, panel.xDomain.upper), (448.0, 1472.0))
    assertEquals((panel.yDomain.lower, panel.yDomain.upper), (156.0, 924.0))
  }

  test("isolines are the served levels' contours, cased, inside the region") {
    val g     = grid()
    val lines = named(MapTileScene.IsolinesName, g) match
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
    val xs = lines.segments.flatMap((a, b) => Vector(a.x, b.x)).map(nativeValue)
    val ys = lines.segments.flatMap((a, b) => Vector(a.y, b.y)).map(nativeValue)
    assert(xs.forall(x => x >= 448.0 && x <= 1472.0), xs.take(5))
    assert(ys.forall(y => y >= 156.0 && y <= 924.0), ys.take(5))
    // The bump is in the top half: so is its half-level contour (row 0 is the top).
    val half = named(MapTileScene.IsolinesName, grid(Vector(0.5))) match
      case Some(l: Grob.Segments) =>
        l.segments.flatMap((a, b) => Vector(a.y, b.y)).map(nativeValue)
      case other => fail(s"no isolines: $other")
    assert(half.nonEmpty && half.forall(_ < 156.0 + 768.0 / 2), half.take(5))
    // No levels, no isolines: studio derives none.
    assertEquals(named(MapTileScene.IsolinesName, grid(Vector.empty)), None)
    // The tile exports as the figure's other panels do.
    assert(right(SvgRenderer.render(scene(g).scene)).value.nonEmpty)
  }

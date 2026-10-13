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

package eyes4s.studio.viz.figure

import eyes4s.studio.app.figures.*
import eyes4s.studio.app.maps.{
  ColourLimits,
  Isolines,
  MapColours,
  MapGrid,
  MapId,
  MapPalette,
  MapStyle,
  RowOrder
}
import eyes4s.studio.app.tokens.{PaletteToken, Tokens}
import eyes4s.studio.core.backend.{PairDesign, Phase, RunId, ScreenRegion, TrialKey}
import eyes4s.studio.core.document.{FigureId, PanelLetter}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import eyes4s.studio.viz.maps.MapTileScene
import eyes4s.studio.viz.plot.{IntaglioColours, ReferenceRole}
import intaglio.{ExtentExpr, Grob, LengthExpr, LengthUnit, alpha, blue, green, red, value}

/** Figure export keeps panel C's served map cells, limits and isolines rather
  * than reducing a drawn tile to its score label.
  */
class FigurePageMapSuite extends munit.FunSuite:

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val trial  = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  private val scale  = right(ScaleIndex.of(2))
  private val region = right(ScreenRegion.of(trial, 100.0, 200.0, 500.0, 400.0))
  private val grid   = right(
    MapGrid.of(
      MapId(RunId(7), trial, scale),
      columns = 2,
      rows = 2,
      RowOrder.TopFirst,
      Vector(Some(0.0), Some(1.0), Some(0.25), Some(0.75)),
      Vector(0.5)
    )
  )
  // The full shared limit is deliberately above the served map's peak.
  private val style = right(
    ColourLimits.sequential(0.0, 2.0).flatMap(MapStyle.of(MapPalette.Mass, _))
  )

  private val page =
    PageVM(
      right(FigureId.of(1)),
      "Figure 1",
      PageWidth.SingleColumn,
      "Page 89 mm · one-column",
      "100%",
      3.5,
      Vector(
        PanelVM(
          right(PanelLetter.of("C")),
          "Density maps",
          selected = true,
          widthMm = 89,
          PanelBody.Maps(
            DensityMapsVM(
              Vector(
                MapTileVM(
                  "Query ret_07",
                  "",
                  "P17 · 2°",
                  StudioRef.Trial(trial),
                  trial,
                  TileMap.Drawn(grid, style, region)
                )
              ),
              "caption",
              "shared limits"
            )
          ),
          StudioRef.Trial(trial)
        )
      ),
      ComposerTab.Figure,
      None,
      "Open in Compare",
      "caption",
      "stamp",
      7,
      AppearanceVM(Vector.empty, Vector.empty, None, ("images", true)),
      greyscale = false,
      ExportVM(Vector.empty, "Export", None),
      AddPanelVM("Add panel", Vector.empty),
      eyes4s.studio.app.figures
        .PanelEditingVM(Some(right(PanelLetter.of("C"))), false, false, false)
    )

  private def grobs(g: Grob): Vector[Grob] = g +: g.children.flatMap(grobs)

  /** Sums of millimetres differ from one another only by float rounding. */
  private val RoundingMm = 1e-9

  /** One line of the page's 7 pt text, in millimetres. */
  private val lineMm = 7 * FigureType.MillimetresPerPoint * FigurePage.LineSpacing

  private val query   = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  private val matched = TrialKey("P17", Phase.Encoding, "enc_03", 1)
  private val control = TrialKey("P17", Phase.Encoding, "enc_01", 1)
  private val run     = RunId(7)
  private def pair(design: PairDesign, ref: TrialKey) =
    StudioRef.Pair(run, scale, design, query, ref)

  /** Panel C's three role tiles; the control's map is half as tall as the
    * others and its title and subtitle are longer than the tile.
    */
  private def roleTiles(subtitle: String) =
    val wide = right(ScreenRegion.of(trial, 0.0, 0.0, 800.0, 200.0))
    Vector(
      MapTileVM(
        "Query ret_07",
        "",
        "P17 · σ 2°",
        StudioRef.QueryContrast(run, scale, query),
        query,
        TileMap.Drawn(grid, style, region)
      ),
      MapTileVM(
        "Matched enc_03",
        "",
        "0.73",
        pair(PairDesign.Matched, matched),
        matched,
        TileMap.Drawn(grid, style, region)
      ),
      MapTileVM(
        "Highest of 12 scored of 19",
        subtitle,
        "0.61",
        pair(PairDesign.Control, control),
        control,
        TileMap.Drawn(grid, style, wide)
      )
    )

  private def tilesPage(subtitle: String) =
    page.copy(panels =
      page.panels.map(p =>
        p.copy(body = PanelBody.Maps(DensityMapsVM(roleTiles(subtitle), "caption", "maps")))
      )
    )

  /** The viewports the page places its maps in, in millimetres (the scenes'
    * own viewports inside them are not).
    */
  private def mapPorts(all: Vector[Grob]) =
    def drawsMap(group: Grob.Group) = group.children.flatMap(grobs).exists {
      case image: Grob.Image => image.name.exists(_.value == MapTileScene.MapName)
      case _                 => false
    }
    def inMm(port: intaglio.Viewport) = port.size.height match
      case ExtentExpr(LengthExpr.Const(l)) => l.unit == LengthUnit.Mm
      case _                               => false
    all
      .collect { case group: Grob.Group if drawsMap(group) => group.viewport }
      .flatten
      .filter(inMm)

  private def native(e: LengthExpr): Double = e match
    case LengthExpr.Const(length) => length.value
    case other                    => fail(s"not a native length: $other")

  private def extent(e: ExtentExpr): Double = e match
    case ExtentExpr(LengthExpr.Const(length)) => length.value
    case other                                => fail(s"not a native extent: $other")

  test("a drawn density tile exports its served cells, shared style and served contour") {
    val all = right(FigurePage.build(page)).scene.grobs.flatMap(grobs)
    val map = all
      .collectFirst {
        case image: Grob.Image if image.name.exists(_.value == MapTileScene.MapName) =>
          image.image
      }
      .getOrElse(fail("no exported map raster"))
    assertEquals((map.width, map.height), (grid.columns, grid.rows))
    def argb(x: Int, y: Int) =
      val pixel = map.pixelUnsafe(x, y)
      (pixel.red << 16) | (pixel.green << 8) | pixel.blue | (pixel.alpha << 24)
    assertEquals(argb(0, 0), MapColours.argb(style, Some(0.0)))
    assertEquals(argb(1, 0), MapColours.argb(style, Some(1.0)))
    assertEquals(argb(0, 1), MapColours.argb(style, Some(0.25)))
    assertEquals(argb(1, 1), MapColours.argb(style, Some(0.75)))
    assertNotEquals(argb(1, 0), MapColours.argb(style, Some(2.0)))

    val contours = all
      .collectFirst {
        case lines: Grob.Segments if lines.name.exists(_.value == MapTileScene.IsolinesName) =>
          lines
      }
      .getOrElse(fail("no exported served contour"))
    assertEquals(contours.segments.size, Isolines.of(grid).flatMap(_.segments).size)

    val tile = all
      .collectFirst {
        case group: Grob.Group
            if group.viewport.nonEmpty && group.children.exists(_.isInstanceOf[Grob.Group]) &&
              group.children.flatMap(grobs).exists {
                case image: Grob.Image => image.name.exists(_.value == MapTileScene.MapName)
                case _                 => false
              } =>
          group
      }
      .getOrElse(fail("no map export viewport"))
    val viewport = tile.viewport.getOrElse(fail("map group has no viewport"))
    assertEquals(
      extent(viewport.size.width) / extent(viewport.size.height),
      region.width / region.height
    )

    val score = all
      .collectFirst { case text: Grob.Text if text.label == "P17 · 2°" => text }
      .getOrElse(fail("no map score label"))
    val box = all
      .collectFirst { case rect: Grob.Rect if rect.gp.stroke.nonEmpty => rect }
      .getOrElse(fail("no map tile box"))
    // The label is the second caption row, one padding below the map's bottom.
    val scoreY = native(score.at.y)
    assertEqualsDouble(
      scoreY,
      native(viewport.origin.y) - FigurePage.TilePaddingMm - lineMm,
      RoundingMm
    )
    assert(
      scoreY <= native(box.center.y) && scoreY >= native(box.center.y) - extent(box.size.height)
    )
  }

  test(
    "the exported figure and every panel retain accessible text, including unavailable panels"
  ) {
    val waiting = page.panels.head.copy(
      letter = right(PanelLetter.of("D")),
      title = "Pending & unavailable",
      body = PanelBody.Unavailable("A source <file> is missing")
    )
    val vm = page.copy(
      title = "Density & context",
      panels = page.panels :+ waiting,
      caption = "Caption & provenance"
    )
    val built = right(FigurePage.build(vm))
    assertEquals(built.scene.semantics.documentId.map(_.value), Some("studio-figure-1"))
    assertEquals(built.scene.semantics.accessibleTitle, Some(vm.title))
    val description =
      built.scene.semantics.accessibleDescription.getOrElse(fail("missing description"))
    Vector(
      vm.caption,
      vm.stamp,
      "Panel C: Density maps",
      "Query ret_07",
      "shared limits",
      "Panel D: Pending & unavailable",
      "A source <file> is missing"
    ).foreach(text => assert(description.contains(text), text))
    val svg = right(intaglio.svg.SvgRenderer.render(right(built.plan(96)))).value
    assert(svg.contains("<title id=\"studio-figure-1-title\">Density &amp; context</title>"))
    assert(svg.contains("aria-describedby=\"studio-figure-1-description\""))
    assert(svg.contains("<title>Panel C: Density maps</title>"))
    assert(svg.contains("<title>Panel D: Pending &amp; unavailable</title>"))
    assert(svg.contains("A source &lt;file&gt; is missing"))
    val ids = "data-semantic-id=\"([^\"]+)\"".r.findAllMatchIn(svg).map(_.group(1)).toVector
    assertEquals(ids, Vector("studio-figure-1-panel-C", "studio-figure-1-panel-D"))
  }

  test("every map in a row shares one top, whatever its title, caption or aspect") {
    val built = right(FigurePage.build(tilesPage("controls · street-112")))
    val all   = built.scene.grobs.flatMap(grobs)
    val ports = mapPorts(all)
    assertEquals(ports.size, 3)
    // Intaglio measures y up: a map's top is its origin plus its height.
    val tops = ports.map(v => native(v.origin.y) + extent(v.size.height))
    tops.foreach(top => assertEqualsDouble(top, tops.head, RoundingMm))
    // The shorter map keeps its own aspect inside the row.
    assert(extent(ports(2).size.height) < extent(ports(0).size.height), ports)
    // Every label shares one baseline, and every title one row.
    def ys(labels: Set[String]) = all.collect {
      case t: Grob.Text if labels.contains(t.label) => native(t.at.y)
    }
    val scores = ys(Set("P17 · σ 2°", "0.73", "0.61"))
    assertEquals(scores.size, 3)
    scores.foreach(y => assertEqualsDouble(y, scores.head, RoundingMm))
    val titles = all.collect {
      case t: Grob.Text
          if t.label.startsWith("Query") || t.label.startsWith("Matched") ||
            t.label.startsWith("Highest") =>
        native(t.at.y)
    }
    assertEquals(titles.size, 3)
    titles.foreach(y => assertEqualsDouble(y, titles.head, RoundingMm))
    // The title row sits one line above the maps' shared top.
    assertEqualsDouble(titles.head, tops.head + lineMm, RoundingMm)
  }

  test("a tile's row longer than the tile is cut to one row ending in an ellipsis") {
    // The rule: as many code points as the row's glyphs, or one fewer and "…".
    assertEquals(FigurePage.fit("0.61", 10.0, 7), "0.61")
    val perRow =
      (10.0 / (7 * FigureType.MillimetresPerPoint * FigurePage.AverageAdvanceEm)).toInt
    val exact = "σ" * perRow
    assertEquals(FigurePage.fit(exact, 10.0, 7), exact)
    val cut = FigurePage.fit(exact + "x", 10.0, 7)
    assertEquals(cut, "σ" * (perRow - 1) + FigurePage.Elision)
    assertEquals(cut.codePointCount(0, cut.length), perRow)

    val long  = "controls · street-112 · a very long item name indeed"
    val built = right(FigurePage.build(tilesPage(long)))
    val texts = built.scene.grobs.flatMap(grobs).collect { case t: Grob.Text => t.label }
    // Drawn once, cut, never wrapped onto a further row.
    val drawn = texts.filter(t => t.startsWith("controls"))
    assertEquals(drawn.size, 1, texts)
    assert(drawn.head.endsWith(FigurePage.Elision) && long.startsWith(drawn.head.dropRight(1)))
    assert(!texts.exists(t => t.nonEmpty && long.drop(drawn.head.length - 1).startsWith(t)))
    // The control's title is cut on its row too, after its glyph.
    val title = texts.filter(_.startsWith("Highest of"))
    assertEquals(title.size, 1, texts)
    assert(title.head.endsWith(FigurePage.Elision), title)
    // The page's description keeps every word.
    val description = built.scene.semantics.accessibleDescription.getOrElse(fail("none"))
    assert(description.contains(s"Highest of 12 scored of 19 $long"), description)
  }

  test("each role tile draws its role glyph as a shape: ● query, ◆ matched, ○ control") {
    val all = right(FigurePage.build(tilesPage(""))).scene.grobs.flatMap(grobs)
    def named(role: ReferenceRole) =
      all.filter(_.name.exists(_.value == FigurePage.glyphName(role)))
    def ink(token: PaletteToken) = Some(IntaglioColours.toIntaglio(Tokens.palette(token)))
    named(ReferenceRole.Query) match
      case Vector(c: Grob.Circle) =>
        assertEquals((c.gp.fill, c.gp.stroke), (ink(PaletteToken.FigQuery), None))
      case other => fail(s"query glyph: $other")
    named(ReferenceRole.Match) match
      case Vector(d: Grob.Polygon) =>
        assertEquals(
          (d.gp.fill, d.gp.stroke, d.points.size),
          (ink(PaletteToken.FigMatch), None, 4)
        )
      case other => fail(s"matched glyph: $other")
    named(ReferenceRole.Control) match
      case Vector(c: Grob.Circle) =>
        assertEquals((c.gp.fill, c.gp.stroke), (None, ink(PaletteToken.FigControl)))
      case other => fail(s"control glyph: $other")
    // Plex has none of the three characters, so no text draws them.
    val texts = all.collect { case t: Grob.Text => t.label }
    assert(!texts.exists(t => t.exists(c => "●◆○".contains(c))), texts)
    // The query's glyph sits on its title's row, left of the title.
    val title = all
      .collectFirst { case t: Grob.Text if t.label == "Query ret_07" => t }
      .getOrElse(fail("no query title"))
    named(ReferenceRole.Query).head match
      case c: Grob.Circle =>
        val r = extent(c.radius)
        assertEqualsDouble(native(c.center.y), native(title.at.y) - lineMm / 2, RoundingMm)
        assert(native(c.center.x) + r < native(title.at.x), (c.center, title.at))
      case other => fail(other.toString)
    // A tile with no role (an unscored trial) draws no glyph.
    val plain = right(FigurePage.build(page)).scene.grobs.flatMap(grobs)
    assert(!plain.exists(_.name.exists(_.value.startsWith("figure-tile-glyph"))))
  }

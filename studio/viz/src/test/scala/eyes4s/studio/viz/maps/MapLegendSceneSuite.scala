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
import eyes4s.studio.app.text.{TrialText, TrialTextId}
import eyes4s.studio.app.tokens.{
  FontFace,
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  ThemedToken,
  Tokens,
  TypeSize
}
import eyes4s.studio.viz.plot.IntaglioColours
import eyes4s.studio.viz.trial.TrialScene
import intaglio.svg.SvgRenderer
import intaglio.{CasingWidth, Grob, HJust, LengthExpr, LineCap, Point, Rgba, StrokeUnit, value}

/** The trial map's legend (review ticket 17; Main.dc.html, MAP_RECIPE.js
  * `mapLegend()`): a strip of eight ramp swatches faded and coloured as the
  * map colours their fractions, framed by a hairline; a sample isoline cased
  * as the map's; and the board's words, from the trial text catalogue, in
  * 11 px on-stage text. Mass maps only.
  */
class MapLegendSceneSuite extends munit.FunSuite:

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val PointsPerPixel = 0.75

  private def legend(
      theme: Theme = Theme.Light,
      stage: StageVariant = StageVariant.Dark,
      limits: LimitsScope = LimitsScope.PerPanel,
      opacity: MapOpacity = MapOpacity.Default
  ): MapLegendScene =
    right(MapLegendScene(MapPalette.Mass, limits, opacity, theme, stage))
      .getOrElse(fail("a mass map has a legend"))

  private def grobs(l: MapLegendScene): Vector[Grob] =
    def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
    l.plot.scene.grobs.flatMap(all)

  private def named(l: MapLegendScene, n: String): Grob =
    grobs(l)
      .find(_.name.exists(_.value == n))
      .getOrElse(fail(s"no grob named $n"))

  private def native(e: LengthExpr): Double = e match
    case LengthExpr.Const(l) => l.value
    case other               => fail(s"not a native constant: $other")

  private def xy(p: Point): (Double, Double) = (native(p.x), native(p.y))

  private def rgba(c: Rgba): (Int, Int, Int, Int) =
    (c.red, c.green, c.blue, math.round(c.alpha * 255).toInt)

  private def channels(argb: Int): (Int, Int, Int, Int) =
    ((argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff, (argb >>> 24) & 0xff)

  private val themes = Vector(Theme.Light, Theme.Dark)

  test("eight swatches, 10 px each, edge to edge in one 80 × 10 strip on the first row") {
    val l = legend()
    for k <- 0 until MapLegendScene.Swatches do
      named(l, s"${MapLegendScene.SwatchPrefix}$k") match
        case p: Grob.Polygon =>
          val left = MapLegendScene.StripLeftPx + 10.0 * k
          val top  = MapLegendScene.FirstRowPx - 5.0
          assertEquals(
            p.points.map(xy),
            Vector(
              (left, top),
              (left + 10.0, top),
              (left + 10.0, top + 10.0),
              (left, top + 10.0)
            ),
            s"swatch $k"
          )
          assertEquals(p.gp.stroke, None)
        case other => fail(s"swatch $k is not a polygon: $other")
    named(l, MapLegendScene.FrameName) match
      case p: Grob.Polygon =>
        val (l0, r0) = (MapLegendScene.StripLeftPx, MapLegendScene.StripLeftPx + 80.0)
        val top      = MapLegendScene.FirstRowPx - 5.0
        assertEquals(
          p.points.map(xy),
          Vector(
            (l0 + 0.5, top + 0.5),
            (r0 - 0.5, top + 0.5),
            (r0 - 0.5, top + 9.5),
            (l0 + 0.5, top + 9.5)
          )
        )
        assertEquals(p.gp.fill, None)
        assertEquals((p.gp.lineWidth, p.gp.lineWidthUnit), (PointsPerPixel, StrokeUnit.Point))
      case other => fail(s"the strip's frame is not a polygon: $other")
  }

  test("each swatch is its fraction's map colour, fading in under the 10% floor, at 0.6") {
    val l     = legend()
    val style = ColourLimits.spanning(MapPalette.Mass, Vector.empty)
    val fills = (0 until MapLegendScene.Swatches).toVector.map { k =>
      named(l, s"${MapLegendScene.SwatchPrefix}$k") match
        case p: Grob.Polygon => rgba(p.gp.fill.getOrElse(fail(s"swatch $k has no fill")))
        case other           => fail(s"swatch $k: $other")
    }
    // The map's own colour for k / 7 of its limits, at the map's opacity.
    for k <- 0 until MapLegendScene.Swatches do
      val d = MapOpacity.Default.over(MapColours.argb(style, Some(k / 7.0)))
      assertEquals(fills(k), channels(d), s"swatch $k")
    // Zero mass is see-through (alpha 0 under the floor); every later
    // fraction lies above 10% of the limits, so is opaque before 0.6.
    assertEquals(fills.head._4, 0)
    assert(1.0 / 7.0 > MapColours.MassAlphaFloor)
    assert(fills.tail.forall(_._4 == math.round(255 * 0.6).toInt), fills)
    // The ramp runs light to dark: the last swatch is its darkest stop.
    val darkest = Tokens.massRamp.stops.last
    assertEquals(fills.last.take3, (darkest.red, darkest.green, darkest.blue))
    // Another opacity is the map's: the legend draws swatches as the map does.
    val opaque = legend(opacity = MapOpacity.of(1.0).getOrElse(fail("1.0")))
    named(opaque, s"${MapLegendScene.SwatchPrefix}7") match
      case p: Grob.Polygon => assertEquals(p.gp.fill.map(rgba).map(_._4), Some(255))
      case other           => fail(s"$other")
  }

  test(
    "the strip's frame is the theme's hairline, and the stage is the stage's, in both themes"
  ) {
    for
      theme <- themes
      stage <- StageVariant.values.toVector
    do
      val l = legend(theme, stage)
      named(l, MapLegendScene.FrameName) match
        case p: Grob.Polygon =>
          assertEquals(p.gp.stroke, Some(IntaglioColours.themed(theme, ThemedToken.Hairline)))
        case other => fail(s"$other")
      named(l, MapLegendScene.StageName) match
        case r: Grob.Rect =>
          assertEquals(r.gp.fill, Some(IntaglioColours.staged(stage, StageToken.Stage)))
        case other => fail(s"$other")
    assertNotEquals(
      IntaglioColours.themed(Theme.Light, ThemedToken.Hairline),
      IntaglioColours.themed(Theme.Dark, ThemedToken.Hairline)
    )
  }

  test("the isoline sample is cased as the map's isolines: 2 px of ink inside 4 px of casing") {
    for theme <- themes do
      named(legend(theme), MapLegendScene.IsolineName) match
        case line: Grob.Lines =>
          assertEquals(
            line.points.map(xy),
            Vector(
              (MapLegendScene.InsetPx + 1.0, MapLegendScene.SecondRowPx),
              (MapLegendScene.InsetPx + 15.0, MapLegendScene.SecondRowPx)
            )
          )
          assertEquals(
            line.gp.stroke,
            Some(IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.IsolineInk)))
          )
          assertEquals(line.gp.lineWidth, TrialScene.IsolinePx * PointsPerPixel)
          assertEquals(line.gp.lineWidthUnit, StrokeUnit.Point)
          // The board's sample is a plain stroke: butt caps.
          assertEquals(line.gp.lineCap, LineCap.Butt)
          val casing = line.gp.casing.getOrElse(fail("the sample is not cased"))
          assertEquals(
            casing.color,
            IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.IsolineCase))
          )
          casing.width match
            case CasingWidth.Absolute(w) =>
              assertEquals(w.value, TrialScene.IsolineCasingPx * PointsPerPixel)
            case other => fail(s"casing width $other")
        case other => fail(s"the isoline sample is not a line: $other")
  }

  test("the words are the board's, from the trial text catalogue, 11 px on-stage Sans") {
    for
      theme <- themes
      stage <- StageVariant.values.toVector
    do
      val l     = legend(theme, stage)
      val texts = grobs(l).collect { case t: Grob.Text => t }
      assertEquals(
        texts.map(_.label),
        Vector(
          "0",
          "max · fixation duration mass per cell · fades below 10% of max",
          "50% · 90% of mass"
        )
      )
      for t <- texts do
        assertEquals(t.gp.fill, Some(IntaglioColours.staged(stage, StageToken.OnStage)))
        assertEquals(t.gp.fontSize.value, TypeSize.T11.px * PointsPerPixel)
        assertEquals(t.gp.fontFamily, Some(FontFace.SansRegular.javaFxFamily))
    val l = legend()
    // The words come from the catalogue; the floor is MapColours'.
    assertEquals(
      MapLegendScene.rampText,
      TrialText(TrialTextId.MapLegendRamp) + TrialText(TrialTextId.Separator) +
        TrialText(TrialTextId.MapLegendFades, "10%")
    )
    // "0" ends just before the strip; the words start just after it.
    named(l, MapLegendScene.ZeroName) match
      case t: Grob.Text =>
        assertEquals(t.anchor.horizontal, HJust.Right)
        assertEquals(xy(t.at), (MapLegendScene.StripLeftPx - 6.0, MapLegendScene.FirstRowPx))
      case other => fail(s"$other")
    named(l, MapLegendScene.RampName) match
      case t: Grob.Text =>
        assertEquals(t.anchor.horizontal, HJust.Left)
        assertEquals(xy(t.at), (MapLegendScene.StripLeftPx + 86.0, MapLegendScene.FirstRowPx))
      case other => fail(s"$other")
    named(l, MapLegendScene.KeyName) match
      case t: Grob.Text =>
        assertEquals(xy(t.at), (MapLegendScene.InsetPx + 22.0, MapLegendScene.SecondRowPx))
      case other => fail(s"$other")
  }

  test("shared limits are said after the isolines' key") {
    val shared = grobs(legend(limits = LimitsScope.Shared)).collect { case t: Grob.Text =>
      t.label
    }
    assertEquals(shared.last, "50% · 90% of mass · shared limits")
  }

  test("the legend describes itself to assistive technology") {
    val l = legend()
    assertEquals(
      l.description,
      "Map legend: fixation duration mass per cell from 0 to max, fading out below 10 " +
        "percent of max; isolines enclose 50 and 90 percent of mass"
    )
    assertEquals(
      l.plot.scene.semantics.plots.headOption.flatMap(_.description),
      Some(l.description)
    )
    assertEquals(
      legend(limits = LimitsScope.Shared).description,
      "Map legend: fixation duration mass per cell from 0 to max, fading out below 10 " +
        "percent of max, shared colour limits across panels; isolines enclose 50 and 90 " +
        "percent of mass"
    )
  }

  test("a difference map has no legend: no board draws one on a trial") {
    assertEquals(
      right(
        MapLegendScene(
          MapPalette.Difference,
          LimitsScope.PerPanel,
          MapOpacity.Default,
          Theme.Light,
          StageVariant.Dark
        )
      ).map(_.description),
      None
    )
    assertEquals(MapLegendScene.describe(MapPalette.Difference, LimitsScope.Shared), None)
  }

  test("the legend exports, its words in the SVG") {
    val svg = right(SvgRenderer.render(legend().plot.scene)).value
    assert(svg.contains("fades below 10% of max"), svg)
    assert(svg.contains("50% · 90% of mass"), svg)
  }

  test("the keys drawn alone are the legend's own strip and sample, at the keys' sizes") {
    for theme <- themes do
      val keys   = right(MapLegendScene.keys(MapOpacity.Default, theme))
      val legend = grobs(this.legend(theme))
      def flat(scene: eyes4s.studio.viz.plot.PlotScene): Vector[Grob] =
        def walk(g: Grob): Vector[Grob] = g +: g.children.flatMap(walk)
        scene.scene.grobs.flatMap(walk)
      val ramp = flat(keys.ramp).collect { case p: Grob.Polygon => p }
      val full = legend.collect { case p: Grob.Polygon => p }
      // The same swatches and frame, moved to the key's own origin: no ground.
      assertEquals(ramp.size, MapLegendScene.Swatches + 1)
      assertEquals(ramp.map(_.gp), full.map(_.gp))
      assertEquals(ramp.map(_.name), full.map(_.name))
      val dx = MapLegendScene.StripLeftPx
      val dy = MapLegendScene.FirstRowPx - MapLegendScene.RampKeyHeightPx / 2.0
      assertEquals(
        ramp.map(_.points.map(xy)),
        full.map(_.points.map(xy).map((x, y) => (x - dx, y - dy)))
      )
      assert(!flat(keys.ramp).exists(_.isInstanceOf[Grob.Rect]), "the ramp key has a ground")
      assertEquals(keys.ramp.panel.xDomain.upper, MapLegendScene.RampKeyWidthPx)
      assertEquals(keys.ramp.panel.yDomain.upper, MapLegendScene.RampKeyHeightPx)
      val line   = flat(keys.isoline).collect { case l: Grob.Lines => l }
      val sample = legend.collect { case l: Grob.Lines => l }
      assertEquals(line.map(_.gp), sample.map(_.gp))
      assertEquals(
        line.flatMap(_.points.map(xy)),
        Vector((1.0, 4.0), (15.0, 4.0))
      )
      assertEquals(keys.isoline.panel.xDomain.upper, MapLegendScene.IsolineKeyWidthPx)
      assertEquals(keys.isoline.panel.yDomain.upper, MapLegendScene.IsolineKeyHeightPx)
  }

  extension (c: (Int, Int, Int, Int)) private def take3: (Int, Int, Int) = (c._1, c._2, c._3)

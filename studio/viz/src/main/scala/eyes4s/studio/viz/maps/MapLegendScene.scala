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

import eyes4s.studio.app.maps.{
  ColourLimits,
  LimitsScope,
  MapColours,
  MapOpacity,
  MapPalette,
  MapStyle
}
import eyes4s.studio.app.text.{Format, TrialText, TrialTextId}
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
import eyes4s.studio.viz.plot.{
  DataPanel,
  IntaglioColours,
  PlotScene,
  PlotSceneError,
  SceneId,
  SceneSummaries
}
import eyes4s.studio.viz.trial.TrialScene
import intaglio.{
  Anchor,
  CasingWidth,
  Clip,
  ExtentExpr,
  GraphicParams,
  GraphicsError,
  GraphicsName,
  Grob,
  HJust,
  Interval,
  Length,
  LengthExpr,
  Point,
  Rgba,
  Scene,
  SceneSemantics,
  Size,
  StrokeCasing,
  StrokeUnit,
  StrokeWidth,
  VJust,
  Viewport,
  YDirection
}

/** The legend under a trial's mass map (review ticket 17), drawn as the
  * Main board draws it below a trial's caption (`docs/studio/design/Main.dc.html`,
  * `mapLegend()` in `MAP_RECIPE.js`), on the stage, in 11 px on-stage text:
  *
  *   - "0", a strip of [[Swatches]] ramp swatches from no mass to the
  *     limit's maximum, each coloured and faded as the map colours that
  *     fraction ([[MapColours.argb]], so the first fades in under
  *     [[MapColours.MassAlphaFloor]]) at the map's opacity, framed by a
  *     hairline, then "max · fixation duration mass per cell · fades below
  *     10% of max";
  *   - a sample isoline cased as the map's are ([[TrialScene.IsolinePx]] of
  *     `--isoline-ink` inside [[TrialScene.IsolineCasingPx]] of
  *     `--isoline-case`), then "50% · 90% of mass", and " · shared limits"
  *     when the panels shown together share their colour limits.
  *
  * Every length is in logical pixels from the canvas's top-left corner, so
  * the legend keeps its size however wide its host is. Mass maps only: no
  * board draws a key for a difference map on a trial view (the System
  * board shows the diverging ramp as a palette, not as a legend), so a
  * difference map has none.
  */
final class MapLegendScene private (val plot: PlotScene, val description: String)

object MapLegendScene:

  // --- Names of the scene's grobs (the tests find them) ---------------------
  val StageName: String    = "map-legend-stage"
  val SwatchPrefix: String = "map-legend-swatch-"
  val FrameName: String    = "map-legend-frame"
  val ZeroName: String     = "map-legend-zero"
  val RampName: String     = "map-legend-ramp"
  val IsolineName: String  = "map-legend-isoline"
  val KeyName: String      = "map-legend-isolines"

  /** The swatches of the ramp strip: fractions 0, 1/7, …, 1 of the limits. */
  val Swatches: Int = 8

  /** Each swatch's size, and so the strip's: 80 × 10. */
  val SwatchWidthPx: Double  = 10.0
  val SwatchHeightPx: Double = 10.0

  /** The legend's left inset, the caption's (`TrialScene`'s 8 px). */
  val InsetPx: Double = 8.0

  /** The room the "0" takes before the strip, and the gap between a key
    * and its words.
    */
  val ZeroPx: Double = 7.0
  val GapPx: Double  = 6.0

  /** The isoline sample's length, inside a 16 px key (1 px either side). */
  val SampleWidthPx: Double = 16.0

  /** Each of the two rows, the gap between them and the padding above the
    * first and below the second.
    */
  val RowPx: Double     = 16.0
  val RowGapPx: Double  = 2.0
  val PaddingPx: Double = 2.0

  /** The legend's height: what a host gives it under the trial. */
  val HeightPx: Double = 2 * PaddingPx + 2 * RowPx + RowGapPx

  /** The strip's left edge, and the vertical centres of the two rows. */
  val StripLeftPx: Double   = InsetPx + ZeroPx + GapPx
  val FirstRowPx: Double    = PaddingPx + RowPx / 2.0
  val SecondRowPx: Double   = PaddingPx + RowPx + RowGapPx + RowPx / 2.0
  private val StripRightPx  = StripLeftPx + Swatches * SwatchWidthPx
  private val NominalWidth  = 480.0
  private val SceneIdPrefix = "studio.map-legend"

  // Intaglio physical lengths are points at 96 logical pixels per inch.
  private val PointsPerPixel         = 72.0 / 96.0
  private def pt(px: Double): Double = px * PointsPerPixel

  /** The fraction of the limits swatch `k` shows: 0 for the first, 1 for
    * the last, evenly between.
    */
  def fractionOf(k: Int): Double = k.toDouble / (Swatches - 1)

  /** The ARGB swatch `k` is drawn in at `opacity`: the colour and floor
    * alpha a mass map gives that fraction of its limits, at the map's opacity.
    */
  def swatchArgb(k: Int, opacity: MapOpacity): Int =
    opacity.over(MapColours.argb(unitMass, Some(fractionOf(k))))

  // A mass style over the unit range (what an empty map spans), so a
  // swatch's value is its fraction.
  private val unitMass: MapStyle = ColourLimits.spanning(MapPalette.Mass, Vector.empty)

  /** The words of the first row after the strip. */
  def rampText: String =
    TrialText(TrialTextId.MapLegendRamp) + TrialText(TrialTextId.Separator) +
      TrialText(TrialTextId.MapLegendFades, Format.percent(MapColours.MassAlphaFloor))

  /** The words of the second row after the isoline sample. */
  def isolineText(limits: LimitsScope): String =
    TrialText(TrialTextId.MapLegendIsolines) + (limits match
      case LimitsScope.Shared =>
        TrialText(TrialTextId.Separator) + TrialText(TrialTextId.MapLegendShared)
      case LimitsScope.PerPanel => "")

  /** What the legend of a `palette` map says to assistive technology, when
    * the map has a legend.
    */
  def describe(palette: MapPalette, limits: LimitsScope): Option[String] =
    palette match
      case MapPalette.Difference => None
      case MapPalette.Mass       =>
        val floor  = Format.decimal(MapColours.MassAlphaFloor * 100.0, 0)
        val shared = limits match
          case LimitsScope.Shared   => TrialText(TrialTextId.MapLegendDescriptionShared)
          case LimitsScope.PerPanel => ""
        Some(TrialText(TrialTextId.MapLegendDescription, floor, shared))

  /** The legend of a `palette` map drawn at `opacity` under `limits`, in
    * `theme` on `stage`; none for a difference map.
    */
  def apply(
      palette: MapPalette,
      limits: LimitsScope,
      opacity: MapOpacity,
      theme: Theme,
      stage: StageVariant
  ): Either[PlotSceneError, Option[MapLegendScene]] =
    describe(palette, limits) match
      case None              => Right(None)
      case Some(description) =>
        for
          id    <- SceneId(s"$SceneIdPrefix.${limits.toString.toLowerCase}")
          built <- Builder(limits, opacity, theme, stage).build.left.map(
            PlotSceneError.Graphics(id.value, "the map legend", _)
          )
          (grobs, viewport) = built
          panel <- DataPanel(id, viewport)
          plot  <- PlotScene(id, Scene(grobs), panel)
        yield Some(
          new MapLegendScene(
            plot.withSemantics(
              SceneSemantics.single(
                SceneSummaries.semantics(
                  id,
                  TrialText(TrialTextId.MapLegendTitle),
                  description,
                  description
                )
              )
            ),
            description
          )
        )

  private final class Builder(
      limits: LimitsScope,
      opacity: MapOpacity,
      theme: Theme,
      stage: StageVariant
  ):
    private type E[A] = Either[GraphicsError, A]

    private def name(value: String): E[GraphicsName] = GraphicsName(value, "map legend")

    private def at(x: Double, y: Double): E[Point] = Point.native(x, y)

    // A closed outline through four corners: the legend draws polygons only.
    private def box(
        left: Double,
        top: Double,
        right: Double,
        bottom: Double
    ): E[Vector[Point]] =
      for
        a <- at(left, top)
        b <- at(right, top)
        c <- at(right, bottom)
        d <- at(left, bottom)
      yield Vector(a, b, c, d)

    private def fill(colour: Rgba): E[GraphicParams] =
      GraphicParams.checked(stroke = None, fill = Some(colour))

    private def stroke(colour: Rgba, widthPx: Double): E[GraphicParams] =
      GraphicParams.checked(
        stroke = Some(colour),
        fill = None,
        lineWidth = pt(widthPx),
        lineWidthUnit = StrokeUnit.Point
      )

    private def text(
        label: String,
        x: Double,
        y: Double,
        hjust: HJust,
        named: String
    ): E[Grob] =
      for
        size <- Length.points(pt(TypeSize.T11.px.toDouble))
        gp   <- GraphicParams.checked(
          stroke = None,
          fill = Some(IntaglioColours.staged(stage, StageToken.OnStage)),
          fontFamily = Some(FontFace.SansRegular.javaFxFamily),
          fontSize = size
        )
        p <- at(x, y)
        n <- name(named)
        t <- Grob.text(label, p, Anchor(hjust, VJust.Center), gp = gp, name = Some(n))
      yield t

    private def swatch(k: Int): E[Grob] =
      val argb   = swatchArgb(k, opacity)
      val colour = Rgba.unsafe(
        (argb >> 16) & 0xff,
        (argb >> 8) & 0xff,
        argb & 0xff,
        ((argb >>> 24) & 0xff) / 255.0
      )
      val left = StripLeftPx + k * SwatchWidthPx
      for
        gp  <- fill(colour)
        pts <- box(
          left,
          FirstRowPx - SwatchHeightPx / 2.0,
          left + SwatchWidthPx,
          FirstRowPx + SwatchHeightPx / 2.0
        )
        n <- name(s"$SwatchPrefix$k")
        g <- Grob.polygon(pts, gp = gp, name = Some(n))
      yield g

    // The strip's hairline, inside its edge as the board draws it.
    private def frame: E[Grob] =
      for
        gp  <- stroke(IntaglioColours.themed(theme, ThemedToken.Hairline), 1.0)
        pts <- box(
          StripLeftPx + 0.5,
          FirstRowPx - SwatchHeightPx / 2.0 + 0.5,
          StripRightPx - 0.5,
          FirstRowPx + SwatchHeightPx / 2.0 - 0.5
        )
        n <- name(FrameName)
        g <- Grob.polygon(pts, gp = gp, name = Some(n))
      yield g

    // The isoline sample, cased as the trial's isolines are.
    private def sample: E[Grob] =
      for
        ink <- stroke(
          IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.IsolineInk)),
          TrialScene.IsolinePx
        )
        width  <- StrokeWidth.points(pt(TrialScene.IsolineCasingPx))
        casing <- StrokeCasing.checked(
          IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.IsolineCase)),
          CasingWidth.Absolute(width)
        )
        a <- at(InsetPx + 1.0, SecondRowPx)
        b <- at(InsetPx + SampleWidthPx - 1.0, SecondRowPx)
        n <- name(IsolineName)
        g <- Grob.lines(Vector(a, b), gp = ink.withCasing(casing), name = Some(n))
      yield g

    def build: E[(Vector[Grob], Viewport)] =
      for
        // Native units are logical pixels from the canvas's top-left corner,
        // however large the canvas is.
        xs       <- Interval(0.0, NominalWidth)
        ys       <- Interval(0.0, HeightPx)
        top      <- LengthExpr.npc(1.0)
        left     <- LengthExpr.npc(0.0)
        width    <- ExtentExpr.points(pt(NominalWidth))
        height   <- ExtentExpr.points(pt(HeightPx))
        viewport <- Viewport.checked(
          Point(left, top - LengthExpr(Length.pointsUnsafe(pt(HeightPx)))),
          Size.fromExtents(width, height),
          xs,
          ys,
          Clip.Off,
          yDirection = YDirection.Down
        )
        stageGp <- fill(IntaglioColours.staged(stage, StageToken.Stage))
        centre  <- Point.npc(0.5, 0.5)
        whole   <- Size.npc(1.0, 1.0)
        sn      <- name(StageName)
        ground  <- Grob.rect(centre, whole, gp = stageGp, name = Some(sn))
        zero    <- text(
          TrialText(TrialTextId.MapLegendZero),
          StripLeftPx - GapPx,
          FirstRowPx,
          HJust.Right,
          ZeroName
        )
        swatches <- (0 until Swatches).toVector.foldLeft[E[Vector[Grob]]](Right(Vector.empty))(
          (acc, k) => acc.flatMap(gs => swatch(k).map(gs :+ _))
        )
        outline <- frame
        ramp    <- text(rampText, StripRightPx + GapPx, FirstRowPx, HJust.Left, RampName)
        line    <- sample
        key     <- text(
          isolineText(limits),
          InsetPx + SampleWidthPx + GapPx,
          SecondRowPx,
          HJust.Left,
          KeyName
        )
      yield (
        Vector(
          ground,
          Grob.group(
            Vector(zero) ++ swatches ++ Vector(outline, ramp, line, key),
            viewport = Some(viewport)
          )
        ),
        viewport
      )

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

import eyes4s.studio.app.maps.{GridPoint, Isolines, MapGrid, MapRaster, MapStyle}
import eyes4s.studio.app.tokens.{PaletteToken, StageToken, StageVariant, Tokens}
import eyes4s.studio.core.backend.ScreenRegion
import eyes4s.studio.viz.plot.{DataPanel, IntaglioColours, PlotScene, PlotSceneError, SceneId}
import intaglio.{
  CasingWidth,
  Clip,
  GraphicParams,
  GraphicsError,
  GraphicsName,
  Grob,
  Interval,
  Point,
  RasterDimensions,
  RasterImage,
  RasterInterpolation,
  Rgba32,
  Scene,
  Size,
  StrokeCasing,
  StrokeUnit,
  StrokeWidth,
  Viewport,
  YDirection
}

/** One density map drawn alone (ticket bd-01M42K7ZYDRR90JXZYNDD2HVXP; the
  * Figures board's panel C tiles): the screen region the grid covers, in
  * screen pixels with y down, on its stage; the map's raster in its style,
  * cell for cell and opaque; and its isolines at the served levels, cased
  * as on the trial view (`--isoline-ink` inside `--isoline-case`). Every
  * colour is a token's or the map palette's; nothing here derives a value
  * or a level.
  */
object MapTileScene:

  val StageName: String    = "map-tile-stage"
  val MapName: String      = "map-tile-map"
  val IsolinesName: String = "map-tile-isolines"

  /** A tile's isoline ink and its casing, in screen pixels of the tile. */
  val IsolinePx: Double       = 1.0
  val IsolineCasingPx: Double = 2.0

  private val PointsPerPixel            = 72.0 / 96.0
  private def pt(px: Double): Double    = px * PointsPerPixel
  private def name(value: String)       = GraphicsName(value, "map tile scene")
  private def toRgba(argb: Int): Rgba32 =
    Rgba32.unsafe((argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff, (argb >>> 24) & 0xff)

  /** `grid` in `style` over `region`, on `stage`. `id` names the scene. */
  def of(
      id: String,
      grid: MapGrid,
      style: MapStyle,
      region: ScreenRegion,
      stage: StageVariant
  ): Either[PlotSceneError, PlotScene] =
    for
      sceneId <- SceneId(id)
      built   <- layers(grid, style, region, stage).left.map(
        PlotSceneError.Graphics(sceneId.value, "the map tile", _)
      )
      (grobs, viewport) = built
      panel <- DataPanel(sceneId, viewport)
      scene <- PlotScene(sceneId, Scene(grobs), panel)
    yield scene

  private def layers(
      grid: MapGrid,
      style: MapStyle,
      region: ScreenRegion,
      stage: StageVariant
  ): Either[GraphicsError, (Vector[Grob], Viewport)] =
    // Opaque, cell for cell: a figure tile is the map alone, not an overlay.
    val raster = MapRaster.render(grid, style)
    for
      xs      <- Interval(region.left, region.right)
      ys      <- Interval(region.top, region.bottom)
      origin  <- Point.npc(0.0, 0.0)
      whole   <- Size.npc(1.0, 1.0)
      vp      <- Viewport.checked(origin, whole, xs, ys, Clip.On, yDirection = YDirection.Down)
      centre  <- Point.npc(0.5, 0.5)
      stageGp <- GraphicParams.checked(
        stroke = None,
        fill = Some(IntaglioColours.staged(stage, StageToken.Stage))
      )
      stageName <- name(StageName)
      field     <- Grob.rect(centre, whole, gp = stageGp, name = Some(stageName))
      dims      <- RasterDimensions(raster.width, raster.height)
      image = RasterImage.tabulate(dims)((x, y) => toRgba(raster.pixel(x, y)))
      mapName <- name(MapName)
      map     <- Grob.image(
        image,
        centre,
        whole,
        interpolation = RasterInterpolation.Nearest,
        name = Some(mapName)
      )
      lines <- isolines(grid, region)
    yield (Vector(Grob.group(Vector(field, map) ++ lines, viewport = Some(vp))), vp)

  private def isolines(
      grid: MapGrid,
      region: ScreenRegion
  ): Either[GraphicsError, Vector[Grob]] =
    val cellW            = region.width / grid.columns
    val cellH            = region.height / grid.rows
    def at(p: GridPoint) = Point.native(region.left + p.x * cellW, region.top + p.y * cellH)
    val segments         = Isolines.of(grid).flatMap(_.segments)
    if segments.isEmpty then Right(Vector.empty)
    else
      def colour(t: PaletteToken) = IntaglioColours.toIntaglio(Tokens.palette(t))
      for
        ink <- GraphicParams.checked(
          stroke = Some(colour(PaletteToken.IsolineInk)),
          fill = None,
          lineWidth = pt(IsolinePx),
          lineWidthUnit = StrokeUnit.Point
        )
        width  <- StrokeWidth.points(pt(IsolineCasingPx))
        casing <- StrokeCasing.checked(
          colour(PaletteToken.IsolineCase),
          CasingWidth.Absolute(width)
        )
        pairs <- segments.foldLeft[Either[GraphicsError, Vector[(Point, Point)]]](
          Right(Vector.empty)
        ) { case (acc, (a, b)) =>
          for
            ps <- acc
            pa <- at(a)
            pb <- at(b)
          yield ps :+ (pa -> pb)
        }
        n     <- name(IsolinesName)
        lines <- Grob.segments(pairs, gp = ink.withCasing(casing), name = Some(n))
      yield Vector(lines)

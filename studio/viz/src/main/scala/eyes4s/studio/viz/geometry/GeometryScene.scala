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

package eyes4s.studio.viz.geometry

import eyes4s.studio.app.geometry.{
  DensityPicture,
  FramePicture,
  MarkPicture,
  MarkPlace,
  ThumbnailPicture
}
import eyes4s.studio.app.tokens.{
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  ThemedToken,
  Tokens
}
import eyes4s.studio.viz.plot.{DataPanel, IntaglioColours, PlotScene, PlotSceneError, SceneId}
import intaglio.{
  BatchColumn,
  Clip,
  ExtentExpr,
  GraphicParams,
  GraphicsError,
  GraphicsName,
  Grob,
  Interval,
  LineType,
  Point,
  PointShape,
  Rgba,
  Scene,
  Size,
  StrokeUnit,
  Viewport,
  YDirection
}

/** The geometry panel's placement pictures (ticket S5.5; Data.dc.html,
  * "Check placement"), drawn in screen pixels with y down: the screen on
  * its stage, the image frame (the analysis window) as a grey field with a
  * dashed on-stage outline, and either one trial's records at their
  * corrected positions or every record's binned density.
  *
  * A record on the screen is a filled neutral mark with the stage halo,
  * inside the dashed frame or outside it as eyes4s placed it; one off the
  * screen lies outside the picture and is only counted in the thumbnail's
  * label. Every colour is a token; the positions and levels are
  * the panel's pictures, which eyes4s placed and binned.
  */
object GeometryScene:

  private val PointsPerPixel            = 72.0 / 96.0
  private def px(value: Double): Double = value * PointsPerPixel

  /** The map opacity every density is drawn at (DESIGN_SPEC section 12). */
  val DensityOpacity: Double = 0.6

  /** One representative trial. `id` names the scene for its host. */
  def thumbnail(
      id: String,
      theme: Theme,
      stage: StageVariant,
      frame: FramePicture,
      trial: ThumbnailPicture
  ): Either[PlotSceneError, PlotScene] =
    build(id, frame)(() => marks(theme, stage, trial.marks).map(_.toVector))(stage)

  /** Every record binned, as ramp levels over the screen. */
  def density(
      id: String,
      stage: StageVariant,
      frame: FramePicture,
      picture: DensityPicture
  ): Either[PlotSceneError, PlotScene] =
    build(id, frame)(() => cells(frame, picture))(stage)

  private def build(id: String, frame: FramePicture)(
      content: () => Either[GraphicsError, Vector[Grob]]
  )(stage: StageVariant): Either[PlotSceneError, PlotScene] =
    for
      sceneId <- SceneId(id)
      built   <- layers(frame, content, stage).left.map(
        PlotSceneError.Graphics(sceneId.value, "the geometry scene", _)
      )
      (grobs, viewport) = built
      panel <- DataPanel(sceneId, viewport)
      scene <- PlotScene(sceneId, Scene(grobs), panel)
    yield scene

  private def layers(
      frame: FramePicture,
      content: () => Either[GraphicsError, Vector[Grob]],
      stage: StageVariant
  ): Either[GraphicsError, (Vector[Grob], Viewport)] =
    val w = frame.screenWidth.toDouble
    val h = frame.screenHeight.toDouble
    for
      xs      <- Interval(0.0, w)
      ys      <- Interval(0.0, h)
      origin  <- Point.npc(0.0, 0.0)
      whole   <- Size.npc(1.0, 1.0)
      vp      <- Viewport.checked(origin, whole, xs, ys, Clip.On, yDirection = YDirection.Down)
      stageGp <- GraphicParams.checked(
        stroke = None,
        fill = Some(IntaglioColours.staged(stage, StageToken.Stage))
      )
      centre    <- Point.npc(0.5, 0.5)
      screen    <- Grob.rect(centre, whole, gp = stageGp)
      imageAt   <- Point.native(frame.left + frame.width / 2.0, frame.top + frame.height / 2.0)
      imageSize <- sizeOf(frame.width.toDouble, frame.height.toDouble)
      fieldGp   <- GraphicParams.checked(
        stroke = None,
        fill = Some(IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.Screen)))
      )
      fieldName <- name("image-frame")
      field     <- Grob.rect(imageAt, imageSize, gp = fieldGp, name = Some(fieldName))
      outlineGp <- GraphicParams.checked(
        stroke = Some(IntaglioColours.staged(stage, StageToken.OnStage)),
        fill = None,
        lineWidth = px(1.0),
        lineType = LineType.Dashed,
        lineWidthUnit = StrokeUnit.Point
      )
      outlineName <- name("window-outline")
      outline     <- Grob.rect(imageAt, imageSize, gp = outlineGp, name = Some(outlineName))
      drawn       <- content()
    yield (
      Vector(
        Grob.group(Vector(screen, field) ++ drawn ++ Vector(outline), viewport = Some(vp))
      ),
      vp
    )

  private def sizeOf(width: Double, height: Double): Either[GraphicsError, Size] =
    for
      w <- ExtentExpr.native(width)
      h <- ExtentExpr.native(height)
    yield Size.fromExtents(w, h)

  private def name(value: String): Either[GraphicsError, GraphicsName] =
    GraphicsName(value, "geometry scene")

  private def marks(
      theme: Theme,
      stage: StageVariant,
      ms: Vector[MarkPicture]
  ): Either[GraphicsError, Option[Grob]] =
    val drawn = ms.filter(_.place != MarkPlace.OutsideScreen)
    if drawn.isEmpty then Right(None)
    else
      val ink  = IntaglioColours.themed(theme, ThemedToken.NeutralMark)
      val halo = IntaglioColours.staged(stage, StageToken.Halo)
      for
        filled <- GraphicParams.checked(
          stroke = Some(halo),
          fill = Some(ink),
          lineWidth = px(1.0),
          lineWidthUnit = StrokeUnit.Point
        )
        points <- traverse(drawn)(m => Point.native(m.x, m.y))
        size   <- ExtentExpr.points(px(5.0))
        named  <- name("records")
        batch  <- Grob.pointBatch(
          points,
          sizes = BatchColumn.Constant(size),
          shapes = BatchColumn.Constant(PointShape.Circle),
          graphicParams = BatchColumn.Constant(filled),
          name = Some(named)
        )
      yield Some(batch)

  private def cells(
      frame: FramePicture,
      picture: DensityPicture
  ): Either[GraphicsError, Vector[Grob]] =
    val cw   = frame.screenWidth.toDouble / picture.columns
    val ch   = frame.screenHeight.toDouble / picture.rows
    val ramp =
      Vector(PaletteToken.Ramp0, PaletteToken.Ramp1, PaletteToken.Ramp2, PaletteToken.Ramp3)
    def colour(level: Int): Rgba =
      IntaglioColours.toIntaglio(Tokens.palette(ramp((level - 1).max(0).min(ramp.size - 1))))
    val filled = picture.levels.zipWithIndex.filter(_._1 > 0)
    for
      size <- sizeOf(cw, ch)
      gps  <- traverse((1 to DensityPicture.Levels).toVector)(level =>
        GraphicParams.checked(stroke = None, fill = Some(colour(level)), alpha = DensityOpacity)
      )
      rects <- traverse(filled) { (level, index) =>
        val col = index % picture.columns
        val row = index / picture.columns
        Point
          .native((col + 0.5) * cw, (row + 0.5) * ch)
          .flatMap(at => Grob.rect(at, size, gp = gps(level - 1)))
      }
      named <- name("density")
    yield Vector(Grob.group(rects, name = Some(named)))

  private def traverse[A, B](as: Vector[A])(f: A => Either[GraphicsError, B]) =
    as.foldLeft[Either[GraphicsError, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    }

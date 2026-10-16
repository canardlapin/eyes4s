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
  FrameState,
  GeometryPanelVM,
  MarkPicture,
  MarkPlace,
  ThumbnailPicture
}
import eyes4s.studio.app.maps.DensityCasing
import eyes4s.studio.app.text.{Format, GeometryText}
import eyes4s.studio.app.text.GeometryTextId.{DensityCellsDrawn, DensityCellsDrawnOne}
import eyes4s.studio.app.tokens.{
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  ThemedToken,
  TokenUsage,
  Tokens
}
import eyes4s.studio.viz.plot.{
  DataPanel,
  IntaglioColours,
  MissingAssetHatch,
  PlotScene,
  PlotSceneError,
  SceneId,
  SceneSummaries
}
import intaglio.{
  BatchColumn,
  Clip,
  ExtentExpr,
  GraphicParams,
  GraphicsError,
  GraphicsName,
  Grob,
  Interval,
  LineCap,
  LineType,
  Point,
  PointShape,
  Rgba,
  Scene,
  SceneSemantics,
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

  /** The density's casing bands (DESIGN_SPEC section 15.1, "Contrast"):
    * the stage halo and, inside it, the inner casing, each this wide outside
    * a cased level's cells. The halo is stroked at twice the two bands and
    * the inner casing at twice one band, centred on the cells' edge, under
    * the fills, so that one band of each shows outside the cell.
    */
  val DensityBandPx: Double = 1.0

  /** The names of the density's casing strokes. */
  val DensityHaloName: String   = "density-halo"
  val DensityCasingName: String = "density-casing"

  /** The names of a missing asset's hatched frame and its X. */
  val MissingFieldName: String = "missing-asset"
  val MissingCrossName: String = "missing-asset-cross"

  // The Data board's missing-asset X: 22-pixel strokes of the screen, at the
  // thumbnail's scale, each arm reaching 140 of the frame's 768 pixels from
  // its centre.
  private val CrossArm: Double     = 140.0 / 768.0
  private val CrossWidthPx: Double = 22.0 * MissingAssetHatch.ThumbnailScale

  /** One representative trial. `id` names the scene for its host. A trial
    * whose asset is missing or unreadable has its image frame hatched, with
    * an X, under its records (DESIGN_SPEC section 9: never blank).
    */
  def thumbnail(
      id: String,
      theme: Theme,
      stage: StageVariant,
      frame: FramePicture,
      trial: ThumbnailPicture
  ): Either[PlotSceneError, PlotScene] =
    val hatched = Option.when(trial.frame != FrameState.Present)(theme)
    val asset   = Option
      .when(trial.frame != FrameState.Present)(s" ${GeometryPanelVM.thumbCount(trial)}.")
      .getOrElse("")
    build(id, frame, hatched)(() => marks(theme, stage, trial.marks).map(_.toVector))(stage)
      .map { scene =>
        scene.withSemantics(
          SceneSemantics.single(
            SceneSummaries.semantics(
              scene.id,
              s"Placement preview: ${trial.trial.label}",
              s"${trial.records} records; ${trial.outsideWindow} outside the image window; " +
                s"${trial.outsideScreen} outside the screen.",
              s"Screen ${frame.screenWidth} × ${frame.screenHeight} pixels, y down. " +
                s"Image window at (${frame.left}, ${frame.top}), ${frame.width} × ${frame.height} pixels.$asset " +
                s"${trial.marks.count(_.place != MarkPlace.OutsideScreen)} marks drawn; off-screen records are counted only."
            )
          )
        )
      }

  /** Every record binned, as ramp levels over the screen. */
  def density(
      id: String,
      stage: StageVariant,
      frame: FramePicture,
      picture: DensityPicture
  ): Either[PlotSceneError, PlotScene] =
    build(id, frame)(() => cells(frame, picture, stage))(stage).map { scene =>
      scene.withSemantics(
        SceneSemantics.single(
          SceneSummaries.semantics(
            scene.id,
            "Placement density",
            s"${picture.records} records binned into ${picture.columns} × ${picture.rows} cells.",
            s"Screen ${frame.screenWidth} × ${frame.screenHeight} pixels, y down. " +
              levelsText(picture)
          )
        )
      )
    }

  /** "{n} cells drawn at four log-spaced levels: 1–3, 4–15, 16–63 and at
    * least 64 records per cell.", from the picture's own steps (DESIGN_SPEC
    * section 15.1), so it names the ranges the key labels.
    */
  private def levelsText(picture: DensityPicture): String =
    val ranges = GeometryPanelVM.densityLabels(picture.steps).init
    val last   = Format.count(picture.steps.starts.last)
    picture.levels.count(_ > 0) match
      case 1 => GeometryText(DensityCellsDrawnOne, (ranges :+ last)*)
      case n => GeometryText(DensityCellsDrawn, (Format.count(n.toLong) +: ranges :+ last)*)

  private def build(id: String, frame: FramePicture, hatched: Option[Theme] = None)(
      content: () => Either[GraphicsError, Vector[Grob]]
  )(stage: StageVariant): Either[PlotSceneError, PlotScene] =
    for
      sceneId <- SceneId(id)
      built   <- layers(frame, content, stage, hatched).left.map(
        PlotSceneError.Graphics(sceneId.value, "the geometry scene", _)
      )
      (grobs, viewport) = built
      panel <- DataPanel(sceneId, viewport)
      scene <- PlotScene(sceneId, Scene(grobs), panel)
    yield scene

  /** The screen, the image frame (hatched with an X in `hatched`'s theme for
    * a missing asset), `content` and the frame's dashed outline.
    */
  private def layers(
      frame: FramePicture,
      content: () => Either[GraphicsError, Vector[Grob]],
      stage: StageVariant,
      hatched: Option[Theme]
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
      fieldGp   <- hatched.fold(
        GraphicParams.checked(
          stroke = None,
          fill = Some(IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.Screen)))
        )
      )(MissingAssetHatch.paint(_, MissingAssetHatch.Thumbnail))
      fieldName <- name(if hatched.isDefined then MissingFieldName else "image-frame")
      field     <- Grob.rect(imageAt, imageSize, gp = fieldGp, name = Some(fieldName))
      cross <- hatched.fold[Either[GraphicsError, Vector[Grob]]](Right(Vector.empty))(theme =>
        name(MissingCrossName).flatMap(n =>
          MissingAssetHatch
            .cross(
              theme,
              frame.left + frame.width / 2.0,
              frame.top + frame.height / 2.0,
              frame.height * CrossArm,
              px(CrossWidthPx),
              Some(n)
            )
            .map(Vector(_))
        )
      )
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
        Grob.group(
          Vector(screen, field) ++ cross ++ drawn ++ Vector(outline),
          viewport = Some(vp)
        )
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

  /** The density's cells, opaque, in their ramp stops, under the two-band
    * casing where [[DensityCasing]] cases their level on `stage`: the edges
    * between a cased level's cells and empty ones, never the edges between
    * two occupied cells (level against level) or along the screen's border.
    */
  private def cells(
      frame: FramePicture,
      picture: DensityPicture,
      stage: StageVariant
  ): Either[GraphicsError, Vector[Grob]] =
    val cw                       = frame.screenWidth.toDouble / picture.columns
    val ch                       = frame.screenHeight.toDouble / picture.rows
    def colour(level: Int): Rgba =
      IntaglioColours.toIntaglio(Tokens.palette(DensityCasing.fill(level)))
    val filled = picture.levels.zipWithIndex.filter(_._1 > 0)
    val edges  = casedEdges(picture, DensityCasing.levels(stage))
    for
      size <- sizeOf(cw, ch)
      gps  <- traverse((1 to DensityPicture.Levels).toVector)(level =>
        GraphicParams.checked(stroke = None, fill = Some(colour(level)))
      )
      rects <- traverse(filled) { (level, index) =>
        val col = index % picture.columns
        val row = index / picture.columns
        Point
          .native((col + 0.5) * cw, (row + 0.5) * ch)
          .flatMap(at => Grob.rect(at, size, gp = gps(level - 1)))
      }
      casing <- casingGrobs(edges, cw, ch, stage)
      named  <- name("density")
    yield casing :+ Grob.group(rects, name = Some(named))

  /** A run of cell edges along one grid line, in grid units: horizontal at
    * row line `line` from column `from` to `to`, or vertical at column line
    * `line` from row `from` to `to`.
    */
  private final case class EdgeRun(horizontal: Boolean, line: Int, from: Int, to: Int)

  /** The edges between an occupied cell of a level in `cased` and an empty
    * cell, merged into maximal runs along each grid line, so that each line
    * is stroked once.
    */
  private def casedEdges(picture: DensityPicture, cased: Vector[Boolean]): Vector[EdgeRun] =
    val cols                                 = picture.columns
    val rows                                 = picture.rows
    def level(col: Int, row: Int): Int       = picture.levels(row * cols + col)
    def isCased(col: Int, row: Int): Boolean =
      val l = level(col, row)
      l > 0 && cased((l - 1).min(cased.size - 1))
    // An edge between two cells is cased when one is a cased level and the
    // other is empty. Only the grid's inner lines are walked: the screen's
    // border is not a ground.
    def between(a: (Int, Int), b: (Int, Int)): Boolean =
      (isCased(a._1, a._2) && level(b._1, b._2) == 0) ||
        (isCased(b._1, b._2) && level(a._1, a._2) == 0)
    def runs(cells: Vector[Int]): Vector[(Int, Int)] =
      cells.foldLeft(Vector.empty[(Int, Int)]) {
        case (acc :+ ((from, to)), c) if c == to => acc :+ (from -> (c + 1))
        case (acc, c)                            => acc :+ (c    -> (c + 1))
      }
    val horizontal = for
      line       <- (1 until rows).toVector
      (from, to) <- runs((0 until cols).toVector.filter(c => between((c, line - 1), (c, line))))
    yield EdgeRun(horizontal = true, line, from, to)
    val vertical = for
      line       <- (1 until cols).toVector
      (from, to) <- runs((0 until rows).toVector.filter(r => between((line - 1, r), (line, r))))
    yield EdgeRun(horizontal = false, line, from, to)
    horizontal ++ vertical

  /** The halo and the inner casing along `edges`, square-capped so that the
    * bands close around a region's corners; nothing when no edge is cased.
    */
  private def casingGrobs(
      edges: Vector[EdgeRun],
      cw: Double,
      ch: Double,
      stage: StageVariant
  ): Either[GraphicsError, Vector[Grob]] =
    if edges.isEmpty then Right(Vector.empty)
    else
      val halo = IntaglioColours.staged(stage, StageToken.Halo)
      // The inner casing is a palette token: every theme resolves it alike.
      val inner = IntaglioColours.toIntaglio(
        Tokens.resolve(TokenUsage.innerCasing(stage), Theme.Light, stage)
      )
      def stroke(colour: Rgba, bands: Double) = GraphicParams.checked(
        stroke = Some(colour),
        fill = None,
        lineWidth = px(2.0 * bands * DensityBandPx),
        lineCap = LineCap.Square,
        lineWidthUnit = StrokeUnit.Point
      )
      for
        segments <- traverse(edges) { e =>
          if e.horizontal then
            for
              a <- Point.native(e.from * cw, e.line * ch)
              b <- Point.native(e.to * cw, e.line * ch)
            yield a -> b
          else
            for
              a <- Point.native(e.line * cw, e.from * ch)
              b <- Point.native(e.line * cw, e.to * ch)
            yield a -> b
        }
        haloGp  <- stroke(halo, 2.0)
        innerGp <- stroke(inner, 1.0)
        haloN   <- name(DensityHaloName)
        innerN  <- name(DensityCasingName)
        haloG   <- Grob.segments(segments, gp = haloGp, name = Some(haloN))
        innerG  <- Grob.segments(segments, gp = innerGp, name = Some(innerN))
      yield Vector(haloG, innerG)

  private def traverse[A, B](as: Vector[A])(f: A => Either[GraphicsError, B]) =
    as.foldLeft[Either[GraphicsError, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    }

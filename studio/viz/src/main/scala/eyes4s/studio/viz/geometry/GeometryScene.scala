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
import eyes4s.studio.app.maps.{DensityBand, DensityCasing, DensityGround}
import eyes4s.studio.app.text.{Format, GeometryText}
import eyes4s.studio.app.text.GeometryTextId.{DensityCellsDrawn, DensityCellsDrawnOne}
import eyes4s.studio.app.tokens.{
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  ThemedToken,
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

  /** The width of the one casing band a cased density edge wears, outside
    * the cell (DESIGN_SPEC section 15.1, "Contrast"). It is stroked at twice
    * this width, centred on the edge, under the fills, so that this much
    * shows outside the cell.
    */
  val DensityBandPx: Double = 1.0

  /** The names of the density's casing strokes: the edges that wear the
    * stage halo, and those that wear the inner casing.
    */
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

  /** The density's cells, opaque, in their ramp stops, over their casing:
    * an edge between an occupied cell and an empty one wears the band that
    * [[DensityCasing]] gives its level against the ground the edge lies on
    * (the stage, or the screen field inside the image frame), if any. The
    * edges between two occupied cells (level against level) and the
    * screen's border are never cased.
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
    val runs   = casedEdges(frame, picture, stage)
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
      halo  <- casing(runs, DensityBand.Halo, DensityHaloName, stage)
      inner <- casing(runs, DensityBand.InnerCasing, DensityCasingName, stage)
      named <- name("density")
    yield (halo ++ inner).toVector :+ Grob.group(rects, name = Some(named))

  /** A run of cased edge along one grid line, in screen pixels: horizontal at
    * `y = line` from `x = from` to `to`, or vertical at `x = line` from
    * `y = from` to `to`, wearing `band`.
    */
  private final case class EdgeRun(
      horizontal: Boolean,
      line: Double,
      from: Double,
      to: Double,
      band: DensityBand
  )

  /** The cased edges: each edge between an occupied cell and an empty one,
    * split where the image frame's boundary crosses it, each part cased by
    * its level against the ground its band lies on (the empty side, just
    * outside the cell). Parts on one grid line with one band are merged
    * into maximal runs, so that each is stroked once.
    */
  private def casedEdges(
      frame: FramePicture,
      picture: DensityPicture,
      stage: StageVariant
  ): Vector[EdgeRun] =
    val cols                           = picture.columns
    val rows                           = picture.rows
    val cw                             = frame.screenWidth.toDouble / cols
    val ch                             = frame.screenHeight.toDouble / rows
    val (left, top)                    = (frame.left.toDouble, frame.top.toDouble)
    val (right, bottom)                = (left + frame.width, top + frame.height)
    def level(col: Int, row: Int): Int = picture.levels(row * cols + col)
    // The ground under a point just off a grid line, on the empty side.
    def groundAt(x: Double, y: Double): DensityGround =
      if x >= left && x < right && y >= top && y < bottom then DensityGround.Screen
      else DensityGround.Stage
    // The cell of one side of an edge, if it is occupied and the other is
    // empty: its level and the sign of the empty side (+1 below or right).
    def cased(a: Int, b: Int): Option[(Int, Double)] =
      if a > 0 && b == 0 then Some(a -> 1.0)
      else if b > 0 && a == 0 then Some(b -> -1.0)
      else None
    // An edge from `from` to `to` along a line, split at the frame's
    // boundary on that axis, each part with its band.
    def parts(
        horizontal: Boolean,
        line: Double,
        from: Double,
        to: Double,
        lvl: Int,
        side: Double
    ): Vector[EdgeRun] =
      val (lo, hi) = if horizontal then (left, right) else (top, bottom)
      val cuts = (Vector(from) ++ Vector(lo, hi).filter(c => c > from && c < to) :+ to).sorted
      cuts.zip(cuts.drop(1)).flatMap { (a, b) =>
        val mid    = (a + b) / 2.0
        val off    = line + side * DensityBandPx / 2.0
        val ground = if horizontal then groundAt(mid, off) else groundAt(off, mid)
        DensityCasing.band(lvl, stage, ground).map(EdgeRun(horizontal, line, a, b, _))
      }
    // Only the grid's inner lines are walked: the screen's border is not a
    // ground.
    val horizontal = for
      line        <- (1 until rows).toVector
      col         <- (0 until cols).toVector
      (lvl, side) <- cased(level(col, line - 1), level(col, line)).toVector
      run         <- parts(true, line * ch, col * cw, (col + 1) * cw, lvl, side)
    yield run
    val vertical = for
      line        <- (1 until cols).toVector
      row         <- (0 until rows).toVector
      (lvl, side) <- cased(level(line - 1, row), level(line, row)).toVector
      run         <- parts(false, line * cw, row * ch, (row + 1) * ch, lvl, side)
    yield run
    (horizontal ++ vertical)
      .groupBy(r => (r.band, r.horizontal, r.line))
      .toVector
      .sortBy((k, _) => (k._1.ordinal, !k._2, k._3))
      .flatMap { (_, rs) =>
        rs.sortBy(_.from).foldLeft(Vector.empty[EdgeRun]) {
          case (acc :+ last, r) if r.from == last.to => acc :+ last.copy(to = r.to)
          case (acc, r)                              => acc :+ r
        }
      }

  /** The runs that wear `band`, as one square-capped stroke named `named`,
    * twice [[DensityBandPx]] wide; nothing when no run wears it.
    */
  private def casing(
      runs: Vector[EdgeRun],
      band: DensityBand,
      named: String,
      stage: StageVariant
  ): Either[GraphicsError, Option[Grob]] =
    val mine = runs.filter(_.band == band)
    if mine.isEmpty then Right(None)
    else
      for
        segments <- traverse(mine) { e =>
          if e.horizontal then
            for
              a <- Point.native(e.from, e.line)
              b <- Point.native(e.to, e.line)
            yield a -> b
          else
            for
              a <- Point.native(e.line, e.from)
              b <- Point.native(e.line, e.to)
            yield a -> b
        }
        gp <- GraphicParams.checked(
          stroke = Some(IntaglioColours.toIntaglio(band.colour(stage))),
          fill = None,
          lineWidth = px(2.0 * DensityBandPx),
          lineCap = LineCap.Square,
          lineWidthUnit = StrokeUnit.Point
        )
        n <- name(named)
        g <- Grob.segments(segments, gp = gp, name = Some(n))
      yield Some(g)

  private def traverse[A, B](as: Vector[A])(f: A => Either[GraphicsError, B]) =
    as.foldLeft[Either[GraphicsError, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    }

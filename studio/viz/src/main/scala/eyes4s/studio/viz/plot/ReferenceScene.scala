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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.tokens.{FontFace, Theme, ThemedToken, TypeSize}
import intaglio.{
  Anchor,
  BatchColumn,
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
  PointShape,
  Rgba,
  Scene,
  SceneSemantics,
  Size,
  StrokeUnit,
  VJust,
  Viewport
}

/** A mark role of the reference scene, drawn as DESIGN_SPEC section 5 draws it:
  * hue, shape and fill together, never hue alone.
  */
enum ReferenceRole derives CanEqual:
  case Query, Match, Control

/** One mark of the reference scene: a role at a data position. */
final case class ReferenceMark(role: ReferenceRole, at: DataPoint)

/** The reference scene of the canvas host (S4.1).
  *
  * A small, fixed scene that exercises what a studio plot draws: a surface
  * fill, a clipped data panel with grid rules and a frame, the three role
  * marks in one point batch, and sans and mono labels. Its recorded drawing
  * operations are the host's op-log golden, and its snapshots the host's
  * visual references. Every colour is a token; the values are fixed test
  * data, not results.
  */
object ReferenceScene:

  /** The data range of the panel. */
  val xDomain: (Double, Double) = (0.0, 10.0)
  val yDomain: (Double, Double) = (0.0, 5.0)

  /** The marks, in draw order. */
  val marks: Vector[ReferenceMark] = Vector(
    ReferenceMark(ReferenceRole.Control, DataPoint(1.0, 2.0)),
    ReferenceMark(ReferenceRole.Control, DataPoint(5.0, 0.5)),
    ReferenceMark(ReferenceRole.Control, DataPoint(9.0, 4.5)),
    ReferenceMark(ReferenceRole.Match, DataPoint(3.0, 1.0)),
    ReferenceMark(ReferenceRole.Match, DataPoint(7.0, 2.5)),
    ReferenceMark(ReferenceRole.Query, DataPoint(2.5, 3.5)),
    ReferenceMark(ReferenceRole.Query, DataPoint(6.0, 4.0)),
    ReferenceMark(ReferenceRole.Query, DataPoint(8.5, 1.5))
  )

  /** The name of the point batch that holds [[marks]]. */
  val marksName: String = "reference-marks"

  private val PointsPerPixel = 72.0 / 96.0

  // A length in logical pixels as Intaglio points (96 logical pixels per inch).
  private def px(value: Double): Double = value * PointsPerPixel

  /** The reference scene in `theme`. */
  def apply(theme: Theme): Either[PlotSceneError, PlotScene] =
    for
      id    <- SceneId(s"studio.reference.${theme.toString.toLowerCase}")
      scene <- build(theme).left.map(
        PlotSceneError.Graphics(id.value, "the reference scene", _)
      )
      (grobs, viewport) = scene
      panel     <- DataPanel(id, viewport)
      plotScene <- PlotScene(id, Scene(grobs), panel)
    yield plotScene.withSemantics(
      SceneSemantics.single(
        SceneSummaries.semantics(
          id,
          "Reference plot",
          "Eight reference marks: three controls, two matches and three queries on a gridded panel.",
          "Control marks are outlined circles, matches are diamonds and queries are filled circles. " +
            "The x range is 0 to 10; the y range is 0 to 5. Fixed example data, not study results."
        )
      )
    )

  private def build(theme: Theme): Either[GraphicsError, (Vector[Grob], Viewport)] =
    def colour(token: ThemedToken): Rgba          = IntaglioColours.themed(theme, token)
    def rule(token: ThemedToken, widthPx: Double) =
      GraphicParams.checked(
        stroke = Some(colour(token)),
        fill = None,
        lineWidth = px(widthPx),
        lineWidthUnit = StrokeUnit.Point
      )
    def label(face: FontFace, token: ThemedToken, size: TypeSize) =
      Length
        .points(px(size.px.toDouble))
        .flatMap(fontSize =>
          GraphicParams.checked(
            stroke = None,
            fill = Some(colour(token)),
            fontFamily = Some(face.javaFxFamily),
            fontSize = fontSize
          )
        )
    def mark(role: ReferenceRole) = role match
      case ReferenceRole.Query =>
        GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Query)))
      case ReferenceRole.Match =>
        GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Match)))
      case ReferenceRole.Control =>
        GraphicParams.checked(
          stroke = Some(colour(ThemedToken.Control)),
          fill = None,
          lineWidth = px(1.5),
          lineWidthUnit = StrokeUnit.Point
        )
    def shape(role: ReferenceRole) = role match
      case ReferenceRole.Query   => PointShape.Circle
      case ReferenceRole.Match   => PointShape.Diamond
      case ReferenceRole.Control => PointShape.Circle
    def native(x: Double, y: Double)                                    = Point.native(x, y)
    def traverse[A, B](as: Vector[A])(f: A => Either[GraphicsError, B]) =
      as.foldLeft[Either[GraphicsError, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
        acc.flatMap(bs => f(a).map(bs :+ _))
      }

    val (x0, x1) = xDomain
    val (y0, y1) = yDomain
    for
      xScale    <- Interval(x0, x1)
      yScale    <- Interval(y0, y1)
      origin    <- Point.npc(0.12, 0.14)
      size      <- Size.npc(0.82, 0.76)
      viewport  <- Viewport.checked(origin, size, xScale, yScale, Clip.On)
      unclipped <- Viewport.checked(origin, size, xScale, yScale, Clip.Off)
      whole     <- Size.npc(1.0, 1.0)
      centre    <- Point.npc(0.5, 0.5)
      surfaceGp <- GraphicParams.checked(
        stroke = None,
        fill = Some(colour(ThemedToken.Surface))
      )
      background <- Grob.rect(centre, whole, gp = surfaceGp)
      gridGp     <- rule(ThemedToken.Hairline, 1.0)
      gridX      <- traverse(Vector(2.0, 4.0, 6.0, 8.0))(x =>
        for a <- native(x, y0); b <- native(x, y1) yield (a, b)
      )
      gridY <- traverse(Vector(1.0, 2.0, 3.0, 4.0))(y =>
        for a <- native(x0, y); b <- native(x1, y) yield (a, b)
      )
      gridName  <- name("reference-grid")
      grid      <- Grob.segments(gridX ++ gridY, gp = gridGp, name = Some(gridName))
      frameGp   <- rule(ThemedToken.Ink3, 1.0)
      frameName <- name("reference-frame")
      frame     <- Grob.rect(centre, whole, gp = frameGp, name = Some(frameName))
      points    <- traverse(marks)(m => native(m.at.x, m.at.y))
      gps       <- traverse(marks)(m => mark(m.role))
      markSize  <- ExtentExpr.points(px(4.5))
      marksId   <- name(marksName)
      markGrob  <- Grob.pointBatch(
        points,
        sizes = BatchColumn.Constant(markSize),
        shapes = BatchColumn.compact(marks.map(m => shape(m.role))),
        graphicParams = BatchColumn.compact(gps),
        name = Some(marksId)
      )
      titleGp <- label(FontFace.SansRegular, ThemedToken.Ink2, TypeSize.T11)
      titleAt <- Point.npc(0.12, 0.95)
      title   <- Grob.text(
        "Reference scene · canvas host",
        titleAt,
        Anchor(HJust.Left, VJust.Top),
        gp = titleGp
      )
      tickGp <- label(FontFace.MonoRegular, ThemedToken.Ink3, TypeSize.T11)
      below  <- LengthExpr.npc(-0.04)
      ticks  <- traverse(Vector(x0, x1))(x =>
        LengthExpr
          .native(x)
          .flatMap(nx =>
            Grob.text(
              f"$x%.0f",
              Point(nx, below),
              Anchor(HJust.Center, VJust.Top),
              gp = tickGp,
              viewport = Some(unclipped)
            )
          )
      )
    yield (
      Vector(
        background,
        Grob.group(Vector(grid, markGrob), viewport = Some(viewport)),
        Grob.group(Vector(frame), viewport = Some(unclipped)),
        title
      ) ++ ticks,
      viewport
    )

  private def name(value: String): Either[GraphicsError, GraphicsName] =
    GraphicsName(value, "reference mark")

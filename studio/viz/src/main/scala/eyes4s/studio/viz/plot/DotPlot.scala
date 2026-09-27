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

import eyes4s.studio.app.plot.{ColumnId, PlotSource, PlotText, PlotTextId, PlotValue}
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
  Point,
  PointShape,
  Rgba,
  Scene,
  Size,
  StrokeUnit,
  VJust,
  Viewport
}

/** The plot host's demonstration plot (ticket S4.5a): one neutral dot per row
  * at its `x` and `y` values, in a framed panel titled by the two columns'
  * headers.
  *
  * It is the smallest [[PlotBuilder]]: the host, TableTwin and parity tests
  * run on it before the real plots (S4.5b–e) exist. It draws no axis
  * numbers, so every number it shows is a row's, and a row with a missing
  * `x` or `y` is returned as [[Unplotted]]. The data domain spans the drawn
  * values with a margin, so no dot is clipped.
  */
final case class DotPlot(x: ColumnId, y: ColumnId, title: String) extends PlotBuilder:

  def kind: String = "dot-plot"

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot] =
    for
      xi <- DotPlot.numericColumn(kind, source, x)
      yi <- DotPlot.numericColumn(kind, source, y)
      split = source.rows.indices.toVector.map { i =>
        val row = source.rows(i)
        (row.values(xi), row.values(yi)) match
          case (PlotValue.Number(vx), PlotValue.Number(vy)) => Right((i, DataPoint(vx, vy)))
          case (PlotValue.Number(_), _)                     =>
            Left(Unplotted(row.ref, i, UnplottedReason.MissingValue(y)))
          case _ => Left(Unplotted(row.ref, i, UnplottedReason.MissingValue(x)))
      }
      drawn     = split.collect { case Right(d) => d }
      unplotted = split.collect { case Left(u) => u }
      id <- SceneId(s"studio.plot.$kind.${theme.toString.toLowerCase}").left
        .map(PlotBuildError.Scene(kind, _))
      built <- DotPlot
        .scene(theme, source, drawn, x, y)
        .left
        .map(PlotBuildError.Graphics(kind, "the dot plot", _))
      (grobs, viewport, marks) = built
      panel     <- DataPanel(id, viewport).left.map(PlotBuildError.Scene(kind, _))
      plotScene <- PlotScene(id, Scene(grobs), panel).left.map(PlotBuildError.Scene(kind, _))
      plot      <- BuiltPlot(
        kind,
        source,
        plotScene,
        title,
        PlotText(PlotTextId.PlotKeys, title),
        marks,
        unplotted
      )
    yield plot

object DotPlot:

  /** A dot's radius and the width of its surface-coloured edge, in logical pixels. */
  val RadiusPx: Double = 4.5
  val EdgePx: Double   = 1.0

  /** The fraction of each data range added on either side of it. */
  val Margin: Double = 0.08

  /** The prefix of each dot's grob name; the suffix is its row. */
  val MarkPrefix: String = "dot-"

  private val PointsPerPixel = 72.0 / 96.0

  // A length in logical pixels as Intaglio points (96 logical pixels per inch).
  private def px(value: Double): Double = value * PointsPerPixel

  private def numericColumn(
      kind: String,
      source: PlotSource,
      id: ColumnId
  ): Either[PlotBuildError, Int] =
    source.indexOf(id) match
      case None => Left(PlotBuildError.MissingColumn(kind, id))
      case Some(i) if !source.columns(i).format.numeric =>
        Left(PlotBuildError.NotNumeric(kind, id))
      case Some(i) => Right(i)

  /** The drawn range of `values`: their span with [[Margin]] either side, or
    * a unit range around a single value.
    */
  def domain(values: Vector[Double]): (Double, Double) =
    if values.isEmpty then (0.0, 1.0)
    else
      val lo = values.min
      val hi = values.max
      if hi > lo then
        val pad = (hi - lo) * Margin
        (lo - pad, hi + pad)
      else (lo - 1.0, hi + 1.0)

  private def scene(
      theme: Theme,
      source: PlotSource,
      drawn: Vector[(Int, DataPoint)],
      x: ColumnId,
      y: ColumnId
  ): Either[GraphicsError, (Vector[Grob], Viewport, Vector[PlotMark])] =
    def colour(token: ThemedToken): Rgba = IntaglioColours.themed(theme, token)
    def header(id: ColumnId): String     =
      source.indexOf(id).fold(id.value)(source.columns(_).header)
    def traverse[A, B](as: Vector[A])(f: A => Either[GraphicsError, B]) =
      as.foldLeft[Either[GraphicsError, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
        acc.flatMap(bs => f(a).map(bs :+ _))
      }
    val (x0, x1) = domain(drawn.map(_._2.x))
    val (y0, y1) = domain(drawn.map(_._2.y))
    for
      xScale   <- Interval(x0, x1)
      yScale   <- Interval(y0, y1)
      origin   <- Point.npc(0.08, 0.1)
      size     <- Size.npc(0.88, 0.78)
      viewport <- Viewport.checked(origin, size, xScale, yScale, Clip.On)
      frameVp  <- Viewport.checked(origin, size, xScale, yScale, Clip.Off)
      whole    <- Size.npc(1.0, 1.0)
      centre   <- Point.npc(0.5, 0.5)
      surface  <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Surface)))
      background <- Grob.rect(centre, whole, gp = surface)
      frameGp    <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Hairline)),
        fill = None,
        lineWidth = px(1.0),
        lineWidthUnit = StrokeUnit.Point
      )
      frameName <- GraphicsName("dot-plot-frame", "dot plot")
      frame     <- Grob.rect(centre, whole, gp = frameGp, name = Some(frameName))
      dotGp     <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Surface)),
        fill = Some(colour(ThemedToken.Ink2)),
        lineWidth = px(EdgePx),
        lineWidthUnit = StrokeUnit.Point
      )
      dotSize <- ExtentExpr.points(px(RadiusPx))
      marked  <- traverse(drawn.zipWithIndex) { case ((row, at), order) =>
        for
          n    <- GraphicsName(s"$MarkPrefix$row", "dot plot mark")
          p    <- Point.native(at.x, at.y)
          grob <- Grob.pointBatch(
            Vector(p),
            sizes = BatchColumn.Constant(dotSize),
            shapes = BatchColumn.Constant(PointShape.Circle),
            graphicParams = BatchColumn.Constant(dotGp),
            name = Some(n)
          )
        yield (grob, PlotMark(source.rows(row).ref, row, at, RadiusPx + EdgePx / 2.0, order, n))
      }
      labelSize <- Length.points(px(TypeSize.T11.px.toDouble))
      labelGp   <- GraphicParams.checked(
        stroke = None,
        fill = Some(colour(ThemedToken.Ink3)),
        fontFamily = Some(FontFace.SansRegular.javaFxFamily),
        fontSize = labelSize
      )
      yAt    <- Point.npc(0.08, 0.95)
      yLabel <- Grob.text(header(y), yAt, Anchor(HJust.Left, VJust.Top), gp = labelGp)
      xAt    <- Point.npc(0.96, 0.02)
      xLabel <- Grob.text(header(x), xAt, Anchor(HJust.Right, VJust.Bottom), gp = labelGp)
    yield (
      Vector(
        background,
        Grob.group(marked.map(_._1), viewport = Some(viewport)),
        Grob.group(Vector(frame), viewport = Some(frameVp)),
        yLabel,
        xLabel
      ),
      viewport,
      marked.map(_._2)
    )

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
import eyes4s.studio.core.selection.StudioRef
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
  LineType,
  Point,
  PointShape,
  Rgba,
  Scene,
  Size,
  StrokeUnit,
  VJust,
  Viewport
}

/** The plot host's demonstration plot (tickets S4.5a and S4.5x): one
  * neutral dot per row at its `x` and `y` values, in a framed panel titled by
  * the two columns' headers.
  *
  * It is the smallest [[PlotBuilder]]: the host, TableTwin and parity tests
  * run on it before the real plots (S4.5b–e) exist. It draws no axis
  * numbers, so every number it shows is a row's. A row with no `x` is
  * [[Unplotted]]; a row with an `x` but no `y` is too, unless
  * `missing` is [[DotPlot.MissingY.Placeholder]], when it is drawn as a
  * positionless dashed empty ring on a band below the panel's data. Rows at
  * one position are stacked dots, or one mark for all of them when
  * `coincident` is [[DotPlot.Coincident.Merge]]. The data domain spans the
  * drawn values with a margin, so no dot is clipped.
  */
final case class DotPlot(
    x: ColumnId,
    y: ColumnId,
    title: String,
    missing: DotPlot.MissingY = DotPlot.MissingY.SetAside,
    coincident: DotPlot.Coincident = DotPlot.Coincident.Stack
) extends PlotBuilder:

  def kind: String = "dot-plot"

  /** Both axes linear: a dot sits at its row's `x` and `y`. */
  def encoding: PositionEncoding =
    PositionEncoding(Axis.Numeric(x, AxisScale.Linear), Axis.Numeric(y, AxisScale.Linear))

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot] =
    for
      xi <- DotPlot.numericColumn(kind, source, x)
      yi <- DotPlot.numericColumn(kind, source, y)
      split = source.rows.indices.toVector.map { i =>
        val row = source.rows(i)
        (row.values(xi), row.values(yi)) match
          case (PlotValue.Number(vx), PlotValue.Number(vy)) =>
            Right(DotPlot.Dot(i, row.ref, vx, Some(vy)))
          case (PlotValue.Number(vx), _) if missing == DotPlot.MissingY.Placeholder =>
            Right(DotPlot.Dot(i, row.ref, vx, None))
          case (PlotValue.Number(_), _) =>
            Left(Unplotted(row.ref, i, NoPosition.MissingValue(y)))
          case _ => Left(Unplotted(row.ref, i, NoPosition.MissingValue(x)))
      }
      drawn     = split.collect { case Right(d) => d }
      unplotted = split.collect { case Left(u) => u }
      id <- SceneId(s"studio.plot.$kind.${theme.toString.toLowerCase}").left
        .map(PlotBuildError.Scene(kind, _))
      built <- DotPlot
        .scene(kind, theme, source, drawn, x, y, coincident)
        .left
        .map(PlotBuildError.Graphics(kind, "the dot plot", _))
      (grobs, viewport, marks) = built
      checked   <- marks
      panel     <- DataPanel(id, viewport).left.map(PlotBuildError.Scene(kind, _))
      plotScene <- PlotScene(id, Scene(grobs), panel).left.map(PlotBuildError.Scene(kind, _))
      plot      <- BuiltPlot(
        kind,
        source,
        plotScene,
        title,
        PlotText(PlotTextId.PlotKeys, title),
        encoding,
        checked,
        unplotted
      )
    yield plot

object DotPlot:

  /** What a dot plot does with a row that has an `x` but no `y`. */
  enum MissingY derives CanEqual:
    /** Returns it as [[Unplotted]]. */
    case SetAside

    /** Draws it as a positionless dashed empty ring at its `x`, on a band
      * below the data: missing is drawn differently from any value.
      */
    case Placeholder

  /** What a dot plot does with rows whose values are equal. */
  enum Coincident derives CanEqual:
    /** Draws a dot for each, stacked in row order. */
    case Stack

    /** Draws one mark for all of them, placed at their shared values. */
    case Merge

  // A row to draw: at (x, y), or at x on the placeholder band when y is None.
  private final case class Dot(row: Int, ref: StudioRef, x: Double, y: Option[Double])

  /** A dot's radius and the width of its surface-coloured edge, in logical pixels. */
  val RadiusPx: Double = 4.5
  val EdgePx: Double   = 1.0

  /** The fraction of each data range added on either side of it. */
  val Margin: Double = 0.08

  /** The prefix of each dot's grob name; the suffix is its (first) row. */
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

  // The scene's grobs, its data viewport and its marks, which are checked
  // only once every grob is drawn.
  private def scene(
      kind: String,
      theme: Theme,
      source: PlotSource,
      drawn: Vector[Dot],
      x: ColumnId,
      y: ColumnId,
      coincident: Coincident
  ): Either[GraphicsError, (Vector[Grob], Viewport, Either[PlotBuildError, Vector[PlotMark]])] =
    def colour(token: ThemedToken): Rgba = IntaglioColours.themed(theme, token)
    def header(id: ColumnId): String     =
      source.indexOf(id).flatMap(source.columns.lift).fold(id.value)(_.header)
    def traverse[A, B](as: Vector[A])(f: A => Either[GraphicsError, B]) =
      as.foldLeft[Either[GraphicsError, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
        acc.flatMap(bs => f(a).map(bs :+ _))
      }
    val (x0, x1)     = domain(drawn.map(_.x))
    val (ys0, y1)    = domain(drawn.flatMap(_.y))
    val placeholders = drawn.exists(_.y.isEmpty)
    // The placeholder band sits one margin below the drawn values.
    val band = ys0 - (if placeholders then (y1 - ys0) * Margin else 0.0)
    val y0   = if placeholders then band - (y1 - ys0) * Margin else ys0
    // Each mark's first row and its others, in order of first rows.
    val groups: Vector[(Dot, Vector[Dot])] = coincident match
      case Coincident.Stack => drawn.map((_, Vector.empty))
      case Coincident.Merge =>
        drawn.foldLeft(Vector.empty[(Dot, Vector[Dot])]) { (gs, d) =>
          gs.indexWhere((f, _) => d.y.isDefined && f.x == d.x && f.y == d.y) match
            case -1 => gs :+ (d, Vector.empty)
            case i  => gs.updated(i, (gs(i)._1, gs(i)._2 :+ d))
        }
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
      emptyGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Ink3)),
        fill = None,
        lineWidth = px(EdgePx),
        lineType = LineType.Dashed,
        lineWidthUnit = StrokeUnit.Point
      )
      dotSize <- ExtentExpr.points(px(RadiusPx))
      reach = RadiusPx + EdgePx / 2.0
      marked <- traverse(groups.zipWithIndex) { case ((first, others), order) =>
        val at = DataPoint(first.x, first.y.getOrElse(band))
        for
          n    <- GraphicsName(s"$MarkPrefix${first.row}", "dot plot mark")
          p    <- Point.native(at.x, at.y)
          grob <- Grob.pointBatch(
            Vector(p),
            sizes = BatchColumn.Constant(dotSize),
            shapes = BatchColumn.Constant(PointShape.Circle),
            graphicParams = BatchColumn.Constant(if first.y.isDefined then dotGp else emptyGp),
            name = Some(n)
          )
        yield
          val rows = (first +: others).map(d =>
            MarkedRow(
              d.ref,
              d.row,
              d.y.fold(RowMarking.Positionless(NoPosition.MissingValue(y)))(vy =>
                RowMarking.Placed(DataPoint(d.x, vy))
              )
            )
          )
          (grob, PlotMark.of(kind, rows, at, reach, order, n))
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
      marked.foldLeft[Either[PlotBuildError, Vector[PlotMark]]](Right(Vector.empty)) {
        case (acc, (_, mark)) => acc.flatMap(ms => mark.map(ms :+ _))
      }
    )

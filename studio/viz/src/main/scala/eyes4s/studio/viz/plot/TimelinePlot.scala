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

import eyes4s.studio.app.plot.{
  ColumnId,
  HalfOpenSpan,
  PlotSource,
  PlotText,
  PlotTextId,
  TimelineColumns
}
import eyes4s.studio.app.text.{TimelineText, TimelineTextId}
import eyes4s.studio.app.tokens.{FontFace, Theme, ThemedToken, TypeSize}
import intaglio.{
  Anchor,
  Clip,
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
  Size,
  StrokeUnit,
  VJust,
  Viewport
}

/** The timeline plot (ticket S4.5e; the Explore board's timeline): one bar
  * per fixation, from its onset to its end on the time axis and as tall as
  * its duration, over a baseline; a brushed span, shaded behind the bars;
  * and a playhead.
  *
  * Its source is [[eyes4s.studio.app.plot.Timeline.source]]. Each bar is a
  * mark of one placed row, at its onset by its duration ([[Axis.Numeric]]
  * of both, linear), and is drawn across to its onset plus its duration. A
  * row without both is [[Unplotted]]. The brush and the playhead are the
  * view's state, not rows: `brush` is the span a drag selected (picking
  * the fixations whose intervals overlap it,
  * [[eyes4s.studio.app.plot.TimelineColumns.brushRule]]) and `playheadMs` the playback
  * time. The plot writes no numbers; every number is a row's in the table.
  */
final case class TimelinePlot(
    columns: TimelineColumns,
    brush: Option[HalfOpenSpan] = None,
    playheadMs: Option[Double] = None,
    trialEndMs: Option[Double] = None
) extends PlotBuilder:

  def kind: String = "timeline"

  /** Onset by duration, both linear. */
  def encoding: PositionEncoding =
    PositionEncoding(
      Axis.Numeric(columns.onset, AxisScale.Linear),
      Axis.Numeric(columns.duration, AxisScale.Linear)
    )

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot] =
    import TimelinePlot.*
    def numeric(id: ColumnId) =
      for
        i <- source.indexOf(id).toRight(PlotBuildError.MissingColumn(kind, id))
        _ <- Either.cond(
          source.columns(i).format.numeric,
          (),
          PlotBuildError.NotNumeric(kind, id)
        )
      yield i
    val rows = source.rows.indices.toVector
    for
      _ <- numeric(columns.onset)
      _ <- numeric(columns.duration)
      placed    = rows.flatMap(i => encoding.place(source, i).map(i -> _))
      unplotted = rows.filterNot(i => placed.exists(_._1 == i)).map { i =>
        val missing =
          if source.number(i, columns.onset).isEmpty then columns.onset else columns.duration
        Unplotted(source.rows(i).ref, i, NoPosition.MissingValue(missing))
      }
      id <- SceneId(s"studio.plot.$kind.${theme.toString.toLowerCase}").left
        .map(PlotBuildError.Scene(kind, _))
      built <- scene(theme, placed).left
        .map(PlotBuildError.Graphics(kind, "the timeline", _))
      (grobs, viewport, names) = built
      marks = placed.zip(names).zipWithIndex.map { case (((i, at), name), order) =>
        PlotMark.placed(source.rows(i).ref, i, at, ReachPx, order, name)
      }
      panel     <- DataPanel(id, viewport).left.map(PlotBuildError.Scene(kind, _))
      plotScene <- PlotScene(id, Scene(grobs), panel).left.map(PlotBuildError.Scene(kind, _))
      plot      <- BuiltPlot(
        kind,
        source,
        plotScene,
        TimelineText(TimelineTextId.Title),
        PlotText(
          PlotTextId.PlotKeys,
          TimelineText(TimelineTextId.Summary, source.caption, placed.size.toString)
        ),
        encoding,
        marks,
        unplotted
      )
    yield plot

  // The grobs, the data viewport and each bar's grob name, in row order.
  private def scene(
      theme: Theme,
      placed: Vector[(Int, DataPoint)]
  ): Either[GraphicsError, (Vector[Grob], Viewport, Vector[GraphicsName])] =
    import TimelinePlot.*
    def colour(token: ThemedToken): Rgba = IntaglioColours.themed(theme, token)
    def traverse[A, B](as: Vector[A])(f: A => Either[GraphicsError, B]) =
      as.foldLeft[Either[GraphicsError, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
        acc.flatMap(bs => f(a).map(bs :+ _))
      }
    // A rectangle from data x0 to x1 and y0 to y1.
    def box(
        x0: Double,
        x1: Double,
        y0: Double,
        y1: Double,
        gp: GraphicParams,
        name: Option[GraphicsName]
    ) =
      for
        cx <- LengthExpr.native((x0 + x1) / 2.0)
        cy <- LengthExpr.native((y0 + y1) / 2.0)
        w  <- intaglio.ExtentExpr.native(x1 - x0)
        h  <- intaglio.ExtentExpr.native(y1 - y0)
        g  <- Grob.rect(Point(cx, cy), Size.fromExtents(w, h), gp = gp, name = name)
      yield g
    val ends     = placed.map((_, at) => at.x + at.y)
    val (x0, x1) = timeDomain(
      placed.map(_._2.x) ++ ends ++ playheadMs.toVector ++ trialEndMs.toVector
    )
    val y1 = (placed.map(_._2.y) :+ 1.0).max * (1.0 + Headroom)
    for
      xScale   <- Interval(x0, x1)
      yScale   <- Interval(0.0, y1)
      origin   <- Point.npc(0.02, 0.2)
      size     <- Size.npc(0.96, 0.74)
      viewport <- Viewport.checked(origin, size, xScale, yScale, Clip.On)
      frameVp  <- Viewport.checked(origin, size, xScale, yScale, Clip.Off)
      whole    <- Size.npc(1.0, 1.0)
      centre   <- Point.npc(0.5, 0.5)
      surface  <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Surface)))
      background <- Grob.rect(centre, whole, gp = surface)
      brushGp    <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Ink3)),
        fill = Some(colour(ThemedToken.Surface3)),
        lineWidth = px(1.0),
        lineWidthUnit = StrokeUnit.Point
      )
      shaded <- brush.fold[Either[GraphicsError, Vector[Grob]]](Right(Vector.empty))(b =>
        box(b.from, b.until, 0.0, y1, brushGp, None).map(Vector(_))
      )
      left   <- LengthExpr.npc(0.0)
      right  <- LengthExpr.npc(1.0)
      zero   <- LengthExpr.native(0.0)
      baseGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Hairline)),
        fill = None,
        lineWidth = px(1.0),
        lineWidthUnit = StrokeUnit.Point
      )
      baseline <- Grob.segments(Vector((Point(left, zero), Point(right, zero))), gp = baseGp)
      barGp    <- GraphicParams.checked(stroke = None, fill = Some(colour(ThemedToken.Ink3)))
      bars     <- traverse(placed) { (i, at) =>
        for
          name <- GraphicsName(s"$MarkPrefix$i", "timeline bar")
          grob <- box(at.x, at.x + at.y, 0.0, at.y, barGp, Some(name))
        yield (grob, name)
      }
      playGp <- GraphicParams.checked(
        stroke = Some(colour(ThemedToken.Ink)),
        fill = None,
        lineWidth = px(PlayheadPx),
        lineWidthUnit = StrokeUnit.Point
      )
      top    <- LengthExpr.npc(1.0)
      bottom <- LengthExpr.npc(0.0)
      head   <- playheadMs.fold[Either[GraphicsError, Vector[Grob]]](Right(Vector.empty))(t =>
        LengthExpr
          .native(t)
          .flatMap(x => Grob.segments(Vector((Point(x, bottom), Point(x, top))), gp = playGp))
          .map(Vector(_))
      )
      labelSize <- Length.points(px(TypeSize.T11.px.toDouble))
      labelGp   <- GraphicParams.checked(
        stroke = None,
        fill = Some(colour(ThemedToken.Ink3)),
        fontFamily = Some(FontFace.SansRegular.javaFxFamily),
        fontSize = labelSize
      )
      titleAt <- Point.npc(0.98, 0.02)
      title   <- Grob.text(
        TimelineText(TimelineTextId.TimeAxis),
        titleAt,
        Anchor(HJust.Right, VJust.Bottom),
        gp = labelGp
      )
    yield (
      Vector(
        background,
        Grob.group(shaded :+ baseline, viewport = Some(frameVp)),
        Grob.group(bars.map(_._1), viewport = Some(viewport)),
        Grob.group(head, viewport = Some(frameVp)),
        title
      ),
      viewport,
      bars.map(_._2)
    )

object TimelinePlot:

  /** How far a bar's mark reaches from its anchor, its top-left corner. */
  val ReachPx: Double = 4.0

  /** The playhead's width. */
  val PlayheadPx: Double = 2.0

  /** The fraction of the tallest bar left above it. */
  val Headroom: Double = 0.1

  /** The fraction of the time range added on either side of it. */
  val Margin: Double = 0.02

  /** The prefix of each bar's grob name; the suffix is its row. */
  val MarkPrefix: String = "timeline-"

  private val PointsPerPixel = 72.0 / 96.0

  // A length in logical pixels as Intaglio points (96 logical pixels per inch).
  private def px(value: Double): Double = value * PointsPerPixel

  /** The time range drawn: zero, every onset and end and the playhead, with
    * [[Margin]] either side.
    */
  def timeDomain(times: Vector[Double]): (Double, Double) =
    val lo  = (times :+ 0.0).min
    val hi  = math.max((times :+ 0.0).max, lo + 1.0)
    val pad = (hi - lo) * Margin
    (lo - pad, hi + pad)

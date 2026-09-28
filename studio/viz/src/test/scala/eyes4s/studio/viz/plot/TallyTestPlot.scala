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

import eyes4s.studio.app.plot.{ColumnId, PlotSource, PlotValue}
import eyes4s.studio.app.tokens.{Theme, ThemedToken}
import intaglio.{
  BatchColumn,
  Clip,
  ExtentExpr,
  GraphicParams,
  GraphicsError,
  GraphicsName,
  Grob,
  Interval,
  Point,
  PointShape,
  Scene,
  Size,
  Viewport
}

/** A test-only builder with aggregate marks (ticket S4.5x), shaped like the
  * scale ladder's histogram fallback (S4.5b): rows of entities are binned by
  * `x` into bins `width` wide, and one mark per bin stands for its rows
  * ([[RowMarking.Represented]]) at the bin's centre and height. Rows with
  * aggregate refs (a group's mean) are drawn by their own mark at their `x`
  * and `y`. As on a log axis, an `x` below zero is off the scale.
  *
  * Its counts are computed here, which a studio builder must not do; it
  * exists to exercise the accounting, not as a plot.
  */
final case class TallyTestPlot(x: ColumnId, y: ColumnId, width: Double) extends PlotBuilder:

  def kind: String = "tally-test"

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot] =
    val rows                     = source.rows.zipWithIndex
    def num(i: Int, c: ColumnId) = source.number(i, c)
    val derived                  = rows.filter(_._1.ref.isAggregate)
    val entities                 = rows.filterNot(_._1.ref.isAggregate)
    val binned    = entities.flatMap((r, i) => num(i, x).filter(_ >= 0.0).map(v => (r, i, v)))
    val unplotted = entities.flatMap { (r, i) =>
      source.value(i, x) match
        case Some(PlotValue.Number(v)) if v < 0.0 =>
          Some(Unplotted(r.ref, i, NoPosition.OffScale(x, v)))
        case Some(PlotValue.Number(_)) => None
        case _                         => Some(Unplotted(r.ref, i, NoPosition.MissingValue(x)))
    } ++ derived.flatMap { (r, i) =>
      if num(i, x).isEmpty then Some(Unplotted(r.ref, i, NoPosition.MissingValue(x)))
      else if num(i, y).isEmpty then Some(Unplotted(r.ref, i, NoPosition.MissingValue(y)))
      else None
    }
    val bins = binned
      .groupBy((_, _, v) => math.floor(v / width).toLong)
      .toVector
      .sortBy(_._1)
      .map((k, members) =>
        (
          DataPoint((k + 0.5) * width, members.size.toDouble),
          members.map((r, i, _) => MarkedRow(r.ref, i, RowMarking.Represented))
        )
      )
    val means = derived.flatMap { (r, i) =>
      for
        vx <- num(i, x)
        vy <- num(i, y)
        at = DataPoint(vx, vy)
      yield (at, Vector(MarkedRow(r.ref, i, RowMarking.Placed(at))))
    }
    val marksAt = bins ++ means
    for
      id    <- SceneId(s"studio.plot.$kind").left.map(PlotBuildError.Scene(kind, _))
      drawn <- scene(theme, marksAt.map(_._1)).left
        .map(PlotBuildError.Graphics(kind, "the tally", _))
      (grobs, viewport, names) = drawn
      marks <- marksAt
        .zip(names)
        .zipWithIndex
        .foldLeft[Either[PlotBuildError, Vector[PlotMark]]](
          Right(Vector.empty)
        ) { case (acc, (((at, rs), n), order)) =>
          acc.flatMap(ms => PlotMark.of(kind, rs, at, 4.0, order, n).map(ms :+ _))
        }
      panel     <- DataPanel(id, viewport).left.map(PlotBuildError.Scene(kind, _))
      plotScene <- PlotScene(id, Scene(grobs), panel).left.map(PlotBuildError.Scene(kind, _))
      plot      <- BuiltPlot(kind, source, plotScene, "Tally", "Tally", marks, unplotted)
    yield plot

  private def scene(
      theme: Theme,
      at: Vector[DataPoint]
  ): Either[GraphicsError, (Vector[Grob], Viewport, Vector[GraphicsName])] =
    val (x0, x1) = DotPlot.domain(at.map(_.x))
    val (y0, y1) = DotPlot.domain(at.map(_.y))
    for
      xs       <- Interval(x0, x1)
      ys       <- Interval(y0, y1)
      origin   <- Point.npc(0.1, 0.1)
      size     <- Size.npc(0.8, 0.8)
      viewport <- Viewport.checked(origin, size, xs, ys, Clip.On)
      dotSize  <- ExtentExpr.points(3.0)
      filled   <- GraphicParams.checked(
        stroke = None,
        fill = Some(IntaglioColours.themed(theme, ThemedToken.Ink2))
      )
      marks <- at.zipWithIndex.foldLeft[Either[GraphicsError, Vector[(Grob, GraphicsName)]]](
        Right(Vector.empty)
      ) { case (acc, (p, i)) =>
        for
          gs   <- acc
          n    <- GraphicsName(s"tally-$i", "tally mark")
          pt   <- Point.native(p.x, p.y)
          grob <- Grob.pointBatch(
            Vector(pt),
            sizes = BatchColumn.Constant(dotSize),
            shapes = BatchColumn.Constant(PointShape.Square),
            graphicParams = BatchColumn.Constant(filled),
            name = Some(n)
          )
        yield gs :+ (grob, n)
      }
    yield (
      Vector(Grob.group(marks.map(_._1), viewport = Some(viewport))),
      viewport,
      marks.map(_._2)
    )

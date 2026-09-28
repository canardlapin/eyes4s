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

import eyes4s.studio.app.plot.PlotSource
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

/** A test-only builder that places every row where `encoding` puts its
  * cells (ticket S4.5x), so the parity properties run on log and category
  * axes. A row it cannot place is [[Unplotted]]: off the scale when a log
  * axis has a number at or below zero, else missing.
  */
final case class EncodedTestPlot(encoding: PositionEncoding) extends PlotBuilder:

  def kind: String = "encoded-test"

  def build(source: PlotSource, theme: Theme): Either[PlotBuildError, BuiltPlot] =
    def reason(row: Int): NoPosition =
      Vector(encoding.x, encoding.y)
        .find(Axis.position(_, source, row).isEmpty)
        .map {
          case Axis.Numeric(c, AxisScale.Log10) if source.number(row, c).isDefined =>
            NoPosition.OffScale(c, source.number(row, c).getOrElse(0.0))
          case axis => NoPosition.MissingValue(Axis.columnOf(axis))
        }
        .getOrElse(NoPosition.MissingValue(Axis.columnOf(encoding.x)))
    val split = source.rows.zipWithIndex.map { (r, i) =>
      encoding
        .place(source, i)
        .toRight(Unplotted(r.ref, i, reason(i)))
        .map(at => (at, Vector(MarkedRow(r.ref, i, RowMarking.Placed(at)))))
    }
    TestPlots.assemble(
      kind,
      source,
      theme,
      encoding,
      split.collect { case Right(m) => m },
      split.collect { case Left(u) => u }
    )

/** The scene of the test-only builders: one filled square per mark. */
object TestPlots:

  /** A built plot of one mark per entry of `marks`, drawn at its point. */
  def assemble(
      kind: String,
      source: PlotSource,
      theme: Theme,
      encoding: PositionEncoding,
      marks: Vector[(DataPoint, Vector[MarkedRow])],
      unplotted: Vector[Unplotted]
  ): Either[PlotBuildError, BuiltPlot] =
    for
      id    <- SceneId(s"studio.plot.$kind").left.map(PlotBuildError.Scene(kind, _))
      drawn <- scene(kind, theme, marks.map(_._1)).left
        .map(PlotBuildError.Graphics(kind, "the test plot", _))
      (grobs, viewport, names) = drawn
      built <- marks
        .zip(names)
        .zipWithIndex
        .foldLeft[Either[PlotBuildError, Vector[PlotMark]]](Right(Vector.empty)) {
          case (acc, (((at, rs), n), order)) =>
            acc.flatMap(ms => PlotMark.of(kind, rs, at, 4.0, order, n).map(ms :+ _))
        }
      panel     <- DataPanel(id, viewport).left.map(PlotBuildError.Scene(kind, _))
      plotScene <- PlotScene(id, Scene(grobs), panel).left.map(PlotBuildError.Scene(kind, _))
      plot      <- BuiltPlot(kind, source, plotScene, kind, kind, encoding, built, unplotted)
    yield plot

  private def scene(
      kind: String,
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
          n    <- GraphicsName(s"$kind-$i", "test mark")
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

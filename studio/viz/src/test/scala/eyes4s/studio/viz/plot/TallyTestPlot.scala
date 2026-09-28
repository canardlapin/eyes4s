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
import eyes4s.studio.app.tokens.Theme

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
    TestPlots.assemble(
      kind,
      source,
      theme,
      PositionEncoding(Axis.Numeric(x, AxisScale.Linear), Axis.Numeric(y, AxisScale.Linear)),
      bins ++ means,
      unplotted
    )

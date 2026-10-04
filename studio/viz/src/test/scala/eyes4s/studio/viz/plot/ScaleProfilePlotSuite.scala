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

import eyes4s.studio.app.plot.*
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.selection.StudioRef
import intaglio.{DashPattern, Grob, LineType}
import intaglio.interaction.NamedPicking
import munit.FunSuite

/** The scale profile builder (ticket S4.5d): the log-spaced σ axis, one
  * faint line per participant, bold group means, and missing or off-scale
  * values set aside or breaking a line. The generic plot/table parity
  * properties run on it in [[PlotTableParitySuite]].
  */
class ScaleProfilePlotSuite extends FunSuite:
  import ProfileSamples.*

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def built(p: ScaleProfile): BuiltPlot =
    right(ScaleProfilePlot(columns).build(source(p), Theme.Light))

  private def targetsOn(plot: BuiltPlot): PlotTargets =
    val surface   = right(PlotSurface(400, 330, 1.0))
    val transform = right(PlotTransform.resolve(plot.plot, surface))
    val picking   = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
    right(PlotTargets.resolve(plot, transform, picking))

  private def series(p: ScaleProfile, name: String): ProfileSeries =
    (p.groups ++ p.participants).find(_.name == name).getOrElse(fail(name))

  private def mark(plot: BuiltPlot, ref: StudioRef): PlotMark =
    right(plot.markOf(ref).toRight(s"no mark for $ref"))

  test("σ is placed on a log axis: scales that double are evenly spaced") {
    val plot = built(board)
    assertEquals(
      plot.encoding,
      PositionEncoding(
        Axis.Numeric(columns.sigma, AxisScale.Log10),
        Axis.Numeric(columns.d, AxisScale.Linear)
      )
    )
    val dots = series(board, "Remembered").points.map(p => mark(plot, p.ref).at)
    assertEquals(dots.map(_.x), protocol.map(math.log10))
    assertEquals(dots.map(_.y), Vector(0.15, 0.24, 0.30, 0.19))
    // On the surface, each doubling is the same number of pixels.
    val t  = targetsOn(plot)
    val xs = series(board, "Remembered").points.map(p =>
      right(t.target(p.ref).toRight(p.ref)).anchor.x
    )
    val steps = xs.zip(xs.tail).map((a, b) => b - a)
    assert(steps.head > 10.0, steps)
    steps.foreach(s => assertEqualsDouble(s, steps.head, 1e-6))
  }

  test("each participant is one faint line of its rows; each group mean one dot") {
    val plot = built(board)
    val p17  = series(board, "P17")
    val line = mark(plot, p17.points.head.ref)
    assertEquals(line.refs, p17.points.map(_.ref))
    assert(line.rows.forall(_.marking.isInstanceOf[RowMarking.Placed]))
    // A missing mean is a positionless row of its line, which it breaks.
    val p05 = mark(plot, series(board, "P05").points.head.ref)
    assertEquals(
      p05.rows.map(_.marking.isInstanceOf[RowMarking.Positionless]),
      Vector(false, true, false, false)
    )
    // A participant with no mean at any scale is set aside, row by row.
    val p99 = series(board, "P99").points.map(_.ref)
    assertEquals(plot.unplotted.map(_.ref), p99)
    assert(plot.unplotted.forall(_.reason == NoPosition.MissingValue(columns.d)))
    // Lines first, then each group's dots in scale order.
    assertEquals(plot.marks.size, 2 + 8)
    assertEquals(plot.marks.take(2).map(_.rows.size), Vector(4, 4))
    assert(plot.marks.drop(2).forall(_.rows.size == 1))
  }

  test("the first group is solid, the others dashed, and lines are faint") {
    val plot   = built(board)
    val groups =
      plot.plot.scene.grobs.flatMap(g => g +: g.children).collect { case l: Grob.Lines => l }
    val bold = groups.filter(_.name.isEmpty)
    assertEquals(
      bold.map(_.gp.lineType),
      Vector(LineType.Solid, LineType.Custom(DashPattern.unsafe(ScaleProfilePlot.MeanDash*)))
    )
    val faint = groups.filter(_.name.isDefined)
    assertEquals(faint.size, 2)
    assert(faint.forall(_.gp.alpha == ScaleProfilePlot.LineAlpha))
    assertEquals(bold.map(_.points.size), Vector(4, 4))
  }

  test("readouts carry each series' n; each grand mean is written and each scale labelled") {
    val plot = built(board)
    val rem  = series(board, "Remembered").points(2).ref
    assertEquals(
      plot.readout(mark(plot, rem)),
      Some("Mean of Remembered, Scale 2°, σ (°) 2.00, D +0.30, n 24 participants")
    )
    val fg = series(board, "Forgotten").points(2).ref
    assert(plot.readout(mark(plot, fg)).exists(_.endsWith("n 23 participants")))
    val line =
      plot.readout(mark(plot, series(board, "P17").points.head.ref)).getOrElse(fail("P17"))
    assert(line.startsWith("4 rows: "), line)
    assert(line.contains("n 19 queries"), line)
    val written = plot.plot.scene.grobs.flatMap(g => g +: g.children).collect {
      case t: Grob.Text => t.label
    }
    Vector("0.5°", "1°", "2°", "4°", "+0.30", "+0.15").foreach(w =>
      assert(written.contains(w), w)
    )
  }

  test("σ at or below zero is off the log scale; a row of another kind is refused") {
    val base = source(board)
    val ref  = series(board, "Remembered").points.head.ref
    val i    = right(base.rowOf(ref).toRight(ref))
    val rows = base.rows.updated(
      i,
      base.rows(i).copy(values = base.rows(i).values.updated(2, PlotValue.Number(0.0)))
    )
    val plot = right(
      ScaleProfilePlot(columns)
        .build(right(PlotSource(base.caption, base.columns, rows)), Theme.Light)
    )
    assert(
      plot.unplotted.contains(Unplotted(ref, i, NoPosition.OffScale(columns.sigma, 0.0))),
      plot.unplotted
    )
    val odd     = StudioRef.Participant("P01")
    val withOdd = right(
      PlotSource(base.caption, base.columns, base.rows.updated(0, base.rows(0).copy(ref = odd)))
    )
    assertEquals(
      ScaleProfilePlot(columns).build(withOdd, Theme.Light).left.toOption,
      Some(PlotBuildError.UnexpectedRow("scale-profile", odd))
    )
  }

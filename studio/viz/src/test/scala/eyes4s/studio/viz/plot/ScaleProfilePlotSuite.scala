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

  test("served report refs keep grouped and overall profile series") {
    import eyes4s.studio.core.backend.ReportRole
    import eyes4s.studio.core.selection.ReportGroup
    def served(point: ProfilePoint): ProfilePoint = point.ref match
      case StudioRef.GroupCell(r, id, scale, group) =>
        point.copy(ref =
          StudioRef.ReportCell(r, id, scale, ReportGroup.Level(group), ReportRole.Difference)
        )
      case StudioRef.ParticipantSummary(r, id, scale, group, person) =>
        point.copy(ref =
          StudioRef.ReportParticipant(
            r,
            id,
            scale,
            group.fold(ReportGroup.Whole)(ReportGroup.Level(_)),
            ReportRole.Difference,
            person
          )
        )
      case other => fail(s"Unexpected fixture ref: $other")
    val report = board.copy(
      groups = board.groups.map(g => g.copy(points = g.points.map(served))),
      participants = board.participants.map(p => p.copy(points = p.points.map(served)))
    )
    val actual = built(report)
    val legacy = built(board)
    assertEquals(actual.marks.size, legacy.marks.size)
    assertEquals(actual.marks.map(_.at), legacy.marks.map(_.at))
    assertEquals(actual.marks.flatMap(actual.readout), legacy.marks.flatMap(legacy.readout))
    assertEquals(
      (actual.marks.flatMap(_.refs) ++ actual.unplotted.map(_.ref)).toSet,
      (report.groups ++ report.participants).flatMap(_.points.map(_.ref)).toSet
    )
  }

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

  // Every grob of the scene, depth first.
  private def grobs(plot: BuiltPlot): Vector[Grob] =
    def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
    plot.plot.scene.grobs.flatMap(all)

  // The bold group lines: every line that is not faint.
  private def bold(plot: BuiltPlot): Vector[Grob.Lines] =
    grobs(plot).collect { case l: Grob.Lines if l.gp.alpha != ScaleProfilePlot.LineAlpha => l }

  // The pieces drawn under a mark's name.
  private def pieces(plot: BuiltPlot, m: PlotMark): Vector[Grob] =
    grobs(plot)
      .collectFirst { case g: Grob.Group if g.name.contains(m.name) => g.children }
      .getOrElse(fail(s"no group for ${m.ref}"))

  test("the first group is solid, the others dashed, and lines are faint") {
    val plot = built(board)
    assertEquals(
      bold(plot).map(_.gp.lineType),
      Vector(LineType.Solid, LineType.Custom(DashPattern.unsafe(ScaleProfilePlot.MeanDash*)))
    )
    assertEquals(bold(plot).map(_.points.size), Vector(4, 4))
    val faint = grobs(plot).collect {
      case l: Grob.Lines if l.gp.alpha == ScaleProfilePlot.LineAlpha => l
    }
    assert(faint.nonEmpty && faint.forall(_.name.isEmpty), faint)
    // The bold means are drawn after (on top of) every participant's line.
    val order     = grobs(plot)
    val lastFaint = order.lastIndexWhere(g => faint.exists(_ eq g))
    assert(order.indexWhere(g => bold(plot).exists(_ eq g)) > lastFaint)
  }

  test("a missing mean breaks a participant's line and a group's line; it is never bridged") {
    assertEquals(
      ScaleProfilePlot.runs(
        Vector(Some(DataPoint(0, 1)), None, Some(DataPoint(2, 3)), Some(DataPoint(3, 4)), None)
      ),
      Vector(Vector(DataPoint(0, 1)), Vector(DataPoint(2, 3), DataPoint(3, 4)))
    )
    val plot = built(board)
    // P05 has no mean at 1°: a lone point at 0.5°, then a line from 2° to 4°.
    val p05 = pieces(plot, mark(plot, series(board, "P05").points.head.ref))
    assertEquals(
      p05.map {
        case l: Grob.Lines      => l.points.size
        case b: Grob.PointBatch => -b.points.size
        case other              => fail(s"$other")
      },
      Vector(-1, 2)
    )
    assert(p05.forall(_.name.isEmpty), p05)
    // P17 is whole: one line through its four points.
    assertEquals(
      pieces(plot, mark(plot, series(board, "P17").points.head.ref)).collect {
        case l: Grob.Lines =>
          l.points.size
      },
      Vector(4)
    )
    // A group without a mean at 1°: its 0.5° dot stands alone, and its line
    // runs from 2° to 4°.
    val gappy = profile(
      protocol,
      Vector(
        ("Remembered", 24, Vector(Some(0.15), None, Some(0.30), Some(0.19))),
        ("Forgotten", 23, Vector(0.07, 0.12, 0.15, 0.09).map(Some(_)))
      ),
      Vector.empty
    )
    val g = built(gappy)
    assertEquals(bold(g).map(_.points.size), Vector(2, 4))
    assertEquals(g.marks.size, 7)
  }

  test("every declared σ is labelled, even where no value is drawn") {
    val none = profile(
      protocol,
      Vector(("Remembered", 24, Vector(Some(0.15), Some(0.24), Some(0.30), None))),
      Vector(("P01", 10, Vector(Some(0.1), Some(0.2), Some(0.2), None)))
    )
    val plot    = built(none)
    val written = grobs(plot).collect { case t: Grob.Text => t.label }
    Vector("0.5°", "1°", "2°", "4°").foreach(l => assert(written.contains(l), l))
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

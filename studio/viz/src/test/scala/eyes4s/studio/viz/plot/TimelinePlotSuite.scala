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
import eyes4s.studio.core.selection.{
  InputCause,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}
import eyes4s.studio.app.Intent
import intaglio.{DevicePoint, Grob}
import intaglio.interaction.NamedPicking
import munit.FunSuite

/** The timeline builder and its brush (ticket S4.5e): one bar per fixation
  * at its onset, as long and as tall as its duration; a drag across the
  * plot selects exactly the fixations that begin under it. The generic
  * plot/table parity properties run on it in [[PlotTableParitySuite]].
  */
class TimelinePlotSuite extends FunSuite:
  import TimelineSamples.*

  private val rule = TimelineColumns.brushRule(columns)

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def built(t: Timeline, builder: TimelinePlot = TimelinePlot(columns)): BuiltPlot =
    right(builder.build(source(t), Theme.Light))

  private def targetsOn(plot: BuiltPlot): PlotTargets =
    val surface   = right(PlotSurface(800, 150, 1.0))
    val transform = right(PlotTransform.resolve(plot.plot, surface))
    val picking   = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
    right(PlotTargets.resolve(plot, transform, picking))

  private def grobs(plot: BuiltPlot): Vector[Grob] =
    def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
    plot.plot.scene.grobs.flatMap(all)

  private def device(t: PlotTargets, ms: Double): DevicePoint =
    right(t.transform.dataToDevice(DataPoint(ms, 10.0)))

  test("each fixation is a bar at its onset, as long and as tall as its duration") {
    val plot = built(board)
    assertEquals(
      plot.encoding,
      PositionEncoding(
        Axis.Numeric(columns.onset, AxisScale.Linear),
        Axis.Numeric(columns.duration, AxisScale.Linear)
      )
    )
    assertEquals(plot.marks.size, 13)
    assertEquals(plot.marks.map(_.ref), board.refs)
    assertEquals(
      plot.marks.map(_.at),
      board.fixations.map(f => DataPoint(f.onsetMs.toDouble, f.durationMs.toDouble))
    )
    // A bar covers its interval: its middle picks it, past its end does not.
    val t = targetsOn(plot)
    board.fixations.zip(board.refs).foreach { (f, ref) =>
      val mid = right(
        t.transform.dataToDevice(DataPoint(f.onsetMs + f.durationMs / 2.0, f.durationMs / 2.0))
      )
      assertEquals(right(t.pick(mid, 0.0)).map(_.ref), Some(ref), ref)
    }
    // Taller for longer: fixation 6 (412 ms) stands above fixation 12 (110 ms).
    val tall  = right(t.target(board.refs(5)).toRight("6")).anchor
    val short = right(t.target(board.refs(11)).toRight("12")).anchor
    assert(tall.y < short.y, s"$tall vs $short")
  }

  test("a drag selects exactly the fixations whose bars it touches") {
    val t = targetsOn(built(board))
    // The board's brush, 1.20 to 2.80 s: fixations 4 to 7 (3 ends at 1.072 s,
    // 8 begins at 2.932 s).
    val b =
      right(
        PlotBrushing.brushed(t, rule, device(t, 1200.0), device(t, 2800.0)).toRight("no brush")
      )
    assertEqualsDouble(b.span.from, 1200.0, 1e-6)
    assertEqualsDouble(b.span.until, 2800.0, 1e-6)
    assertEquals(b.refs, board.refs.slice(3, 7))
    // Dragged right to left, the same.
    assertEquals(
      PlotBrushing.brushed(t, rule, device(t, 2800.0), device(t, 1200.0)).map(_.refs),
      Some(b.refs)
    )
    // The brush's rows are the source's rows whose intervals overlap the span.
    assertEquals(b.refs, PlotBrush.rows(t.plot.source, rule, b.span))
    // No movement is no brush.
    assertEquals(PlotBrushing.brushed(t, rule, device(t, 1500.0), device(t, 1500.0)), None)
    // A brush inside fixation 4's bar, where no fixation begins, selects 4.
    assertEquals(
      PlotBrushing.brushed(t, rule, device(t, 1500.0), device(t, 1580.0)).map(_.refs),
      Some(Vector(board.refs(3)))
    )
  }

  test("a brush selection replaces the view's selection; an empty one clears it") {
    val view  = right(ViewId.of("explore.timeline"))
    val state = MarkInputState.initial[StudioRef](view, SelectionState.empty)
    val refs  = board.refs.slice(3, 7)
    state.brushed(refs).intents match
      case Vector(Intent.Select(input)) =>
        assertEquals(input.mode, SelectionMode.Replace)
        assertEquals(input.refs, refs)
        assertEquals(input.stamp.cause, InputCause.Pointer)
      case other => fail(s"$other")
    state.brushed(Vector.empty).intents match
      case Vector(Intent.Select(input)) => assertEquals(input.mode, SelectionMode.Clear)
      case other                        => fail(s"$other")
  }

  test("the brush is shaded behind the bars and the playhead drawn over them") {
    val span  = right(HalfOpenSpan.between(1200.0, 2800.0).toRight("span"))
    val plain = grobs(built(board)).size
    val plot  = built(board, TimelinePlot(columns, Some(span), Some(2160.0)))
    assertEquals(grobs(plot).size, plain + 2)
    // The plot writes no numbers: only the axis title.
    assertEquals(grobs(plot).collect { case t: Grob.Text => t.label }, Vector("Onset (ms)"))
  }

  test("a row without an onset or a duration is set aside") {
    val base = source(board)
    val rows = base.rows
      .updated(0, base.rows(0).copy(values = base.rows(0).values.updated(1, PlotValue.Missing)))
      .updated(1, base.rows(1).copy(values = base.rows(1).values.updated(2, PlotValue.Missing)))
    val plot = right(
      TimelinePlot(columns)
        .build(right(PlotSource(base.caption, base.columns, rows)), Theme.Light)
    )
    assertEquals(
      plot.unplotted,
      Vector(
        Unplotted(board.refs(0), 0, NoPosition.MissingValue(columns.onset)),
        Unplotted(board.refs(1), 1, NoPosition.MissingValue(columns.duration))
      )
    )
  }

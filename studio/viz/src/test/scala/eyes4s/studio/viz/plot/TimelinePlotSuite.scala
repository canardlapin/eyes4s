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
import eyes4s.studio.app.tokens.{Theme, ThemedToken}
import eyes4s.studio.core.selection.{
  ContextRevision,
  InputCause,
  InputStamp,
  SelectionInput,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}
import eyes4s.studio.app.Intent
import intaglio.{DashPattern, DevicePoint, GraphicParams, Grob, LineType, Point, value}
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

  test(
    "an undeclared large-origin timeline uses observed coverage without changing its source"
  ) {
    val t                          = timeline(Vector(600000L -> 400L, 600500L -> 300L))
    val plot                       = built(t)
    val targets                    = targetsOn(plot)
    val first                      = device(targets, 600000.0)
    val end                        = device(targets, 600400.0)
    val MinimumHalfCoverageWidthPx = 300.0
    assert(
      end.x - first.x > MinimumHalfCoverageWidthPx,
      s"First400ms of800ms coverage is compressed: ${end.x - first.x}px"
    )
    assertEquals(plot.source, source(t))
    assertEquals(plot.marks.map(_.at.x), Vector(600000.0, 600500.0))
  }

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

  test(
    "each bar covers exactly [onset, onset + duration): picked just inside both ends, not past its end"
  ) {
    val plot      = built(board)
    val surface   = right(PlotSurface(2000, 300, 1.0))
    val transform = right(PlotTransform.resolve(plot.plot, surface))
    val picking   = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
    val t         = right(PlotTargets.resolve(plot, transform, picking))
    board.fixations.zip(board.refs).foreach { (f, ref) =>
      val y     = f.durationMs / 2.0
      val start = right(transform.dataToDevice(DataPoint(f.onsetMs.toDouble, y)))
      val end = right(transform.dataToDevice(DataPoint((f.onsetMs + f.durationMs).toDouble, y)))
      def at(dx: Double) = right(t.pick(DevicePoint(dx, start.y), 0.0)).map(_.ref)
      assertEquals(at(start.x + 1.0), Some(ref), s"$ref just after its onset")
      assertEquals(at(end.x - 1.0), Some(ref), s"$ref just before its end")
      // Every gap between bars is at least 80 ms: past the end is no bar.
      assertEquals(at(end.x + 1.0), None, s"$ref just past its end")
      assertEquals(at(start.x - 1.0).filter(_ == ref), None, s"$ref just before its onset")
    }
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

  test(
    "a declared trial end keeps the gap after fixation bars inside the initial plot domain"
  ) {
    val declared = targetsOn(built(board, TimelinePlot(columns, trialEndMs = Some(10000.0))))
    val end      = declared.transform.deviceToCanvas(device(declared, 10000.0))
    assert(end.x <= declared.transform.surface.logicalWidth)
    val coverage = targetsOn(built(board))
    val beyond   = coverage.transform.deviceToCanvas(device(coverage, 10000.0))
    assert(beyond.x > coverage.transform.surface.logicalWidth)
  }

  test("a fixation outside the window is a dashed outline with no fill, still picked") {
    val outside = board.refs(11)
    val plot    = built(board, TimelinePlot(columns, outsideWindow = Set(outside)))
    val rects   = grobs(plot).collect {
      case r: Grob.Rect if r.name.exists(_.value.startsWith(TimelinePlot.MarkPrefix)) =>
        r.name.get.value -> r
    }.toMap
    assertEquals(rects.size, 13)
    val ink3 = IntaglioColours.themed(Theme.Light, ThemedToken.Ink3)
    val dash = right(DashPattern(TimelinePlot.OutsideDashPx))
    board.refs.zipWithIndex.foreach { (ref, i) =>
      val gp = rects(s"${TimelinePlot.MarkPrefix}$i").gp
      if ref == outside then
        assertEquals(gp.fill, None, "outside: no fill")
        assertEquals(gp.stroke, Some(ink3), "outside: Ink3 outline")
        assertEquals(gp.lineType, LineType.Custom(dash), "outside: the custom dash")
      else assertEquals((gp.fill, gp.stroke), (Some(ink3), None), s"$ref: a filled bar")
    }
    // Unfilled, it is still the fixation a click in it picks.
    val t   = targetsOn(plot)
    val f   = board.fixations(11)
    val mid =
      right(
        t.transform.dataToDevice(DataPoint(f.onsetMs + f.durationMs / 2.0, f.durationMs / 2.0))
      )
    assertEquals(right(t.pick(mid, 0.0)).map(_.ref), Some(outside))
    // Every other mark is drawn as before.
    assertEquals(plot.marks, built(board).marks)
  }

  // Each bar's paint, by row.
  private def paints(plot: BuiltPlot): Map[Int, GraphicParams] =
    grobs(plot).collect {
      case r: Grob.Rect if r.name.exists(_.value.startsWith(TimelinePlot.MarkPrefix)) =>
        r.name.get.value.stripPrefix(TimelinePlot.MarkPrefix).toInt -> r.gp
    }.toMap

  private def span(from: Double, until: Double): HalfOpenSpan =
    right(HalfOpenSpan.between(from, until).toRight(s"$from to $until"))

  private def brushed(s: HalfOpenSpan, outside: Set[StudioRef] = Set.empty): BuiltPlot =
    built(board, TimelinePlot(columns, Some(s), outsideWindow = outside))

  test("without a brush no bar is dimmed, in either theme") {
    Theme.values.foreach { theme =>
      val plot = right(TimelinePlot(columns).build(source(board), theme))
      val ink3 = IntaglioColours.themed(theme, ThemedToken.Ink3)
      assertEquals(paints(plot).size, 13)
      paints(plot).foreach((i, gp) =>
        assertEquals((gp.fill, gp.stroke), (Some(ink3), None), s"$theme $i")
      )
    }
  }

  test("a brush dims to the neutral mark exactly the bars it does not pick") {
    Theme.values.foreach { theme =>
      val b    = span(1200.0, 2800.0)
      val plot = right(TimelinePlot(columns, Some(b)).build(source(board), theme))
      val ink3 = IntaglioColours.themed(theme, ThemedToken.Ink3)
      val dim  = IntaglioColours.themed(theme, ThemedToken.NeutralMark)
      assertNotEquals(ink3, dim)
      // The board's brush, 1.20 to 2.80 s, picks fixations 4 to 7.
      val picked = PlotBrush.rows(plot.source, rule, b)
      assertEquals(picked, board.refs.slice(3, 7))
      board.refs.zipWithIndex.foreach { (ref, i) =>
        val gp   = paints(plot)(i)
        val fill = if picked.contains(ref) then ink3 else dim
        assertEquals((gp.fill, gp.stroke), (Some(fill), None), s"$theme $ref")
      }
      // Dimming moves no mark.
      assertEquals(plot.marks, right(TimelinePlot(columns).build(source(board), theme)).marks)
    }
  }

  test("a bar is inside the brush iff its half-open interval overlaps the span") {
    val ink3 = IntaglioColours.themed(Theme.Light, ThemedToken.Ink3)
    def undimmed(s: HalfOpenSpan): Set[Int] =
      paints(brushed(s)).collect { case (i, gp) if gp.fill.contains(ink3) => i }.toSet
    // [1072, 2932): fixation 3 ends at 1072 and fixation 8 begins at 2932;
    // touching an end of the span is not overlap, so both are dimmed.
    assertEquals(undimmed(span(1072.0, 2932.0)), Set(3, 4, 5, 6))
    // One millisecond wider on each side, both cross an edge and are inside.
    assertEquals(undimmed(span(1071.0, 2933.0)), Set(2, 3, 4, 5, 6, 7))
    // A span inside one bar keeps that bar, though no fixation begins in it.
    assertEquals(undimmed(span(1500.0, 1580.0)), Set(3))
    // A span in a gap picks nothing, so every bar is dimmed.
    assertEquals(undimmed(span(2870.0, 2910.0)), Set.empty[Int])
    // Always the rows the brush selects.
    Vector(span(1072.0, 2932.0), span(1071.0, 2933.0), span(1500.0, 1580.0)).foreach { s =>
      val plot = brushed(s)
      assertEquals(
        undimmed(s).toVector.sorted.map(board.refs),
        PlotBrush.rows(plot.source, rule, s)
      )
    }
  }

  test("outside the window and the brush, a bar is a dashed outline in the neutral mark") {
    val ink3 = IntaglioColours.themed(Theme.Light, ThemedToken.Ink3)
    val dim  = IntaglioColours.themed(Theme.Light, ThemedToken.NeutralMark)
    val dash = LineType.Custom(right(DashPattern(TimelinePlot.OutsideDashPx)))
    // Fixation 5 is in the brush, 12 is not; both are outside the window.
    val plot = brushed(span(1200.0, 2800.0), Set(board.refs(4), board.refs(11)))
    val in   = paints(plot)(4)
    assertEquals((in.fill, in.stroke, in.lineType), (None, Some(ink3), dash), "5: in the brush")
    val out = paints(plot)(11)
    assertEquals((out.fill, out.stroke, out.lineType), (None, Some(dim), dash), "12: dimmed")
    assertEquals(out.lineWidth, in.lineWidth)
    // A bar in the window and outside the brush is a dimmed fill.
    assertEquals((paints(plot)(10).fill, paints(plot)(10).stroke), (Some(dim), None))
  }

  test("a dimmed bar's selection and focus rings are drawn at full strength") {
    val view                   = right(ViewId.of("explore.timeline"))
    val ref                    = board.refs(11)
    def rings(plot: BuiltPlot) =
      val t   = targetsOn(plot)
      val bus = right(
        SelectionState.empty.submit(
          SelectionInput(
            InputStamp(ContextRevision(0), view, 0L, InputCause.Pointer),
            SelectionMode.Replace,
            Vector(ref)
          )
        )
      )
      val state = MarkInputState
        .initial[StudioRef](view, bus)
        .moveFocus(Some(ref), t)
        .state
        .focusChanged(true)
        .state
      val overlay = state.overlay(t)
      val scene   = right(
        OverlayRings.scene(overlay, OverlayPalette.onSurface(Theme.Light), 800, 150, 1.0)
      )
      def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
      (overlay, scene.grobs.flatMap(all).collect { case p: Grob.Polygon => p.gp })
    val dimmed = brushed(span(1200.0, 2800.0))
    val dim    = IntaglioColours.themed(Theme.Light, ThemedToken.NeutralMark)
    assertEquals(paints(dimmed)(11).fill, Some(dim))
    val (overlay, gps) = rings(dimmed)
    assertEquals(overlay.map(_.kind), Vector(RingKind.Selected, RingKind.Focus))
    // The same rings, in the same paint, as on the undimmed bar.
    assertEquals((overlay, gps), rings(built(board)))
    assert(!gps.exists(gp => gp.stroke.contains(dim) || gp.fill.contains(dim)), s"$gps")
  }

  test("a bar's selection and focus rings are rectangles round the bar, not circles") {
    val plot      = built(board)
    val scale     = 2.0
    val transform = right(PlotTransform.resolve(plot.plot, right(PlotSurface(800, 150, scale))))
    val picking   = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
    val t         = right(PlotTargets.resolve(plot, transform, picking))
    val f         = board.fixations(5)
    val ref       = board.refs(5)
    val corner    = right(transform.dataToDevice(DataPoint(f.onsetMs.toDouble, 0.0)))
    val opposite  = right(
      transform.dataToDevice(
        DataPoint((f.onsetMs + f.durationMs).toDouble, f.durationMs.toDouble)
      )
    )
    val bar = DeviceBox.spanning(corner, opposite)
    assertEquals(right(t.target(ref).toRight("6")).box, Some(bar))
    val view = right(ViewId.of("explore.timeline"))
    val bus  = right(
      SelectionState.empty.submit(
        SelectionInput(
          InputStamp(ContextRevision(0), view, 0L, InputCause.Pointer),
          SelectionMode.Replace,
          Vector(ref)
        )
      )
    )
    val state = MarkInputState
      .initial[StudioRef](view, bus)
      .moveFocus(Some(ref), t)
      .state
      .focusChanged(true)
      .state
    val shape = RingShape.Box(bar.halfWidth, bar.halfHeight)
    assertEquals(
      state.overlay(t),
      Vector(
        OverlayRing(RingKind.Selected, ref, bar.centre, OverlayRings.GapPx * scale, shape),
        OverlayRing(
          RingKind.Focus,
          ref,
          bar.centre,
          (OverlayRings.GapPx + 2.0 * OverlayRings.SelectedBandPx) * scale,
          shape
        )
      )
    )
    // Drawn, each band is a four-cornered outline round the bar.
    val scene = right(
      OverlayRings.scene(
        state.overlay(t),
        OverlayPalette.onSurface(Theme.Light),
        1600,
        300,
        scale
      )
    )
    def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
    val outlines = scene.grobs.flatMap(all).collect { case p: Grob.Polygon => p.points }
    val out      = OverlayRings.SelectedBandPx / 2.0 * scale + OverlayRings.GapPx * scale
    val inner    = Vector((-1.0, -1.0), (1.0, -1.0), (1.0, 1.0), (-1.0, 1.0)).map((sx, sy) =>
      right(
        Point.native(
          bar.centre.x + sx * (bar.halfWidth + out),
          bar.centre.y + sy * (bar.halfHeight + out)
        )
      )
    )
    assertEquals(outlines.map(_.size), Vector(4, 4, 4))
    assertEquals(outlines.head, inner)
  }

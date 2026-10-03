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
import eyes4s.studio.app.text.{Format, LadderText, LadderTextId}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.selection.StudioRef
import intaglio.GraphicsName
import intaglio.interaction.NamedPicking
import munit.FunSuite

/** The scale ladder builder (ticket S4.5b): what it draws for each row of
  * the ladder's source, the beeswarm, the histogram fallback and its bins,
  * and the summary readout of an aggregate mark. The generic plot/table
  * parity properties run on it in [[PlotTableParitySuite]].
  */
class ScaleLadderPlotSuite extends FunSuite:
  import LadderSamples.*
  import ScaleLadderPlot.Role

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def built(ladder: ScaleLadder, focus: Option[String] = Some("2°")): BuiltPlot =
    right(ScaleLadderPlot(columns, focus).build(source(ladder), Theme.Light))

  private def targetsOn(plot: BuiltPlot): PlotTargets =
    val surface   = right(PlotSurface(640, 400, 1.0))
    val transform = right(PlotTransform.resolve(plot.plot, surface))
    val picking   = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
    right(PlotTargets.resolve(plot, transform, picking))

  private def role(m: PlotMark): Role = right(ScaleLadderPlot.roleOf(m.ref).toRight(m.ref))

  // The marks on the row of scale `label`.
  private def onRow(plot: BuiltPlot, label: String): Vector[PlotMark] =
    val level = ScaleLadderPlot.scaleLevels(plot.source, columns.scale).indexOf(label).toDouble
    plot.marks.filter(_.at.y == level)

  test("the board's ladder draws D, B, the controls and M on each scale's row, in that order") {
    val plot = built(board)
    assertEquals(
      plot.encoding,
      PositionEncoding(
        Axis.Numeric(columns.cosine, AxisScale.Linear),
        Axis.Category(columns.scale, Vector("0.5°", "1°", "2°", "4°"))
      )
    )
    Vector("0.5°", "1°", "4°").foreach { label =>
      assertEquals(onRow(plot, label).map(role), Vector(Role.Contrast, Role.Mean, Role.Matched))
    }
    assertEquals(
      onRow(plot, "2°").map(role),
      Vector(Role.Contrast, Role.Mean) ++ Vector.fill(19)(Role.Control) :+ Role.Matched
    )
    // Rows are drawn row by row, so the roving order is the drawing order.
    assertEquals(plot.marks.map(_.at.y), plot.marks.map(_.at.y).sorted)
    // The 57 controls the fixture scores at 2° only are listed, not drawn.
    assertEquals(plot.unplotted.size, 57)
    plot.unplotted.foreach { u =>
      assertEquals(ScaleLadderPlot.roleOf(u.ref), Some(Role.Control))
      assertEquals(u.reason, NoPosition.MissingValue(columns.cosine))
    }
  }

  test("the focus row's B, M and controls sit at their cosines, and D spans B to M") {
    val plot  = built(board)
    val row   = onRow(plot, "2°")
    val focus = board.scales(2)
    def one(r: Role) = row.filter(role(_) == r)
    assertEquals(one(Role.Mean).map(_.rows.head.marking), Vector(RowMarking.Placed(DataPoint(0.35, 2.0))))
    assertEquals(one(Role.Matched).map(_.rows.head.marking), Vector(RowMarking.Placed(DataPoint(0.73, 2.0))))
    assertEquals(one(Role.Mean).map(_.ref), Vector(focus.mean))
    assertEquals(one(Role.Matched).map(_.ref), Vector(focus.matched))
    assertEquals(
      one(Role.Contrast).map(m => (m.ref, m.rows.head.marking, m.at)),
      Vector(
        (
          focus.contrast,
          RowMarking.Positionless(NoPosition.MissingValue(columns.cosine)),
          DataPoint((0.35 + 0.73) / 2.0, 2.0)
        )
      )
    )
    assertEquals(
      one(Role.Control).map(_.rows.head.marking),
      focusControls.map(c => RowMarking.Placed(DataPoint(c, 2.0)))
    )
    // Each mark says its table row: D's is its signed difference.
    val d = one(Role.Contrast).head
    assertEquals(plot.readout(d), plot.source.rowOf(d.ref).flatMap(plot.source.rowText))
    assert(plot.readout(d).exists(_.contains("D = M − B +0.38")), plot.readout(d))
    assert(plot.readout(one(Role.Mean).head).exists(_.contains("Cosine 0.35")))
  }

  test("the beeswarm spreads close controls ±7 px on the focus row and ±5 px on the others") {
    val controls = Vector(Some(0.30), Some(0.30), Some(0.30), Some(0.30), Some(0.70))
    val ladder   = LadderSamples.ladder(
      Spec("1°", 0.5, 0.3, 0.2, controls),
      Spec("2°", 0.5, 0.3, 0.2, controls)
    )
    def nudges(plot: BuiltPlot, label: String) =
      onRow(plot, label).filter(role(_) == Role.Control).map(_.nudgePx.dyPx)
    val focused = built(ladder)
    assertEquals(nudges(focused, "2°"), Vector(0.0, 7.0, -7.0, 0.0, 0.0))
    assertEquals(nudges(focused, "1°"), Vector(0.0, 5.0, -5.0, 0.0, 0.0))
    // With no focus, every row takes the focus step.
    assertEquals(nudges(built(ladder, None), "1°"), Vector(0.0, 7.0, -7.0, 0.0, 0.0))
    // Nudged marks are apart for the cursor, as on screen.
    val t       = targetsOn(focused)
    val anchors = onRow(focused, "2°").filter(role(_) == Role.Control).take(3)
      .flatMap(m => t.target(m.ref).map(_.anchor))
    assertEquals(anchors.distinct.size, 3)
  }

  test("the swarm takes lanes in order of value and restarts beyond the dodge") {
    val lanes = ScaleLadderPlot.swarm(
      Vector(0 -> 0.50, 1 -> 0.20, 2 -> 0.505, 3 -> 0.51, 4 -> 0.515, 5 -> 0.9),
      0.01
    )
    assertEquals(lanes, Map(1 -> 0, 0 -> 0, 2 -> 1, 3 -> -1, 4 -> 0, 5 -> 0))
  }

  test("the first scale is the top row") {
    val plot = built(board)
    val t    = targetsOn(plot)
    val ys   = Vector("0.5°", "1°", "2°", "4°").map(l =>
      t.target(onRow(plot, l).filter(role(_) == Role.Matched).head.ref).get.anchor.y
    )
    assertEquals(ys, ys.sorted)
    assert(ys.distinct.size == 4, ys)
  }

  private def sixty(n: Int): ScaleLadder =
    LadderSamples.ladder(
      Spec("2°", 0.73, 0.35, 0.38, Vector.tabulate(n)(k => Some(0.1 + 0.8 * k / n)))
    )

  test("above 50 controls a scale's controls become a histogram of declared bins") {
    val plot    = built(sixty(60))
    val bars    = plot.marks.filter(m => m.rows.forall(_.marking == RowMarking.Represented))
    val drawn   = plot.marks.filter(role(_) == Role.Control)
    assertEquals(drawn, bars)
    assertEquals(bars.flatMap(_.refs).toSet, sixty(60).scales.head.controls.map(_.ref).toSet)
    assertEquals(bars.map(_.rows.size).sum, 60)
    bars.foreach { bar =>
      val k = LadderBins.of(bar.at.x)
      assertEqualsDouble(bar.at.x, (LadderBins.lower(k) + LadderBins.upper(k)) / 2.0, 1e-12)
      bar.rows.foreach { r =>
        val cosine = right(plot.source.number(r.row, columns.cosine).toRight(r.ref))
        assert(LadderBins.lower(k) <= cosine && cosine < LadderBins.upper(k), s"$cosine in bin $k")
      }
      // The bar's summary names its scale and its edges; the table lists its rows.
      assertEquals(
        plot.readout(bar),
        Some(
          PlotText(
            PlotTextId.MarkRows,
            bar.rows.size.toString,
            LadderText(
              LadderTextId.Bin,
              "2°",
              Format.decimal(LadderBins.lower(k), 2),
              Format.decimal(LadderBins.upper(k), 2)
            )
          )
        )
      )
      // A bar is drawn up from the row, as tall as its count allows.
      assert(bar.nudgePx.dyPx < 0.0, bar.nudgePx)
    }
    assertEquals(
      bars.map(_.nudgePx.dyPx).min,
      -(ScaleLadderPlot.BinLiftPx + ScaleLadderPlot.BarMaxPx / 2.0)
    )
    // B, D and M are drawn as on any ladder.
    assertEquals(plot.marks.map(role).filterNot(_ == Role.Control), Vector(Role.Contrast, Role.Mean, Role.Matched))
  }

  test("the histogram starts above 50 shown controls") {
    def aggregates(n: Int) = built(sixty(n)).marks.count(_.rows.size > 1)
    assertEquals(aggregates(50), 0)
    assert(aggregates(51) > 0)
  }

  test("a bin's edges are exact: every value lies in the bin its edges name") {
    (-60L to 60L).foreach { k =>
      assertEquals(LadderBins.of(LadderBins.lower(k)), k)
      assertEquals(LadderBins.of(Math.nextDown(LadderBins.upper(k))), k)
    }
    assertEquals(LadderBins.of(0.35), 7L)
    assertEquals(LadderBins.of(0.7), 14L)
  }

  test("D without a placed B or M is listed, not drawn") {
    val plot0  = built(board)
    val source = plot0.source
    val bRow   = right(source.rowOf(board.scales(2).mean).toRight("no B"))
    val ci     = right(source.indexOf(columns.cosine).toRight("no cosine"))
    val noB    = right(
      PlotSource(
        source.caption,
        source.columns,
        source.rows.updated(
          bRow,
          source.rows(bRow).copy(values = source.rows(bRow).values.updated(ci, PlotValue.Missing))
        )
      )
    )
    val plot = right(ScaleLadderPlot(columns, Some("2°")).build(noB, Theme.Light))
    val dRow = right(noB.rowOf(board.scales(2).contrast).toRight("no D"))
    assertEquals(plot.markOf(board.scales(2).contrast), None)
    assert(plot.unplotted.contains(Unplotted(board.scales(2).contrast, dRow, NoPosition.MissingValue(columns.cosine))))
    assert(plot.unplotted.contains(Unplotted(board.scales(2).mean, bRow, NoPosition.MissingValue(columns.cosine))))
  }

  test("a row the ladder does not draw, or a D with a cosine, is refused by name") {
    val source = LadderSamples.source(board)
    val stray  = right(
      PlotSource(
        source.caption,
        source.columns,
        source.rows :+ PlotRow(StudioRef.Participant("P17"), source.rows.head.values)
      )
    )
    assertEquals(
      ScaleLadderPlot(columns).build(stray, Theme.Light).left.toOption,
      Some(PlotBuildError.UnexpectedRow("scale-ladder", StudioRef.Participant("P17")))
    )
    val dRow = right(source.rowOf(board.scales(0).contrast).toRight("no D"))
    val ci   = right(source.indexOf(columns.cosine).toRight("no cosine"))
    val dWithCosine = right(
      PlotSource(
        source.caption,
        source.columns,
        source.rows.updated(
          dRow,
          source.rows(dRow).copy(values = source.rows(dRow).values.updated(ci, PlotValue.Number(0.2)))
        )
      )
    )
    assertEquals(
      ScaleLadderPlot(columns).build(dWithCosine, Theme.Light).left.toOption,
      Some(PlotBuildError.UnexpectedRow("scale-ladder", board.scales(0).contrast))
    )
    val otherColumns = columns.copy(cosine = right(ColumnId.of("score")))
    assertEquals(
      ScaleLadderPlot(otherColumns).build(source, Theme.Light).left.toOption,
      Some(PlotBuildError.MissingColumn("scale-ladder", otherColumns.cosine))
    )
  }

  test("a summary is refused for a mark of one row or with no words, and else read out") {
    val name  = right(GraphicsName("m", "test mark"))
    val refs  = Vector(StudioRef.Participant("P1"), StudioRef.Participant("P2"))
    val rows  = refs.zipWithIndex.map((r, i) => MarkedRow(r, i, RowMarking.Represented))
    val one   = right(PlotMark.of("k", rows.take(1), DataPoint(0, 0), 1.0, 0, name))
    val two   = right(PlotMark.of("k", rows, DataPoint(0, 0), 1.0, 0, name))
    assertEquals(one.summarised("k", "bin"), Left(PlotBuildError.SummaryOfOne("k", "m")))
    assertEquals(two.summarised("k", "  "), Left(PlotBuildError.BlankSummary("k", "m")))
    val said = right(two.summarised("k", "bin 3"))
    assertEquals(said.summary, Some("bin 3"))
    // Moving or renaming the mark keeps its summary.
    assertEquals(said.withOrder(4).summary, Some("bin 3"))
    assertEquals(right(said.nudged("k", 1.0, 2.0)).summary, Some("bin 3"))
  }

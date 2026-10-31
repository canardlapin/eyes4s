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
import intaglio.{Grob, LineType}
import munit.FunSuite

/** The Results summary's slopegraphs (overhaul v4): one line per
  * participant per σ at its two means, the panel's mean line after them, a
  * missing mean set aside rather than drawn as zero, and the frame's
  * selection, thin participants and Δ by σ. The generic plot/table parity
  * properties run on it in [[PlotTableParitySuite]].
  */
class SlopegraphPlotSuite extends FunSuite:
  import ParticipantSamples.*

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def built(m: ParticipantMeans, frame: SlopeFrame = SlopeFrame.empty): BuiltPlot =
    right(SlopegraphPlot(slopeColumns, frame).build(slopes(m), Theme.Light))

  private def grobs(plot: BuiltPlot): Vector[Grob] =
    def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
    plot.plot.scene.grobs.flatMap(all)

  private def lineOf(plot: BuiltPlot, m: PlotMark): Grob.Lines =
    grobs(plot)
      .collectFirst { case l: Grob.Lines if l.name.contains(m.name) => l }
      .getOrElse(fail(s"no line ${m.name}"))

  test("each σ panel holds every participant's line and then the mean line") {
    val plot = built(board)
    assertEquals(
      plot.encoding,
      PositionEncoding(
        Axis.Panels(
          slopeColumns.scale,
          Vector("σ 0.5°", "σ 2°"),
          slopeColumns.group,
          Vector("Remembered", "Forgotten"),
          SlopegraphPlot.Pitch,
          1.0
        ),
        Axis.Numeric(slopeColumns.d, AxisScale.Linear)
      )
    )
    // Two panels of 24 participants and a mean line each.
    assertEquals(plot.marks.size, 2 * 25)
    assertEquals(plot.unplotted, Vector.empty)
    fixtureRows.zipWithIndex.foreach { case ((p, r, _, f, _), k) =>
      val second = plot.marks(25 + k)
      assertEquals(second.rows.size, 2)
      assertEquals(
        second.rows.map(_.marking),
        Vector(
          RowMarking.Placed(DataPoint(SlopegraphPlot.Pitch, r)),
          RowMarking.Placed(DataPoint(SlopegraphPlot.Pitch + 1.0, f))
        ),
        p
      )
    }
    val mean = plot.marks(49)
    assertEquals(
      mean.rows.map(_.marking),
      Vector(
        RowMarking.Placed(DataPoint(SlopegraphPlot.Pitch, 0.30)),
        RowMarking.Placed(DataPoint(SlopegraphPlot.Pitch + 1.0, 0.15))
      )
    )
  }

  test("a missing mean is set aside, never drawn at zero; one mean is a lone dot") {
    val plot    = built(zeroAndMissing)
    val missing = plot.unplotted.map(u => (u.reason, slopes(zeroAndMissing).rows(u.row).ref))
    assert(missing.nonEmpty)
    assert(missing.forall(_._1 == NoPosition.MissingValue(slopeColumns.d)), missing)
    // Every placed row sits at its own value: none at zero for a missing one.
    plot.marks.flatMap(_.rows).foreach { r =>
      r.marking match
        case RowMarking.Placed(at) =>
          assertEquals(Some(at.y), slopes(zeroAndMissing).number(r.row, slopeColumns.d))
        case other => fail(s"unexpected $other")
    }
    assert(plot.marks.exists(_.rows.size == 1), "no lone dot")
  }

  test("the selected participant is drawn darker and a thin one dotted") {
    val plot = built(
      board,
      SlopeFrame(Some("σ 2°"), Some("P17"), Set("P21"), Vector("Rem", "Forg"), Vector.empty)
    )
    def line(p: String) =
      lineOf(plot, plot.marks.find(m => slopes(board).text(m.rows.head.row, 2).contains(p)).get)
    assertEquals(line("P17").gp.alpha, SlopegraphPlot.SelectedAlpha)
    assertEquals(line("P01").gp.alpha, SlopegraphPlot.FaintAlpha)
    assert(line("P21").gp.lineType.isInstanceOf[LineType.Custom], line("P21").gp.lineType)
    assertEquals(line("P01").gp.lineType, LineType.Solid)
  }

  test("Δ by σ is drawn only when the frame carries the contrast at every σ") {
    val bare  = grobs(built(board)).size
    val delta = grobs(
      built(
        board,
        SlopeFrame(
          Some("σ 2°"),
          None,
          Set.empty,
          Vector("Rem", "Forg"),
          Vector(SlopeDelta("0.5°", Some(0.13), 23, 24), SlopeDelta("2°", Some(0.18), 24, 24))
        )
      )
    ).size
    assert(delta > bare, s"$delta vs $bare")
  }

  test("the axes reach the next tick past every value, with headroom") {
    assertEquals(
      SlopegraphPlot.yBounds(Vector(0.11, 0.55)),
      (0.0, 0.62, Vector(0.0, 0.2, 0.4, 0.6))
    )
    assertEquals(SlopegraphPlot.yBounds(Vector(0.1, 0.25))._3, Vector(0.0, 0.1, 0.2, 0.3))
    val (lo, hi, ticks) = SlopegraphPlot.yBounds(Vector(-0.23, 0.45))
    assert(lo < -0.2 && hi > 0.45, (lo, hi))
    assertEquals(ticks.head, -0.4)
    val (d0, d1) = SlopegraphPlot.deltaBounds(Vector(0.13, 0.19, 0.18, 0.10))
    assertEqualsDouble(d0, 0.0, 1e-9)
    assertEqualsDouble(d1, 0.25, 1e-9)
  }

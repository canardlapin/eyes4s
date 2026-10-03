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
import eyes4s.studio.app.text.{ParticipantText, ParticipantTextId}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.backend.Response
import eyes4s.studio.core.selection.StudioRef
import intaglio.{BatchColumn, DashPattern, Grob, LineType, value}
import intaglio.interaction.NamedPicking
import munit.FunSuite

/** The participant plot builder (ticket S4.5c): what it draws for each row
  * of the participant means' source, a missing mean drawn apart from zero,
  * and the n each mark's readout carries. The generic plot/table parity
  * properties run on it in [[PlotTableParitySuite]].
  */
class ParticipantPlotSuite extends FunSuite:
  import ParticipantSamples.*

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def built(means: ParticipantMeans): BuiltPlot =
    right(ParticipantPlot(columns).build(source(means), Theme.Light))

  private def targetsOn(plot: BuiltPlot): PlotTargets =
    val surface   = right(PlotSurface(560, 340, 1.0))
    val transform = right(PlotTransform.resolve(plot.plot, surface))
    val picking   = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
    right(PlotTargets.resolve(plot, transform, picking))

  private def cell(means: ParticipantMeans, p: String, g: String): ParticipantCell =
    means.cells.find(c => c.participant == p && c.group.label == g).getOrElse(fail(s"$p $g"))

  private def grand(means: ParticipantMeans, g: String): GroupGrandMean =
    means.groups.find(_.group.label == g).getOrElse(fail(g))

  private def mark(plot: BuiltPlot, ref: StudioRef): PlotMark =
    right(plot.markOf(ref).toRight(s"no mark for $ref"))

  // Every grob of the scene, depth first.
  private def grobs(plot: BuiltPlot): Vector[Grob] =
    def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
    plot.plot.scene.grobs.flatMap(all)

  private def grobOf(plot: BuiltPlot, m: PlotMark): Grob =
    grobs(plot)
      .find {
        case p: Grob.PointBatch => p.name.contains(m.name)
        case s: Grob.Segments   => s.name.contains(m.name)
        case _                  => false
      }
      .getOrElse(fail(s"no grob ${m.name.value}"))

  test("the board's 24 participants are placed at their means in their group's column") {
    val plot = built(board)
    assertEquals(
      plot.encoding,
      PositionEncoding(
        Axis.Category(columns.group, Vector("Remembered", "Forgotten")),
        Axis.Numeric(columns.d, AxisScale.Linear)
      )
    )
    assertEquals(plot.marks.size, 50)
    assertEquals(plot.unplotted, Vector.empty)
    // Column by column: the participants in row order, then the grand mean.
    assertEquals(plot.marks.map(_.ref), source(board).rows.map(_.ref))
    fixtureRows.foreach { (p, r, _, f, _) =>
      assertEquals(mark(plot, cell(board, p, "Remembered").ref).at, DataPoint(0.0, r))
      assertEquals(mark(plot, cell(board, p, "Forgotten").ref).at, DataPoint(1.0, f))
    }
    assertEquals(mark(plot, grand(board, "Remembered").ref).at, DataPoint(0.0, 0.30))
    assertEquals(mark(plot, grand(board, "Forgotten").ref).at, DataPoint(1.0, 0.15))
    // A participant's nudge spreads it across its column, the same way each
    // time; grand means are not nudged.
    val p17 = mark(plot, cell(board, "P17", "Remembered").ref)
    assertEquals(p17.nudgePx.dxPx, ParticipantPlot.jitterPx(16))
    assertEquals(
      mark(plot, cell(board, "P17", "Forgotten").ref).nudgePx.dxPx,
      ParticipantPlot.jitterPx(16 + ParticipantPlot.JitterShift)
    )
    assertEquals(mark(plot, grand(board, "Forgotten").ref).nudgePx, PixelOffset.Zero)
    assert(plot.marks.map(_.nudgePx.dxPx).distinct.size > 5)
  }

  test("solid and dashed groups: the first group's dots are filled and its tick solid") {
    val plot               = built(board)
    def gp(ref: StudioRef) = grobOf(plot, mark(plot, ref)) match
      case p: Grob.PointBatch =>
        p.graphicParams match
          case BatchColumn.Constant(gp) => gp
          case other                    => fail(s"$other")
      case s: Grob.Segments => s.gp
      case other            => fail(s"$other")
    val remembered = gp(cell(board, "P01", "Remembered").ref)
    val forgotten  = gp(cell(board, "P01", "Forgotten").ref)
    assert(remembered.fill.isDefined && remembered.stroke.isEmpty, remembered)
    assert(forgotten.stroke.isDefined, forgotten)
    assertNotEquals(remembered.fill, forgotten.fill)
    assertEquals(gp(grand(board, "Remembered").ref).lineType, LineType.Solid)
    assertEquals(
      gp(grand(board, "Forgotten").ref).lineType,
      LineType.Custom(DashPattern.unsafe(ParticipantPlot.TickDash*))
    )
  }

  test("a missing mean is a dashed empty ring below every value; a zero mean sits on zero") {
    val means   = zeroAndMissing
    val plot    = built(means)
    val zero    = mark(plot, cell(means, "P01", "Remembered").ref)
    val missing = mark(plot, cell(means, "P02", "Remembered").ref)
    assertEquals(zero.rows.map(_.marking), Vector(RowMarking.Placed(DataPoint(0.0, 0.0))))
    assertEquals(
      missing.rows.map(_.marking),
      Vector(RowMarking.Positionless(NoPosition.MissingValue(columns.d)))
    )
    // Drawn in its group's column, on a band below every value and zero.
    assertEquals(missing.at.x, 0.0)
    val lowest = plot.marks
      .filter(_ != missing)
      .flatMap(m => m.rows.collect { case MarkedRow(_, _, RowMarking.Placed(at)) => at.y })
    assert(lowest.forall(_ > missing.at.y), s"${missing.at.y} vs $lowest")
    assert(missing.at.y < 0.0)
    // Drawn differently: a larger, dashed, unfilled ring.
    val (zeroGrob, missingGrob) = (grobOf(plot, zero), grobOf(plot, missing)) match
      case (a: Grob.PointBatch, b: Grob.PointBatch) => (a, b)
      case other                                    => fail(s"$other")
    assertNotEquals(zeroGrob.sizes, missingGrob.sizes)
    assert(missing.reachPx > zero.reachPx)
    missingGrob.graphicParams match
      case BatchColumn.Constant(gp) =>
        assertEquals(
          gp.lineType,
          LineType.Custom(DashPattern.unsafe(ParticipantPlot.EmptyDash*))
        )
      case other => fail(s"$other")
    // The missing mean reads out as missing, and the zero mean as zero.
    val missingText = right(plot.readout(missing).toRight("no readout"))
    assert(missingText.contains(s"D ${PlotSource.MissingText}"), missingText)
    assert(right(plot.readout(zero).toRight("no readout")).contains("D 0.00"))
    // Both are picked where they are drawn.
    val t = targetsOn(plot)
    Vector(zero, missing).foreach { m =>
      val at = right(t.target(m.ref).toRight(m.ref)).anchor
      assertEquals(right(t.pick(at, 0.0)).map(_.ref), Some(m.ref))
    }
  }

  test(
    "a grand mean says its group's n of participants; a participant's dot its n of queries"
  ) {
    val plot = built(board)
    val tick = right(plot.readout(mark(plot, grand(board, "Forgotten").ref)).toRight("tick"))
    assertEquals(
      tick,
      "Group Forgotten, Mean of all participants, D +0.15, n 24"
    )
    val p17 =
      right(plot.readout(mark(plot, cell(board, "P17", "Forgotten").ref)).toRight("P17"))
    assertEquals(p17, "Group Forgotten, Mean of P17, D +0.32, n 2")
    // Each column writes its group's n under it.
    val written = grobs(plot).collect { case t: Grob.Text => t.label }
    assertEquals(
      written.count(_ == ParticipantText(ParticipantTextId.GroupN, "24")),
      2
    )
    // And each grand mean beside its tick, as the table writes it.
    assert(written.contains("+0.30") && written.contains("+0.15"), written)
  }

  test(
    "a grand mean without a value, or a row without a group, is set aside, with its reason"
  ) {
    val means = zeroAndMissing
    val base  = source(means)
    val g     = grand(means, "Forgotten").ref
    val p     = cell(means, "P03", "Remembered").ref
    val gi    = right(base.rowOf(g).toRight(g))
    val pi    = right(base.rowOf(p).toRight(p))
    val rows  = base.rows.zipWithIndex.map { (r, i) =>
      if i == gi then r.copy(values = r.values.updated(2, PlotValue.Missing))
      else if i == pi then r.copy(values = r.values.updated(0, PlotValue.Missing))
      else r
    }
    val shown = right(PlotSource(base.caption, base.columns, rows))
    val plot  = right(ParticipantPlot(columns).build(shown, Theme.Light))
    assertEquals(
      plot.unplotted,
      Vector(
        Unplotted(p, pi, NoPosition.MissingValue(columns.group)),
        Unplotted(g, gi, NoPosition.MissingValue(columns.d))
      ).sortBy(_.row)
    )
  }

  test("rows of another kind, an ambiguous column or a missing column are refused") {
    val base    = source(board)
    val kind    = ParticipantPlot(columns).kind
    val odd     = StudioRef.Participant("P01")
    val withOdd = right(
      PlotSource(base.caption, base.columns, base.rows.updated(0, base.rows(0).copy(ref = odd)))
    )
    assertEquals(
      ParticipantPlot(columns).build(withOdd, Theme.Light).left.toOption,
      Some(PlotBuildError.UnexpectedRow(kind, odd))
    )
    // A Forgotten mean written in the Remembered column.
    val forgotten = cell(board, "P01", "Forgotten").ref
    val fi        = right(base.rowOf(forgotten).toRight(forgotten))
    val mixed     = right(
      PlotSource(
        base.caption,
        base.columns,
        base.rows.updated(
          fi,
          base
            .rows(fi)
            .copy(values = base.rows(fi).values.updated(0, PlotValue.Text("Remembered")))
        )
      )
    )
    ParticipantPlot(columns).build(mixed, Theme.Light) match
      case Left(PlotBuildError.AmbiguousLevel(_, c, "Remembered", ids)) =>
        assertEquals(c, columns.group)
        assertEquals(
          ids.toSet,
          Set(grand(board, "Remembered").ref, grand(board, "Forgotten").ref)
        )
      case other => fail(s"$other")
    val renamed = right(ColumnId.of("delta"))
    assertEquals(
      ParticipantPlot(columns.copy(d = renamed)).build(base, Theme.Light).left.toOption,
      Some(PlotBuildError.MissingColumn(kind, renamed))
    )
    assertEquals(
      ParticipantPlot(columns.copy(n = columns.meanOf)).build(base, Theme.Light).left.toOption,
      Some(PlotBuildError.NotNumeric(kind, columns.meanOf))
    )
  }

  test("every mark has its own grob name and the roving order is the drawing order") {
    val plot = built(board)
    assertEquals(plot.marks.map(_.name).distinct.size, plot.marks.size)
    assertEquals(plot.marks.map(_.order), plot.marks.indices.toVector)
    assert(plot.marks.forall(_.name.value.startsWith(ParticipantPlot.MarkPrefix)))
    assertEquals(
      plot.marks.map(m => source(board).rowOf(m.ref)),
      plot.marks.indices.toVector.map(Some(_))
    )
    assertEquals(board.groups.map(_.group), Vector(Response.Remembered, Response.Forgotten))
  }

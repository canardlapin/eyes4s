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

import eyes4s.studio.app.Intent
import eyes4s.studio.app.plot.*
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.backend.{Response, RunId}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.selection.{
  InputCause,
  ScaleIndex,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}
import intaglio.{DevicePoint, GraphicsName, Grob, LineType}
import intaglio.interaction.NamedPicking
import munit.ScalaCheckSuite
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Prop}

/** A plot and its TableTwin read one value source (S4.5a, S4.5x).
  *
  * For generated sources and builders (the dot plot with dots set aside,
  * placeholders or merged marks, and a tally with aggregate marks): every
  * row is accounted for exactly once, by one mark or as unplotted; a placed
  * row sits at its row's numbers, and writing its position in the column's
  * format gives the table's cell text; a positionless or unplotted row's
  * table cell shows why; the drawn marks, resolved through the plot's
  * transform on a 1x and a 2x surface, map back to their anchors and pick
  * their own mark; a focused mark says what its rows say. Selecting a row
  * rings the mark that accounts for it, and selecting a mark selects all its
  * rows, through the one bus.
  */
class PlotTableParitySuite extends ScalaCheckSuite:

  // Each case lays a plot out and compiles its picking plan twice.
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(60)

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  /** How far a data position may drift through a device round trip: floating
    * point of the frame arithmetic, far below a displayed decimal.
    */
  private val RoundTripTolerance = 1e-6

  private val plotView  = right(ViewId.of("results.participant-plot"))
  private val tableView = right(ViewId.of("results.participant-plot.table"))

  private def column(id: String, format: ColumnFormat): PlotColumn =
    PlotColumn(right(ColumnId.of(id)), id.toUpperCase, format)

  private val genFormat: Gen[ColumnFormat] = Gen.oneOf(
    Gen.choose(0, 3).map(ColumnFormat.Decimal(_)),
    Gen.choose(0, 3).map(ColumnFormat.Signed(_)),
    Gen.const(ColumnFormat.Count)
  )

  // Coarse values repeat, so rows coincide and share bins.
  private def genValue(format: ColumnFormat, coarse: Boolean): Gen[PlotValue] =
    val number = (format, coarse) match
      case (_, true)               => Gen.choose(0, 2).map(_.toDouble)
      case (ColumnFormat.Count, _) => Gen.choose(-50000, 50000).map(_.toDouble)
      case _                       =>
        Gen.oneOf(
          Gen.choose(-1.0, 1.0),
          Gen.choose(-1000.0, 1000.0),
          Gen.choose(-3, 3).map(_ / 4.0)
        )
    Gen.frequency(9 -> number.map(PlotValue.Number(_)), 1 -> Gen.const(PlotValue.Missing))

  private val reporting = right(ReportingId.of("participant-plot"))
  private val scale     = right(ScaleIndex.of(0))

  /** A source: a label column, then two to four numeric columns; up to 40
    * participants' rows, then up to two groups' rows (derived values such as
    * a group mean, with their own refs).
    */
  private val genSource: Gen[PlotSource] =
    for
      formats <- Gen.choose(2, 4).flatMap(Gen.listOfN(_, genFormat))
      coarse  <- Gen.prob(0.3)
      columns = column("participant", ColumnFormat.Label) +:
        formats.zipWithIndex.map((f, i) => column(s"v$i", f)).toVector
      values = Gen.sequence[Vector[PlotValue], PlotValue](formats.map(genValue(_, coarse)))
      n      <- Gen.choose(0, 40)
      rows   <- Gen.listOfN(n, values)
      groups <- Gen.choose(0, 2).flatMap(Gen.listOfN(_, values))
    yield right(
      PlotSource(
        "Generated",
        columns,
        rows.zipWithIndex.map { (vs, i) =>
          PlotRow(StudioRef.Participant(s"P$i"), PlotValue.Text(s"P$i") +: vs)
        }.toVector ++ groups.zipWithIndex.map { (vs, i) =>
          PlotRow(
            StudioRef.GroupCell(RunId(1), reporting, scale, Response(s"G$i")),
            PlotValue.Text(s"G$i") +: vs
          )
        }
      )
    )

  private def x(source: PlotSource): ColumnId = source.columns(1).id
  private def y(source: PlotSource): ColumnId = source.columns(2).id

  /** A builder of every kind the host must carry. */
  private def genBuilder(source: PlotSource): Gen[PlotBuilder] =
    Gen.oneOf(
      Gen.const(DotPlot(x(source), y(source), "Generated")),
      for
        missing    <- Gen.oneOf(DotPlot.MissingY.values.toSeq)
        coincident <- Gen.oneOf(
          DotPlot.Coincident.Stack,
          DotPlot.Coincident.Merge,
          DotPlot.Coincident.Nudge(7.0)
        )
      yield DotPlot(x(source), y(source), "Generated", missing, coincident),
      Gen.oneOf(0.5, 1.0, 100.0).map(TallyTestPlot(x(source), y(source), _)),
      genEncoding(source).map(EncodedTestPlot(_))
    )

  /** Linear, log and category axes over the source's columns; a category
    * axis lists every participant's label, in a generated order.
    */
  private def genEncoding(source: PlotSource): Gen[PositionEncoding] =
    val labels = source.rows.indices.toVector.flatMap(source.text(_, 0))
    val scales = Gen.oneOf(AxisScale.values.toSeq)
    for
      order <- Gen.pick(labels.size, labels).map(_.toVector)
      xAxis <- Gen.oneOf(
        scales.map(Axis.Numeric(x(source), _)),
        Gen.const(Axis.Category(source.columns(0).id, order))
      )
      yAxis <- scales.map(Axis.Numeric(y(source), _))
    yield PositionEncoding(xAxis, yAxis)

  private val genBuilt: Gen[(PlotSource, PlotBuilder, BuiltPlot)] =
    for
      source  <- genSource
      builder <- genBuilder(source)
      theme   <- Gen.oneOf(Theme.values.toSeq)
    yield (source, builder, right(builder.build(source, theme)))

  private val genPlot: Gen[(PlotSource, BuiltPlot)] = genBuilt.map((s, _, p) => (s, p))

  private def targetsOn(plot: BuiltPlot, scale: Double): PlotTargets =
    val surface   = right(PlotSurface(640, 400, scale))
    val transform = right(PlotTransform.resolve(plot.plot, surface))
    val picking   = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
    right(PlotTargets.resolve(plot, transform, picking))

  // The table's cell of `column` for the row of a reason.
  private def reasonShown(source: PlotSource, row: Int, why: NoPosition): Unit =
    val c    = right(source.indexOf(NoPosition.columnOf(why)).toRight("no column"))
    val cell = right(source.text(row, c).toRight("no cell"))
    why match
      case NoPosition.MissingValue(_) => assertEquals(cell, PlotSource.MissingText)
      case NoPosition.OffScale(_, v)  =>
        assertEquals(cell, PlotSource.write(PlotValue.Number(v), source.columns(c).format))
        assertEquals(source.number(row, source.columns(c).id), Some(v))

  test("the generators reach every kind of mark and every reason") {
    val plots = Gen
      .listOfN(300, genPlot)
      .apply(Gen.Parameters.default, Seed(4545L))
      .getOrElse(fail("no sample"))
      .map(_._2)
    val rows = plots.flatMap(_.marks.flatMap(_.rows))
    val seen = Map(
      "an aggregate of represented rows" -> plots.exists(
        _.marks.exists(m =>
          m.rows.size > 1 && m.rows.forall(_.marking == RowMarking.Represented)
        )
      ),
      "a mark of several placed rows" -> plots.exists(
        _.marks.exists(m =>
          m.rows.size > 1 && m.rows.forall(_.marking.isInstanceOf[RowMarking.Placed])
        )
      ),
      "a positionless mark"  -> rows.exists(_.marking.isInstanceOf[RowMarking.Positionless]),
      "a derived row's mark" -> rows.exists(_.ref.isInstanceOf[StudioRef.GroupCell]),
      "a missing value"      -> plots.exists(
        _.unplotted.exists(_.reason.isInstanceOf[NoPosition.MissingValue])
      ),
      "a value off the scale" -> plots.exists(
        _.unplotted.exists(_.reason.isInstanceOf[NoPosition.OffScale])
      ),
      "a row placed on a log axis" -> plots.exists(p =>
        p.encoding.x == Axis.Numeric(p.source.columns(1).id, AxisScale.Log10) &&
          p.marks.nonEmpty
      ),
      "a row placed on a category axis" -> plots.exists(p =>
        p.encoding.x.isInstanceOf[Axis.Category] && p.marks.nonEmpty
      )
    )
    assertEquals(seen.filterNot(_._2).keySet, Set.empty[String])
  }

  // A placed coordinate `v` on `axis` is the table's cell of `row`.
  private def placedShown(source: PlotSource, axis: Axis, row: Int, v: Double): Unit =
    val column = Axis.columnOf(axis)
    val i      = right(source.indexOf(column).toRight(s"no column $column"))
    val cell   = right(source.text(row, i).toRight(s"no cell $row"))
    axis match
      case Axis.Numeric(_, AxisScale.Linear) =>
        assertEquals(PlotSource.write(PlotValue.Number(v), source.columns(i).format), cell)
        assertEquals(source.number(row, column), Some(v))
      case Axis.Numeric(_, AxisScale.Log10) =>
        val n = right(source.number(row, column).toRight(s"no number at $row"))
        assert(n > 0.0, s"$n is placed on a log axis")
        assertEqualsDouble(math.pow(10.0, v), n, 1e-9 * math.max(1.0, math.abs(n)))
      case Axis.Category(_, levels) =>
        assertEquals(v, math.rint(v))
        assertEquals(levels.lift(v.toInt), Some(cell))

  property("every row is accounted for once, and placed rows sit at the table's values") {
    Prop.forAll(genBuilt) { (source, builder, plot) =>
      val table = TableTwinState.initial(tableView, SelectionState.empty).vm(source)
      assertEquals(table.rows.map(_.ref), source.rows.map(_.ref))
      assertEquals(
        (plot.marks.flatMap(_.refs) ++ plot.unplotted.map(_.ref)).sortBy(r => source.rowOf(r)),
        source.rows.map(_.ref)
      )
      plot.marks.foreach { m =>
        assertEquals(m.ref, m.rows.head.ref)
        m.rows.foreach { r =>
          val row = table.rows(r.row)
          assertEquals(row.ref, r.ref)
          assertEquals(plot.markOf(r.ref), Some(m))
          r.marking match
            case RowMarking.Placed(at) =>
              placedShown(source, plot.encoding.x, r.row, at.x)
              placedShown(source, plot.encoding.y, r.row, at.y)
              if m.rows.size == 1 then assertEquals(m.at, at)
              // A dot plot draws every placed row of a mark at the mark.
              if builder.isInstanceOf[DotPlot] then
                assertEquals(at, m.at, s"${r.ref} in ${m.name}")
            case RowMarking.Positionless(why) => reasonShown(source, r.row, why)
            case RowMarking.Represented       => ()
        }
        val texts = m.rows.map(r => table.rows(r.row).accessibleText)
        plot.readout(m) match
          case Some(said) if m.rows.size == 1 => assertEquals(said, texts.head)
          case Some(said)                     =>
            assert(said.startsWith(s"${m.rows.size} rows: "), said)
            texts.foreach(t => assert(said.contains(t), s"'$said' omits '$t'"))
          case None => fail(s"no readout for ${m.ref}")
      }
      plot.unplotted.foreach { u =>
        assertEquals(plot.markOf(u.ref), None)
        reasonShown(source, u.row, u.reason)
        assertEquals(
          plot.unplottedText(u),
          Some(
            PlotText(
              PlotTextId.Unplotted,
              table.rows(u.row).accessibleText,
              plot.reasonText(u.reason)
            )
          )
        )
      }
      assertEquals(plot.marks.map(_.order), plot.marks.indices.toVector)
    }
  }

  property("drawn marks map back to their anchors and pick their own mark, at 1x and 2x") {
    Prop.forAll(genPlot) { (_, plot) =>
      List(1.0, 2.0).foreach { scale =>
        val t = targetsOn(plot, scale)
        assertEquals(t.targets.map(_.mark), plot.marks)
        t.targets.foreach { target =>
          assertEquals(target.refs, target.mark.refs)
          target.refs.foreach(r => assertEquals(t.target(r), Some(target)))
          // The anchor less the mark's nudge is its data point.
          val nudge = target.mark.nudgePx
          val back  = t.transform.deviceToData(
            DevicePoint(
              target.anchor.x - nudge.dxPx * scale,
              target.anchor.y - nudge.dyPx * scale
            )
          )
          assertEqualsDouble(back.x, target.mark.at.x, RoundTripTolerance)
          assertEqualsDouble(back.y, target.mark.at.y, RoundTripTolerance)
          // The mark itself, or one drawn over it that covers its centre.
          right(t.pick(target.anchor, 0.5 * scale)) match
            case Some(hit) =>
              assert(
                hit.ref == target.ref || (hit.order > target.order &&
                  math.hypot(hit.anchor.x - target.anchor.x, hit.anchor.y - target.anchor.y) <=
                  hit.reachPx * scale + RoundTripTolerance),
                s"${scale}x: ${target.ref} picked ${hit.ref}"
              )
            case None => fail(s"${scale}x: nothing picked at ${target.ref}")
        }
      }
    }
  }

  property("selecting a row rings its mark, and selecting a mark selects all its rows") {
    val cases = for
      (source, plot) <- genPlot
      if plot.marks.nonEmpty
      mark <- Gen.oneOf(plot.marks)
      row  <- Gen.oneOf(mark.rows)
    yield (source, plot, mark, row)
    Prop.forAll(cases) { (source, plot, mark, row) =>
      val t            = targetsOn(plot, 1.0)
      val anchor       = t.target(mark.ref).get.anchor
      val initialPlot  = MarkInputState.initial[StudioRef](plotView, SelectionState.empty)
      val initialTable = TableTwinState.initial(tableView, SelectionState.empty)

      // Row -> mark: Enter on the table's cursor row selects that row alone
      // and rings the mark that accounts for it.
      val onRow   = initialTable.moveCursor(Some(row.ref), source).state
      val byTable =
        bus(SelectionState.empty, onRow.handle(RovingKey.Activate(false), source).intents)
      assertEquals(byTable.selected, Vector(row.ref))
      val plotSees = initialPlot.project(byTable)
      assertEquals(plotSees.intents, Vector.empty)
      // A mark of several rows is only partly selected by one.
      val partial = mark.rows.size > 1
      assertEquals(
        plotSees.state.selectionRings(t).map(r => (r.kind, r.ref, r.centre)),
        Vector(
          (if partial then RingKind.PartlySelected else RingKind.Selected, mark.ref, anchor)
        )
      )
      assertEquals(
        onRow.project(byTable).state.vm(source).rows.map(_.selected),
        source.rows.map(_.ref == row.ref)
      )

      // Mark -> rows: Enter on the plot's focused mark, which the table's
      // cursor row carries to the plot.
      val onMark = initialPlot.moveFocus(Some(row.ref), t).state
      assertEquals(onMark.focus, Some(mark.ref))
      val byPlot = bus(
        SelectionState.empty,
        right(onMark.handle(MarkInputEvent.Key(RovingKey.Activate(false)), t, 0.0)).intents
      )
      assertEquals(byPlot.selected, mark.refs)
      val tableSees = initialTable.project(byPlot)
      assertEquals(tableSees.intents, Vector.empty)
      assertEquals(
        tableSees.state.vm(source).rows.map(_.selected),
        source.rows.map(r => mark.refs.contains(r.ref))
      )
      assertEquals(
        onMark.project(byPlot).state.selectionRings(t).map(r => (r.kind, r.ref)),
        Vector((RingKind.Selected, mark.ref))
      )

      // The focused mark says what its rows say, selected or not; a mark of
      // one row says exactly what its cursor row says.
      val readout = right(plot.readout(mark).toRight("no readout"))
      assertEquals(t.accessibleText(onMark), readout)
      assertEquals(
        t.accessibleText(onMark.project(byTable).state),
        if partial then
          PlotText(PlotTextId.PartlySelected, readout, "1", mark.rows.size.toString)
        else PlotText.selected(readout, true)
      )
      assertEquals(
        t.accessibleText(onMark.project(byPlot).state),
        PlotText.selected(readout, true)
      )
      if mark.rows.size == 1 then
        assertEquals(
          t.accessibleText(onMark.project(byPlot).state),
          onRow.project(byPlot).state.vm(source).accessibleText
        )
        assertEquals(t.accessibleText(onMark), onRow.vm(source).accessibleText)
    }
  }

  property("a selection of several rows rings each mark once, and the table marks every row") {
    val cases = for
      (source, plot) <- genPlot
      if plot.marks.size >= 2
      refs = plot.marks.flatMap(_.refs)
      two  <- Gen.pick(2, refs)
      more <- Gen.someOf(refs)
    yield (source, plot, (two.toVector ++ more).distinct)
    Prop.forAll(cases) { (source, plot, refs) =>
      val t           = targetsOn(plot, 1.0)
      val (_, intent) = ViewSelection
        .initial(tableView, SelectionState.empty)
        .submit(SelectionMode.Replace, refs, InputCause.Pointer)
      val selected = bus(SelectionState.empty, Vector(intent))
      val rings    = MarkInputState
        .initial[StudioRef](plotView, SelectionState.empty)
        .project(selected)
        .state
        .selectionRings(t)
      val marks             = refs.flatMap(plot.markOf).distinct
      def kind(m: PlotMark) =
        if m.refs.forall(refs.contains) then RingKind.Selected else RingKind.PartlySelected
      assertEquals(rings.map(r => (r.kind, r.ref)), marks.map(m => (kind(m), m.ref)))
      assertEquals(rings.map(_.centre), marks.map(m => t.target(m.ref).get.anchor))
      assertEquals(
        TableTwinState.initial(tableView, selected).vm(source).rows.map(_.selected),
        source.rows.map(r => refs.contains(r.ref))
      )
    }
  }

  test("a builder refuses a column the source lacks or cannot place, naming it") {
    val source = right(
      PlotSource(
        "Participants",
        Vector(column("participant", ColumnFormat.Label), column("d", ColumnFormat.Signed(2))),
        Vector.empty
      )
    )
    val label   = right(ColumnId.of("participant"))
    val missing = right(ColumnId.of("nope"))
    val d       = right(ColumnId.of("d"))
    assertEquals(
      DotPlot(missing, d, "x").build(source, Theme.Light),
      Left(PlotBuildError.MissingColumn("dot-plot", missing))
    )
    assertEquals(
      DotPlot(label, d, "x").build(source, Theme.Light),
      Left(PlotBuildError.NotNumeric("dot-plot", label))
    )
    assert(PlotBuildError.NotNumeric("dot-plot", label).message.contains("'participant'"))
  }

  /** Three participants: P0 and P1 coincide at (0, 1); P2 has no y. */
  private val three = right(
    PlotSource(
      "Three",
      Vector(
        column("participant", ColumnFormat.Label),
        column("x", ColumnFormat.Decimal(1)),
        column("y", ColumnFormat.Decimal(1))
      ),
      Vector(
        ("P0", PlotValue.Number(0.0), PlotValue.Number(1.0)),
        ("P1", PlotValue.Number(0.0), PlotValue.Number(1.0)),
        ("P2", PlotValue.Number(2.0), PlotValue.Missing)
      ).map((p, vx, vy) => PlotRow(StudioRef.Participant(p), Vector(PlotValue.Text(p), vx, vy)))
    )
  )
  private val xId                              = right(ColumnId.of("x"))
  private val yId                              = right(ColumnId.of("y"))
  private val Vector(p0, p1, p2)               = three.rows.map(_.ref): @unchecked
  private def name(n: String)                  = right(GraphicsName(n, "test mark"))
  private def placed(ref: StudioRef, row: Int) =
    MarkedRow(ref, row, RowMarking.Placed(DataPoint(0.0, 1.0)))

  test("a built plot refuses marks that misstate their rows, twice-drawn rows and misorder") {
    val good = right(DotPlot(xId, yId, "Three").build(three, Theme.Light))
    def rebuilt(marks: Vector[PlotMark], unplotted: Vector[Unplotted] = good.unplotted) =
      BuiltPlot(
        "dot-plot",
        three,
        good.plot,
        good.title,
        good.description,
        good.encoding,
        marks,
        unplotted
      )
    val Vector(a, b) = good.marks: @unchecked
    assert(rebuilt(good.marks).isRight)
    assertEquals(
      rebuilt(Vector(PlotMark.placed(a.ref, 1, a.at, a.reachPx, 0, a.name), b)),
      Left(PlotBuildError.MarkRow("dot-plot", a.ref, 1))
    )
    assertEquals(
      rebuilt(Vector(a), good.unplotted),
      Left(PlotBuildError.RowAccounting("dot-plot", b.ref, 0))
    )
    assertEquals(
      rebuilt(good.marks, good.unplotted :+ Unplotted(b.ref, 1, NoPosition.MissingValue(xId))),
      Left(PlotBuildError.RowAccounting("dot-plot", b.ref, 2))
    )
    assertEquals(
      rebuilt(Vector(a, b.withName(a.name))),
      Left(PlotBuildError.DuplicateMarkName("dot-plot", "dot-0"))
    )
    assertEquals(
      rebuilt(Vector(b.withOrder(0), a.withOrder(1))).map(_.marks.map(_.ref)),
      Right(Vector(b.ref, a.ref)),
      "a builder chooses the order"
    )
    assertEquals(
      rebuilt(Vector(a.withOrder(1), b)),
      Left(PlotBuildError.MarkOrder("dot-plot", 0, 1))
    )
  }

  test("the accounting refuses a row in two marks, twice in one, or in none, naming it") {
    val good = right(DotPlot(xId, yId, "Three").build(three, Theme.Light))
    def rebuilt(marks: Vector[PlotMark], unplotted: Vector[Unplotted] = good.unplotted) =
      BuiltPlot(
        "dot-plot",
        three,
        good.plot,
        good.title,
        good.description,
        good.encoding,
        marks,
        unplotted
      )
    def mark(rows: Vector[MarkedRow], order: Int, n: String) =
      right(PlotMark.of("dot-plot", rows, DataPoint(0.0, 1.0), 4.0, order, name(n)))
    val both = mark(Vector(placed(p0, 0), placed(p1, 1)), 0, "both")
    // One aggregate for P0 and P1, P2 set aside: accepted.
    assertEquals(rebuilt(Vector(both)).map(_.markOf(p1)), Right(Some(both)))
    // P1 in the aggregate and in a mark of its own.
    assertEquals(
      rebuilt(Vector(both, mark(Vector(placed(p1, 1)), 1, "p1"))),
      Left(PlotBuildError.RowAccounting("dot-plot", p1, 2))
    )
    // P0 twice in one aggregate.
    assertEquals(
      rebuilt(
        Vector(
          mark(Vector(placed(p0, 0), placed(p0, 0)), 0, "twice"),
          mark(Vector(placed(p1, 1)), 1, "p1")
        )
      ),
      Left(PlotBuildError.RowAccounting("dot-plot", p0, 2))
    )
    // P2 neither drawn, represented nor set aside.
    assertEquals(
      rebuilt(Vector(both), Vector.empty),
      Left(PlotBuildError.RowAccounting("dot-plot", p2, 0))
    )
    // A represented row claims another row's index.
    assertEquals(
      rebuilt(
        Vector(mark(Vector(placed(p0, 0), MarkedRow(p1, 2, RowMarking.Represented)), 0, "x"))
      ),
      Left(PlotBuildError.MarkRow("dot-plot", p1, 2))
    )
    // An aggregate for no row cannot be made.
    assertEquals(
      PlotMark.of("dot-plot", Vector.empty, DataPoint(0.0, 0.0), 4.0, 0, name("empty")),
      Left(PlotBuildError.EmptyMark("dot-plot", "empty"))
    )
    // A reason must name a column of the source.
    val nope = right(ColumnId.of("nope"))
    assertEquals(
      rebuilt(Vector(both), Vector(Unplotted(p2, 2, NoPosition.OffScale(nope, -1.0)))),
      Left(PlotBuildError.ReasonColumn("dot-plot", p2, nope))
    )
    val positionless = PlotMark.positionless(
      p2,
      2,
      NoPosition.MissingValue(nope),
      DataPoint(2.0, 0.0),
      4.0,
      1,
      name("p2")
    )
    assertEquals(
      rebuilt(Vector(both, positionless), Vector.empty),
      Left(PlotBuildError.ReasonColumn("dot-plot", p2, nope))
    )
    List(
      PlotBuildError.RowAccounting("dot-plot", p1, 2).message   -> "Participant(P1)",
      PlotBuildError.EmptyMark("dot-plot", "empty").message     -> "'empty'",
      PlotBuildError.ReasonColumn("dot-plot", p2, nope).message -> "'nope'"
    ).foreach((m, operand) => assert(m.contains(operand) && m.contains("dot-plot"), m))
  }

  test("a row with a missing value is set aside with its reason and still listed") {
    val plot = right(DotPlot(xId, yId, "Three").build(three, Theme.Light))
    assertEquals(plot.marks.map(_.ref), Vector(p0, p1))
    assertEquals(plot.unplotted, Vector(Unplotted(p2, 2, NoPosition.MissingValue(yId))))
    assertEquals(
      plot.unplottedText(plot.unplotted.head),
      Some("PARTICIPANT P2, X 2.0, Y — not drawn: no Y")
    )
    assertEquals(plot.markOf(p2), None)
    assertEquals(
      plot.reasonText(NoPosition.OffScale(xId, -0.5)),
      "X −0.5 is off the scale"
    )
  }

  test("a placeholder draws a missing value as a positionless mark, apart from every value") {
    val plot = right(
      DotPlot(xId, yId, "Three", missing = DotPlot.MissingY.Placeholder)
        .build(three, Theme.Light)
    )
    assertEquals(plot.unplotted, Vector.empty)
    val empty = plot.markOf(p2).get
    assertEquals(
      empty.rows,
      Vector(MarkedRow(p2, 2, RowMarking.Positionless(NoPosition.MissingValue(yId))))
    )
    // Missing is not zero, nor any value: the empty ring sits below every dot,
    // at its own x, and says its row's words.
    val placedYs =
      plot.marks.flatMap(_.rows).collect { case MarkedRow(_, _, RowMarking.Placed(at)) => at.y }
    assert(placedYs.forall(_ > empty.at.y), s"${empty.at} is not below $placedYs")
    assertEquals(empty.at.x, 2.0)
    assertEquals(plot.readout(empty), three.rowText(2))
    val t = targetsOn(plot, 1.0)
    assertEquals(t.target(p2).map(_.mark), Some(empty))
    assertEquals(t.step(Some(p1), RovingMove.Next).map(_.ref), Some(p2))
  }

  test("a merged mark is partly selected by one row; a toggle adds or subtracts its rows") {
    val plot = right(
      DotPlot(xId, yId, "Three", coincident = DotPlot.Coincident.Merge)
        .build(three, Theme.Light)
    )
    val merged = plot.markOf(p1).get
    assertEquals(merged.rows, Vector(placed(p0, 0), placed(p1, 1)))
    assertEquals(plot.markOf(p0), Some(merged))
    assertEquals(
      plot.readout(merged),
      Some(
        "2 rows: PARTICIPANT P0, X 0.0, Y 1.0; PARTICIPANT P1, X 0.0, Y 1.0"
      )
    )
    val t = targetsOn(plot, 1.0)
    // P1 alone is selected elsewhere: the merged mark is partly selected.
    val (_, other) = ViewSelection
      .initial(tableView, SelectionState.empty)
      .submit(SelectionMode.Replace, Vector(p1), InputCause.Pointer)
    val partly  = bus(SelectionState.empty, Vector(other))
    val focused = MarkInputState
      .initial[StudioRef](plotView, SelectionState.empty)
      .moveFocus(Some(p0), t)
      .state
      .project(partly)
      .state
    val target = t.target(p0).get
    assertEquals(focused.share(target), SelectionShare.Partly(1, 2))
    assertEquals(focused.selectionRings(t).map(_.kind), Vector(RingKind.PartlySelected))
    assertEquals(
      t.accessibleText(focused),
      PlotText(PlotTextId.PartlySelected, plot.readout(merged).get, "1", "2")
    )
    // A toggle adds the rest of a partly selected mark.
    val toggle                                  = MarkInputEvent.Key(RovingKey.Activate(true))
    def modes(state: MarkInputState[StudioRef]) =
      right(state.handle(toggle, t, 0.0)).intents.collect { case Intent.Select(i) =>
        (i.mode, i.refs)
      }
    assertEquals(modes(focused), Vector((SelectionMode.Add, Vector(p0, p1))))
    val whole = bus(partly, right(focused.handle(toggle, t, 0.0)).intents)
    assertEquals(whole.selected.toSet, Set(p0, p1))
    // Wholly selected, a toggle subtracts every row.
    val again = focused.project(whole).state
    assertEquals(again.share(target), SelectionShare.All)
    assertEquals(modes(again), Vector((SelectionMode.Subtract, Vector(p0, p1))))
    assertEquals(bus(whole, right(again.handle(toggle, t, 0.0)).intents).selected, Vector.empty)
    // Unselected, a toggle adds every row.
    val none = focused.project(SelectionState.empty).state
    assertEquals(none.share(target), SelectionShare.Unselected)
    assertEquals(modes(none), Vector((SelectionMode.Add, Vector(p0, p1))))
  }

  test("a partly selected ring has the selection ring's token bands, dashed") {
    def lines(kind: RingKind) =
      val ring  = OverlayRing(kind, p0, DevicePoint(50.0, 50.0), 8.0)
      val scene = right(
        OverlayRings.scene(Vector(ring), OverlayPalette.onSurface(Theme.Light), 100, 100, 1.0)
      )
      scene.grobs.flatMap(_.children).collect { case p: Grob.Polygon =>
        (p.gp.stroke, p.gp.lineType)
      }
    val solid  = lines(RingKind.Selected)
    val dashed = lines(RingKind.PartlySelected)
    assertEquals(solid.map(_._2), Vector(LineType.Solid, LineType.Solid))
    assertEquals(dashed, solid.map((stroke, _) => (stroke, LineType.Dashed)))
  }

  test("nudged marks at one data point are apart for the cursor and the pointer") {
    val plot = right(
      DotPlot(xId, yId, "Three", coincident = DotPlot.Coincident.Nudge(7.0))
        .build(three, Theme.Light)
    )
    assertEquals(plot.marks.map(m => (m.ref, m.nudgePx.dxPx)), Vector(p0 -> -0.0, p1 -> 7.0))
    List(1.0, 2.0).foreach { scale =>
      val t            = targetsOn(plot, scale)
      val Vector(a, b) = t.targets: @unchecked
      assertEqualsDouble(b.anchor.x - a.anchor.x, 7.0 * scale, RoundTripTolerance)
      assertEqualsDouble(b.anchor.y, a.anchor.y, RoundTripTolerance)
      assertEquals(t.step(Some(p0), RovingMove.Right).map(_.ref), Some(p1))
      assertEquals(t.step(Some(p1), RovingMove.Left).map(_.ref), Some(p0))
      assertEquals(t.step(Some(p1), RovingMove.Right), None)
      assertEquals(right(t.pick(a.anchor, 0.5 * scale)).map(_.ref), Some(p0))
      assertEquals(right(t.pick(b.anchor, 0.5 * scale)).map(_.ref), Some(p1))
    }
    plot.marks.head.nudged("dot-plot", Double.NaN, 0.0) match
      case Left(e @ PlotBuildError.Nudge("dot-plot", "dot-0", dx, 0.0)) if dx.isNaN =>
        assert(e.message.contains("'dot-0'"), e.message)
      case other => fail(s"unexpected $other")
  }

  test("retargeting moves a focus or hover to the key of the mark that now holds its row") {
    val stacked = targetsOn(right(DotPlot(xId, yId, "Three").build(three, Theme.Light)), 1.0)
    val merged  = targetsOn(
      right(
        DotPlot(xId, yId, "Three", coincident = DotPlot.Coincident.Merge)
          .build(three, Theme.Light)
      ),
      1.0
    )
    val onP1 = right(
      MarkInputState
        .initial[StudioRef](plotView, SelectionState.empty)
        .moveFocus(Some(p1), stacked)
        .state
        .handle(MarkInputEvent.PointerMoved(stacked.target(p1).get.anchor), stacked, 0.5)
    ).state
    assertEquals((onP1.focus, onP1.hover), (Some(p1), Some(p1)))
    val moved = onP1.retarget(merged)
    assertEquals((moved.state.focus, moved.state.hover), (Some(p0), Some(p0)))
    assertEquals(moved.intents, Vector(Intent.HoverOver(plotView, Some(p0))))
    // And back: P0's mark exists on both, so nothing moves.
    val back = moved.state.retarget(stacked)
    assertEquals(
      (back.state.focus, back.state.hover, back.intents),
      (Some(p0), Some(p0), Vector.empty)
    )
  }

  test("merging joins rows only at equal x and y") {
    val stacked = right(
      PlotSource(
        "Stacked",
        three.columns,
        Vector(("P0", 1.0), ("P1", 2.0)).map((p, vy) =>
          PlotRow(
            StudioRef.Participant(p),
            Vector(PlotValue.Text(p), PlotValue.Number(0.0), PlotValue.Number(vy))
          )
        )
      )
    )
    val plot = right(
      DotPlot(xId, yId, "Stacked", coincident = DotPlot.Coincident.Merge)
        .build(stacked, Theme.Light)
    )
    assertEquals(plot.marks.map(_.refs), Vector(Vector(p0), Vector(p1)))
    assertEquals(plot.marks.map(_.at), Vector(DataPoint(0.0, 1.0), DataPoint(0.0, 2.0)))
  }

  test("a placed row must sit where the encoding puts its cells, and a reason must hold") {
    val good = right(DotPlot(xId, yId, "Three").build(three, Theme.Light))
    def rebuilt(marks: Vector[PlotMark], unplotted: Vector[Unplotted] = good.unplotted) =
      BuiltPlot(
        "dot-plot",
        three,
        good.plot,
        good.title,
        good.description,
        good.encoding,
        marks,
        unplotted
      )
    val Vector(a, b)             = good.marks: @unchecked
    def at(x: Double, y: Double) = PlotMark.placed(p0, 0, DataPoint(x, y), 4.0, 0, a.name)
    assertEquals(
      rebuilt(Vector(at(0.0, 2.0), b)),
      Left(PlotBuildError.Position("dot-plot", p0, yId, 2.0))
    )
    assertEquals(
      rebuilt(Vector(at(0.5, 1.0), b)),
      Left(PlotBuildError.Position("dot-plot", p0, xId, 0.5))
    )
    // P2 has an x of 2.0 and no y.
    assertEquals(
      rebuilt(good.marks, Vector(Unplotted(p2, 2, NoPosition.MissingValue(xId)))),
      Left(PlotBuildError.ReasonValue("dot-plot", p2, xId))
    )
    assertEquals(
      rebuilt(good.marks, Vector(Unplotted(p2, 2, NoPosition.OffScale(yId, -1.0)))),
      Left(PlotBuildError.ReasonValue("dot-plot", p2, yId))
    )
    assertEquals(
      rebuilt(good.marks, Vector(Unplotted(p2, 2, NoPosition.OffScale(xId, 3.0)))),
      Left(PlotBuildError.ReasonValue("dot-plot", p2, xId))
    )
    assert(rebuilt(good.marks, Vector(Unplotted(p2, 2, NoPosition.OffScale(xId, 2.0)))).isRight)
    val positionless =
      PlotMark.positionless(
        p2,
        2,
        NoPosition.MissingValue(xId),
        DataPoint(2.0, 0.0),
        4.0,
        2,
        name("p2")
      )
    assertEquals(
      rebuilt(good.marks :+ positionless, Vector.empty),
      Left(PlotBuildError.ReasonValue("dot-plot", p2, xId))
    )
    val nope = right(ColumnId.of("nope"))
    assertEquals(
      BuiltPlot(
        "dot-plot",
        three,
        good.plot,
        good.title,
        good.description,
        PositionEncoding(Axis.Numeric(nope, AxisScale.Linear), good.encoding.y),
        good.marks,
        good.unplotted
      ),
      Left(PlotBuildError.MissingColumn("dot-plot", nope))
    )
    List(
      PlotBuildError.Position("dot-plot", p0, yId, 2.0).message -> "'y'",
      PlotBuildError.ReasonValue("dot-plot", p2, xId).message   -> "'x'"
    ).foreach((m, operand) => assert(m.contains(operand) && m.contains("Participant"), m))
  }

  /** σ per participant: P2's σ of zero has no place on a log axis. */
  private val sigmas = right(
    PlotSource(
      "Sigma",
      Vector(
        column("participant", ColumnFormat.Label),
        column("sigma", ColumnFormat.Decimal(2)),
        column("y", ColumnFormat.Decimal(1))
      ),
      Vector(("P0", 0.5), ("P1", 20.0), ("P2", 0.0)).map((p, sigma) =>
        PlotRow(
          StudioRef.Participant(p),
          Vector(PlotValue.Text(p), PlotValue.Number(sigma), PlotValue.Number(1.0))
        )
      )
    )
  )
  private val sigmaId = right(ColumnId.of("sigma"))

  test("a log axis places a row at the log of its value, and zero is off the scale") {
    val encoding =
      PositionEncoding(
        Axis.Numeric(sigmaId, AxisScale.Log10),
        Axis.Numeric(yId, AxisScale.Linear)
      )
    val plot = right(EncodedTestPlot(encoding).build(sigmas, Theme.Light))
    assertEquals(
      plot.marks.flatMap(_.rows).map(r => (r.ref, r.marking)),
      Vector(
        p0 -> RowMarking.Placed(DataPoint(math.log10(0.5), 1.0)),
        p1 -> RowMarking.Placed(DataPoint(math.log10(20.0), 1.0))
      )
    )
    assertEquals(plot.unplotted, Vector(Unplotted(p2, 2, NoPosition.OffScale(sigmaId, 0.0))))
    assertEquals(plot.reasonText(plot.unplotted.head.reason), "SIGMA 0.00 is off the scale")
    // Placed at the value itself, not its log: refused.
    val m = plot.marks.head
    assertEquals(
      BuiltPlot(
        "encoded-test",
        sigmas,
        plot.plot,
        plot.title,
        plot.description,
        encoding,
        PlotMark.placed(p0, 0, DataPoint(0.5, 1.0), m.reachPx, 0, m.name) +: plot.marks.tail,
        plot.unplotted
      ),
      Left(PlotBuildError.Position("encoded-test", p0, sigmaId, 0.5))
    )
  }

  test("a category axis places a row at its level's index, and refuses a repeated level") {
    val participant               = right(ColumnId.of("participant"))
    def encoding(levels: String*) =
      PositionEncoding(
        Axis.Category(participant, levels.toVector),
        Axis.Numeric(yId, AxisScale.Linear)
      )
    val plot = right(EncodedTestPlot(encoding("P2", "P0", "P1")).build(sigmas, Theme.Light))
    assertEquals(
      plot.marks.map(_.at),
      Vector(DataPoint(1.0, 1.0), DataPoint(2.0, 1.0), DataPoint(0.0, 1.0))
    )
    val m                                                     = plot.marks.head
    def rebuilt(e: PositionEncoding, marks: Vector[PlotMark]) =
      BuiltPlot(
        "encoded-test",
        sigmas,
        plot.plot,
        plot.title,
        plot.description,
        e,
        marks,
        Vector.empty
      )
    assertEquals(
      rebuilt(
        encoding("P2", "P0", "P1"),
        PlotMark.placed(p0, 0, DataPoint(0.0, 1.0), m.reachPx, 0, m.name) +: plot.marks.tail
      ),
      Left(PlotBuildError.Position("encoded-test", p0, participant, 0.0))
    )
    assertEquals(
      rebuilt(encoding("P2", "P0", "P1", "P0"), plot.marks),
      Left(PlotBuildError.DuplicateLevel("encoded-test", participant, "P0"))
    )
    // A row whose level is not listed cannot be placed there.
    assertEquals(
      rebuilt(encoding("P2", "P1"), plot.marks),
      Left(PlotBuildError.Position("encoded-test", p0, participant, 1.0))
    )
  }

  // The bus after applying `intents`' selection inputs.
  private def bus(state: SelectionState, intents: Vector[Intent]): SelectionState =
    intents.foldLeft(state) {
      case (s, Intent.Select(input)) => right(s.submit(input))
      case (s, _)                    => s
    }

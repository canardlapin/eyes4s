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
import eyes4s.studio.core.selection.{
  InputCause,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}
import intaglio.interaction.NamedPicking
import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}

/** A plot and its TableTwin read one value source (S4.5a).
  *
  * For generated sources: every row is a mark or an unplotted row exactly
  * once; a mark sits at its row's numbers, and writing its position in the
  * column's format gives the table's cell text; the drawn marks, resolved
  * through the plot's transform on a 1x and a 2x surface, map back to those
  * numbers and pick their own row; a focused mark says what its row says.
  * Selecting a row selects its mark, and selecting a mark its row, through
  * the one bus.
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

  private def genValue(format: ColumnFormat): Gen[PlotValue] =
    val number = format match
      case ColumnFormat.Count => Gen.choose(-50000, 50000).map(_.toDouble)
      case _                  =>
        Gen.oneOf(
          Gen.choose(-1.0, 1.0),
          Gen.choose(-1000.0, 1000.0),
          Gen.choose(-3, 3).map(_ / 4.0)
        )
    Gen.frequency(9 -> number.map(PlotValue.Number(_)), 1 -> Gen.const(PlotValue.Missing))

  /** A source: a label column, then two to four numeric columns; up to 40 rows. */
  private val genSource: Gen[PlotSource] =
    for
      formats <- Gen.choose(2, 4).flatMap(Gen.listOfN(_, genFormat))
      columns = column("participant", ColumnFormat.Label) +:
        formats.zipWithIndex.map((f, i) => column(s"v$i", f)).toVector
      n    <- Gen.choose(0, 40)
      rows <- Gen.listOfN(
        n,
        Gen.sequence[Vector[PlotValue], PlotValue](formats.map(genValue))
      )
    yield right(
      PlotSource(
        "Generated",
        columns,
        rows.zipWithIndex.map { (vs, i) =>
          PlotRow(StudioRef.Participant(s"P$i"), PlotValue.Text(s"P$i") +: vs)
        }.toVector
      )
    )

  private def dots(source: PlotSource): DotPlot =
    DotPlot(source.columns(1).id, source.columns(2).id, "Generated")

  private def targetsOn(plot: BuiltPlot, scale: Double): PlotTargets =
    val surface   = right(PlotSurface(640, 400, scale))
    val transform = right(PlotTransform.resolve(plot.plot, surface))
    val picking   = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
    right(PlotTargets.resolve(plot, transform, picking))

  property("every row is drawn or set aside once, and marks sit at the table's values") {
    Prop.forAll(genSource) { source =>
      val plot     = right(dots(source).build(source, Theme.Light))
      val table    = TableTwinState.initial(tableView, SelectionState.empty).vm(source)
      val (xi, yi) = (1, 2)
      assertEquals(table.rows.map(_.ref), source.rows.map(_.ref))
      assertEquals(
        (plot.marks.map(_.ref) ++ plot.unplotted.map(_.ref)).sortBy(r => source.rowOf(r)),
        source.rows.map(_.ref)
      )
      plot.marks.foreach { m =>
        val row = table.rows(m.row)
        assertEquals(row.ref, m.ref)
        assertEquals(
          PlotSource.write(PlotValue.Number(m.at.x), source.columns(xi).format),
          row.cells(xi).text
        )
        assertEquals(
          PlotSource.write(PlotValue.Number(m.at.y), source.columns(yi).format),
          row.cells(yi).text
        )
        assertEquals(Some(m.at.x), source.number(m.row, source.columns(xi).id))
        assertEquals(Some(m.at.y), source.number(m.row, source.columns(yi).id))
        assertEquals(plot.readout(m), Some(row.accessibleText))
      }
      plot.unplotted.foreach { u =>
        val cells = table.rows(u.row).cells
        assert(
          cells(xi).text == PlotSource.MissingText || cells(yi).text == PlotSource.MissingText,
          s"${u.ref} is not drawn but the table shows ${cells.map(_.text)}"
        )
      }
      assertEquals(plot.marks.map(_.order), plot.marks.indices.toVector)
    }
  }

  property("drawn marks map back to the table's values and pick their own row, at 1x and 2x") {
    Prop.forAll(genSource) { source =>
      val plot = right(dots(source).build(source, Theme.Dark))
      List(1.0, 2.0).foreach { scale =>
        val t = targetsOn(plot, scale)
        assertEquals(t.targets.map(_.mark), plot.marks)
        t.targets.foreach { target =>
          val back = t.transform.deviceToData(target.anchor)
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

  property("selecting a row selects its mark, and selecting a mark its row") {
    val cases = for
      source <- genSource
      plot = right(dots(source).build(source, Theme.Light))
      if plot.marks.nonEmpty
      i <- Gen.choose(0, plot.marks.size - 1)
    yield (source, plot, plot.marks(i))
    Prop.forAll(cases) { (source, plot, mark) =>
      val t            = targetsOn(plot, 1.0)
      val initialPlot  = MarkInputState.initial[StudioRef](plotView, SelectionState.empty)
      val initialTable = TableTwinState.initial(tableView, SelectionState.empty)

      // Row -> mark: Enter on the table's cursor row.
      val onRow   = initialTable.moveCursor(Some(mark.ref), source).state
      val byTable =
        bus(SelectionState.empty, onRow.handle(RovingKey.Activate(false), source).intents)
      val plotSees = initialPlot.project(byTable)
      assertEquals(plotSees.intents, Vector.empty)
      assertEquals(
        plotSees.state.selectionRings(t).map(r => (r.ref, r.centre)),
        Vector((mark.ref, t.target(mark.ref).get.anchor))
      )
      assertEquals(
        onRow.project(byTable).state.vm(source).rows.map(_.selected),
        selectedAt(source, mark)
      )

      // Mark -> row: Enter on the plot's focused mark.
      val onMark = initialPlot.moveFocus(Some(mark.ref), t).state
      val byPlot = bus(
        SelectionState.empty,
        right(onMark.handle(MarkInputEvent.Key(RovingKey.Activate(false)), t, 0.0)).intents
      )
      val tableSees = initialTable.project(byPlot)
      assertEquals(tableSees.intents, Vector.empty)
      assertEquals(tableSees.state.vm(source).rows.map(_.selected), selectedAt(source, mark))
      assertEquals(onMark.project(byPlot).state.selectionRings(t).map(_.ref), Vector(mark.ref))

      // The focused mark and the cursor row say the same, selected or not.
      val cursorRow = onRow.project(byPlot).state.vm(source)
      assertEquals(t.accessibleText(onMark.project(byPlot).state), cursorRow.accessibleText)
      assertEquals(t.accessibleText(onMark), onRow.vm(source).accessibleText)
    }
  }

  property("a selection of several marks rings every one, and the table marks every row") {
    val cases = for
      source <- genSource
      plot = right(dots(source).build(source, Theme.Light))
      if plot.marks.size >= 2
      two  <- Gen.pick(2, plot.marks)
      more <- Gen.someOf(plot.marks)
    yield (source, plot, (two.toVector ++ more).distinct)
    Prop.forAll(cases) { (source, plot, picked) =>
      val t           = targetsOn(plot, 1.0)
      val refs        = picked.map(_.ref)
      val (_, intent) = ViewSelection
        .initial(tableView, SelectionState.empty)
        .submit(SelectionMode.Replace, refs, InputCause.Pointer)
      val selected = bus(SelectionState.empty, Vector(intent))
      val rings    = MarkInputState
        .initial[StudioRef](plotView, SelectionState.empty)
        .project(selected)
        .state
        .selectionRings(t)
      assertEquals(rings.map(r => (r.kind, r.ref)), refs.map((RingKind.Selected, _)))
      assertEquals(rings.map(_.centre), refs.map(r => t.target(r).get.anchor))
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

  test("a built plot refuses marks that misstate their rows, twice-drawn rows and misorder") {
    val source = right(
      PlotSource(
        "Two",
        Vector(
          column("participant", ColumnFormat.Label),
          column("x", ColumnFormat.Decimal(1)),
          column("y", ColumnFormat.Decimal(1))
        ),
        Vector(0, 1).map(i =>
          PlotRow(
            StudioRef.Participant(s"P$i"),
            Vector(PlotValue.Text(s"P$i"), PlotValue.Number(i.toDouble), PlotValue.Number(1.0))
          )
        )
      )
    )
    val good = right(
      DotPlot(right(ColumnId.of("x")), right(ColumnId.of("y")), "Two")
        .build(source, Theme.Light)
    )
    def rebuilt(marks: Vector[PlotMark], unplotted: Vector[Unplotted] = Vector.empty) =
      BuiltPlot("dot-plot", source, good.plot, good.title, good.description, marks, unplotted)
    val Vector(a, b) = good.marks: @unchecked
    assert(rebuilt(good.marks).isRight)
    assertEquals(
      rebuilt(Vector(a.copy(row = 1), b)),
      Left(PlotBuildError.MarkRow("dot-plot", a.ref, 1))
    )
    assertEquals(
      rebuilt(Vector(a), Vector.empty),
      Left(PlotBuildError.RowAccounting("dot-plot", b.ref, 0))
    )
    assertEquals(
      rebuilt(
        good.marks,
        Vector(Unplotted(b.ref, 1, UnplottedReason.MissingValue(right(ColumnId.of("x")))))
      ),
      Left(PlotBuildError.RowAccounting("dot-plot", b.ref, 2))
    )
    assertEquals(
      rebuilt(Vector(a, b.copy(name = a.name))),
      Left(PlotBuildError.DuplicateMarkName("dot-plot", "dot-0"))
    )
    assertEquals(
      rebuilt(Vector(b.copy(order = 0), a.copy(order = 1))).map(_.marks.map(_.ref)),
      Right(Vector(b.ref, a.ref)),
      "a builder chooses the order"
    )
    assertEquals(
      rebuilt(Vector(a.copy(order = 1), b)),
      Left(PlotBuildError.MarkOrder("dot-plot", 0, 1))
    )
  }

  test("a row with a missing value is set aside with its reason and still listed") {
    val source = right(
      PlotSource(
        "Missing",
        Vector(
          column("participant", ColumnFormat.Label),
          column("x", ColumnFormat.Decimal(1)),
          column("y", ColumnFormat.Decimal(1))
        ),
        Vector(
          PlotRow(
            StudioRef.Participant("P1"),
            Vector(PlotValue.Text("P1"), PlotValue.Number(0.0), PlotValue.Missing)
          ),
          PlotRow(
            StudioRef.Participant("P2"),
            Vector(PlotValue.Text("P2"), PlotValue.Number(0.0), PlotValue.Number(0.0))
          )
        )
      )
    )
    val plot = right(
      DotPlot(right(ColumnId.of("x")), right(ColumnId.of("y")), "Missing")
        .build(source, Theme.Light)
    )
    assertEquals(plot.marks.map(_.ref), Vector(StudioRef.Participant("P2")))
    assertEquals(
      plot.unplotted,
      Vector(
        Unplotted(
          StudioRef.Participant("P1"),
          0,
          UnplottedReason.MissingValue(right(ColumnId.of("y")))
        )
      )
    )
    assertEquals(
      plot.unplottedText(plot.unplotted.head),
      Some("PARTICIPANT P1, X 0.0, Y — not drawn: no Y")
    )
    // Missing is not zero: P1 has no mark at y = 0, where P2 is drawn.
    assertEquals(plot.markOf(StudioRef.Participant("P1")), None)
  }

  private def selectedAt(source: PlotSource, mark: PlotMark): Vector[Boolean] =
    source.rows.map(_.ref == mark.ref)

  // The bus after applying `intents`' selection inputs.
  private def bus(state: SelectionState, intents: Vector[Intent]): SelectionState =
    intents.foldLeft(state) {
      case (s, Intent.Select(input)) => right(s.submit(input))
      case (s, _)                    => s
    }

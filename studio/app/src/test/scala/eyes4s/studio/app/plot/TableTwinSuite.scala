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

package eyes4s.studio.app.plot

import eyes4s.studio.app.Intent
import eyes4s.studio.core.selection.{
  InputCause,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}

/** A TableTwin's row cursor (S4.5a): one focus stop, arrows and Home/End move
  * the cursor, Enter selects and Escape clears through stamped intents, a
  * click moves the cursor and selects, a projected selection emits nothing,
  * and the view-model writes every row as the source does.
  */
class TableTwinSuite extends munit.FunSuite:

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val view = right(ViewId.of("compare.participant-plot.table"))

  private val source = right(
    PlotSource(
      "Participant D",
      Vector(
        PlotColumn(right(ColumnId.of("participant")), "Participant", ColumnFormat.Label),
        PlotColumn(right(ColumnId.of("d")), "Mean D", ColumnFormat.Signed(2))
      ),
      (1 to 4).toVector.map(i =>
        PlotRow(
          StudioRef.Participant(s"P$i"),
          Vector(PlotValue.Text(s"P$i"), PlotValue.Number(i / 10.0))
        )
      )
    )
  )
  private def ref(i: Int): StudioRef = source.rows(i).ref

  private def key(s: TableTwinState, moves: RovingMove*): TableTwinState =
    moves.foldLeft(s)((acc, m) => acc.handle(RovingKey.Move(m), source).state)

  private val initial = TableTwinState.initial(view, SelectionState.empty)

  test("arrows move the row cursor, clamped at both ends; Left and Right stay") {
    assertEquals(key(initial, RovingMove.Down).cursor, Some(ref(0)))
    assertEquals(key(initial, RovingMove.Last).cursor, Some(ref(3)))
    assertEquals(
      key(initial, RovingMove.Down, RovingMove.Down, RovingMove.Next).cursor,
      Some(ref(2))
    )
    val atLast = key(initial, RovingMove.Last)
    val stayed = atLast.handle(RovingKey.Move(RovingMove.Down), source)
    assertEquals((stayed.state.cursor, stayed.changed), (Some(ref(3)), false))
    val left = atLast.handle(RovingKey.Move(RovingMove.Left), source)
    assertEquals((left.state.cursor, left.changed), (Some(ref(3)), false))
    assertEquals(key(atLast, RovingMove.Up, RovingMove.Previous).cursor, Some(ref(1)))
    assertEquals(key(atLast, RovingMove.First).cursor, Some(ref(0)))
  }

  test("Enter selects the cursor row, Shift+Enter toggles, Escape clears: stamped intents") {
    val at1   = key(initial, RovingMove.Down, RovingMove.Down)
    val enter = at1.handle(RovingKey.Activate(false), source)
    enter.intents match
      case Vector(Intent.Select(input)) =>
        assertEquals(input.mode, SelectionMode.Replace)
        assertEquals(input.refs, Vector(ref(1)))
        assertEquals(input.stamp.origin, view)
        assertEquals(input.stamp.cause, InputCause.Keyboard)
        assertEquals(input.stamp.sequence, 0L)
      case other => fail(s"unexpected $other")
    // Not selected until the bus projects it back.
    assert(!enter.state.vm(source).rows(1).selected)
    val toggle = enter.state.handle(RovingKey.Activate(true), source)
    toggle.intents match
      case Vector(Intent.Select(input)) =>
        assertEquals((input.mode, input.stamp.sequence), (SelectionMode.Toggle, 1L))
      case other => fail(s"unexpected $other")
    toggle.state.handle(RovingKey.Clear, source).intents match
      case Vector(Intent.Select(input)) => assertEquals(input.mode, SelectionMode.Clear)
      case other                        => fail(s"unexpected $other")
    assertEquals(initial.handle(RovingKey.Activate(false), source).intents, Vector.empty)
  }

  test("a click moves the cursor there and selects the row with pointer cause") {
    val step = initial.click(ref(2), toggle = false, source)
    assertEquals(step.state.cursor, Some(ref(2)))
    assert(step.changed)
    step.intents match
      case Vector(Intent.Select(input)) =>
        assertEquals((input.refs, input.stamp.cause), (Vector(ref(2)), InputCause.Pointer))
      case other => fail(s"unexpected $other")
    val unknown = initial.click(StudioRef.Participant("P99"), toggle = false, source)
    assertEquals((unknown.intents, unknown.changed), (Vector.empty, false))
  }

  test("a projected selection marks rows selected and emits nothing") {
    val (selected, _) = submitted(initial.click(ref(3), toggle = false, source))
    val step          = initial.project(selected)
    assertEquals(step.intents, Vector.empty)
    assert(step.changed)
    val vm = step.state.vm(source)
    assertEquals(vm.rows.map(_.selected), Vector(false, false, false, true))
    assertEquals(
      vm.rows(3).accessibleText,
      PlotText.selected(source.rowTextOf(source.rows(3)), true)
    )
    // Projection keeps the view's sequence past what the bus applied.
    val again = step.state.click(ref(0), toggle = false, source)
    again.intents match
      case Vector(Intent.Select(input)) => assertEquals(input.stamp.sequence, 1L)
      case other                        => fail(s"unexpected $other")
  }

  test("the view-model writes every cell with the source's formatter") {
    val vm = key(initial, RovingMove.Down).vm(source)
    assertEquals(
      vm.headers.map(h => (h.text, h.numeric)),
      Vector(("Participant", false), ("Mean D", true))
    )
    assertEquals(
      vm.rows.map(_.cells.map(_.text)),
      source.rows.map(source.cellsOf)
    )
    assertEquals(vm.cursorRow, Some(0))
    assertEquals(vm.accessibleText, source.rowTextOf(source.rows(0)))
    assertEquals(
      initial.vm(source).accessibleText,
      PlotText(PlotTextId.TableKeys, "Participant D")
    )
  }

  test("carrying a mark keeps a cursor already on one of its rows, else goes to the first") {
    val mark   = Vector(ref(1), ref(2))
    val onRow2 = initial.moveCursor(Some(ref(2)), source).state
    val kept   = onRow2.carry(mark, source)
    assertEquals(
      (kept.state.cursor, kept.changed, kept.intents),
      (Some(ref(2)), false, Vector.empty)
    )
    assertEquals(initial.carry(mark, source).state.cursor, Some(ref(1)))
    val elsewhere = initial.moveCursor(Some(ref(3)), source).state.carry(mark, source)
    assertEquals((elsewhere.state.cursor, elsewhere.changed), (Some(ref(1)), true))
    assertEquals(onRow2.carry(Vector.empty, source).state.cursor, Some(ref(2)))
  }

  test("moveCursor and retarget keep the cursor on listed rows only") {
    val carried = initial.moveCursor(Some(ref(2)), source)
    assertEquals((carried.state.cursor, carried.intents), (Some(ref(2)), Vector.empty))
    assertEquals(initial.moveCursor(Some(StudioRef.Participant("P99")), source).changed, false)
    val fewer = right(PlotSource(source.caption, source.columns, source.rows.take(2)))
    assertEquals(carried.state.retarget(fewer).state.cursor, None)
    assertEquals(carried.state.retarget(source).changed, false)
  }

  // The bus's state after the step's one intent.
  private def submitted(step: TableTwinStep): (SelectionState, TableTwinState) =
    step.intents match
      case Vector(Intent.Select(input)) =>
        (right(SelectionState.empty.submit(input)), step.state)
      case other => fail(s"unexpected $other")

  // --- Content-sized columns ------------------------------------------------------------

  private def labelled(n: Int, label: Int => String, header: String = "Participant") =
    right(
      PlotSource(
        "Rows",
        Vector(
          PlotColumn(right(ColumnId.of("label")), header, ColumnFormat.Label),
          PlotColumn(right(ColumnId.of("d")), "D", ColumnFormat.Signed(2))
        ),
        Vector.tabulate(n)(i =>
          PlotRow(
            StudioRef.Participant(s"P$i"),
            Vector(PlotValue.Text(label(i)), PlotValue.Number(i / 10.0))
          )
        )
      )
    )

  test("columns share the width by their widest text, header or cell, within bounds") {
    // "Participant" (11) against "Mean D"'s widest "+0.40" (6): 11 : 6.
    val shares = TableColumns.shares(source)
    assertEqualsDouble(shares.sum, 1.0, 1e-12)
    assertEqualsDouble(shares(0), 11.0 / 17.0, 1e-12)
    // A cell wider than its header widens its column; the widest counts as 32.
    val wide = TableColumns.shares(labelled(3, i => "x" * (10 + 30 * i)))
    assertEqualsDouble(wide(0), 32.0 / (32.0 + 5.0), 1e-12)
    // A narrow column counts as 4.
    val narrow = TableColumns.shares(labelled(2, _ => "a", header = "L"))
    assertEqualsDouble(narrow(0), 4.0 / (4.0 + 5.0), 1e-12)
    assertEquals(TableColumns.shares(labelled(0, _ => "")).size, 2)
  }

  test("a long source is sized from a bounded sample that keeps its first and last rows") {
    val n      = 10000
    val picked = TableColumns.sampled(labelled(n, _ => "a"))
    assertEquals(picked.size, TableColumns.Sample)
    assertEquals((picked.head, picked.last), (0, n - 1))
    assertEquals(picked, picked.distinct.sorted)
    // A wide row the sample skips does not size the column; the last row does.
    val skipped = (0 until n).find(i => !picked.contains(i)).get
    val missed  = labelled(n, i => if i == skipped then "x" * 30 else "a")
    // "D" is at widest "+999.90" (7), at the last row.
    assertEqualsDouble(TableColumns.shares(missed)(0), 11.0 / 18.0, 1e-12)
    val last = labelled(n, i => if i == n - 1 then "x" * 30 else "a")
    assertEqualsDouble(TableColumns.shares(last)(0), 30.0 / 37.0, 1e-12)
    assertEquals(TableColumns.sampled(labelled(5, _ => "a")), Vector.range(0, 5))
  }

  // --- Pinned · selected (bead bd-01M44PD9XEHG6QWPCJATAJZ15W) ----------------------------

  private val pinning = TableTwinState.initial(view, SelectionState.empty, pinsSelected = true)

  // The bus's selection after toggling `rows` in, each as a table click.
  private def selected(src: PlotSource, rows: Seq[Int]): SelectionState =
    rows.foldLeft(SelectionState.empty) { (acc, r) =>
      TableTwinState.initial(view, acc).click(src.rows(r).ref, toggle = true, src).intents match
        case Vector(Intent.Select(input)) => right(acc.submit(input))
        case other                        => fail(s"unexpected $other")
    }

  private def selecting(s: TableTwinState, rows: Int*): TableTwinState =
    s.project(selected(source, rows)).state

  private def shownRefs(s: TableTwinState): Vector[StudioRef] = s.vm(source).rows.map(_.ref)

  test("a pinning table shows a selected row once, at the top, annotated; others in order") {
    val p2 = selecting(pinning, 2)
    assertEquals(shownRefs(p2), Vector(ref(2), ref(0), ref(1), ref(3)))
    val rows = p2.vm(source).rows
    assertEquals(rows.map(_.pinned), Vector(true, false, false, false))
    assertEquals(rows.head.annotation, Some("pinned · selected"))
    assert(rows.head.accessibleText.endsWith(", pinned · selected"), rows.head.accessibleText)
    assertEquals(rows(1).annotation, None)
    // Two selected rows are pinned in source order.
    assertEquals(shownRefs(selecting(pinning, 3, 1)), Vector(ref(1), ref(3), ref(0), ref(2)))
    // Deselecting returns the row to its place.
    assertEquals(shownRefs(p2.project(SelectionState.empty).state), source.rows.map(_.ref))
    // A table that does not pin keeps source order.
    assertEquals(shownRefs(selecting(initial, 2)), source.rows.map(_.ref))
    assertEquals(selecting(initial, 2).vm(source).rows.map(_.pinned), Vector.fill(4)(false))
  }

  test("the cursor moves in shown order, and the pinned row is row 0") {
    val p2 = selecting(pinning, 2)
    assertEquals(key(p2, RovingMove.Down).cursor, Some(ref(2)))
    assertEquals(key(p2, RovingMove.Down, RovingMove.Down).cursor, Some(ref(0)))
    assertEquals(key(p2, RovingMove.Last, RovingMove.Up).cursor, Some(ref(1)))
    val onPinned = key(p2, RovingMove.Down)
    assertEquals(onPinned.vm(source).cursorRow, Some(0))
    assertEquals(key(p2, RovingMove.Last).vm(source).cursorRow, Some(3))
    assertEquals(p2.shownIndex(source, ref(0)), Some(1))
    assertEquals(p2.sourceIndex(source, 0), Some(2))
  }

  test("shown and source positions are inverse for any selection") {
    val random = scala.util.Random(17L)
    (1 to 200).foreach { _ =>
      val n      = random.nextInt(40)
      val picked = Vector.fill(random.nextInt(8))(random.nextInt(40)).filter(_ < n).distinct
      val src    = labelled(n, i => s"P$i")
      val st     = TableTwinState.initial(view, selected(src, picked), pinsSelected = true)
      val shown  = Vector.range(0, n).flatMap(st.sourceIndex(src, _))
      assertEquals(shown.sorted, Vector.range(0, n))
      assertEquals(shown.take(picked.size), picked.sorted)
      assertEquals(shown.drop(picked.size), Vector.range(0, n).filterNot(picked.contains))
      shown.zipWithIndex.foreach((r, i) =>
        assertEquals(st.shownIndex(src, src.rows(r).ref), Some(i))
      )
      assertEquals(st.sourceIndex(src, n), None)
    }
  }

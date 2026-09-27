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
    assertEquals(vm.rows(3).accessibleText, PlotText.selected(source.rowText(3), true))
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
      source.rows.indices.map(source.cells).toVector
    )
    assertEquals(vm.cursorRow, Some(0))
    assertEquals(vm.accessibleText, source.rowText(0))
    assertEquals(
      initial.vm(source).accessibleText,
      PlotText(PlotTextId.TableKeys, "Participant D")
    )
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

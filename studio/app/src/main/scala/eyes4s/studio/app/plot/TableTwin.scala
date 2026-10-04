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

/** A column header of a TableTwin. Numeric columns are right-aligned. */
final case class TableHeaderVM(text: String, numeric: Boolean) derives CanEqual

/** One cell as the table shows it. */
final case class TableCellVM(text: String, numeric: Boolean) derives CanEqual

/** One row: its ref, its cells, whether it is selected or under the cursor,
  * and what a screen reader says for it.
  */
final case class TableRowVM(
    ref: StudioRef,
    cells: Vector[TableCellVM],
    selected: Boolean,
    cursor: Boolean,
    accessibleText: String
) derives CanEqual

/** What a TableTwin shows (ticket S4.5a): every row of the plot's source, in
  * the source's order, written by [[PlotSource.text]].
  */
final case class TableTwinVM(
    caption: String,
    headers: Vector[TableHeaderVM],
    rows: Vector[TableRowVM],
    cursorRow: Option[Int],
    accessibleText: String
) derives CanEqual

/** What an input did to a table: the next state, the intents to dispatch, and
  * whether its view-model changed.
  */
final case class TableTwinStep(state: TableTwinState, intents: Vector[Intent], changed: Boolean)

/** A TableTwin's input state (ticket S4.5a): the row cursor, keyboard focus
  * and the selection as last projected from the bus.
  *
  * The table is one focus stop with an arrow-key row cursor (DESIGN_SPEC
  * sections 10 and 12). Enter selects the cursor row and Escape clears, as
  * in the plot; a row is selected only when the bus projects it back, so
  * the plot's mark and the table's row always show one selection.
  */
final case class TableTwinState private (
    selection: ViewSelection,
    cursor: Option[StudioRef],
    focused: Boolean
) derives CanEqual:

  def view: ViewId = selection.view

  /** Applies one key to the rows of `source`. */
  def handle(key: RovingKey, source: PlotSource): TableTwinStep =
    key match
      case RovingKey.Move(move) =>
        step(source, move).fold(unchanged)(ref =>
          TableTwinStep(copy(cursor = Some(ref)), Vector.empty, true)
        )
      case RovingKey.Activate(toggle) =>
        cursor
          .filter(source.rowOf(_).isDefined)
          .fold(unchanged)(ref => choose(ref, toggle, InputCause.Keyboard))
      case RovingKey.Clear => submit(SelectionMode.Clear, Vector.empty, InputCause.Keyboard)

  /** A primary click on the row of `ref`: the cursor moves there and the row
    * is selected (`toggle` adds or removes it).
    */
  def click(ref: StudioRef, toggle: Boolean, source: PlotSource): TableTwinStep =
    if source.rowOf(ref).isEmpty then unchanged
    else
      val step = copy(cursor = Some(ref)).choose(ref, toggle, InputCause.Pointer)
      step.copy(changed = !cursor.contains(ref))

  /** Puts the cursor on `ref` without selecting, as when the plot's focused
    * mark is carried to the table.
    */
  def moveCursor(ref: Option[StudioRef], source: PlotSource): TableTwinStep =
    val next = ref.filter(source.rowOf(_).isDefined)
    if next.isEmpty || next == cursor then unchanged
    else TableTwinStep(copy(cursor = next), Vector.empty, true)

  /** Carries the plot's focused mark to the table: the cursor stays when it
    * is already on one of the mark's rows `refs`, and otherwise goes to the
    * first of them.
    */
  def carry(refs: Vector[StudioRef], source: PlotSource): TableTwinStep =
    if cursor.exists(refs.contains) && cursor.flatMap(source.rowOf).isDefined then unchanged
    else moveCursor(refs.headOption, source)

  /** The table gained or lost keyboard focus. */
  def focusChanged(now: Boolean): TableTwinStep =
    TableTwinStep(copy(focused = now), Vector.empty, now != focused)

  /** The selection as the bus now holds it: emits nothing. */
  def project(selection: SelectionState): TableTwinStep =
    val (next, changed) = this.selection.project(selection)
    TableTwinStep(copy(selection = next), Vector.empty, changed)

  /** The same state on another source: a cursor on a row it lacks is dropped. */
  def retarget(source: PlotSource): TableTwinStep =
    val kept = cursor.filter(source.rowOf(_).isDefined)
    TableTwinStep(copy(cursor = kept), Vector.empty, kept != cursor)

  /** The view-model of `source` under this state. */
  def vm(source: PlotSource): TableTwinVM =
    val headers = source.columns.map(c => TableHeaderVM(c.header, c.format.numeric))
    val rows    = source.rows.map { row =>
      val selected = selection.isSelected(row.ref)
      TableRowVM(
        row.ref,
        source.cellsOf(row).zip(source.columns).map((t, c) => TableCellVM(t, c.format.numeric)),
        selected,
        cursor.contains(row.ref),
        PlotText.selected(source.rowTextOf(row), selected)
      )
    }
    val cursorRow = cursor.flatMap(source.rowOf)
    val spoken    = cursorRow.flatMap(rows.lift) match
      case Some(r) => r.accessibleText
      case None    =>
        if rows.isEmpty then PlotText(PlotTextId.TableEmpty, source.caption)
        else PlotText(PlotTextId.TableKeys, source.caption)
    TableTwinVM(source.caption, headers, rows, cursorRow, spoken)

  private def unchanged: TableTwinStep = TableTwinStep(this, Vector.empty, false)

  // The row the cursor goes to, or None if it stays. With no cursor every
  // move starts at the first row (Last at the last).
  private def step(source: PlotSource, move: RovingMove): Option[StudioRef] =
    val refs = source.rows.map(_.ref)
    cursor.flatMap(source.rowOf) match
      case None =>
        move match
          case RovingMove.Left | RovingMove.Right => None
          case RovingMove.Last                    => refs.lastOption
          case _                                  => refs.headOption
      case Some(i) =>
        val to = move match
          case RovingMove.Up | RovingMove.Previous => i - 1
          case RovingMove.Down | RovingMove.Next   => i + 1
          case RovingMove.First                    => 0
          case RovingMove.Last                     => refs.size - 1
          case RovingMove.Left | RovingMove.Right  => i
        if to == i then None else refs.lift(to)

  private def choose(ref: StudioRef, toggle: Boolean, cause: InputCause): TableTwinStep =
    submit(if toggle then SelectionMode.Toggle else SelectionMode.Replace, Vector(ref), cause)

  private def submit(mode: SelectionMode, refs: Vector[StudioRef], cause: InputCause) =
    val (next, intent) = selection.submit(mode, refs, cause)
    TableTwinStep(copy(selection = next), Vector(intent), false)

object TableTwinState:

  /** A table with no cursor, showing `selection`. */
  def initial(view: ViewId, selection: SelectionState): TableTwinState =
    TableTwinState(ViewSelection.initial(view, selection), None, false)

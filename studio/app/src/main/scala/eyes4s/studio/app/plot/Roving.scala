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
  ContextRevision,
  InputCause,
  InputStamp,
  SelectionInput,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}

/** How a roving cursor moves (DESIGN_SPEC section 10). */
enum RovingMove derives CanEqual:
  /** To the nearest mark strictly on that side (device axes, y down); in a
    * table, Up and Down are the previous and next row.
    */
  case Left, Right, Up, Down

  /** To the first or last mark (or row) in order. */
  case First, Last

  /** To the next or previous mark (or row) in order: every one is reachable. */
  case Next, Previous

/** A key of a plot's or table's roving cursor (DESIGN_SPEC section 10). */
enum RovingKey derives CanEqual:
  /** An arrow, Home/End or Page Up/Down. */
  case Move(move: RovingMove)

  /** Enter or Space: select the focused mark or row; `toggle` adds or removes it. */
  case Activate(toggle: Boolean)

  /** Escape: clear the selection. */
  case Clear

/** One view's side of the selection bus (tickets S4.2 and S4.5a): the
  * selection as the bus last projected it, the context it was made in, and
  * the stamp sequence of the view's next input.
  *
  * Selection flows one way. A view [[submit]]s a stamped input as an
  * [[Intent.Select]]; what it shows changes only when the bus [[project]]s
  * the result back, which emits nothing, so a view never echoes its own or
  * another view's selection. A plot and its TableTwin are two views, each
  * with its own id, projecting the same bus.
  */
final case class ViewSelection private (
    view: ViewId,
    selected: Vector[StudioRef],
    context: ContextRevision,
    sequence: Long
) derives CanEqual:

  def isSelected(ref: StudioRef): Boolean = selected.contains(ref)

  /** A stamped selection input from this view, and the view after it. */
  def submit(
      mode: SelectionMode,
      refs: Vector[StudioRef],
      cause: InputCause
  ): (ViewSelection, Intent) =
    val stamp = InputStamp(context, view, sequence, cause)
    (copy(sequence = sequence + 1), Intent.Select(SelectionInput(stamp, mode, refs)))

  /** The selection as the bus now holds it, and whether its refs changed. */
  def project(selection: SelectionState): (ViewSelection, Boolean) =
    val next = copy(
      selected = selection.selected,
      context = selection.context,
      sequence = sequence.max(ViewSelection.nextSequence(view, selection))
    )
    (next, selection.selected != selected)

object ViewSelection:

  /** `view` showing `selection`. */
  def initial(view: ViewId, selection: SelectionState): ViewSelection =
    ViewSelection(view, selection.selected, selection.context, nextSequence(view, selection))

  // The bus refuses a sequence at or below the view's last applied one, so a
  // view re-attached under the same id continues after it.
  private def nextSequence(view: ViewId, selection: SelectionState): Long =
    selection.delivered.get(view).fold(0L)(_ + 1L)

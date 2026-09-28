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

import eyes4s.studio.app.text.Messages

/** The strings of a plot and its TableTwin (ticket S4.5a; DESIGN_SPEC section
  * 10). Kept apart from the shell's catalogue, as [[eyes4s.studio.app.text.TrialText]]
  * is; templates name their arguments by position.
  */
enum PlotTextId derives CanEqual:
  /** One cell spoken as "header value", and the separator between cells. */
  case Cell, CellSeparator

  /** A row or mark that is selected. */
  case Selected

  /** The accessible role and usage of a plot's single focus stop. */
  case PlotRole, PlotKeys

  /** The accessible usage of a table's single focus stop, and of an empty one. */
  case TableKeys, TableEmpty

  /** The Table tab's title (StudioLayouts' sibling Table pane). */
  case TableTab

  /** A mark that accounts for several rows, and the separator between their words. */
  case MarkRows, RowSeparator

  /** Rows the plot could not place or draw, and why. */
  case Unplotted, MissingValue, OffScale

  /** A plot whose builder refused its source, and why. */
  case Refused

/** A plot's strings in the boards' wording. */
object PlotText:

  /** The reference English template of `id`. */
  def english(id: PlotTextId): String =
    import PlotTextId.*
    id match
      case Cell          => "{0} {1}"
      case CellSeparator => ", "
      case Selected      => "{0}, selected"
      case PlotRole      => "plot"
      case PlotKeys      =>
        "{0}. One focus stop; arrow keys move to the nearest mark, Page Up and Page Down " +
          "step in order, Enter selects, Escape clears the selection."
      case TableKeys =>
        "{0}. One focus stop; arrow keys move the row cursor, Enter selects, Escape clears " +
          "the selection."
      case TableEmpty   => "{0}. No rows."
      case TableTab     => "Table"
      case Unplotted    => "{0} not drawn: {1}"
      case MissingValue => "no {0}"
      case OffScale     => "{0} {1} is off the scale"
      case MarkRows     => "{0} rows: {1}"
      case RowSeparator => "; "
      case Refused      => "{0}: plot not drawn. {1}"

  /** `id`'s English template with `args` filled in. */
  def apply(id: PlotTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

  /** `text`, marked as selected when `selected`. */
  def selected(text: String, selected: Boolean): String =
    if selected then apply(PlotTextId.Selected, text) else text

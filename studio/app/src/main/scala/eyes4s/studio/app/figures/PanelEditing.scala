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

package eyes4s.studio.app.figures

import eyes4s.studio.core.document.{FigureSpec, PanelLetter}

/** Actions on the selected panel; letter identity survives a move. */
final case class PanelEditingVM(
    selected: Option[PanelLetter],
    canRemove: Boolean,
    canMoveEarlier: Boolean,
    canMoveLater: Boolean
) derives CanEqual

object PanelEditing:
  val Remove: String      = "Remove panel"
  val Earlier: String     = "Move panel earlier"
  val Later: String       = "Move panel later"
  val NoSelection: String = "Select a panel to remove or move it."
  val LastPanel: String   = "A figure must keep at least one panel."

  def view(figure: FigureSpec, selected: Option[PanelLetter]): PanelEditingVM =
    val index = selected.flatMap(l =>
      figure.panels.indexWhere(_.letter == l) match
        case -1 => None
        case i  => Some(i)
    )
    PanelEditingVM(
      index.flatMap(figure.panels.lift).map(_.letter),
      index.isDefined && figure.panels.size > 1,
      index.exists(_ > 0),
      index.exists(_ < figure.panels.size - 1)
    )

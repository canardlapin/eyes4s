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

package eyes4s.studio.app.text

/** The strings of Compare's query and reference trial panels (ticket S8.2;
  * Main.dc.html, panels). Templates name their arguments by position, as
  * [[Messages]] does.
  */
enum PanelTextId derives CanEqual:
  /** The role pills. */
  case Query, Matched, Control

  /** A panel's title: participant, trial and item, or without the item. */
  case Title, TitleNoItem

  /** The two readouts, kept apart: the query's contrast and the inspected
    * pair's score.
    */
  case ContrastReadout, PairReadout, Reading, Unscored

  /** The reference panel's way back, and the matched reference it names. */
  case BackToMatched, MatchedIs

  /** The query panel's underlay toggle. */
  case Underlay

  /** Nothing to show yet. */
  case NoQuery

object PanelText:

  /** The reference English template of `id`. */
  def english(id: PanelTextId): String =
    import PanelTextId.*
    id match
      case Query           => "Query"
      case Matched         => "Matched"
      case Control         => "Control"
      case Title           => "{0} · {1} · {2}"
      case TitleNoItem     => "{0} · {1}"
      case ContrastReadout => "Query contrast D · σ {0}: {1}"
      case PairReadout     => "Inspected pair score · σ {0}: {1}"
      case Reading         => "reading…"
      case Unscored        => "{0}"
      case BackToMatched   => "Back to matched reference"
      case MatchedIs       => "Matched reference: {0}"
      case Underlay        => "Underlay remembered image"
      case NoQuery         => "Choose a query in the Queries navigator"

  /** `id`'s English template with `args` filled in. */
  def apply(id: PanelTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

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

/** The strings of Compare's contrast readout (ticket S8.3; Main.dc.html,
  * contrast). Templates name their arguments by position, as [[Messages]]
  * does.
  */
enum ContrastTextId derives CanEqual:
  /** The readout's caption: the query's trial and the scale shown. */
  case Caption

  /** The hero value's unit, M's and B's labels. */
  case D, M, B

  /** What D can and cannot say (Main.dc.html, verbatim). */
  case Confound

  /** The inspected pair: heading, role and item, what the pair is. */
  case Inspected, RoleItem, MatchedNote, ControlNote

  /** Prev and Next through the references, and where the pair ranks. */
  case Prev, Next, PrevName, NextName, MatchedRank, ControlRank

  /** While the ladder is read, or why it could not be. */
  case Reading, Unreadable, NoQuery

object ContrastText:

  /** The reference English template of `id`. */
  def english(id: ContrastTextId): String =
    import ContrastTextId.*
    id match
      case Caption  => "{0} contrast · σ {1}"
      case D        => "D"
      case M        => "M matched"
      case B        => "B mean of {0} controls"
      case Confound =>
        "Spatial correspondence, not replay. D does not separate participant-specific " +
          "reinstatement from item-driven salience common to all viewers of that image, " +
          "and may retain residual centre bias."
      case Inspected   => "Inspected pair"
      case RoleItem    => "{0} · {1}"
      case MatchedNote => "The matched pair. Its cosine is M."
      case ControlNote => "One of {0} controls. It enters B; it is not M."
      case Prev        => "← Prev"
      case Next        => "Next →"
      case PrevName    => "← Prev, previous reference"
      case NextName    => "Next →, next reference"
      case MatchedRank => "matched · then {0} controls"
      case ControlRank => "control {0} of {1} by cosine"
      case Reading     => "Reading the contrast of {0}…"
      case Unreadable  => "The contrast of {0} could not be read: {1}"
      case NoQuery     => "Choose a query in the Queries navigator"

  /** `id`'s English template with `args` filled in. */
  def apply(id: ContrastTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

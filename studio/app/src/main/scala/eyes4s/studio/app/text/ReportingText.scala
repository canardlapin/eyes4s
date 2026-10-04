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

/** The strings of Compare's reporting editor (ticket S8.7; Results.dc.html,
  * reporting). Templates name their arguments by position, as [[Messages]]
  * does.
  */
enum ReportingTextId derives CanEqual:
  case Kind, NoSpec, Reuses, ReusesUnknown
  case GroupBy, NoGrouping, GroupOption, GroupValues
  case FilterTitle, AllContributing, AllContributingUnknown, Outside, OutsideOff, OutsideOn
  case KeepFilter
  case Minimum, MinimumWouldDrop, MinimumDrops, MinimumNone, MinimumUnknown, Cell, CellGroup
  case WeightTitle, WeightUnit, EqualParticipants, PooledQueries
  case EstimandTitle, EstimandDuration, EstimandCount, Confound
  case SavedTitle, SaveAs, SaveAsName, Save, Cancel, DefaultCopyName
  case SavedCurrent, SavedOther, UsedBy, NotUsed

object ReportingText:
  import ReportingTextId.*

  /** The reference English template of `id`. */
  def english(id: ReportingTextId): String = id match
    case Kind   => "Reporting · no rerun"
    case NoSpec => "No reporting spec yet."
    case Reuses =>
      "Reuses {0}'s {1} pair scores. Grouping, filtering and weighting never change a " +
        "pair score or a control set."
    case ReusesUnknown =>
      "Reuses {0}'s pair scores. Grouping, filtering and weighting never change a pair " +
        "score or a control set."
    case GroupBy     => "Group by"
    case NoGrouping  => "None"
    case GroupOption => "{0}"
    case GroupValues => "{0}"

    case FilterTitle            => "Filter queries"
    case AllContributing        => "All contributing {0} queries ({1})"
    case AllContributingUnknown => "All contributing {0} queries"
    case Outside                =>
      "Exclude queries with >{0} of fixation duration outside the analysis window"
    case OutsideOff => "count shown when applied"
    case OutsideOn  => "applied · the count is not yet served by this backend"
    case KeepFilter => "Keep {0}: {1}"

    case Minimum          => "Minimum queries per group: {0}"
    case MinimumWouldDrop => "off · would drop {0} cells ({1})"
    case MinimumDrops     => "on · drops {0} cells ({1})"
    case MinimumNone      => "no participant has fewer than {0} queries in a group"
    case MinimumUnknown   => "no served summary of the run is grouped this way yet"
    case Cell             => "{0} n {1}"
    case CellGroup        => "{0} {1}"

    case WeightTitle       => "Summary unit and weighting"
    case WeightUnit        => "Query trial → participant mean → grand mean"
    case EqualParticipants => "Equal weight per participant"
    case PooledQueries     => "Weight by contributing queries"

    case EstimandTitle    => "What D measures"
    case EstimandDuration =>
      "D is the matched-minus-control spatial similarity of duration-weighted fixation " +
        "density. It reflects spatial correspondence, not sequential replay."
    case EstimandCount =>
      "D is the matched-minus-control spatial similarity of fixation-count density. It " +
        "reflects spatial correspondence, not sequential replay."
    case Confound =>
      "D does not separate participant-specific reinstatement from item-driven salience " +
        "common to all viewers of that image, and may retain residual centre bias."

    case SavedTitle      => "Saved reporting specs"
    case SaveAs          => "Save as…"
    case SaveAsName      => "Name of the new reporting spec"
    case Save            => "Save"
    case Cancel          => "Cancel"
    case DefaultCopyName => "{0} (copy)"
    case SavedCurrent    => "{0} · current view · {1}"
    case SavedOther      => "{0} · {1}"
    case UsedBy          => "used by {0}"
    case NotUsed         => "not used by a figure"

  /** `id`'s English template with `args` filled in. */
  def apply(id: ReportingTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

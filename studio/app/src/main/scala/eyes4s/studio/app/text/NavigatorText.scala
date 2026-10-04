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

/** The trials navigator's strings (ticket S6.1; Explore.dc.html, left),
  * kept apart from the other catalogues so the navigator adds its own ids.
  * Templates name their arguments by position, as [[Messages]] does.
  */
enum NavigatorTextId derives CanEqual:
  // --- The panes ---------------------------------------------------------------
  case TrialsTitle, ItemsTitle, TrialsFilter, ItemsFilter, TrialsList, ItemsList
  case NoDataset, Reading, ReadFailed, DisplaysFailed, Retry, NoMatch, Footer

  // --- Rows ----------------------------------------------------------------------
  case ParticipantDetail, Quarantined, Absent, ImagesMissing, Matching, PhaseHeader
  case PhaseHeaderMixed, Range, RangeAdmitted, Occurrence, ItemDetail, NoItem
  case TrialOf, Collapsed, Expanded, GroupAccessible

  // --- Statuses ------------------------------------------------------------------------
  case StatusAbsent, StatusQuarantined, StatusNoFixations, StatusImageMissing
  case LongAbsent, LongQuarantined, LongNoFixations, LongImageMissing

  // --- Display kinds -------------------------------------------------------------------
  case KindImage, KindBlank, KindBlankWithCross, KindCue, KindUnknown, KindMissing
  case KindNotAdmitted

/** The trials navigator's strings in the board's wording. */
object NavigatorText:
  import NavigatorTextId.*

  /** The reference English template of `id`. */
  def english(id: NavigatorTextId): String = id match
    case TrialsTitle    => "Trials"
    case ItemsTitle     => "Items"
    case TrialsFilter   => "Filter participant, trial, item"
    case ItemsFilter    => "Filter items"
    case TrialsList     => "Trials of {0}"
    case ItemsList      => "Items of {0}"
    case NoDataset      => "No admitted dataset revision yet: admit one in Data."
    case Reading        => "Reading the trials of {0}…"
    case ReadFailed     => "The trials of {0} are not available: {1}"
    case DisplaysFailed => "The displays of {0} are not available: {1}"
    case Retry          => "Retry"
    case NoMatch        => "No trials match “{0}”."
    case Footer         => "Explore is view-only. Nothing here changes an analysis."

    case ParticipantDetail => "{0}"
    case Quarantined       => "{0} quar."
    case Absent            => "{0} absent"
    case ImagesMissing     => "{0} image missing"
    case Matching          => "{0} match"
    case PhaseHeader       => "{0} · {1} · {2}"
    case PhaseHeaderMixed  => "{0} · {1}"
    case Range             => "{0}–{1}"
    case RangeAdmitted     => "{0} admitted"
    case Occurrence        => "{0} occ {1}"
    case ItemDetail        => "{0} trials"
    case NoItem            => "(no item)"
    case TrialOf           => "{0} · {1}"
    case Collapsed         => "collapsed"
    case Expanded          => "expanded"
    case GroupAccessible   => "{0}, {1}, {2}"

    case StatusAbsent       => "absent · no records"
    case StatusQuarantined  => "quarantined · {0}"
    case StatusNoFixations  => "no-fixations"
    case StatusImageMissing => "image missing"
    case LongAbsent         => "absent: in {0}, no fixation records"
    case LongQuarantined    => "quarantined: {0}"
    case LongNoFixations    => "no-fixations: records, none admissible"
    case LongImageMissing   => "image missing: {0}"

    case KindImage          => "Image"
    case KindBlank          => "Blank"
    case KindBlankWithCross => "Blank + cross"
    case KindCue            => "Cue"
    case KindUnknown        => "Unknown display"
    case KindMissing        => "Image missing"
    case KindNotAdmitted    => "Not admitted"

  /** `id`'s English template with its arguments filled. */
  def apply(id: NavigatorTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

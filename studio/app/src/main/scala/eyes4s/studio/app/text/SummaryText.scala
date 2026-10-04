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

import eyes4s.studio.core.selection.QueryCount

/** The strings of Compare's summary layout (ticket S8.6; the Results
  * board). They are kept apart from [[MessageId]] so the layout adds its own
  * ids without editing the shell's catalogue. Templates name their
  * arguments by position, as [[Messages]] does.
  */
enum SummaryTextId derives CanEqual:
  /** The participant table's and query table's captions. */
  case ParticipantCaption, QueryCaption

  /** The participant table's headers; `GroupHeader` takes the group. */
  case Participant, Requested, Contributing, Failed, NoMatch, NotAdmitted
  case MeanM, MeanB, MeanD, GroupHeader

  /** A participant's mean in a group with its n of queries. */
  case GroupCell

  /** The query table's headers. */
  case Query, Item, Response, Status, M, B, D

  /** A query's status when it has no scores. */
  case StatusFailed, StatusNoMatch, StatusNotAdmitted, StatusContributing

  /** The notes beside the participant plot (Results board). */
  case PairedN, Weighting, Unit, GroupRange

  /** The σ selector and why a scale cannot be chosen. */
  case Scale, ScaleUnavailable

  /** The Explain action and what it keeps. */
  case Explain, Keeps

  /** While the summary is read, or why it could not be. */
  case Reading, NoRun, Unreadable

  /** The Queries navigator (S8.1): a participant's header, the D column's
    * header, a query's status, and the count strip.
    */
  case ParticipantHeader, DColumn, Filter
  case StripRequested, StripContributing, StripFailed, StripNoMatchNotAdmitted, StripByDesign
  case StripPair, NotApplicable, ItemHeader

  /** A run's query tally (StudioRef.QueryTally) as a path, the outcomes'
    * own names, and the row cursor as announced.
    */
  case TallyOf, TallyNoMatch, TallyNotAdmitted, CursorHeader, CursorQuery
  case GroupOpen, GroupClosed, QueryRowSpoken
  case NoReportingSpec, NoMeansScale, NoScales

/** Compare's summary strings in the boards' wording. */
object SummaryText:

  /** The reference English template of `id`. */
  def english(id: SummaryTextId): String =
    import SummaryTextId.*
    id match
      case ParticipantCaption      => "Participant summary at σ {0}, run {1}"
      case QueryCaption            => "Queries of run {0} at σ {1}"
      case Participant             => "Participant"
      case Requested               => "Requested"
      case Contributing            => "Contributing"
      case Failed                  => "Failed"
      case NoMatch                 => "No match"
      case NotAdmitted             => "Not admitted"
      case MeanM                   => "Mean M"
      case MeanB                   => "Mean B"
      case MeanD                   => "Mean D"
      case GroupHeader             => "{0} D (n)"
      case GroupCell               => "{0} ({1})"
      case Query                   => "Query"
      case Item                    => "Item"
      case Response                => "Response"
      case Status                  => "Status"
      case M                       => "M"
      case B                       => "B"
      case D                       => "D"
      case StatusFailed            => "failed"
      case StatusNoMatch           => "no match"
      case StatusNotAdmitted       => "not admitted"
      case StatusContributing      => "contributing"
      case PairedN                 => "Paired n = {0} participants"
      case Weighting               => "Means: equal participant weight"
      case Unit                    => "Unit below each dot: query trial"
      case GroupRange              => "Per-group n: {0}–{1} queries per participant"
      case Scale                   => "σ {0}"
      case ScaleUnavailable        => "Participant means are served at σ {0} only"
      case Explain                 => "Explain {0} →"
      case Keeps                   => "keeps: {0}"
      case Reading                 => "Reading run {0}…"
      case NoRun                   => "No run yet — Open Analysis ⌘3"
      case Unreadable              => "Run {0} could not be read: {1}"
      case ParticipantHeader       => "{0} of {1} · {2}"
      case DColumn                 => "D · {0}"
      case Filter                  => "Filter participant or item"
      case StripRequested          => "Query contrasts requested"
      case StripContributing       => "Contributing"
      case StripFailed             => "Failed (empty map)"
      case StripNoMatchNotAdmitted => "No match · query not admitted"
      case StripByDesign           => "By design (n/a for this preset)"
      case StripPair               => "{0} · {1}"
      case NotApplicable           => "n/a"
      case ItemHeader              => "{0} queries"
      case TallyOf                 => "{0} · run {1}"
      case TallyNoMatch            => "No match"
      case TallyNotAdmitted        => "Query not admitted"
      case CursorHeader            => "{0}: {1}, {2}"
      case CursorQuery             => "{0}: {1}"
      case GroupOpen               => "open"
      case GroupClosed             => "closed"
      case QueryRowSpoken          => "{0}, {1}, D {2}"
      case NoReportingSpec         =>
        "Run {0}'s queries are read, but the document has no reporting spec to show them under."
      case NoMeansScale => "Run {0}'s queries are read, but no σ has its participant means: {1}"
      case NoScales     => "the run has no scales"

  /** A run's query tally as a path ("Contributing · run 7"). */
  def tally(run: eyes4s.studio.core.backend.RunId, count: QueryCount): String =
    import SummaryTextId.*
    val what = count match
      case QueryCount.Requested        => StripRequested
      case QueryCount.Contributing     => StripContributing
      case QueryCount.Failed           => StripFailed
      case QueryCount.NoMatch          => TallyNoMatch
      case QueryCount.QueryNotAdmitted => TallyNotAdmitted
    apply(TallyOf, apply(what), run.number.toString)

  /** `id`'s English template with `args` filled in. */
  def apply(id: SummaryTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

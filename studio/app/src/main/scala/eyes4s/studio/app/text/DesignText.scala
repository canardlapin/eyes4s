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

import eyes4s.studio.core.backend.AnalysisRevision
import eyes4s.studio.core.selection.DesignCount

/** The resolved-design pane's strings (ticket S7.5; Analysis.dc.html,
  * resolved design), kept apart from [[MessageId]] as [[GeometryText]] is.
  * Templates name their arguments by position, as [[Messages]] does.
  */
enum DesignTextId derives CanEqual:
  case Title, NoRevision, Accessible, AccessibleHelp, EmptyFilter

  // --- Filter chips --------------------------------------------------------------------
  case ChipAll, ChipEligible, ChipNoMatch, ChipNotAdmitted, ChipByDesign, NotApplicable
  case Pending

  // --- Columns and rows ----------------------------------------------------------------------
  case ColParticipant, ColQuery, ColItem, ColMatched, ColControls, ColStatus, NoValue
  case StatusEligible, StatusNoMatch, StatusNoMatchBecause, StatusNotAdmitted
  case DispositionAdmitted, DispositionAbsent, DispositionNoFixations, DispositionQuarantined
  case RowAccessible

  // --- Counting state and footer -------------------------------------------------------------
  case ModeExact, ModeCounting, ModePreparing, ModeRefused, CountingChip, CountingNote
  case CandidatesNote, ExactNote, StampNote, SameDesign, RowsWaiting, RowsFailed
  case PreviewFailed

  // --- Tallies (StudioRef.DesignTally) -------------------------------------------------------
  case TallyRequested, TallyEligible, TallyUnmatched, TallyNotAdmitted, TallyByDesign
  case TallyFocalTrials, TallyReferenceTrials, TallyCandidatePairs, TallyEligiblePairs, TallyOf
  case TallyParticipants

/** The resolved-design pane's strings in the board's wording. */
object DesignText:
  import DesignTextId.*

  /** The reference English template of `id`. */
  def english(id: DesignTextId): String = id match
    case Title      => "Resolved design"
    case NoRevision => "No analysis revision to preview yet."
    // The name is the table's focus stop: stable while counts arrive, which
    // the help and the chips carry.
    case Accessible     => "Resolved design, {0}; arrow keys move the row cursor"
    case AccessibleHelp => "{0} requested queries"
    case EmptyFilter    => "No query has this status."

    case ChipAll         => "All requested"
    case ChipEligible    => "Eligible"
    case ChipNoMatch     => "No match"
    case ChipNotAdmitted => "Query not admitted"
    case ChipByDesign    => "No study trial, by design"
    case NotApplicable   => "—"
    case Pending         => "…"

    case ColParticipant         => "P"
    case ColQuery               => "Query"
    case ColItem                => "Item"
    case ColMatched             => "Matched"
    case ColControls            => "Controls"
    case ColStatus              => "Status"
    case NoValue                => "—"
    case StatusEligible         => "Eligible"
    case StatusNoMatch          => "No match"
    case StatusNoMatchBecause   => "No match · {0}"
    case StatusNotAdmitted      => "Query not admitted · {0}"
    case DispositionAdmitted    => "admitted"
    case DispositionAbsent      => "absent from fixations.csv"
    case DispositionNoFixations => "no fixations"
    case DispositionQuarantined => "quarantined ({0})"
    case RowAccessible          => "{0} · {1} · {2}"

    case ModeExact      => "paged · exact"
    case ModeCounting   => "paging"
    case ModePreparing  => "preparing"
    case ModeRefused    => "not prepared"
    case CountingChip   => "counting eligible pairs… {0} of {1} participants"
    case CountingNote   => "While paging, counts read {0} and the run stays disabled."
    case CandidatesNote =>
      "Candidate pairs before paging {0} per scale ({1} queries × {2} references, " +
        "Cartesian · eyes4s candidatePairCount)."
    case ExactNote     => "Exact eligible after paging {0} per scale."
    case StampNote     => "input digest {0} · plan {1} · {2}"
    case SameDesign    => "Preview and run use this same prepared design."
    case RowsWaiting   => "Reading the resolved design…"
    case RowsFailed    => "The resolved design could not be read: {0}"
    case PreviewFailed => "The backend did not prepare the design: {0}"

    case TallyRequested       => "Requested queries"
    case TallyEligible        => "Eligible queries"
    case TallyUnmatched       => "Queries with no match"
    case TallyNotAdmitted     => "Queries not admitted"
    case TallyByDesign        => "Queries without a study trial, by design"
    case TallyParticipants    => "Participants"
    case TallyFocalTrials     => "Focal trials"
    case TallyReferenceTrials => "Reference trials"
    case TallyCandidatePairs  => "Candidate pairs per scale"
    case TallyEligiblePairs   => "Eligible pairs per scale"
    case TallyOf              => "{0} · {1}"

  def apply(id: DesignTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

  /** "rev 5 · Eligible queries": a [[DesignCount]] of a revision. */
  def tally(revision: AnalysisRevision, count: DesignCount): String =
    val what = count match
      case DesignCount.RequestedQueries       => TallyRequested
      case DesignCount.EligibleQueries        => TallyEligible
      case DesignCount.UnmatchedQueries       => TallyUnmatched
      case DesignCount.QueriesNotAdmitted     => TallyNotAdmitted
      case DesignCount.ByDesignQueries        => TallyByDesign
      case DesignCount.Participants           => TallyParticipants
      case DesignCount.FocalTrials            => TallyFocalTrials
      case DesignCount.ReferenceTrials        => TallyReferenceTrials
      case DesignCount.CandidatePairsPerScale => TallyCandidatePairs
      case DesignCount.EligiblePairsPerScale  => TallyEligiblePairs
    apply(TallyOf, revision.label, english(what))

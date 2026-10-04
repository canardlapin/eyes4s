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

/** The strings of Compare's inspector (ticket S8.4; Main.dc.html,
  * inspector): why the reference panel's trial is the query's reference,
  * the analysis and the reporting the run was made and read with, and the
  * view's appearance. Templates name their arguments by position, as
  * [[Messages]] does. Every explanation is one template per design choice,
  * filled from the prepared design; none is written for a case.
  */
enum WhyTextId derives CanEqual:
  case Title, NoQuery, Reading

  // --- Why this reference? ------------------------------------------------------------
  /** One template per eyes4s `MatchedReferences` choice. */
  case MatchedRequireOne, MatchedSameOccurrence, MatchedSelect, MatchedMeanOfAll

  /** One template per eyes4s `ControlReferences` choice, with and without a count. */
  case ControlSameSelection, ControlAllOccurrences, ControlsCounted, ControlsUncounted

  /** A query that has no reference, by its status and the recipe's policy. */
  case NoMatchReport, NoMatchRefuse, QueryNotAdmitted

  case Participant, SameParticipant, OtherParticipant
  case PhaseOccurrence, OccurrenceOf, OccurrenceOnly
  case Item, Controls, ControlsValue, ControlsUnknown
  case Excluded, ExcludedNone, ExcludedSome, ExcludedTrial, ExcludedReading,
    ExcludedUnreadable
  case OutsideReference, OutsideQuery, OutsideValue, OutsideReading, OutsideUnknown

  case DispositionAbsent, DispositionNoFixations, DispositionQuarantined, DispositionAdmitted

  // --- Analysis --------------------------------------------------------------------------
  case AnalysisTitle, AnalysisKind, NoAnalysis, EditInAnalysis
  case Representation, DurationWeighted, CountWeighted
  case Window, WindowImageFrame, WindowBox, WindowWhole, OffExcluded, OffFails
  case InitialFixation, KeepAll, DropFirst, DropNearCross
  case Smoother, Gaussian, Grid, GridCell, GridOnly
  case MetricControls, MetricSameSelection, MetricAllOccurrences

  // --- Reporting -------------------------------------------------------------------------
  case ReportingTitle, ReportingKind, NoReporting, Spec, GroupBy, NoGrouping
  case MinimumPerGroup, MinimumOff, MinimumOn, GroupRange, Filters, NoFilters
  case FilterKeep, FilterOutside, Summary, ParticipantMeans, PooledQueries

  // --- Appearance ------------------------------------------------------------------------
  case AppearanceTitle, ViewOnly, Stage, StageDark, StageMid, StageLight
  case Limits, LimitsShared, LimitsPerPanel, Opacity, OpacityValue, Retry

object WhyText:
  import WhyTextId.*

  /** The reference English template of `id`. */
  def english(id: WhyTextId): String = id match
    case Title   => "Why this reference?"
    case NoQuery => "Choose a query in the Queries navigator"
    case Reading => "Reading the run's queries…"

    // {0} reference phase, {1} participant, {2} item, {3} focal phase (lower case)
    case MatchedRequireOne =>
      "Matched reference: the only admitted {0} trial of {1} whose match item is {2}, " +
        "the item this {3} trial cued."
    case MatchedSameOccurrence =>
      "Matched reference: the admitted {0} trial of {1} whose match item is {2} at the " +
        "same occurrence as this {3} trial."
    // {4} the pick ("first", "last", "occurrence 2")
    case MatchedSelect =>
      "Matched reference: the {4} admitted {0} occurrence of {2} for {1}, the item this " +
        "{3} trial cued."
    case MatchedMeanOfAll =>
      "Matched reference: an admitted {0} trial of {1} whose match item is {2}; M is the " +
        "mean over every such trial."
    // {0} reference phase, {1} participant
    case ControlSameSelection =>
      "Eligible control: an admitted {0} trial of the same participant ({1}) showing a " +
        "different item."
    case ControlAllOccurrences =>
      "Eligible control: an admitted {0} trial of the same participant ({1}) showing a " +
        "different item, every occurrence of it counted."
    case ControlsCounted   => "All {0} such trials are used; none are sampled."
    case ControlsUncounted => "Every such trial is used; none are sampled."
    case NoMatchReport     =>
      "No matched reference: no admitted {0} trial of {1} has match item {2}. The query " +
        "is reported as no match."
    case NoMatchRefuse =>
      "No matched reference: no admitted {0} trial of {1} has match item {2}. The " +
        "recipe refuses a study with such a query."
    case QueryNotAdmitted => "The query was not admitted ({0}), so it has no reference."

    case Participant        => "Participant"
    case SameParticipant    => "{0}, same as query"
    case OtherParticipant   => "{0}"
    case PhaseOccurrence    => "Phase · occurrence"
    case OccurrenceOf       => "{0} · {1} of {2}"
    case OccurrenceOnly     => "{0} · {1}"
    case Item               => "Item"
    case Controls           => "Controls"
    case ControlsValue      => "{0} · all used"
    case ControlsUnknown    => "—"
    case Excluded           => "Excluded candidates"
    case ExcludedNone       => "none of {0} {1} trials"
    case ExcludedSome       => "{0} of {1} {2} trials: {3}"
    case ExcludedTrial      => "{0} ({1})"
    case ExcludedReading    => "reading the ledger…"
    case ExcludedUnreadable => "the ledger could not be read: {0}"
    case OutsideReference   => "Outside window"
    case OutsideQuery       => "Query outside"
    case OutsideValue       => "{0} of {1} fix · {2} dur"
    case OutsideReading     => "reading…"
    case OutsideUnknown     => "—"

    case DispositionAbsent      => "absent from fixations.csv"
    case DispositionNoFixations => "no fixations"
    case DispositionQuarantined => "quarantined, {0}"
    case DispositionAdmitted    => "admitted"

    case AnalysisTitle        => "Analysis {0}"
    case AnalysisKind         => "Analysis · rerun"
    case NoAnalysis           => "The document holds no analysis {0}."
    case EditInAnalysis       => "Edit in Analysis ⌘3"
    case Representation       => "Representation"
    case DurationWeighted     => "Duration-weighted fixation density"
    case CountWeighted        => "Fixation-count density"
    case Window               => "Window"
    case WindowImageFrame     => "Image frame {0}×{1}"
    case WindowBox            => "{0}×{1} px"
    case WindowWhole          => "Whole screen"
    case OffExcluded          => "{0}, outside excluded"
    case OffFails             => "{0}, outside fails the trial"
    case InitialFixation      => "Initial fixation"
    case KeepAll              => "Keep all"
    case DropFirst            => "Drop first fixation"
    case DropNearCross        => "Drop within {0}° of the cross"
    case Smoother             => "Smoother"
    case Gaussian             => "Gaussian, σ {0}"
    case Grid                 => "Grid"
    case GridCell             => "{0}×{1} · cell {2}"
    case GridOnly             => "{0}×{1}"
    case MetricControls       => "Metric · controls"
    case MetricSameSelection  => "{0} · all eligible"
    case MetricAllOccurrences => "{0} · all occurrences"

    case ReportingTitle   => "Reporting"
    case ReportingKind    => "Reporting · no rerun"
    case NoReporting      => "No reporting spec yet."
    case Spec             => "Spec"
    case GroupBy          => "Group by"
    case NoGrouping       => "None"
    case MinimumPerGroup  => "Min queries per group"
    case MinimumOff       => "Off"
    case MinimumOn        => "{0}"
    case GroupRange       => "{0} · n per group {1}–{2}"
    case Filters          => "Filters"
    case NoFilters        => "None"
    case FilterKeep       => "{0}: {1}"
    case FilterOutside    => "≤ {0} duration outside window"
    case Summary          => "Summary"
    case ParticipantMeans => "Participant means, equal weight"
    case PooledQueries    => "Query trials pooled, equal weight"

    case AppearanceTitle => "Appearance"
    case ViewOnly        => "View only"
    case Stage           => "Stage"
    case StageDark       => "Dark"
    case StageMid        => "Mid"
    case StageLight      => "Light"
    case Limits          => "Colour limits"
    case LimitsShared    => "Shared"
    case LimitsPerPanel  => "Per panel"
    case Opacity         => "Map opacity"
    case OpacityValue    => "{0}"
    case Retry           => "Retry"

  /** `id`'s English template with `args` filled in. */
  def apply(id: WhyTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

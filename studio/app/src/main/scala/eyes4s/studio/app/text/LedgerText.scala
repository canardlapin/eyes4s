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

/** The admission ledger's strings (ticket S5.6; Data.dc.html, admission),
  * kept apart from the other catalogues so the ledger adds its own ids.
  * Templates name their arguments by position, as [[Messages]] does.
  */
enum LedgerTextId derives CanEqual:
  // --- Header ------------------------------------------------------------------
  case Title, NoDataset, CountedFromInventory, CountedFromRecords, TrialsHeader

  // --- Counts --------------------------------------------------------------------
  case Inventory, Admitted, Quarantined, QuarantinedName, NoFixations, AbsentRow
  case OutsideScreen, OutsideScreenName
  case OutsideScreenRecords, Uncounted, CountAccessible, CauseAccessible, Equation
  case EquationMismatch, NoFixationsTerm, NoFixationsDefinition, AbsentTerm
  case AbsentDefinition, OpensHint

  // --- An opened count -------------------------------------------------------------
  case OpenedTitle, Close, NoTrials, TrialAbsent, TrialNoFixations, TrialAdmitted
  case TrialOutside, TrialAccessible, LedgerWaiting, LedgerFailed

  // --- The decision ------------------------------------------------------------------
  case DecisionTitle, DecisionLegend, RequireComplete, RequireCompleteRefuses
  case RequireCompleteAdmits, AbsentJoined, ReviewExclusions, ReviewExclusionsNote
  case ReviewExclusionsNoInventory, Changes, Creates, WouldStale, Admit, AdmitWaiting
  case AdmitRefused, AdmitVerifying, AdmittedStatus, NowStale, AdmittedNote

  // --- Where the counts come from ----------------------------------------------------------
  case CountsFrom, CountsWaiting, CountsFailed

  // --- Crumbs and paths ---------------------------------------------------------------------
  case CrumbCause, PathCount

/** The admission ledger's strings in the board's wording. */
object LedgerText:
  import LedgerTextId.*

  /** The reference English template of `id`. */
  def english(id: LedgerTextId): String = id match
    case Title                => "Admission"
    case NoDataset            => "No dataset revision yet: import fixations.csv first."
    case CountedFromInventory => "Counted from the {0} inventory"
    case CountedFromRecords   => "Counted from {0}; no trial inventory"
    case TrialsHeader         => "Trials"

    case Inventory       => "Inventory"
    case Admitted        => "Admitted"
    case Quarantined     => "Quarantined — whole trial held back when any record is invalid"
    case QuarantinedName => "Quarantined"
    case NoFixations     => "no-fixations"
    case AbsentRow       => "{0} — {1}"
    case OutsideScreen   => "Outside screen — excluded from maps and reported, not quarantined"
    case OutsideScreenName    => "Outside screen"
    case OutsideScreenRecords => "{0} records"
    case Uncounted            => "—"
    case CountAccessible      => "{0}: {1} trials"
    case CauseAccessible      => "{0} trials quarantined for {1}"
    case Equation             => "{0} + {1} + {2} = {3}."
    case EquationMismatch     =>
      "The counts do not add up: {0} admitted, {1} quarantined and {2} absent, but {3} in " +
        "the inventory."
    case NoFixationsTerm       => "no-fixations"
    case NoFixationsDefinition => ": the trial has records but none is admissible."
    case AbsentTerm            => "Absent"
    case AbsentDefinition      => ": listed in the inventory, no records in {0}."
    case OpensHint             => "Each count opens its trials and records."

    case OpenedTitle      => "{0} · {1} trials"
    case Close            => "Close"
    case NoTrials         => "No trials."
    case TrialAbsent      => "in the inventory, no fixation records"
    case TrialNoFixations => "records, none admissible"
    case TrialAdmitted    => "admitted"
    case TrialOutside     => "{0} outside screen: records {1}"
    case TrialAccessible  => "{0}, {1}: {2}. Opens the trial in Explore."
    case LedgerWaiting    => "Reading the ledger of {0}…"
    case LedgerFailed     => "The ledger of {0} is not available: {1}"

    case DecisionTitle          => "Admit dataset {0}"
    case DecisionLegend         => "Admission decision"
    case RequireComplete        => "Require complete"
    case RequireCompleteRefuses =>
      "Refuses {0} while {1} trials are quarantined (eyes4s admission)."
    case RequireCompleteAdmits => "Admits {0}: no trial is quarantined."
    case AbsentJoined          => "The {0} absent trials are counted by the inventory join."
    case ReviewExclusions      => "Review exclusions"
    case ReviewExclusionsNote  =>
      "Admits {0} trials; {1} quarantined and {2} absent are recorded with their causes in {3}."
    case ReviewExclusionsNoInventory =>
      "Admits {0} trials; {1} quarantined are recorded with their causes in {2}."
    case Changes      => "Changes from {0}: {1}."
    case Creates      => "Admitting creates dataset {0}."
    case WouldStale   => "{0} stays on {1} and is marked stale."
    case Admit        => "Admit as {0}"
    case AdmitWaiting =>
      "Admission waits for eyes4s's counts of {0}."
    case AdmitRefused =>
      "Require complete refuses {0}: {1} trials are quarantined. Review exclusions admits " +
        "{2} trials."
    case AdmitVerifying => "Verifying {0} with eyes4s…"
    case AdmittedStatus => "{0} is admitted."
    case NowStale       => "{0} ({1}) used {2} and is now stale."
    case AdmittedNote   =>
      "A change to its mapping or geometry creates a new dataset revision."

    case CountsFrom    => "Counts: eyes4s admission of {0}."
    case CountsWaiting => "Counts: waiting for eyes4s admission."
    case CountsFailed  => "Counts of {0} are not available: {1}"

    case CrumbCause => "{0} · {1}"
    case PathCount  => "{0} · {1}"

  /** `id`'s English template with its arguments filled. */
  def apply(id: LedgerTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

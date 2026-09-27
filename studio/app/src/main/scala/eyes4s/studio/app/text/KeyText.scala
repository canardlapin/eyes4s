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

/** The trial key builder's strings (ticket S5.3; Data.dc.html, key builder),
  * kept apart from [[ImportTextId]] so the key builder adds its own ids
  * without editing the wizard's catalogue. Templates name their arguments by
  * position, as [[Messages]] does.
  */
enum KeyTextId derives CanEqual:
  case Title, Rule, Plus, Equals, NoFile
  case PartParticipant, PartPhase, PartTrial, PartOccurrence
  case BlockColumn, BlockNoColumn, BlockLeftOut, BlockCandidate
  case AddOccurrence, LeaveOutOccurrence, OccurrenceUnavailable
  case UniqueKeys, UniqueKey, Keys, OneKey, FileLine
  case NoDuplicates, RepeatOneWithout, RepeatManyWithout, RepeatOneOccurrences
  case RepeatManyOccurrences, RepeatOneRecords, RepeatManyRecords
  case UnresolvedOne, UnresolvedMany
  case OccurrenceAllOne, OccurrenceRange, OccurrenceLeftOut
  case TrialOccurrence, TrialRecords, TrialRecord, RepeatedAccessible, RepeatedInFile
  case TrialSeparator
  case GapIssue, ConflictIssue, UnresolvedIssue, InventoryIssue, NoOccurrenceColumn

/** The key builder's strings in the boards' wording. */
object KeyText:

  /** The reference English template of `id`. */
  def english(id: KeyTextId): String =
    import KeyTextId.*
    id match
      case Title  => "Trial identity"
      case Rule   => "Every record and every inventory entry must resolve to exactly one key."
      case Plus   => "+"
      case Equals => "="
      case NoFile => "Choose fixations.csv to compose the trial key."
      case PartParticipant       => "Participant"
      case PartPhase             => "Phase"
      case PartTrial             => "Trial"
      case PartOccurrence        => "Occurrence"
      case BlockColumn           => "{0}: column {1}"
      case BlockNoColumn         => "{0}: no column"
      case BlockLeftOut          => "{0}: not in the key; eyes4s takes 1 for every trial"
      case BlockCandidate        => "{0}: not in the key; column {1} can hold it"
      case AddOccurrence         => "Add Occurrence to the key (column {0})"
      case LeaveOutOccurrence    => "Leave Occurrence out of the key"
      case OccurrenceUnavailable => "No column can hold the occurrence"
      case UniqueKeys            => "{0} unique keys"
      case UniqueKey             => "1 unique key"
      case Keys                  => "{0} keys"
      case OneKey                => "1 key"
      case FileLine              => "{0}: {1} {2}"
      case NoDuplicates          => "0 duplicates"
      case RepeatOneWithout      => "1 key repeats without Occurrence"
      case RepeatManyWithout     => "{0} keys repeat without Occurrence"
      case RepeatOneOccurrences  => "1 key names more than one occurrence"
      case RepeatManyOccurrences => "{0} keys name more than one occurrence"
      case RepeatOneRecords      => "1 key repeats"
      case RepeatManyRecords     => "{0} keys repeat"
      case UnresolvedOne         => "1 record resolves to no key"
      case UnresolvedMany        => "{0} records resolve to no key"
      case OccurrenceAllOne      => "Occurrence is 1 for every trial"
      case OccurrenceRange       => "Occurrence {0}–{1} · {2} later presentations"
      case OccurrenceLeftOut     => "Occurrence left out: 1 for every trial"
      case TrialOccurrence       => "occurrence {0} · {1}"
      case TrialRecords          => "records {0}"
      case TrialRecord           => "record {0}"
      case RepeatedInFile        => "{0} · {1}"
      case TrialSeparator        => "  |  "
      case RepeatedAccessible    => "Key {0} resolves to {1} trials: {2}"
      case GapIssue              => "Trial key: {0}"
      case ConflictIssue         =>
        "Warning (trial key): {0}: {1}. eyes4s identifies a trial by participant, phase " +
          "and trial, and quarantines each of these trials (occurrence conflict)."
      case UnresolvedIssue =>
        "Warning (trial key): {0}: {1}; eyes4s rejects them on admission. First: {2}."
      case NoOccurrenceColumn =>
        "{0} has no column that can hold the occurrence (occurrence, block, repeat or repetition)."
      case InventoryIssue =>
        "Warning (trial key, mapped in S5.4): {0}: {1} on more than one record. First: {2}."

  /** `id`'s English template with its arguments filled. */
  def apply(id: KeyTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

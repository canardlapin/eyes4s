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

/** The import wizard's strings (ticket S5.2; Data.dc.html, column mapping).
  * They are kept apart from [[MessageId]] so that the wizard adds its own ids
  * without editing the shell's catalogue. Templates name their arguments by
  * position, as [[Messages]] does.
  */
enum ImportTextId derives CanEqual:
  // --- Tabs and headers ------------------------------------------------------
  case Title, TabFixationMapping, TabTrialMetadata, TabGeometry, TabDataIssues
  case TabIssues
  case HeaderColumn, HeaderSamples, HeaderRole, HeaderUnits, HeaderTrialsColumn
  case FileSummary, FileSummaryRagged, NoFixationFile, NoTrialsFile, ChooseFile, ChooseTrials

  // --- Roles and units -----------------------------------------------------
  case RoleParticipant, RolePhase, RoleTrial, RoleOccurrence, RoleItem, RoleResponse
  case RoleOrdinal, RoleSampleCount, RoleX, RoleY, RoleOnset, RoleDuration, RoleAttribute
  case RoleFor, Required
  case UnitNone, UnitScreenPx, UnitSamples, UnitUndeclared, UnitText
  case TimeUnitLabel, TimeUndeclared, TimeMilliseconds, TimeMicroseconds, TimeSeconds
  case TimeNote

  // --- Geometry ----------------------------------------------------------------
  case GeometryNote, GeometryScreenWidth, GeometryScreenHeight, GeometryImageLeft
  case GeometryImageTop, GeometryImageWidth, GeometryImageHeight, GeometryPixelsPerDegree

  // --- Presets -------------------------------------------------------------------
  case PresetLabel, PresetNone, PresetApply, PresetNameLabel, PresetSave, PresetSaved
  case PresetApplied

  // --- Issues and commit ---------------------------------------------------------
  case NoIssues, IssuesSummaryOne, IssuesSummaryMany, RaggedIssue
  case CommitImport, CommitImportAs, CommitApply, CommitReadmit, Cancel
  case NoChange, NeedFixations, ReadFailed, AttributesNote, StoreFailed, TrialsNote
  case NotDatasetSource, RaggedMore, TrialWarning

  // --- Platform dialogs ------------------------------------------------------------
  case DialogFixations, DialogTrials, DialogFilter

  // --- The column-mapping pane (re-map of the selected revision) ------------------
  case Revert, PaneNoDataset, PaneReading, PaneNoProject

/** The wizard's strings in the boards' wording. */
object ImportText:

  /** The reference English template of `id`. */
  def english(id: ImportTextId): String =
    import ImportTextId.*
    id match
      case Title              => "Import sources"
      case TabFixationMapping => "Column mapping"
      case TabTrialMetadata   => "Trial metadata"
      case TabGeometry        => "Geometry"
      case TabDataIssues      => "Data issues"
      case TabIssues          => "{0} ({1})"
      case HeaderColumn       => "fixations.csv column"
      case HeaderTrialsColumn => "trials.csv column"
      case HeaderSamples      => "First records"
      case HeaderRole         => "Role"
      case HeaderUnits        => "Units (declared)"
      case FileSummary        => "{0} · {1} records · {2} columns · {3}-separated"
      case FileSummaryRagged  => "{0} · {1} records · {2} columns · {3}-separated · {4} ragged"
      case NoFixationFile => "No fixations file yet. Choose fixations.csv to map its columns."
      case NoTrialsFile   =>
        "No trials.csv. Absent trials cannot be counted without a trial inventory."
      case ChooseFile       => "Choose fixations.csv…"
      case ChooseTrials     => "Choose trials.csv…"
      case RoleParticipant  => "Participant"
      case RolePhase        => "Phase"
      case RoleTrial        => "Trial"
      case RoleOccurrence   => "Occurrence"
      case RoleItem         => "Match item"
      case RoleResponse     => "Response"
      case RoleOrdinal      => "Ordinal"
      case RoleSampleCount  => "Sample count"
      case RoleX            => "x"
      case RoleY            => "y"
      case RoleOnset        => "Onset"
      case RoleDuration     => "Duration"
      case RoleAttribute    => "Attribute (pass-through)"
      case RoleFor          => "Role for {0}"
      case Required         => "{0} (required)"
      case UnitNone         => "—"
      case UnitScreenPx     => "screen px"
      case UnitSamples      => "samples"
      case UnitUndeclared   => "undeclared"
      case UnitText         => "text"
      case TimeUnitLabel    => "Time unit (declared)"
      case TimeUndeclared   => "Not declared"
      case TimeMilliseconds => "ms"
      case TimeMicroseconds => "µs"
      case TimeSeconds      => "s"
      case TimeNote         =>
        "Onset and duration units are declared here; Eyes Studio never infers them."
      case GeometryNote =>
        "Declared, not calibrated: screen px, image frame = analysis window, linear px/°."
      case GeometryScreenWidth     => "Screen width (px)"
      case GeometryScreenHeight    => "Screen height (px)"
      case GeometryImageLeft       => "Image left (px)"
      case GeometryImageTop        => "Image top (px)"
      case GeometryImageWidth      => "Image width (px)"
      case GeometryImageHeight     => "Image height (px)"
      case GeometryPixelsPerDegree => "Pixels per degree (declared)"
      case PresetLabel             => "Import preset"
      case PresetNone              => "No presets saved"
      case PresetApply             => "Apply preset"
      case PresetNameLabel         => "Preset name"
      case PresetSave              => "Save as preset"
      case PresetSaved             => "Saved preset {0}."
      case PresetApplied           => "Applied preset {0} to {1}."
      case NoIssues                => "No issues: every required role has a column."
      case IssuesSummaryOne        => "1 issue blocks the import."
      case IssuesSummaryMany       => "{0} issues block the import."
      case RaggedIssue             =>
        "{0}: record {1} has {2} fields; the header has {3}. eyes4s rejects it on admission."
      case CommitImport   => "Import"
      case CommitImportAs => "Import as {0}"
      case CommitApply    => "Apply to {0}"
      case CommitReadmit  => "Re-admit as {0}"
      case Cancel         => "Cancel"
      case NoChange      => "The mapping, units and geometry are those of {0}; nothing changes."
      case NeedFixations => "Choose a fixations file first."
      case ReadFailed    => "{0} could not be read: {1}"
      case DialogFixations => "Choose the fixations file"
      case DialogTrials    => "Choose the trial inventory (trials.csv)"
      case DialogFilter    => "Delimited text"
      case StoreFailed     => "Not saved: {0}"
      case TrialsNote      =>
        "Trial metadata is mapped in S5.4: these roles are checked here but not yet applied, " +
          "and their issues are warnings that do not block the import."
      case NotDatasetSource =>
        "{0} is not {1}'s fixation file; a re-map reads the revision's own file."
      case RaggedMore     => "{0}: {1} more records have a width other than the header's."
      case TrialWarning   => "Warning (trial metadata, mapped in S5.4): {0}"
      case AttributesNote =>
        "Columns without a role pass through as attributes, kept as written."
      case Revert        => "Revert"
      case PaneNoDataset => "No dataset revision yet. Import sources to map their columns."
      case PaneReading   => "Reading {0}’s files from the project…"
      case PaneNoProject => "no project is open to read it from"

  /** `id`'s English template with its arguments filled. */
  def apply(id: ImportTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

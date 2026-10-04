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

/** The geometry panel's strings (ticket S5.5; Data.dc.html, geometry). They
  * are kept apart from [[MessageId]] so that the panel adds its own ids
  * without editing the shell's catalogue. Templates name their arguments by
  * position, as [[Messages]] does.
  */
enum GeometryTextId derives CanEqual:
  // --- Header and target -------------------------------------------------------
  case Title, NoDataset, EditsPending, ReadmitsAs, Verifying

  // --- Declared geometry -------------------------------------------------------------
  case GazeCoordinates, GazeCoordinatesValue, ScreenLabel, ScreenValue, PlacementLabel
  case PlacementValue, WindowLabel, WindowValue, PpdLabel, PpdDeclared
  case FieldScreenWidth, FieldScreenHeight, FieldImageLeft, FieldImageTop, FieldImageWidth
  case FieldImageHeight, FieldPpd, ViewingDistance, PhysicalWidth, NotRecorded, PhysicalNote

  // --- Worked example ------------------------------------------------------------------
  case ExampleRaw, ExampleRawValue, ExampleCorrected, ExampleCorrectedValue, ExampleImage
  case ExampleImageValue, ExampleImageOutside, ExampleDegrees, ExampleDegreesValue
  case ExampleNoDegrees

  // --- Placement check ---------------------------------------------------------------------
  case CheckPlacement, CheckPlacementNote, ThumbInside, ThumbOutside, ThumbOffScreen
  case ThumbAccessible, ThumbMarked, AllTrials, AllTrialsCaption, AllTrialsCorrected
  case DensityAccessible, PositionsWaiting, PositionsFailed, PositionsUnplaced

  // --- Corrections ---------------------------------------------------------------------------
  case MarkOrientation, MarkNeedsTrial, OrientationTitle, FixX, FixY, ScopeTrial
  case ScopeParticipant, RecordCorrection, Cancel, RulesTitle, RulesNote, RulesEmpty
  case RuleText, RuleRemove, TargetAll, TargetParticipant, TargetTrial, CorrectionFlipX
  case CorrectionFlipY, CorrectionTranslate, RuleOverlaps, PlacementTally

  // --- Off-screen policy and counts ------------------------------------------------------------
  case PolicyTitle, PolicyExclude, PolicyQuarantine, PolicyExcludeNote, PolicyQuarantineNote
  case OutsideWindowTitle, OutsideScreenTitle, CountValue, OutsideWindowNote
  case OutsideScreenExcluded, OutsideScreenQuarantined, CountsFrom, CountsPending
  case CountsWaiting, CountsFailed

/** The geometry panel's strings in the board's wording. */
object GeometryText:
  import GeometryTextId.*

  /** The reference English template of `id`. */
  def english(id: GeometryTextId): String = id match
    case Title        => "Geometry"
    case NoDataset    => "No dataset revision yet: import fixations.csv first."
    case EditsPending => "Changes edit {0}, which is not admitted yet."
    case ReadmitsAs   => "{0} is admitted: a change creates dataset {1}, pending admission."
    case Verifying => "{0} is being verified: a change creates dataset {1}, pending admission."

    case GazeCoordinates      => "Gaze coordinates"
    case GazeCoordinatesValue => "Screen px · origin top-left · y down"
    case ScreenLabel          => "Screen"
    case ScreenValue          => "{0} × {1} px"
    case PlacementLabel       => "Image placement"
    case PlacementValue       => "{0} × {1} at ({2}, {3})"
    case WindowLabel          => "Analysis window"
    case WindowValue          => "Image frame"
    case PpdLabel             => "Pixels per degree"
    case PpdDeclared          => "· declared, not calibrated"
    case FieldScreenWidth     => "Screen width"
    case FieldScreenHeight    => "Screen height"
    case FieldImageLeft       => "Image left"
    case FieldImageTop        => "Image top"
    case FieldImageWidth      => "Image width"
    case FieldImageHeight     => "Image height"
    case FieldPpd             => "px/°"
    case ViewingDistance      => "Viewing distance"
    case PhysicalWidth        => "Physical width"
    case NotRecorded          => "not recorded"
    case PhysicalNote         =>
      "Pixels per degree is declared: eyes4s does not derive it from a viewing distance " +
        "or a physical screen size, and neither is recorded."

    case ExampleRaw            => "Record {0} raw"
    case ExampleRawValue       => "screen ({0}, {1}) px"
    case ExampleCorrected      => "→ corrected (rule {0})"
    case ExampleCorrectedValue => "screen ({0}, {1}) px · {2}"
    case ExampleImage          => "→ image frame"
    case ExampleImageValue     => "({0}, {1}) px · ({2}°, {3}°)"
    case ExampleImageOutside   => "({0}, {1}) px · outside the frame"
    case ExampleDegrees        => "Degrees"
    case ExampleDegreesValue   => "from image centre · x right · y up"
    case ExampleNoDegrees      => "({0}, {1}) px · degrees unavailable"

    case CheckPlacement     => "Check placement"
    case CheckPlacementNote => "screen outline · image frame"
    case ThumbInside        => "{0} · {1} records inside"
    case ThumbOutside       => "{0} · {1} of {2} outside"
    case ThumbOffScreen     => "{0} · {1} of {2} off screen"
    case ThumbAccessible    =>
      "{0}: {1} records, {2} outside the image frame, {3} outside the screen"
    case ThumbMarked      => "{0}, chosen for marking"
    case AllTrials        => "All trials overlaid"
    case AllTrialsCaption =>
      "{0} records, screen coordinates. A flipped or shifted participant shows up as mass " +
        "away from the image frame."
    case AllTrialsCorrected =>
      "{0} records, screen coordinates, as the {1} recorded corrections place them. A flipped " +
        "or shifted participant shows up as mass away from the image frame."
    case DensityAccessible =>
      "Density of all {0} fixation records overlaid, screen coordinates"
    case PositionsWaiting  => "Reading {0}…"
    case PositionsFailed   => "Fixation positions are not available: {0}"
    case PositionsUnplaced => "{0} records have no finite position and are not drawn."

    case MarkOrientation  => "Mark trial as wrong orientation…"
    case MarkNeedsTrial   => "Choose a trial above to mark."
    case OrientationTitle => "Mark {0} as wrong orientation"
    case FixX             => "Flip horizontally (x)"
    case FixY             => "Flip vertically (y)"
    case ScopeTrial       => "This trial"
    case ScopeParticipant => "Every trial of {0}"
    case RecordCorrection => "Record correction"
    case Cancel           => "Cancel"
    case RulesTitle       => "Recorded corrections"
    case RulesNote        =>
      "Recorded in the admission ledger and applied by eyes4s at admission; source " +
        "coordinates are never rewritten."
    case RulesEmpty          => "No corrections recorded."
    case RuleText            => "{0} · {1} · {2}"
    case RuleRemove          => "Remove rule {0}"
    case TargetAll           => "all trials"
    case TargetParticipant   => "every trial of {0}"
    case TargetTrial         => "{0}"
    case CorrectionFlipX     => "flip horizontally"
    case CorrectionFlipY     => "flip vertically"
    case CorrectionTranslate => "shift by ({0}, {1}) px"
    case RuleOverlaps        => "Not recorded: {0}"
    case PlacementTally      => "placement"

    case PolicyTitle       => "Records outside the screen"
    case PolicyExclude     => "Exclude the record (default)"
    case PolicyQuarantine  => "Quarantine its trial"
    case PolicyExcludeNote =>
      "A finite record outside the screen is kept, left out of every map and reported."
    case PolicyQuarantineNote =>
      "A finite record outside the screen quarantines its whole trial."
    case OutsideWindowTitle => "Outside image frame"
    case OutsideScreenTitle => "Outside screen"
    case CountValue         => "{0} of {1} records · {2} trials"
    case OutsideWindowNote  =>
      "Reported, not dropped from the data. Excluded from maps because the analysis window " +
        "is the image frame; each trial lists its own count."
    case OutsideScreenExcluded =>
      "Excluded record by record and reported under its own cause; not a quarantine."
    case OutsideScreenQuarantined =>
      "Under Quarantine trial, each trial with such a record is quarantined."
    case CountsFrom    => "Counts: eyes4s admission of {0}."
    case CountsPending =>
      "Counts: eyes4s admission of {0}. {1} is pending; its own counts follow its verification."
    case CountsWaiting => "Counting records outside the frame…"
    case CountsFailed  => "Counts for {0} are not available: {1}"

  /** `id`'s English template with its arguments filled. */
  def apply(id: GeometryTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

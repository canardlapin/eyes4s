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

/** The strings of Explore's fixation inspector (ticket S6.5;
  * Explore.dc.html, right). Templates name their arguments by position, as
  * [[Messages]] does.
  */
enum InspectorTextId derives CanEqual:
  case Title, ViewOnly
  case OnsetDuration, Milliseconds, ImagePx, ScreenRaw, Degrees, Window, Pair, Missing
  case Onset, Duration, Ms, Admission
  case Inside, OutsideExcluded, OutsideFails, OffScreen, DroppedInitial, InsideTrialFails
  case FrameNote, ScaleOfRecipe, ScaleOfDataset
  case SourceTitle, File, Record, RecordOf, Digest, DigestShort, Ledger, Admitted, ShowRaw

  /** Show raw record's accessible name: which of Explore's two it is (S10.5). */
  case ShowRawName
  case UsedByTitle, UsedByNoScale, UsedByNoRun, MatchedOne, MatchedMany, AsControl, AsQuery,
    UsedByNone
  case TrialTitle, Key, KeyValue, DisplayLabel, DisplayImage, DisplayBlank, DisplayCross
  case DisplayCue, DisplayUnknown, MatchItem, Fixations, FixationsValue, Extent, ExtentSeconds,
    ExtentMissing
  case NoFixation, Reading

object InspectorText:

  def english(id: InspectorTextId): String =
    import InspectorTextId.*
    id match
      case Title            => "Fixation {0} of {1}"
      case ViewOnly         => "read-only"
      case OnsetDuration    => "Onset · duration"
      case Onset            => "Onset"
      case Duration         => "Duration"
      case Ms               => "{0} ms"
      case Admission        => "Admission"
      case Milliseconds     => "{0} ms · {1} ms"
      case ImagePx          => "Image px"
      case ScreenRaw        => "Screen px (raw)"
      case Degrees          => "Degrees"
      case Window           => "Window"
      case Pair             => "{0}, {1}"
      case Missing          => "—"
      case Inside           => "inside"
      case OutsideExcluded  => "Outside · excluded from maps, reported"
      case OutsideFails     => "Outside · the trial fails"
      case InsideTrialFails =>
        "Inside · the trial fails ({0} of {1} fixations outside the window)"
      case OffScreen      => "Off screen"
      case DroppedInitial => "Dropped by the initial-fixation policy"
      case FrameNote      => "Degrees from the image centre, x right, y up, at {0}."
      case ScaleOfRecipe  => "the recipe's {0} px/°"
      case ScaleOfDataset => "a declared {0} px/° (not calibrated)"
      case SourceTitle    => "Source"
      case File           => "File"
      case Record         => "Record"
      case RecordOf       => "{0} of {1}"
      case Digest         => "Digest"
      case DigestShort    => "sha256:{0}…{1}"
      case Ledger         => "Ledger"
      case Admitted       => "admitted · dataset {0}"
      case ShowRaw        => "Show raw record"
      case ShowRawName    => "Show raw record in the inspector"
      case UsedByTitle    => "Used by run {0} at {1}"
      case UsedByNoScale  => "Used by (run {0}): its scales are not in the project"
      case UsedByNoRun    => "Used by (no run shown)"
      case MatchedOne     => "{0} · matched reference"
      case MatchedMany    => "{0} queries (matched reference)"
      case AsControl      => "{0} pairs as a control"
      case AsQuery        => "its own contrast ({0} pairs as the query)"
      case UsedByNone     => "No pair of the run uses this trial"
      case TrialTitle     => "Trial"
      case Key            => "Key"
      case KeyValue       => "{0} · {1} · {2} · occ {3}"
      case DisplayLabel   => "Display"
      case DisplayImage   => "Image · {0}"
      case DisplayBlank   => "Blank"
      case DisplayCross   => "Blank + fixation cross"
      case DisplayCue     => "Cue"
      case DisplayUnknown => "Unknown"
      case MatchItem      => "Match item"
      case Extent         => "Extent"
      case ExtentSeconds  => "{0} s"
      case ExtentMissing  => "Not declared"
      case Fixations      => "Fixations"
      case FixationsValue => "{0} · {1} outside"
      case NoFixation     => "Select a fixation to inspect it"
      case Reading        => "Reading…"

  def apply(id: InspectorTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

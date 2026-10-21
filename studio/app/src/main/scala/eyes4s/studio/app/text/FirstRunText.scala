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

/** First-run strings, including guidance that never declares scientific geometry. */
enum FirstRunTextId derives CanEqual:
  case BadDropDetails
  case Intro, SourcesHelp, ChecklistIntro, ColumnRoles, ColumnRolesHelp, TimeUnits,
    TimeUnitsHelp, FramePlacement, FramePlacementHelp, PixelsPerDegree, PixelsPerDegreeHelp,
    TrialIdentity, TrialIdentityHelp, AdmissionReview, Readmit
  case NoSources, AddFixations, Title, Fixations, Required, FixationsHelp, ChooseFixations,
    Trials, Recommended,
    TrialsHelp, ChooseTrials, DropHere, Images, ImagesHelp, ChooseImages, Presets, PresetHelp,
    Encoding, Imagery, Recognition, EncodingHelp, ImageryHelp, RecognitionHelp, Recent,
    NoRecent, Example, Local, Checklist, CheckMapping, CheckUnits, CheckInventory,
    CheckGeometry, DropAnywhere, BadDrop, ImagesStored, ImageFailure, ExampleOperation,
    WizardGuidance

object FirstRunText:
  def apply(id: FirstRunTextId, args: String*): String =
    val template = id match
      case FirstRunTextId.BadDropDetails => "No files were imported. {0}"
      case FirstRunTextId.Intro          =>
        "Bring in a fixation table. Eyes Studio will propose how to read it and show you every trial it admits or sets aside before any analysis runs."
      case FirstRunTextId.SourcesHelp =>
        "Sources you add are listed here with their record counts and file digests."
      case FirstRunTextId.ChecklistIntro =>
        "It proposes an answer to each question, and you confirm it. Nothing that changes the science is guessed silently."
      case FirstRunTextId.ColumnRoles     => "Column roles"
      case FirstRunTextId.ColumnRolesHelp =>
        "Participant, phase, trial, x, y, onset, duration, plus the fixation ordinal and sample count."
      case FirstRunTextId.TimeUnits      => "Time units"
      case FirstRunTextId.TimeUnitsHelp  => "Declared by you, never inferred from the values."
      case FirstRunTextId.FramePlacement => "Screen frame and image placement"
      case FirstRunTextId.FramePlacementHelp =>
        "Origin, y direction, screen size, and where each image sat on the screen."
      case FirstRunTextId.PixelsPerDegree     => "Pixels per degree"
      case FirstRunTextId.PixelsPerDegreeHelp =>
        "Declared directly, or derived from viewing distance and screen size. Shown as “declared · not calibrated”."
      case FirstRunTextId.TrialIdentity     => "Trial identity"
      case FirstRunTextId.TrialIdentityHelp =>
        "Which columns make a trial unique, including repeated presentations."
      case FirstRunTextId.AdmissionReview =>
        "Then you review admission: which trials are admitted, which are set aside, and why."
      case FirstRunTextId.Readmit =>
        "Changing these answers later creates a new dataset revision, pending admission."

      case FirstRunTextId.NoSources       => "No sources yet"
      case FirstRunTextId.AddFixations    => "Add fixations.csv…"
      case FirstRunTextId.Title           => "Start a study"
      case FirstRunTextId.Fixations       => "fixations.csv"
      case FirstRunTextId.Required        => "Required"
      case FirstRunTextId.FixationsHelp   => "One record per fixation. CSV or TSV."
      case FirstRunTextId.ChooseFixations => "Choose fixations.csv file…"
      case FirstRunTextId.Trials          => "trials.csv"
      case FirstRunTextId.Recommended     => "Recommended"
      case FirstRunTextId.TrialsHelp      =>
        "The trial inventory, with responses and conditions. Without it, trials that have no fixation records cannot be counted."
      case FirstRunTextId.ChooseTrials => "Choose trials.csv file…"
      case FirstRunTextId.DropHere     => "or drop it here"
      case FirstRunTextId.Images       => "Stimulus images"
      case FirstRunTextId.ImagesHelp   => "Optional folder, matched to trials by file name"
      case FirstRunTextId.ChooseImages => "Choose stimulus image folder…"
      case FirstRunTextId.Presets      => "Start from a preset"
      case FirstRunTextId.PresetHelp   =>
        "Shapes the import guidance only; nothing is analysed yet. Confirm the column mappings, displays and units in the import wizard."
      case FirstRunTextId.Encoding     => "Encoding → Retrieval"
      case FirstRunTextId.Imagery      => "Perception → Imagery"
      case FirstRunTextId.Recognition  => "Study → Recognition"
      case FirstRunTextId.EncodingHelp =>
        "Retrieval trials compared with the encoding trial of the same item."
      case FirstRunTextId.ImageryHelp =>
        "Imagery on a blank display compared with the perceived image."
      case FirstRunTextId.RecognitionHelp =>
        "Old probes compared with their exact studied item. Lures have no corresponding study trial, by design."
      case FirstRunTextId.Recent   => "Recent projects"
      case FirstRunTextId.NoRecent => "No recent projects yet"
      case FirstRunTextId.Example  => "Open the bundled example study (golden fixture)"
      case FirstRunTextId.Local    =>
        "Everything stays on this computer. Sources are copied into the project, so the originals are never changed."
      case FirstRunTextId.Checklist    => "What Eyes Studio will ask you"
      case FirstRunTextId.CheckMapping =>
        "Which columns hold participant, phase, trial, position and time."
      case FirstRunTextId.CheckUnits     => "What units your times and positions use."
      case FirstRunTextId.CheckInventory =>
        "Which trials were planned, including trials without fixation records."
      case FirstRunTextId.CheckGeometry =>
        "The display frame, image placement and off-screen policy. Geometry is declared, never guessed."
      case FirstRunTextId.DropAnywhere => "Drop files anywhere in this window"
      case FirstRunTextId.BadDrop      =>
        "Choose one fixation table and at most one trials table, or a stimulus folder. No files were imported."
      case FirstRunTextId.ImagesStored     => "Stored {0} stimulus images."
      case FirstRunTextId.ImageFailure     => "Stimulus images: {0}"
      case FirstRunTextId.ExampleOperation => "Open example study"
      case FirstRunTextId.WizardGuidance   => "Import guidance: {0}. {1}"
    Messages.fill(template, args.toVector)

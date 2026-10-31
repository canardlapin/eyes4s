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

enum RecipeTextId derives CanEqual:
  case Apply, Loading, Checking, Applied, Unapplied, Locked, Field, Metadata, Default, Optional
  case ScanpathFileKind, ScanpathWorkspaceTitle, ScanpathChoosing
  case Add, Remove, ReadOnly, Scanpaths, Cell, CellUnknown, BelowCells, NearUniform, Scale,
    NewScale
  case Scales, AddScale, RemoveScale, ScaleEntry, ScaleHelp, InvalidScale, Diff, NoDiff, Recipe,
    Description
  case Name, Notes, Served, PresetHelp, SelectionNeeded, MinimumItems, MaximumItems, LastScale,
    CrossDeclared, DescriptionSave

  /** The σ rings beside the scales (overhaul v4): the note, the cell key, a new ring's label, and
    * the preset row's label.
    */
  case RingsNote, CellKey, NewRing, PresetLabel

  /** The recipe pane's subtitle: "draft rev 5 · 1 change from rev 4". */
  case PaneDraft, PaneSaved, OneChange, Changes

object RecipeText:
  def english(id: RecipeTextId): String =
    import RecipeTextId.*
    id match
      case ScanpathFileKind       => "Scanpath JSON"
      case ScanpathWorkspaceTitle => "Eyes Studio — MultiMatch scanpath workspace"
      case ScanpathChoosing       => "Finish or cancel the open file chooser to continue."
      case Apply                  => "Apply settings"
      case Loading                => "Loading recipe fields…"
      case Checking               => "Checking settings…"
      case Applied                => "Settings applied."
      case Unapplied              => "Apply these changes before Save & run."
      case Locked      => "Finish or discard the other draft before editing this revision."
      case Field       => "{0}"
      case Metadata    => "{0} · v{1} · {2}"
      case Default     => "Default: {0} · {1}"
      case Optional    => "Use {0}"
      case Add         => "Add {0}"
      case Remove      => "Remove {0}"
      case ReadOnly    => "{0} is supplied by the dataset."
      case Scanpaths   => "Open MultiMatch scanpath workspace"
      case Cell        => "cell {0}° × {1}°"
      case CellUnknown => "Cell size in degrees needs a declared angular scale."
      case BelowCells  => "{0}° is {1} cells wide: grid-limited"
      case NearUniform => "{0}° radius is {1} of the frame height: near-uniform maps"
      case RingsNote   =>
        "Radius σ on the {0} × {1} frame at a declared {2} px/° (not calibrated); inner rings {3}."
      case CellKey       => "1 grid cell ({0}°)"
      case NewRing       => "{0} new"
      case PresetLabel   => "Preset"
      case PaneDraft     => "draft {0} · {1} from {2}"
      case PaneSaved     => "{0}"
      case OneChange     => "1 change"
      case Changes       => "{0} changes"
      case Scale         => "{0}°"
      case NewScale      => "{0}° new"
      case Scales        => "Scales σ"
      case AddScale      => "Add scale"
      case RemoveScale   => "Remove scale"
      case ScaleEntry    => "New scale σ in degrees"
      case CrossDeclared =>
        "Drop leading fixations within {0}° of the cross at ({1}, {2}) screen px"
      case MinimumItems    => "Keep at least {0} entries."
      case MaximumItems    => "At most {0} entries are allowed."
      case LastScale       => "Keep at least one scale."
      case ScaleHelp       => "Arrow keys move; Delete removes the selected scale."
      case InvalidScale    => "Enter a positive scale in degrees that is not already present."
      case Diff            => "Diff vs {0}"
      case NoDiff          => "No recipe changes."
      case Recipe          => "Recipe"
      case DescriptionSave =>
        "Saved with this revision when you choose Save & run. Finish the current draft before editing another revision."
      case Description     => "Description"
      case Name            => "Analysis name"
      case Notes           => "Analysis notes"
      case Served          => "{0}"
      case PresetHelp      => "Preset"
      case SelectionNeeded =>
        "Declare the inventory column and values for lures and novel probes below."
  def apply(id: RecipeTextId, args: String*): String = Messages.fill(english(id), args.toVector)

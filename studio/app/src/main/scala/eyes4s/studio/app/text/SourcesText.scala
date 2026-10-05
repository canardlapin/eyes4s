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

import eyes4s.studio.core.assets.{SourceBlock, SourceFinding, SourceState}
import eyes4s.studio.core.assets.DisplayKind
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.selection.DisplayCount

/** The Data perspective's Sources pane strings (ticket S5.7; Data.dc.html,
  * left), kept apart from the other catalogues so the pane adds its own ids.
  * Templates name their arguments by position, as [[Messages]] does.
  */
enum SourcesTextId derives CanEqual:
  // --- The sources -----------------------------------------------------------------
  case KindFixations, KindInventory, KindImages, Stored, InventoryTrials, ImagesFound
  case ImagesNotServed, Stimuli, NoDataset

  // --- What each trial displayed --------------------------------------------------------
  case DisplaysTitle, ShownRow, NoneShown, Reading, Unreadable, RetryRead

  // --- Kinds ----------------------------------------------------------------------------------
  case Image, Blank, BlankWithFixationCross, Cue, Unknown

  // --- Missing assets ---------------------------------------------------------------------
  case MissingTitle, MissingTitleOne, MissingBody, MissingTrialsOf, Repair, ShowTrials
  case RepairNeedsProject, RepairFailed, Repaired

  // --- Tallies as paths -------------------------------------------------------------------
  case TallyShown, TallyNamed, TallyFound, TallyMissingFiles, TallyMissingTrials, TallyTrials
  case RepairsTitle, RepairLine, RepairOrphaned

  // --- Source repair (S2.5) ------------------------------------------------------------------
  case SourceChanged, SourceMissing, SourceWithheld, SourceUnreadable, RepairSource
  case SourceProblem, RunBlocked, SourceRecopied, SourceReplaced, SourceRepairFailed
  case BlockedUnchecked, BlockedCheckFailed, SourceUnchecked, RunWaiting, CancelWaiting
  case ImagesUnstored, RevisionChanged

/** The Sources pane's strings in the board's wording. */
object SourcesText:
  import SourcesTextId.*

  /** The reference English template of `id`. */
  def english(id: SourcesTextId): String = id match
    case KindFixations   => "Fixations"
    case KindInventory   => "Inventory"
    case KindImages      => "Images"
    case Stored          => "byte digest recorded · copied into project"
    case InventoryTrials => "{0} trials · inventory"
    case ImagesFound     => "{0} of {1} images found"
    case ImagesNotServed => "Display kinds are not served for this revision"
    case Stimuli         => "stimuli/"
    case NoDataset       => "No dataset revision is selected."

    case DisplaysTitle => "What each trial displayed"
    case ShownRow      => "{0} · {1}"
    case NoneShown     => "{0}"
    case Reading       => "Reading the trial displays…"
    case Unreadable    => "The trial displays cannot be read: {0}"
    case RetryRead     => "Retry"

    case Image                  => "Image"
    case Blank                  => "Blank"
    case BlankWithFixationCross => "Blank + fixation cross"
    case Cue                    => "Cue"
    case Unknown                => "Unknown"

    case MissingTitle    => "{0} image files missing"
    case MissingTitleOne => "1 image file missing"
    case MissingBody     =>
      "{0}. Their {1} trials ({2}) are drawn as missing asset, never as blank. " +
        "Gaze and scores are unaffected."
    case MissingTrialsOf    => "{0} {1}"
    case Repair             => "Repair…"
    case ShowTrials         => "Show {0} trials"
    case RepairNeedsProject => "Save the project to store a repaired image."
    case RepairFailed       => "{0} could not be repaired: {1}"
    case Repaired           => "{0} repaired with {1}"

    case TallyShown         => "{0} · {1} · {2}"
    case TallyNamed         => "{0} · image files named"
    case TallyFound         => "{0} · image files found"
    case TallyMissingFiles  => "{0} · image files missing"
    case TallyMissingTrials => "{0} · trials with a missing image"
    case TallyTrials        => "{0} · trials with a display"
    case RepairsTitle       => "Repaired images"
    case RepairLine         => "{0} ← {1} · sha256:{2}"
    case RepairOrphaned     =>
      "{0} was stored, but the revision changed before it could be repaired; repair it again."
    case SourceChanged    => "changed since it was stored (sha256:{0} recorded, {1} found)"
    case SourceMissing    => "missing from the project (sha256:{0} recorded)"
    case SourceWithheld   => "withheld from this copy of the project (sha256:{0} recorded)"
    case SourceUnreadable => "unreadable (sha256:{0} recorded): {1}"
    case RepairSource     => "Repair {0}…"
    case SourceProblem    => "{0} is {1}"
    case RunBlocked       =>
      "{0} is blocked: {1}. Repair it in Data · Sources, or admit a revision that replaces it."
    case SourceUnchecked =>
      "not checked yet (sha256:{0} recorded); the project's stored files are being checked"
    case CancelWaiting => "Cancel the waiting Save & run"
    case RunWaiting    =>
      "Save & run is already waiting for the project's stored files to be checked."
    case BlockedUnchecked =>
      "{0} is blocked until the project's stored files are checked against their digests."
    case BlockedCheckFailed =>
      "{0} is blocked: the project's stored files could not be checked ({1})."
    case SourceRecopied =>
      "{0} re-copied: its bytes are the ones {1} recorded (sha256:{2})."
    case SourceReplaced =>
      "{0} has other bytes than {1} recorded; {2} replaces it and must be admitted before it is run."
    case SourceRepairFailed => "{0} could not be repaired: {1}"
    case ImagesUnstored     => "{0} stored image files changed or missing"
    case RevisionChanged    =>
      "The revision changed before {0} could be repaired; nothing was stored. Repair it again."

  /** `id`'s English template with `args` filled in. */
  def apply(id: SourcesTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

  /** What the project holds for a source that is not present (S2.5). */
  def state(f: SourceFinding): String =
    val recorded = f.source.bytes.hex
    f.state match
      case SourceState.Present         => ""
      case SourceState.Missing         => apply(SourceMissing, recorded)
      case SourceState.Withheld        => apply(SourceWithheld, recorded)
      case SourceState.Unreadable(why) => apply(SourceUnreadable, recorded, why)
      case SourceState.Changed(found)  => apply(SourceChanged, recorded, found.hex)
      case SourceState.Unchecked       => apply(SourceUnchecked, recorded)

  /** Why `dataset` may not be run or previewed, naming each file. */
  def blocked(dataset: DatasetRevision, block: SourceBlock): String = block match
    case SourceBlock.Unchecked           => apply(BlockedUnchecked, dataset.label)
    case SourceBlock.CheckFailed(reason) => apply(BlockedCheckFailed, dataset.label, reason)
    case SourceBlock.Damaged(findings)   =>
      apply(
        RunBlocked,
        dataset.label,
        findings.map(f => apply(SourceProblem, f.name, state(f))).mkString("; ")
      )

  /** A display kind as the board names it. */
  def kind(k: DisplayKind): String = k match
    case DisplayKind.Image                  => apply(Image)
    case DisplayKind.Blank                  => apply(Blank)
    case DisplayKind.BlankWithFixationCross => apply(BlankWithFixationCross)
    case DisplayKind.Cue                    => apply(Cue)
    case DisplayKind.Unknown                => apply(Unknown)

  /** A display tally as a path ("r3 · Image · Encoding"). */
  def tally(dataset: DatasetRevision, count: DisplayCount): String = count match
    case DisplayCount.Shown(k, phase) => apply(TallyShown, dataset.label, kind(k), phase.label)
    case DisplayCount.ImagesNamed     => apply(TallyNamed, dataset.label)
    case DisplayCount.ImagesFound     => apply(TallyFound, dataset.label)
    case DisplayCount.MissingFiles    => apply(TallyMissingFiles, dataset.label)
    case DisplayCount.MissingTrials   => apply(TallyMissingTrials, dataset.label)
    case DisplayCount.Trials          => apply(TallyTrials, dataset.label)

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

import eyes4s.studio.core.selection.StudioRef
import eyes4s.plan.{MapPlacement, OffWindowPolicy}

/** The strings a trial view draws on its stage (ticket S4.3a; DESIGN_SPEC
  * sections 5 and 9). They are kept apart from [[MessageId]] so that the
  * trial view adds its own ids without editing the shell's catalogue.
  * Templates name their arguments by position, as [[Messages]] does.
  */
enum TrialTextId derives CanEqual:
  /** "Displayed: …": the display kind the trial showed. */
  case DisplayedImage, DisplayedBlank, DisplayedBlankCross, DisplayedCue, DisplayedCueItem
  case DisplayedUnknown, DisplayedMissing, DisplayedUnreadable, DisplayedLoading

  /** Where the image frame lies in the screen frame. */
  case Placement

  /** Joins caption clauses. */
  case Separator

  /** The mark encoding and the order-line disclaimer. */
  case MarkerArea, OrderNote

  /** The analysis-window outline's label. */
  case WindowLabel

  /** Labels drawn inside the image frame. */
  case MissingLabel, UnreadableLabel, LoadingLabel, UnknownGlyph

  /** The accessible name of a trial's fixation marks. */
  case MarksTitle

  /** The accessible text of the trial view and of its focused mark (S4.2). */
  case PlotRole, PlotKeys, MarkFocus, MarkSelected

  /** The core placement state announced for a focused fixation. */
  case MarkInMap, MarkDroppedInitial, MarkOutsideScreen, MarkOutsideWindowExcluded,
    MarkOutsideWindowFailsTrial

  /** The remembered image of a retrieval trial: shown as an underlay, with
    * its disclosure, or not shown (S4.3b).
    */
  case RememberedShown, RememberedHidden

/** The trial view's strings in the boards' wording. */
object TrialText:

  /** The reference English template of `id`. */
  def english(id: TrialTextId): String =
    import TrialTextId.*
    id match
      case DisplayedImage      => "Displayed: image {0}"
      case DisplayedBlank      => "Displayed: blank"
      case DisplayedBlankCross => "Displayed: blank + fixation cross"
      case DisplayedCue        => "Displayed: cue"
      case DisplayedCueItem    => "Displayed: cue {0}"
      case DisplayedUnknown    => "Displayed: unknown display"
      case DisplayedMissing    => "Displayed: image {0} · missing asset"
      case DisplayedUnreadable => "Displayed: image {0} · asset unreadable"
      case DisplayedLoading    => "Displayed: image {0} · loading…"
      case Placement           => "{0}×{1} at ({2}, {3}) in {4}×{5}"
      case Separator           => " · "
      case MarkerArea          => "Marker area ∝ duration"
      case OrderNote       => "Lines show order between fixation centres, not measured saccades"
      case WindowLabel     => "analysis window · image frame {0}×{1}"
      case MissingLabel    => "Missing asset · {0}"
      case UnreadableLabel => "Unreadable asset · {0}"
      case LoadingLabel    => "Loading {0}…"
      case UnknownGlyph    => "?"
      case MarksTitle      => "Fixations of {0}"
      case PlotRole        => "fixation plot"
      case PlotKeys        =>
        "Fixations of {0}. Arrow keys move to the nearest fixation, Page Up and Page Down " +
          "step in order, Enter selects, Escape clears the selection."
      case MarkFocus                   => "Fixation {0} of {1}"
      case MarkSelected                => "{0}, selected"
      case MarkInMap                   => "{0} · in map"
      case MarkDroppedInitial          => "{0} · dropped by initial-fixation policy"
      case MarkOutsideScreen           => "{0} · outside screen"
      case MarkOutsideWindowExcluded   => "{0} · outside window, excluded from map"
      case MarkOutsideWindowFailsTrial => "{0} · outside window, trial fails"
      case RememberedShown             => "Reference image — not displayed during this trial"
      case RememberedHidden            => "Remembered image not shown"

  /** `id`'s English template with `args` filled in. */
  def apply(id: TrialTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

  /** The accessible text of a focused fixation mark, from its semantic id. */
  def mark(ref: StudioRef.Fixation, placement: MapPlacement, selected: Boolean): String =
    val focus  = apply(TrialTextId.MarkFocus, ref.index.value.toString, ref.trial.label)
    val placed = placement match
      case MapPlacement.InMap          => apply(TrialTextId.MarkInMap, focus)
      case MapPlacement.DroppedInitial => apply(TrialTextId.MarkDroppedInitial, focus)
      case MapPlacement.OutsideScreen  => apply(TrialTextId.MarkOutsideScreen, focus)
      case MapPlacement.OutsideWindow(OffWindowPolicy.Exclude) =>
        apply(TrialTextId.MarkOutsideWindowExcluded, focus)
      case MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial) =>
        apply(TrialTextId.MarkOutsideWindowFailsTrial, focus)
    if selected then apply(TrialTextId.MarkSelected, placed) else placed

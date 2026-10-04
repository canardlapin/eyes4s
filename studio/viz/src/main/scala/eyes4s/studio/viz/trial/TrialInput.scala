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

package eyes4s.studio.viz.trial

import eyes4s.studio.app.text.{TrialText, TrialTextId}
import eyes4s.studio.core.backend.TrialKey
import eyes4s.studio.core.selection.{SelectionState, StudioRef, ViewId}
import eyes4s.studio.viz.plot.{MarkInputState, MarkInputStep, SelectionShare}

// A trial view's input (ticket S4.2) is the input of any view with marks
// (eyes4s.studio.viz.plot.MarkInputState, S4.5a) over fixation refs; these
// names keep the trial's vocabulary.
export eyes4s.studio.app.plot.{RovingKey, RovingMove}
export eyes4s.studio.viz.plot.{
  MarkInputEvent as TrialInputEvent,
  OverlayPalette,
  OverlayRing,
  OverlayRings,
  RingKind
}

/** A trial view's input state: the roving cursor over its fixations. */
type TrialInputState = MarkInputState[StudioRef.Fixation]

/** What an input did to a trial view. */
type TrialInputStep = MarkInputStep[StudioRef.Fixation]

object TrialInputState:

  /** A view with nothing focused or hovered, showing `selection`. */
  def initial(view: ViewId, selection: SelectionState): TrialInputState =
    MarkInputState.initial(view, selection)

extension (state: TrialInputState)
  /** The view's accessible text: the focused mark, from its semantic id, or
    * how to use the view when no mark is focused.
    */
  def accessibleText(trial: TrialKey, targets: TrialTargets): String =
    state.spoken(
      targets,
      (ref, share) => TrialText.mark(ref, share != SelectionShare.Unselected),
      TrialText(TrialTextId.PlotKeys, trial.label)
    )

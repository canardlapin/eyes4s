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
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.viz.plot.{MarkInputState, SelectionShare}

extension (state: MarkInputState[StudioRef.Fixation])
  /** The view's accessible text: the focused mark, from its semantic id, or
    * how to use the view when no mark is focused. The mark's text states its
    * map placement.
    */
  def accessibleText(trial: TrialKey, targets: TrialTargets): String =
    state.focus.flatMap(ref => targets.target(ref).map(ref -> _)) match
      case Some((ref, t)) =>
        TrialText.mark(ref, t.mark.placement, state.share(t) != SelectionShare.Unselected)
      case None => TrialText(TrialTextId.PlotKeys, trial.label)

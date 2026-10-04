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

package eyes4s.studio.app.compare

import eyes4s.plan.MapPlacement
import eyes4s.studio.core.assets.TrialDisplay
import eyes4s.studio.core.backend.{AnalysisRevision, TrialKey}
import eyes4s.studio.core.document.ScreenSize
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}

/** One fixation of a trial as a trial panel draws it: its position in
  * screen pixels, its onset and duration, and where it falls against the
  * analysis window.
  */
final case class ContentFixation(
    trial: TrialKey,
    index: FixationIndex,
    screenX: Double,
    screenY: Double,
    onsetMs: Int,
    durationMs: Int,
    placement: MapPlacement
) derives CanEqual:
  def ref: StudioRef = StudioRef.Fixation(trial, index)

/** What a trial panel draws a trial from: the pieces of a trial scene's
  * input that come from the study (its display, the screen, its fixations).
  */
final case class TrialContent(
    display: TrialDisplay,
    screen: ScreenSize,
    fixations: Vector[ContentFixation]
) derives CanEqual

/** Why a trial's content could not be had; every case names the trial. */
enum ContentError derives CanEqual:
  /** The window's source serves no trial content. */
  case NotServed(trial: TrialKey)

  /** The source could not read it; `reason` says why. */
  case Unreadable(trial: TrialKey, reason: String)

  def message: String = this match
    case NotServed(t)          => s"The content of ${t.label} is not served."
    case Unreadable(t, reason) => s"The content of ${t.label} could not be read: $reason"

/** Where Compare's trial panels read a trial's content (ticket S8.2): a port,
  * so the panels do not depend on how a backend serves trials. S6.2's trial
  * fixations and preview serve it in the window; `done` may be called on any
  * thread.
  */
trait TrialContentSource:
  def content(
      revision: AnalysisRevision,
      trial: TrialKey,
      done: Either[ContentError, TrialContent] => Unit
  ): Unit

object TrialContentSource:

  /** No content: every trial is [[ContentError.NotServed]]. */
  val notServed: TrialContentSource = (_, trial, done) =>
    done(Left(ContentError.NotServed(trial)))

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

package eyes4s.studio.app.explore

import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.document.{Perspective, SourceRole}
import eyes4s.studio.core.selection.{RecordNumber, StudioRef}

/** Linked selection in Explore (ticket S6.6; Explore.dc.html, interactive).
  *
  * Every Explore view selects through the one selection, so a mark, a
  * timeline bar, a source record or Prev/Next selects the same fixation in
  * all of them. Two rules live here: Prev and Next step through the trial
  * view's fixations in scanpath order, and the trail follows the selection:
  * its last crumbs become the selected fixation and its source record.
  */
object ExploreLinked:

  /** The fixation the selection names in the trial view's trial, if any: a
    * fixation, or a record of one.
    */
  def selected(view: ExploreTrialView, m: AppModel): Option[StudioRef.Fixation] =
    FixationInspector.focusOf(m).filter(f => view.trial.contains(f.trial))

  /** The trial view's fixations in scanpath order, as read. */
  private def fixations(view: ExploreTrialView): Vector[StudioRef.Fixation] =
    view.fixations.toOption
      .collect { case BackendAnswer.Answered(f) => f.fixations.map(_.ref) }
      .getOrElse(Vector.empty)

  /** The fixation Prev (`-1`) or Next (`+1`) selects: the one before or after
    * the selected fixation; with none selected, the first (Next) or the last
    * (Prev). None at either end, or before the fixations are read.
    */
  def step(view: ExploreTrialView, m: AppModel, by: Int): Option[StudioRef.Fixation] =
    val fs = fixations(view)
    selected(view, m).flatMap(f => Some(fs.indexOf(f)).filter(_ >= 0)) match
      case Some(i) => fs.lift(i + by)
      case None    => if by > 0 then fs.headOption else fs.lastOption

  /** Whether Prev (`-1`) or Next (`+1`) can step. */
  def canStep(view: ExploreTrialView, m: AppModel, by: Int): Boolean =
    step(view, m, by).isDefined

  /** The trial view's focus stops after its toggles (and Retry): Prev and
    * Next while each can step, then the marks' one stop, named `marks`
    * (the plot's accessible text), when it has one.
    */
  def focusStops(
      view: ExploreTrialView,
      m: AppModel,
      marks: Option[String]
  ): Vector[eyes4s.studio.app.vm.FocusStop] =
    import eyes4s.studio.app.vm.{A11yRole, FocusStop}
    import eyes4s.studio.app.text.{ExploreText, ExploreTextId}
    val vm = ExploreTrialViewVM.of(view, m)
    ExploreTrialViewVM.focusStops(vm) ++
      Option
        .when(canStep(view, m, -1))(
          FocusStop(A11yRole.Button, ExploreText(ExploreTextId.PrevFixation))
        )
        .toVector ++
      Option
        .when(canStep(view, m, 1))(
          FocusStop(A11yRole.Button, ExploreText(ExploreTextId.NextFixation))
        )
        .toVector ++
      marks.map(FocusStop(A11yRole.Region, _)).toVector

  /** The trail's crumb for `f`: its source record, which names it, when the
    * trial view knows its record.
    */
  def crumb(view: ExploreTrialView, f: StudioRef.Fixation): Option[StudioRef.SourceRecord] =
    for
      record <- view.fixations.toOption
        .collect { case BackendAnswer.Answered(fs) => fs.fixations }
        .flatMap(_.find(_.ref == f))
        .map(_.record)
      number <- RecordNumber.of(record).toOption
    yield StudioRef.SourceRecord(f.trial, Some(f.index), SourceRole.Fixations, number)

  /** The navigation that brings Explore's trail to the selected fixation and
    * its record (the trail's fixation and record crumbs), when the trail is
    * not already there. Only in Explore, and only for the trial view's trial.
    */
  def follow(view: ExploreTrialView, m: AppModel): Option[Intent] =
    for
      f      <- selected(view, m) if m.perspective == Perspective.Explore
      record <- crumb(view, f)
      if !m.location.trail.lastOption.contains(Place.At(record))
    yield Intent.Follow(Place.At(record))

/** When the trail follows the selection (S6.6): after the selection changes,
  * once, as soon as the trial view can name the selected fixation's record.
  * The selection itself is not followed, so a crumb clicked above the
  * fixation leaves the trail where it was clicked.
  */
final case class TrailFollow(seen: Vector[StudioRef], pending: Boolean) derives CanEqual

object TrailFollow:

  /** Nothing to follow: `m`'s selection is the one the trail was made with. */
  def initial(m: AppModel): TrailFollow = TrailFollow(m.selection.selected, false)

  /** The follow state after `m` with the trial view `view`, and the
    * navigation to ask for now, if any.
    */
  def step(f: TrailFollow, view: ExploreTrialView, m: AppModel): (TrailFollow, Option[Intent]) =
    val changed = m.selection.selected != f.seen
    val pending = f.pending || changed
    if !pending then (f, None)
    else
      val wanted = ExploreLinked.follow(view, m)
      // Done once asked, or once the fixations are read and there is nothing to ask.
      val settled = wanted.isDefined || ExploreLinked.selected(view, m).isEmpty ||
        view.fixations.toOption.isDefined
      (TrailFollow(m.selection.selected, !settled), wanted)

  /** The follow to apply now, when the follow asked at `asked` is run later
    * (after the update that asked it): derived again from the model `now`,
    * and only while nothing has moved since. A crumb clicked, a perspective
    * switched or a selection changed in between wins over the follow.
    */
  def deferred(asked: AppModel, view: ExploreTrialView, now: AppModel): Option[Intent] =
    if now.location != asked.location || now.selection.selected != asked.selection.selected
    then None
    else ExploreLinked.follow(view, now)

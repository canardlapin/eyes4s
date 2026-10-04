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

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.core.backend.{AnalysisRevision, TrialFixations, TrialKey, TrialPreview}
import eyes4s.studio.core.document.DatasetRevisionSpec

/** A toolbar toggle of Explore's trial view (Explore.dc.html, centre). */
enum TrialToggle derives CanEqual:
  /** Fixation marks. */
  case Points

  /** Order lines between fixation centres. */
  case Order

  /** The backend's preview map. */
  case Map

/** The backend's answer to a trial view request: the view, or the backend's
  * refusal, which asking again would only repeat. A platform failure to
  * reach the backend is the intent's `Left` instead, and can be retried.
  */
enum BackendAnswer[+A] derives CanEqual:
  case Answered(value: A)
  case Refused(reason: String)

/** A user action or platform fact Explore's trial view dispatches. */
enum TrialViewIntent derives CanEqual:
  case Switch(toggle: TrialToggle)

  /** Read again what the platform failed to read, and what is still
    * outstanding; a backend's refusal is not asked again.
    */
  case Retry

  /** The backend's admitted fixations of `trial` under `revision`, asked for
    * by ask `ask`.
    */
  case FixationsRead(
      revision: AnalysisRevision,
      trial: TrialKey,
      ask: Int,
      result: Either[String, BackendAnswer[TrialFixations]]
  )

  /** The backend's preview of `trial` under `revision`, asked for by `ask`. */
  case PreviewRead(
      revision: AnalysisRevision,
      trial: TrialKey,
      ask: Int,
      result: Either[String, BackendAnswer[TrialPreview]]
  )

  /** The trial displays of the revision's dataset, asked for by `ask`. */
  case DisplaysRead(revision: AnalysisRevision, ask: Int, result: Either[String, DisplaySource])

/** What the platform must do after an update of the trial view. */
enum TrialViewEffect derives CanEqual:
  case RequestFixations(revision: AnalysisRevision, trial: TrialKey, ask: Int)
  case RequestPreview(revision: AnalysisRevision, trial: TrialKey, ask: Int)
  case RequestDisplays(revision: AnalysisRevision, dataset: DatasetRevisionSpec, ask: Int)

/** Explore's trial view (ticket S6.2; Explore.dc.html, centre): the trial
  * Explore's trail is on, drawn with neutral marks under the analysis
  * revision of the run Explore shows, with the Points, Order and Map toggles.
  *
  * Its fixations, their placement and the preview map are the backend's
  * (protocol 1.6: [[TrialFixations]], [[TrialPreview]]); its display is the
  * revision's dataset's [[DisplaySource]]. The map is a preview of the
  * trial, labelled as not a result; Explore shows no score. `ask` numbers the
  * reads of the shown trial; an answer to an earlier ask is ignored.
  */
final case class ExploreTrialView(
    revision: Option[AnalysisRevision],
    dataset: Option[DatasetRevisionSpec],
    trial: Option[TrialKey],
    ask: Int,
    fixations: Loading[BackendAnswer[TrialFixations]],
    preview: Loading[BackendAnswer[TrialPreview]],
    displays: Loading[DisplaySource],
    points: Boolean,
    order: Boolean,
    map: Boolean
) derives CanEqual:
  def isOn(toggle: TrialToggle): Boolean = toggle match
    case TrialToggle.Points => points
    case TrialToggle.Order  => order
    case TrialToggle.Map    => map

object ExploreTrialView:

  /** Points, Order and Map on, as the board opens. */
  val empty: ExploreTrialView =
    ExploreTrialView(
      None,
      None,
      None,
      0,
      Loading.Idle,
      Loading.Idle,
      Loading.Idle,
      true,
      true,
      true
    )

  private val none: Vector[TrialViewEffect] = Vector.empty

  /** The analysis revision Explore shows: the shown run's. */
  def shownRevision(model: AppModel): Option[AnalysisRevision] =
    model.document.presentation.shownRun.flatMap(model.document.run).map(_.analysis)

  /** The dataset revision `revision` stands on. */
  private def datasetOf(model: AppModel, revision: AnalysisRevision) =
    model.document.analysis(revision).flatMap(a => model.document.dataset(a.dataset))

  private def reread(
      view: ExploreTrialView,
      fixations: Boolean,
      preview: Boolean,
      displays: Boolean
  ): (ExploreTrialView, Vector[TrialViewEffect]) =
    (view.revision, view.trial, view.dataset) match
      case (Some(r), Some(t), Some(d)) =>
        val next = view.ask + 1
        (
          view.copy(
            ask = next,
            fixations = if fixations then Loading.Waiting else view.fixations,
            preview = if preview then Loading.Waiting else view.preview,
            displays = if displays then Loading.Waiting else view.displays
          ),
          Option.when(fixations)(TrialViewEffect.RequestFixations(r, t, next)).toVector ++
            Option.when(preview)(TrialViewEffect.RequestPreview(r, t, next)) ++
            Option.when(displays)(TrialViewEffect.RequestDisplays(r, d, next))
        )
      case _ => (view, none)

  /** Follow Explore: its trial (the trail's) and the shown run's revision.
    * A change of either asks for the trial's fixations, preview and display;
    * the toggles stay as they are.
    */
  def sync(
      view: ExploreTrialView,
      model: AppModel
  ): (ExploreTrialView, Vector[TrialViewEffect]) =
    val revision = shownRevision(model)
    val trial    = TrialsNavigator.selected(model)
    val dataset  = revision.flatMap(datasetOf(model, _))
    if revision == view.revision && trial == view.trial && dataset == view.dataset then
      (view, none)
    else
      val reset = view.copy(
        revision = revision,
        dataset = dataset,
        trial = trial,
        fixations = Loading.Idle,
        preview = Loading.Idle,
        displays = Loading.Idle
      )
      reread(reset, true, true, true)

  /** The Elm-style update: pure; effects are data. */
  def update(
      view: ExploreTrialView,
      intent: TrialViewIntent
  ): (ExploreTrialView, Vector[TrialViewEffect]) =
    import TrialViewIntent.*
    intent match
      case Switch(TrialToggle.Points) => (view.copy(points = !view.points), none)
      case Switch(TrialToggle.Order)  => (view.copy(order = !view.order), none)
      case Switch(TrialToggle.Map)    => (view.copy(map = !view.map), none)
      case Retry                      =>
        if failed(view.fixations) || failed(view.preview) || failed(view.displays) then
          reread(
            view,
            unanswered(view.fixations),
            unanswered(view.preview),
            unanswered(view.displays)
          )
        else (view, none)
      case FixationsRead(r, t, n, result) =>
        if !answers(view, r, Some(t), n) then (view, none)
        else (view.copy(fixations = loaded(result)), none)
      case PreviewRead(r, t, n, result) =>
        if !answers(view, r, Some(t), n) then (view, none)
        else (view.copy(preview = loaded(result)), none)
      case DisplaysRead(r, n, result) =>
        if !answers(view, r, view.trial, n) then (view, none)
        else (view.copy(displays = loaded(result)), none)

  private def loaded[A](result: Either[String, A]): Loading[A] =
    result.fold(Loading.Failed(_), Loading.Ready(_))

  private def failed(read: Loading[?]): Boolean = read match
    case Loading.Failed(_) => true
    case _                 => false

  /** A read that failed, or whose answer is still outstanding. */
  private def unanswered(read: Loading[?]): Boolean = read match
    case Loading.Ready(_) => false
    case _                => true

  private def answers(
      view: ExploreTrialView,
      revision: AnalysisRevision,
      trial: Option[TrialKey],
      n: Int
  ): Boolean =
    view.revision.contains(revision) && view.trial == trial && view.ask == n

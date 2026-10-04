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

package eyes4s.studio.desktop.explore

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.explore.*
import eyes4s.studio.app.maps.{ColourLimits, MapOpacity, MapPalette}
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  BackendError,
  TrialFixations,
  TrialKey,
  TrialPreview
}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective}
import eyes4s.studio.desktop.runtime.StudioSession
import eyes4s.studio.desktop.trial.{MapRequest, StimulusSource, TrialView}
import eyes4s.studio.viz.trial.{
  MapCoverage,
  MarkStyle,
  ScreenRect,
  TrialFixation,
  TrialSceneInput,
  TrialSceneOptions
}
import javafx.application.Platform

/** Where Explore's trial view reads a trial's fixations, its preview map and
  * its dataset's displays. `done` may be called on any thread.
  */
trait TrialViewInputs:
  def fixations(
      revision: AnalysisRevision,
      trial: TrialKey,
      done: Either[String, BackendAnswer[TrialFixations]] => Unit
  ): Unit
  def preview(
      revision: AnalysisRevision,
      trial: TrialKey,
      done: Either[String, BackendAnswer[TrialPreview]] => Unit
  ): Unit
  def displays(dataset: DatasetRevisionSpec, done: Either[String, DisplaySource] => Unit): Unit

object TrialViewInputs:
  /** The window's backend (protocol 1.6), and `source` for the displays. */
  def of(session: StudioSession, source: NavigatorDisplays): TrialViewInputs =
    new TrialViewInputs:
      // A transport failure can be retried; the backend's refusal is its answer.
      private def answer[A](
          done: Either[String, BackendAnswer[A]] => Unit
      ): Either[Throwable, Either[BackendError, A]] => Unit = {
        case Left(e)          => done(Left(Option(e.getMessage).getOrElse(e.toString)))
        case Right(Left(err)) => done(Right(BackendAnswer.Refused(err.message)))
        case Right(Right(a))  => done(Right(BackendAnswer.Answered(a)))
      }
      def fixations(
          revision: AnalysisRevision,
          trial: TrialKey,
          done: Either[String, BackendAnswer[TrialFixations]] => Unit
      ): Unit = session.run(session.backend.trialFixations(revision, trial))(answer(done))
      def preview(
          revision: AnalysisRevision,
          trial: TrialKey,
          done: Either[String, BackendAnswer[TrialPreview]] => Unit
      ): Unit = session.run(session.backend.trialPreview(revision, trial))(answer(done))
      def displays(
          dataset: DatasetRevisionSpec,
          done: Either[String, DisplaySource] => Unit
      ): Unit = done(source.displays(dataset))

/** Explore's trial view on the desktop (Explore.dc.html, centre; see
  * [[ExploreTrialView]]): the toolbar, the trial on its stage (S4.3a's
  * [[TrialView]] with neutral marks and the preview map) and the legend. It
  * follows Explore's trial and the shown run, and reads the trial's views
  * from the backend. Use on the JavaFX thread.
  */
final class ExploreTrialViewHost(
    model: () => AppModel,
    inputs: TrialViewInputs,
    stimuli: StimulusSource
):
  private var view    = ExploreTrialView.empty
  private var started = false

  val trialView: TrialView                   = TrialView(stimuli)
  val pane: ExploreTrialViewPane             = ExploreTrialViewPane(dispatch, trialView)
  def node: javafx.scene.Node                = pane.node
  private var drawn: Option[TrialSceneInput] = None
  private var mapped: Option[MapRequest]     = None

  /** The view's state now. */
  def state: ExploreTrialView = view

  /** The pane's focus stops after its own (none until it has started). */
  def focusStops: Vector[eyes4s.studio.app.vm.FocusStop] =
    if !started then Vector.empty else ExploreTrialViewVM.focusStops(vm)

  /** The view-model now shown. */
  def vm: ExploreTrialViewVM = ExploreTrialViewVM.of(view, model())

  /** Follow the model. Nothing is read until Explore has been shown. */
  def sync(m: AppModel): Unit =
    started = started || m.perspective == Perspective.Explore
    if started then
      val (next, effects) = ExploreTrialView.sync(view, m)
      view = next
      perform(effects)
      render(m)

  /** A user action on the view, or a platform answer. */
  def dispatch(intent: TrialViewIntent): Unit =
    val (next, effects) = ExploreTrialView.update(view, intent)
    view = next
    perform(effects)
    render(model())

  def dispose(): Unit = trialView.dispose()

  private def render(m: AppModel): Unit =
    val vm               = ExploreTrialViewVM.of(view, m)
    val (input, refused) =
      vm.shown.fold((None, Vector.empty[String]))(s =>
        val (i, r) = ExploreTrialViewHost.sceneInput(s)
        (Some(i), r)
      )
    pane.render(vm, refused)
    if input != drawn then
      drawn = input
      input.fold(trialView.clear())(trialView.show)
    val request = vm.shown.flatMap(
      ExploreTrialViewHost.mapRequest(_, m.document.presentation.mapOpacity.value)
    )
    if request != mapped then
      mapped = request
      trialView.showMap(request)

  private def perform(effects: Vector[TrialViewEffect]): Unit =
    effects.foreach {
      case TrialViewEffect.RequestFixations(r, t, ask) =>
        inputs.fixations(
          r,
          t,
          a => Platform.runLater(() => dispatch(TrialViewIntent.FixationsRead(r, t, ask, a)))
        )
      case TrialViewEffect.RequestPreview(r, t, ask) =>
        inputs.preview(
          r,
          t,
          a => Platform.runLater(() => dispatch(TrialViewIntent.PreviewRead(r, t, ask, a)))
        )
      case TrialViewEffect.RequestDisplays(r, d, ask) =>
        inputs.displays(
          d,
          a => Platform.runLater(() => dispatch(TrialViewIntent.DisplaysRead(r, ask, a)))
        )
    }

object ExploreTrialViewHost:

  /** The preview map of a shown trial as the trial view draws it: over the
    * screen region its grid covers, at the document's opacity.
    */
  def mapRequest(shown: ShownTrialVM, opacity: Double): Option[MapRequest] =
    for
      map    <- shown.map
      alpha  <- MapOpacity.of(opacity)
      region <- ScreenRect
        .of(map.region.left, map.region.top, map.region.right, map.region.bottom)
        .toOption
    yield MapRequest(
      map.grid,
      ColourLimits.spanning(MapPalette.Mass, Vector(map.grid)),
      alpha,
      MapCoverage.Region(region)
    )

  /** The trial scene input of a shown trial: neutral marks (Explore never
    * shows roles), the toolbar's Points and Order, the window outline, and
    * the document's theme and stage; and the words of any mark the scene
    * refused, which is left out rather than losing the trial.
    */
  def sceneInput(shown: ShownTrialVM): (TrialSceneInput, Vector[String]) =
    val built = shown.marks.map(mk =>
      TrialFixation
        .of(shown.trial, mk.index, mk.screenX, mk.screenY, mk.durationMs, mk.placement)
        .left
        .map(_.message)
    )
    (
      TrialSceneInput(
        shown.display,
        shown.screen,
        built.collect { case Right(f) => f },
        MarkStyle.Neutral,
        shown.theme,
        shown.stage,
        options = TrialSceneOptions(points = shown.points, order = shown.order)
      ),
      built.collect { case Left(why) => why }
    )

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

import cats.syntax.all.*
import eyes4s.studio.app.AppModel
import eyes4s.studio.app.explore.*
import eyes4s.studio.app.maps.{ColourLimits, MapOpacity, MapPalette}
import eyes4s.studio.app.tokens.{StageVariant, Theme}
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  BackendError,
  TrialFixations,
  TrialKey,
  TrialPreview
}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective, StageAppearance}
import eyes4s.studio.desktop.runtime.StudioSession
import eyes4s.studio.desktop.trial.{MapRequest, StimulusSource, TrialView}
import eyes4s.studio.viz.trial.{MarkStyle, TrialFixation, TrialSceneInput, TrialSceneOptions}
import javafx.application.Platform

/** Where Explore's trial view reads a trial's fixations, its preview map and
  * its dataset's displays. `done` may be called on any thread.
  */
trait TrialViewInputs:
  def fixations(
      revision: AnalysisRevision,
      trial: TrialKey,
      done: Either[String, TrialFixations] => Unit
  ): Unit
  def preview(
      revision: AnalysisRevision,
      trial: TrialKey,
      done: Either[String, TrialPreview] => Unit
  ): Unit
  def displays(dataset: DatasetRevisionSpec, done: Either[String, DisplaySource] => Unit): Unit

object TrialViewInputs:
  /** The window's backend (protocol 1.6), and `source` for the displays. */
  def of(session: StudioSession, source: NavigatorDisplays): TrialViewInputs =
    new TrialViewInputs:
      private def answer[A](
          done: Either[String, A] => Unit
      ): Either[Throwable, Either[BackendError, A]] => Unit = {
        case Left(e)          => done(Left(Option(e.getMessage).getOrElse(e.toString)))
        case Right(Left(err)) => done(Left(err.message))
        case Right(Right(a))  => done(Right(a))
      }
      def fixations(
          revision: AnalysisRevision,
          trial: TrialKey,
          done: Either[String, TrialFixations] => Unit
      ): Unit = session.run(session.backend.trialFixations(revision, trial))(answer(done))
      def preview(
          revision: AnalysisRevision,
          trial: TrialKey,
          done: Either[String, TrialPreview] => Unit
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
  def vm: ExploreTrialViewVM = ExploreTrialViewVM.of(view)

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
    val vm = ExploreTrialViewVM.of(view)
    pane.render(vm)
    val input = vm.shown.flatMap(ExploreTrialViewHost.sceneInput(_, m))
    if input != drawn then
      drawn = input
      input.fold(trialView.clear())(trialView.show)
    val request = for
      s       <- vm.shown
      grid    <- s.map
      opacity <- MapOpacity.of(m.document.presentation.mapOpacity.value)
    yield MapRequest(grid, ColourLimits.spanning(MapPalette.Mass, Vector(grid)), opacity)
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

  /** The trial scene input of a shown trial: neutral marks (Explore never
    * shows roles), the toolbar's Points and Order, the window outline, and
    * the document's theme and stage. A fixation's duration is drawn to the
    * millisecond.
    */
  def sceneInput(shown: ShownTrialVM, m: AppModel): Option[TrialSceneInput] =
    val theme = m.document.presentation.theme match
      case eyes4s.studio.core.document.Theme.Light => Theme.Light
      case eyes4s.studio.core.document.Theme.Dark  => Theme.Dark
    val stage = m.document.presentation.stage match
      case StageAppearance.Dark  => StageVariant.Dark
      case StageAppearance.Mid   => StageVariant.Mid
      case StageAppearance.Light => StageVariant.Light
    shown.fixations
      .traverse(f =>
        TrialFixation.of(
          shown.trial,
          f.ref.index,
          f.screenX,
          f.screenY,
          math.max(1L, math.round(f.durationMs)).toInt,
          f.placement
        )
      )
      .toOption
      .map(fixations =>
        TrialSceneInput(
          shown.display,
          shown.screen,
          fixations,
          MarkStyle.Neutral,
          theme,
          stage,
          options = TrialSceneOptions(points = shown.points, order = shown.order)
        )
      )

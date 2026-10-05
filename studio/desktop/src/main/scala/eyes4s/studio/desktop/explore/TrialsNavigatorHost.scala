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

import eyes4s.studio.app.explore.*
import eyes4s.studio.app.vm.FocusStop
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{DatasetRevision, LedgerEntry, LedgerPages}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective}
import eyes4s.studio.core.fixture.GoldenAssets
import eyes4s.studio.core.assets.{AssetFile, AssetRef, AssetRegistry}
import eyes4s.studio.core.bundle.InputKind
import eyes4s.studio.desktop.runtime.{ProjectPort, StudioSession}

import scala.util.chaining.*
import javafx.application.Platform

/** Where the trials navigator reads a dataset revision's trials and their
  * displays. `done` may be called on any thread.
  */
trait NavigatorInputs:
  def entries(dataset: DatasetRevision, done: Either[String, Vector[LedgerEntry]] => Unit): Unit
  def displays(dataset: DatasetRevisionSpec, done: Either[String, DisplaySource] => Unit): Unit

/** Where a window's trial displays come from (injected): the golden
  * registry for a story session, or the project's own stored inventory and
  * stimulus images (S5.7). `done` may be called on any thread.
  */
trait NavigatorDisplays:
  def read(dataset: DatasetRevisionSpec, done: Either[String, DisplaySource] => Unit): Unit

/** Which backend a window's session runs on. */
enum SessionBackend derives CanEqual:
  /** FakeStudyBackend at a story moment, over fixtures/studio-golden. */
  case Story

  /** The eyes4s backend (S3.7). */
  case Real

object NavigatorDisplays:
  /** The display source of a session on `backend`: the golden registry for a
    * story session; for a real one, the open project's own (S5.7), or none
    * without a project.
    */
  def of(backend: SessionBackend, project: Option[ProjectPort]): NavigatorDisplays =
    backend match
      case SessionBackend.Story => golden
      case SessionBackend.Real  => project.fold(notServed)(stored)

  /** No display kinds: the trials are listed without them and nothing is
    * invented.
    */
  val notServed: NavigatorDisplays = (_, done) => done(Right(DisplaySource.NotServed))

  /** The story and fake sessions: fixtures/studio-golden's registry, served
    * only for a revision whose trial inventory is the golden trials.csv,
    * byte for byte; any other revision's display kinds are not served.
    */
  val golden: NavigatorDisplays = (dataset, done) =>
    done(
      if !GoldenAssets.describes(dataset) then Right(DisplaySource.NotServed)
      else GoldenAssets.registry(dataset).map(DisplaySource.Served(_))
    )

  /** A project's own displays (S5.7): the registry its stored trials.csv
    * states through the revision's display columns, with the stimulus
    * images it stores. A revision that maps no display columns, or has no
    * trial inventory, has none to serve; bytes that are not the source's
    * are refused ([[AssetRegistry.fromInventory]]).
    */
  def stored(project: ProjectPort): NavigatorDisplays = (dataset, done) =>
    (dataset.sources.trials, dataset.inventory.flatMap(_.displays)) match
      case (Some(trials), Some(_)) =>
        project.storedInputs {
          case Left(reason)  => done(Left(reason))
          case Right(inputs) =>
            val stimuli = inputs
              .filter(_.kind == InputKind.StimulusImage)
              .flatMap(e =>
                e.name.flatMap(n => AssetFile.of(n).toOption).map(AssetRef(_, e.sha256))
              )
            project.readInput(
              trials,
              _.flatMap(bytes =>
                AssetRegistry
                  .fromInventory(dataset, bytes, stimuli, Vector.empty)
                  .left
                  .map(_.message)
                  .map(DisplaySource.Served(_))
              ).pipe(done)
            )
        }
      case _ => done(Right(DisplaySource.NotServed))

object NavigatorInputs:
  /** The window's backend for the trials, and `source` for the displays. */
  def of(session: StudioSession, source: NavigatorDisplays): NavigatorInputs =
    new NavigatorInputs:
      def entries(
          dataset: DatasetRevision,
          done: Either[String, Vector[LedgerEntry]] => Unit
      ): Unit =
        session.run(LedgerPages.all(session.backend.ledger(dataset, _))) {
          case Left(e)          => done(Left(Option(e.getMessage).getOrElse(e.toString)))
          case Right(Left(err)) => done(Left(err.message))
          case Right(Right(es)) => done(Right(es))
        }
      def displays(
          dataset: DatasetRevisionSpec,
          done: Either[String, DisplaySource] => Unit
      ): Unit = source.read(dataset, done)

/** Explore's trials navigator on the desktop (Explore.dc.html, left; see
  * [[TrialsNavigator]]): the Trials pane and the Items pane over one state.
  * It follows the model's latest admitted dataset revision, reads its trials
  * and displays, and sends the explored trial to the app through `app`. Use
  * on the JavaFX thread.
  */
final class TrialsNavigatorHost(
    model: () => AppModel,
    app: Intent => Unit,
    inputs: NavigatorInputs
):
  private var navigator = TrialsNavigator.empty
  private var started   = false

  val trials: TrialsNavigatorView = TrialsNavigatorView(dispatch, NavigatorIntent.Filter(_))
  val items: TrialsNavigatorView = TrialsNavigatorView(dispatch, NavigatorIntent.FilterItems(_))

  /** The navigator's state now. */
  def state: TrialsNavigator = navigator

  def trialsVM: NavigatorPaneVM = TrialsNavigatorVM.trials(navigator, model())
  def itemsVM: NavigatorPaneVM  = TrialsNavigatorVM.items(navigator, model())

  /** A pane's focus stops after its own (none until it has started). */
  def trialsStops: Vector[FocusStop] =
    if !started then Vector.empty else TrialsNavigatorVM.focusStops(trialsVM)
  def itemsStops: Vector[FocusStop] =
    if !started then Vector.empty else TrialsNavigatorVM.focusStops(itemsVM)

  /** Follow the model. Nothing is read until Explore has been shown. */
  def sync(m: AppModel): Unit =
    started = started || m.perspective == Perspective.Explore
    if started then
      val (next, effects) = TrialsNavigator.sync(navigator, m)
      navigator = next
      perform(effects)
      render(m)

  /** A user action on a pane, or a platform answer. */
  def dispatch(intent: NavigatorIntent): Unit =
    val (next, effects) = TrialsNavigator.update(navigator, model(), intent)
    navigator = next
    perform(effects)
    render(model())

  private def render(m: AppModel): Unit =
    trials.render(TrialsNavigatorVM.trials(navigator, m))
    items.render(TrialsNavigatorVM.items(navigator, m))

  private def perform(effects: Vector[NavigatorEffect]): Unit =
    effects.foreach {
      case NavigatorEffect.App(i)                 => app(i)
      case NavigatorEffect.RequestEntries(d, ask) =>
        inputs.entries(
          d,
          r => Platform.runLater(() => dispatch(NavigatorIntent.EntriesRead(d, ask, r)))
        )
      case NavigatorEffect.RequestDisplays(spec, ask) =>
        inputs.displays(
          spec,
          r => Platform.runLater(() => dispatch(NavigatorIntent.DisplaysRead(spec.id, ask, r)))
        )
    }

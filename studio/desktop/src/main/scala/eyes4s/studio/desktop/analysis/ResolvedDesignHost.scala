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

package eyes4s.studio.desktop.analysis

import cats.effect.IO
import eyes4s.studio.app.analysis.*
import eyes4s.studio.app.vm.FocusStop
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{AnalysisRevision, BackendError, PageRequest, PreviewPage}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.preview.{PreviewBudget, PreviewEvent, PreviewId}
import eyes4s.studio.desktop.runtime.StudioSession
import fs2.Stream
import javafx.application.Platform

/** Where the resolved-design pane asks the backend for its preview and its
  * rows. Callbacks may run on any thread; `ended` follows the last event of
  * a bounded page (and a refusal).
  */
trait DesignInputs:
  def preview(
      revision: AnalysisRevision,
      budget: PreviewBudget,
      event: Either[String, PreviewEvent] => Unit,
      ended: () => Unit
  ): Unit

  def continuePreview(
      id: PreviewId,
      budget: PreviewBudget,
      event: Either[String, PreviewEvent] => Unit,
      ended: () => Unit
  ): Unit

  def rows(
      revision: AnalysisRevision,
      page: PageRequest,
      done: Either[String, PreviewPage] => Unit
  ): Unit

object DesignInputs:
  /** The window's backend. */
  def of(session: StudioSession): DesignInputs =
    new DesignInputs:
      private def drain(
          stream: Stream[IO, Either[BackendError, PreviewEvent]],
          event: Either[String, PreviewEvent] => Unit,
          ended: () => Unit
      ): Unit =
        session.run(stream.evalMap(e => IO(event(e.left.map(_.message)))).compile.drain) {
          case Left(e) =>
            event(Left(Option(e.getMessage).getOrElse(e.toString)))
            ended()
          case Right(()) => ended()
        }

      def preview(
          revision: AnalysisRevision,
          budget: PreviewBudget,
          event: Either[String, PreviewEvent] => Unit,
          ended: () => Unit
      ): Unit = drain(session.backend.previewCounting(revision, budget), event, ended)

      def continuePreview(
          id: PreviewId,
          budget: PreviewBudget,
          event: Either[String, PreviewEvent] => Unit,
          ended: () => Unit
      ): Unit = drain(session.backend.continuePreview(id, budget), event, ended)

      def rows(
          revision: AnalysisRevision,
          page: PageRequest,
          done: Either[String, PreviewPage] => Unit
      ): Unit =
        session.run(session.backend.previewRows(revision, page)) {
          case Left(e)          => done(Left(Option(e.getMessage).getOrElse(e.toString)))
          case Right(Left(err)) => done(Left(err.message))
          case Right(Right(p))  => done(Right(p))
        }

/** The Analysis perspective's resolved-design pane on the desktop
  * (Analysis.dc.html, resolved design; see [[ResolvedDesign]]). It follows
  * the model's target revision, asks the backend for its preview a bounded
  * page of participants at a time and for its rows a page at a time, and
  * renders the table. Its app intents (opening a trial, the prepared
  * design) go to `app`. Use on the JavaFX thread.
  */
final class ResolvedDesignHost(app: Intent => Unit, inputs: DesignInputs):
  private var panel   = ResolvedDesign.empty
  private var started = false

  val view: ResolvedDesignView = ResolvedDesignView(dispatch)
  def node: javafx.scene.Node  = view.node

  /** The pane's state now. */
  def state: ResolvedDesign = panel

  /** The pane's focus stops after its own: the choosable chips, then the table. */
  def focusStops: Vector[FocusStop] = ResolvedDesignVM.of(panel).focusStops

  /** Follow the model. Nothing is asked of the backend until the Analysis
    * perspective has been shown.
    */
  def sync(m: AppModel): Unit =
    started = started || m.perspective == Perspective.Analysis
    if started then
      val (next, effects) = ResolvedDesign.sync(panel, m)
      panel = next
      perform(effects)
      view.render(ResolvedDesignVM.of(panel))

  /** A user action or a backend answer. */
  def dispatch(intent: DesignIntent): Unit =
    val (next, effects) = ResolvedDesign.update(panel, intent)
    panel = next
    perform(effects)
    view.render(ResolvedDesignVM.of(panel))

  private def later(intent: DesignIntent): Unit = Platform.runLater(() => dispatch(intent))

  private def perform(effects: Vector[DesignEffect]): Unit =
    effects.foreach {
      case DesignEffect.App(i)                            => app(i)
      case DesignEffect.StartPreview(g, revision, budget) =>
        inputs.preview(revision, budget, answer(g), () => later(DesignIntent.PageEnded(g)))
      case DesignEffect.ContinuePreview(g, id, budget) =>
        inputs.continuePreview(id, budget, answer(g), () => later(DesignIntent.PageEnded(g)))
      case DesignEffect.ReadRows(g, revision, page) =>
        inputs.rows(revision, page, r => later(DesignIntent.RowsRead(g, r)))
    }

  private def answer(generation: Long)(event: Either[String, PreviewEvent]): Unit =
    later(
      event.fold(
        DesignIntent.PreviewRefused(generation, _),
        DesignIntent.Previewed(generation, _)
      )
    )

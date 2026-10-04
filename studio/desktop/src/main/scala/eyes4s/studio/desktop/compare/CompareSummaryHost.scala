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

package eyes4s.studio.desktop.compare

import cats.effect.IO
import eyes4s.studio.app.compare.*
import eyes4s.studio.app.plot.{ParticipantColumns, PlotSource, ProfileColumns}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{PageRequest, QueryRow, RunId}
import eyes4s.studio.core.selection.ViewId
import eyes4s.studio.desktop.plot.{PlotTwin, TableTwinView}
import eyes4s.studio.desktop.runtime.StudioSession
import eyes4s.studio.viz.plot.{ParticipantPlot, ScaleProfilePlot}
import javafx.application.Platform
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label, ToggleButton, ToggleGroup, Tooltip}
import javafx.scene.layout.{HBox, Priority, Region, VBox}

/** Where the summary layout asks for a run's summary and queries. `done`
  * may be called on any thread.
  */
trait SummaryInputs:
  def summary(run: RunId, done: SummaryAnswer => Unit): Unit
  def queries(run: RunId, done: QueriesAnswer => Unit): Unit

object SummaryInputs:

  /** The window's backend: the summary, and every page of the queries. */
  def of(session: StudioSession): SummaryInputs =
    new SummaryInputs:
      def summary(run: RunId, done: SummaryAnswer => Unit): Unit =
        session.run(session.backend.result(run)) {
          case Left(e)          => done(SummaryAnswer.Failed(reason(e)))
          case Right(Left(err)) => done(SummaryAnswer.Refused(err))
          case Right(Right(s))  => done(SummaryAnswer.Answered(s))
        }
      def queries(run: RunId, done: QueriesAnswer => Unit): Unit =
        def from(offset: Int, got: Vector[QueryRow]): IO[Either[String, Vector[QueryRow]]] =
          PageRequest.of(offset, PageRequest.MaximumSize) match
            case Left(e)        => IO.pure(Left(e.message))
            case Right(request) =>
              session.backend.queries(run, request).flatMap {
                case Left(err)   => IO.pure(Left(err.message))
                case Right(page) =>
                  val rows = got ++ page.rows
                  page.page.next.filter(_ > offset).fold(IO.pure(Right(rows)))(from(_, rows))
              }
        session.run(from(0, Vector.empty)) {
          case Left(e)           => done(QueriesAnswer.Failed(reason(e)))
          case Right(Left(why))  => done(QueriesAnswer.Failed(why))
          case Right(Right(all)) => done(QueriesAnswer.Answered(all))
        }

  private def reason(e: Throwable): String = Option(e.getMessage).getOrElse(e.toString)

/** Compare's summary layout on the desktop (ticket S8.6; Results.dc.html):
  * the participant plot and the scale profile, each with its Table tab, the
  * participant table with the σ selector, the notes, the newer run's
  * freshness and Explain, and the query table. It follows the model
  * ([[CompareSummary.sync]]), asks the backend through `inputs`, and binds
  * [[CompareSummaryVM]] to the panes; Explain sends the view-model's
  * intents to the app. Every view is a view of the selection bus. Use on
  * the JavaFX thread.
  */
final class CompareSummaryHost(
    model: () => AppModel,
    app: Intent => Unit,
    inputs: SummaryInputs
):
  private var state: CompareSummary                           = CompareSummary.empty
  private var shown: Map[String, (Option[PlotSource], Theme)] = Map.empty
  private var disposed: Boolean                               = false

  private def view(id: String): ViewId =
    ViewId.of(id).fold(e => throw IllegalStateException(e.message), identity)

  private val selection = model().selection

  /** The participant plot and its table. */
  val participantPlot: PlotTwin = PlotTwin
    .attach(
      ParticipantPlot(
        ParticipantColumns.standard.fold(e => throw IllegalStateException(e.message), identity)
      ),
      view("compare.participant-plot"),
      view("compare.participant-plot.table"),
      selection,
      app
    )
    .fold(e => throw IllegalStateException(e.message), identity)

  /** The scale profile and its table. */
  val scaleProfile: PlotTwin = PlotTwin
    .attach(
      ScaleProfilePlot(
        ProfileColumns.standard.fold(e => throw IllegalStateException(e.message), identity)
      ),
      view("compare.scale-profile"),
      view("compare.scale-profile.table"),
      selection,
      app
    )
    .fold(e => throw IllegalStateException(e.message), identity)

  /** The participant table and the query table. */
  val participantTable: TableTwinView =
    TableTwinView.attach(view("compare.participant-table"), selection, app)
  val queryTable: TableTwinView =
    TableTwinView.attach(view("compare.query-table"), selection, app)

  private val status    = Label()
  private val freshness = Label()
  private val scales    = HBox(4.0)
  private val notes     = VBox(2.0)
  private val explain   = Button()
  private val keeps     = Label()
  status.getStyleClass.addAll("summary-status", "t12")
  freshness.getStyleClass.addAll("summary-freshness", "t11")
  notes.getStyleClass.add("summary-notes")
  keeps.getStyleClass.addAll("summary-keeps", "t11")
  explain.getStyleClass.add("summary-explain")
  explain.setOnAction(_ => explainNow())

  private val spacer = Region()
  HBox.setHgrow(spacer, Priority.ALWAYS)
  private val toolbar = HBox(8.0, scales, spacer, freshness)
  toolbar.setAlignment(Pos.CENTER_LEFT)
  toolbar.getStyleClass.add("summary-toolbar")
  private val explainRow = HBox(8.0, explain, keeps)
  explainRow.setAlignment(Pos.CENTER_LEFT)
  VBox.setVgrow(participantTable, Priority.ALWAYS)

  /** The participant table pane: σ selector and freshness, the notes and
    * Explain above the table.
    */
  val participantNode: VBox = VBox(4.0, toolbar, status, notes, explainRow, participantTable)
  participantNode.getStyleClass.add("compare-summary")

  /** The state now. */
  def summary: CompareSummary = state

  /** The view-model now shown. */
  def vm: CompareSummaryVM = CompareSummaryVM.of(state, model())

  /** Follows the model: a newly shown run is read; the views show the bus's selection. */
  def sync(m: AppModel): Unit = if !disposed then
    val (next, effects) = CompareSummary.sync(state, m)
    state = next
    perform(effects)
    participantPlot.project(m.selection)
    scaleProfile.project(m.selection)
    participantTable.project(m.selection)
    queryTable.project(m.selection)
    render(m)

  /** A backend answer or a user action. */
  def dispatch(intent: SummaryIntent): Unit = if !disposed then
    val (next, effects) = CompareSummary.update(state, intent)
    state = next
    perform(effects)
    render(model())

  /** What Explain does now: the view-model's intents, in order. */
  def explainNow(): Unit = vm.explain.foreach(_.intents.foreach(app))

  private def perform(effects: Vector[SummaryEffect]): Unit =
    effects.foreach {
      case SummaryEffect.App(i)              => app(i)
      case SummaryEffect.RequestSummary(run) =>
        inputs.summary(
          run,
          a => Platform.runLater(() => dispatch(SummaryIntent.SummaryRead(run, a)))
        )
      case SummaryEffect.RequestQueries(run) =>
        inputs.queries(
          run,
          a => Platform.runLater(() => dispatch(SummaryIntent.QueriesRead(run, a)))
        )
    }

  private def render(m: AppModel): Unit =
    if !disposed then
      val v     = CompareSummaryVM.of(state, m)
      val theme = m.document.presentation.theme match
        case eyes4s.studio.core.document.Theme.Light => Theme.Light
        case eyes4s.studio.core.document.Theme.Dark  => Theme.Dark
      // The run's status, then why any part could not be drawn.
      val parts = Vector(v.participantPlot, v.profile, v.participants, v.queries)
        .flatMap(_.flatMap(_.left.toOption))
      val said = v.status.toVector ++ parts
      status.setText(said.mkString("\n"))
      status.setVisible(said.nonEmpty)
      status.setManaged(said.nonEmpty)
      freshness.setText(v.freshness.getOrElse(""))
      notes.getChildren.setAll(v.notes.map { n =>
        val l = Label(n); l.getStyleClass.add("t11"); l
      }*)
      val group = ToggleGroup()
      scales.getChildren.setAll(v.scales.map { c =>
        val b = ToggleButton(c.label)
        // A σ that cannot be chosen says why, to the pointer and to a reader.
        val why = c.unavailable.fold(c.label)(r => s"${c.label}: $r")
        b.setAccessibleText(why)
        c.unavailable.foreach(r => b.setTooltip(Tooltip(r)))
        b.setToggleGroup(group)
        b.setSelected(c.chosen)
        b.setDisable(!c.available)
        b.setOnAction(_ => dispatch(SummaryIntent.ChooseScale(c.scale)))
        b
      }*)
      v.explain match
        case Some(e) =>
          explain.setText(e.label); keeps.setText(e.keeps)
          explain.setAccessibleText(s"${e.label} ${e.keeps}")
          explain.setVisible(true); keeps.setVisible(true)
        case None =>
          explain.setVisible(false); keeps.setVisible(false)
      show("participant-plot", v.participantPlot, theme)(
        participantPlot.show(_, theme),
        participantPlot.clear()
      )
      show("scale-profile", v.profile, theme)(scaleProfile.show(_, theme), scaleProfile.clear())
      show("participant-table", v.participants, theme)(
        participantTable.show,
        participantTable.clear()
      )
      show("query-table", v.queries, theme)(queryTable.show, queryTable.clear())

  // Draws a part when its source or the theme changes. A part with no source
  // (the run is still being read) or one that could not be built is cleared,
  // so no earlier run's values stay under the new run's status.
  private def show(part: String, source: Option[Either[String, PlotSource]], theme: Theme)(
      into: PlotSource => Unit,
      clear: => Unit
  ): Unit =
    val now = source.flatMap(_.toOption)
    if shown.get(part) != Some((now, theme)) then
      shown = shown.updated(part, (now, theme))
      now.fold(clear)(into)

  /** Disposes the views; the host ignores the model from then on. Idempotent. */
  def dispose(): Unit =
    if !disposed then
      disposed = true
      participantPlot.dispose()
      scaleProfile.dispose()
      participantTable.dispose()
      queryTable.dispose()

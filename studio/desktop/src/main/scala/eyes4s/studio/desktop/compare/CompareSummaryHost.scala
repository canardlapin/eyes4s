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
import eyes4s.studio.app.plot.{LadderColumns, SlopeColumns, SlopeFrame, PlotSource, ScaleLadder}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{
  DensityGrid,
  PageRequest,
  QueryRow,
  ResultAddress,
  RunId,
  TrialKey
}
import eyes4s.studio.app.maps.{LimitsScope, MapId}
import eyes4s.studio.app.text.{PanelText, PanelTextId}
import eyes4s.studio.core.document.ReportingSpec
import eyes4s.studio.core.selection.{ScaleIndex, ViewId}
import eyes4s.studio.desktop.plot.{PlotTwin, TableTwinView}
import eyes4s.studio.desktop.runtime.StudioSession
import eyes4s.studio.desktop.tokens.TokenFiles
import eyes4s.studio.viz.plot.{ParticipantPlot, ScaleLadderPlot, SlopegraphPlot}
import javafx.application.Platform
import javafx.beans.value.ChangeListener
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{HBox, Priority, VBox}

/** Where the summary layout asks for a run's summary and queries. `done`
  * may be called on any thread.
  */
trait SummaryInputs:
  def mapGrid(id: MapId.Result, done: Either[String, DensityGrid] => Unit): Unit =
    done(Left(PanelText(PanelTextId.MapNotServed, id.trial.label, id.scale.value.toString)))
  def summary(run: RunId, done: SummaryAnswer => Unit): Unit
  def queries(run: RunId, done: QueriesAnswer => Unit): Unit
  def report(
      run: RunId,
      reporting: ReportingSpec,
      scale: ScaleIndex,
      done: ReportAnswer => Unit
  ): Unit =
    done(
      ReportAnswer.Failed(
        s"Report ${reporting.id.value} for $run at scale ${scale.value} is not served."
      )
    )

  /** Inspects one result item: the trial panels' pair (S8.2). */
  def inspect(run: RunId, address: ResultAddress, done: PairAnswer => Unit): Unit

  /** Reads every pair row of a run at one scale (S8.5, protocol 1.9). */
  def pairs(run: RunId, scale: ScaleIndex, done: PairsAnswer => Unit): Unit

  /** Reads a query's scale ladder at the run's scales (S8.3). */
  def ladder(
      run: RunId,
      query: TrialKey,
      scales: Vector[String],
      done: LadderAnswer => Unit
  ): Unit

object SummaryInputs:

  /** The window's backend: the summary, and every page of the queries. */
  def of(session: StudioSession): SummaryInputs =
    new SummaryInputs:
      override def mapGrid(id: MapId.Result, done: Either[String, DensityGrid] => Unit): Unit =
        session.run(session.reads.mapGrid(id.run, id.scale.value, id.trial)) {
          case Left(e)       => done(Left(reason(e)))
          case Right(answer) => done(answer.left.map(_.message))
        }
      def summary(run: RunId, done: SummaryAnswer => Unit): Unit =
        session.run(session.reads.result(run)) {
          case Left(e)          => done(SummaryAnswer.Failed(reason(e)))
          case Right(Left(err)) => done(SummaryAnswer.Refused(err))
          case Right(Right(s))  => done(SummaryAnswer.Answered(s))
        }
      override def report(
          run: RunId,
          reporting: ReportingSpec,
          scale: ScaleIndex,
          done: ReportAnswer => Unit
      ): Unit =
        session.run(session.reads.report(run, reporting, scale.value)) {
          case Left(e)          => done(ReportAnswer.Failed(reason(e)))
          case Right(Left(err)) => done(ReportAnswer.Refused(err))
          case Right(Right(v))  => done(ReportAnswer.Answered(v))
        }
      def inspect(run: RunId, address: ResultAddress, done: PairAnswer => Unit): Unit =
        session.run(session.reads.inspect(run, address)) {
          case Left(e)          => done(PairAnswer.Failed(reason(e)))
          case Right(Left(err)) => done(PairAnswer.Refused(err))
          case Right(Right(i))  => done(PairAnswer.Answered(i))
        }
      def ladder(
          run: RunId,
          query: TrialKey,
          scales: Vector[String],
          done: LadderAnswer => Unit
      ): Unit =
        session.run(
          ScaleLadder.load[IO](session.reads.inspect, session.navigator)(run, query, scales)
        ) {
          case Left(e)            => done(LadderAnswer.Failed(reason(e)))
          case Right(Left(err))   => done(LadderAnswer.Failed(err.message))
          case Right(Right(full)) => done(LadderAnswer.Answered(full))
        }
      def pairs(run: RunId, scale: ScaleIndex, done: PairsAnswer => Unit): Unit =
        session.run(
          eyes4s.studio.core.figures.MethodsReads
            .pairRowsAt[IO](session.reads.pairRows, run, scale.value)
        ) {
          case Left(e)             => done(PairsAnswer.Broken(reason(e)))
          case Right(Left(err))    => done(PairsAnswer.Failed(err))
          case Right(Right(pages)) => done(PairsAnswer.Answered(pages.flatMap(_.rows)))
        }
      def queries(run: RunId, done: QueriesAnswer => Unit): Unit =
        def from(offset: Int, got: Vector[QueryRow]): IO[Either[String, Vector[QueryRow]]] =
          PageRequest.of(offset, PageRequest.MaximumSize) match
            case Left(e)        => IO.pure(Left(e.message))
            case Right(request) =>
              session.reads.queries(run, request).flatMap {
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

object CompareSummaryHost:

  /** The classpath resource of the Results headline's stylesheet. */
  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-results.css"

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
    inputs: SummaryInputs,
    sources: PanelSources = PanelSources.notServed
):
  private var state: CompareSummary                           = CompareSummary.empty
  private var shown: Map[String, (Option[PlotSource], Theme)] = Map.empty
  private var disposed: Boolean                               = false

  private def view(id: String): ViewId =
    ViewId.of(id).fold(e => throw IllegalStateException(e.message), identity)

  private val selection         = model().selection
  private var navigator         = QueriesNavigator.initial
  private var participantsState = eyes4s.studio.app.compare.ParticipantsNavigator.empty
  private var panelState        = TrialPanels.empty
  private var contrast          = ContrastPane.empty
  private var pairsState        = PairsTable.empty

  private val slopeColumns: SlopeColumns =
    SlopeColumns.standard.fold(e => throw IllegalStateException(e.message), identity)

  // What the slopegraphs were last built with beyond their source.
  private var slopes: SlopeFrame = SlopeFrame.empty

  /** The slopegraphs at every σ with Δ by σ (overhaul v4), and their table. */
  val participantPlot: PlotTwin = PlotTwin
    .attach(
      SlopegraphPlot(slopeColumns),
      view("compare.participant-plot"),
      view("compare.participant-plot.table"),
      selection,
      app
    )
    .fold(e => throw IllegalStateException(e.message), identity)

  // The lines the annotation column has room for: the participant plot's
  // frame as drawn now. A resize re-renders only when that count changes.
  private var noteSlots: Int = ParticipantPlot.noteSlots(participantPlot.plotHost.getHeight)
  private val plotResized: ChangeListener[Number] = (_, _, height) =>
    val slots = ParticipantPlot.noteSlots(height.doubleValue)
    if !disposed && slots != noteSlots then
      noteSlots = slots
      render(model())
  participantPlot.plotHost.heightProperty.addListener(plotResized)

  /** The summary's chrome: headline, reporting menu, legend and facts. */
  val chrome: SummaryChrome = SummaryChrome(
    app,
    scale => dispatch(SummaryIntent.ChooseScale(scale)),
    () => app(Intent.FocusPane(eyes4s.studio.app.layout.StudioLayouts.reporting))
  )

  /** The Results headline over the slopegraphs, hidden when the shown report
    * declares no contrast.
    */
  val headline: javafx.scene.layout.HBox = chrome.headline
  Option(getClass.getClassLoader.getResource(CompareSummaryHost.stylesheetResource))
    .foreach(url => headline.getStylesheets.add(url.toExternalForm))
  headline.setVisible(false)
  headline.setManaged(false)

  /** The slopegraphs' pane: the headline, the plot at its natural height,
    * then its legend.
    */
  val participantPlotNode: VBox = VBox(headline, participantPlot.plotNode, chrome.legend)
  participantPlot.plotNode.setMinHeight(SlopegraphPlot.HeightPx)
  participantPlot.plotNode.setPrefHeight(SlopegraphPlot.HeightPx)
  VBox.setVgrow(participantPlot.plotNode, Priority.ALWAYS)
  Option(getClass.getClassLoader.getResource(CompareSummaryHost.stylesheetResource))
    .foreach(url => participantPlotNode.getStylesheets.add(url.toExternalForm))

  /** The participant table. */
  // The Results board pins the selected participant's row at the top
  // ("pinned · selected"; bead bd-01M44PD9XEHG6QWPCJATAJZ15W).
  val participantTable: TableTwinView =
    TableTwinView.attach(view("compare.participant-table"), selection, app, pinsSelected = true)

  private val status = Label()
  status.getStyleClass.addAll("summary-status", "t12")
  VBox.setVgrow(participantTable, Priority.ALWAYS)

  /** The participant table pane: why a part could not be drawn, the table,
    * then the facts under it.
    */
  val participantNode: VBox = VBox(status, participantTable, chrome.footer)
  participantNode.getStyleClass.add("compare-summary")
  Option(getClass.getClassLoader.getResource(CompareSummaryHost.stylesheetResource))
    .foreach { url =>
      participantNode.getStylesheets.add(url.toExternalForm)
      chrome.reporting.getStylesheets.add(url.toExternalForm)
    }

  private var boardVM: SummaryBoardVM = SummaryBoard.empty

  /** The summary chrome's words as last rendered. */
  def board: SummaryBoardVM = boardVM

  /** Compare's Queries and Items navigators (S8.1), on the same run's answers. */
  val queries: QueriesNavigatorView =
    QueriesNavigatorView(
      NavigatorKind.Queries,
      toggle,
      keyed(NavigatorKind.Queries),
      filterQueries,
      app
    )
  val items: QueriesNavigatorView =
    QueriesNavigatorView(
      NavigatorKind.Items,
      toggle,
      keyed(NavigatorKind.Items),
      filterQueries,
      app
    )

  val participants: ParticipantsNavigatorView = ParticipantsNavigatorView(
    key =>
      if !disposed then {
        val nav            = eyes4s.studio.app.compare.ParticipantsNavigator
        val current        = nav.vm(participantsState, state, model())
        val (next, intent) = nav.key(participantsState, current, key)
        participantsState = next
        render(model())
        intent.foreach(app)
      },
    row =>
      if !disposed then {
        participantsState = participantsState.copy(cursor = Some(row.ref))
        app(row.open)
      }
  )

  private def toggle(key: String, open: Boolean): Unit = if !disposed then
    navigator = QueriesNavigator.toggle(navigator, key, open)
    render(model())

  private def keyed(kind: NavigatorKind)(key: NavigatorKey): Unit = if !disposed then
    val (next, intent) = QueriesNavigator.key(navigator, navigatorVM, kind, key)
    navigator = next
    render(model())
    intent.foreach(app)

  private def filterQueries(text: String): Unit = if !disposed then
    navigator = QueriesNavigator.filter(navigator, text)
    render(model())

  /** Compare's query and reference trial panels (S8.2), on the run's rows. */
  val panels: TrialPanelsView =
    TrialPanelsView(
      app,
      on => panelIntent(PanelsIntent.Underlay(on)),
      () =>
        maps.retry()
        panelIntent(PanelsIntent.Retry)
      ,
      sources.stimuli,
      (side, which, on) => panelIntent(PanelsIntent.Overlay(side, which, on))
    )

  private val maps                           = CompareMapHost(inputs, panels)
  def chooseLimits(scope: LimitsScope): Unit = maps.chooseLimits(scope)

  /** What Compare's inspector reads of these views (S8.4). */
  def inspectorInputs: InspectorInputs =
    val scale  = panelState.focus.map(_.scale).orElse(state.shown)
    val report = scale.flatMap(scale => state.reports.get((scale, false))).collect {
      case ReportAnswer.Answered(view) => view
    }
    InspectorInputs(panelState, shownRun, state.answered, state.reporting, report)

  // Called after each render, so panes beside these views follow their answers.
  private var rendered: Vector[() => Unit] = Vector.empty

  /** Calls `f` after every render of these views. */
  def onRendered(f: () => Unit): Unit = rendered = rendered :+ f

  /** The run Compare shows, with its revision and rows, once both are read. */
  private def shownRun: Option[ShownRun] =
    for
      run <- state.run
      sum <- state.answered
    yield ShownRun(run, sum.revision, rows)

  /** The trial panels' Table tabs: each shown trial's fixations. */
  val queryTrialTable: TableTwinView =
    TableTwinView.attach(view("compare.query-trial.table"), selection, app)
  val referenceTrialTable: TableTwinView =
    TableTwinView.attach(view("compare.reference-trial.table"), selection, app)

  /** The pair's Table (overhaul v4): both trials' fixations, query first. */
  val pairTables: javafx.scene.layout.VBox =
    val v = javafx.scene.layout.VBox(queryTrialTable, referenceTrialTable)
    Vector(queryTrialTable, referenceTrialTable)
      .foreach(t => javafx.scene.layout.VBox.setVgrow(t, javafx.scene.layout.Priority.ALWAYS))
    v

  private def rows: Vector[QueryRow] = state.queries match
    case Some(QueriesAnswer.Answered(rs)) => rs
    case _                                => Vector.empty

  private def scaleLabels: Vector[String] = state.answered.fold(Vector.empty)(_.scaleLabels)

  private def panelIntent(intent: PanelsIntent): Unit = if !disposed then
    panelState = TrialPanels.update(panelState, intent)
    // Retry forgets what failed; the sync asks for it again.
    if intent == PanelsIntent.Retry then syncPanels(model())
    render(model())

  private val ladderColumns: LadderColumns =
    LadderColumns.standard.fold(e => throw IllegalStateException(e.message), identity)

  /** The contrast pane's scale ladder and its table (S8.3). A pick of a
    * pair in the ladder also opens it, so the reference panel follows.
    */
  val ladder: PlotTwin = PlotTwin
    .attach(
      ScaleLadderPlot(ladderColumns, None),
      view("compare.contrast"),
      view("compare.contrast.table"),
      selection,
      picked
    )
    .fold(e => throw IllegalStateException(e.message), identity)

  // The ladder focuses the trail's scale; a new focus rebuilds the plot.
  private var ladderFocus: Option[String] = None

  private def picked(intent: Intent): Unit =
    app(intent)
    intent match
      case Intent.Select(input) =>
        input.refs match
          case Vector(ref @ eyes4s.studio.core.selection.StudioRef.Pair(_, _, _, focal, _))
              if panelState.focus.exists(_.query == focal) =>
            app(Intent.Explain(eyes4s.studio.app.nav.Place.At(ref)))
          case _ => ()
      case _ => ()

  /** The pairs table (S8.5): every pair row of the run at the trail's
    * scale, virtualised, with Retry after a failed read.
    */
  val pairsTable: TableTwinView =
    TableTwinView.attach(view("compare.pairs"), selection, app)
  private val pairsStatus = Label()
  pairsStatus.getStyleClass.addAll("summary-status", "t12")
  pairsStatus.setWrapText(true)
  private val pairsRetry = Button()
  pairsRetry.getStyleClass.add("btn")
  pairsRetry.setOnAction(_ =>
    val (next, reads) = PairsTable.retry(pairsState)
    pairsState = next
    readPairs(reads)
    render(model())
  )
  VBox.setVgrow(pairsTable, Priority.ALWAYS)

  /** The pairs table pane: its status, Retry, and the table. */
  val pairsNode: VBox = VBox(4.0, pairsStatus, pairsRetry, pairsTable)

  /** The pairs table's view-model now; the source it builds is kept, so the
    * next render that changes neither the answer nor the rows reuses it.
    */
  def pairsVM: PairsVM =
    val focus         = panelState.focus
    val (kept, built) = PairsTable.view(
      pairsState,
      focus,
      focus.flatMap(f => TrialPanels.referenceOf(f, rows)),
      rows,
      scaleLabels
    )
    pairsState = kept
    built

  private def readPairs(reads: Vector[PairsEffect]): Unit =
    reads.foreach { case PairsEffect.ReadPairs(run, scale) =>
      inputs.pairs(
        run,
        scale,
        a =>
          Platform.runLater { () =>
            if !disposed then
              pairsState = PairsTable.read(pairsState, run, scale, a)
              render(model())
          }
      )
    }

  /** The contrast readout beside the ladder. */
  val readout: ContrastReadoutView = ContrastReadoutView(app, () => retryLadder())

  /** The Across scales pane: the ladder (overhaul v4); its readout leads
    * the inspector.
    */
  val contrastNode: HBox =
    HBox.setHgrow(ladder.plotNode, Priority.ALWAYS)
    val box = HBox(ladder.plotNode)
    box.setMinHeight(0.0)
    box

  /** The readout's stops, in the inspector it leads: Prev and Next. */
  def readoutStops: Vector[eyes4s.studio.app.vm.FocusStop] = ContrastPane.focusStops(contrastVM)

  /** The stops inside the contrast pane: the ladder, one stop with a roving
    * cursor named by the plot it draws, while it shows one, then Prev and Next.
    */
  def contrastStops: Vector[eyes4s.studio.app.vm.FocusStop] =
    ladder.focusStops

  /** Complete Results contracts, derived from served view-models and portable
    * plot/table cursor state. No native role/name introspection is involved.
    */
  def focusPlans: Map[eyes4s.studio.app.layout.PaneId, eyes4s.studio.app.vm.PaneFocus] =
    val contents = Map(
      "compare.participants" -> Vector.empty[eyes4s.studio.app.vm.FocusStop],
      // The reporting menu in the plot's header, then the plot's own stop.
      "compare.participant-plot" -> (Option
        .when(boardVM.reporting.nonEmpty)(
          eyes4s.studio.app.vm
            .FocusStop(eyes4s.studio.app.vm.A11yRole.MenuButton, boardVM.reporting)
        )
        .toVector ++ participantPlot.focusStops),
      "compare.participant-plot.table" -> participantPlot.table.focusStops,
      "compare.participant-table"      -> participantTable.focusStops
    )
    val nav = ParticipantsNavigator.vm(participantsState, state, model())
    eyes4s.studio.app.layout.StudioLayouts.spec.all
      .flatMap(_.panes)
      .distinct
      .flatMap(decl =>
        contents.get(decl.id.value).map { stops =>
          decl.id -> eyes4s.studio.app.vm.ResultsFocus.pane(decl, nav, stops)
        }
      )
      .toMap

  /** The pair pane's subtitle (the golden's): the focused scale and the
    * run's declared pixels per degree.
    */
  def pairSubtitle: String =
    val m = model()
    (for
      f   <- panelState.focus
      l   <- contrast.ladder.collect { case LadderAnswer.Answered(l) => l }
      s   <- l.scales.find(_.scale == f.scale)
      run <- m.document.run(f.run)
      d   <- m.document.dataset(run.dataset)
    yield eyes4s.studio.app.text.TrialText(
      eyes4s.studio.app.text.TrialTextId.PairSubtitle,
      s.label,
      eyes4s.studio.app.text.Format.decimal(d.geometry.pixelsPerDegree.value, 0)
    )).getOrElse("")

  /** Across scales' subtitle (the golden's): the focused query. */
  def acrossSubtitle: String = panelState.focus.fold("")(f =>
    eyes4s.studio.app.text
      .TrialText(eyes4s.studio.app.text.TrialTextId.AcrossSubtitle, f.query.trial)
  )

  /** The contrast pane's view-model now. */
  def contrastVM: ContrastVM =
    val focus = panelState.focus
    ContrastPane.vm(
      contrast,
      focus,
      focus.flatMap(f => TrialPanels.referenceOf(f, rows)),
      ladderColumns
    )

  /** Read the focused query's ladder again after a failed read. */
  private def retryLadder(): Unit =
    val (c, loads) = ContrastPane.retry(contrast)
    contrast = c
    loadLadders(loads)
    render(model())

  private def loadLadders(loads: Vector[ContrastEffect]): Unit =
    loads.foreach { case ContrastEffect.LoadLadder(run, query) =>
      inputs.ladder(
        run,
        query,
        scaleLabels,
        a =>
          Platform.runLater { () =>
            if !disposed then
              contrast = ContrastPane.read(contrast, run, query, a)
              render(model())
          }
      )
    }

  private def syncPanels(m: AppModel): Unit =
    val (next, effects) = TrialPanels.sync(panelState, m, shownRun)
    panelState = next
    if scaleLabels.nonEmpty then
      val (c, loads) = ContrastPane.sync(contrast, next.focus)
      contrast = c
      loadLadders(loads)
      val (p, reads) = PairsTable.sync(pairsState, next.focus)
      pairsState = p
      readPairs(reads)
    effects.foreach {
      case PanelsEffect.InspectPair(run, address, pair) =>
        inputs.inspect(
          run,
          address,
          a => Platform.runLater(() => panelIntent(PanelsIntent.PairRead(pair, a)))
        )
      case PanelsEffect.ReadContent(key) =>
        sources.content.content(
          key.revision,
          key.trial,
          a => Platform.runLater(() => panelIntent(PanelsIntent.ContentRead(key, a)))
        )
    }

  /** The focused query's scale profile, while Compare shows the query layout. */
  /** The panels' view-model now. */
  def panelsVM: TrialPanelsVM = TrialPanels.vm(panelState, shownRun, scaleLabels)

  /** The navigators' view-model now. */
  def navigatorVM: QueriesNavigatorVM = navigatorVMOf(model())

  private var navigatorProjection: Option[
    (
        QueriesNavigator,
        CompareSummary,
        Vector[eyes4s.studio.core.selection.StudioRef],
        QueriesNavigatorVM
    )
  ] = None
  private var participantProjection: Option[
    (
        ParticipantsNavigator,
        CompareSummary,
        Vector[eyes4s.studio.core.selection.StudioRef],
        ParticipantsNavigatorVM
    )
  ] = None

  private def navigatorVMOf(m: AppModel): QueriesNavigatorVM =
    val selected = QueriesNavigator.selection(m)
    navigatorProjection match
      case Some((nav, previous, refs, value))
          if nav == navigator && previous == state && refs == selected =>
        value
      case _ =>
        val value = QueriesNavigator.vm(navigator, state, selected)
        navigatorProjection = Some((navigator, state, selected, value))
        value

  private def participantVMOf(m: AppModel): ParticipantsNavigatorVM =
    val selected = QueriesNavigator.selection(m)
    participantProjection match
      case Some((nav, previous, refs, value))
          if nav == participantsState && previous == state && refs == selected =>
        value
      case _ =>
        val value = ParticipantsNavigator.vm(participantsState, state, m)
        participantProjection = Some((participantsState, state, selected, value))
        value

  /** The state now. */
  def summary: CompareSummary = state

  /** The view-model now shown. */
  private var presentation: Option[(CompareSummary, AppModel, Int, CompareSummaryVM)] = None

  private def summarySelection(m: AppModel) = m.selection.selected.filter {
    case _: eyes4s.studio.core.selection.StudioRef.ParticipantSummary => true
    case _: eyes4s.studio.core.selection.StudioRef.ReportParticipant  => true
    case _: eyes4s.studio.core.selection.StudioRef.Participant        => true
    case _                                                            => false
  }

  // These are the model inputs read by CompareSummaryVM: report metadata,
  // participant selection and the current freshness text. Navigation and a
  // fixation cursor do not change the served tables or plot sources.
  private def viewModel(m: AppModel): CompareSummaryVM = presentation match
    case Some((previous, model, slots, result))
        if previous == state && slots == noteSlots &&
          model.document.analyses == m.document.analyses &&
          model.document.runs == m.document.runs &&
          model.document.reporting == m.document.reporting &&
          summarySelection(model) == summarySelection(m) &&
          result.freshness == eyes4s.studio.app.vm.Shell.context(m).newer.map(_.text) =>
      result
    case _ =>
      val result = CompareSummaryVM.of(state, m, noteSlots = noteSlots)
      presentation = Some((state, m, noteSlots, result))
      result

  def vm: CompareSummaryVM = viewModel(model())

  /** Follows the model: a newly shown run is read; the views show the bus's selection. */
  def sync(m: AppModel): Unit = if !disposed then
    val (next, effects) = CompareSummary.sync(state, m)
    state = next
    navigator = QueriesNavigator.follow(navigator, next.run)
    perform(effects)
    syncPanels(m)
    participantPlot.project(m.selection)
    ladder.project(m.selection)
    pairsTable.project(m.selection)
    participantTable.project(m.selection)
    queryTrialTable.project(m.selection)
    referenceTrialTable.project(m.selection)
    render(m)

  /** A backend answer or a user action. */
  def dispatch(intent: SummaryIntent): Unit = if !disposed then
    val (next, effects) = CompareSummary.update(state, intent)
    state = next
    perform(effects)
    syncPanels(model())
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
      case SummaryEffect.RequestReport(run, reporting, scale, overall) =>
        inputs.report(
          run,
          reporting,
          scale,
          a =>
            Platform.runLater(() =>
              dispatch(SummaryIntent.ReportRead(run, reporting, scale, overall, a))
            )
        )
    }

  private def render(m: AppModel): Unit =
    if !disposed then
      participantsState =
        eyes4s.studio.app.compare.ParticipantsNavigator.follow(participantsState, state)
      participants.render(participantVMOf(m))
      val navigators = navigatorVMOf(m)
      queries.render(navigators)
      items.render(navigators)
      val v     = viewModel(m)
      val theme = m.theme match
        case eyes4s.studio.core.document.Theme.Light => Theme.Light
        case eyes4s.studio.core.document.Theme.Dark  => Theme.Dark
      // The run's status, then why any part could not be drawn.
      val parts = Vector(v.participantPlot, v.profile, v.participants, v.queries)
        .flatMap(_.flatMap(_.left.toOption))
      val said = (v.status.toVector ++ parts ++ v.scales.flatMap(_.unavailable)).distinct
      status.setText(said.mkString("\n"))
      status.setVisible(said.nonEmpty)
      status.setManaged(said.nonEmpty)
      boardVM = SummaryBoard.vm(state, m, v)
      chrome.render(boardVM, v.paired)
      // A new reported σ, selection or Δ rebuilds the slopegraphs.
      if v.slopes != slopes then
        slopes = v.slopes
        participantPlot.rebuild(SlopegraphPlot(slopeColumns, slopes))
      show("participant-plot", v.participantPlot, theme)(
        participantPlot.show(_, theme),
        participantPlot.clear()
      )
      val pairs = pairsVM
      pairsStatus.setText(pairs.status.getOrElse(""))
      pairsStatus.setVisible(pairs.status.isDefined);
      pairsStatus.setManaged(pairs.status.isDefined)
      pairsRetry.setText(pairs.retry.getOrElse(""))
      pairsRetry.setAccessibleText(pairs.retry.orNull)
      pairsRetry.setVisible(pairs.retry.isDefined); pairsRetry.setManaged(pairs.retry.isDefined)
      pairs.source match
        case Some(src) =>
          pairsTable.show(src)
          pairsTable.moveCursor(pairs.focused)
        case None => pairsTable.clear()
      show("participant-table", v.participants, theme)(
        participantTable.show,
        participantTable.clear()
      )
      val cv = contrastVM
      readout.render(cv)
      if ladderFocus != cv.focusScale then
        ladderFocus = cv.focusScale
        ladder.rebuild(ScaleLadderPlot(ladderColumns, cv.focusScale))
      show("ladder", cv.ladder.map(Right(_)), theme)(ladder.show(_, theme), ladder.clear())
      val pv = panelsVM
      panels.render(
        pv,
        theme,
        eyes4s.studio.app.explore.ExploreTrialViewVM.appearance(m)._2,
        m.document.presentation.mapOpacity.value
      )
      maps.sync(panelState.focus, pv, m.document.presentation.mapOpacity.value)
      renderStrip(eyes4s.studio.app.explore.ExploreTrialViewVM.appearance(m)._2)
      def table(p: Option[TrialPanelVM]) = p.map(_.content).collect {
        case PanelContent.Shown(c) => TrialPanels.fixationTable(c).left.map(_.message)
      }
      show("query-trial-table", table(pv.query), theme)(
        queryTrialTable.show,
        queryTrialTable.clear()
      )
      show("reference-trial-table", table(pv.reference), theme)(
        referenceTrialTable.show,
        referenceTrialTable.clear()
      )
      rendered.foreach(_())

  // The pair's control strip (overhaul decision 7): the focused query's
  // ladder at the focused scale, each reference's map read once.
  val strip: PairStripView = PairStripView(app)
  panels.stripSlot.getChildren.setAll(strip.node)
  private var stripMaps =
    Map.empty[(RunId, ScaleIndex, TrialKey), Either[String, DensityGrid]]
  private var stripAsked = Set.empty[(RunId, ScaleIndex, TrialKey)]

  private def renderStrip(stage: eyes4s.studio.app.tokens.StageVariant): Unit =
    val focused = for
      f <- panelState.focus
      l <- contrast.ladder.collect { case LadderAnswer.Answered(l) => l }
      s <- l.scales.find(_.scale == f.scale)
    yield (f, s)
    focused match
      case None         => strip.clear()
      case Some((f, s)) =>
        val trials = s.matchedTrial +: s.controls.map(_.reference)
        trials.map((f.run, f.scale, _)).filterNot(stripAsked).foreach { key =>
          stripAsked += key
          inputs.mapGrid(
            MapId.Result(key._1, key._3, key._2),
            answer =>
              Platform.runLater { () =>
                stripMaps = stripMaps.updated(key, answer)
                renderStrip(eyes4s.studio.app.explore.ExploreTrialViewVM.appearance(model())._2)
              }
          )
        }
        val shown = TrialPanels.referenceOf(f, rows).map((d, k) => f.pair(d, k))
        strip.render(
          eyes4s.studio.app.compare.PairStrip
            .vm(
              f.run,
              f.scale,
              s,
              shown.orElse(Some(s.matched)),
              t => stripMaps.get((f.run, f.scale, t))
            ),
          stage
        )

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
      participantPlot.plotHost.heightProperty.removeListener(plotResized)
      participantPlot.dispose()
      participantTable.dispose()
      ladder.dispose()
      participants.dispose()
      queries.dispose()
      items.dispose()
      queryTrialTable.dispose()
      referenceTrialTable.dispose()
      panels.dispose()
      maps.dispose()
      pairsTable.dispose()

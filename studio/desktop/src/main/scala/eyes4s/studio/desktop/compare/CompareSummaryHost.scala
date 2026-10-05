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
import eyes4s.studio.app.plot.{
  LadderColumns,
  ParticipantColumns,
  PlotSource,
  ProfileColumns,
  ScaleLadder,
  ScaleProfile
}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{PageRequest, QueryRow, ResultAddress, RunId, TrialKey}
import eyes4s.studio.core.document.ReportingSpec
import eyes4s.studio.core.selection.{ScaleIndex, ViewId}
import eyes4s.studio.desktop.plot.{PlotTwin, TableTwinView}
import eyes4s.studio.desktop.runtime.StudioSession
import eyes4s.studio.viz.plot.{ParticipantPlot, ScaleLadderPlot, ScaleProfilePlot}
import javafx.application.Platform
import javafx.geometry.Pos
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.control.{Button, Label, ToggleButton, ToggleGroup, Tooltip}
import javafx.scene.layout.{HBox, Priority, Region, VBox}

/** Where the summary layout asks for a run's summary and queries. `done`
  * may be called on any thread.
  */
trait SummaryInputs:
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
      def summary(run: RunId, done: SummaryAnswer => Unit): Unit =
        session.run(session.backend.result(run)) {
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
        session.run(session.backend.report(run, reporting, scale.value)) {
          case Left(e)          => done(ReportAnswer.Failed(reason(e)))
          case Right(Left(err)) => done(ReportAnswer.Refused(err))
          case Right(Right(v))  => done(ReportAnswer.Answered(v))
        }
      def inspect(run: RunId, address: ResultAddress, done: PairAnswer => Unit): Unit =
        session.run(session.backend.inspect(run, address)) {
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
        val backend = session.backend
        session.run(
          ScaleLadder.load[IO](backend.inspect, backend.navigator)(run, query, scales)
        ) {
          case Left(e)            => done(LadderAnswer.Failed(reason(e)))
          case Right(Left(err))   => done(LadderAnswer.Failed(err.message))
          case Right(Right(full)) => done(LadderAnswer.Answered(full))
        }
      def pairs(run: RunId, scale: ScaleIndex, done: PairsAnswer => Unit): Unit =
        session.run(
          eyes4s.studio.core.figures.MethodsReads
            .pairRowsAt[IO](session.backend.pairRows, run, scale.value)
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
    inputs: SummaryInputs,
    sources: PanelSources = PanelSources.notServed
):
  private var state: CompareSummary                           = CompareSummary.empty
  private var shown: Map[String, (Option[PlotSource], Theme)] = Map.empty
  private var disposed: Boolean                               = false

  private def view(id: String): ViewId =
    ViewId.of(id).fold(e => throw IllegalStateException(e.message), identity)

  private val selection  = model().selection
  private var navigator  = QueriesNavigator.initial
  private var panelState = TrialPanels.empty
  private var contrast   = ContrastPane.empty
  private var pairsState = PairsTable.empty

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
  // The Results board pins the selected participant's row at the top
  // ("pinned · selected"; bead bd-01M44PD9XEHG6QWPCJATAJZ15W).
  val participantTable: TableTwinView =
    TableTwinView.attach(view("compare.participant-table"), selection, app, pinsSelected = true)
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
      () => panelIntent(PanelsIntent.Retry),
      sources.stimuli
    )

  /** What Compare's inspector reads of these views (S8.4). */
  def inspectorInputs: InspectorInputs =
    InspectorInputs(panelState, shownRun, state.answered, state.reporting)

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

  private def rows: Vector[QueryRow] = state.queries match
    case Some(QueriesAnswer.Answered(rs)) => rs
    case _                                => Vector.empty

  private def scaleLabels: Vector[String] = state.answered.fold(Vector.empty)(_.scales)

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

  /** The contrast pane: the ladder and its readout. */
  val contrastNode: HBox =
    HBox.setHgrow(ladder.plotNode, Priority.ALWAYS)
    // The readout scrolls when the pane is shorter than it, never overflowing the tabs.
    val side = javafx.scene.control.ScrollPane(readout.node)
    side.setFitToWidth(true)
    side.setHbarPolicy(javafx.scene.control.ScrollPane.ScrollBarPolicy.NEVER)
    side.setMinWidth(javafx.scene.layout.Region.USE_PREF_SIZE)
    side.setFocusTraversable(false)
    val box = HBox(ladder.plotNode, side)
    box.setMinHeight(0.0)
    box

  /** The stops inside the contrast pane: the ladder, one stop with a roving
    * cursor named by the plot it draws, while it shows one, then Prev and Next.
    */
  def contrastStops: Vector[eyes4s.studio.app.vm.FocusStop] =
    val plot = Option(ladder.plotHost.getAccessibleText)
      .filter(_ => ladder.plotHost.isFocusTraversable)
      .map(eyes4s.studio.app.vm.FocusStop(eyes4s.studio.app.vm.A11yRole.Region, _))
    plot.toVector ++ ContrastPane.focusStops(contrastVM)

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
  private def queryProfile(m: AppModel): Option[Either[String, PlotSource]] =
    for
      f <- panelState.focus
      if m.layout == eyes4s.studio.app.layout.StudioLayouts.compareQuery
      row <- rows.find(_.query == f.query)
      rep <- state.reporting
      rev <- m.document.run(f.run).flatMap(r => m.document.analysis(r.analysis))
    yield ScaleProfile
      .ofQuery(f.run, rep, row, scaleLabels, rev.recipe.scales)
      .left
      .map(_.message)
      .flatMap(
        ScaleProfile.source(_, profileColumns).left.map(_.message)
      )

  private val profileColumns: ProfileColumns =
    ProfileColumns.standard.fold(e => throw IllegalStateException(e.message), identity)

  /** The panels' view-model now. */
  def panelsVM: TrialPanelsVM = TrialPanels.vm(panelState, shownRun, scaleLabels)

  /** The navigators' view-model now. */
  def navigatorVM: QueriesNavigatorVM = navigatorVMOf(model())

  private def navigatorVMOf(m: AppModel): QueriesNavigatorVM =
    QueriesNavigator.vm(navigator, state, QueriesNavigator.selection(m))

  /** The state now. */
  def summary: CompareSummary = state

  /** The view-model now shown. */
  def vm: CompareSummaryVM = CompareSummaryVM.of(state, model())

  /** Follows the model: a newly shown run is read; the views show the bus's selection. */
  def sync(m: AppModel): Unit = if !disposed then
    val (next, effects) = CompareSummary.sync(state, m)
    state = next
    navigator = QueriesNavigator.follow(navigator, next.run)
    perform(effects)
    syncPanels(m)
    participantPlot.project(m.selection)
    scaleProfile.project(m.selection)
    ladder.project(m.selection)
    pairsTable.project(m.selection)
    participantTable.project(m.selection)
    queryTable.project(m.selection)
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
      val navigators = navigatorVMOf(m)
      queries.render(navigators)
      items.render(navigators)
      val v     = CompareSummaryVM.of(state, m)
      val theme = m.theme match
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
      val focusedScale = (0 until scales.getChildren.size).iterator
        .map(scales.getChildren.get)
        .collectFirst { case b: ToggleButton if b.isFocused => b.getText }
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
        b.addEventFilter(
          KeyEvent.KEY_PRESSED,
          event =>
            val step = event.getCode match
              case KeyCode.LEFT  => -1
              case KeyCode.RIGHT => 1
              case _             => 0
            if step != 0 && !event.isAltDown && !event.isControlDown && !event.isMetaDown && !event.isShiftDown
            then
              val choices = v.scales.filter(_.available)
              val at      = choices.indexWhere(_.scale == c.scale)
              if at >= 0 then
                val next = choices(Math.floorMod(at + step, choices.size))
                event.consume()
                dispatch(SummaryIntent.ChooseScale(next.scale))
                (0 until scales.getChildren.size).iterator
                  .map(scales.getChildren.get)
                  .collectFirst {
                    case nextButton: ToggleButton if nextButton.getText == next.label =>
                      nextButton
                  }
                  .foreach(_.requestFocus())
        )
        b
      }*)
      focusedScale.foreach { label =>
        (0 until scales.getChildren.size).iterator
          .map(scales.getChildren.get)
          .collectFirst { case b: ToggleButton if b.getText == label => b }
          .foreach(_.requestFocus())
      }
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
      // The shared scale profile pane: the summary's groups, or in the
      // query layout the focused query's own D at every scale (S8.5).
      show("scale-profile", queryProfile(m).orElse(v.profile), theme)(
        scaleProfile.show(_, theme),
        scaleProfile.clear()
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
      show("query-table", v.queries, theme)(queryTable.show, queryTable.clear())
      val cv = contrastVM
      readout.render(cv)
      if ladderFocus != cv.focusScale then
        ladderFocus = cv.focusScale
        ladder.rebuild(ScaleLadderPlot(ladderColumns, cv.focusScale))
      show("ladder", cv.ladder.map(Right(_)), theme)(ladder.show(_, theme), ladder.clear())
      val pv = panelsVM
      panels.render(pv, theme, eyes4s.studio.app.explore.ExploreTrialViewVM.appearance(m)._2)
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
      ladder.dispose()
      queries.dispose()
      items.dispose()
      queryTrialTable.dispose()
      referenceTrialTable.dispose()
      panels.dispose()
      pairsTable.dispose()

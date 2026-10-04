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

package eyes4s.studio.desktop.figures

import cats.effect.IO
import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.figures.*
import eyes4s.studio.app.plot.{ParticipantColumns, PlotSource, ProfileColumns}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{DatasetRevision, LedgerPages, RunId, TrialKey}
import eyes4s.studio.core.diff.{LedgerUnavailable, StatusChanges, StatusDiff}
import eyes4s.studio.core.document.{DatasetRevisionSpec, FigureId, PanelLetter}
import eyes4s.studio.core.figures.{ReferenceReads, ReferenceScores}
import eyes4s.studio.core.selection.{ScaleIndex, ViewId}
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.plot.{PlotTwin, TableTwinView}
import eyes4s.studio.desktop.runtime.StudioSession
import eyes4s.studio.desktop.tokens.TokenFiles
import eyes4s.studio.viz.plot.{ParticipantPlot, PlotBuilder, ScaleProfilePlot}
import javafx.application.Platform
import javafx.geometry.Pos
import javafx.scene.Node
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import javafx.scene.AccessibleRole
import javafx.scene.control.{Button, Label, ScrollPane}
import javafx.scene.layout.{FlowPane, HBox, Priority, Region, VBox}

/** Where the Figures perspective reads what its panels show. `done` may be
  * called on any thread.
  */
trait FigureInputs:
  def summary(run: RunId, done: SummaryAnswer => Unit): Unit
  def references(
      run: RunId,
      scale: ScaleIndex,
      query: TrialKey,
      done: Either[String, ReferenceScores] => Unit
  ): Unit
  def displays(dataset: DatasetRevisionSpec, done: Either[String, DisplaySource] => Unit): Unit

  /** The trial statuses of `from` and `to`, compared (S5.8). */
  def status(from: DatasetRevision, to: DatasetRevision, done: StatusDiff => Unit): Unit

object FigureInputs:

  /** The window's backend and navigator, and `displays` for what trials showed. */
  def of(session: StudioSession, source: NavigatorDisplays): FigureInputs =
    new FigureInputs:
      def summary(run: RunId, done: SummaryAnswer => Unit): Unit =
        session.run(session.backend.result(run)) {
          case Left(e)          => done(SummaryAnswer.Failed(reason(e)))
          case Right(Left(err)) => done(SummaryAnswer.Refused(err))
          case Right(Right(s))  => done(SummaryAnswer.Answered(s))
        }
      def references(
          run: RunId,
          scale: ScaleIndex,
          query: TrialKey,
          done: Either[String, ReferenceScores] => Unit
      ): Unit =
        session.run(
          ReferenceReads.read[IO](
            session.backend.inspect(run, _),
            session.backend.navigator.pairs,
            run,
            scale,
            query
          )
        ) {
          case Left(e)       => done(Left(reason(e)))
          case Right(answer) => done(answer.left.map(_.message))
        }
      def displays(
          dataset: DatasetRevisionSpec,
          done: Either[String, DisplaySource] => Unit
      ): Unit = done(source.displays(dataset))
      def status(from: DatasetRevision, to: DatasetRevision, done: StatusDiff => Unit): Unit =
        def whole(d: DatasetRevision) =
          LedgerPages
            .all(session.backend.ledger(d, _))
            .map(_.left.map(LedgerUnavailable.Refused(_)))
        session.run(whole(from).flatMap(a => whole(to).map(b => (a, b)))) {
          case Left(e) =>
            done(StatusDiff.Unavailable(from, LedgerUnavailable.Failed(reason(e))))
          case Right((Left(why), _))       => done(StatusDiff.Unavailable(from, why))
          case Right((_, Left(why)))       => done(StatusDiff.Unavailable(to, why))
          case Right((Right(a), Right(b))) =>
            done(
              StatusChanges
                .between(from, a, to, b)
                .fold(
                  e => StatusDiff.Unavailable(from, LedgerUnavailable.Failed(e.message)),
                  StatusDiff.Compared(_)
                )
            )
        }

  private def reason(e: Throwable): String = Option(e.getMessage).getOrElse(e.toString)

/** The Figures perspective on the desktop (tickets S9.1 and S9.2a;
  * Figures.dc.html): the navigator with New figure, the stale notice and the
  * rebind dialog; the page, its width and zoom, its panels; the Table tab of
  * the selected panel; and the inspector's binding. It follows the model
  * ([[FigureComposer.sync]]), reads through `inputs`, and binds
  * [[ComposerVM]] to the panes. The page is paper whatever the theme, so
  * its plots are drawn light. Use on the JavaFX thread.
  */
final class FiguresHost(
    model: () => AppModel,
    app: Intent => Unit,
    inputs: FigureInputs
):
  private var state: FigureComposer = FigureComposer.empty
  private var disposed: Boolean     = false

  // The plot of each plotted panel, kept while its figure is shown.
  private var twins: Map[(FigureId, PanelLetter), (PlotTwin, Option[PlotSource])] = Map.empty

  private def view(id: String): ViewId =
    ViewId.of(id).fold(e => throw IllegalStateException(e.message), identity)

  private def sheet(node: javafx.scene.Parent): Unit =
    Option(getClass.getClassLoader.getResource(FiguresHost.stylesheetResource))
      .foreach(url => node.getStylesheets.add(url.toExternalForm))

  // --- the navigator -------------------------------------------------------------
  val newFigure: Button = Button()
  newFigure.getStyleClass.add("figures-new")
  newFigure.setOnAction(_ => dispatch(ComposerIntent.NewFigure))
  private val count = Label()
  count.getStyleClass.addAll("figures-count", "t11")
  private val rows    = VBox()
  private val problem = Label()
  problem.getStyleClass.addAll("figures-problem", "t11")
  problem.setWrapText(true)
  private val notice = VBox(4.0)
  notice.getStyleClass.add("figures-notice")
  private val dialog = VBox(4.0)
  dialog.getStyleClass.add("figures-dialog")

  /** The Figures navigator pane. */
  val navigatorNode: VBox =
    val head = HBox(8.0, newFigure, spacer(), count)
    head.setAlignment(Pos.CENTER_LEFT)
    val box = VBox(6.0, head, rows, notice, dialog, problem)
    box.getStyleClass.add("figures-navigator")
    sheet(box)
    box

  // --- the page --------------------------------------------------------------------
  private val title = Label()
  title.getStyleClass.addAll("figures-title", "t13")
  private val widths  = HBox(4.0)
  private val zoomOut = Button("−")
  private val zoomIn  = Button("+")
  private val zoom    = Label()
  zoom.getStyleClass.add("t11")
  zoomOut.setOnAction(_ => dispatch(ComposerIntent.ZoomOut))
  zoomIn.setOnAction(_ => dispatch(ComposerIntent.ZoomIn))
  zoomOut.setAccessibleText("Zoom out (−)")
  zoomIn.setAccessibleText("Zoom in (+)")
  private val width = Label()
  width.getStyleClass.add("t11")

  /** The page's panels, laid out in its columns. */
  val paper: FlowPane = FlowPane()
  paper.getStyleClass.add("figures-paper")

  /** The page pane: the toolbar, then the paper. */
  val pageNode: VBox =
    val bar = HBox(8.0, title, width, widths, spacer(), zoomOut, zoom, zoomIn)
    bar.setAlignment(Pos.CENTER_LEFT)
    bar.getStyleClass.add("figures-toolbar")
    val scroll = ScrollPane(paper)
    scroll.setFitToWidth(false)
    VBox.setVgrow(scroll, Priority.ALWAYS)
    val box = VBox(bar, scroll)
    box.getStyleClass.add("figures-page")
    sheet(box)
    box

  // --- the Table tab ------------------------------------------------------------------
  /** The selected panel's values. */
  val table: TableTwinView =
    TableTwinView.attach(view("figures.page.table"), model().selection, app)
  private val tableNote = Label()
  tableNote.getStyleClass.addAll("figures-note", "t11")
  tableNote.setWrapText(true)

  /** The Table pane: the selected panel's table, or why it has none. */
  val tableNode: VBox =
    VBox.setVgrow(table, Priority.ALWAYS)
    val box = VBox(4.0, tableNote, table)
    sheet(box)
    box

  // --- the inspector ---------------------------------------------------------------------
  private val binding = VBox(4.0)

  /** The inspector pane: the figure's binding. */
  val inspectorNode: VBox =
    binding.getStyleClass.add("figures-binding")
    sheet(binding)
    binding

  /** The state now. */
  def composer: FigureComposer = state

  /** The view-model now shown. */
  def vm: ComposerVM = FigureComposer.view(state, model())

  /** The plot of panel `letter` of `figure`, while it is shown. */
  def plot(figure: FigureId, letter: PanelLetter): Option[PlotTwin] =
    twins.get((figure, letter)).map(_._1)

  /** Follows the model. */
  def sync(m: AppModel): Unit = if !disposed then
    val (next, effects) = FigureComposer.sync(state, m)
    state = next
    perform(effects)
    twins.values.foreach(_._1.project(m.selection))
    table.project(m.selection)
    render(m)

  /** A user action or a platform answer. */
  def dispatch(intent: ComposerIntent): Unit = if !disposed then
    val (next, effects) = FigureComposer.update(state, model(), intent)
    state = next
    perform(effects)
    render(model())

  private def perform(effects: Vector[ComposerEffect]): Unit =
    def later(i: ComposerIntent): Unit = Platform.runLater(() => dispatch(i))
    effects.foreach {
      case ComposerEffect.App(i)              => app(i)
      case ComposerEffect.RequestSummary(run) =>
        inputs.summary(run, a => later(ComposerIntent.SummaryRead(run, a)))
      case ComposerEffect.RequestReferences(run, scale, query) =>
        inputs.references(
          run,
          scale,
          query,
          a => later(ComposerIntent.ReferencesRead(run, scale, query, a))
        )
      case ComposerEffect.RequestDisplays(dataset) =>
        inputs.displays(dataset, a => later(ComposerIntent.DisplaysRead(dataset.id, a)))
      case ComposerEffect.Binding(FigureEffect.RequestStatus(from, to)) =>
        inputs.status(
          from,
          to,
          s => later(ComposerIntent.Binding(FigureIntent.StatusRead(from, to, s)))
        )
      case ComposerEffect.Binding(FigureEffect.App(i)) => app(i)
    }

  private def render(m: AppModel): Unit = if !disposed then
    val v = FigureComposer.view(state, m)
    renderNavigator(v)
    renderPage(v)
    renderInspector(v)

  private def renderNavigator(v: ComposerVM): Unit =
    newFigure.setText(v.newFigure)
    newFigure.setAccessibleText(v.newFigure)
    count.setText(v.figures.header)
    rows.getChildren.setAll(v.figures.rows.map { r =>
      val name = Label(r.title); name.getStyleClass.add("t12")
      val chip = Label(r.status); chip.getStyleClass.addAll("figures-chip", "t11")
      if r.stale then chip.getStyleClass.add("figures-stale"): Unit
      val bound = Label(r.binding); bound.getStyleClass.addAll("figures-binding-text", "t11")
      val rep   = Label(r.reporting); rep.getStyleClass.add("t11")
      val row   = VBox(2.0, HBox(6.0, name, chip), bound, rep)
      row.getStyleClass.add("figures-row")
      if r.selected then row.getStyleClass.add("figures-row-selected"): Unit
      row.setAccessibleText(FiguresHost.rowName(r))
      row.setAccessibleRole(AccessibleRole.BUTTON)
      row.setFocusTraversable(true)
      row.setOnMouseClicked(_ =>
        dispatch(ComposerIntent.Binding(FigureIntent.Select(r.figure)))
      )
      row
    }*): Unit
    val selected = v.figures.rows.find(_.selected).map(_.figure)
    notice.getChildren.setAll(v.figures.notice.toVector.flatMap { n =>
      val text   = Label(n.text); text.setWrapText(true); text.getStyleClass.add("t11")
      val rebind = button(n.rebind)
      val keep   = button(n.keep)
      selected.foreach { f =>
        rebind.setOnAction(_ => dispatch(ComposerIntent.Binding(FigureIntent.Rebind(f))))
        keep.setOnAction(_ => dispatch(ComposerIntent.Binding(FigureIntent.Keep(f))))
      }
      Vector(text, HBox(6.0, rebind, keep))
    }*): Unit
    dialog.getChildren.setAll(v.figures.dialog.toVector.flatMap { d =>
      val head  = Label(d.title); head.getStyleClass.add("t12")
      val lines = (Vector(d.plan, d.data) ++ d.conflicts).map { t =>
        val l = Label(t); l.setWrapText(true); l.getStyleClass.add("t11"); l
      }
      val confirm = button(d.confirm)
      confirm.setDisable(!d.canConfirm)
      confirm.setOnAction(_ => dispatch(ComposerIntent.Binding(FigureIntent.ConfirmRebind)))
      val cancel = button(d.cancel)
      cancel.setOnAction(_ => dispatch(ComposerIntent.Binding(FigureIntent.CancelRebind)))
      (head +: lines) :+ HBox(6.0, confirm, cancel)
    }*): Unit
    val said = (v.problem.toVector ++ v.figures.problem.toVector).distinct
    problem.setText(said.mkString("\n"))
    problem.setVisible(said.nonEmpty)
    problem.setManaged(said.nonEmpty)

  private def renderPage(v: ComposerVM): Unit =
    widths.getChildren.setAll(v.widths.map { (w, label, chosen) =>
      val b = button(label)
      b.setAccessibleText(FiguresHost.widthName(label, chosen))
      if chosen then b.getStyleClass.add("figures-chosen"): Unit
      b.setOnAction(_ => dispatch(ComposerIntent.SetWidth(w)))
      b
    }*): Unit
    v.page match
      case None =>
        title.setText("")
        width.setText("")
        zoom.setText("")
        paper.getChildren.clear()
        retire(Set.empty)
        tableNote.setText("")
        table.clear()
      case Some(p) =>
        title.setText(p.title)
        width.setText(p.widthLabel)
        zoom.setText(p.zoom)
        val px = p.pxPerMm
        paper.setPrefWrapLength(p.width.mm * px)
        paper.setMaxWidth(p.width.mm * px)
        paper.setHgap(PageLayout.GutterMm * px)
        paper.setVgap(PageLayout.GutterMm * px)
        retire(p.panels.map(q => (p.figure, q.letter)).toSet)
        paper.getChildren.setAll(p.panels.map(q => panelNode(p.figure, q, px))*): Unit
        p.table match
          case None =>
            tableNote.setText("Select a panel to see its values.")
            table.clear()
          case Some(Left(why)) =>
            tableNote.setText(why)
            table.clear()
          case Some(Right(source)) =>
            tableNote.setText("")
            if !table.source.contains(source) then table.show(source)

  private def panelNode(figure: FigureId, p: PanelVM, px: Double): Node =
    // The panel's letter is its button: Tab reaches it, and Enter selects it.
    val letter = button(p.letter.value)
    letter.getStyleClass.addAll("figures-letter", "t13")
    letter.setAccessibleText(FiguresHost.panelName(p))
    letter.setOnAction(_ => dispatch(ComposerIntent.SelectPanel(figure, p.letter)))
    val name       = Label(p.title); name.getStyleClass.addAll("figures-panel-title", "t11")
    val w          = p.widthMm * px
    val body: Node = p.body match
      case PanelBody.Plot(plot) =>
        val twin = twinFor(figure, p, plot.kind, plot.source)
        twin.plotNode.setPrefSize(w, w * 0.62)
        val notes = plot.notes.map { n =>
          val l = Label(n); l.getStyleClass.add("t11"); l
        }
        VBox(2.0, (twin.plotNode +: notes)*)
      case PanelBody.Maps(maps) =>
        val tiles = HBox(
          4.0,
          maps.tiles.map { t =>
            val head  = Label(t.title); head.getStyleClass.add("t11")
            val score = Label(t.label); score.getStyleClass.addAll("figures-score", "t11")
            val tile  = VBox(2.0, head, score)
            tile.getStyleClass.add("figures-tile")
            tile.setPrefWidth((w - 8) / 3)
            tile.setAccessibleText(s"${t.title}, ${t.label}")
            tile
          }*
        )
        val caption = Label(maps.caption); caption.setWrapText(true);
        caption.getStyleClass.add("t11")
        val note = Label(maps.maps); note.setWrapText(true);
        note.getStyleClass.addAll("figures-note", "t11")
        VBox(4.0, tiles, caption, note)
      case PanelBody.Gaze(g) =>
        val heading = Label(g.heading); heading.getStyleClass.add("t11")
        val shown = Label(g.displayed); shown.setWrapText(true); shown.getStyleClass.add("t11")
        val gaze  = Label(g.gaze); gaze.setWrapText(true);
        gaze.getStyleClass.addAll("figures-note", "t11")
        VBox(2.0, heading, shown, gaze)
      case PanelBody.Waiting(why) =>
        val l = Label(why); l.setWrapText(true); l.getStyleClass.addAll("figures-note", "t11");
        l
      case PanelBody.Unavailable(why) =>
        val l = Label(why); l.setWrapText(true);
        l.getStyleClass.addAll("figures-problem", "t11"); l
    val box = VBox(4.0, HBox(6.0, letter, name), body)
    box.getStyleClass.add("figures-panel")
    if p.selected then box.getStyleClass.add("figures-panel-selected"): Unit
    box.setPrefWidth(w)
    box.setMinWidth(w)
    box.setMaxWidth(w)
    box.setId(s"figures-panel-${p.letter.value}")
    box.setAccessibleText(FiguresHost.panelName(p))
    box.setOnMouseClicked(_ => dispatch(ComposerIntent.SelectPanel(figure, p.letter)))
    box

  private def builderOf(kind: PlotKind): PlotBuilder = kind match
    case PlotKind.Participant =>
      ParticipantPlot(
        ParticipantColumns.standard.fold(e => throw IllegalStateException(e.message), identity)
      )
    case PlotKind.Profile =>
      ScaleProfilePlot(
        ProfileColumns.standard.fold(e => throw IllegalStateException(e.message), identity)
      )

  private def twinFor(
      figure: FigureId,
      p: PanelVM,
      kind: PlotKind,
      source: PlotSource
  ): PlotTwin =
    val key  = (figure, p.letter)
    val twin = twins.get(key).map(_._1).getOrElse {
      val made = PlotTwin
        .attach(
          builderOf(kind),
          view(s"figures.panel.${figure.number}.${p.letter.value}"),
          view(s"figures.panel.${figure.number}.${p.letter.value}.table"),
          model().selection,
          app
        )
        .fold(e => throw IllegalStateException(e.message), identity)
      twins = twins.updated(key, (made, None))
      made
    }
    if !twins.get(key).flatMap(_._2).contains(source) then
      twin.show(source, Theme.Light)
      twins = twins.updated(key, (twin, Some(source)))
    twin

  /** Disposes the plots of panels no longer shown. */
  private def retire(keep: Set[(FigureId, PanelLetter)]): Unit =
    val (kept, gone) = twins.partition((k, _) => keep.contains(k))
    gone.values.foreach(_._1.dispose())
    twins = kept

  private def renderInspector(v: ComposerVM): Unit =
    binding.getChildren.setAll(v.figures.binding.toVector.flatMap { b =>
      val head                          = Label(b.title); head.getStyleClass.add("t12")
      val lock                          = Label(b.lock); lock.getStyleClass.add("t11")
      def row(k: String, value: String) =
        val key = Label(k); key.getStyleClass.addAll("figures-key", "t11")
        val vl  = Label(value); vl.getStyleClass.add("t11")
        HBox(6.0, key, vl)
      val open = button(v.page.map(_.openInCompare).getOrElse("Open in Compare"))
      open.setOnAction(_ => dispatch(ComposerIntent.OpenInCompare))
      val rebind = button(b.rebind)
      v.figures.rows
        .find(_.selected)
        .foreach(r =>
          rebind
            .setOnAction(_ => dispatch(ComposerIntent.Binding(FigureIntent.Rebind(r.figure))))
        )
      val note = Label(b.note); note.setWrapText(true); note.getStyleClass.add("t11")
      Vector(
        HBox(6.0, head, lock),
        row("Run", b.run),
        row("Reporting spec", b.reporting),
        row("Unit", b.unit),
        HBox(6.0, open, rebind),
        note
      )
    }*): Unit

  /** Disposes the views; the host ignores the model from then on. Idempotent. */
  def dispose(): Unit =
    if !disposed then
      disposed = true
      retire(Set.empty)
      table.dispose()

  private def button(text: String): Button =
    val b = Button(text)
    b.setAccessibleText(text)
    b

  // --- Tab order (S1.11): each pane's controls follow its stop ------------------------

  /** The navigator's controls, in Tab order. */
  def navigatorStops: Vector[FocusStop] =
    val v = vm
    Vector(FocusStop(A11yRole.Button, v.newFigure)) ++
      v.figures.rows.map(r => FocusStop(A11yRole.Button, FiguresHost.rowName(r))) ++
      v.figures.notice.toVector.flatMap(n =>
        Vector(FocusStop(A11yRole.Button, n.rebind), FocusStop(A11yRole.Button, n.keep))
      ) ++
      v.figures.dialog.toVector.flatMap(d =>
        Option.when(d.canConfirm)(FocusStop(A11yRole.Button, d.confirm)).toVector :+
          FocusStop(A11yRole.Button, d.cancel)
      )

  /** The page's controls, in Tab order: the widths, the zoom, then each
    * panel's button and, when it is plotted, its plot.
    */
  def pageStops: Vector[FocusStop] =
    val v = vm
    v.page.toVector.flatMap { p =>
      v.widths.map((_, label, chosen) =>
        FocusStop(A11yRole.Button, FiguresHost.widthName(label, chosen))
      ) ++
        Vector(
          FocusStop(A11yRole.Button, "Zoom out (−)"),
          FocusStop(A11yRole.Button, "Zoom in (+)")
        ) ++
        p.panels.flatMap { q =>
          FocusStop(A11yRole.Button, FiguresHost.panelName(q)) +:
            (q.body match
              case PanelBody.Plot(_) =>
                plot(p.figure, q.letter).toVector.map(t =>
                  FocusStop(A11yRole.Region, Option(t.plotHost.getAccessibleText).getOrElse(""))
                )
              case _ => Vector.empty)
        }
    }

  /** The Table tab's control: its table. */
  def tableStops: Vector[FocusStop] =
    Vector(FocusStop(A11yRole.Region, Option(table.getAccessibleText).getOrElse("")))

  /** The inspector's controls: Open in Compare and Rebind figure…. */
  def inspectorStops: Vector[FocusStop] =
    val v = vm
    v.figures.binding.toVector.flatMap(b =>
      Vector(
        FocusStop(A11yRole.Button, v.page.map(_.openInCompare).getOrElse("Open in Compare")),
        FocusStop(A11yRole.Button, b.rebind)
      )
    )

  private def spacer(): Region =
    val r = Region(); HBox.setHgrow(r, Priority.ALWAYS); r

object FiguresHost:
  /** A navigator row's accessible name. */
  def rowName(r: FigureRowVM): String = s"${r.title}, ${r.status}, ${r.binding}"

  /** A page width button's accessible name. */
  def widthName(label: String, chosen: Boolean): String =
    if chosen then s"$label, selected" else label

  /** A panel button's accessible name: "Panel D, Participant D by response". */
  def panelName(p: PanelVM): String =
    s"Panel ${p.letter.value}, ${p.title}" + (if p.selected then ", selected" else "")

  /** The classpath resource of the Figures stylesheet. */
  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-figures.css"

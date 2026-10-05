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
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  LedgerPages,
  RunId,
  TrialFixations,
  TrialKey
}
import eyes4s.studio.core.diff.{LedgerUnavailable, StatusChanges, StatusDiff}
import eyes4s.studio.core.document.{DatasetRevisionSpec, FigureId, PanelLetter}
import eyes4s.studio.core.figures.{MethodsFacts, MethodsReads, ReferenceReads, ReferenceScores}
import eyes4s.studio.core.selection.{ScaleIndex, ViewId}
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.plot.{PlotTwin, TableTwinView}
import eyes4s.studio.desktop.runtime.{ProjectPort, StudioSession}
import eyes4s.studio.desktop.tokens.TokenFiles
import eyes4s.studio.viz.figure.{FigureGaze, PlotGeometry}
import eyes4s.studio.desktop.trial.{StimulusError, StimulusSource, TrialView}
import eyes4s.studio.viz.plot.{ParticipantPlot, PlotBuilder, ScaleProfilePlot}
import javafx.application.Platform
import javafx.geometry.Pos
import javafx.scene.Node
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import javafx.scene.AccessibleRole
import eyes4s.studio.app.plot.ParticipantLines
import eyes4s.studio.app.tokens.FontFace
import javafx.beans.property.ReadOnlyObjectWrapper
import javafx.scene.control.{Button, Label, ScrollPane, TextArea}
import javafx.scene.effect.ColorAdjust
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.text.Font
import javafx.stage.{DirectoryChooser, FileChooser, Window}

import java.nio.file.Files
import scala.util.control.NonFatal

import scala.jdk.CollectionConverters.*
import javafx.scene.layout.{FlowPane, HBox, Priority, Region, VBox}

/** Where the Figures perspective reads what its panels show. `done` may be
  * called on any thread.
  */
trait FigureInputs:
  /** Where the gaze panels' stimulus images are read. */
  def stimuli: StimulusSource

  /** A gaze panel's fixations under `revision` (S6.2 trialFixations). */
  def fixations(
      revision: AnalysisRevision,
      trial: TrialKey,
      done: Either[String, TrialFixations] => Unit
  ): Unit

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

  /** The admission and query facts the methods text of `run` cites (S9.4). */
  def methods(
      run: RunId,
      dataset: DatasetRevision,
      done: Either[String, MethodsFacts] => Unit
  ): Unit

  /** Write an export bundle into a folder the author chooses (S9.5); the
    * answer is where it went, or why it was not written.
    */
  def bundle(request: BundleRequest, done: Either[String, String] => Unit): Unit

  /** Save an exported figure, suggesting `name`; the answer is where it went,
    * or why it was not saved (S9.3).
    */
  def save(
      name: String,
      format: ExportFormat,
      bytes: IArray[Byte],
      done: Either[String, String] => Unit
  ): Unit

object FigureInputs:

  /** A window with no stimulus store: every image is "not stored". */
  val NoStimuli: StimulusSource =
    asset => Left(StimulusError.NotStored(asset.file, "this window"))

  /** Where an export bundle goes: the folder the author chooses, titled
    * `title`, or none (the platform's directory chooser; a test passes its
    * own folder).
    */
  type ChooseFolder = (String, Option[Window]) => Option[java.nio.file.Path]

  val directoryChooser: ChooseFolder = (title, owner) =>
    val chooser = DirectoryChooser()
    chooser.setTitle(title)
    Option(chooser.showDialog(owner.orNull)).map(_.toPath)

  /** The window's backend and navigator, and `displays` for what trials showed. */
  def of(
      session: StudioSession,
      source: NavigatorDisplays,
      owner: () => Option[Window],
      project: Option[ProjectPort] = None,
      images: StimulusSource = FigureInputs.NoStimuli,
      chooseFolder: ChooseFolder = directoryChooser
  ): FigureInputs =
    new FigureInputs:
      def stimuli: StimulusSource = images
      def fixations(
          revision: AnalysisRevision,
          trial: TrialKey,
          done: Either[String, TrialFixations] => Unit
      ): Unit =
        session.run(session.backend.trialFixations(revision, trial)) {
          case Left(e)  => done(Left(reason(e)))
          case Right(a) => done(a.left.map(_.message))
        }
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
      ): Unit = source.read(dataset, done)
      def methods(
          run: RunId,
          dataset: DatasetRevision,
          done: Either[String, MethodsFacts] => Unit
      ): Unit =
        session.run(
          MethodsReads
            .read[IO](session.backend.admission, session.backend.queries, run, dataset)
        ) {
          case Left(e)  => done(Left(reason(e)))
          case Right(a) => done(a.left.map(_.message))
        }
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
      def save(
          name: String,
          format: ExportFormat,
          bytes: IArray[Byte],
          done: Either[String, String] => Unit
      ): Unit =
        val chooser = FileChooser()
        chooser.setInitialFileName(name)
        chooser.getExtensionFilters.add(
          FileChooser.ExtensionFilter(s"${format.label} figure", s"*.${format.extension}")
        )
        Option(chooser.showSaveDialog(owner().orNull)) match
          case None       => done(Left("no file was chosen"))
          case Some(file) =>
            try
              Files.write(file.toPath, Array.from(bytes)): Unit
              done(Right(file.toString))
            catch case NonFatal(e) => done(Left(reason(e)))

      def bundle(request: BundleRequest, done: Either[String, String] => Unit): Unit =
        chooseFolder(s"Export ${request.folder}", owner()) match
          case None         => done(Left("no folder was chosen"))
          case Some(parent) =>
            val target = parent.resolve(request.folder)
            val run    = request.source.run.id
            val read   = for
              summary <- session.backend.result(run)
              rows    <- MethodsReads.queryRows(session.backend.queries, run)
            yield (summary.left.map(_.message), rows.left.map(_.message))
            session.run(read) {
              case Left(e)                              => done(Left(reason(e)))
              case Right((Left(why), _))                => done(Left(why))
              case Right((_, Left(why)))                => done(Left(why))
              case Right((Right(summary), Right(rows))) =>
                BundleFiles.assemble(
                  request,
                  summary,
                  rows,
                  FigureExport.rasters(request.page, images)
                ) match
                  case Left(why)   => done(Left(why))
                  case Right(file) =>
                    val snapshot = Option.when(request.items.contains(BundleItem.Snapshot))(
                      project.fold[BundleWriter.Snapshot]((_, answer) =>
                        answer(Left("this project is not saved in a bundle"))
                      )(port =>
                        (to, answer) => port.snapshot(to, request.includeImages, answer)
                      )
                    )
                    BundleWriter.write(target, file, snapshot, done)
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

  // The plot of each plotted panel, kept while its figure is shown: the
  // twin, the source it shows, and the plot kind and lines it draws them with.
  private final case class Drawn(
      twin: PlotTwin,
      source: Option[PlotSource],
      kind: PlotKind,
      lines: ParticipantLines
  )
  private var twins: Map[(FigureId, PanelLetter), Drawn] = Map.empty

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
  private val widths = HBox(4.0)

  /** "Greyscale check": the paper shown without colour, as it prints in greyscale. */
  private val greyscale = Button("Greyscale check")
  private val grey      = ColorAdjust()
  grey.setSaturation(-1.0)

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
    val bar = HBox(8.0, title, width, widths, spacer(), greyscale, zoomOut, zoom, zoomIn)
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

  // --- methods.md and its diff (S9.4) ---------------------------------------------------
  private val methodsHeading = Label()
  methodsHeading.getStyleClass.addAll("figures-note", "t11")
  methodsHeading.setWrapText(true)
  private val showDiff   = Button()
  private val regenerate = Button()
  showDiff.setOnAction(_ => dispatch(ComposerIntent.Methods(MethodsIntent.ShowDiff)))
  regenerate.setOnAction(_ => dispatch(ComposerIntent.Methods(MethodsIntent.Regenerate)))
  private val methodsStatus = Label()
  methodsStatus.getStyleClass.addAll("figures-note", "t11")
  methodsStatus.setWrapText(true)

  /** The methods text, editable; each edit goes to the composer as it is typed. */
  val methodsEditor: TextArea = TextArea()
  methodsEditor.getStyleClass.addAll("figures-methods", "serif", "t12")
  methodsEditor.setWrapText(true)
  methodsEditor.setAccessibleText(FiguresHost.MethodsText)
  // Tab and Shift+Tab leave the text, as from any other stop, rather than
  // typing a tab: the TextArea moves focus on Ctrl+Tab, so a plain Tab is
  // passed on as one.
  methodsEditor.addEventFilter(
    KeyEvent.KEY_PRESSED,
    (e: KeyEvent) =>
      if e.getCode == KeyCode.TAB && !e.isControlDown && !e.isAltDown && !e.isMetaDown then
        e.consume()
        methodsEditor.fireEvent(
          KeyEvent(
            KeyEvent.KEY_PRESSED,
            "",
            "",
            KeyCode.TAB,
            e.isShiftDown,
            true,
            false,
            false
          )
        )
  )
  // True while render writes the text, so the write is not taken for an edit.
  private var writing = false
  methodsEditor.textProperty.addListener((_, _, text) =>
    if !writing then dispatch(ComposerIntent.Methods(MethodsIntent.Edit(text)))
  )

  /** The methods.md pane: where the text comes from, Show diff and Regenerate, the text. */
  val methodsNode: VBox =
    VBox.setVgrow(methodsEditor, Priority.ALWAYS)
    val bar = HBox(6.0, methodsHeading, spacer(), showDiff, regenerate)
    bar.setAlignment(Pos.CENTER_LEFT)
    val box = VBox(4.0, bar, methodsStatus, methodsEditor)
    box.getStyleClass.add("figures-methods-pane")
    sheet(box)
    box

  private val diffCaption = Label()
  diffCaption.getStyleClass.addAll("figures-note", "t11")
  private val diffChoice = HBox(6.0)

  /** The diff, a line per sentence. */
  val diffLines: VBox = VBox(2.0)

  /** The "Diff vs generated" pane: sentence by sentence, and the choice a
    * regeneration over edits waits on.
    */
  val methodsDiffNode: VBox =
    val scroll = ScrollPane(diffLines)
    scroll.setFitToWidth(true)
    VBox.setVgrow(scroll, Priority.ALWAYS)
    val box = VBox(4.0, diffCaption, diffChoice, scroll)
    box.getStyleClass.add("figures-methods-pane")
    sheet(box)
    box

  /** The state now. */
  def composer: FigureComposer = state

  /** The view-model now shown. */
  def vm: ComposerVM = FigureComposer.view(state, model())

  /** The plot of panel `letter` of `figure`, while it is shown. */
  def plot(figure: FigureId, letter: PanelLetter): Option[PlotTwin] =
    twins.get((figure, letter)).map(_.twin)

  /** Follows the model. */
  def sync(m: AppModel): Unit = if !disposed then
    val (next, effects) = FigureComposer.sync(state, m)
    state = next
    perform(effects)
    twins.values.foreach(_.twin.project(m.selection))
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
      case ComposerEffect.WriteBundle(request) =>
        inputs.bundle(request, a => later(ComposerIntent.BundleExported(a)))
      case ComposerEffect.RequestMethods(run, dataset) =>
        inputs.methods(
          run,
          dataset,
          a => later(ComposerIntent.Methods(MethodsIntent.FactsRead(run, a)))
        )
      case ComposerEffect.RequestFixations(revision, trial) =>
        inputs.fixations(
          revision,
          trial,
          a => later(ComposerIntent.FixationsRead(revision, trial, a))
        )
      case ComposerEffect.RequestDisplays(dataset) =>
        inputs.displays(dataset, a => later(ComposerIntent.DisplaysRead(dataset.id, a)))
      case ComposerEffect.Binding(FigureEffect.RequestStatus(from, to)) =>
        inputs.status(
          from,
          to,
          s => later(ComposerIntent.Binding(FigureIntent.StatusRead(from, to, s)))
        )
      case ComposerEffect.Binding(FigureEffect.App(i))     => app(i)
      case ComposerEffect.ExportFigure(format, page, name) =>
        FigureExport.render(format, page, FigureExport.rasters(page, inputs.stimuli)) match
          case Left(why)   => dispatch(ComposerIntent.Exported(Left(why)))
          case Right(file) =>
            inputs.save(name, format, file, a => later(ComposerIntent.Exported(a)))
    }

  private def render(m: AppModel): Unit = if !disposed then
    val v = FigureComposer.view(state, m)
    // The panes are rebuilt from the view-model; focus returns to the control
    // of the same name, so a sync never drops the keyboard user's place.
    keepingFocus {
      renderNavigator(v)
      renderPage(v)
      renderInspector(v)
      renderMethods(v)
    }

  private def keepingFocus(rebuild: => Unit): Unit =
    val panes           = Vector(navigatorNode, pageNode, inspectorNode, methodsDiffNode)
    val owner           = Option(navigatorNode.getScene).flatMap(sc => Option(sc.getFocusOwner))
    def within(n: Node) =
      Iterator.iterate(n)(_.getParent).takeWhile(_ != null).exists(a => panes.exists(_ eq a))
    val name = owner.filter(within).flatMap(o => Option(o.getAccessibleText))
    rebuild
    name.foreach { text =>
      def all(n: Node): Vector[Node] = n +: (n match
        case p: javafx.scene.Parent => p.getChildrenUnmodifiable.asScala.toVector.flatMap(all)
        case _                      => Vector.empty)
      panes
        .flatMap(all)
        .find(n => n.isFocusTraversable && n.getAccessibleText == text)
        .filterNot(n => owner.exists(_ eq n))
        .foreach(_.requestFocus())
    }

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
        greyscale.setAccessibleText(FiguresHost.greyscaleName(p.greyscale))
        greyscale.setOnAction(_ => dispatch(ComposerIntent.SetGreyscale(!p.greyscale)))
        if p.greyscale then greyscale.getStyleClass.add("figures-chosen"): Unit
        else greyscale.getStyleClass.remove("figures-chosen"): Unit
        // A check of the paper only: the export keeps its colours.
        paper.setEffect(if p.greyscale then grey else null)
        title.setText(p.title)
        width.setText(p.widthLabel)
        zoom.setText(p.zoom)
        val px = p.pxPerMm
        paper.setPrefWrapLength(p.width.mm * px)
        paper.setMaxWidth(p.width.mm * px)
        paper.setHgap(PageLayout.GutterMm * px)
        paper.setVgap(PageLayout.GutterMm * px)
        retire(p.panels.map(q => (p.figure, q.letter)).toSet)
        val text    = FigureType.px(p.textPt, px)
        val caption = paperLabel(p.caption, text)
        val stamp   = paperLabel(p.stamp, text)
        stamp.getStyleClass.add("figures-stamp")
        val foot = VBox(2.0, caption, stamp)
        foot.getStyleClass.add("figures-foot")
        foot.setPrefWidth(p.width.mm * px)
        paper.getChildren.setAll(
          (p.panels.map(q => panelNode(p.figure, q, px, text)) :+ foot)*
        ): Unit
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

  private def panelNode(figure: FigureId, p: PanelVM, px: Double, text: Double): Node =
    // The panel's letter is its button: Tab reaches it, and Enter selects it.
    val letter = button(p.letter.value)
    letter.getStyleClass.add("figures-letter")
    paperFont(letter, FigureType.LetterFace, FigureType.px(FigureType.LetterPt, px))
    letter.setAccessibleText(FiguresHost.panelName(p))
    letter.setOnAction(_ => dispatch(ComposerIntent.SelectPanel(figure, p.letter)))
    val name       = paperLabel(p.title, text); name.getStyleClass.add("figures-panel-title")
    val w          = p.widthMm * px
    val body: Node = p.body match
      case PanelBody.Plot(plot) =>
        val twin = twinFor(figure, p, plot.kind, plot.source, plot.lines)
        twin.plotNode.setPrefSize(w, w * PlotGeometry.Aspect)
        val notes = plot.notes.map(paperLabel(_, text))
        VBox(2.0, (twin.plotNode +: notes)*)
      case PanelBody.Maps(maps) =>
        val tiles = HBox(
          4.0,
          maps.tiles.map { t =>
            val head  = paperLabel(t.title, text)
            val score = paperLabel(t.label, text); score.getStyleClass.add("figures-score")
            val tile  = VBox(2.0, head, score)
            tile.getStyleClass.add("figures-tile")
            tile.setPrefWidth((w - 8) / 3)
            tile.setAccessibleText(s"${t.title}, ${t.label}")
            tile
          }*
        )
        val caption = paperLabel(maps.caption, text)
        val note    = paperLabel(maps.maps, text); note.getStyleClass.add("figures-note")
        VBox(4.0, tiles, caption, note)
      case PanelBody.Gaze(g) =>
        val heading = paperLabel(g.heading, text)
        val shown   = paperLabel(g.displayed, text)
        val gaze    = paperLabel(g.gaze, text); gaze.getStyleClass.add("figures-note")
        val drawing = g.drawn.toVector.map { d =>
          val view = gazeFor(figure, p, d)
          view.setPrefSize(w, w * FigureGaze.heightRatio(d))
          view
        }
        VBox(2.0, (drawing ++ Vector(heading, shown, gaze))*)
      case PanelBody.Waiting(why) =>
        val l = paperLabel(why, text); l.getStyleClass.add("figures-note"); l
      case PanelBody.Unavailable(why) =>
        val l = paperLabel(why, text); l.getStyleClass.add("figures-problem"); l
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

  private def builderOf(kind: PlotKind, lines: ParticipantLines): PlotBuilder = kind match
    case PlotKind.Participant =>
      ParticipantPlot(
        ParticipantColumns.standard.fold(e => throw IllegalStateException(e.message), identity),
        lines
      )
    case PlotKind.Profile =>
      ScaleProfilePlot(
        ProfileColumns.standard.fold(e => throw IllegalStateException(e.message), identity)
      )

  private def twinFor(
      figure: FigureId,
      p: PanelVM,
      kind: PlotKind,
      source: PlotSource,
      lines: ParticipantLines
  ): PlotTwin =
    val key   = (figure, p.letter)
    val drawn = twins.getOrElse(
      key, {
        val made = PlotTwin
          .attach(
            builderOf(kind, lines),
            view(s"figures.panel.${figure.number}.${p.letter.value}"),
            view(s"figures.panel.${figure.number}.${p.letter.value}.table"),
            model().selection,
            app
          )
          .fold(e => throw IllegalStateException(e.message), identity)
        Drawn(made, None, kind, lines)
      }
    )
    // A panel whose template changed (D's scale set to every scale) or whose
    // lines changed is drawn by its new builder.
    if drawn.kind != kind || drawn.lines != lines then
      drawn.twin.rebuild(builderOf(kind, lines))
    if !drawn.source.contains(source) then drawn.twin.show(source, Theme.Light)
    twins = twins.updated(key, Drawn(drawn.twin, Some(source), kind, lines))
    drawn.twin

  /** Disposes the plots of panels no longer shown. */
  /** Panels A and B: one trial view per panel, kept while the panel is
    * shown; it reads the display's image itself.
    */
  private var gazes: Map[(FigureId, PanelLetter), TrialView] = Map.empty

  private def gazeFor(figure: FigureId, p: PanelVM, d: GazeTrialVM): TrialView =
    val key  = (figure, p.letter)
    val view = gazes.getOrElse(
      key, {
        val made = TrialView(inputs.stimuli)
        gazes = gazes.updated(key, made)
        made
      }
    )
    view.setAccessibleText(FiguresHost.gazeName(p, d))
    val input = FigureGaze.input(d, Map.empty)
    if !view.input.exists(_.copy(rasters = Map.empty) == input) then view.show(input)
    view

  /** The trial view of panel `letter` of `figure`, while it is shown. */
  def gaze(figure: FigureId, letter: PanelLetter): Option[TrialView] =
    gazes.get((figure, letter))

  private def retire(keep: Set[(FigureId, PanelLetter)]): Unit =
    val (kept, gone) = twins.partition((k, _) => keep.contains(k))
    gone.values.foreach(_.twin.dispose())
    twins = kept
    val (keptGaze, goneGaze) = gazes.partition((k, _) => keep.contains(k))
    goneGaze.values.foreach(_.dispose())
    gazes = keptGaze

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
      ) ++ v.page.toVector.flatMap(p => appearance(p.appearance, p.exporting)) ++
        v.bundle.toVector.flatMap(bundleSection)
    }*): Unit

  /** The inspector's Bundle section (S9.5): a toggle per file, then Export bundle…. */
  private def bundleSection(b: BundleVM): Vector[Node] =
    val head = Label("Bundle"); head.getStyleClass.add("t12")
    val rows = b.rows.map { r =>
      val t = button(FiguresHost.bundleText(r))
      t.setAccessibleText(FiguresHost.bundleName(r))
      if r.chosen && r.unavailable.isEmpty then t.getStyleClass.add("figures-chosen"): Unit
      t.setDisable(r.unavailable.isDefined)
      t.setOnAction(_ => dispatch(ComposerIntent.ToggleBundle(r.item)))
      val why = r.unavailable.toVector.map { w =>
        val l = Label(w); l.setWrapText(true); l.getStyleClass.addAll("figures-note", "t11"); l
      }
      VBox(2.0, (t +: why)*)
    }
    val write = button(b.action)
    write.setDisable(b.written.isEmpty)
    write.setOnAction(_ => dispatch(ComposerIntent.ExportBundle))
    (head +: rows) ++ Vector(write) ++ b.status.toVector.map { s =>
      val l = Label(s); l.setWrapText(true); l.getStyleClass.add("t11"); l
    }

  /** The inspector's Appearance (view only) and Export sections. */
  private def appearance(a: AppearanceVM, e: ExportVM): Vector[Node] =
    def chooser[A](label: String, options: Vector[(A, String, Boolean)])(
        set: A => ComposerIntent
    ) =
      val key     = Label(label); key.getStyleClass.addAll("figures-key", "t11")
      val buttons = options.map { (value, text, chosen) =>
        val b = button(text)
        b.setAccessibleText(FiguresHost.chosenName(label, text, chosen))
        if chosen then b.getStyleClass.add("figures-chosen"): Unit
        b.setOnAction(_ => dispatch(set(value)))
        b
      }
      HBox(6.0, (key +: buttons)*)
    val head  = Label("Appearance"); head.getStyleClass.add("t12")
    val only  = Label("View only"); only.getStyleClass.add("t11")
    val width = a.panelWidthMm.toVector.map { (letter, mm, text) =>
      val key      = Label("Panel width"); key.getStyleClass.addAll("figures-key", "t11")
      val narrower = button("−"); narrower.setAccessibleText(FiguresHost.narrower(text))
      val wider    = button("+"); wider.setAccessibleText(FiguresHost.wider(text))
      val value    = Label(text); value.getStyleClass.add("t11")
      narrower.setOnAction(_ => dispatch(ComposerIntent.SetPanelWidth(letter, mm - 1)))
      wider.setOnAction(_ => dispatch(ComposerIntent.SetPanelWidth(letter, mm + 1)))
      HBox(6.0, key, narrower, value, wider)
    }
    val exporting              = Label("Export"); exporting.getStyleClass.add("t12")
    val (imagesText, imagesOn) = a.includeImages
    val images                 = button(imagesText)
    images.setAccessibleText(FiguresHost.includeName(imagesText, imagesOn))
    if imagesOn then images.getStyleClass.add("figures-chosen"): Unit
    images.setOnAction(_ => dispatch(ComposerIntent.IncludeImages(!imagesOn)))
    Vector(
      HBox(6.0, head, only),
      chooser("Text size", a.textSizes)(ComposerIntent.SetTextSize(_)),
      chooser("Participant lines", a.lines)(ComposerIntent.SetParticipantLines(_))
    ) ++ width ++ Vector(
      exporting,
      chooser("Figure format", e.formats)(ComposerIntent.ChooseFormat(_)),
      images,
      exportButton(e)
    ) ++ e.status.toVector.map { s =>
      val l = Label(s); l.setWrapText(true); l.getStyleClass.add("t11"); l
    }

  private def exportButton(e: ExportVM): Button =
    val b = button(e.action)
    b.setOnAction(_ => dispatch(ComposerIntent.Export))
    b

  /** A label on the paper, in the figure's body face at `px`. */
  private def paperLabel(text: String, px: Double): Label =
    val l = Label(text)
    l.setWrapText(true)
    paperFont(l, FigureType.BodyFace, px)
    l

  // The paper is set in the figure's typography, not the studio's type scale:
  // a bound font is one the scene's stylesheets do not restyle.
  private def paperFont(node: javafx.scene.control.Labeled, face: FontFace, px: Double): Unit =
    node.fontProperty.bind(ReadOnlyObjectWrapper(Font.font(face.javaFxFamily, px)))

  /** Disposes the views; the host ignores the model from then on. Idempotent. */
  def dispose(): Unit =
    if !disposed then
      disposed = true
      retire(Set.empty)
      table.dispose()

  private def renderMethods(v: ComposerVM): Unit =
    val m = v.methods
    methodsHeading.setText(m.fold(MethodsCopy.NoFigure)(_.heading))
    for (b, label) <- Vector(showDiff -> m.map(_.showDiff), regenerate -> m.map(_.regenerate))
    do
      b.setText(label.getOrElse(""))
      b.setAccessibleText(label.getOrElse(""))
      b.setDisable(label.isEmpty)
    val status = m.flatMap(_.status)
    methodsStatus.setText(status.getOrElse(""))
    methodsStatus.setVisible(status.isDefined)
    methodsStatus.setManaged(status.isDefined)
    val (text, why) = m.map(_.text) match
      case Some(Right(t))  => (t, None)
      case Some(Left(why)) => ("", Some(why))
      case None            => ("", Some(MethodsCopy.NoFigure))
    methodsEditor.setDisable(why.isDefined)
    methodsEditor.setPromptText(why.getOrElse(""))
    if methodsEditor.getText != text then
      writing = true
      try methodsEditor.setText(text)
      finally writing = false
    diffCaption.setText(m.fold("")(_.diffCaption))
    diffChoice.getChildren.setAll(m.flatMap(_.choice).toVector.flatMap { (keep, use) =>
      val k = button(keep)
      k.setOnAction(_ => dispatch(ComposerIntent.Methods(MethodsIntent.KeepEdits)))
      val u = button(use)
      u.setOnAction(_ => dispatch(ComposerIntent.Methods(MethodsIntent.UseGenerated)))
      Vector(k, u)
    }*): Unit
    diffLines.getChildren.setAll(m.toVector.flatMap(_.diff).map { line =>
      val l = Label(FiguresHost.diffLine(line))
      l.setWrapText(true)
      l.getStyleClass.addAll("serif", "t12", FiguresHost.diffStyle(line))
      l
    }*): Unit

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
          FocusStop(A11yRole.Button, FiguresHost.greyscaleName(p.greyscale)),
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

  /** The inspector's controls: Open in Compare, Rebind figure…, then the
    * appearance and export controls.
    */
  /** The methods.md pane: Show diff, Regenerate, then the text. */
  def methodsStops: Vector[FocusStop] =
    vm.methods.toVector.flatMap(m =>
      Vector(
        FocusStop(A11yRole.Button, m.showDiff),
        FocusStop(A11yRole.Button, m.regenerate)
      ) ++ m.text.toOption.map(_ => FocusStop(A11yRole.TextArea, FiguresHost.MethodsText))
    )

  /** The diff pane: the choice a regeneration over edits waits on. */
  def methodsDiffStops: Vector[FocusStop] =
    vm.methods.toVector
      .flatMap(_.choice)
      .flatMap((keep, use) =>
        Vector(FocusStop(A11yRole.Button, keep), FocusStop(A11yRole.Button, use))
      )

  def inspectorStops: Vector[FocusStop] =
    val v               = vm
    def b(name: String) = FocusStop(A11yRole.Button, name)
    v.figures.binding.toVector.flatMap(binding =>
      Vector(b(v.page.map(_.openInCompare).getOrElse("Open in Compare")), b(binding.rebind)) ++
        v.page.toVector.flatMap { p =>
          val a = p.appearance
          a.textSizes.map((_, t, c) => b(FiguresHost.chosenName("Text size", t, c))) ++
            a.lines.map((_, t, c) => b(FiguresHost.chosenName("Participant lines", t, c))) ++
            a.panelWidthMm.toVector.flatMap((_, _, t) =>
              Vector(b(FiguresHost.narrower(t)), b(FiguresHost.wider(t)))
            ) ++
            // Export: the format, the images option, then Export figure….
            p.exporting.formats.map((_, t, c) =>
              b(FiguresHost.chosenName("Figure format", t, c))
            ) ++
            Vector(
              b(FiguresHost.includeName(a.includeImages._1, a.includeImages._2)),
              b(p.exporting.action)
            )
        } ++
        // Bundle: each file that can be written, then Export bundle….
        v.bundle.toVector.flatMap(bundle =>
          bundle.rows.filter(_.unavailable.isEmpty).map(r => b(FiguresHost.bundleName(r))) ++
            Option.when(bundle.written.nonEmpty)(b(bundle.action))
        )
    )

  private def spacer(): Region =
    val r = Region(); HBox.setHgrow(r, Priority.ALWAYS); r

object FiguresHost:
  /** A gaze panel drawing's accessible name. */
  def gazeName(p: PanelVM, d: GazeTrialVM): String =
    s"Panel ${p.letter.value} trial ${d.trial.label}: ${d.marks.size} fixations"

  /** A bundle row as drawn: "results.csv · 480 queries × 4 σ". */
  def bundleText(r: BundleRowVM): String =
    if r.detail.isEmpty then r.file else s"${r.file} · ${r.detail}"

  /** A bundle row's accessible name, with whether it is in the bundle. */
  def bundleName(r: BundleRowVM): String =
    s"${bundleText(r)}, ${if r.chosen then "in the bundle" else "not in the bundle"}"

  /** The methods editor's accessible name. */
  val MethodsText: String = "Methods text"

  /** A diff line as shown: removed and added sentences are marked by sign,
    * not by colour alone.
    */
  def diffLine(line: DiffLine): String = line match
    case DiffLine.Same(t)    => s"  $t"
    case DiffLine.Removed(t) => s"− $t"
    case DiffLine.Added(t)   => s"+ $t"

  def diffStyle(line: DiffLine): String = line match
    case DiffLine.Same(_)    => "figures-diff-same"
    case DiffLine.Removed(_) => "figures-diff-removed"
    case DiffLine.Added(_)   => "figures-diff-added"

  /** The greyscale check's accessible name, with its state. */
  def greyscaleName(on: Boolean): String =
    s"Greyscale check, ${if on then "on" else "off"}"

  /** An appearance option's accessible name: "Text size 7 pt, selected". */
  def chosenName(control: String, option: String, chosen: Boolean): String =
    s"$control $option" + (if chosen then ", selected" else "")

  def narrower(width: String): String = s"Narrower − (panel width $width)"
  def wider(width: String): String    = s"Wider + (panel width $width)"

  /** The export option's accessible name, with its state. */
  def includeName(text: String, on: Boolean): String = s"$text, ${if on then "on" else "off"}"

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

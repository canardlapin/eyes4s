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

package eyes4s.studio.desktop

import cats.effect.unsafe.IORuntime
import eyes4s.studio.app.{AppModel, Intent, PlatformDialog}
import eyes4s.studio.app.layout.{PaneId, StudioLayouts}
import eyes4s.studio.app.vm.FocusStop
import eyes4s.studio.app.text.{MessageId, Messages}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.app.{ClockTime, ProjectName}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.admission.{AdmissionLedgerHost, LedgerInputs}
import eyes4s.studio.desktop.compare.{CompareSummaryHost, PanelSources, SummaryInputs}
import eyes4s.studio.desktop.explore.{
  ExploreTimelineHost,
  ExploreTrialViewHost,
  NavigatorDisplays,
  NavigatorInputs,
  TrialViewInputs,
  TrialsNavigatorHost
}
import eyes4s.studio.desktop.trial.StimulusSource
import eyes4s.studio.desktop.figures.{FigureInputs, FiguresHost}
import eyes4s.studio.desktop.dock.{DockGesture, PerspectiveHost}
import eyes4s.studio.desktop.runtime.{
  DesktopEffects,
  PlatformDialogs,
  ProjectPort,
  StudioRuntime,
  StudioSession
}
import eyes4s.studio.desktop.analysis.{DesignInputs, ResolvedDesignHost}
import eyes4s.studio.desktop.importing.{ColumnMappingPaneHost, ImportWizardHost}
import eyes4s.studio.desktop.platform.FilePresetStore
import eyes4s.studio.desktop.shell.AppShell
import javafx.application.Platform
import javafx.scene.control.{Alert, TextInputDialog}
import scaladock.fx.DockTheme

/** Why a studio window could not open. Unreadable saved layouts never
  * stop it: they show their defaults, with a notice.
  */
enum WindowError:
  case Styles(missing: MissingStylesheet)

  def message: String = this match
    case Styles(m) => m.message

/** One studio window's parts (tickets S1.4, S1.5a): the services, the Elm
  * runtime, the perspective host and the shell that renders it.
  */
final class StudioWindow private (
    val session: StudioSession,
    val runtime: StudioRuntime,
    val host: PerspectiveHost,
    val shell: AppShell,
    val effects: DesktopEffects,
    val project: Option[ProjectPort],
    val columnMapping: ColumnMappingPaneHost,
    val admission: AdmissionLedgerHost,
    val summary: CompareSummaryHost,
    summaryListener: AppModel => Unit,
    val navigator: TrialsNavigatorHost,
    navigatorListener: AppModel => Unit,
    val explore: ExploreTrialViewHost,
    exploreListener: AppModel => Unit,
    val timeline: ExploreTimelineHost,
    timelineListener: AppModel => Unit,
    val resolvedDesign: ResolvedDesignHost,
    designListener: AppModel => Unit,
    val figures: FiguresHost,
    figuresListener: AppModel => Unit
):
  /** The window content, with the studio stylesheets. */
  def root: javafx.scene.Parent = shell.root

  /** The title the native window shows now. */
  def title: String = eyes4s.studio.app.vm.Menus.windowTitle(runtime.model)

  /** The controls a pane shows inside its own focus stop, in Tab order. */
  def paneStops(pane: PaneId): Vector[FocusStop] =
    if pane == StudioLayouts.columnMapping then columnMapping.focusStops
    else if pane == StudioLayouts.admission then admission.focusStops
    else if pane == StudioLayouts.compareQueries || pane == StudioLayouts.compareItems then
      eyes4s.studio.app.compare.QueriesNavigator.focusStops(summary.navigatorVM)
    else if pane == StudioLayouts.queryTrial then
      eyes4s.studio.app.compare.TrialPanels.queryStops(summary.panelsVM)
    else if pane == StudioLayouts.referenceTrial then
      eyes4s.studio.app.compare.TrialPanels.referenceStops(summary.panelsVM)
    else if pane == StudioLayouts.contrast then summary.contrastStops
    else if pane == StudioLayouts.trials then navigator.trialsStops
    else if pane == StudioLayouts.items then navigator.itemsStops
    else if pane == StudioLayouts.trialView then explore.focusStops
    else if pane == StudioLayouts.timeline then timeline.focusStops
    else if pane == StudioLayouts.resolvedDesign then resolvedDesign.focusStops
    else
      pane.value match
        case "figures.figures"      => figures.navigatorStops
        case "figures.page"         => figures.pageStops
        case "figures.page.table"   => figures.tableStops
        case "figures.panel"        => figures.inspectorStops
        case "figures.methods"      => figures.methodsStops
        case "figures.methods-diff" => figures.methodsDiffStops
        case _                      => Vector.empty

  /** Store each perspective's arrangement in the document (view-only). */
  def captureLayouts(): Unit = runtime.dispatch(Intent.LayoutsCaptured(host.capture()))

  /** Keep `stage`'s title on the model's window title (S1.4). */
  def bind(stage: javafx.stage.Stage): Unit = runtime.listen(_ => stage.setTitle(title))

  def close(): Unit =
    runtime.unlisten(summaryListener)
    runtime.unlisten(navigatorListener)
    runtime.unlisten(exploreListener)
    runtime.unlisten(timelineListener)
    explore.dispose()
    timeline.dispose()
    runtime.unlisten(designListener)
    runtime.unlisten(figuresListener)
    summary.dispose()
    figures.dispose()
    project.foreach(_.close())
    session.close()

object StudioWindow:

  /** The studio's dock theme: `studio-dock.css`, which maps scaladock's
    * variables onto the studio tokens.
    */
  val dockThemeResource: String = s"${tokens.TokenFiles.resourceDirectory}/studio-dock.css"

  def dockTheme: Either[MissingStylesheet, DockTheme] =
    Option(getClass.getClassLoader.getResource(dockThemeResource))
      .toRight(MissingStylesheet(dockThemeResource))
      .map(url => DockTheme.Custom(url.toExternalForm))

  /** The answer to Rename…: the typed name, or why it was refused. */
  def renameAnswer(text: String): Intent =
    ProjectName.of(text).fold(Intent.RenameRefused(_), Intent.RenameProject(_))

  /** The model's intent for a gesture the dock made itself, if the model does
    * not already agree. A gesture is judged when it is delivered, against
    * whether the dock is maximized *then*: a restore that a later maximize
    * has overtaken is not replayed.
    */
  def follow(model: AppModel, gesture: DockGesture, dockMaximized: Boolean): Option[Intent] =
    gesture match
      case DockGesture.Focused(p) =>
        Option.when(model.focusedPane != p)(Intent.FocusPane(p))
      case DockGesture.Maximized(p) =>
        Option.when(dockMaximized && (!model.isMaximized || model.focusedPane != p))(
          Intent.SetMaximized(Some(p))
        )
      case DockGesture.Restored =>
        Option.when(!dockMaximized && model.isMaximized)(Intent.SetMaximized(None))

  /** Platform dialogs as JavaFX dialogs (non-blocking). */
  def fxDialogs(
      model: () => AppModel,
      messages: Messages,
      project: Option[ProjectPort] = None,
      presets: FilePresetStore = FilePresetStore.userDefault
  ): PlatformDialogs =
    (dialog: PlatformDialog, dispatch: Intent => Unit) =>
      dialog match
        case PlatformDialog.RenameProject =>
          val d = TextInputDialog(model().project.fold("")(_.value))
          d.setTitle(messages(MessageId.CommandRenameProject))
          d.setHeaderText(eyes4s.studio.app.vm.Menus.windowTitle(model(), messages))
          d.setOnHidden(_ => Option(d.getResult).foreach(t => dispatch(renameAnswer(t))))
          d.show()
        case PlatformDialog.ProjectInfo =>
          val a = Alert(Alert.AlertType.INFORMATION)
          a.setTitle(messages(MessageId.CommandProjectInfo))
          a.setHeaderText(eyes4s.studio.app.vm.Menus.windowTitle(model(), messages))
          a.show()
        case PlatformDialog.ImportSources =>
          // The import wizard (S5.2): its commands come back as intents.
          val theme = model().document.presentation.theme match
            case eyes4s.studio.core.document.Theme.Light => Theme.Light
            case eyes4s.studio.core.document.Theme.Dark  => Theme.Dark
          val sheets = StudioStyles.stylesheets(theme).getOrElse(Nil)
          ImportWizardHost.openWindow(
            () => model().document,
            dispatch,
            presets,
            sheets,
            project
          ): Unit
        case PlatformDialog.OpenProject =>
          System.err.println(s"$dialog is not available until S2.9.")

  /** Open a window on `initial`, served by the fake backend at `moment`.
    * On the JavaFX thread. Each execution-service event reaches the model as
    * [[Intent.Execution]] on the JavaFX thread; jobs the document's running
    * runs name are adopted first (t3's run 8).
    */
  def open(
      initial: AppModel,
      moment: StoryMoment,
      displays: NavigatorDisplays,
      stimuli: StimulusSource,
      theme: Theme = Theme.Light,
      dialogs: Option[PlatformDialogs] = None,
      messages: Messages = Messages.english,
      project: Option[ProjectPort] = None,
      clock: () => Option[ClockTime] = DesktopEffects.wallClock,
      nativeMenu: Boolean = AppShell.systemMenuBar,
      presets: FilePresetStore = FilePresetStore.userDefault,
      panels: PanelSources = PanelSources.notServed
  )(using IORuntime): Either[WindowError, StudioWindow] =
    for
      sheets <- StudioStyles.stylesheets(theme).left.map(WindowError.Styles(_))
      dock   <- dockTheme.left.map(WindowError.Styles(_))
      window <- build(
        initial,
        moment,
        displays,
        stimuli,
        dock,
        dialogs,
        messages,
        project,
        clock,
        nativeMenu,
        presets,
        panels
      )
    yield
      window.root.getStylesheets.setAll(sheets*)
      window

  private def build(
      initial: AppModel,
      moment: StoryMoment,
      displays: NavigatorDisplays,
      stimuli: StimulusSource,
      dockTheme: DockTheme,
      dialogs: Option[PlatformDialogs],
      messages: Messages,
      project: Option[ProjectPort],
      clock: () => Option[ClockTime],
      nativeMenu: Boolean,
      presets: FilePresetStore,
      panels: PanelSources
  )(using IORuntime): Either[WindowError, StudioWindow] =
    // Late-bound: the runtime, the host and the effects refer to each other.
    var runtime: Option[StudioRuntime] = None
    def dispatch(i: Intent): Unit      = runtime.foreach(_.dispatch(i))
    def later(i: Intent): Unit         = Platform.runLater(() => dispatch(i))

    val session = StudioSession.start(moment, e => later(Intent.Execution(e)))
    // A gesture is reported after the dock's own update has finished, and
    // judged against the model and the dock it then meets.
    var dockOf: () => Boolean = () => false
    val host                  = PerspectiveHost(
      StudioLayouts.spec,
      dockTheme,
      gesture =>
        Platform.runLater { () =>
          runtime.foreach(r => follow(r.model, gesture, dockOf()).foreach(r.dispatch))
        }
    )
    dockOf = () => host.dock.state.maximized.isDefined
    // Late-bound too: a verification's answer goes to the admission ledger.
    var ledger: Option[AdmissionLedgerHost] = None
    val effects                             = DesktopEffects(
      session,
      dialogs.getOrElse(
        fxDialogs(() => runtime.fold(initial)(_.model), messages, project, presets)
      ),
      p =>
        host.reset(p)
        runtime.foreach(r => host.sync(r.model))
      ,
      host.perform,
      f => Platform.runLater(() => f()),
      project,
      clock,
      (dataset, content, answer) => ledger.foreach(_.verified(dataset, content, answer))
    )
    val adopted = session.adopt(initial.document)
    adopted.collect { case Left(e) => e }.foreach(e => System.err.println(e.message))
    val booted = AppModel.update(initial, Intent.JobsChanged(session.jobs))._1
    val r      = StudioRuntime(booted, effects)
    runtime = Some(r)
    val shell      = AppShell(host, dispatch, messages, () => r.model, nativeMenu)
    val unreadable = host.restore(booted.document.presentation.layouts, booted)
    if unreadable.nonEmpty then
      r.dispatch(
        Intent.LayoutsUnreadable(
          unreadable.map(_.perspective),
          unreadable.map(_.reason).distinct.mkString("; ")
        )
      )
    r.listen(shell.render)
    // The column-mapping pane (Data): the import wizard on the selected
    // revision. Saved presets are read once, off the JavaFX thread.
    val mapping = ColumnMappingPaneHost(
      () => r.model,
      dispatch,
      ImportWizardHost.fxPlatform(
        () => Option(shell.root.getScene).map(_.getWindow).orNull,
        presets,
        project
      ),
      project
    )
    val presetReader = Thread(
      () =>
        val (saved, errors) = presets.load
        Platform.runLater(() => mapping.presetsLoaded(saved, errors))
      ,
      "eyes4s-presets-read"
    )
    presetReader.setDaemon(true)
    presetReader.start()
    host.host(StudioLayouts.columnMapping, mapping.node)
    r.listen(mapping.sync)
    // The admission ledger (Data): the selected revision's counts, and Admit.
    val admission = AdmissionLedgerHost(() => r.model, dispatch, LedgerInputs.of(session))
    ledger = Some(admission)
    host.host(StudioLayouts.admission, admission.node)
    r.listen(admission.sync)
    // Compare's summary layout (Results board): the shown run's summary.
    val summary =
      CompareSummaryHost(() => r.model, dispatch, SummaryInputs.of(session), panels)
    Vector(
      "compare.participant-plot"       -> summary.participantPlot.plotNode,
      "compare.participant-plot.table" -> summary.participantPlot.tableNode,
      "compare.scale-profile"          -> summary.scaleProfile.plotNode,
      "compare.scale-profile.table"    -> summary.scaleProfile.tableNode,
      "compare.participant-table"      -> summary.participantNode,
      "compare.query-table"            -> summary.queryTable,
      "compare.queries"                -> summary.queries.node,
      "compare.query-trial"            -> summary.panels.queryNode,
      "compare.reference-trial"        -> summary.panels.referenceNode,
      "compare.contrast"               -> summary.contrastNode,
      "compare.query-trial.table"      -> summary.queryTrialTable,
      "compare.reference-trial.table"  -> summary.referenceTrialTable,
      "compare.items"                  -> summary.items.node
    ).foreach((id, node) => PaneId.of(id).foreach(host.host(_, node)))
    val summaryListener: AppModel => Unit = summary.sync
    r.listen(summaryListener)
    // The trials navigator (Explore): the latest admitted revision's trials.
    val navigator =
      TrialsNavigatorHost(() => r.model, dispatch, NavigatorInputs.of(session, displays))
    host.host(StudioLayouts.trials, navigator.trials.node)
    host.host(StudioLayouts.items, navigator.items.node)
    val navigatorListener: AppModel => Unit = navigator.sync
    r.listen(navigatorListener)
    // Explore's trial view: the explored trial under the shown run's revision.
    val explore =
      ExploreTrialViewHost(() => r.model, TrialViewInputs.of(session, displays), stimuli)
    host.host(StudioLayouts.trialView, explore.node)
    val exploreListener: AppModel => Unit = explore.sync
    r.listen(exploreListener)
    // Explore's timeline: the trial view's fixations, the playhead and the brush.
    val timeline = ExploreTimelineHost(() => r.model, dispatch, () => explore.state)
    host.host(StudioLayouts.timeline, timeline.node)
    host.host(StudioLayouts.timelineTable, timeline.tableNode)
    explore.onChange(() => timeline.refresh())
    val timelineListener: AppModel => Unit = timeline.sync
    r.listen(timelineListener)
    // The resolved-design table (Analysis): the backend's preview of the
    // target revision, prepared once the perspective is shown.
    val design = ResolvedDesignHost(dispatch, DesignInputs.of(session))
    host.host(StudioLayouts.resolvedDesign, design.node)
    val designListener: AppModel => Unit = design.sync
    r.listen(designListener)
    design.sync(r.model)
    // The Figures perspective: navigator, page, Table tab and binding.
    val figures = FiguresHost(
      () => r.model,
      dispatch,
      FigureInputs.of(
        session,
        displays,
        () => Option(shell.root.getScene).map(_.getWindow),
        project,
        stimuli
      )
    )
    Vector(
      "figures.figures"      -> figures.navigatorNode,
      "figures.page"         -> figures.pageNode,
      "figures.page.table"   -> figures.tableNode,
      "figures.panel"        -> figures.inspectorNode,
      "figures.methods"      -> figures.methodsNode,
      "figures.methods-diff" -> figures.methodsDiffNode
    ).foreach((id, node) => PaneId.of(id).foreach(host.host(_, node)))
    val figuresListener: AppModel => Unit = figures.sync
    r.listen(figuresListener)
    figures.sync(r.model)
    Right(
      StudioWindow(
        session,
        r,
        host,
        shell,
        effects,
        project,
        mapping,
        admission,
        summary,
        summaryListener,
        navigator,
        navigatorListener,
        explore,
        exploreListener,
        timeline,
        timelineListener,
        design,
        designListener,
        figures,
        figuresListener
      )
    )

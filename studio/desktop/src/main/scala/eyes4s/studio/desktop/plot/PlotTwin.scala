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

package eyes4s.studio.desktop.plot

import eyes4s.studio.app.Intent
import eyes4s.studio.app.plot.{PlotSource, PlotText, PlotTextId}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.selection.{SelectionState, StudioRef, ViewId}
import eyes4s.studio.viz.plot.{
  BuiltPlot,
  MarkInputState,
  OverlayPalette,
  PlotBuildError,
  PlotBuilder,
  PlotReadout,
  PlotTargetError,
  PlotTargets
}
import javafx.application.Platform
import javafx.beans.property.{ReadOnlyObjectProperty, ReadOnlyObjectWrapper}
import javafx.beans.value.ChangeListener
import javafx.geometry.Pos
import javafx.scene.control.Label
import javafx.scene.layout.StackPane

/** What a [[PlotTwin]] shows. */
enum PlotTwinStatus derives CanEqual:
  case Empty

  /** `plot` is handed to the canvas host and its source to the table. */
  case Shown(plot: BuiltPlot)

  /** The builder refused `source`: the plot pane says why, and the table
    * still lists every row.
    */
  case Refused(source: PlotSource, error: PlotBuildError)

  case Disposed

/** The two views of a [[PlotTwin]]. */
enum PlotTwinView derives CanEqual:
  case Plot, Table

/** Why a plot host could not be attached. */
enum PlotTwinError derives CanEqual:
  /** A pointer tolerance must be finite and not negative. */
  case InvalidTolerance(toleranceLogicalPx: Double)

  /** The plot and its table must be two views of the bus. */
  case SameView(view: ViewId)

  def message: String = this match
    case InvalidTolerance(t) =>
      s"pointer tolerance $t logical px is not finite and non-negative"
    case SameView(v) => s"the plot and its table are both view '${v.value}'"

/** The common plot host (ticket S4.5a): one value source shown as a plot and
  * as its TableTwin.
  *
  * [[show]] builds the plot from the source with the host's [[PlotBuilder]]
  * (S4.5b–e plug in here) and hands the same source to the table, so the
  * marks and the rows show the same values. The plot is a [[CanvasPlotHost]]
  * with a [[MarkInputAdapter]] (one focus stop, roving cursor, pointer
  * picks); the table is a [[TableTwinView]]. They are two views of the
  * selection bus with their own ids, and both show only what the bus
  * [[project]]s: selecting a row selects its mark, and selecting a mark its
  * row. Moving keyboard focus from one to the other by traversal carries
  * the cursor ([[focusArrived]]).
  *
  * [[plotNode]] and [[tableNode]] are the two panes' contents; the dock puts
  * them in one group as the plot's tab and its sibling Table tab
  * ([[eyes4s.studio.app.layout.StudioLayouts]]). FX thread only.
  */
final class PlotTwin private (
    initialBuilder: PlotBuilder,
    plotView: ViewId,
    tableView: ViewId,
    selection: SelectionState,
    dispatch: Intent => Unit,
    toleranceLogicalPx: Double
):

  private var builder: PlotBuilder = initialBuilder

  /** The plot's canvas host. */
  val plotHost: CanvasPlotHost = CanvasPlotHost()

  private val refusal = Label()
  refusal.getStyleClass.addAll("plot-refusal", "t12")
  refusal.setVisible(false)
  refusal.setMouseTransparent(true)

  // What the hovered mark, or else the selection's mark, says: its rows'
  // words, as the table writes them (a group's n, for one).
  private val readout = Label()
  readout.getStyleClass.addAll("plot-readout", "t11")
  readout.setVisible(false)
  readout.setMouseTransparent(true)
  StackPane.setAlignment(readout, Pos.TOP_RIGHT)

  /** The plot pane's content. */
  val plotNode: StackPane = StackPane(plotHost, refusal, readout)
  plotNode.getStyleClass.add("plot-pane")
  Option(getClass.getClassLoader.getResource(TableTwinView.stylesheetResource))
    .foreach(url => plotNode.getStylesheets.add(url.toExternalForm))

  /** The Table tab's content. */
  val table: TableTwinView     = TableTwinView.attach(tableView, selection, dispatch)
  def tableNode: TableTwinView = table

  private val statusWrapper =
    ReadOnlyObjectWrapper[PlotTwinStatus](this, "status", PlotTwinStatus.Empty)
  private var theme: Theme                = Theme.Light
  private var disposed: Boolean           = false
  private var hovered: Option[StudioRef]  = None
  private var selected: Vector[StudioRef] = selection.selected

  private object Layer extends MarkLayer[StudioRef, PlotTargetError, PlotTargets]:
    def resolve(frame: PlotFrame): Option[Either[PlotTargetError, PlotTargets]] =
      shownPlot
        .filter(_.plot.scene eq frame.plan.scene)
        .map(PlotTargets.resolve(_, frame.transform, frame.picking))
    def palette: Option[OverlayPalette] = shownPlot.map(_ => OverlayPalette.onSurface(theme))
    def spoken(state: MarkInputState[StudioRef], targets: PlotTargets): Option[String] =
      Some(targets.accessibleText(state))
    def roleDescription: String       = PlotText(PlotTextId.PlotRole)
    override def idle: Option[String] = statusWrapper.get match
      case PlotTwinStatus.Refused(source, error) =>
        Some(PlotText(PlotTextId.Refused, source.caption, error.message))
      case PlotTwinStatus.Shown(plot) => Some(plot.idleText)
      case _                          => Some(PlotText(PlotTextId.NothingDrawn))

  /** The plot's input: its roving cursor, hover and projected selection. */
  val input: MarkInputAdapter[StudioRef, PlotTargetError, PlotTargets] =
    MarkInputAdapter(
      plotHost,
      Layer,
      MarkInputState.initial(plotView, selection),
      plotIntent,
      toleranceLogicalPx
    )

  // The plot's intents go to the app; its own hover also to the readout.
  private def plotIntent(intent: Intent): Unit =
    dispatch(intent)
    intent match
      case Intent.HoverOver(view, target) if view == plotView =>
        hovered = target
        describeMark()
      case _ => ()

  // A node's focus-visible flag is set with its focus, before either is
  // notified: it is true only when keyboard traversal brought the focus.
  private val plotFocus: ChangeListener[java.lang.Boolean] = (_, _, now) =>
    if now.booleanValue then focusArrived(PlotTwinView.Plot, plotHost.isFocusVisible)
  private val tableFocus: ChangeListener[java.lang.Boolean] = (_, _, now) =>
    if now.booleanValue then focusArrived(PlotTwinView.Table, table.isFocusVisible)
  plotHost.focusedProperty.addListener(plotFocus)
  table.focusedProperty.addListener(tableFocus)

  /** Keyboard focus arrived at `view`. When keyboard traversal brought it
    * (`byKeyboard`), the other view's cursor comes along: the plot focuses
    * the table's cursor row, the table puts its cursor on the plot's focused
    * mark (or keeps it where it is when it is on one of that mark's rows).
    * Focus from a pointer press or a program carries nothing, so a click
    * lands where it was aimed and the table does not scroll away.
    */
  def focusArrived(view: PlotTwinView, byKeyboard: Boolean): Unit =
    onFxThread("focusArrived")
    if !disposed && byKeyboard then
      view match
        case PlotTwinView.Plot  => input.moveFocus(table.state.cursor)
        case PlotTwinView.Table =>
          val focus = input.state.focus
          table.carry(
            focus
              .flatMap(f => input.targets.flatMap(_.target(f)))
              .fold(focus.toVector)(_.refs)
          )

  /** What the host shows. */
  def status: ReadOnlyObjectProperty[PlotTwinStatus] = statusWrapper.getReadOnlyProperty

  /** What the plot's readout line says ([[PlotReadout.of]] of the hover
    * intent's key and the projected selection).
    */
  def readoutText: Option[String] = Option(readout.getText).filter(_.nonEmpty)

  private def describeMark(): Unit =
    if !disposed then
      val said = shownPlot.flatMap(PlotReadout.of(_, hovered, selected))
      readout.setText(said.getOrElse(""))
      readout.setVisible(said.isDefined)

  /** The plot on the canvas host, if the builder accepted the source. */
  def plot: Option[BuiltPlot] = shownPlot

  private def shownPlot: Option[BuiltPlot] = statusWrapper.get match
    case PlotTwinStatus.Shown(p) => Some(p)
    case _                       => None

  /** Shows `source` in `theme`: builds the plot and lists the rows. */
  def show(source: PlotSource, theme: Theme): Unit =
    onFxThread("show")
    if !disposed then
      this.theme = theme
      table.show(source)
      builder.build(source, theme) match
        case Right(plot) =>
          refusal.setVisible(false)
          refusal.setText("")
          statusWrapper.set(PlotTwinStatus.Shown(plot))
          plotHost.show(plot.plot)
        case Left(error) =>
          plotHost.clear()
          refusal.setText(error.message)
          refusal.setVisible(true)
          statusWrapper.set(PlotTwinStatus.Refused(source, error))
      input.refresh()
      describeMark()

  /** Draws the shown source again with `next`, as a brush's span changes
    * the timeline's builder (S4.5e); the table, which reads the same source,
    * is unchanged. With nothing shown, `next` draws the next source.
    */
  def rebuild(next: PlotBuilder): Unit =
    onFxThread("rebuild")
    if !disposed then
      builder = next
      statusWrapper.get match
        case PlotTwinStatus.Shown(plot)                     => show(plot.source, theme)
        case PlotTwinStatus.Refused(source, _)              => show(source, theme)
        case PlotTwinStatus.Empty | PlotTwinStatus.Disposed => ()

  /** Shows nothing. */
  def clear(): Unit =
    onFxThread("clear")
    if !disposed then
      plotHost.clear()
      table.clear()
      refusal.setVisible(false)
      statusWrapper.set(PlotTwinStatus.Empty)
      input.refresh()
      describeMark()

  /** The selection as the bus now holds it, for the plot and the table. */
  def project(selection: SelectionState): Unit =
    onFxThread("project")
    if !disposed then
      input.project(selection)
      table.project(selection)
      selected = selection.selected
      describeMark()

  /** Disposes the input, the canvas host and the table. Idempotent. */
  def dispose(): Unit =
    onFxThread("dispose")
    if !disposed then
      disposed = true
      plotHost.focusedProperty.removeListener(plotFocus)
      table.focusedProperty.removeListener(tableFocus)
      input.dispose()
      plotHost.dispose()
      table.dispose()
      plotNode.getChildren.clear()
      statusWrapper.set(PlotTwinStatus.Disposed)

  private def onFxThread(operation: String): Unit =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"PlotTwin.$operation must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )

object PlotTwin:

  /** The pointer tolerance: a hit within this many logical pixels of a mark's
    * painted outline picks it.
    */
  val DefaultTolerancePx: Double = 4.0

  /** A plot host drawing with `builder`, its plot as `plotView` and its table
    * as `tableView`, showing `selection`; selection and hover intents go to
    * `dispatch`. On the FX application thread.
    */
  def attach(
      builder: PlotBuilder,
      plotView: ViewId,
      tableView: ViewId,
      selection: SelectionState,
      dispatch: Intent => Unit,
      toleranceLogicalPx: Double = DefaultTolerancePx
  ): Either[PlotTwinError, PlotTwin] =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        "PlotTwin.attach must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )
    else if !toleranceLogicalPx.isFinite || toleranceLogicalPx < 0.0 then
      Left(PlotTwinError.InvalidTolerance(toleranceLogicalPx))
    else if plotView == tableView then Left(PlotTwinError.SameView(plotView))
    else Right(PlotTwin(builder, plotView, tableView, selection, dispatch, toleranceLogicalPx))

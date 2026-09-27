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
import eyes4s.studio.app.plot.{
  PlotSource,
  PlotText,
  PlotTextId,
  TableTwinState,
  TableTwinStep,
  TableTwinVM
}
import eyes4s.studio.core.selection.{SelectionState, StudioRef, ViewId}
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.application.Platform
import javafx.beans.value.ChangeListener
import javafx.css.PseudoClass
import javafx.event.EventHandler
import javafx.scene.{AccessibleAttribute, AccessibleRole}
import javafx.scene.control.{Label, ScrollPane}
import javafx.scene.input.{KeyEvent, MouseButton, MouseEvent}
import javafx.scene.layout.{HBox, Priority, VBox}

import scala.jdk.CollectionConverters.*

/** A plot's Table tab (ticket S4.5a): every row of the plot's value source,
  * written by [[PlotSource.text]], with the selection the plot shows.
  *
  * The table is one focus stop with an arrow-key row cursor (DESIGN_SPEC
  * sections 10 and 12); rows are not tab stops. Keys and clicks go to the
  * pure [[TableTwinState]], whose selection intents are dispatched; a row
  * shows as selected only when the bus [[project]]s it back, so a row and
  * its plot mark always agree. Rows are rebuilt only when the source
  * changes; a selection or cursor change restyles them.
  *
  * FX thread only.
  */
final class TableTwinView private (initial: TableTwinState, dispatch: Intent => Unit)
    extends VBox:
  import TableTwinView.*

  getStyleClass.add("table-twin")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => getStylesheets.add(url.toExternalForm))
  setFocusTraversable(true)
  setAccessibleRole(AccessibleRole.PARENT)
  setAccessibleRoleDescription(PlotText(PlotTextId.TableTab))

  private val head = HBox()
  head.getStyleClass.add("table-twin-head")
  private val body = VBox()
  body.getStyleClass.add("table-twin-body")
  private val scroll = ScrollPane(body)
  scroll.setFitToWidth(true)
  scroll.setFocusTraversable(false)
  scroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  VBox.setVgrow(scroll, Priority.ALWAYS)
  getChildren.setAll(head, scroll)

  private var current: TableTwinState                          = initial
  private var shown: Option[PlotSource]                        = None
  private var rows: Vector[HBox]                               = Vector.empty
  private var disposed: Boolean                                = false
  private val keyHandler: EventHandler[KeyEvent]               = e => onKey(e)
  private val focusListener: ChangeListener[java.lang.Boolean] =
    (_, _, now) => commit(current.focusChanged(now.booleanValue))
  addEventHandler(KeyEvent.KEY_PRESSED, keyHandler)
  focusedProperty.addListener(focusListener)
  render()

  /** The row cursor and the projected selection. */
  def state: TableTwinState = current

  /** The source the table lists, if any. */
  def source: Option[PlotSource] = shown

  /** What the table shows, if it lists a source. */
  def vm: Option[TableTwinVM] = shown.map(current.vm)

  /** The row nodes, in source order. */
  private[plot] def rowNodes: Vector[HBox] = rows

  /** Lists `source`; a cursor on a row it lacks is dropped. */
  def show(source: PlotSource): Unit =
    onFxThread("show")
    if !disposed then
      val rebuild = !shown.contains(source)
      shown = Some(source)
      if rebuild then build(source)
      commit(current.retarget(source), force = true)

  /** Lists nothing. */
  def clear(): Unit =
    onFxThread("clear")
    if !disposed then
      shown = None
      rows = Vector.empty
      head.getChildren.clear()
      body.getChildren.clear()
      render()

  /** The selection as the bus now holds it: restyles, emits nothing. */
  def project(selection: SelectionState): Unit =
    onFxThread("project")
    if !disposed then commit(current.project(selection))

  /** Puts the cursor on `ref` without selecting it, if the table lists it. */
  def moveCursor(ref: Option[StudioRef]): Unit =
    onFxThread("moveCursor")
    shown.foreach(s => commit(current.moveCursor(ref, s)))

  def isDisposed: Boolean = disposed

  /** Removes the table's handlers and rows. Idempotent. */
  def dispose(): Unit =
    onFxThread("dispose")
    if !disposed then
      disposed = true
      removeEventHandler(KeyEvent.KEY_PRESSED, keyHandler)
      focusedProperty.removeListener(focusListener)
      shown = None
      rows = Vector.empty
      head.getChildren.clear()
      body.getChildren.clear()

  // --- Rendering ---------------------------------------------------------------------

  private def cell(text: String, numeric: Boolean, classes: String*): Label =
    val l = Label(text)
    l.getStyleClass.addAll(classes*)
    if numeric then l.getStyleClass.add("numeric"): Unit
    // Equal shares of the row: every row and the header divide one width alike.
    l.setMinWidth(0.0)
    l.setPrefWidth(0.0)
    l.setMaxWidth(Double.MaxValue)
    HBox.setHgrow(l, Priority.ALWAYS)
    l

  private def build(source: PlotSource): Unit =
    val vm = current.vm(source)
    head.getChildren.setAll(vm.headers.map(h => cell(h.text, h.numeric, "t11")).asJava)
    rows = vm.rows.map { r =>
      val row = HBox()
      row.getStyleClass.add("table-twin-row")
      row.getChildren.setAll(
        r.cells.zipWithIndex.map { (c, i) =>
          // The first column names the row (sans); values are mono.
          val face = if i == 0 then Vector("t12") else Vector("mono", "t12")
          cell(c.text, c.numeric, ("table-twin-cell" +: face)*)
        }.asJava
      )
      row.addEventHandler(
        MouseEvent.MOUSE_CLICKED,
        (e: MouseEvent) =>
          if e.getButton == MouseButton.PRIMARY then
            requestFocus()
            shown.foreach(s =>
              commit(current.click(r.ref, e.isShiftDown || e.isShortcutDown, s))
            )
      )
      row
    }
    body.getChildren.setAll(rows.asJava): Unit

  private def render(): Unit =
    val model = vm
    model.foreach { m =>
      m.rows.zip(rows).foreach { (r, node) =>
        node.pseudoClassStateChanged(Selected, r.selected)
        node.pseudoClassStateChanged(Cursor, r.cursor)
      }
      m.cursorRow.foreach(reveal)
    }
    val text = model.map(_.accessibleText)
    setAccessibleText(text.orNull)
    notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT)

  // Scrolls the cursor row into view.
  private def reveal(row: Int): Unit =
    rows.lift(row).foreach { node =>
      val content  = body.getHeight
      val viewport = scroll.getViewportBounds.getHeight
      if content > viewport && viewport > 0.0 then
        val top    = scroll.getVvalue * (content - viewport)
        val bounds = node.getBoundsInParent
        val next   =
          if bounds.getMinY < top then bounds.getMinY
          else if bounds.getMaxY > top + viewport then bounds.getMaxY - viewport
          else top
        scroll.setVvalue(next / (content - viewport))
    }

  // --- Input -------------------------------------------------------------------------

  private def onKey(e: KeyEvent): Unit =
    for
      s <- shown
      k <- RovingKeys.of(e)
    do
      commit(current.handle(k, s))
      e.consume()

  // The state is updated before dispatching: the bus may project its result
  // back synchronously, which must see (and not be overwritten by) this step.
  private def commit(step: TableTwinStep, force: Boolean = false): Unit =
    if !disposed then
      current = step.state
      step.intents.foreach(dispatch)
      if step.changed || force then render()

  private def onFxThread(operation: String): Unit =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"TableTwinView.$operation must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )

object TableTwinView:

  /** The classpath resource of the plot host's stylesheet. */
  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-plot.css"

  /** A row shown as selected, and the row under the cursor. */
  val Selected: PseudoClass = PseudoClass.getPseudoClass("selected")
  val Cursor: PseudoClass   = PseudoClass.getPseudoClass("cursor")

  /** A table as `view`, showing `selection`; its selection intents go to
    * `dispatch`. On the FX application thread.
    */
  def attach(view: ViewId, selection: SelectionState, dispatch: Intent => Unit): TableTwinView =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        "TableTwinView.attach must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )
    TableTwinView(TableTwinState.initial(view, selection), dispatch)

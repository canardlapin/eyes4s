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
  TableColumns,
  TableRowVM,
  TableTwinState,
  TableTwinStep,
  TableTwinVM
}
import eyes4s.studio.core.selection.{SelectionState, StudioRef, ViewId}
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.application.Platform
import javafx.beans.value.ChangeListener
import javafx.collections.FXCollections
import javafx.css.PseudoClass
import javafx.event.EventHandler
import javafx.scene.{AccessibleAction, AccessibleAttribute, AccessibleRole, Node}
import javafx.scene.control.{ContentDisplay, Label, ListCell, ListView}
import javafx.scene.input.{KeyEvent, MouseButton, MouseEvent}
import javafx.scene.layout.{Pane, Priority, VBox}

import scala.jdk.CollectionConverters.*

/** A plot's Table tab (ticket S4.5a): every row of the plot's value source,
  * written by [[PlotSource.text]], with the selection the plot shows.
  *
  * The table is one focus stop with an arrow-key row cursor (DESIGN_SPEC
  * sections 10 and 12); rows are not tab stops. Keys and clicks go to the
  * pure [[TableTwinState]], whose selection intents are dispatched; a row
  * shows as selected only when the bus [[project]]s it back, so a row and
  * its plot mark always agree.
  *
  * Rows are virtualised (bead bd-01M3JD4G5D3GF9NHPWANXFS3PX): a `ListView`
  * builds only the rows in view, each from [[TableTwinState.rowVM]], so a
  * run's thousands of pair rows cost what a screenful does. Columns take
  * the shares [[TableColumns]] sizes to their content.
  *
  * To assistive technology the focus stop is a table (`TABLE_VIEW`): it
  * answers its row and column counts, the cursor row as the focus item, the
  * selected rows, and the cell at a row and column, scrolling it into view;
  * its rows are `TABLE_ROW`s and its cells `TABLE_CELL`s that know their
  * row, column and selection.
  *
  * FX thread only.
  */
final class TableTwinView private (initial: TableTwinState, dispatch: Intent => Unit)
    extends VBox:
  import TableTwinView.*

  getStyleClass.add("table-twin")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => getStylesheets.add(url.toExternalForm))
  setFocusTraversable(false)
  setAccessibleRole(AccessibleRole.TABLE_VIEW)
  setAccessibleRoleDescription(PlotText(PlotTextId.TableTab))

  private var shares: Vector[Double] = Vector.empty

  private val head = ColumnRow(() => shares)
  head.getStyleClass.add("table-twin-head")
  private val list = ListView[Integer]()
  list.getStyleClass.add("table-twin-body")
  list.setFocusTraversable(false)
  list.setFixedCellSize(RowHeight)
  // The list is how the rows are laid out, not a control of its own: the
  // table answers for its rows.
  list.setAccessibleRole(AccessibleRole.PARENT)
  VBox.setVgrow(list, Priority.ALWAYS)
  getChildren.setAll(head, list)

  private var current: TableTwinState                          = initial
  private var shown: Option[PlotSource]                        = None
  private var disposed: Boolean                                = false
  private val keyHandler: EventHandler[KeyEvent]               = e => onKey(e)
  private val focusListener: ChangeListener[java.lang.Boolean] =
    (_, _, now) => commit(current.focusChanged(now.booleanValue))
  addEventFilter(KeyEvent.KEY_PRESSED, keyHandler)
  focusedProperty.addListener(focusListener)

  // One row per visible index, rebound to the state whenever it is reused.
  list.setCellFactory(_ => RowCell())

  /** A row: its cells are updated in place, so the node a reader holds for
    * a row's cell stays the same node while the row stays in view (review
    * F2).
    */
  private final class RowCell extends ListCell[Integer]:
    getStyleClass.add("table-twin-row")
    setAccessibleRole(AccessibleRole.TABLE_ROW)
    private var row: Option[TableRowVM] = None
    private var at: Int                 = -1
    private val cells                   = ColumnRow(() => shares)
    // The list's own selection is never used: the bus selects.
    addEventFilter(
      MouseEvent.MOUSE_PRESSED,
      (e: MouseEvent) =>
        e.consume()
        TableTwinView.this.requestFocus()
    )
    addEventFilter(
      MouseEvent.MOUSE_CLICKED,
      (e: MouseEvent) =>
        e.consume()
        if e.getButton == MouseButton.PRIMARY then
          for
            s <- shown
            r <- row
          do commit(current.click(r.ref, e.isShiftDown || e.isShortcutDown, s))
    )

    def vm: Option[TableRowVM] = row

    /** The row this cell shows, or -1. */
    def rowIndex: Int = at

    def cellAt(column: Int): Option[TwinCell] =
      cells.getChildren.asScala.lift(column).collect { case c: TwinCell => c }

    /** Shows row `index` of the current state, or nothing. */
    def bind(index: Option[Int]): Unit =
      setText(null)
      row = index.flatMap(i => shown.flatMap(current.rowVM(_, i)))
      at = if row.isDefined then index.getOrElse(-1) else -1
      row match
        case None =>
          setGraphic(null)
          cells.getChildren.clear()
          pseudoClassStateChanged(Selected, false)
          pseudoClassStateChanged(Cursor, false)
          pseudoClassStateChanged(Pinned, false)
        case Some(r) =>
          val kids = cells.getChildren
          if kids.size != r.cells.size then
            kids.setAll(r.cells.indices.map(i => TwinCell(i)).asJava): Unit
          r.cells.zipWithIndex.foreach { (c, i) =>
            kids.get(i) match
              case t: TwinCell =>
                t.show(c.text, c.numeric, at, r.selected, r.annotation.filter(_ => i == 0))
              case _ => ()
          }
          if getGraphic ne cells then setGraphic(cells)
          pseudoClassStateChanged(Selected, r.selected)
          pseudoClassStateChanged(Cursor, r.cursor)
          pseudoClassStateChanged(Pinned, r.pinned)
      // A list row rebound by scrolling may now hold, or no longer hold, the
      // focus item.
      if this ne privateRow then rowRebound()

    /** The same row again, from the current state. */
    def rebind(): Unit = bind(Option.when(at >= 0)(at))

    override def updateItem(item: Integer, empty: Boolean): Unit =
      super.updateItem(item, empty)
      bind(Option(item).filter(_ => !empty).map(_.intValue))

    override def queryAccessibleAttribute(
        attribute: AccessibleAttribute,
        parameters: AnyRef*
    ): AnyRef =
      attribute match
        case AccessibleAttribute.INDEX    => Integer.valueOf(at)
        case AccessibleAttribute.SELECTED => java.lang.Boolean.valueOf(row.exists(_.selected))
        case AccessibleAttribute.TEXT     => row.map(_.accessibleText).orNull
        case _ => super.queryAccessibleAttribute(attribute, parameters*)

  // A row a reader asks for that is not in view: built from the state, out
  // of the list, so answering never scrolls (as TableViewSkin's private
  // cell; review F1).
  private val privateRow: RowCell =
    val c = RowCell()
    c.setVisible(false)
    c.setManaged(false)
    getChildren.add(c)
    c

  /** The row cursor and the projected selection. */
  def state: TableTwinState = current

  /** The source the table lists, if any. */
  def source: Option[PlotSource] = shown

  /** What the table shows, if it lists a source. Builds every row: for
    * tests and small tables; the view itself never needs it.
    */
  def vm: Option[TableTwinVM] = shown.map(current.vm)

  /** How many rows the table lists. */
  def rowCount: Int = shown.fold(0)(_.rows.size)

  /** The columns' shares of the width ([[TableColumns.shares]]). */
  def columnShares: Vector[Double] = shares

  /** Lists `source`; a cursor on a row it lacks is dropped. */
  def show(source: PlotSource): Unit =
    onFxThread("show")
    if !disposed then
      // A source is the same when it is the same value: a kept source is not
      // compared row by row (S8.5 review).
      if !shown.exists(_ eq source) then
        shown = Some(source)
        shares = TableColumns.shares(source)
        head.getChildren.setAll(
          current
            .headers(source)
            .zipWithIndex
            .map((h, i) => HeaderCell(h.text, h.numeric, i))
            .asJava
        )
        list.setItems(
          FXCollections.observableArrayList(source.rows.indices.map(Integer.valueOf).asJava)
        )
        notifyAccessibleAttributeChanged(AccessibleAttribute.ROW_COUNT)
        notifyAccessibleAttributeChanged(AccessibleAttribute.COLUMN_COUNT)
      commit(current.retarget(source), force = true)

  /** Lists nothing. */
  def clear(): Unit =
    onFxThread("clear")
    if !disposed then
      shown = None
      shares = Vector.empty
      head.getChildren.clear()
      list.setItems(FXCollections.observableArrayList())
      notifyAccessibleAttributeChanged(AccessibleAttribute.ROW_COUNT)
      notifyAccessibleAttributeChanged(AccessibleAttribute.COLUMN_COUNT)
      render()

  /** The selection as the bus now holds it: restyles, emits nothing. */
  def project(selection: SelectionState): Unit =
    onFxThread("project")
    if !disposed then commit(current.project(selection))

  /** Puts the cursor on `ref` without selecting it, if the table lists it. */
  def moveCursor(ref: Option[StudioRef]): Unit =
    onFxThread("moveCursor")
    shown.foreach(s => commit(current.moveCursor(ref, s)))

  /** Carries the plot's focused mark, whose rows are `refs`, to the cursor. */
  def carry(refs: Vector[StudioRef]): Unit =
    onFxThread("carry")
    shown.foreach(s => commit(current.carry(refs, s)))

  def isDisposed: Boolean = disposed

  /** Removes the table's handlers and rows. Idempotent. */
  def dispose(): Unit =
    onFxThread("dispose")
    if !disposed then
      disposed = true
      removeEventFilter(KeyEvent.KEY_PRESSED, keyHandler)
      focusedProperty.removeListener(focusListener)
      shown = None
      head.getChildren.clear()
      list.setItems(FXCollections.observableArrayList())

  // --- Rendered rows (for accessibility and tests) -----------------------------------

  private def built: Vector[RowCell] =
    list.lookupAll(".table-twin-row").asScala.toVector.collect {
      case c: RowCell if (c ne privateRow) && !c.isEmpty && c.vm.isDefined => c
    }

  private def builtRow(index: Int): Option[RowCell] = built.find(_.rowIndex == index)

  /** How many row nodes the list has built: far fewer than a long source's rows. */
  private[desktop] def builtRows: Int = built.size

  /** The first row in view, if the list has built any: what scrolling moves. */
  private[desktop] def topRow: Option[Int] =
    list.layout()
    built.map(_.rowIndex).minOption

  /** Whether row `index` is in view (its node is built) without scrolling. */
  private[desktop] def inView(index: Int): Boolean =
    list.layout()
    builtRow(index).isDefined

  /** The built row node of row `index`, scrolled into view if it is not:
    * the node shows that row only until the list next reuses it. For tests
    * that click a row.
    */
  private[desktop] def rowNode(index: Int): Option[ListCell[Integer]] =
    scrolledTo(index)

  /** Every row as its drawn node shows it, each row scrolled into view and
    * its cells read, the list then scrolled back. Fails for a table that is
    * not on screen, which draws no rows: use [[modelRowTexts]] there. For
    * tests of small tables.
    */
  private[desktop] def rowTexts: Vector[Vector[String]] =
    drawnRows(_.cellTexts)

  /** Whether each drawn row shows as selected, as [[rowTexts]] reads rows. */
  private[desktop] def rowSelected: Vector[Boolean] =
    drawnRows(_.getPseudoClassStates.contains(Selected))

  /** Every row's cells as the table's state writes them
    * ([[TableTwinState.rowVM]]), drawn or not: for a table off screen.
    */
  private[desktop] def modelRowTexts: Vector[Vector[String]] =
    modelRows.map(_.cells.map(_.text))

  /** Whether each row is selected in the table's state, drawn or not. */
  private[desktop] def modelRowSelected: Vector[Boolean] = modelRows.map(_.selected)

  private def modelRows: Vector[TableRowVM] =
    shown.fold(Vector.empty)(s => Vector.range(0, rowCount).flatMap(current.rowVM(s, _)))

  /** Whether the list lays out rows: in a scene, styled, and given room. */
  private[desktop] def onScreen: Boolean =
    getScene != null && list.getSkin != null && list.getHeight > 0.0

  private def drawnRows[A](read: RowCell => A): Vector[A] =
    if !onScreen then
      throw IllegalStateException(
        "TableTwinView draws no rows off screen; read modelRowTexts or modelRowSelected"
      )
    val top = built.map(_.rowIndex).minOption
    try Vector.range(0, rowCount).flatMap(i => scrolledTo(i).map(read))
    finally
      top.foreach(list.scrollTo)
      list.layout()

  extension (c: RowCell)
    private def cellTexts: Vector[String] =
      Vector.range(0, shares.size).flatMap(k => c.cellAt(k).map(_.getText))

  // Row `index`'s built node, scrolled into view if need be (tests and
  // the SHOW_ITEM / SCROLL_TO_INDEX actions only).
  private def scrolledTo(index: Int): Option[RowCell] =
    if index < 0 || index >= rowCount then None
    else
      list.layout()
      builtRow(index).orElse {
        list.scrollTo(index)
        list.layout()
        builtRow(index)
      }

  // Row `index` for a reader, without scrolling: its built node when in
  // view, else the private row bound to it (review F1).
  private def answering(index: Int): Option[RowCell] =
    if index < 0 || index >= rowCount then None
    else
      builtRow(index).orElse {
        privateRow.bind(Some(index))
        Option.when(privateRow.rowIndex == index)(privateRow)
      }

  private def cellAt(row: Int, column: Int): Option[TwinCell] =
    answering(row).flatMap(_.cellAt(column))

  // The node a reader holds as the focus item: the cursor row's first cell.
  private def focusItem: Option[TwinCell] =
    shown.flatMap(s => current.cursor.flatMap(current.shownIndex(s, _))).flatMap(cellAt(_, 0))

  override def queryAccessibleAttribute(
      attribute: AccessibleAttribute,
      parameters: AnyRef*
  ): AnyRef =
    def int(k: Int): Option[Int] = parameters.lift(k).collect { case i: Integer => i.intValue }
    attribute match
      case AccessibleAttribute.ROW_COUNT          => Integer.valueOf(rowCount)
      case AccessibleAttribute.COLUMN_COUNT       => Integer.valueOf(shares.size)
      case AccessibleAttribute.MULTIPLE_SELECTION => java.lang.Boolean.TRUE
      case AccessibleAttribute.HEADER             => head
      case AccessibleAttribute.FOCUS_ITEM         => focusItem.orNull
      case AccessibleAttribute.SELECTED_ITEMS     =>
        // The selected rows in view: a selection scrolled out of view reads
        // as empty until a row of it is in view again.
        FXCollections.observableArrayList[Node](
          built.filter(_.vm.exists(_.selected)).flatMap(_.cellAt(0)).asJava
        )
      case AccessibleAttribute.CELL_AT_ROW_COLUMN =>
        (for
          r <- int(0)
          c <- int(1)
          n <- cellAt(r, c)
        yield n).orNull
      case AccessibleAttribute.ROW_AT_INDEX    => int(0).flatMap(answering).orNull
      case AccessibleAttribute.COLUMN_AT_INDEX =>
        int(0).flatMap(head.getChildren.asScala.lift).orNull
      case _ => super.queryAccessibleAttribute(attribute, parameters*)

  // A reader scrolls the table only by asking to: SHOW_ITEM on a row or a
  // cell (JavaFX has no scroll-to-index action).
  override def executeAccessibleAction(action: AccessibleAction, parameters: AnyRef*): Unit =
    def show(index: Int): Unit =
      if index >= 0 && index < rowCount then list.scrollTo(index)
    action match
      case AccessibleAction.SHOW_ITEM =>
        parameters.headOption
          .collect {
            case c: TwinCell => c.row
            case r: RowCell  => r.rowIndex
          }
          .foreach(show)
      case _ => super.executeAccessibleAction(action, parameters*)

  // --- Rendering ---------------------------------------------------------------------

  // What was last announced: a render notifies only what changed.
  private var revealed: Option[StudioRef] = None
  private var spoken: Option[String]      = None
  private var chosen: Option[Set[Int]]    = None
  private var focused: Option[Node]       = None
  private var focusedRow: Int             = -1

  private def wholly(row: Int): Boolean =
    list.layout()
    builtRow(row).exists { c =>
      val b    = c.localToScene(c.getLayoutBounds)
      val view = list.localToScene(list.getLayoutBounds)
      b.getMinY >= view.getMinY - 0.5 && b.getMaxY <= view.getMaxY + 0.5
    }

  private var textNotices: Int      = 0
  private var focusNotices: Int     = 0
  private var selectionNotices: Int = 0

  /** How many times the table has announced a new accessible text. */
  private[desktop] def textNotified: Int = textNotices

  /** How many times the table has announced a new focus item. */
  private[desktop] def focusNotified: Int = focusNotices

  /** How many times the table has announced a new set of selected items. */
  private[desktop] def selectionNotified: Int = selectionNotices

  // The cursor row's first cell is the focus item: announce it whenever it
  // is another row or another node (a row scrolled in or out of view).
  private def checkFocus(): Unit =
    val now  = focusItem
    val same = (now, focused) match
      case (None, None)       => true
      case (Some(a), Some(b)) => (a eq b) && focusedRow == a.row
      case _                  => false
    focusedRow = now.fold(-1)(_.row)
    if !same then
      focused = now
      focusNotices += 1
      notifyAccessibleAttributeChanged(AccessibleAttribute.FOCUS_ITEM)

  private var checking: Boolean = false

  private def rowRebound(): Unit =
    if !checking && revealed.isDefined then
      checking = true
      try checkFocus()
      finally checking = false

  private def render(): Unit =
    // A render announces the focus item once, at its end.
    checking = true
    try rerender()
    finally checking = false

  private def rerender(): Unit =
    // The rows in view show the new state in place: their nodes stay.
    list.layout()
    built.foreach(_.rebind())
    if current.cursor != revealed then
      revealed = current.cursor
      // A row already wholly in view stays put; others scroll to near the top.
      for
        s   <- shown
        row <- current.cursor.flatMap(current.shownIndex(s, _))
        if !wholly(row)
      do
        list.scrollTo(math.max(0, row - 2))
        list.layout()
    checkFocus()
    val selected =
      shown.map(s => current.selection.selected.flatMap(current.shownIndex(s, _)).toSet)
    if selected != chosen then
      chosen = selected
      selectionNotices += 1
      notifyAccessibleAttributeChanged(AccessibleAttribute.SELECTED_ITEMS)
    val text = shown.map(current.spoken)
    if text != spoken then
      spoken = text
      textNotices += 1
      setAccessibleText(text.orNull)
      notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT)
    // A pending or cleared table has no served name or content. It becomes
    // a focus stop after its source supplies an accessible caption.
    setFocusTraversable(text.nonEmpty)

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

  /** A row's height, as the stylesheet's `.table-twin-row` fixes it. */
  val RowHeight: Double = 24.0

  /** A row pinned at the top because it is selected. */
  val Pinned: PseudoClass = PseudoClass.getPseudoClass("pinned")

  /** A table as `view`, showing `selection`; its selection intents go to
    * `dispatch`. `pinsSelected` shows its selected rows once, pinned at the
    * top ([[TableTwinState]]). On the FX application thread.
    */
  def attach(
      view: ViewId,
      selection: SelectionState,
      dispatch: Intent => Unit,
      pinsSelected: Boolean = false
  ): TableTwinView =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        "TableTwinView.attach must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )
    TableTwinView(TableTwinState.initial(view, selection, pinsSelected), dispatch)

  /** A header or a row: its children laid side by side, each across its
    * column's share of the width, so the header and every row align.
    */
  private[plot] final class ColumnRow(shares: () => Vector[Double]) extends Pane:
    override protected def layoutChildren(): Unit =
      val in    = getInsets
      val width = (getWidth - in.getLeft - in.getRight).max(0.0)
      val h     = (getHeight - in.getTop - in.getBottom).max(0.0)
      val kids  = getManagedChildren[Node].asScala.toVector
      val fs    = shares()
      val each  = if kids.isEmpty then 0.0 else 1.0 / kids.size
      kids.zipWithIndex.foldLeft(in.getLeft) { case (x, (k, i)) =>
        val w = width * fs.lift(i).getOrElse(each)
        k.resizeRelocate(snapPositionX(x), in.getTop, snapSizeX(w), h)
        x + w
      }: Unit

    override protected def computePrefWidth(height: Double): Double =
      val in = getInsets
      in.getLeft + in.getRight

    override protected def computeMinWidth(height: Double): Double = computePrefWidth(height)

    override protected def computePrefHeight(width: Double): Double =
      val in = getInsets
      getManagedChildren[Node].asScala.map(_.prefHeight(-1)).maxOption.getOrElse(0.0) +
        in.getTop + in.getBottom

  /** A cell of column `column`: a label that knows its row and selection,
    * updated in place as its row cell is reused.
    */
  private[plot] final class TwinCell(val column: Int) extends Label:
    private var at: Int = -1
    private var chosen  = false
    private var numeric = false
    // The first column names the row (sans); values are mono.
    getStyleClass.addAll(
      ("table-twin-cell" +: (if column == 0 then Vector("t12") else Vector("mono", "t12")))*
    )
    setMinWidth(0.0)
    setAccessibleRole(AccessibleRole.TABLE_CELL)

    def row: Int          = at
    def selected: Boolean = chosen

    private val note = Label()
    note.getStyleClass.addAll("t11", "table-twin-note")
    setContentDisplay(ContentDisplay.RIGHT)
    setGraphicTextGap(6.0)

    /** The annotation drawn after the text, if any ("pinned · selected"). */
    def annotation: Option[String] = Option(getGraphic).map(_ => note.getText)

    /** Shows `text` of row `row`, selected or not, with `annotation`. */
    def show(
        text: String,
        isNumeric: Boolean,
        row: Int,
        isSelected: Boolean,
        annotation: Option[String] = None
    ): Unit =
      if getText != text then setText(text)
      annotation match
        case Some(a) =>
          if note.getText != a then note.setText(a)
          if getGraphic ne note then setGraphic(note)
        case None => if getGraphic != null then setGraphic(null)
      if isNumeric != numeric then
        numeric = isNumeric
        if isNumeric then getStyleClass.add("numeric"): Unit
        else getStyleClass.remove("numeric"): Unit
      if row != at then
        at = row
        notifyAccessibleAttributeChanged(AccessibleAttribute.ROW_INDEX)
      if isSelected != chosen then
        chosen = isSelected
        notifyAccessibleAttributeChanged(AccessibleAttribute.SELECTED)

    override def queryAccessibleAttribute(
        attribute: AccessibleAttribute,
        parameters: AnyRef*
    ): AnyRef =
      attribute match
        case AccessibleAttribute.ROW_INDEX    => Integer.valueOf(at)
        case AccessibleAttribute.COLUMN_INDEX => Integer.valueOf(column)
        case AccessibleAttribute.SELECTED     => java.lang.Boolean.valueOf(chosen)
        // Its own text and annotation, styled or not (a private row's cell
        // is never shown).
        case AccessibleAttribute.TEXT => annotation.fold(getText)(a => s"$getText, $a")
        case _                        => super.queryAccessibleAttribute(attribute, parameters*)

  /** A column header: a label that knows its column. */
  private[plot] final class HeaderCell(text: String, numeric: Boolean, val column: Int)
      extends Label(text):
    getStyleClass.add("t11")
    if numeric then getStyleClass.add("numeric"): Unit
    setMinWidth(0.0)
    setAccessibleRole(AccessibleRole.TABLE_COLUMN)

    override def queryAccessibleAttribute(
        attribute: AccessibleAttribute,
        parameters: AnyRef*
    ): AnyRef =
      attribute match
        case AccessibleAttribute.INDEX => Integer.valueOf(column)
        case _                         => super.queryAccessibleAttribute(attribute, parameters*)

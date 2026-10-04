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

import eyes4s.studio.app.analysis.*
import eyes4s.studio.core.backend.TrialKey
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.css.PseudoClass
import javafx.geometry.Pos
import javafx.scene.AccessibleRole
import javafx.scene.control.{Label, ScrollPane, ToggleButton}
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.scene.layout.*

/** The resolved-design table's JavaFX view (ticket S7.5; Analysis.dc.html,
  * resolved design). It binds a [[ResolvedDesignVM]] to nodes and dispatches
  * [[DesignIntent]]s; every word, count and enablement comes from the
  * view-model. The table is one focus stop: the arrow, Page and Home/End keys
  * move its row cursor, and Enter or a click opens the row's trial.
  */
final class ResolvedDesignView(dispatch: DesignIntent => Unit):
  import ResolvedDesignView.*

  // Programmatic updates of controls must not echo back as intents.
  private var rendering                        = false
  private def fire(intent: DesignIntent): Unit = if !rendering then dispatch(intent)

  // --- the chip bar ----------------------------------------------------------------
  /** A filter chip: its label is its text; its count is a separate label. */
  final class Chip(val filter: DesignFilter):
    val count: Label         = label("design-chip-count", "mono", "t11")
    val button: ToggleButton = ToggleButton()
    button.setMnemonicParsing(false)
    button.getStyleClass.addAll("design-chip", "t11")
    button.setGraphic(count)
    button.setContentDisplay(javafx.scene.control.ContentDisplay.RIGHT)
    button.setOnAction(_ => fire(DesignIntent.ChooseFilter(filter)))

  val chips: Vector[Chip] = DesignFilter.values.toVector.map(Chip(_))

  val counting: Label = label("design-counting", "t11")
  val mode: Label     = label("t11")
  private val modeDot = Region()
  modeDot.getStyleClass.add("design-mode-dot")
  private val modeBox = HBox(modeDot, mode)
  modeBox.getStyleClass.add("design-mode")
  private val bar = HBox()
  bar.getChildren.addAll(chips.map(_.button)*)
  bar.getChildren.addAll(spacer(), counting, modeBox)
  bar.getStyleClass.add("design-bar")

  // --- the table ----------------------------------------------------------------------
  private val head = HBox()
  head.getStyleClass.addAll("design-head", "t11")

  private val rowBox = VBox()
  private val scroll = ScrollPane(rowBox)
  scroll.setFitToWidth(true)
  scroll.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER)
  scroll.setFocusTraversable(false)
  scroll.getStyleClass.add("edge-to-edge")

  /** The table: one focus stop that holds the row cursor. */
  val table: StackPane = StackPane(scroll)
  table.getStyleClass.add("design-table")
  table.setFocusTraversable(true)
  table.setAccessibleRole(AccessibleRole.TABLE_VIEW)
  VBox.setVgrow(table, Priority.ALWAYS)
  table.addEventHandler(
    KeyEvent.KEY_PRESSED,
    (e: KeyEvent) =>
      keyIntent(e.getCode).foreach { i =>
        fire(i)
        e.consume()
      }
  )

  val rowsNote: Label = label("design-problem", "t12")
  val problem: Label  = label("design-problem", "t12")
  problem.setWrapText(true)

  // --- notes under the table -------------------------------------------------------------
  private val notes = VBox()
  notes.getStyleClass.add("design-notes")

  val empty: Label = label("design-problem", "t12")

  val node: VBox = VBox(bar, problem, head, table, rowsNote, notes)
  node.getStyleClass.add("design-pane")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  private var shownRows: Vector[DesignRowVM] = Vector.empty
  private var rowNodes: Vector[HBox]         = Vector.empty

  /** The row nodes now drawn, in order. */
  def rows: Vector[HBox] = rowNodes

  /** The note labels now drawn, in order. */
  def noteLabels: Vector[Label] = notes.getChildren.toArray.toVector.collect { case l: Label =>
    l
  }

  /** Bind `vm`. Rows are rebuilt only when the shown rows change; a cursor
    * move only moves the cursor's mark.
    */
  def render(vm: ResolvedDesignVM): Unit =
    rendering = true
    try
      chips.zip(vm.chips).foreach { (chip, c) =>
        chip.button.setText(c.label)
        chip.count.setText(c.count)
        chip.button.setAccessibleText(c.label)
        chip.button.setAccessibleHelp(c.count)
        chip.button.setDisable(!c.enabled)
        chip.button.setSelected(c.on)
      }
      shown(counting, vm.counting)
      mode.setText(vm.mode)
      modeBox.pseudoClassStateChanged(Exact, vm.exact)
      if head.getChildren.isEmpty then
        head.getChildren.addAll(vm.columns.zipWithIndex.map((c, i) => cell(c, i))*): Unit
      shown(problem, vm.problem.orElse(vm.empty))
      table.setAccessibleText(vm.accessible)
      table.setAccessibleHelp(vm.accessibleHelp)
      val unfocused = vm.rows.map(_.copy(focused = false))
      if unfocused != shownRows then
        shownRows = unfocused
        rowNodes = vm.rows.map(rowNode)
        rowBox.getChildren.setAll(rowNodes*): Unit
      rowNodes.zip(vm.rows).foreach((n, r) => n.pseudoClassStateChanged(Cursor, r.focused))
      vm.cursor.foreach(reveal)
      shown(rowsNote, vm.rowsNote)
      notes.getChildren.setAll(vm.notes.map { n =>
        val classes = if n.mono then Vector("design-stamp", "mono", "t11") else Vector("t11")
        val l       = label(classes*)
        l.setText(n.text)
        l.setWrapText(true)
        l
      }*): Unit
    finally rendering = false

  private def rowNode(r: DesignRowVM): HBox =
    def none(text: Option[String], style: String, index: Int) =
      text.fold(cell("—", index, "design-none"))(t => cell(t, index, style))
    val status = cell(
      r.status,
      5,
      if r.tone == StatusTone.Warning then "design-warn" else "design-status"
    )
    val n = HBox(
      cell(r.participant, 0),
      cell(r.trial, 1, "mono"),
      cell(r.item, 2),
      none(r.matched, "design-matched", 3),
      none(r.controls, "design-controls", 4),
      status
    )
    n.getStyleClass.addAll("design-row", "t12")
    n.setAccessibleText(r.accessible)
    n.addEventHandler(
      MouseEvent.MOUSE_CLICKED,
      (_: MouseEvent) =>
        table.requestFocus()
        fire(DesignIntent.OpenRow(r.query))
    )
    n

  /** Keep the cursor's row inside the viewport. */
  private def reveal(index: Int): Unit =
    if rowNodes.nonEmpty then
      val content  = rowBox.getHeight
      val viewport = scroll.getViewportBounds.getHeight
      if content > viewport && viewport > 0 then
        val node        = rowNodes(index.min(rowNodes.size - 1))
        val top         = node.getBoundsInParent.getMinY
        val bottom      = node.getBoundsInParent.getMaxY
        val shownTop    = scroll.getVvalue * (content - viewport)
        val shownBottom = shownTop + viewport
        if top < shownTop then scroll.setVvalue(top / (content - viewport))
        else if bottom > shownBottom then
          scroll.setVvalue((bottom - viewport) / (content - viewport))

  /** The query of the row a click on `node` opens, if it is a row. */
  def queryOf(node: HBox): Option[TrialKey] =
    rowNodes.indexWhere(_ eq node) match
      case -1 => None
      case i  => shownRows.lift(i).map(_.query)

object ResolvedDesignView:

  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-design.css"

  val Exact: PseudoClass  = PseudoClass.getPseudoClass("exact")
  val Cursor: PseudoClass = PseudoClass.getPseudoClass("cursor")

  /** Column widths in logical pixels; the item and status columns grow. */
  val Widths: Vector[Double] = Vector(36, 64, 110, 64, 56, 220)

  /** The row-cursor key a press of `code` is, if any. */
  def keyIntent(code: KeyCode): Option[DesignIntent] = code match
    case KeyCode.UP        => Some(DesignIntent.Move(RowMove.Up))
    case KeyCode.DOWN      => Some(DesignIntent.Move(RowMove.Down))
    case KeyCode.PAGE_UP   => Some(DesignIntent.Move(RowMove.PageUp))
    case KeyCode.PAGE_DOWN => Some(DesignIntent.Move(RowMove.PageDown))
    case KeyCode.HOME      => Some(DesignIntent.Move(RowMove.First))
    case KeyCode.END       => Some(DesignIntent.Move(RowMove.Last))
    case KeyCode.ENTER     => Some(DesignIntent.OpenFocused)
    case _                 => None

  def label(classes: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(classes*)
    l

  private def cell(text: String, column: Int, classes: String*): Label =
    val l = label(classes*)
    l.setText(text)
    val w = Widths(column)
    l.setMinWidth(w)
    l.setPrefWidth(w)
    l.setMaxWidth(if column == 2 || column == 5 then Double.MaxValue else w)
    l.setAlignment(if column == 4 then Pos.CENTER_RIGHT else Pos.CENTER_LEFT)
    if column == 2 || column == 5 then HBox.setHgrow(l, Priority.ALWAYS)
    l

  private def shown(l: Label, text: Option[String]): Unit =
    l.setText(text.getOrElse(""))
    l.setVisible(text.isDefined)
    l.setManaged(text.isDefined)

  def spacer(): Region =
    val r = Region()
    HBox.setHgrow(r, Priority.ALWAYS)
    r

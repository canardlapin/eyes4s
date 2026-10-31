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
import eyes4s.studio.desktop.shell.CompositeFocus
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.css.PseudoClass
import javafx.geometry.Pos
import javafx.scene.{AccessibleAttribute, AccessibleRole}
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
  val table: StackPane = new StackPane(scroll):
    override def queryAccessibleAttribute(
        attribute: AccessibleAttribute,
        parameters: AnyRef*
    ): AnyRef =
      attribute match
        case AccessibleAttribute.ROW_COUNT  => Integer.valueOf(shownRows.size)
        case AccessibleAttribute.FOCUS_ITEM => cursorNode.orNull
        case _ => super.queryAccessibleAttribute(attribute, parameters*)
  table.getStyleClass.add("design-table")
  table.setFocusTraversable(true)
  table.setAccessibleRole(AccessibleRole.TABLE_VIEW)
  VBox.setVgrow(table, Priority.ALWAYS)
  // A focused composite always shows its cursor item (DESIGN_SPEC section 15):
  // taking focus with no cursor puts it on the first row.
  table.focusedProperty.addListener((_, _, now) =>
    if now && !hasCursor then
      shownRows.headOption.foreach(r => fire(DesignIntent.FocusRow(r.query)))
  )
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

  // --- the funnel (overhaul v4, analysis.html) -------------------------------------------
  private val funnelBar  = FunnelBar()
  private val funnelKeys = HBox(18.0)
  funnelKeys.getStyleClass.addAll("design-funnel-keys", "t11")
  private val exceptionsTitle = label("design-funnel-note", "t11")
  private val exceptionsBar   = FunnelBar()
  private val exceptionKeys   = HBox()
  exceptionKeys.getStyleClass.addAll("design-funnel-keys", "t11")
  private val funnel = VBox(
    4.0,
    funnelBar.node,
    funnelKeys,
    exceptionsTitle,
    exceptionsBar.node,
    exceptionKeys
  )
  funnel.getStyleClass.add("design-funnel")

  notes.setVisible(false)
  notes.setManaged(false)
  val node: VBox = VBox(funnel, bar, problem, head, table, rowsNote, notes)
  node.getStyleClass.add("design-pane")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  private var shownRows: Vector[DesignRowVM] = Vector.empty
  private var rowNodes: Vector[HBox]         = Vector.empty
  private var hasCursor: Boolean             = false
  private var rowStart                       = 0
  private var cursorIndex: Option[Int]       = None
  private var shownNotes                     = Vector.empty[DesignNoteVM]
  private val beforeRows                     = Region()
  private val afterRows                      = Region()
  private var realizing                      = false

  scroll.vvalueProperty.addListener((_, _, _) => if !rendering then realizeRows())
  scroll.viewportBoundsProperty.addListener((_, _, _) =>
    if !rendering then
      cursorIndex.foreach(reveal)
      realizeRows()
  )
  CompositeFocus.registerCursor(table)(cursorNode)

  private def cursorNode = rowNodes.lift(cursorIndex.getOrElse(0) - rowStart)

  /** Every filtered query remains available; only viewport rows have nodes. */
  def queries: Vector[TrialKey] = shownRows.map(_.query)

  private def spacerHeight(node: Region, height: Double): Unit =
    node.setMinHeight(height)
    node.setPrefHeight(height)
    node.setMaxHeight(height)

  private def realizeRows(force: Boolean = false): Unit = if !realizing then
    realizing = true
    try
      val viewport = math.max(RowHeight, scroll.getViewportBounds.getHeight)
      val offset   = scroll.getVvalue * math.max(0.0, shownRows.size * RowHeight - viewport)
      val first    = math.max(0, math.floor(offset / RowHeight).toInt - 1)
      val end = math.min(shownRows.size, math.ceil((offset + viewport) / RowHeight).toInt + 1)
      if force || first != rowStart || end - first != rowNodes.size then
        val previous = if force then Map.empty[Int, HBox]
        else rowNodes.zipWithIndex.map((n, i) => (rowStart + i) -> n).toMap
        rowStart = first
        rowNodes =
          (first until end).toVector.map(i => previous.getOrElse(i, rowNode(shownRows(i))))
        spacerHeight(beforeRows, first * RowHeight)
        spacerHeight(afterRows, (shownRows.size - end) * RowHeight)
        rowBox.getChildren.setAll((Vector(beforeRows) ++ rowNodes :+ afterRows)*): Unit
      rowNodes.zipWithIndex.foreach((n, i) =>
        n.pseudoClassStateChanged(Cursor, cursorIndex.getOrElse(0) == rowStart + i)
      )
    finally realizing = false

  /** The row nodes now drawn, in order. */
  def rows: Vector[HBox] = rowNodes

  /** The note labels now drawn, in order. */
  def noteLabels: Vector[Label] = notes.getChildren.toArray.toVector.collect { case l: Label =>
    l
  }

  /** Bind `vm`. Rows are rebuilt only when the shown rows change; a cursor
    * move only moves the cursor's mark.
    */
  private def renderFunnel(parts: Vector[FunnelVM]): Unit =
    funnel.setVisible(parts.nonEmpty)
    funnel.setManaged(parts.nonEmpty)
    def key(p: FunnelVM, sized: Boolean) =
      val swatch = Region()
      swatch.getStyleClass.addAll("design-funnel-swatch", partClass(p.part))
      val count = label("design-funnel-count", "t11")
      count.setText(eyes4s.studio.app.text.Format.count(p.count))
      val words = label("design-funnel-label", "t11")
      words.setText(p.label)
      val h = if sized then HBox(4.0, count, words) else HBox(6.0, swatch, count, words)
      h.setAlignment(Pos.CENTER_LEFT)
      h
    funnelBar.show(parts.map(p => (p.count.toDouble, partClass(p.part))))
    funnelKeys.getChildren.setAll(parts.map(key(_, sized = false))*)
    val exceptions = parts.filterNot(_.part == FunnelPart.Contributing)
    val total      = exceptions.map(_.count).sum
    exceptionsTitle.setText(
      eyes4s.studio.app.text.DesignText(
        eyes4s.studio.app.text.DesignTextId.FunnelExceptions,
        eyes4s.studio.app.text.Format.count(total)
      )
    )
    exceptionsBar.show(exceptions.map(p => (p.count.toDouble, partClass(p.part))))
    // Each exception's count sits under its own segment.
    exceptionKeys.getChildren.setAll(exceptions.map { p =>
      val h = key(p, sized = true)
      h.prefWidthProperty.bind(
        exceptionKeys.widthProperty.multiply(if total > 0 then p.count.toDouble / total
        else 0.0)
      )
      h.setMinWidth(0)
      h
    }*): Unit

  def render(vm: ResolvedDesignVM): Unit =
    renderFunnel(vm.funnel)
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
      shown(counting, vm.counting.map(_.text))
      mode.setText(vm.mode)
      modeBox.pseudoClassStateChanged(Exact, vm.exact)
      if head.getChildren.isEmpty then
        head.getChildren.addAll(vm.columns.zipWithIndex.map((c, i) => cell(c, i))*): Unit
      shown(problem, vm.problem.orElse(vm.empty))
      table.setAccessibleText(vm.accessible)
      // How eyes4s counted is read with the table; the golden does not show it.
      table.setAccessibleHelp(
        (vm.accessibleHelp +: vm.notes.map(_.text)).mkString(" ")
      )
      val unfocused = vm.rows.map(_.copy(focused = false))
      val changed   = unfocused != shownRows
      shownRows = unfocused
      hasCursor = vm.cursor.isDefined
      cursorIndex = vm.cursor
      if changed && cursorIndex.isEmpty then scroll.setVvalue(0.0)
      cursorIndex.foreach(reveal)
      realizeRows(force = changed)
      // Only a table with no rows has no row to draw the cursor on, so only
      // then does the table draw the focus ring.
      CompositeFocus.noCursor(table, shownRows.isEmpty)
      shown(rowsNote, vm.rowsNote)
      if shownNotes != vm.notes then
        shownNotes = vm.notes
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
      text.fold(
        cell(
          eyes4s.studio.app.text.DesignText(eyes4s.studio.app.text.DesignTextId.NoValue),
          index,
          "design-none"
        )
      )(t => cell(t, index, style))
    val status = cell(
      r.status,
      5,
      if r.tone == StatusTone.Warning then "design-warn" else "design-status"
    )
    status.setTooltip(new javafx.scene.control.Tooltip(r.status))
    val n = HBox(
      cell(r.participant, 0),
      cell(r.trial, 1, "mono"),
      cell(r.item, 2),
      none(r.matched, "design-matched", 3),
      none(r.controls, "design-controls", 4),
      status
    )
    n.getStyleClass.addAll("design-row", "t12")
    n.setMinHeight(RowHeight)
    n.setPrefHeight(RowHeight)
    n.setMaxHeight(RowHeight)
    n.setAccessibleRole(AccessibleRole.TABLE_ROW)
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
    val content  = shownRows.size * RowHeight
    val viewport = scroll.getViewportBounds.getHeight
    if content > viewport && viewport > 0 then
      val top      = index * RowHeight
      val bottom   = top + RowHeight
      val shownTop = scroll.getVvalue * (content - viewport)
      if top < shownTop then scroll.setVvalue(top / (content - viewport))
      else if bottom > shownTop + viewport then
        scroll.setVvalue((bottom - viewport) / (content - viewport))

  /** The query of the row a click on `node` opens, if it is a row. */
  def queryOf(node: HBox): Option[TrialKey] =
    rowNodes.indexWhere(_ eq node) match
      case -1 => None
      case i  => shownRows.lift(rowStart + i).map(_.query)

object ResolvedDesignView:

  /** A funnel part's style class: its colour. */
  def partClass(p: FunnelPart): String = p match
    case FunnelPart.Contributing => "design-funnel-contributing"
    case FunnelPart.Failed       => "design-funnel-failed"
    case FunnelPart.NoMatch      => "design-funnel-no-match"
    case FunnelPart.NotAdmitted  => "design-funnel-not-admitted"

  /** The fixed board row height; also the virtual scroll extent. */
  val RowHeight: Double = 22.0

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

/** A 10 px bar of parts, each as wide as its share, 2 px apart. */
final class FunnelBar:
  private var parts = Vector.empty[Double]
  val node: Pane    = new Pane:
    setMinHeight(10.0); setPrefHeight(10.0); setMaxHeight(10.0)
    override def layoutChildren(): Unit =
      val total = parts.sum
      var x     = 0.0
      getChildren.toArray.toVector.zip(parts).foreach {
        case (r: Region, n) =>
          val w = if total > 0 then n / total * getWidth else 0.0
          r.resizeRelocate(x, 0, math.max(w - 2.0, 0.0), 10.0)
          x += w
        case _ => ()
      }

  /** Draw `next`: each part's count and its style class. */
  def show(next: Vector[(Double, String)]): Unit =
    parts = next.map(_._1)
    node.getChildren.setAll(next.map { (_, style) =>
      val r = Region()
      r.getStyleClass.addAll("design-funnel-part", style)
      r
    }*)
    node.requestLayout()

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

package eyes4s.studio.desktop.explore

import eyes4s.studio.app.explore.*
import eyes4s.studio.desktop.tokens.TokenFiles
import javafx.geometry.Pos
import javafx.scene.control.*
import javafx.scene.input.{KeyCode, MouseButton, MouseEvent}
import javafx.scene.layout.*
import javafx.scene.shape.{Line, Rectangle, Shape}

/** One pane of the trials navigator (ticket S6.1; Explore.dc.html, left):
  * the Trials tree or the Items list. It binds a [[NavigatorPaneVM]] to
  * nodes and dispatches [[NavigatorIntent]]s; every word, glyph, count and
  * openness comes from the view-model. The rows are one list: Enter or a
  * double click activates a row (opens or closes a group, explores a
  * trial), and Right and Left open and close a group.
  */
final class TrialsNavigatorView(
    dispatch: NavigatorIntent => Unit,
    filtered: String => NavigatorIntent
):
  import TrialsNavigatorView.*

  // Programmatic updates of controls must not echo back as intents.
  private var rendering                           = false
  private def fire(intent: NavigatorIntent): Unit = if !rendering then dispatch(intent)

  val filter: TextField = TextField()
  filter.getStyleClass.addAll("nav-filter", "t12")
  filter.textProperty.addListener((_, _, text) => fire(filtered(text)))
  private val bar = HBox(filter)
  bar.getStyleClass.add("nav-bar")
  HBox.setHgrow(filter, Priority.ALWAYS)

  val note: Label = label("nav-note", "t11")
  note.setWrapText(true)
  val retry: Button = Button()
  retry.setMnemonicParsing(false)
  retry.getStyleClass.add("nav-button")
  retry.setOnAction(_ => fire(NavigatorIntent.Retry))
  private val status = VBox(note, retry)
  status.getStyleClass.add("nav-status")

  val rows: ListView[NavigatorRowVM] = ListView()
  rows.getStyleClass.add("nav-rows")
  rows.setCellFactory(_ => RowCell())
  VBox.setVgrow(rows, Priority.ALWAYS)
  rows.setOnKeyPressed(e =>
    current.foreach { r =>
      val act = e.getCode match
        case KeyCode.ENTER => r.activate
        case KeyCode.RIGHT => r.activate.filter(_ => r.open.contains(false))
        case KeyCode.LEFT  => r.activate.filter(_ => r.open.contains(true))
        case _             => None
      act.foreach { a =>
        fire(a)
        e.consume()
      }
    }
  )
  rows.addEventHandler(
    MouseEvent.MOUSE_CLICKED,
    (e: MouseEvent) =>
      if e.getButton == MouseButton.PRIMARY && e.getClickCount == 2 then
        current.flatMap(_.activate).foreach(fire)
  )
  private def current: Option[NavigatorRowVM] = Option(rows.getSelectionModel.getSelectedItem)

  private val legend = FlowPane()
  legend.getStyleClass.add("nav-legend")
  val footer: Label = label("nav-footer", "t11")
  footer.setWrapText(true)
  private val foot = VBox(legend, footer)
  foot.getStyleClass.add("nav-foot")

  val node: VBox = VBox(bar, status, rows, foot)
  node.getStyleClass.add("nav-panel")
  Option(getClass.getClassLoader.getResource(stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** Bind `vm`: the filter, the status, the rows and the legend. */
  def render(vm: NavigatorPaneVM): Unit =
    rendering = true
    try
      filter.setPromptText(vm.filterPrompt)
      filter.setAccessibleText(vm.filterPrompt)
      if filter.getText != vm.filter then filter.setText(vm.filter)
      show(note, vm.note)
      vm.retry.foreach { r =>
        retry.setText(r)
        retry.setAccessibleText(r)
      }
      visible(retry, vm.retry.isDefined)
      visible(status, vm.note.isDefined || vm.retry.isDefined)
      rows.setAccessibleText(vm.list)
      if !rows.getItems.toArray.sameElements(vm.rows) then
        val at = current.flatMap(r => vm.rows.find(v => v.ref.isDefined && v.ref == r.ref))
        rows.getItems.setAll(vm.rows*)
        // The selection follows Explore's trial, else stays on its row.
        vm.rows.indexWhere(_.selected) match
          case -1 => at.foreach(r => rows.getSelectionModel.select(r))
          case i  => rows.getSelectionModel.select(i)
      legend.getChildren.setAll(vm.legend.map { l =>
        val text = label("nav-legend-label", "t11")
        text.setText(l.label)
        val item = HBox(glyph(l.glyph), text)
        item.setAlignment(Pos.CENTER_LEFT)
        item.getStyleClass.add("nav-legend-item")
        item
      }*)
      footer.setText(vm.footer)
    finally rendering = false

object TrialsNavigatorView:

  val stylesheetResource: String = s"${TokenFiles.resourceDirectory}/studio-navigator.css"

  def label(classes: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(classes*)
    l

  private def show(l: Label, text: Option[String]): Unit =
    l.setText(text.getOrElse(""))
    visible(l, text.isDefined)

  private def visible(n: javafx.scene.Node, on: Boolean): Unit =
    n.setVisible(on)
    n.setManaged(on)

  /** The style class of a glyph, for tests and the stylesheet. */
  def glyphClass(g: TrialGlyph): String = g match
    case TrialGlyph.Image          => "glyph-image"
    case TrialGlyph.Blank          => "glyph-blank"
    case TrialGlyph.BlankWithCross => "glyph-blank-cross"
    case TrialGlyph.Cue            => "glyph-cue"
    case TrialGlyph.Unknown        => "glyph-unknown"
    case TrialGlyph.MissingAsset   => "glyph-missing"
    case TrialGlyph.NotAdmitted    => "glyph-not-admitted"

  /** A 12 px glyph: a screen frame, with what it showed drawn in it. */
  def glyph(g: TrialGlyph): Pane =
    def line(x1: Double, y1: Double, x2: Double, y2: Double): Shape =
      val l = Line(x1, y1, x2, y2)
      l.getStyleClass.add("glyph-mark")
      l
    val frame = Rectangle(1.5, 2.5, 9, 7)
    frame.getStyleClass.add("glyph-frame")
    val marks: Vector[Shape] = g match
      case TrialGlyph.Image => Vector(line(2, 9, 5, 6), line(5, 6, 7, 8), line(7, 8, 10, 5.5))
      case TrialGlyph.Blank => Vector.empty
      case TrialGlyph.BlankWithCross => Vector(line(6, 4.5, 6, 7.5), line(4.5, 6, 7.5, 6))
      case TrialGlyph.Cue            => Vector(line(4.5, 6, 7.5, 6))
      case TrialGlyph.Unknown        => Vector(line(6, 4.5, 6, 6.5), line(6, 7.5, 6, 8))
      case TrialGlyph.MissingAsset   => Vector(line(2, 9, 6, 3), line(6, 9, 10, 3))
      case TrialGlyph.NotAdmitted    => Vector.empty
    val p = Pane((frame +: marks)*)
    p.getStyleClass.addAll("nav-glyph", glyphClass(g))
    p.setMinSize(12, 12)
    p.setPrefSize(12, 12)
    p.setMaxSize(12, 12)
    p

  /** One row: indent, disclosure, glyph, label, item and detail. */
  final class RowCell extends ListCell[NavigatorRowVM]:
    private val indent = Region()
    private val arrow  = label("nav-arrow", "t11")
    private val mark   = StackPane()
    mark.getStyleClass.add("nav-mark")
    mark.setMinWidth(14)
    private val name   = label("nav-label", "mono", "t12")
    private val item   = label("nav-item", "t12")
    private val detail = label("nav-detail", "t11")
    HBox.setHgrow(item, Priority.ALWAYS)
    item.setMaxWidth(Double.MaxValue)
    private val box = HBox(indent, arrow, mark, name, item, detail)
    box.setAlignment(Pos.CENTER_LEFT)
    box.getStyleClass.add("nav-row-box")
    private val kinds =
      NavigatorRowKind.values.toVector.map(k => s"nav-${k.toString.toLowerCase}")

    override def updateItem(row: NavigatorRowVM, isEmpty: Boolean): Unit =
      super.updateItem(row, isEmpty)
      getStyleClass.removeAll((kinds ++ Vector("warn", "current"))*)
      if isEmpty || row == null then
        setGraphic(null)
        setText(null)
        setAccessibleText(null)
      else
        getStyleClass.add(s"nav-${row.kind.toString.toLowerCase}")
        if row.warn then getStyleClass.add("warn"): Unit
        if row.selected then getStyleClass.add("current"): Unit
        indent.setMinWidth(row.depth * 14.0)
        arrow.setText(row.open.fold("")(o => if o then "▾" else "▸"))
        mark.getChildren.setAll(row.glyph.map(glyph).toSeq*)
        name.setText(row.label)
        item.setText(row.item)
        detail.setText(row.detail)
        setText(null)
        setGraphic(box)
        setAccessibleText(row.accessible)

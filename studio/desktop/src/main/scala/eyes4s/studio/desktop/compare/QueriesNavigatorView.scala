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

import eyes4s.studio.app.Intent
import eyes4s.studio.app.compare.{
  NavigatorKey,
  NavigatorKind,
  QueriesNavigatorVM,
  QueryEntry,
  QueryGroup
}
import eyes4s.studio.app.text.{SummaryText, SummaryTextId}
import eyes4s.studio.core.selection.StudioRef
import javafx.geometry.Pos
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.control.{Button, Label, ScrollPane, TextField}
import javafx.scene.layout.{GridPane, HBox, Pane, Priority, Region, VBox}

/** The Queries navigator (or, of `kind` Items, the Items navigator) of
  * Compare's query layout on the desktop (ticket S8.1; Main.dc.html, left):
  * a filter, a group per participant (or item) that opens to its queries
  * with their D and zero-centred D bars, and, for queries, the count
  * strip. It only binds [[QueriesNavigatorVM]]: a group header asks to open
  * or close it, a query row sends its open intent, the filter its text.
  * Use on the JavaFX thread.
  */
final class QueriesNavigatorView(
    kind: NavigatorKind,
    toggle: (String, Boolean) => Unit,
    keyed: NavigatorKey => Unit,
    filtered: String => Unit,
    app: Intent => Unit
):
  private val byItem = kind == NavigatorKind.Items
  private val filter = TextField()
  filter.setPromptText(SummaryText(SummaryTextId.Filter))
  filter.setAccessibleText(SummaryText(SummaryTextId.Filter))
  filter.textProperty.addListener((_, _, now) => filtered(now))

  private val empty  = Label()
  private val dHead  = Label()
  private val list   = VBox()
  private val scroll = ScrollPane(list)
  scroll.setFitToWidth(true)
  VBox.setVgrow(scroll, Priority.ALWAYS)
  private val strip = GridPane()
  strip.setHgap(8.0)
  strip.setVgap(3.0)
  strip.getStyleClass.addAll("queries-strip", "t11")
  empty.getStyleClass.addAll("queries-empty", "t12")
  dHead.getStyleClass.add("t11")

  private val head = HBox(6.0, label("Query", "t11"), spacer(), dHead)
  head.getStyleClass.add("queries-head")

  /** The pane's content. */
  val node: VBox = VBox(4.0, filter, empty, head, scroll, strip)
  node.getStyleClass.add(if byItem then "items-navigator" else "queries-navigator")
  Option(
    getClass.getClassLoader.getResource(
      eyes4s.studio.desktop.plot.TableTwinView.stylesheetResource
    )
  )
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** The text of the count strip's lines, as shown: label and count. */
  def stripLines: Vector[(String, String)] =
    import scala.jdk.CollectionConverters.*
    strip.getChildren.asScala.toVector
      .collect { case l: Label => l.getText }
      .grouped(2)
      .collect { case Vector(a, b) => (a, b) }
      .toVector

  /** The rows shown: a group's header, or a query's trial, item and D. */
  def rows: Vector[Vector[String]] =
    import scala.jdk.CollectionConverters.*
    list.getChildren.asScala.toVector.map {
      case b: Button => Vector(b.getText)
      case h: HBox   => h.getChildren.asScala.toVector.collect { case l: Label => l.getText }
      case _         => Vector.empty
    }

  /** Each shown query's D bar, in the order of the query rows: the zero
    * line's x, and the ink's x and width, in the bar's own coordinates.
    */
  def bars: Vector[(Double, Option[(Double, Double)])] =
    import scala.jdk.CollectionConverters.*
    list.getChildren.asScala.toVector.collect {
      case h: HBox if h.getUserData.isInstanceOf[StudioRef] =>
        val frame = h.getChildren.asScala.collectFirst {
          case p: Pane if p.getStyleClass.contains("queries-bar-frame") => p
        }.get
        val parts = frame.getChildren.asScala.toVector.collect { case r: Region => r }
        val zero  = parts.find(_.getStyleClass.contains("queries-zero")).get
        val ink   = parts.find(_.getStyleClass.contains("queries-bar"))
        (zero.getLayoutX, ink.map(i => (i.getLayoutX, i.getWidth)))
    }

  // The pane's own focus stop, the node that hosts this one, takes the row
  // cursor's keys (a navigator is one stop with a row cursor, DESIGN_SPEC
  // section 10): the arrows, Home and End, Enter or Space.
  private val keys: javafx.event.EventHandler[KeyEvent] = e =>
    val k = e.getCode match
      case KeyCode.UP | KeyCode.KP_UP       => Some(NavigatorKey.Up)
      case KeyCode.DOWN | KeyCode.KP_DOWN   => Some(NavigatorKey.Down)
      case KeyCode.HOME                     => Some(NavigatorKey.First)
      case KeyCode.END                      => Some(NavigatorKey.Last)
      case KeyCode.ENTER | KeyCode.SPACE    => Some(NavigatorKey.Activate)
      case KeyCode.RIGHT | KeyCode.KP_RIGHT => Some(NavigatorKey.Expand)
      case KeyCode.LEFT | KeyCode.KP_LEFT   => Some(NavigatorKey.Collapse)
      case _                                => None
    k.foreach { key =>
      keyed(key)
      e.consume()
    }
  node.parentProperty.addListener { (_, was, now) =>
    Option(was).foreach(_.removeEventHandler(KeyEvent.KEY_PRESSED, keys))
    Option(now).foreach(_.addEventHandler(KeyEvent.KEY_PRESSED, keys))
  }

  /** The empty-state text, if shown. */
  def emptyText: Option[String] = Option.when(empty.isVisible)(empty.getText)

  /** Opens or closes the group labelled `label`, as a click on its header does. */
  def press(label: String): Unit =
    import scala.jdk.CollectionConverters.*
    list.getChildren.asScala
      .collectFirst {
        case b: Button if b.getText.startsWith(s"$label  ") => b
      }
      .foreach(_.fire())

  /** Opens the query row of `ref`, as a click does. */
  def open(ref: StudioRef): Unit =
    import scala.jdk.CollectionConverters.*
    list.getChildren.asScala
      .collectFirst {
        case h: HBox if h.getUserData == ref => h
      }
      .foreach(_.getOnMouseClicked.handle(null))

  def render(vm: QueriesNavigatorVM): Unit =
    empty.setText(vm.empty.getOrElse(""))
    empty.setVisible(vm.empty.isDefined)
    empty.setManaged(vm.empty.isDefined)
    Vector(filter, head).foreach { n =>
      n.setVisible(vm.empty.isEmpty)
      n.setManaged(vm.empty.isEmpty)
    }
    dHead.setText(vm.dColumn)
    val groups = vm.groupsOf(kind)
    list.getChildren.setAll(groups.flatMap(groupNodes)*)
    strip.getChildren.clear()
    if !byItem then
      vm.strip.zipWithIndex.foreach { (line, i) =>
        val count = label(line.count, "mono")
        if line.failure then count.getStyleClass.add("queries-failed"): Unit
        strip.add(label(line.label, "t11"), 0, i)
        strip.add(count, 1, i)
      }
    strip.setVisible(!byItem && vm.strip.nonEmpty)
    strip.setManaged(!byItem && vm.strip.nonEmpty)

  private def groupNodes(g: QueryGroup): Vector[javafx.scene.Node] =
    val header = Button(s"${g.label}  ${g.summary}")
    header.getStyleClass.add("queries-group")
    header.setAccessibleText(s"${header.getText}, ${if g.open then "open" else "closed"}")
    header.setMaxWidth(Double.MaxValue)
    header.setFocusTraversable(false)
    if g.cursor then header.getStyleClass.add("queries-cursor"): Unit
    header.setOnAction(_ => toggle(g.key, g.open))
    header +: (if g.open then g.entries.map(entryNode) else Vector.empty)

  private def entryNode(e: QueryEntry): HBox =
    // The board's columns: query 50 px, item the rest, D 44 px, bar 40 px.
    val trial = fixed(label(e.trial, "mono"), 50.0)
    val item  = label(e.item, "t12")
    item.setMaxWidth(Double.MaxValue)
    HBox.setHgrow(item, Priority.ALWAYS)
    val d = fixed(label(e.said, "mono"), 44.0)
    d.setAlignment(Pos.CENTER_RIGHT)
    if !e.scored then d.getStyleClass.add("queries-status"): Unit
    val row = HBox(6.0, trial, item, d, bar(e))
    row.setAlignment(Pos.CENTER_LEFT)
    row.getStyleClass.add("queries-row")
    if e.selected then row.getStyleClass.add("queries-selected"): Unit
    row.setUserData(e.ref)
    if e.cursor then row.getStyleClass.add("queries-cursor"): Unit
    row.setAccessibleText(s"${e.trial}, ${e.item}, D ${e.said}")
    row.setOnMouseClicked(_ => app(e.open))
    row

  // A 40 px bar: a zero line at its centre and an ink bar to D.
  private def bar(e: QueryEntry): Pane =
    val half = 18.0
    val zero = Region()
    zero.getStyleClass.add("queries-zero")
    sized(zero, 1.0, 10.0)
    zero.relocate(half, 0.0)
    val pane = Pane(zero)
    pane.setMinSize(2 * half + 1.0, 10.0)
    pane.setPrefSize(2 * half + 1.0, 10.0)
    pane.setMaxSize(2 * half + 1.0, 10.0)
    pane.getStyleClass.add("queries-bar-frame")
    e.bar.foreach { b =>
      val ink   = Region()
      val width = math.abs(b.fromZeroTo) * half
      ink.getStyleClass.add("queries-bar")
      sized(ink, width, 6.0)
      ink.relocate(if b.fromZeroTo >= 0.0 then half + 1.0 else half - width, 2.0)
      pane.getChildren.add(ink)
    }
    pane

  // A Pane keeps its children's positions but sizes them to their preferred size.
  private def sized(r: Region, w: Double, h: Double): Unit =
    r.setMinSize(w, h)
    r.setPrefSize(w, h)
    r.setMaxSize(w, h)

  private def fixed(l: Label, w: Double): Label =
    l.setMinWidth(w)
    l.setPrefWidth(w)
    l

  private def label(text: String, style: String): Label =
    val l = Label(text)
    l.getStyleClass.add(style)
    l

  private def spacer(): Region =
    val r = Region()
    HBox.setHgrow(r, Priority.ALWAYS)
    r

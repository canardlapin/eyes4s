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

import eyes4s.studio.app.compare.{NavigatorKey, ParticipantNavRow, ParticipantsNavigatorVM}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.shell.CompositeFocus
import javafx.beans.value.ChangeListener
import javafx.event.EventHandler
import javafx.scene.{AccessibleAttribute, AccessibleRole, Parent}
import javafx.scene.control.{Label, ListCell, ListView}
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.scene.layout.{HBox, Priority, VBox}
import scala.jdk.CollectionConverters.*

/** One composite pane: the portable cursor and exact activation intent are bound here. */
final class ParticipantsNavigatorView(
    keyed: NavigatorKey => Unit,
    picked: ParticipantNavRow => Unit
):
  private val heading    = Label()
  private val accounting = Label()
  private val status     = Label()
  private val list       = ListView[ParticipantNavRow]()
  // One 26 px row per participant: its id, its Δ on the shared strip and
  // its tag (overhaul v4, results.html).
  list.setFixedCellSize(ParticipantsNavigatorView.RowPx)
  list.getStyleClass.add("participants-list")
  list.setFocusTraversable(false)
  list.setAccessibleRole(AccessibleRole.PARENT)
  // The portable cursor and selection own interaction, as in TableTwinView.
  list.addEventFilter(MouseEvent.MOUSE_PRESSED, (e: MouseEvent) => e.consume())
  list.setCellFactory(_ =>
    new ListCell[ParticipantNavRow]:
      private var shownRow: Option[ParticipantNavRow]                        = None
      override def updateItem(item: ParticipantNavRow, empty: Boolean): Unit =
        super.updateItem(item, empty)
        val next = Option(item).filter(_ => !empty)
        if next != shownRow then
          shownRow = next
          setText(null)
          setGraphic(next.map(row).orNull)
          setAccessibleText(next.map(_.spoken).orNull)
      override protected def layoutChildren(): Unit =
        super.layoutChildren()
        Option(getGraphic).collect { case box: HBox =>
          box.resizeRelocate(0, 0, getWidth, getHeight)
        }: Unit
  )
  VBox.setVgrow(list, Priority.ALWAYS)
  Vector(accounting, status).foreach { l => l.getStyleClass.add("t11"); l.setWrapText(true) }
  heading.getStyleClass.add("t12-strong")
  // The strip's axis over the rows: zero and the top of the shared scale.
  private val axisLow  = Label()
  private val axisHigh = Label()
  Vector(axisLow, axisHigh).foreach(_.getStyleClass.addAll("participants-axis-label", "t11"))
  private val axisStrip = javafx.scene.layout.Pane(axisLow, axisHigh)
  axisStrip.setMinSize(ParticipantsNavigatorView.StripPx, 14.0)
  axisStrip.setPrefSize(ParticipantsNavigatorView.StripPx, 14.0)
  axisStrip.setMaxSize(ParticipantsNavigatorView.StripPx, 14.0)
  private val axisGap = javafx.scene.layout.Region()
  axisGap.setMinWidth(ParticipantsNavigatorView.NamePx)
  private val axis = HBox(8.0, axisGap, axisStrip)
  axis.getStyleClass.add("participants-axis")
  // The pane's header names the list; these stay for a reader of the pane.
  Vector(heading, accounting).foreach { l => l.setVisible(false); l.setManaged(false) }
  val node: VBox = VBox(heading, accounting, status, axis, list)
  node.getStyleClass.add("participants-navigator")
  CompositeFocus.paneContent(node)
  CompositeFocus.registerCursor(node)(Option(node.lookup(".participants-cursor")))
  Option(getClass.getClassLoader.getResource("eyes4s/studio/desktop/studio-participants.css"))
    .foreach(u => node.getStylesheets.add(u.toExternalForm))
  private var shown: ParticipantsNavigatorVM =
    ParticipantsNavigatorVM("", "", None, Vector.empty)
  private var stopName: Option[String]     = None
  private val keys: EventHandler[KeyEvent] = e =>
    val key = e.getCode match
      case KeyCode.UP | KeyCode.KP_UP     => Some(NavigatorKey.Up)
      case KeyCode.DOWN | KeyCode.KP_DOWN => Some(NavigatorKey.Down)
      case KeyCode.HOME                   => Some(NavigatorKey.First)
      case KeyCode.END                    => Some(NavigatorKey.Last)
      case KeyCode.ENTER | KeyCode.SPACE  => Some(NavigatorKey.Activate)
      case _                              => None
    key.foreach { k => keyed(k); e.consume() }
  private val parented: ChangeListener[Parent] = (_, was, now) =>
    Option(was).foreach { p =>
      p.removeEventHandler(KeyEvent.KEY_PRESSED, keys); p.setAccessibleText(stopName.orNull)
    }
    stopName = Option(now).flatMap(p => Option(p.getAccessibleText))
    Option(now).foreach(_.addEventHandler(KeyEvent.KEY_PRESSED, keys))
    announce()
  node.parentProperty.addListener(parented)

  def rows: Vector[ParticipantNavRow] = shown.rows

  /** The pane's subtitle: what the strip shows ("Δ D at σ 2°"). */
  def subtitle: String           = shown.subtitle
  def emptyText: Option[String]  = shown.status
  def open(ref: StudioRef): Unit = shown.rows.find(_.ref == ref).foreach(picked)
  def stopText: Option[String]   =
    Option(node.getParent).flatMap(p => Option(p.getAccessibleText))

  def render(vm: ParticipantsNavigatorVM): Unit =
    val oldCursor   = shown.rows.find(_.cursor).map(_.ref)
    val rowsChanged = shown.rows != vm.rows
    shown = vm
    heading.setText(vm.heading)
    accounting.setText(vm.accounting)
    status.setText(vm.status.getOrElse(""))
    status.setVisible(vm.status.isDefined)
    status.setManaged(vm.status.isDefined)
    val hasRows = vm.rows.nonEmpty
    axis.setVisible(hasRows); axis.setManaged(hasRows)
    def tick(v: Double) =
      if math.abs(v) < 1e-9 then "0" else eyes4s.studio.app.text.Format.signed(v, 1)
    axisLow.setText(tick(vm.axis._1))
    axisHigh.setText(tick(vm.axis._2))
    // "0" centred on its tick, the top of the scale ending at the strip's end.
    axisLow.layoutXProperty.bind(
      axisLow.widthProperty.divide(-2.0).add(ParticipantsNavigatorView.StripInsetPx)
    )
    axisHigh.layoutXProperty.bind(axisStrip.widthProperty.subtract(axisHigh.widthProperty))
    if rowsChanged then list.getItems.setAll(vm.rows.asJava): Unit
    CompositeFocus.noCursor(node, !vm.rows.exists(_.cursor))
    if oldCursor != vm.rows.find(_.cursor).map(_.ref) && vm.rows.nonEmpty then
      val index = vm.rows.indexWhere(_.cursor).max(0)
      list.scrollTo(index)
    announce()

  private def announce(): Unit = Option(node.getParent).foreach { p =>
    val text = (stopName.toVector ++ shown.cursorText).mkString(", ")
    if Option(p.getAccessibleText) != Some(text) then
      p.setAccessibleText(text)
      p.notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT)
  }

  private def row(r: ParticipantNavRow): HBox =
    import ParticipantsNavigatorView.*
    val name = Label(r.participant)
    name.getStyleClass.addAll("participants-name", "t12")
    name.setMinWidth(NamePx); name.setPrefWidth(NamePx)
    val (lo, hi)     = shown.axis
    def x(v: Double) =
      StripInsetPx + (v.max(lo).min(hi) - lo) / math.max(hi - lo, 1e-9) *
        (StripPx - 2 * StripInsetPx)
    def line(x0: Double, y0: Double, x1: Double, y1: Double, style: String) =
      val l = javafx.scene.shape.Line(x0, y0, x1, y1)
      l.getStyleClass.add(style)
      l
    val marks = Vector(
      line(StripInsetPx, 6.0, StripPx - StripInsetPx, 6.0, "participants-strip-rule"),
      line(x(0.0), 1.0, x(0.0), 11.0, "participants-strip-zero")
    ) ++ shown.mean.map(m => line(x(m), 2.0, x(m), 10.0, "participants-strip-mean")) ++
      r.delta.map { d =>
        val c = javafx.scene.shape.Circle(x(d), 6.0, DotRadiusPx)
        c.getStyleClass.add("participants-strip-dot")
        c
      }
    val strip = javafx.scene.layout.Pane(marks*)
    strip.setMinSize(StripPx, 12.0); strip.setPrefSize(StripPx, 12.0)
    strip.setMaxSize(StripPx, 12.0)
    val tag = Label(r.tag.getOrElse(""))
    tag.getStyleClass.addAll("participants-tag", "t11")
    tag.setVisible(r.tag.isDefined)
    tag.setMinWidth(javafx.scene.layout.Region.USE_PREF_SIZE)
    // The tag sits at the row's end and may overlap the strip's last pixels,
    // as the golden's does; the row never asks for more than the pane.
    val tagBox = HBox(tag)
    tagBox.setAlignment(javafx.geometry.Pos.CENTER_RIGHT)
    tagBox.setMinWidth(0.0)
    HBox.setHgrow(tagBox, Priority.ALWAYS)
    val box = HBox(8.0, name, strip, tagBox)
    box.setMinWidth(0.0)
    box.setPrefWidth(0.0)
    box.setAlignment(javafx.geometry.Pos.CENTER_LEFT)
    box.getStyleClass.add("participants-row")
    if r.selected then box.getStyleClass.add("participants-selected"): Unit
    if r.cursor then box.getStyleClass.add("participants-cursor"): Unit
    box.setUserData(r.ref)
    box.setAccessibleText(r.spoken)
    box.setOnMouseClicked(_ => picked(r))
    box

  def dispose(): Unit =
    node.parentProperty.removeListener(parented)
    Option(node.getParent).foreach { p =>
      p.removeEventHandler(KeyEvent.KEY_PRESSED, keys); p.setAccessibleText(stopName.orNull)
    }

object ParticipantsNavigatorView:
  /** The golden's row: 26 px, a 30 px id, a 120 px strip, a 52 px tag. */
  val RowPx: Double        = 26.0
  val NamePx: Double       = 30.0
  val StripPx: Double      = 120.0
  val StripInsetPx: Double = 4.0
  val DotRadiusPx: Double  = 2.6

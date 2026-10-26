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
import javafx.scene.{AccessibleAttribute, Parent}
import javafx.scene.control.{Label, ScrollPane}
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.layout.{HBox, Priority, VBox}

/** One composite pane: the portable cursor and exact activation intent are bound here. */
final class ParticipantsNavigatorView(
    keyed: NavigatorKey => Unit,
    picked: ParticipantNavRow => Unit
):
  private val heading    = Label()
  private val accounting = Label()
  private val status     = Label()
  private val list       = VBox(2.0)
  private val scroll     = ScrollPane(list)
  scroll.setFitToWidth(true)
  scroll.setFocusTraversable(false)
  VBox.setVgrow(scroll, Priority.ALWAYS)
  Vector(accounting, status).foreach { l => l.getStyleClass.add("t11"); l.setWrapText(true) }
  heading.getStyleClass.add("t12-strong")
  val node: VBox = VBox(4.0, heading, accounting, status, scroll)
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
  def emptyText: Option[String]       = shown.status
  def open(ref: StudioRef): Unit      = shown.rows.find(_.ref == ref).foreach(picked)
  def stopText: Option[String]        =
    Option(node.getParent).flatMap(p => Option(p.getAccessibleText))

  def render(vm: ParticipantsNavigatorVM): Unit =
    val oldCursor = shown.rows.find(_.cursor).map(_.ref)
    shown = vm
    heading.setText(vm.heading)
    accounting.setText(vm.accounting)
    status.setText(vm.status.getOrElse(""))
    Vector(heading, accounting).foreach { n =>
      n.setVisible(vm.status.isEmpty); n.setManaged(vm.status.isEmpty)
    }
    status.setVisible(vm.status.isDefined)
    status.setManaged(vm.status.isDefined)
    list.getChildren.setAll(vm.rows.map(row)*)
    CompositeFocus.noCursor(node, !vm.rows.exists(_.cursor))
    if oldCursor != vm.rows.find(_.cursor).map(_.ref) && vm.rows.nonEmpty then
      val index = vm.rows.indexWhere(_.cursor).max(0)
      scroll.setVvalue(index.toDouble / (vm.rows.size - 1).max(1).toDouble)
    announce()

  private def announce(): Unit = Option(node.getParent).foreach { p =>
    val text = (stopName.toVector ++ shown.cursorText).mkString(", ")
    if Option(p.getAccessibleText) != Some(text) then
      p.setAccessibleText(text)
      p.notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT)
  }

  private def row(r: ParticipantNavRow): VBox =
    val name = Label(r.participant)
    name.getStyleClass.addAll("mono", "t12")
    if r.selected then name.getStyleClass.add("mono-500"): Unit
    val value = Label(
      eyes4s.studio.app.text.ParticipantsNavText(
        eyes4s.studio.app.text.ParticipantsNavTextId.Mean,
        r.value,
        r.queries
      )
    )
    value.getStyleClass.addAll("mono", "t12")
    if r.selected then value.getStyleClass.add("mono-500"): Unit
    HBox.setHgrow(name, Priority.ALWAYS)
    name.setMaxWidth(Double.MaxValue)
    val count = Label(r.accounting)
    count.getStyleClass.addAll("mono", "t11")
    if r.selected then count.getStyleClass.add("mono-500"): Unit
    val box = VBox(2.0, HBox(6.0, name, value), count)
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

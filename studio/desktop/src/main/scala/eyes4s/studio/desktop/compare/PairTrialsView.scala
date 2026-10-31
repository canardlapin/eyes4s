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
import eyes4s.studio.app.compare.{NavigatorKey, PairTrialRow, PairTrialsVM}
import eyes4s.studio.app.explore.ExploreRole
import eyes4s.studio.desktop.shell.CompositeFocus
import javafx.beans.value.ChangeListener
import javafx.event.EventHandler
import javafx.geometry.Pos
import javafx.scene.{AccessibleAttribute, Parent}
import javafx.scene.control.{Hyperlink, Label, ScrollPane}
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.layout.{HBox, Priority, Region, VBox}

/** The Results drill's navigator (overhaul v4, `explore.html`): the query,
  * its matched reference and its controls by cosine, one composite pane
  * whose row cursor arrows move and Enter opens. Every word comes from
  * [[PairTrialsVM]].
  */
final class PairTrialsView(
    keyed: NavigatorKey => Unit,
    picked: PairTrialRow => Unit,
    app: Intent => Unit
):
  private val status = Label()
  status.getStyleClass.addAll("pair-trials-status", "t12")
  status.setWrapText(true)
  private val body = VBox()
  body.getStyleClass.add("pair-trials-body")
  private val scroll = ScrollPane(body)
  scroll.setFitToWidth(true)
  scroll.setFocusTraversable(false)
  scroll.getStyleClass.add("pair-trials-scroll")
  VBox.setVgrow(scroll, Priority.ALWAYS)
  private val all = Hyperlink()
  all.getStyleClass.addAll("pair-trials-all", "t11")
  all.setFocusTraversable(false)
  private val foot = HBox(all)
  foot.getStyleClass.add("pair-trials-foot")
  foot.setAlignment(Pos.CENTER_RIGHT)

  val node: VBox = VBox(status, scroll, foot)
  node.getStyleClass.add("pair-trials")
  CompositeFocus.paneContent(node)
  CompositeFocus.registerCursor(node)(Option(node.lookup(".pair-trials-cursor")))
  Vector("studio-participants.css", "studio-plot.css").foreach(f =>
    Option(getClass.getClassLoader.getResource(s"eyes4s/studio/desktop/$f"))
      .foreach(u => node.getStylesheets.add(u.toExternalForm))
  )

  private var shown: PairTrialsVM          = PairTrialsVM("", None, Vector.empty, None)
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

  /** The rows shown now, and the pane header's count. */
  def vm: PairTrialsVM = shown
  def count: String    = shown.count

  def render(vm: PairTrialsVM): Unit =
    val changed = vm != shown
    shown = vm
    status.setText(vm.status.getOrElse(""))
    status.setVisible(vm.status.isDefined)
    status.setManaged(vm.status.isDefined)
    if changed then
      body.getChildren.setAll(vm.sections.flatMap { s =>
        val h = Label(s.heading)
        h.getStyleClass.addAll("pair-trials-heading", "t12")
        h +: s.rows.map(row)
      }*): Unit
    vm.all match
      case Some((words, go)) =>
        all.setText(words)
        all.setOnAction(_ => app(go))
        foot.setVisible(true); foot.setManaged(true)
      case None => foot.setVisible(false); foot.setManaged(false)
    CompositeFocus.noCursor(node, !vm.rows.exists(_.cursor))
    announce()

  private def announce(): Unit = Option(node.getParent).foreach { p =>
    val text = (stopName.toVector ++ shown.cursorText).mkString(", ")
    if Option(p.getAccessibleText) != Some(text) then
      p.setAccessibleText(text)
      p.notifyAccessibleAttributeChanged(AccessibleAttribute.TEXT)
  }

  private def row(r: PairTrialRow): HBox =
    val glyph = Region()
    glyph.getStyleClass.addAll(
      "pair-glyph",
      r.role match
        case ExploreRole.Query   => "pill-query"
        case ExploreRole.Matched => "pill-matched"
        case ExploreRole.Control => "pill-control"
    )
    val trial = Label(r.trial.trial)
    trial.getStyleClass.addAll("participants-name", "t12")
    trial.setMinWidth(Region.USE_PREF_SIZE)
    val item = Label(r.item)
    item.getStyleClass.addAll("pair-trials-item", "t12")
    item.setMinWidth(0)
    val spacer = Region()
    HBox.setHgrow(spacer, Priority.ALWAYS)
    val value = Label(r.value)
    value.getStyleClass.addAll("pair-trials-value", "t11")
    value.setMinWidth(Region.USE_PREF_SIZE)
    val box = HBox(8.0, glyph, trial, item, spacer, value)
    box.setAlignment(Pos.CENTER_LEFT)
    box.getStyleClass.addAll("participants-row", "pair-trials-row")
    if r.shown then box.getStyleClass.add("participants-selected"): Unit
    if r.cursor then box.getStyleClass.addAll("participants-cursor", "pair-trials-cursor"): Unit
    box.setUserData(r.trial)
    box.setAccessibleText(r.spoken)
    box.setOnMouseClicked(_ => picked(r))
    box

  def dispose(): Unit =
    node.parentProperty.removeListener(parented)
    Option(node.getParent).foreach { p =>
      p.removeEventHandler(KeyEvent.KEY_PRESSED, keys); p.setAccessibleText(stopName.orNull)
    }

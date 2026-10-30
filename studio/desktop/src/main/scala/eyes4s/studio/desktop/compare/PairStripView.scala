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
import eyes4s.studio.app.compare.{PairStripVM, StripTileVM}
import eyes4s.studio.app.figures.TileMap
import eyes4s.studio.app.tokens.StageVariant
import eyes4s.studio.desktop.plot.CanvasPlotHost
import eyes4s.studio.viz.maps.MapTileScene
import javafx.geometry.Pos
import javafx.scene.Node
import javafx.scene.control.Label
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.layout.{HBox, Region, VBox}

/** The pair's control strip on the desktop (overhaul decision 7): its title
  * and note, then two rows of 48 px density maps with their cosines, B's
  * marker where it falls. One focus stop: ← and → move the cursor, Enter
  * opens the pair under it; a click opens one. Use on the JavaFX thread.
  */
final class PairStripView(app: Intent => Unit):

  private val title = Label()
  title.getStyleClass.addAll("strip-title", "t12")
  private val note = Label()
  note.getStyleClass.addAll("strip-note", "t11")
  note.setMinWidth(0)
  private val head = HBox(8.0, title, note)
  head.setAlignment(Pos.BASELINE_LEFT)
  private val rows = VBox(6.0)

  val node: VBox = VBox(6.0, head, rows)
  node.getStyleClass.add("pair-strip")
  node.setFocusTraversable(true)
  node.setAccessibleRole(javafx.scene.AccessibleRole.LIST_VIEW)

  /** The strip's focus stop while it is shown: one list, named by its title. */
  def stop: Option[eyes4s.studio.app.vm.FocusStop] =
    shown
      .filter(_ => node.isVisible)
      .map((vm, _) =>
        eyes4s.studio.app.vm.FocusStop(eyes4s.studio.app.vm.A11yRole.List, vm.title)
      )

  private var shown: Option[(PairStripVM, StageVariant)] = None
  private var cursor                                     = 0
  private var hosts: Map[Int, (CanvasPlotHost, Any)]     = Map.empty

  /** The strip as drawn, for tests. */
  def text: String = shown.fold("")(_._1.text)

  node.addEventHandler(
    KeyEvent.KEY_PRESSED,
    (e: KeyEvent) =>
      shown.foreach { (vm, stage) =>
        e.getCode match
          case KeyCode.RIGHT if cursor < vm.tiles.size - 1 =>
            cursor += 1; draw(vm, stage); e.consume()
          case KeyCode.LEFT if cursor > 0 =>
            cursor -= 1; draw(vm, stage); e.consume()
          case KeyCode.ENTER =>
            vm.tiles.lift(cursor).foreach(t => app(t.open)); e.consume()
          case _ => ()
      }
  )

  def clear(): Unit =
    shown = None
    rows.getChildren.clear()
    node.setVisible(false)
    node.setManaged(false)

  def render(vm: PairStripVM, stage: StageVariant): Unit =
    if !shown.contains((vm, stage)) then
      if !shown.exists(_._1.tiles.map(_.ref) == vm.tiles.map(_.ref)) then
        cursor = vm.tiles.indexWhere(_.current).max(0)
      shown = Some((vm, stage))
      draw(vm, stage)
    node.setVisible(true)
    node.setManaged(true)

  private def draw(vm: PairStripVM, stage: StageVariant): Unit =
    title.setText(vm.title)
    note.setText(vm.note)
    node.setAccessibleText(vm.title)
    node.setAccessibleHelp(vm.text)
    val cells: Vector[Node] = vm.tiles.zipWithIndex.map((t, i) => tile(t, i, stage))
    val all                 = vm.b.fold(cells)((at, value) =>
      cells.patch(at.min(cells.size), Vector(bMarker(value)), 0)
    )
    val (first, second) = all.splitAt(vm.firstRow)
    val r1              = HBox(6.0, first*)
    val r2              = HBox(6.0, second*)
    rows.getChildren.setAll(r1, r2): Unit

  private def tile(t: StripTileVM, i: Int, stage: StageVariant): VBox =
    val map: Node = t.map match
      case TileMap.Drawn(grid, style, region) =>
        val host = hosts.get(i) match
          case Some((h, key)) if key == (grid, style, stage) => h
          case old                                           =>
            val h = old.fold(CanvasPlotHost())(_._1)
            MapTileScene
              .of(s"compare.strip.$i", grid, style, region, stage, glance = true)
              .foreach(h.show)
            hosts = hosts.updated(i, (h, (grid, style, stage)))
            h
        val height = PairStripView.TilePx * region.height / region.width
        host.setMinSize(PairStripView.TilePx, height)
        host.setPrefSize(PairStripView.TilePx, height)
        host.setMaxSize(PairStripView.TilePx, height)
        host
      case _ =>
        val r = Region()
        r.getStyleClass.add("strip-empty")
        r.setMinSize(PairStripView.TilePx, PairStripView.TilePx * 0.75)
        r.setPrefSize(PairStripView.TilePx, PairStripView.TilePx * 0.75)
        r
    val score = Label(if t.matched then s"◆ ${t.score}" else t.score)
    score.getStyleClass.addAll("strip-score", "t11")
    if t.matched then score.getStyleClass.add("strip-matched"): Unit
    val cell = VBox(2.0, map, score)
    cell.getStyleClass.add("strip-tile")
    if i == cursor && node.isFocused then cell.getStyleClass.add("strip-cursor"): Unit
    if t.current then cell.getStyleClass.add("strip-current"): Unit
    cell.setOnMouseClicked { _ =>
      cursor = i
      app(t.open)
    }
    cell

  private def bMarker(value: String): VBox =
    val bar = Region()
    bar.getStyleClass.add("strip-b-bar")
    val l = Label("B")
    l.getStyleClass.addAll("strip-b", "t11")
    val v = VBox(2.0, bar, l)
    v.setAlignment(Pos.TOP_CENTER)
    v.getStyleClass.add("strip-b-cell")
    v.setAccessibleText(s"B, the controls' mean, $value")
    v

  node.focusedProperty.addListener((_, _, _) => shown.foreach((vm, st) => draw(vm, st)))

object PairStripView:
  /** The golden's tile width. */
  val TilePx: Double = 48

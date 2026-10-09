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

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.analysis.{AnalysesNavigator, AnalysesNavigatorVM, AnalysisHistoryRow}
import eyes4s.studio.app.text.{AnalysesText, AnalysesTextId}
import eyes4s.studio.app.vm.FocusStop
import eyes4s.studio.core.backend.{AnalysisRevision, RunId}
import eyes4s.studio.core.document.AnalysisFamilyId
import javafx.application.Platform
import javafx.scene.control.{Button, Label, ScrollPane, ToggleButton}
import javafx.scene.layout.{Priority, VBox}

/** The Analysis navigator binds document history, retaining keyed controls and focus. */
final class AnalysesNavigatorHost(app: Intent => Unit):
  private type Key = (AnalysisRevision, Option[RunId])
  private var disposed                           = false
  private var shown: Option[AnalysesNavigatorVM] = None
  private var controls                           = Map.empty[Key, ToggleButton]
  private var structure                          = Vector.empty[(AnalysisFamilyId, Vector[Key])]
  private var titles                             = Map.empty[AnalysisFamilyId, Label]
  val create: Button = Button(AnalysesText(AnalysesTextId.NewAnalysis))
  create.setAccessibleText(create.getText)
  create.getStyleClass.add("btn")
  create.setOnAction(_ => if !disposed then shown.flatMap(_.create).foreach(app))
  val current = Label()
  current.setWrapText(true)
  current.getStyleClass.add("t11")
  val showCurrent = Button(AnalysesText(AnalysesTextId.ShowCurrent))
  showCurrent.setAccessibleText(showCurrent.getText)
  showCurrent.getStyleClass.add("btn")
  showCurrent.setOnAction(_ =>
    if !disposed then shown.flatMap(_.current).flatMap(_.show).foreach(app)
  )
  private val history    = VBox(6.0)
  val scroll: ScrollPane = ScrollPane(history)
  scroll.setFitToWidth(true)
  scroll.setFocusTraversable(false)
  scroll.setMinHeight(0)
  VBox.setVgrow(scroll, Priority.ALWAYS)
  private val note = Label()
  note.setWrapText(true)
  note.getStyleClass.add("t11")
  val node: VBox = VBox(8.0, create, current, showCurrent, scroll, note)
  node.getStyleClass.add("inspector")

  private def key(row: AnalysisHistoryRow): Key = (row.revision, row.run)
  private def rows: Vector[AnalysisHistoryRow]  =
    shown.toVector.flatMap(_.groups.flatMap(_.rows))
  def labels: Vector[String]         = rows.map(_.accessible)
  def selectedLabels: Vector[String] =
    rows.filter(r => controls.get(key(r)).exists(_.isSelected)).map(_.accessible)
  def button(label: String): Option[ToggleButton] =
    rows.find(_.accessible == label).flatMap(r => controls.get(key(r)))
  def select(label: String): Unit   = button(label).foreach(_.fire())
  def focusStops: Vector[FocusStop] = shown.toVector.flatMap(AnalysesNavigator.focusStops)

  private def reveal(button: ToggleButton): Unit = if !disposed then
    val height   = history.getLayoutBounds.getHeight
    val viewport = scroll.getViewportBounds.getHeight
    if height > viewport && viewport > 0 then
      val bounds = button.getBoundsInParent
      val top    = (height - viewport) * scroll.getVvalue
      val next   = if bounds.getMinY < top then bounds.getMinY
      else if bounds.getMaxY > top + viewport then bounds.getMaxY - viewport
      else top
      scroll.setVvalue(math.max(0.0, math.min(1.0, next / (height - viewport))))

  private def make(): ToggleButton =
    val button = ToggleButton()
    button.getStyleClass.add("btn")
    button.setWrapText(true)
    button.setMaxWidth(Double.MaxValue)
    button.focusedProperty.addListener((_, _, focused) =>
      if focused then Platform.runLater(() => reveal(button))
    )
    button

  def sync(model: AppModel): Unit = if !disposed then
    val view = AnalysesNavigator.vm(model)
    if !shown.contains(view) then
      val focused = controls.collectFirst { case (id, button) if button.isFocused => id }
      shown = Some(view)
      create.setDisable(view.create.isEmpty)
      note.setText(view.note)
      current.setText(view.currentLabel)
      val canShow = view.current.exists(_.show.isDefined)
      showCurrent.setDisable(!canShow)
      showCurrent.setVisible(canShow)
      showCurrent.setManaged(canShow)
      val keys = rows.map(key).toSet
      controls = controls.filter((id, _) => keys(id))
      rows.foreach { row =>
        val id     = key(row)
        val button = controls.getOrElse(id, make())
        controls = controls.updated(id, button)
        button.setText(row.accessible)
        button.setAccessibleText(row.accessible)
        button.setSelected(row.selected)
        button.setOnAction(_ =>
          if !disposed then
            button.setSelected(true)
            app(row.open)
        )
      }
      val next = view.groups.map(group => group.family -> group.rows.map(key))
      if next != structure then
        structure = next
        history.getChildren.clear()
        titles = titles.filter((id, _) => view.groups.exists(_.family == id))
        view.groups.foreach { group =>
          val title = titles.getOrElse(
            group.family, {
              val label = Label()
              label.getStyleClass.add("t13")
              label.setWrapText(true)
              label
            }
          )
          titles = titles.updated(group.family, title)
          history.getChildren.add(title)
          group.rows.foreach(row => history.getChildren.add(controls(key(row))))
        }
        focused.flatMap(controls.get).foreach(_.requestFocus())
      view.groups.foreach { group =>
        titles.get(group.family).foreach { title =>
          title.setText(group.heading)
          title.setAccessibleText(group.accessible)
        }
      }

  def groupLabels: Vector[String] =
    shown.toVector.flatMap(_.groups.flatMap(g => titles.get(g.family).map(_.getText)))

  def dispose(): Unit =
    disposed = true
    controls = Map.empty
    titles = Map.empty
    shown = None

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
import eyes4s.studio.app.analysis.PresetPicker
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.document.Preset
import javafx.scene.control.{Button, Label, RadioButton, ScrollPane}
import javafx.scene.layout.VBox

/** The recipe pane's actual preset controls, including the first analysis
  * on admitted data. The pure picker owns commands and configuration.
  */
final class RecipePaneHost(model: () => AppModel, app: Intent => Unit):
  private var disposed = false
  private var binding  = false
  private val heading  = Label()
  heading.getStyleClass.add("t13")
  heading.setWrapText(true)
  private val note = Label()
  note.getStyleClass.add("t12")
  note.setWrapText(true)
  private val configuration = Label()
  configuration.getStyleClass.add("t11")
  configuration.setWrapText(true)
  // The model binds the single selected preset. Keep each enabled
  // alternative in Tab order even when the selected preset is disabled.
  private val options = PresetPicker.offered.map { (preset, _) =>
    val choose = RadioButton()
    choose.getStyleClass.add("t12")
    choose.setWrapText(true)
    val detail = Label()
    detail.getStyleClass.add("t11")
    detail.setWrapText(true)
    choose.setOnAction(_ =>
      if !binding && !disposed then
        PresetPicker
          .vm(model().document, eyes4s.studio.app.analysis.AnalysesNavigator.selected(model()))
          .options
          .find(_.preset == preset.preset)
          .flatMap(_.choose)
          .foreach(app)
    )
    (preset.preset, choose, detail)
  }
  val editor: RecipeForm = RecipeForm(model, app)
  private val content    = VBox(8.0)
  content.getStyleClass.add("inspector")
  content.getChildren.addAll(heading)
  options.foreach((_, choose, detail) => content.getChildren.addAll(choose, detail))
  val scanpaths = Button("Open MultiMatch scanpath workspace")
  scanpaths.setWrapText(true)
  scanpaths.setAccessibleText("Open MultiMatch scanpath workspace")
  private var scanpathWindows = Vector.empty[javafx.stage.Stage]
  scanpaths.setOnAction { _ =>
    if !disposed then
      val window =
        eyes4s.studio.desktop.scanpath.ScanpathWorkbench.open(node.getScene.getWindow)
      scanpathWindows = scanpathWindows.filter(_.isShowing) :+ window
  }
  content.getChildren.addAll(note, configuration, editor.node, scanpaths)
  val node: ScrollPane = ScrollPane(content)
  node.setFitToWidth(true)
  node.setFocusTraversable(false)

  def sync(value: AppModel): Unit = if !disposed then
    val picker = PresetPicker.vm(
      value.document,
      eyes4s.studio.app.analysis.AnalysesNavigator.selected(value)
    )
    val selected = eyes4s.studio.app.analysis.AnalysesNavigator.selected(value)
    val editable = value.document.draft.forall(d => selected.contains(d.id))
    editor.sync(value, editable)
    binding = true
    try
      heading.setText(picker.heading)
      note.setText(picker.note.getOrElse(""))
      note.setVisible(picker.note.isDefined)
      note.setManaged(picker.note.isDefined)
      options.zip(picker.options).foreach { case ((_, choose, detail), option) =>
        choose.setText(option.title)
        choose.setAccessibleText(option.accessible)
        choose.setDisable(option.choose.isEmpty)
        choose.setSelected(option.selected)
        detail.setText(option.detail + " · " + option.changes)
      }
      val recipe = eyes4s.studio.app.analysis.AnalysisSelection.selected(value).map(_.recipe)
      configuration.setText(
        recipe.fold("")(r =>
          s"${r.weighting.render} · grid ${r.grid.render} · scales ${r.scales.render}"
        )
      )
    finally binding = false

  def choose(preset: Preset): Unit     = options.find(_._1 == preset).foreach(_._2.fire())
  def enabled(preset: Preset): Boolean = options.find(_._1 == preset).exists(!_._2.isDisabled)
  def focusStops: Vector[FocusStop]    = PresetPicker
    .vm(model().document, eyes4s.studio.app.analysis.AnalysesNavigator.selected(model()))
    .options
    .filter(_.choose.isDefined)
    .map(o => FocusStop(A11yRole.RadioButton, o.accessible)) ++ editor.focusStops :+
    FocusStop(A11yRole.Button, "Open MultiMatch scanpath workspace")
  def dispose(): Unit =
    disposed = true
    editor.dispose()
    scanpathWindows.foreach(_.close())
    scanpathWindows = Vector.empty

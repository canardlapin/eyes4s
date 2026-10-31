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
import eyes4s.studio.app.analysis.{AnalysisSelection, AnalysesNavigator, PresetPicker}
import eyes4s.studio.app.text.{RecipeText, RecipeTextId as T}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.document.Preset
import eyes4s.studio.desktop.shell.SegmentedBar
import javafx.scene.control.{Button, Label, ScrollPane}
import javafx.scene.layout.VBox

/** Presets share the studio's one-stop segmented control and its selection styles. */
final class RecipePaneHost(
    model: () => AppModel,
    app: Intent => Unit,
    inputs: RecipeInputs,
    platform: Option[eyes4s.studio.core.platform.Platform[cats.effect.IO]] = None
):
  private var disposed    = false
  private var reasonNodes = Vector.empty[javafx.scene.control.ToggleButton]
  private val heading     = Label()
  heading.getStyleClass.add("t12-label")
  heading.setWrapText(true)
  private val note = Label()
  note.getStyleClass.add("t12")
  note.setWrapText(true)
  private val details = Label()
  details.getStyleClass.add("t11")
  details.setWrapText(true)
  val presets = SegmentedBar[Preset](preset =>
    if !disposed then
      PresetPicker
        .vm(model().document, AnalysesNavigator.selected(model()))
        .options
        .find(_.preset == preset)
        .flatMap(_.choose)
        .foreach(app)
  )
  val editor       = RecipeForm(model, app, inputs)
  private val diff = Label()
  diff.setWrapText(true)
  diff.getStyleClass.addAll("t12", "mono")
  editor.followEditing(_ => refreshDiff())
  private def refreshDiff(): Unit =
    diff.setText(
      editor.state.result.fold(RecipeText(T.Loading))(r =>
        r.differenceProblem.getOrElse(if r.differences.isEmpty then RecipeText(T.NoDiff)
        else r.differences.map(s => RecipeText(T.Served, s)).mkString("\n"))
      )
    )
  private val diffHeading = Label()
  diffHeading.getStyleClass.add("t12-label")
  private val diffContent = VBox(8, diffHeading, diff)
  diffContent.setPadding(javafx.geometry.Insets(10, 16, 10, 16))
  val diffNode = ScrollPane(diffContent)
  diffNode.setFitToWidth(true)
  diffNode.setFocusTraversable(false)
  val scanpaths = Button(RecipeText(T.Scanpaths))
  scanpaths.setWrapText(true)
  scanpaths.setAccessibleText(RecipeText(T.Scanpaths))
  private var scanpathWindows = Vector.empty[javafx.stage.Stage]
  scanpaths.setOnAction { _ =>
    if !disposed then
      val window =
        eyes4s.studio.desktop.scanpath.ScanpathWorkbench.open(node.getScene.getWindow, platform)
      scanpathWindows = scanpathWindows.filter(_.isShowing) :+ window
  }
  // The preset bar is the recipe grid's first row; its heading and the
  // presets' detail lines are read, not shown (overhaul v4).
  Vector(heading, details).foreach { l => l.setVisible(false); l.setManaged(false) }
  editor.presetSlot.getChildren.setAll(presets.node, note)
  scanpaths.getStyleClass.add("recipe-quiet")
  private val content = VBox(8, heading, details, editor.node, scanpaths)
  content.getStyleClass.add("inspector")
  content.setPadding(javafx.geometry.Insets(10, 16, 10, 16))
  val node = ScrollPane(content)
  node.setFitToWidth(true)
  node.setFocusTraversable(false)

  def sync(value: AppModel): Unit = if !disposed then
    val selected = AnalysesNavigator.selected(value)
    val picker   = PresetPicker.vm(value.document, selected)
    val context  = AnalysisSelection.selected(value)
    val editable = value.document.draft.forall(d => selected.contains(d.id))
    editor.sync(value, editable)
    heading.setText(picker.heading)
    presets.show(
      picker.options.map(o => SegmentedBar.Segment(o.preset, o.title, o.accessible, o.selected))
    )
    presets.node.setDisable(
      !picker.options.exists(_.choose.nonEmpty) && !context.exists(_ => editable)
    )
    details.setText(
      picker.options.map(o => RecipeText(T.Served, o.title + " · " + o.detail)).mkString("\n")
    )
    val selectionNote = context
      .filter(c => eyes4s.studio.core.preset.RecipePresets.recognition.holds(c.recipe))
      .filter(_.recipe.expectedUnmatched.isEmpty)
      .map(_ => RecipeText(T.SelectionNeeded))
    val reason =
      if !editable then Some(RecipeText(T.Locked)) else selectionNote.orElse(picker.note)
    note.setText(reason.getOrElse(""))
    note.setVisible(reason.nonEmpty)
    note.setManaged(reason.nonEmpty)
    if reasonNodes != presets.segmentNodes then
      presets.attachReason(note)
      reasonNodes = presets.segmentNodes
    diffHeading.setText(
      RecipeText(T.Diff, context.flatMap(_.savedBase).fold(RecipeText(T.Recipe))(_.id.label))
    )
    refreshDiff()

  /** The pane header's subtitle (overhaul v4): the revision, and for a
    * draft how many changes it makes to its saved base.
    */
  def subtitle: String =
    val m = model()
    AnalysisSelection.selected(m).fold("") { c =>
      val draft = m.document.draft.exists(_.id == c.id)
      c.savedBase.filter(_ => draft).fold(RecipeText(T.PaneSaved, c.id.label)) { base =>
        val n =
          eyes4s.studio.core.document.RecipeChange.between(base.recipe, c.recipe).size
        RecipeText(
          T.PaneDraft,
          c.id.label,
          if n == 1 then RecipeText(T.OneChange) else RecipeText(T.Changes, n.toString),
          base.id.label
        )
      }
    }

  def choose(preset: Preset): Unit     = if enabled(preset) then presets.choose(preset)
  def enabled(preset: Preset): Boolean = PresetPicker
    .vm(model().document, AnalysesNavigator.selected(model()))
    .options
    .exists(o => o.preset == preset && o.choose.nonEmpty)
  def focusStops: Vector[FocusStop] =
    presets.segmentNodes
      .filter(n => n.isFocusTraversable && !n.isDisabled)
      .map(n => FocusStop(A11yRole.RadioButton, n.getAccessibleText)) ++
      editor.focusStops :+
      FocusStop(A11yRole.Button, RecipeText(T.Scanpaths))
  def dispose(): Unit =
    disposed = true
    editor.dispose()
    scanpathWindows.foreach(_.close())
    scanpathWindows = Vector.empty

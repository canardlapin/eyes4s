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
import eyes4s.studio.app.analysis.{PresetPicker, RecipeEditor, RecipeInput}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.document.*
import javafx.scene.control.{Button, ComboBox, Label, TextField}
import javafx.scene.layout.{GridPane, VBox}
import javafx.util.StringConverter
import scala.jdk.CollectionConverters.*

/** An explicit apply step keeps incomplete numeric text out of the document. */
final class RecipeForm(model: () => AppModel, app: Intent => Unit):
  private var shown: Option[Recipe] = None
  private var binding = false
  private var disposed = false
  private val labels = Vector(
    "focal" -> "Query phase", "reference" -> "Reference phase",
    "columns" -> "Grid columns", "rows" -> "Grid rows",
    "scales" -> "Gaussian σ (degrees, comma separated)",
    "pixelsPerDegree" -> "Pixels per degree (blank for dataset)",
    "xMin" -> "Window x minimum (px)", "yMin" -> "Window y minimum (px)",
    "xMax" -> "Window x maximum (exclusive, px)", "yMax" -> "Window y maximum (exclusive, px)"
  )
  val fields: Map[String, TextField] = labels.map { (key, label) =>
    val f = TextField()
    f.setId(s"recipe-$key")
    f.setAccessibleText(label)
    f.textProperty.addListener((_, _, _) => changed())
    key -> f
  }.toMap
  private def choices[A](values: Vector[A], label: String)(show: A => String): ComboBox[A] =
    val box = ComboBox[A]()
    box.getItems.setAll(values.asJava)
    box.setAccessibleText(label)
    box.setConverter(new StringConverter[A]:
      def toString(a: A): String = Option(a).fold("")(show)
      def fromString(s: String): A = box.getValue
    )
    box.setOnAction(_ => changed())
    box
  val weighting: ComboBox[WeightChoice] = choices(WeightChoice.values.toVector, "Fixation weighting")(_.render)
  val unmatched: ComboBox[UnmatchedChoice] = choices(UnmatchedChoice.values.toVector, "Queries without a match")(_.render)
  val apply: Button = Button("Apply settings")
  apply.setId("recipe-apply")
  val problem: Label = Label()
  problem.setWrapText(true)
  private val conventions = Label()
  conventions.setWrapText(true)
  private val form = GridPane()
  form.setHgap(8)
  form.setVgap(6)
  labels.zipWithIndex.foreach { case ((key, title), row) =>
    val label = Label(title)
    label.setWrapText(true)
    label.setLabelFor(fields(key))
    form.add(label, 0, row)
    form.add(fields(key), 1, row)
  }
  Vector("Fixation weighting" -> weighting, "Queries without a match" -> unmatched)
    .zipWithIndex.foreach { case ((title, control), index) =>
      val label = Label(title)
      label.setLabelFor(control)
      form.add(label, 0, labels.size + index)
      form.add(control, 1, labels.size + index)
    }
  val node: VBox = VBox(8, conventions, form, problem, apply)
  private def text(key: String): String = fields(key).getText
  def input: RecipeInput = RecipeInput(text("focal"), text("reference"), text("columns"),
    text("rows"), text("scales"), text("pixelsPerDegree"), text("xMin"), text("yMin"),
    text("xMax"), text("yMax"), weighting.getValue, unmatched.getValue)
  private def changed(): Unit = if !binding && !disposed && shown.isDefined then
    RecipeEditor.parse(shown.get, input) match
      case Left(error) =>
        problem.setText(error.message)
        apply.setDisable(true)
      case Right(next) =>
        val dirty = !shown.contains(next)
        problem.setText(if dirty then "Apply these changes before Save & run." else "Settings applied.")
        apply.setDisable(!dirty || node.isDisabled)
  apply.setOnAction { _ =>
    if !disposed && !node.isDisabled then
      RecipeEditor.command(model().document, input) match
        case Left(error) => problem.setText(error.message)
        case Right(command) => command.foreach(c => app(Intent.Dispatch(c)))
  }
  def sync(value: AppModel, editable: Boolean): Unit = if !disposed then
    val current = PresetPicker.edited(value.document).map(_._2)
    node.setVisible(current.isDefined)
    node.setManaged(current.isDefined)
    node.setDisable(!editable)
    if current != shown then
      shown = current
      binding = true
      try current.foreach { r =>
        val f = RecipeEditor.input(r)
        val values = Vector(f.focal, f.reference, f.columns, f.rows, f.scales, f.pixelsPerDegree,
          f.xMin, f.yMin, f.xMax, f.yMax)
        labels.map(_._1).zip(values).foreach((k,v) => fields(k).setText(v))
        weighting.setValue(f.weighting)
        unmatched.setValue(f.unmatched)
        conventions.setText(s"${r.method.render}; ${r.matched.render}; controls: ${r.controls.render}; " +
          s"${r.failurePolicy.render}; ${r.initialFixations.render}. Window is half-open; clear all four bounds for the full frame.")
        problem.setText("Settings applied.")
        apply.setDisable(true)
      }
      finally binding = false
    else if !editable then apply.setDisable(true)
  def focusStops: Vector[FocusStop] =
    if node.isDisabled || !node.isVisible then Vector.empty
    else labels.map((_, title) => FocusStop(A11yRole.TextField, title)) ++ Vector(
      FocusStop(A11yRole.ComboBox, "Fixation weighting"),
      FocusStop(A11yRole.ComboBox, "Queries without a match"),
      FocusStop(A11yRole.Button, "Apply settings"))
  def dispose(): Unit = disposed = true

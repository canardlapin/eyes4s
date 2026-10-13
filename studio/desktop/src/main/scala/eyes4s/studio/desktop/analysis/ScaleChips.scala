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

import eyes4s.plan.{RawValue, Numeral}
import eyes4s.studio.app.text.{RecipeText, RecipeTextId as T}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.backend.RecipeFormResult
import eyes4s.studio.desktop.shell.{CompositeFocus, DisabledReason, SegmentedBar}
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.layout.{HBox, VBox}

/** The shared segmented control selects one scale for removal; every chip
  * remains an analysis scale. All cell sizes and warnings are served by eyes4s.
  */
final class ScaleChips(changed: RawValue => Unit):
  private var raw                    = Vector.empty[RawValue]
  private var scales                 = Vector.empty[Double]
  private var cursor: Option[Double] = None
  private var baseline               = Set.empty[Double]
  private val title                  = Label(RecipeText(T.Scales))
  title.getStyleClass.add("t12-label")
  val bar          = SegmentedBar[Double](value => { cursor = Some(value); buttons() })
  private val cell = Label()
  cell.getStyleClass.addAll("t11", "mono")
  private val warnings = Label()
  warnings.setWrapText(true)
  warnings.getStyleClass.addAll("t11", "recipe-warning")
  val entry = TextField()
  entry.setAccessibleText(RecipeText(T.ScaleEntry))
  entry.setPromptText(RecipeText(T.ScaleEntry))
  entry.setPrefColumnCount(20)
  entry.getStyleClass.addAll("t12", "mono", "studio-field")
  val add    = Button(RecipeText(T.AddScale))
  val remove = Button(RecipeText(T.RemoveScale))
  add.getStyleClass.add("studio-button")
  remove.getStyleClass.add("studio-button")
  add.setAccessibleText(add.getText)
  remove.setAccessibleText(remove.getText)
  private val reason = Label()
  reason.getStyleClass.add("t11")
  reason.setWrapText(true)
  DisabledReason.attach(add, reason)
  DisabledReason.attach(remove, reason)
  private def number =
    entry.getText.trim.toDoubleOption.filter(n => n.isFinite && n > 0 && !scales.contains(n))
  private def buttons(): Unit =
    add.setDisable(number.isEmpty || raw.isEmpty)
    remove.setDisable(raw.size <= 1 || cursor.isEmpty)
    reason.setText(
      (Vector(
        Option.when(number.isEmpty)(RecipeText(T.InvalidScale)),
        Option.when(raw.size <= 1)(RecipeText(T.LastScale))
      ).flatten :+ RecipeText(T.ScaleHelp)).mkString("\n")
    )
  entry.textProperty.addListener((_, _, _) => buttons())
  add.setOnAction { _ =>
    number.foreach { value =>
      raw.headOption.foreach { template =>
        val next = template match
          case RawValue.Variant(token, parts) =>
            RawValue.Variant(
              token,
              parts.map {
                case (id, _) if id.value == "sigma" =>
                  id -> RawValue.Number(Numeral.real.write(value))
                case other => other
              }
            )
          case other => other
        cursor = Some(value)
        changed(RawValue.Items(raw :+ next))
        entry.clear()
      }
    }
  }
  private def removeSelected(): Unit = if scales.size > 1 then
    cursor.foreach { selected =>
      val i = scales.indexOf(selected)
      if i >= 0 then
        cursor = scales.patch(i, Nil, 1).lift(math.min(i, scales.size - 2))
        changed(RawValue.Items(raw.patch(i, Nil, 1)))
    }
  remove.setOnAction(_ => removeSelected())
  bar.node.addEventFilter(
    KeyEvent.KEY_PRESSED,
    (e: KeyEvent) =>
      if e.getCode == KeyCode.DELETE || e.getCode == KeyCode.BACK_SPACE then
        e.consume()
        removeSelected()
  )
  val node = VBox(5, title, bar.node, HBox(6, entry, add, remove), cell, warnings, reason)
  def show(value: RawValue, saved: Set[Double], result: Option[RecipeFormResult]): Boolean =
    raw = value match
      case RawValue.Items(values) => values
      case _                      => Vector.empty
    val parsed = raw.map {
      case RawValue.Variant("degrees.gaussian", parts) =>
        parts.collectFirst {
          case (id, RawValue.Number(text)) if id.value == "sigma" => text.toDoubleOption
        }.flatten
      case _ => None
    }
    val supported = raw.nonEmpty && parsed.forall(_.exists(n => n.isFinite && n > 0))
    if supported then
      scales = parsed.flatten
      baseline = saved
      if !cursor.exists(scales.contains) then cursor = scales.lastOption
      bar.show(
        scales.map(n =>
          SegmentedBar.Segment(
            n,
            RecipeText(if baseline(n) then T.Scale else T.NewScale, Numeral.real.write(n)),
            RecipeText(if baseline(n) then T.Scale else T.NewScale, Numeral.real.write(n)),
            cursor.contains(n)
          )
        )
      )
      CompositeFocus.noCursor(bar.node, scales.isEmpty)
      cell.setText(
        result
          .flatMap(_.cellDegrees)
          .fold(RecipeText(T.CellUnknown))(c =>
            RecipeText(T.Cell, f"${c.x}%.2f", f"${c.y}%.2f")
          )
      )
      warnings.setText(
        result.toVector
          .flatMap(_.warnings)
          .flatMap(w =>
            scales.lift(w.scale).map { scale =>
              if w.kind == "belowCells" then
                RecipeText(
                  T.BelowCells,
                  Numeral.real.write(scale),
                  f"${w.actual}%.1f",
                  Numeral.real.write(w.limit)
                )
              else RecipeText(T.NearUniform, Numeral.real.write(scale), f"${w.actual}%.2f")
            }
          )
          .mkString("\n")
      )
      buttons()
    node.setVisible(supported)
    node.setManaged(supported)
    supported
  def focusStops: Vector[FocusStop] =
    if !node.isVisible || node.isDisabled then Vector.empty
    else
      bar.segmentNodes
        .filter(_.isFocusTraversable)
        .map(n => FocusStop(A11yRole.RadioButton, n.getAccessibleText)) ++
        Vector(FocusStop(A11yRole.TextField, entry.getAccessibleText)) ++
        Vector(add, remove)
          .filterNot(_.isDisabled)
          .map(n => FocusStop(A11yRole.Button, n.getText))

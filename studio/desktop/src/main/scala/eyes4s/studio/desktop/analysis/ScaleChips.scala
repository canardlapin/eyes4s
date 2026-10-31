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
  // The grid's row label names the scales (overhaul v4).
  private val title = Label(RecipeText(T.Scales))
  title.getStyleClass.add("t12-label")
  title.setVisible(false)
  title.setManaged(false)
  val bar          = SegmentedBar[Double](value => { cursor = Some(value); buttons() })
  private val cell = Label()
  cell.getStyleClass.addAll("t11", "mono")
  private val warnings = VBox(6.0)
  warnings.getStyleClass.add("recipe-warnings")
  private val ringsNote = Label()
  ringsNote.setWrapText(true)
  ringsNote.getStyleClass.addAll("t11", "recipe-rings-note")
  private val rings = SigmaRings()
  val entry         = TextField()
  entry.setAccessibleText(RecipeText(T.ScaleEntry))
  entry.setPromptText(RecipeText(T.ScaleEntry))
  entry.setPrefColumnCount(5)
  entry.setPromptText("σ °")
  entry.getStyleClass.addAll("t12", "studio-field", "recipe-scale-entry")
  val add    = Button(RecipeText(T.AddScale))
  val remove = Button(RecipeText(T.RemoveScale))
  add.getStyleClass.addAll("studio-button", "recipe-quiet")
  remove.getStyleClass.addAll("studio-button", "recipe-quiet")
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
  // The golden's scales row: the chips, then a new scale's entry and its
  // quiet actions; under them the rings to scale beside their notes.
  cell.setVisible(false)
  cell.setManaged(false)
  bar.node.getStyleClass.add("recipe-chips")
  private val chipsRow = HBox(6, bar.node, entry, add, remove)
  chipsRow.setAlignment(javafx.geometry.Pos.CENTER_LEFT)
  private val ringsRow = HBox(14, rings.node, VBox(6, ringsNote, warnings))
  val node             = VBox(6, title, chipsRow, ringsRow, cell, reason)
  def show(
      value: RawValue,
      saved: Set[Double],
      result: Option[RecipeFormResult],
      frame: Option[SigmaRings.Frame] = None
  ): Boolean =
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
      warnings.getChildren.setAll(
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
          .map { words =>
            val l = Label(words)
            l.setGraphic(
              eyes4s.studio.desktop.icons.StudioIcons
                .graphic(
                  eyes4s.studio.app.icons.Icon.Warning,
                  eyes4s.studio.desktop.icons.IconSize.Px16
                )
            )
            l.setGraphicTextGap(5.0)
            l.setWrapText(true)
            l.getStyleClass.addAll("t11", "recipe-warning")
            l
          }*
      )
      val cellDeg = result.flatMap(_.cellDegrees).map(_.x)
      rings.show(frame, scales.map(n => (n, !baseline(n))), cellDeg)
      ringsNote.setText(
        frame.fold("")(f =>
          RecipeText(
            T.RingsNote,
            f.widthPx.toString,
            f.heightPx.toString,
            Numeral.real.write(f.pixelsPerDegree),
            rings.inner.map(n => Numeral.real.write(n) + "°").mkString(" and ")
          )
        )
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

/** The scales as rings to scale on the image frame (overhaul v4,
  * `analysis.html`): each σ's radius on a 160 × 120 drawing of the frame,
  * a new σ in the accent, the larger rings labelled, and one grid cell's key.
  */
final class SigmaRings:
  import SigmaRings.*
  private val pane = javafx.scene.layout.Pane()
  pane.getStyleClass.add("recipe-rings")
  pane.setMinSize(WidthPx, HeightPx)
  pane.setPrefSize(WidthPx, HeightPx)
  pane.setMaxSize(WidthPx, HeightPx)
  val node: javafx.scene.layout.Pane = pane
  private var unlabelled             = Vector.empty[Double]

  /** The scales drawn without a label: too small to name on the drawing. */
  def inner: Vector[Double] = unlabelled

  def show(
      frame: Option[Frame],
      scales: Vector[(Double, Boolean)],
      cellDeg: Option[Double]
  ): Unit =
    pane.getChildren.clear()
    pane.setVisible(frame.isDefined)
    pane.setManaged(frame.isDefined)
    frame.foreach { f =>
      val k      = WidthPx / f.widthPx
      val cx     = WidthPx / 2
      val cy     = HeightPx / 2
      val ground = javafx.scene.shape.Rectangle(0, 0, WidthPx, HeightPx)
      ground.getStyleClass.add("recipe-rings-ground")
      pane.getChildren.add(ground)
      unlabelled = Vector.empty
      scales.foreach { (deg, fresh) =>
        val r = deg * f.pixelsPerDegree * k
        val c = javafx.scene.shape.Circle(cx, cy, r)
        c.getStyleClass.addAll(
          "recipe-ring",
          if fresh then "recipe-ring-new" else "recipe-ring-saved"
        )
        pane.getChildren.add(c)
        if r >= LabelPx then
          val words =
            if fresh then RecipeText(T.NewRing, Numeral.real.write(deg) + "°")
            else Numeral.real.write(deg) + "°"
          val t = javafx.scene.text.Text(words)
          t.getStyleClass.addAll(
            "recipe-ring-label",
            if fresh then "recipe-ring-new-label" else ""
          )
          t.setX(cx - t.getLayoutBounds.getWidth / 2)
          t.setY(cy - r + 9.5)
          pane.getChildren.add(t)
        else unlabelled = unlabelled :+ deg
      }
      cellDeg.foreach { d =>
        val side = math.max(d * f.pixelsPerDegree * k, 1.0)
        val sq   = javafx.scene.shape.Rectangle(8, HeightPx - 8 - side, side, side)
        sq.getStyleClass.add("recipe-rings-cell")
        val key = javafx.scene.text.Text(14.5, HeightPx - 8, RecipeText(T.CellKey, f"$d%.2f"))
        key.getStyleClass.add("recipe-ring-label")
        pane.getChildren.addAll(sq, key)
      }
    }

object SigmaRings:
  /** The drawing's size; the frame is scaled to its width. */
  val WidthPx: Double  = 160.0
  val HeightPx: Double = 120.0

  /** A ring smaller than this is drawn without its label. */
  val LabelPx: Double = 9.0

  /** The image frame the maps are on, and its declared angular scale. */
  final case class Frame(widthPx: Int, heightPx: Int, pixelsPerDegree: Double) derives CanEqual

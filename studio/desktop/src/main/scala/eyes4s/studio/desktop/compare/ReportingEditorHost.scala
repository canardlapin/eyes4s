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

import eyes4s.studio.app.compare.*
import eyes4s.studio.app.vm.FocusStop
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.document.{Covariate, ReportingWeight}
import eyes4s.studio.desktop.plot.TableTwinView
import javafx.geometry.Pos
import javafx.scene.control.{
  Button,
  CheckBox,
  Label,
  RadioButton,
  ScrollPane,
  TextField,
  ToggleGroup
}
import javafx.scene.layout.{HBox, Priority, Region, VBox}

/** Compare's reporting editor on the desktop (ticket S8.7; Results.dc.html,
  * reporting; see [[ReportingEditor]]): the shown spec's grouping, filters,
  * minimum and weighting, the estimand, and the saved specs. It reads the
  * summary layout's state through `summary`, follows the model, and only
  * binds [[ReportingEditorVM]]; each control sends the editor's intents to
  * the app. Use on the JavaFX thread.
  */
final class ReportingEditorHost(
    model: () => AppModel,
    app: Intent => Unit,
    summary: () => CompareSummary
):
  private var state    = ReportingEditor.empty
  private var disposed = false
  // True while render sets the controls, so their handlers do not echo.
  private var binding = false

  private def label(styles: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(styles*)
    l.setWrapText(true)
    l

  private val title              = label("t13")
  private val kind               = label("kind")
  private val status             = label("t12", "inspector-status")
  private val reuses             = label("t11", "inspector-note")
  private val groupBy            = label("lbl")
  private val groups             = VBox(4.0)
  private val values             = label("t11", "inspector-key")
  private val contrastTitle      = label("lbl")
  private val contrastMinuend    = TextField()
  private val contrastSubtrahend = TextField()
  private val contrastApply      = Button()
  private val contrastClear      = Button()
  private var contrastBound
      : Option[(Option[eyes4s.studio.core.document.ReportingId], String, String)] = None
  private val filters                                                             = label("lbl")
  private val all                                                                 = label("t12")
  private val outside                                                             = CheckBox()
  private val outNote  = label("t11", "inspector-key")
  private val keeps    = VBox(2.0)
  private val minimum  = CheckBox()
  private val minNote  = label("t11", "inspector-key")
  private val weight   = label("lbl")
  private val unit     = label("t11", "inspector-note")
  private val weights  = ToggleGroup()
  private val equal    = RadioButton()
  private val pooled   = RadioButton()
  private val estimand = label("lbl")
  private val measures = label("t12")
  private val savedLbl = label("lbl")
  private val saveAs   = Button()
  private val name     = TextField()
  private val save     = Button()
  private val cancel   = Button()
  private val saved    = VBox(4.0)
  private val error    = label("t11", "inspector-note")
  // What the group-by choices and the saved specs were last built from.
  private var groupOptions: Vector[(Option[Covariate], String)] = Vector.empty
  private var groupButtons: Vector[RadioButton]                 = Vector.empty
  private var savedListed
      : Vector[(eyes4s.studio.core.document.ReportingId, String, String, Boolean)] =
    Vector.empty
  Vector(outside, minimum, equal, pooled).foreach(_.getStyleClass.add("t12"))
  Vector(outside, minimum, equal, pooled).foreach(_.setWrapText(true))
  Vector(saveAs, save, cancel, contrastApply, contrastClear).foreach(_.getStyleClass.add("btn"))
  equal.setToggleGroup(weights)
  pooled.setToggleGroup(weights)

  contrastApply.setOnAction(_ =>
    act(ReportingIntent.ContrastOperands(contrastMinuend.getText, contrastSubtrahend.getText))
  )
  contrastMinuend.setOnAction(_ => contrastApply.fire())
  contrastSubtrahend.setOnAction(_ => contrastApply.fire())
  contrastClear.setOnAction(_ => act(ReportingIntent.SetContrast(None)))

  outside.setOnAction(_ => act(ReportingIntent.OutsideFilter(outside.isSelected)))
  minimum.setOnAction(_ => act(ReportingIntent.Minimum(minimum.isSelected)))
  equal.setOnAction(_ => act(ReportingIntent.Weight(ReportingWeight.ParticipantMeans)))
  pooled.setOnAction(_ => act(ReportingIntent.Weight(ReportingWeight.PooledQueries)))
  saveAs.setOnAction(_ => act(ReportingIntent.OpenSaveAs))
  name.textProperty.addListener((_, _, t) => if !binding then act(ReportingIntent.Name(t)))
  name.setOnAction(_ => act(ReportingIntent.ConfirmSaveAs))
  save.setOnAction(_ => act(ReportingIntent.ConfirmSaveAs))
  cancel.setOnAction(_ => act(ReportingIntent.CancelSaveAs))

  private val headRow =
    val spacer = Region()
    HBox.setHgrow(spacer, Priority.ALWAYS)
    val h = HBox(8.0, title, spacer, kind)
    h.setAlignment(Pos.CENTER_LEFT)
    h
  private val savedRow =
    val spacer = Region()
    HBox.setHgrow(spacer, Priority.ALWAYS)
    val h = HBox(8.0, savedLbl, spacer, saveAs)
    h.setAlignment(Pos.CENTER_LEFT)
    h
  private val naming = HBox(4.0, name, save, cancel)
  HBox.setHgrow(name, Priority.ALWAYS)

  private def section(children: javafx.scene.Node*): VBox =
    val box = VBox(6.0, children*)
    box.getStyleClass.add("sect")
    box

  private val content = VBox(
    VBox(
      10.0,
      status,
      headRow,
      reuses,
      VBox(4.0, groupBy, groups, values),
      VBox(
        4.0,
        contrastTitle,
        contrastMinuend,
        Label("−"),
        contrastSubtrahend,
        HBox(4.0, contrastApply, contrastClear)
      ),
      VBox(5.0, filters, all, outside, outNote, keeps, minimum, minNote),
      VBox(5.0, weight, unit, equal, pooled)
    ),
    section(estimand, measures),
    section(savedRow, naming, saved, error)
  )
  content.setPadding(javafx.geometry.Insets(12, 14, 12, 14))
  content.setSpacing(10.0)

  /** The pane's content. */
  val node: ScrollPane = ScrollPane(content)
  node.setFitToWidth(true)
  node.setFocusTraversable(false)
  node.getStyleClass.addAll("fixation-inspector", "reporting-editor")
  Option(getClass.getClassLoader.getResource(TableTwinView.stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** The view-model now. */
  def vm: ReportingEditorVM =
    val s      = summary()
    val report = s.shown.flatMap(scale => s.reports.get((scale, false))).collect {
      case ReportAnswer.Answered(view) => view
    }
    ReportingEditor.vm(state, model().document, s.reporting, s.run, s.answered, s.shown, report)

  /** The controls inside the pane's own stop, in Tab order. */
  def focusStops: Vector[FocusStop] = ReportingEditor.focusStops(vm)

  /** The user's gestures, as the controls make them. */
  def act(intent: ReportingIntent): Unit = if !binding && !disposed then
    val s               = summary()
    val (next, intents) = ReportingEditor.update(state, model().document, s.reporting, intent)
    state = next
    intents.foreach(app)
    render()

  /** The words shown: the spec's title and the notes, for tests. */
  def shown: Map[String, String] = Map(
    "title"    -> title.getText,
    "reuses"   -> reuses.getText,
    "all"      -> all.getText,
    "outside"  -> outNote.getText,
    "minimum"  -> minNote.getText,
    "measures" -> measures.getText
  )

  /** Whether each toggle is on, as shown: outside, minimum, pooled. */
  def toggles: (Boolean, Boolean, Boolean) =
    (outside.isSelected, minimum.isSelected, pooled.isSelected)

  /** The group-by choice nodes, as shown (the same nodes across renders). */
  def groupNodes: Vector[RadioButton] = groupButtons

  /** The minimum's accessible help: its note, as a reader announces it. */
  def minimumHelp: String = minimum.getAccessibleHelp

  /** Clicks a toggle, as the user does. */
  def clickMinimum(): Unit = minimum.fire()
  def clickOutside(): Unit = outside.fire()

  /** Enter and apply both operands through the same controls a user uses. */
  def applyContrast(minuend: String, subtrahend: String): Unit =
    contrastMinuend.setText(minuend)
    contrastSubtrahend.setText(subtrahend)
    contrastApply.fire()

  def clearContrast(): Unit = contrastClear.fire()

  /** Draws the view-model now. */
  def render(): Unit = if !disposed then
    val v = vm
    binding = true
    try
      status.setText(v.status.getOrElse(""))
      status.setVisible(v.status.isDefined); status.setManaged(v.status.isDefined)
      headRow.setVisible(v.status.isEmpty)
      title.setText(v.title)
      kind.setText(v.kind)
      reuses.setText(v.reuses.text)
      groupBy.setText(v.groupLabel)
      // The choices are rebuilt only when they change, so a keyboard user
      // who picks one keeps focus on it.
      val options = v.groups.map(c => (c.value, c.label))
      if options != groupOptions then
        groupOptions = options
        val group = ToggleGroup()
        groupButtons = v.groups.map { c =>
          val b = RadioButton(c.label)
          b.getStyleClass.add("t12")
          b.setToggleGroup(group)
          b.setAccessibleText(s"${v.groupLabel}: ${c.label}")
          val choice: Option[Covariate] = c.value
          b.setOnAction(_ => act(ReportingIntent.GroupBy(choice)))
          b
        }
        groups.getChildren.setAll(groupButtons*): Unit
      groupButtons.zip(v.groups).foreach((b, c) => b.setSelected(c.chosen))
      values.setText(v.groupValues)
      values.setVisible(v.groupValues.nonEmpty); values.setManaged(v.groupValues.nonEmpty)
      contrastTitle.setText(v.contrast.title)
      contrastMinuend.setPromptText(v.contrast.minuendLabel)
      contrastMinuend.setAccessibleText(v.contrast.minuendLabel)
      contrastSubtrahend.setPromptText(v.contrast.subtrahendLabel)
      contrastSubtrahend.setAccessibleText(v.contrast.subtrahendLabel)
      contrastApply.setText(v.contrast.apply); contrastApply.setAccessibleText(v.contrast.apply)
      contrastClear.setText(v.contrast.clear); contrastClear.setAccessibleText(v.contrast.clear)
      Vector(contrastMinuend, contrastSubtrahend, contrastApply, contrastClear)
        .foreach(_.setDisable(!v.contrast.enabled))
      val contrastNow = (summary().reporting, v.contrast.minuend, v.contrast.subtrahend)
      if !contrastBound.contains(contrastNow) then
        contrastBound = Some(contrastNow)
        contrastMinuend.setText(v.contrast.minuend)
        contrastSubtrahend.setText(v.contrast.subtrahend)
      filters.setText(v.filterTitle)
      all.setText(v.contributing.text)
      outside.setText(v.outside.label); outside.setAccessibleText(v.outside.label)
      outside.setSelected(v.outside.on)
      outNote.setText(v.outside.note.text)
      outside.setAccessibleHelp(v.outside.note.text)
      keeps.getChildren.setAll(v.keeps.map { k =>
        val l = label("t12"); l.setText(k); l
      }*)
      minimum.setText(v.minimum.label); minimum.setAccessibleText(v.minimum.label)
      minimum.setSelected(v.minimum.on)
      minNote.setText(v.minimum.note.text)
      minimum.setAccessibleHelp(v.minimum.note.text)
      weight.setText(v.weightTitle)
      unit.setText(v.weightUnit)
      Vector(equal, pooled).zip(v.weights).foreach { (b, c) =>
        b.setText(c.label)
        b.setAccessibleText(s"${v.weightTitle}: ${c.label}")
        b.setSelected(c.chosen)
      }
      estimand.setText(v.estimandTitle)
      measures.setText(v.estimand)
      savedLbl.setText(v.savedTitle)
      saveAs.setText(v.saveAs); saveAs.setAccessibleText(v.saveAs)
      v.saving match
        case Some(sv) =>
          if name.getText != sv.name then name.setText(sv.name)
          name.setAccessibleText(sv.label)
          save.setText(sv.save); save.setAccessibleText(sv.save)
          cancel.setText(sv.cancel); cancel.setAccessibleText(sv.cancel)
          naming.setVisible(true); naming.setManaged(true)
        case None =>
          naming.setVisible(false); naming.setManaged(false)
      val listed = v.saved.map(sp => (sp.id, sp.name, sp.detail, sp.current))
      if listed != savedListed then
        savedListed = listed
        saved.getChildren.setAll(v.saved.map { sp =>
          if sp.current then
            val l = label("t12")
            l.setText(s"${sp.name}\n${sp.detail}")
            l
          else
            val b = Button(s"${sp.name}\n${sp.detail}")
            b.getStyleClass.add("btn")
            b.setAccessibleText(s"${sp.name}, ${sp.detail}")
            b.setOnAction(_ => act(ReportingIntent.Choose(sp.id)))
            b
        }*): Unit
      error.setText(v.error.getOrElse(""))
      error.setVisible(v.error.isDefined); error.setManaged(v.error.isDefined)
    finally binding = false

  /** The host ignores the model from then on. Idempotent. */
  def dispose(): Unit = disposed = true

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

import eyes4s.studio.app.analysis.{Preflight, PreflightVM, ResolvedDesign}
import eyes4s.studio.app.diagnostics.{DiagnosticsPresenter, FindingSeverity, FindingVM}
import eyes4s.studio.app.plot.ViewSelection
import eyes4s.studio.app.text.{DiagnosticText, DiagnosticTextId, PreflightText, PreflightTextId}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.desktop.plot.TableTwinView
import eyes4s.studio.desktop.shell.DisabledReason
import javafx.geometry.Pos
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.control.{Button, Label, ScrollPane, TextArea, TextField}
import eyes4s.studio.core.document.{Preset, RevisionName, StudioFields}
import eyes4s.studio.core.command.Command
import javafx.scene.layout.{ColumnConstraints, GridPane, HBox, Priority, Region, VBox}

/** The Analysis perspective's preflight pane and run card on the desktop
  * (ticket S7.6; Analysis.dc.html, preflight; see [[Preflight]]): the studio's
  * checks and eyes4s's findings in separate sections, each finding with its
  * severity, title, detail and remedy; what is not checked; and the run card
  * with Save & run, enabled or disabled with its reason. It only binds
  * [[PreflightVM]]; a remedy opens its trials, Save & run dispatches. Use on
  * the JavaFX thread.
  */
final class PreflightHost(model: () => AppModel, app: Intent => Unit):
  private var design                              = ResolvedDesign.empty
  private var editing: Option[String]             = None
  def recipeEditing(reason: Option[String]): Unit =
    if editing != reason then
      editing = reason
      render()
  // The view the remedies select from (a constant; PreflightSuite checks it).
  private var selection =
    Preflight.viewId.toOption.map(ViewSelection.initial(_, model().selection))

  private def label(text: String, styles: String*): Label =
    val l = Label(text)
    l.getStyleClass.addAll(styles*)
    l.setWrapText(true)
    l.setMinHeight(Region.USE_PREF_SIZE)
    l

  private val status     = label("", "t12", "preflight-status")
  private val studio     = VBox(6.0)
  private val eyes       = VBox(6.0)
  private val report     = label("", "t11", "mono", "preflight-report")
  private val notChecked = label("", "t11", "preflight-note")
  private val lines      = GridPane()
  lines.setHgap(8.0)
  lines.setVgap(3.0)
  private val keyColumn   = ColumnConstraints(116)
  private val valueColumn = ColumnConstraints()
  valueColumn.setMinWidth(0)
  valueColumn.setHgrow(Priority.ALWAYS)
  lines.getColumnConstraints.setAll(keyColumn, valueColumn)
  private val run = Button()
  run.getStyleClass.add("preflight-run")
  run.setMaxWidth(Double.MaxValue)
  val runName: TextField = TextField()
  runName.setAccessibleText(
    eyes4s.studio.app.text.RecipeText(eyes4s.studio.app.text.RecipeTextId.Name)
  )
  val runDescription: TextArea = TextArea()
  runDescription.setAccessibleText(
    eyes4s.studio.app.text.RecipeText(eyes4s.studio.app.text.RecipeTextId.Notes)
  )
  runDescription.setPrefRowCount(3)
  runDescription.setWrapText(true)
  runDescription.addEventFilter(
    KeyEvent.KEY_PRESSED,
    (e: KeyEvent) =>
      if e.getCode == KeyCode.TAB && !e.isControlDown && !e.isAltDown && !e.isMetaDown then
        e.consume()
        runDescription.fireEvent(
          KeyEvent(KeyEvent.KEY_PRESSED, "", "", KeyCode.TAB, e.isShiftDown, true, false, false)
        )
  )
  private val descriptionText = TextArea()
  descriptionText.setAccessibleText(
    eyes4s.studio.app.text.RecipeText(eyes4s.studio.app.text.RecipeTextId.Description)
  )
  descriptionText.getStyleClass.add("studio-field")
  descriptionText.setWrapText(true)
  descriptionText.setFocusTraversable(true)
  descriptionText.textProperty.bindBidirectional(runDescription.textProperty)
  descriptionText.disableProperty.bind(runDescription.disableProperty)
  descriptionText.addEventFilter(
    KeyEvent.KEY_PRESSED,
    (e: KeyEvent) =>
      if e.getCode == KeyCode.TAB && !e.isControlDown && !e.isAltDown && !e.isMetaDown then
        e.consume()
        descriptionText.fireEvent(
          KeyEvent(KeyEvent.KEY_PRESSED, "", "", KeyCode.TAB, e.isShiftDown, true, false, false)
        )
  )
  private val descriptionReason = label(
    eyes4s.studio.app.text.RecipeText(eyes4s.studio.app.text.RecipeTextId.DescriptionSave),
    "t11"
  )
  DisabledReason.attach(descriptionText, descriptionReason)
  val descriptionNode = VBox(8, descriptionText, descriptionReason)
  descriptionNode.setPadding(javafx.geometry.Insets(10, 16, 10, 16))
  def descriptionStops: Vector[FocusStop] = Option
    .when(!descriptionText.isDisabled)(
      FocusStop(A11yRole.TextArea, descriptionText.getAccessibleText)
    )
    .toVector
  private val nameError = label("", "t11", "preflight-note")
  private var metadataFor: Option[eyes4s.studio.core.backend.AnalysisRevision] = None
  private var bindingMetadata                                                  = false
  private def metadata                                                         = RevisionName
    .of(runName.getText)
    .map(n =>
      val preset = eyes4s.studio.app.analysis.AnalysisSelection
        .selected(model())
        .map(c => eyes4s.studio.core.preset.RecipePresets.resolve(c.studio.preset, c.recipe))
        .getOrElse(Preset.Custom)
      StudioFields(preset, n, runDescription.getText)
    )
  runName.textProperty.addListener((_, _, _) =>
    if !bindingMetadata then
      run.setDisable(vm.card.run.isEmpty || metadata.isLeft || editing.nonEmpty)
      nameError.setText(metadata.fold(_.message, _ => ""))
  )
  private val verdict = label("", "t11", "preflight-note")
  // Disabled, Save & run takes no focus: the name's error and the verdict
  // reach assistive technology as its help.
  DisabledReason.attach(run, nameError, verdict)

  private def heading(text: String, side: javafx.scene.Node): HBox =
    val spacer = Region()
    HBox.setHgrow(spacer, Priority.ALWAYS)
    val h = HBox(6.0, label(text, "t13"), spacer, side)
    h.setAlignment(Pos.CENTER_LEFT)
    h

  private val studioHeading =
    heading(
      DiagnosticText(DiagnosticTextId.StudioSection),
      label(DiagnosticText(DiagnosticTextId.StudioRuleNote), "kind")
    )
  private val eyesHeading =
    heading(PreflightText(PreflightTextId.Eyes4sHeading), report)
  // The golden's Run section: the analysis name first, then its lines and
  // Save and run; the notes are the recipe's Notes tab (overhaul v4).
  private val notesLabel =
    label(eyes4s.studio.app.text.RecipeText(eyes4s.studio.app.text.RecipeTextId.Notes), "t11")
  Vector(notesLabel, runDescription).foreach { n => n.setVisible(false); n.setManaged(false) }
  runName.getStyleClass.add("studio-field")
  private val card = VBox(
    8.0,
    label(PreflightText(PreflightTextId.RunHeading), "t13", "preflight-section"),
    label(
      eyes4s.studio.app.text.RecipeText(eyes4s.studio.app.text.RecipeTextId.Name),
      "t11",
      "preflight-key"
    ),
    runName,
    nameError,
    lines,
    notesLabel,
    runDescription,
    run,
    verdict
  )
  card.getStyleClass.add("preflight-run-section")

  /** The pane header's control: Ready, or Blocked (overhaul v4). */
  private val readiness = label("", "t12", "preflight-readiness")
  readiness.setMinWidth(Region.USE_PREF_SIZE)
  readiness.setWrapText(false)
  val headerControls: Vector[javafx.scene.Node] = Vector(readiness)
  private val counts                            = label("", "t11", "preflight-note")

  // The golden's rail: the counts, the findings, what is not checked, then
  // Run; the studio's own section only when it has findings.
  eyesHeading.setVisible(false)
  eyesHeading.setManaged(false)
  private val checks =
    VBox(8.0, status, counts, studioHeading, studio, eyesHeading, eyes, notChecked)
  checks.getStyleClass.add("preflight-checks")
  private val content = VBox(0.0, checks, card)

  /** The pane's content. */
  val node: ScrollPane = ScrollPane(content)
  node.setFitToWidth(true)
  node.setFocusTraversable(false)
  node.getStyleClass.add("preflight")
  Option(getClass.getClassLoader.getResource(TableTwinView.stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** The view-model now shown. */
  def vm: PreflightVM = Preflight.vm(design, model())

  /** Save & run as shown: its label, whether it is enabled, and why not. */
  def runButton: (String, Boolean, String) = (run.getText, !run.isDisabled, verdict.getText)

  /** The findings shown, by section: the studio's, then eyes4s's. */
  def shown: (Vector[FindingVM], Vector[FindingVM]) =
    import scala.jdk.CollectionConverters.*
    def findings(box: VBox) = box.getChildren.asScala.toVector.flatMap {
      case v: VBox =>
        v.getUserData match
          case f: FindingVM => Vector(f)
          case _            => Vector.empty
      case _ => Vector.empty
    }
    (findings(studio), findings(eyes))

  /** Clicks the remedy of the finding `finding`, as the user does. */
  def remedy(finding: FindingVM): Unit =
    import scala.jdk.CollectionConverters.*
    (studio.getChildren.asScala ++ eyes.getChildren.asScala)
      .collectFirst {
        case v: VBox if v.getUserData == finding =>
          v.getChildren.asScala.collectFirst { case b: Button => b }
      }
      .flatten
      .foreach(_.fire())

  /** Clicks Save & run, as the user does. */
  def pressRun(): Unit = run.fire()

  /** The controls inside the pane's own stop: each remedy, then Save & run
    * when it is enabled.
    */
  def focusStops: Vector[FocusStop] =
    val v        = vm
    val remedies = v.findings.toVector.flatMap(f => (f.studio ++ f.eyes4s).flatMap(_.remedy))
    remedies.map(r => FocusStop(A11yRole.Button, r.label)) ++
      (if runName.isDisabled then Vector.empty
       else
         Vector(
           FocusStop(
             A11yRole.TextField,
             eyes4s.studio.app.text.RecipeText(eyes4s.studio.app.text.RecipeTextId.Name)
           )
         )) ++
      Option.when(!run.isDisabled)(FocusStop(A11yRole.Button, v.card.button))

  /** Follows the resolved design's state. */
  def follow(state: ResolvedDesign): Unit =
    design = state
    render()

  /** Follows the model: the selection the remedies submit against. */
  def sync(m: AppModel): Unit =
    selection = selection.map(_.project(m.selection)._1)
    render()

  private def findingNode(f: FindingVM): VBox =
    val blocker = f.severity == FindingSeverity.Blocker
    val title   = label(f.title, "preflight-title", "t12")
    title.setGraphic(
      eyes4s.studio.desktop.icons.StudioIcons
        .graphic(
          eyes4s.studio.app.icons.Icon.Warning,
          eyes4s.studio.desktop.icons.IconSize.Px16
        )
    )
    title.setGraphicTextGap(6.0)
    // The severity is read with the title; the card's colour shows it.
    title.setAccessibleText(s"${f.severity.label}: ${f.title}")
    val head = HBox(6.0, title)
    head.setAlignment(Pos.CENTER_LEFT)
    val box = VBox(3.0, head, label(f.detail, "t11", "preflight-note"))
    f.remedy.foreach { r =>
      val b = Button(r.label)
      b.getStyleClass.addAll("studio-button", "preflight-remedy")
      b.setAccessibleText(r.label)
      b.setOnAction { _ =>
        selection.foreach { s =>
          val (next, intents) = DiagnosticsPresenter.open(r, s)
          selection = Some(next)
          intents.foreach(app)
        }
      }
      box.getChildren.add(b)
    }
    box.getStyleClass.addAll(
      "preflight-finding",
      if blocker then "preflight-finding-blocker" else "preflight-finding-warning"
    )
    box.setUserData(f)
    box

  private var rendered: Option[PreflightVM] = None

  private def render(): Unit =
    val v = vm
    status.setText(v.status.getOrElse(""))
    status.setVisible(v.status.isDefined)
    status.setManaged(v.status.isDefined)
    report.setText(v.report)
    val f = v.findings
    if !rendered.exists(_.findings == f) then
      studio.getChildren.setAll(
        f.fold(Vector.empty[javafx.scene.Node])(x =>
          if x.studio.isEmpty then
            Vector(
              label(PreflightText(PreflightTextId.NoStudioChecks), "t11", "preflight-note")
            )
          else x.studio.map(findingNode)
        )*
      )
      eyes.getChildren.setAll(
        f.fold(Vector.empty[javafx.scene.Node])(_.eyes4s.map(findingNode))*
      ): Unit
    notChecked.setText(v.notChecked)
    val hasStudio = f.exists(_.studio.nonEmpty)
    Vector(studioHeading, studio).foreach { n =>
      n.setVisible(hasStudio); n.setManaged(hasStudio)
    }
    counts.setText(f.fold("") { x =>
      def n(k: Int, one: PreflightTextId, many: PreflightTextId) =
        if k == 1 then PreflightText(one) else PreflightText(many, k.toString)
      PreflightText(
        PreflightTextId.Counts,
        n(x.blockers, PreflightTextId.OneBlocker, PreflightTextId.Blockers),
        n(x.warnings, PreflightTextId.OneWarning, PreflightTextId.Warnings)
      )
    })
    counts.setVisible(f.isDefined); counts.setManaged(f.isDefined)
    if !rendered.exists(_.card.lines == v.card.lines) then
      lines.getChildren.clear()
      v.card.lines.zipWithIndex.foreach { (l, i) =>
        lines.add(label(l.label, "t12", "preflight-key"), 0, i)
        lines.add(label(l.value, "t12"), 1, i)
      }
    run.setText(v.card.button)
    run.setAccessibleText(v.card.button)
    run.setDisable(!v.card.enabled)
    val context = eyes4s.studio.app.analysis.AnalysisSelection.selected(model())
    val id      = context.map(_.id)
    if id != metadataFor then
      metadataFor = id
      bindingMetadata = true
      try
        runName.setText(context.fold("")(_.studio.name.value))
        runDescription.setText(context.fold("")(_.studio.description))
      finally bindingMetadata = false
    val editable = context.exists(c => model().document.draft.forall(_.id == c.id))
    runName.setDisable(!editable)
    runDescription.setDisable(!editable)
    run.setDisable(v.card.run.isEmpty || metadata.isLeft || editing.nonEmpty)
    nameError.setText(metadata.fold(_.message, _ => ""))
    run.setOnAction(_ =>
      if editing.isEmpty then
        metadata.foreach { fields =>
          v.card.run.foreach {
            case Intent.Dispatch(Command.SaveAndRun(_)) =>
              app(Intent.Dispatch(Command.SaveAndRun(Some(fields))))
            case intent => app(intent)
          }
        }
    )
    verdict.setText(editing.orElse(v.card.reason).getOrElse(v.card.verdict))
    readiness.setText(
      PreflightText(if run.isDisabled then PreflightTextId.BlockedShort
      else PreflightTextId.Ready)
    )
    readiness.getStyleClass.removeAll("preflight-ready", "preflight-blocked")
    readiness.getStyleClass.add(if run.isDisabled then "preflight-blocked"
    else "preflight-ready")
    rendered = Some(v)

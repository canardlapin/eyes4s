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
import eyes4s.studio.app.maps.LimitsScope
import eyes4s.studio.app.vm.FocusStop
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{DatasetRevision, LedgerPages}
import eyes4s.studio.core.document.StageAppearance
import eyes4s.studio.desktop.plot.TableTwinView
import eyes4s.studio.desktop.runtime.StudioSession
import javafx.application.Platform
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label, RadioButton, ScrollPane, Slider, ToggleGroup}
import javafx.scene.layout.{GridPane, HBox, Priority, Region, VBox}

/** Where Compare's inspector reads a dataset's admission ledger. `done` may
  * be called on any thread.
  */
trait LedgerSource:
  def ledger(dataset: DatasetRevision, done: LedgerAnswer => Unit): Unit

object LedgerSource:
  /** The window's backend: every page of the ledger. */
  def of(session: StudioSession): LedgerSource = (dataset, done) =>
    session.run(LedgerPages.all(session.backend.ledger(dataset, _))) {
      case Left(e) => done(LedgerAnswer.Failed(Option(e.getMessage).getOrElse(e.toString)))
      case Right(Left(err)) => done(LedgerAnswer.Failed(err.message))
      case Right(Right(es)) => done(LedgerAnswer.Answered(es))
    }

/** Compare's inspector on the desktop (ticket S8.4; Main.dc.html, inspector;
  * see [[WhyReference]]): why this reference, the analysis and reporting the
  * run was made and read with, and the appearance controls. It reads the
  * Compare views through `inputs`, follows the model, and only binds
  * [[CompareInspectorVM]]; Edit and the appearance controls send the
  * view-model's intents. Use on the JavaFX thread.
  */
final class CompareInspectorHost(
    model: () => AppModel,
    app: Intent => Unit,
    source: LedgerSource,
    inputs: () => InspectorInputs
):
  private var state    = WhyReference.empty
  private var disposed = false
  // True while render sets the controls, so their listeners do not echo.
  private var binding = false

  private def label(styles: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(styles*)
    l

  private def grid(): GridPane =
    val g = GridPane()
    g.setHgap(8.0)
    g.setVgap(5.0)
    g.getStyleClass.add("kv")
    g

  private def head(title: Label, kind: Label): HBox =
    val spacer = Region()
    HBox.setHgrow(spacer, Priority.ALWAYS)
    val h = HBox(8.0, title, spacer, kind)
    h.setAlignment(Pos.CENTER_LEFT)
    h

  private def section(children: javafx.scene.Node*): VBox =
    val box = VBox(8.0, children*)
    box.getStyleClass.add("sect")
    box

  private val title       = label("t13")
  private val status      = label("t12", "inspector-status")
  private val explanation = label("t12")
  explanation.setWrapText(true)
  private val why = grid()

  private val analysisTitle = label("t13")
  private val analysisKind  = label("kind")
  private val analysis      = grid()
  private val edit          = Button()
  edit.getStyleClass.add("btn")
  edit.setOnAction(_ => app(vm.edit._2))

  private val reportingTitle = label("t13")
  private val reportingKind  = label("kind")
  private val reporting      = grid()

  private val appearanceTitle = label("t13")
  private val appearanceKind  = label("kind")
  private val stageLabel      = label("t12")
  private val limitsLabel     = label("t12")
  private val opacityLabel    = label("t12")
  private val opacityValue    = label("mono", "t11")

  private val stageGroup                                     = ToggleGroup()
  private val stages: Vector[(StageAppearance, RadioButton)] =
    StageAppearance.values.toVector.map { st =>
      val b = RadioButton()
      b.getStyleClass.add("t12")
      b.setToggleGroup(stageGroup)
      b.setOnAction(_ => if !binding then app(WhyReference.stageIntent(st)))
      st -> b
    }
  private val limitsGroup                                = ToggleGroup()
  private val limits: Vector[(LimitsScope, RadioButton)] =
    LimitsScope.values.toVector.map { l =>
      val b = RadioButton()
      b.getStyleClass.add("t12")
      b.setToggleGroup(limitsGroup)
      b.setOnAction(_ => if !binding then dispatch(WhyIntent.ChooseLimits(l)))
      l -> b
    }
  private val opacity = Slider(0.0, 1.0, 0.6)
  opacity.setPrefWidth(120.0)
  // A drag is dispatched once, on release; a click or a key step at once.
  opacity.valueChangingProperty.addListener((_, _, changing) => if !changing then commit())
  opacity.valueProperty.addListener((_, _, _) => if !opacity.isValueChanging then commit())

  private def commit(): Unit =
    if !binding && !disposed then
      WhyReference
        .opacityIntent(opacity.getValue, model().document.presentation.mapOpacity.value)
        .foreach(app)

  private def row(name: Label, control: javafx.scene.Node*): HBox =
    val spacer = Region()
    HBox.setHgrow(spacer, Priority.ALWAYS)
    val h = HBox(6.0, (name +: spacer +: control)*)
    h.setAlignment(Pos.CENTER_LEFT)
    h

  private val retry = Button()
  retry.getStyleClass.add("btn")
  retry.setOnAction(_ => dispatch(WhyIntent.Retry))

  private val content = VBox(
    VBox(8.0, title, status, explanation, why, retry),
    section(head(analysisTitle, analysisKind), analysis, edit),
    section(head(reportingTitle, reportingKind), reporting),
    section(
      head(appearanceTitle, appearanceKind),
      row(stageLabel, stages.map(_._2)*),
      row(limitsLabel, limits.map(_._2)*),
      row(opacityLabel, opacity, opacityValue)
    )
  )
  content.setPadding(javafx.geometry.Insets(12, 14, 12, 14))
  content.setSpacing(10.0)

  /** The pane's content. */
  val node: ScrollPane = ScrollPane(content)
  node.setFitToWidth(true)
  node.setFocusTraversable(false)
  node.getStyleClass.addAll("fixation-inspector", "compare-inspector")
  Option(getClass.getClassLoader.getResource(TableTwinView.stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** The view-model now. */
  def vm: CompareInspectorVM = WhyReference.vm(state, model().document, inputs())

  /** The controls inside the pane's own stop, in Tab order. */
  def focusStops: Vector[FocusStop] = WhyReference.focusStops(vm)

  /** The explanation as shown, and the facts of each section, as text. */
  def explanationText: String         = explanation.getText
  def facts: Vector[(String, String)] =
    import scala.jdk.CollectionConverters.*
    Vector(why, analysis, reporting).flatMap(g =>
      g.getChildren.asScala.toVector.collect { case l: Label => l.getText }.grouped(2).collect {
        case Vector(a, b) => (a, b)
      }
    )

  /** The appearance as shown: the chosen stage and limits, and the opacity. */
  def chosen: (Option[StageAppearance], Option[LimitsScope], Double) =
    (
      stages.collectFirst { case (s, b) if b.isSelected => s },
      limits.collectFirst { case (l, b) if b.isSelected => l },
      opacity.getValue
    )

  /** The user's gestures: a stage, a limit scope, an opacity, Edit. */
  def chooseStage(s: StageAppearance): Unit = stages.find(_._1 == s).foreach(_._2.fire())
  def chooseLimits(l: LimitsScope): Unit    = limits.find(_._1 == l).foreach(_._2.fire())
  def setOpacity(v: Double): Unit           = opacity.setValue(v)
  def pressEdit(): Unit                     = edit.fire()

  /** Follows the Compare views; the summary host calls it after each render,
    * since the shown run's rows arrive after the model changed.
    */
  def refresh(): Unit = sync(model())

  def sync(m: AppModel): Unit = if !disposed then
    val (next, effects) = WhyReference.sync(state, WhyReference.datasetOf(m, inputs().shown))
    state = next
    perform(effects)
    render()

  def dispatch(intent: WhyIntent): Unit = if !disposed then
    val (next, effects) = WhyReference.update(state, intent)
    state = next
    perform(effects)
    render()

  private def perform(effects: Vector[WhyEffect]): Unit =
    effects.foreach { case WhyEffect.ReadLedger(d) =>
      source.ledger(d, a => Platform.runLater(() => dispatch(WhyIntent.LedgerRead(d, a))))
    }

  private def fill(g: GridPane, facts: Vector[InspectorFact]): Unit =
    g.getChildren.clear()
    facts.zipWithIndex.foreach { (f, i) =>
      val k = label("inspector-key"); k.setText(f.label)
      val v = label(if f.ref.isDefined then "mono" else "t12"); v.setText(f.value)
      v.setWrapText(true)
      g.add(k, 0, i); g.add(v, 1, i)
    }

  /** Draws the view-model now. */
  def render(): Unit = if !disposed then
    val v = vm
    binding = true
    try
      title.setText(v.title)
      status.setText(v.status.getOrElse(""))
      status.setVisible(v.status.isDefined); status.setManaged(v.status.isDefined)
      explanation.setText(v.why.fold("")(_.explanation))
      fill(why, v.why.fold(Vector.empty)(_.facts))
      retry.setText(v.retry.getOrElse(""))
      retry.setAccessibleText(v.retry.orNull)
      retry.setVisible(v.retry.isDefined); retry.setManaged(v.retry.isDefined)
      analysisTitle.setText(v.analysis.fold("")(_.title))
      analysisKind.setText(v.analysis.fold("")(_.kind))
      fill(analysis, v.analysis.fold(Vector.empty)(_.facts))
      edit.setText(v.edit._1); edit.setAccessibleText(v.edit._1)
      reportingTitle.setText(v.reporting.title)
      reportingKind.setText(v.reporting.kind)
      fill(reporting, v.reporting.facts)
      val a = v.appearance
      appearanceTitle.setText(a.title)
      appearanceKind.setText(a.kind)
      stageLabel.setText(a.stageLabel)
      limitsLabel.setText(a.limitsLabel)
      opacityLabel.setText(a.opacityLabel)
      opacityValue.setText(a.opacityText)
      def bind[A](pairs: Vector[(A, RadioButton)], segs: Vector[SegmentVM[A]], name: String) =
        pairs.foreach { (value, b) =>
          segs.find(_.value == value).foreach { seg =>
            b.setText(seg.label)
            b.setAccessibleText(s"$name: ${seg.label}")
            b.setSelected(seg.chosen)
          }
        }
      bind(stages, a.stages, a.stageLabel)
      bind(limits, a.limits, a.limitsLabel)
      if !opacity.isValueChanging then opacity.setValue(a.opacity)
      opacity.setAccessibleText(a.opacityLabel)
    finally binding = false

  /** The host ignores the model from then on. Idempotent. */
  def dispose(): Unit = disposed = true

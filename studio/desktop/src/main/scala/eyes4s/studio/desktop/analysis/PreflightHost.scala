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
import eyes4s.studio.core.selection.ViewId
import eyes4s.studio.desktop.plot.TableTwinView
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label, ScrollPane}
import javafx.scene.layout.{GridPane, HBox, Priority, Region, VBox}

/** The Analysis perspective's preflight pane and run card on the desktop
  * (ticket S7.6; Analysis.dc.html, preflight; see [[Preflight]]): the studio's
  * checks and eyes4s's findings in separate sections, each finding with its
  * severity, title, detail and remedy; what is not checked; and the run card
  * with Save & run, enabled or disabled with its reason. It only binds
  * [[PreflightVM]]; a remedy opens its trials, Save & run dispatches. Use on
  * the JavaFX thread.
  */
final class PreflightHost(model: () => AppModel, app: Intent => Unit):
  private var design = ResolvedDesign.empty
  private val viewId =
    ViewId.of("analysis.preflight").fold(e => throw IllegalStateException(e.message), identity)
  private var selection = ViewSelection.initial(viewId, model().selection)

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
  private val run = Button()
  run.getStyleClass.add("preflight-run")
  run.setMaxWidth(Double.MaxValue)
  private val verdict = label("", "t11", "preflight-note")

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
  private val card = VBox(
    6.0,
    heading(
      PreflightText(PreflightTextId.RunHeading),
      label(PreflightText(PreflightTextId.RunKind), "kind")
    ),
    lines,
    run,
    verdict
  )
  card.getStyleClass.add("find")

  private val content = VBox(
    8.0,
    status,
    studioHeading,
    studio,
    eyesHeading,
    eyes,
    VBox(
      2.0,
      label(PreflightText(PreflightTextId.NotCheckedHeading), "t11", "preflight-key"),
      notChecked
    ),
    card
  )
  content.setPadding(javafx.geometry.Insets(10, 12, 10, 12))

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

  /** The findings shown, by section: (title, severity, remedy label). */
  def shown: (Vector[String], Vector[String]) =
    import scala.jdk.CollectionConverters.*
    def titles(box: VBox) = box.getChildren.asScala.toVector.flatMap {
      case v: VBox =>
        v.getUserData match
          case s: String => Vector(s)
          case _         => Vector.empty
      case _ => Vector.empty
    }
    (titles(studio), titles(eyes))

  /** Clicks the remedy of the finding titled `title`, as the user does. */
  def remedy(title: String): Unit =
    import scala.jdk.CollectionConverters.*
    (studio.getChildren.asScala ++ eyes.getChildren.asScala)
      .collectFirst {
        case v: VBox if v.getUserData == title =>
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
      Option.when(v.card.enabled)(FocusStop(A11yRole.Button, v.card.button))

  /** Follows the resolved design's state. */
  def follow(state: ResolvedDesign): Unit =
    design = state
    render()

  /** Follows the model: the selection the remedies submit against. */
  def sync(m: AppModel): Unit =
    selection = selection.project(m.selection)._1
    render()

  private def findingNode(f: FindingVM): VBox =
    val pill = label(f.severity.label, "pill")
    pill.getStyleClass.add(
      if f.severity == FindingSeverity.Blocker then "pill-blocker" else "pill-warning"
    )
    val head = HBox(6.0, pill, label(f.title, "preflight-title"))
    head.setAlignment(Pos.CENTER_LEFT)
    val box = VBox(3.0, head, label(f.detail, "t11", "preflight-note"))
    f.remedy.foreach { r =>
      val b = Button(r.label)
      b.getStyleClass.add("chip")
      b.setAccessibleText(r.label)
      b.setOnAction { _ =>
        val (next, intents) = DiagnosticsPresenter.open(r, selection)
        selection = next
        intents.foreach(app)
      }
      box.getChildren.add(b)
    }
    box.getStyleClass.add("find")
    box.setUserData(f.title)
    box

  private def render(): Unit =
    val v = vm
    status.setText(v.status.getOrElse(""))
    status.setVisible(v.status.isDefined)
    status.setManaged(v.status.isDefined)
    report.setText(v.report)
    val f = v.findings
    studio.getChildren.setAll(
      f.fold(Vector.empty[javafx.scene.Node])(x =>
        if x.studio.isEmpty then
          Vector(label(PreflightText(PreflightTextId.NoStudioChecks), "t11", "preflight-note"))
        else x.studio.map(findingNode)
      )*
    )
    eyes.getChildren.setAll(f.fold(Vector.empty[javafx.scene.Node])(_.eyes4s.map(findingNode))*)
    notChecked.setText(v.notChecked)
    lines.getChildren.clear()
    v.card.lines.zipWithIndex.foreach { (l, i) =>
      lines.add(label(l.label, "t11", "preflight-key"), 0, i)
      lines.add(label(l.value, "t11", "mono"), 1, i)
    }
    run.setText(v.card.button)
    run.setAccessibleText(v.card.button)
    run.setDisable(!v.card.enabled)
    run.setOnAction(_ => v.card.run.foreach(app))
    verdict.setText(v.card.reason.getOrElse(v.card.verdict))

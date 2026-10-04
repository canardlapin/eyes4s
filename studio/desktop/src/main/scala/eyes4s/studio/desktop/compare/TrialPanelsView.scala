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

import eyes4s.studio.app.Intent
import eyes4s.studio.app.compare.{PanelRole, TrialPanelVM, TrialPanelsVM}
import eyes4s.studio.app.text.{PanelText, PanelTextId}
import eyes4s.studio.desktop.plot.TableTwinView
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label, ToggleButton}
import javafx.scene.layout.{HBox, Priority, Region, StackPane, VBox}

/** Compare's query and reference trial panels on the desktop (ticket S8.2;
  * Main.dc.html, panels). Each panel shows its role pill and title, its
  * toolbar and its readout above the stage: the query panel the underlay
  * toggle and the query's contrast, the reference panel the way back to the
  * matched reference, the matched reference's identity and the inspected
  * pair's score. It only binds [[TrialPanelsVM]]. Use on the JavaFX thread.
  */
final class TrialPanelsView(app: Intent => Unit, underlay: Boolean => Unit):

  private final class Panel(styleClass: String):
    val pill    = Label()
    val title   = Label()
    val readout = Label()
    val note    = Label()
    val tools   = HBox(4.0)
    val stage   = StackPane()
    pill.getStyleClass.add("panel-pill")
    title.getStyleClass.addAll("panel-title", "t12")
    readout.getStyleClass.addAll("panel-readout", "mono", "t11")
    note.getStyleClass.addAll("panel-note", "t11")
    stage.getStyleClass.add("stage")
    VBox.setVgrow(stage, Priority.ALWAYS)
    private val spacer = Region()
    HBox.setHgrow(spacer, Priority.ALWAYS)
    private val head = HBox(6.0, pill, title)
    head.setAlignment(Pos.CENTER_LEFT)
    private val bar = HBox(8.0, tools, spacer, readout)
    bar.setAlignment(Pos.CENTER_LEFT)
    bar.getStyleClass.add("panel-toolbar")
    val node: VBox = VBox(4.0, head, bar, note, stage)
    node.getStyleClass.addAll("trial-panel", styleClass)
    Option(getClass.getClassLoader.getResource(TableTwinView.stylesheetResource))
      .foreach(url => node.getStylesheets.add(url.toExternalForm))

    def show(vm: Option[TrialPanelVM], empty: Option[String]): Unit =
      vm match
        case Some(p) =>
          pill.setText(p.role.label)
          pill.getStyleClass.setAll("panel-pill", pillClass(p.role))
          title.setText(p.title)
          readout.setText(p.readout.text)
          readout.setAccessibleText(p.readout.text)
          node.setAccessibleText(s"${p.role.label}: ${p.title}")
        case None =>
          pill.setText("")
          title.setText(empty.getOrElse(""))
          readout.setText("")
      pill.setVisible(vm.isDefined)
      pill.setManaged(vm.isDefined)

  private def pillClass(role: PanelRole): String = role match
    case PanelRole.Query   => "pill-query"
    case PanelRole.Matched => "pill-matched"
    case PanelRole.Control => "pill-control"

  private val query     = Panel("query-panel")
  private val reference = Panel("reference-panel")

  private val underlayToggle = ToggleButton(PanelText(PanelTextId.Underlay))
  underlayToggle.getStyleClass.add("tog")
  underlayToggle.setAccessibleText(PanelText(PanelTextId.Underlay))
  underlayToggle.setOnAction(_ => underlay(underlayToggle.isSelected))
  query.tools.getChildren.add(underlayToggle)

  private val back = Button()
  back.getStyleClass.add("tog")
  reference.tools.getChildren.add(back)

  /** The query panel's content. */
  def queryNode: VBox = query.node

  /** The reference panel's content. */
  def referenceNode: VBox = reference.node

  /** The stages, where the trial views go. */
  def queryStage: StackPane     = query.stage
  def referenceStage: StackPane = reference.stage

  /** What each panel shows: role pill, title and readout. */
  def shown: Vector[(String, String, String)] =
    Vector(query, reference).map(p => (p.pill.getText, p.title.getText, p.readout.getText))

  /** The way back to the matched reference, if it is offered. */
  def backOffered: Option[String] = Option.when(back.isVisible)(back.getText)

  /** The matched reference the reference panel names. */
  def matchedNote: String = reference.note.getText

  /** Clicks the way back, as the user does. */
  def pressBack(): Unit = back.fire()

  def render(vm: TrialPanelsVM): Unit =
    query.show(vm.query, vm.empty)
    reference.show(vm.reference, None)
    underlayToggle.setSelected(vm.underlay)
    underlayToggle.setVisible(vm.query.isDefined)
    underlayToggle.setManaged(vm.query.isDefined)
    val extras = vm.extras
    reference.note.setText(extras.fold("")(_.matched))
    extras.flatMap(_.back) match
      case Some((label, intent)) =>
        back.setText(label)
        back.setAccessibleText(label)
        back.setOnAction(_ => app(intent))
        back.setVisible(true)
        back.setManaged(true)
      case None =>
        back.setVisible(false)
        back.setManaged(false)

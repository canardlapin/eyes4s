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
import eyes4s.studio.app.compare.{ContrastVM, ReadoutValue}
import eyes4s.studio.app.text.{ContrastText, ContrastTextId}
import eyes4s.studio.desktop.plot.TableTwinView
import javafx.geometry.Pos
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{GridPane, HBox, Priority, Region, VBox}

/** The contrast pane's readout (ticket S8.3; Main.dc.html, contrast): the
  * caption, the hero D, M and B, the confound sentence, and the inspected
  * pair with Prev and Next. It only binds [[ContrastVM]]; Prev and Next send
  * their intents. Use on the JavaFX thread.
  */
final class ContrastReadoutView(app: Intent => Unit):

  private def label(style: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(style*)
    l

  private val status   = label("t12", "contrast-status")
  private val caption  = label("lbl")
  private val hero     = label("t28", "mono")
  private val heroUnit = label("contrast-unit")
  private val values   = GridPane()
  private val confound = label("t11", "contrast-confound")
  confound.setWrapText(true)
  confound.setMinHeight(Region.USE_PREF_SIZE)
  values.setHgap(8.0)
  values.setVgap(2.0)

  private val inspectedHead  = label("lbl")
  private val inspectedRole  = label("contrast-role")
  private val inspectedScore = label("mono", "t16")
  private val inspectedNote  = label("t11", "contrast-note")
  inspectedNote.setWrapText(true)
  inspectedNote.setMinHeight(Region.USE_PREF_SIZE)
  private val rank = label("t11", "mono", "contrast-rank")
  private val prev = Button(ContrastText(ContrastTextId.Prev))
  private val next = Button(ContrastText(ContrastTextId.Next))
  prev.setAccessibleText(ContrastText(ContrastTextId.PrevName))
  next.setAccessibleText(ContrastText(ContrastTextId.NextName))
  prev.getStyleClass.add("btn")
  next.getStyleClass.add("btn")
  Vector(prev, next).foreach(_.setMinWidth(Region.USE_PREF_SIZE))

  private val spacer = Region()
  HBox.setHgrow(spacer, Priority.ALWAYS)
  private val heroRow = HBox(8.0, hero, heroUnit)
  heroRow.setAlignment(Pos.BASELINE_LEFT)
  private val roleRow = HBox(8.0, inspectedRole, spacer, inspectedScore)
  roleRow.setAlignment(Pos.CENTER_LEFT)
  private val stepRow = HBox(4.0, prev, next)
  stepRow.setAlignment(Pos.CENTER_LEFT)
  private val inspected = VBox(3.0, inspectedHead, roleRow, inspectedNote, stepRow, rank)
  inspected.getStyleClass.add("contrast-inspected")

  /** The readout's content. */
  val node: VBox = VBox(6.0, status, caption, heroRow, values, confound, inspected)
  node.getStyleClass.add("contrast-readout")
  node.setPrefWidth(250.0)
  node.setMinWidth(250.0)
  Option(getClass.getClassLoader.getResource(TableTwinView.stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** The readout's lines as shown: the caption, the hero, each value line,
    * the confound sentence, and the inspected pair's heading, score, note
    * and rank.
    */
  def lines: Vector[String] =
    import scala.jdk.CollectionConverters.*
    Vector(caption.getText, s"${hero.getText} ${heroUnit.getText}") ++
      values.getChildren.asScala.toVector
        .collect { case l: Label => l.getText }
        .grouped(2)
        .map(_.mkString(" "))
        .toVector ++
      Vector(confound.getText) ++
      (if inspected.isVisible then
         Vector(
           inspectedRole.getText,
           inspectedScore.getText,
           inspectedNote.getText,
           rank.getText
         )
       else Vector.empty)

  /** The status, if one is shown. */
  def statusText: Option[String] = Option.when(status.isVisible)(status.getText)

  /** Prev and Next, as the user clicks them; whether each is enabled. */
  def pressPrev(): Unit         = prev.fire()
  def pressNext(): Unit         = next.fire()
  def steps: (Boolean, Boolean) = (!prev.isDisabled, !next.isDisabled)

  def render(vm: ContrastVM): Unit =
    status.setText(vm.status.getOrElse(""))
    status.setVisible(vm.status.isDefined)
    status.setManaged(vm.status.isDefined)
    caption.setText(vm.caption)
    hero.setText(vm.hero.fold("")(_.value))
    heroUnit.setText(vm.hero.fold("")(_.label))
    hero.setAccessibleText(vm.hero.fold("")(h => s"${h.label} ${h.value}"))
    values.getChildren.clear()
    Vector(vm.m, vm.b).flatten.zipWithIndex.foreach { (v, i) => line(v, i) }
    confound.setText(vm.confound)
    vm.inspected match
      case Some(i) =>
        inspectedHead.setText(i.score.label)
        inspectedRole.setText(i.heading)
        inspectedScore.setText(i.score.value)
        inspectedNote.setText(i.note)
        rank.setText(i.rank)
        step(prev, i.prev)
        step(next, i.next)
        inspected.setVisible(true)
        inspected.setManaged(true)
      case None =>
        inspected.setVisible(false)
        inspected.setManaged(false)

  private def line(v: ReadoutValue, row: Int): Unit =
    val name  = label("mono")
    val value = label("mono")
    name.setText(v.label)
    value.setText(v.value)
    values.add(name, 0, row)
    values.add(value, 1, row)

  private def step(b: Button, intent: Option[Intent]): Unit =
    b.setDisable(intent.isEmpty)
    b.setOnAction(_ => intent.foreach(app))

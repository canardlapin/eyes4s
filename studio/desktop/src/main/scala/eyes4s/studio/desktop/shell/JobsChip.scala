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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.Intent
import eyes4s.studio.app.icons.Icon
import eyes4s.studio.app.vm.{JobsChipState, JobsChipVM}
import eyes4s.studio.desktop.icons.{IconSize, StudioIcons}
import javafx.css.PseudoClass
import javafx.scene.control.{Button, Label, ProgressBar}
import javafx.scene.layout.HBox

/** The jobs chip (ticket S1.4): "No jobs"; "Run 8 · Comparing · 21,400 /
  * 44,845 pairs" with a thin progress bar and Cancel; "Run 8 failed · 2
  * diagnostics", which opens the Diagnostics pane. It renders a
  * [[JobsChipVM]] and dispatches the intents the view-model carries.
  */
final class JobsChip(dispatch: Intent => Unit):

  private val parts = HBox()
  parts.getStyleClass.add("jobs-chip-parts")

  /** The chip itself: a button, which opens what the state offers. */
  val chip: Button = Button()
  chip.getStyleClass.addAll("chip", "jobs-chip")
  chip.setGraphic(parts)

  val progress: ProgressBar = ProgressBar()
  progress.getStyleClass.add("jobs-progress")

  val cancel: Button = Button()
  cancel.getStyleClass.addAll("bar-button", "jobs-cancel")
  cancel.setMnemonicParsing(false)

  val node: HBox = HBox(chip, progress, cancel)
  node.getStyleClass.add("jobs")

  private var shown: Option[JobsChipVM] = None

  /** The chip's words as drawn: title · stage · count. */
  def text: String = shown.fold("")(_.text)

  /** The text runs as drawn, for tests: each part's label, in order. */
  def drawnParts: Vector[String] =
    import scala.jdk.CollectionConverters.*
    parts.getChildren.asScala.toVector.collect {
      case l: Label if l.getStyleClass.contains("jobs-part") => l.getText
    }

  def render(vm: JobsChipVM): Unit =
    if !shown.contains(vm) then
      shown = Some(vm)
      JobsChip.states.foreach((s, pc) => node.pseudoClassStateChanged(pc, s == vm.state))
      val icon = vm.state match
        case JobsChipState.Idle   => Icon.Jobs
        case JobsChipState.Failed => Icon.JobsFailed
        case JobsChipState.Ready  => Icon.Check
        case _                    => Icon.JobsRunning
      val runs = (Vector(vm.title -> "jobs-title") ++ vm.stage.map(_ -> "jobs-stage") ++
        vm.count.map(_ -> "jobs-count")).zipWithIndex.flatMap { case ((t, cls), i) =>
        val part = Fx.label(t, "jobs-part", cls)
        if i == 0 then Vector(part) else Vector(Fx.label("·", "sep"), part)
      }
      parts.getChildren.setAll((StudioIcons.graphic(icon, IconSize.Px14) +: runs)*)
      chip.setAccessibleText(vm.accessible)
      chip.setOnAction(_ => vm.open.foreach(dispatch))
      chip.setFocusTraversable(vm.open.isDefined)
      progress.setProgress(vm.progress.getOrElse(0.0))
      show(progress, vm.progress.isDefined)
      vm.action match
        case Some(a) =>
          cancel.setText(a.label)
          cancel.setAccessibleText(a.label)
          cancel.setDisable(!a.enabled)
          cancel.setOnAction(_ => dispatch(a.intent))
          show(cancel, true)
        case None => show(cancel, false)

  private def show(n: javafx.scene.Node, visible: Boolean): Unit =
    n.setVisible(visible)
    n.setManaged(visible)

object JobsChip:
  private val states: Vector[(JobsChipState, PseudoClass)] =
    JobsChipState.values.toVector.map(s =>
      s -> PseudoClass.getPseudoClass(s.toString.toLowerCase)
    )

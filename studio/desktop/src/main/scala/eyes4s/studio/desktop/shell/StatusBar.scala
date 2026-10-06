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
import eyes4s.studio.app.vm.StatusBarVM
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{HBox, Region}

/** The status bar (ticket S1.8), 24 px, always four slots: `● Selected:
  * path` · the context hint · the job (mirroring the jobs chip, with Cancel
  * or Show) · `Saved hh:mm` from the last atomic save. It renders a
  * [[StatusBarVM]]. The job slot's nodes are kept and updated in place, so a
  * focused Cancel keeps its focus while the job's count changes.
  */
final class StatusBar(dispatch: Intent => Unit):

  val node: HBox = HBox()
  node.getStyleClass.addAll("status-bar", "t11")
  Fx.fixHeight(node, StatusBar.HeightPx)

  /** The four slots as drawn, for tests: selected, hint, job, saved. */
  val selected: HBox = HBox()
  val hint: Label    = Fx.label("", "status-hint")
  val job: HBox      = HBox()
  val saved: Label   = Fx.label("", "status-saved")

  private val dot   = Region()
  private val label = Fx.label("", "status-label")
  private val path  = Fx.label("", "mono", "t11", "status-path")

  /** The job slot: its words, its count in Plex Mono, and its button. */
  val jobText: Label    = Fx.label("", "status-job-text")
  val jobCount: Label   = Fx.label("", "mono", "t11", "status-job-count")
  val jobAction: Button = Button()
  jobAction.setMnemonicParsing(false)
  jobAction.getStyleClass.addAll("bar-button", "status-action")

  dot.getStyleClass.add("dot")
  selected.getStyleClass.add("status-selected")
  selected.getChildren.setAll(dot, label, path)
  job.getStyleClass.add("status-job")
  job.getChildren.setAll(jobText, jobCount, jobAction)
  node.getChildren.setAll(selected, Fx.rule(), hint, Fx.spacer(), job, Fx.rule(), saved)

  private def show(n: javafx.scene.Node, visible: Boolean): Unit =
    n.setVisible(visible)
    n.setManaged(visible)

  def render(vm: StatusBarVM): Unit =
    vm.selected match
      case Some(p) =>
        label.setText(vm.selectedLabel)
        path.setText(p)
        show(path, true)
      case None =>
        label.setText(vm.noSelection)
        show(path, false)
    hint.setText(vm.hint)
    jobText.setText(vm.job.text)
    jobCount.setText(vm.job.count.getOrElse(""))
    show(jobCount, vm.job.count.isDefined)
    vm.job.action match
      case Some(a) =>
        jobAction.setText(a.label)
        jobAction.setAccessibleText(a.accessible)
        jobAction.setDisable(!a.enabled)
        jobAction.setOnAction(_ => dispatch(a.intent))
        show(jobAction, true)
      case None => show(jobAction, false)
    saved.setText(vm.saved)

object StatusBar:
  val HeightPx: Double = 24

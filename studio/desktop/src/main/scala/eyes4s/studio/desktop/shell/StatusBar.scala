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
import javafx.scene.control.Label
import javafx.scene.layout.{HBox, Region}

/** The status bar (24 px), always four slots: `● Selected: path` · hint ·
  * job · `Saved hh:mm`. S1.8 refines it; this renders the S1.0
  * [[StatusBarVM]] as it is.
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

  selected.getStyleClass.add("status-selected")
  job.getStyleClass.add("status-job")
  node.getChildren.setAll(selected, Fx.rule(), hint, Fx.spacer(), job, Fx.rule(), saved)

  def render(vm: StatusBarVM): Unit =
    val dot = Region()
    dot.getStyleClass.add("dot")
    vm.selected match
      case Some(path) =>
        selected.getChildren.setAll(
          dot,
          Fx.label(vm.selectedLabel, "status-label"),
          Fx.label(path, "mono", "t11", "status-path")
        )
      case None => selected.getChildren.setAll(dot, Fx.label(vm.noSelection, "status-label"))
    hint.setText(vm.hint)
    job.getChildren.setAll(
      (Vector(Fx.label(vm.job.text, "status-job-text")) ++
        vm.job.count.map(c => Fx.label(c, "mono", "t11", "status-job-count")) ++
        vm.job.action.map(a => Fx.button(a, dispatch, "bar-button", "status-action")))*
    )
    saved.setText(vm.saved)

object StatusBar:
  val HeightPx: Double = 24

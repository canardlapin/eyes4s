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
import javafx.scene.layout.HBox

/** The status bar (ticket S1.8; overhaul v4), 26 px: the context hint, then
  * `Saved hh:mm` from the last atomic save at the right. The selection and
  * the job, which it once repeated, are the panes' and the state button's.
  * It renders a [[StatusBarVM]].
  */
final class StatusBar(dispatch: Intent => Unit):

  val node: HBox = HBox()
  node.getStyleClass.addAll("status-bar", "t11")
  Fx.fixHeight(node, StatusBar.HeightPx)

  /** The two slots as drawn, for tests: hint and saved. */
  val hint: Label  = Fx.label("", "status-hint")
  val saved: Label = Fx.label("", "status-saved")

  node.getChildren.setAll(hint, Fx.spacer(), saved)

  // The bar dispatches nothing now; the parameter keeps the shell's wiring.
  locally(dispatch)

  def render(vm: StatusBarVM): Unit =
    hint.setText(vm.hint)
    saved.setText(vm.saved)

object StatusBar:
  /** The v4 golden's status bar. */
  val HeightPx: Double = 26

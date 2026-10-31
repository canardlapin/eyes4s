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

package eyes4s.studio.desktop.explore

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.explore.TrialsNavigator
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.app.text.Messages
import eyes4s.studio.core.document.Perspective
import javafx.scene.control.ToggleButton
import javafx.scene.layout.HBox

/** Data's navigator switch (overhaul v4, `data-trials.html`): Setup, the
  * dataset's sources and checks, or Trials, its participants and trials. A
  * choice navigates; the switch follows the trail.
  */
final class DataSwitch(model: () => AppModel, app: Intent => Unit):
  private def segment(id: eyes4s.studio.app.text.MessageId, trials: Boolean) =
    val b = ToggleButton(Messages.english(id))
    b.setMnemonicParsing(false)
    b.setAccessibleText(b.getText)
    b.getStyleClass.addAll("segment", "t12")
    b.setOnAction { _ =>
      sync(model())
      TrialsNavigator.shown(model()).foreach { d =>
        val trail =
          if trials then Vector(Place.Dataset(d.id), Place.DataView(DataSection.Trials))
          else Vector(Place.Dataset(d.id))
        app(Intent.Navigate(Location(Perspective.Data, trail)))
      }
    }
    b
  private val setup  = segment(eyes4s.studio.app.text.MessageId.DataSetup, trials = false)
  private val trials = segment(eyes4s.studio.app.text.MessageId.SectionTrials, trials = true)

  /** The switch, for a navigator pane's header. */
  val node: HBox = HBox(setup, trials)
  node.getStyleClass.add("segmented-bar")

  /** Its keyboard stops, in Tab order: Setup, then Trials. */
  def focusStops: Vector[eyes4s.studio.app.vm.FocusStop] =
    Vector(setup, trials).map(b =>
      eyes4s.studio.app.vm.FocusStop(eyes4s.studio.app.vm.A11yRole.ToggleButton, b.getText)
    )

  /** Show which half the trail is in. */
  def sync(m: AppModel): Unit =
    val in = StudioLayouts.dataTrialsAt(m.navigation.trail(Perspective.Data))
    setup.setSelected(!in)
    trials.setSelected(in)

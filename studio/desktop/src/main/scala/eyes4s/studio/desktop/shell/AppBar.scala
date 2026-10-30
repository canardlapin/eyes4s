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
import eyes4s.studio.app.vm.{ActionVM, AppBarVM, StateButtonVM}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.desktop.icons.{IconSize, StudioIcons}
import javafx.geometry.Pos
import javafx.scene.control.{MenuButton, MenuItem, ToggleButton, ToggleGroup, Tooltip}
import javafx.scene.layout.{HBox, StackPane}

/** The toolbar (ticket S1.4; overhaul v4), 48 px: the eye mark and the
  * project menu on the left, the study-wide state button on the right, and
  * the perspective switcher centred on the window (a [[StackPane]] over the
  * row, so the switcher reads the same in every perspective).
  */
final class AppBar(dispatch: Intent => Unit):

  private val brand = HBox(StudioIcons.graphic(Icon.EyeAperture, IconSize.Px20))
  brand.getStyleClass.add("brand")

  val project: MenuButton = MenuButton()
  project.getStyleClass.addAll("project-chip")
  project.setMnemonicParsing(false)
  brand.getChildren.add(project)

  /** One group: exactly one perspective is selected, as the model says. */
  val group: ToggleGroup = ToggleGroup()

  val switcher: Map[Perspective, ToggleButton] =
    Perspective.values.toVector.map { p =>
      val t = ToggleButton()
      t.setToggleGroup(group)
      t.setMnemonicParsing(false)
      t.getStyleClass.add("perspective")
      p -> t
    }.toMap

  /** The switcher pill. */
  val perspectives: HBox = HBox(Perspective.values.toVector.map(switcher)*)
  perspectives.getStyleClass.add("perspectives")
  perspectives.setMaxWidth(javafx.scene.layout.Region.USE_PREF_SIZE)

  val state: StateButton = StateButton(dispatch)

  // Three layers in reading order, so Tab goes project, switcher, state:
  // the brand at the left, the switcher centred on the window, the state
  // button at the right.
  brand.setMaxWidth(javafx.scene.layout.Region.USE_PREF_SIZE)
  state.node.setMaxWidth(javafx.scene.layout.Region.USE_PREF_SIZE)
  StackPane.setAlignment(brand, Pos.CENTER_LEFT)
  StackPane.setAlignment(perspectives, Pos.CENTER)
  StackPane.setAlignment(state.node, Pos.CENTER_RIGHT)

  val node: StackPane = StackPane(brand, perspectives, state.node)
  node.getStyleClass.add("app-bar")
  Fx.fixHeight(node, AppBar.HeightPx)

  def render(vm: AppBarVM, projectMenu: Vector[ActionVM], stateVm: StateButtonVM): Unit =
    project.setText(vm.project)
    project.setAccessibleText(vm.projectAccessible)
    project.getItems.setAll(projectMenu.map { a =>
      val item = MenuItem(a.label)
      item.setMnemonicParsing(false)
      item.setDisable(!a.enabled)
      item.setOnAction(_ => dispatch(a.intent))
      item
    }*)
    vm.perspectives.foreach { b =>
      val t = switcher(b.perspective)
      t.setText(b.label)
      t.setAccessibleText(b.accessible)
      if Option(t.getTooltip).forall(_.getText != b.accessible) then
        t.setTooltip(Tooltip(b.accessible))
      t.setSelected(b.selected)
      t.setOnAction(_ => dispatch(b.intent))
    }
    state.render(stateVm)

object AppBar:
  /** The v4 golden's toolbar. */
  val HeightPx: Double = 48

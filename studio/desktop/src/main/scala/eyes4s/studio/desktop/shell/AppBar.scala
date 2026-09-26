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
import eyes4s.studio.app.vm.{ActionVM, AppBarVM}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.desktop.icons.{IconSize, StudioIcons}
import javafx.scene.control.{
  ContentDisplay,
  Label,
  MenuButton,
  MenuItem,
  ToggleButton,
  ToggleGroup
}
import javafx.scene.layout.HBox

/** The app bar (ticket S1.4), 44 px: the eye mark and "Eyes Studio"
  * wordmark (13/600), the project chip and its menu, the perspective
  * switcher (five toggle buttons with their shortcuts, one group), and the
  * jobs chip.
  */
final class AppBar(dispatch: Intent => Unit):

  val wordmark: Label = Fx.label("", "t13", "wordmark")

  private val brand = HBox(StudioIcons.graphic(Icon.EyeAperture, IconSize.Px20), wordmark)
  brand.getStyleClass.add("brand")

  val project: MenuButton = MenuButton()
  project.getStyleClass.addAll("chip", "project-chip")
  project.setMnemonicParsing(false)

  /** One group: exactly one perspective is selected, as the model says. */
  val group: ToggleGroup = ToggleGroup()

  val switcher: Map[Perspective, ToggleButton] =
    Perspective.values.toVector.map { p =>
      val t        = ToggleButton()
      val shortcut = Fx.label("", "mono", "t11", "shortcut")
      t.setGraphic(shortcut)
      t.setContentDisplay(ContentDisplay.RIGHT)
      t.setToggleGroup(group)
      t.setMnemonicParsing(false)
      t.getStyleClass.add("perspective")
      p -> t
    }.toMap

  /** The switcher pill. */
  val perspectives: HBox = HBox(Perspective.values.toVector.map(switcher)*)
  perspectives.getStyleClass.add("perspectives")

  val jobs: JobsChip = JobsChip(dispatch)

  val node: HBox =
    HBox(brand, project, Fx.spacer(), perspectives, Fx.spacer(), jobs.node)
  node.getStyleClass.add("app-bar")
  Fx.fixHeight(node, AppBar.HeightPx)

  def render(vm: AppBarVM, projectMenu: Vector[ActionVM]): Unit =
    wordmark.setText(vm.appName)
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
      t.getGraphic match
        case l: Label => l.setText(b.shortcut)
        case _        => ()
      t.setAccessibleText(b.accessible)
      t.setSelected(b.selected)
      t.setOnAction(_ => dispatch(b.intent))
    }
    jobs.render(vm.jobs)

object AppBar:
  /** DESIGN_SPEC section 3. */
  val HeightPx: Double = 44

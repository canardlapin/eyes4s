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
import eyes4s.studio.app.icons.{AccessibleText, Icon}
import eyes4s.studio.app.vm.ActionVM
import eyes4s.studio.desktop.icons.{IconSize, StudioIcons}
import javafx.css.PseudoClass
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{Priority, Region}

/** Small node builders shared by the shell's bars. Every text they show is a
  * view-model string; every style is a class of `studio-shell.css`.
  */
private[desktop] object Fx:

  val Current: PseudoClass = PseudoClass.getPseudoClass("current")

  def label(text: String, classes: String*): Label =
    val l = Label(text)
    l.getStyleClass.addAll(classes*)
    l

  /** Runs `chosen` whenever `r` becomes selected: by a click, Space, or the
    * arrow keys moving through its toggle group, which select without an
    * action event (S10.5 K3). A view guards its own renders, as for actions.
    */
  def onChosen(r: javafx.scene.control.RadioButton)(chosen: => Unit): Unit =
    r.selectedProperty.addListener((_, was, now) => if now && !was then chosen)

  /** A bar of fixed height: min, preferred and max. */
  def fixHeight(region: Region, px: Double): Unit =
    region.setMinHeight(px)
    region.setPrefHeight(px)
    region.setMaxHeight(px)

  def spacer(): Region =
    val r = Region()
    javafx.scene.layout.HBox.setHgrow(r, Priority.ALWAYS)
    r

  /** A thin vertical rule between status-bar slots. */
  def rule(): Region =
    val r = Region()
    r.getStyleClass.add("bar-rule")
    r

  /** A text button for an action: its label, enablement and intent. */
  def button(action: ActionVM, dispatch: Intent => Unit, classes: String*): Button =
    val b = Button(action.label)
    b.setMnemonicParsing(false)
    b.setAccessibleText(action.label)
    b.getStyleClass.addAll(classes*)
    b.setDisable(!action.enabled)
    b.setOnAction(_ => dispatch(action.intent))
    b

  /** An icon-only button for an action; the label is its accessible name. */
  def iconButton(icon: Icon, action: ActionVM, dispatch: Intent => Unit): Button =
    // A view-model label is never blank; if one were, the button shows its
    // (empty) text rather than an unnamed icon.
    val b = AccessibleText
      .parse(action.label)
      .fold(_ => Button(action.label), StudioIcons.button(icon, _, IconSize.Px14))
    b.setDisable(!action.enabled)
    b.setOnAction(_ => dispatch(action.intent))
    b

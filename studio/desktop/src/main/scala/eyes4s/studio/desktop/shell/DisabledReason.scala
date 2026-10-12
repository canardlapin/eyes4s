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

import javafx.beans.Observable
import javafx.beans.binding.Bindings
import javafx.scene.Node
import javafx.scene.control.Labeled

/** A pending or blocked action and the visible words that say why
  * (DESIGN_SPEC §14, "Disabled and pending"). A disabled JavaFX control
  * takes no focus and shows no tooltip, so its reason is a label drawn next
  * to it. [[attach]] gives assistive technology the same words as the
  * control's accessible help, bound to the labels so that it follows every
  * change of their text, and marks the control `has-reason`, which
  * `studio-shell.css` keeps readable while it is disabled. Only controls
  * with no workflow meaning fade. Use on the JavaFX thread.
  */
private[desktop] object DisabledReason:

  /** The style class of a control whose disabled state has a drawn reason. */
  val StyleClass: String = "has-reason"

  private val Key = "eyes4s.studio.desktop.disabled-reason"

  /** Attaches `reasons`, in reading order, to `control`. Its accessible help
    * is bound from then on; attach once per control.
    */
  def attach(control: Node, reasons: Labeled*): Unit =
    val said = reasons.toVector
    if !control.getStyleClass.contains(StyleClass) then
      control.getStyleClass.add(StyleClass): Unit
    control.getProperties.put(Key, said): Unit
    val inputs = said.flatMap(l => Vector[Observable](l.textProperty, l.visibleProperty))
    control.accessibleHelpProperty.bind(
      Bindings.createStringBinding(() => text(said).orNull, inputs*)
    )

  /** The reason labels attached to `control`, in reading order. */
  def reasons(control: Node): Vector[Labeled] =
    control.getProperties.get(Key) match
      case said: Vector[?] => said.collect { case l: Labeled => l }
      case _               => Vector.empty

  /** What `reasons` say: each visible, non-blank text, one per line. */
  def text(reasons: Vector[Labeled]): Option[String] =
    val lines =
      reasons.filter(_.isVisible).flatMap(l => Option(l.getText)).filter(_.trim.nonEmpty)
    Option.when(lines.nonEmpty)(lines.mkString("\n"))

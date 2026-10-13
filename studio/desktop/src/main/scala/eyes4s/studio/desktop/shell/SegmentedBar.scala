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

import javafx.event.ActionEvent
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.scene.layout.HBox
import javafx.scene.control.{Labeled, ToggleButton}
import javafx.scene.{AccessibleRole, TraversalDirection}

/** A row of mutually exclusive segments (REVIEW ticket 16, slice b;
  * System.dc.html, the `.sg` specimen; `studio-controls.css`): the Compare
  * inspector's stage and colour limits, and any other one-of-few choice.
  *
  * It behaves as an ARIA radio group. The bar is one Tab stop: only the
  * chosen segment (the first while none is) is focus-traversable. Left and
  * Up choose the previous segment, Right and Down the next, wrapping at the
  * ends; Home and End choose the first and the last; Space and Enter choose
  * the focused one. An arrow moves the focus by traversal, so the moved
  * focus is keyboard focus and draws its ring; a click focuses its segment
  * as a button does, with no ring. Each segment's accessible role is
  * [[AccessibleRole.RADIO_BUTTON]], not TOGGLE_BUTTON: a toggle button reads
  * as one independent on/off control and Tab would be expected to visit
  * each, while a radio button is one choice of a group whose selected choice
  * alone is a Tab stop, which is what the bar is, what the board's markup
  * says (`role=radiogroup`, `role=radio`) and what studio-app's focus stops
  * declare (`A11yRole.RadioButton`).
  *
  * The chosen segment is drawn with the selection fill and weight 600
  * (DESIGN_SPEC section 15: selection is a fill, focus is a ring); the bar
  * fades to 0.4 when disabled unless [[attachReason]] gave it a drawn
  * reason. Every word it shows comes from the caller, which takes it from
  * its text catalogue. `chose` runs when a gesture chooses a segment other
  * than the chosen one; [[show]] never runs it. Use on the JavaFX thread.
  */
final class SegmentedBar[A](chose: A => Unit):
  import SegmentedBar.Segment

  private var segments: Vector[Segment[A]]   = Vector.empty
  private var buttons: Vector[SegmentButton] = Vector.empty

  /** The bar. */
  val node: HBox = HBox()
  node.getStyleClass.add(SegmentedBar.StyleClass)
  node.addEventFilter(KeyEvent.KEY_PRESSED, (e: KeyEvent) => key(e))

  /** One segment: a toggle button whose firing chooses it. */
  private final class SegmentButton(val index: Int) extends ToggleButton:
    getStyleClass.setAll(SegmentedBar.SegmentStyleClass)
    setMnemonicParsing(false)
    setAccessibleRole(AccessibleRole.RADIO_BUTTON)
    setFocusTraversable(false)
    // A segment that is not a Tab stop still takes the focus of a click.
    addEventHandler(
      MouseEvent.MOUSE_PRESSED,
      (_: MouseEvent) => if !isFocused then requestFocus()
    )

    /** Space, a click or the program: choose this segment. A chosen segment
      * stays chosen; a toggle button would release it.
      */
    override def fire(): Unit = if !isDisabled then
      choose(index)
      fireEvent(ActionEvent())

  /** Shows `next`, in order, marking the one whose `chosen` holds (the first
    * such, if several do). Reuses the segments while their number holds, so
    * the focus stays where it is.
    */
  def show(next: Vector[Segment[A]]): Unit =
    if next.size != buttons.size then
      buttons = next.indices.toVector.map(SegmentButton(_))
      node.getChildren.setAll(buttons*): Unit
    segments = next
    buttons.zip(next).foreach { (b, s) =>
      b.setText(s.label)
      b.setAccessibleText(s.accessibleText)
    }
    mark(next.indexWhere(_.chosen))

  /** The value of the chosen segment, if one is. */
  def chosen: Option[A] =
    buttons.indexWhere(_.isSelected) match
      case -1 => None
      case i  => segments.lift(i).map(_.value)

  /** The segment nodes, in order. */
  def segmentNodes: Vector[ToggleButton] = buttons

  /** The user's gesture of choosing `value`'s segment, as a click is. */
  def choose(value: A): Unit =
    segments.indexWhere(_.value == value) match
      case -1 => ()
      case i  => buttons(i).fire()

  /** Draws `reasons` next to the disabled bar (DisabledReason): the bar and
    * each segment take them as their accessible help, and the disabled bar
    * stays readable instead of fading. Attach once, after [[show]].
    */
  def attachReason(reasons: Labeled*): Unit =
    DisabledReason.attach(node, reasons*)
    buttons.foreach(DisabledReason.attach(_, reasons*))

  private def choose(i: Int): Unit =
    val was = buttons.indexWhere(_.isSelected)
    mark(i)
    if i != was then segments.lift(i).foreach(s => chose(s.value))

  /** Selects segment `i` alone (none when `i` is -1) and makes it the bar's
    * one Tab stop; with none chosen, the first segment is the stop.
    */
  private def mark(i: Int): Unit =
    val stop = if buttons.indices.contains(i) then i else 0
    buttons.foreach { b =>
      b.setSelected(b.index == i)
      b.setFocusTraversable(b.index == stop)
    }

  private def key(e: KeyEvent): Unit =
    buttons.find(_.isFocused).foreach { at =>
      val last   = buttons.size - 1
      val target = e.getCode match
        case KeyCode.LEFT | KeyCode.UP | KeyCode.KP_LEFT | KeyCode.KP_UP =>
          Some(if at.index == 0 then last else at.index - 1)
        case KeyCode.RIGHT | KeyCode.DOWN | KeyCode.KP_RIGHT | KeyCode.KP_DOWN =>
          Some(if at.index == last then 0 else at.index + 1)
        case KeyCode.HOME  => Some(0)
        case KeyCode.END   => Some(last)
        case KeyCode.ENTER => Some(at.index)
        case _             => None
      target.foreach { i =>
        e.consume()
        if !node.isDisabled then
          buttons(i).fire()
          if i != at.index then
            // By traversal, so the focus arrives as keyboard focus: the new
            // stop is the only traversable segment between `at` and it.
            val direction =
              if i > at.index then TraversalDirection.NEXT else TraversalDirection.PREVIOUS
            if !at.requestFocusTraversal(direction) || !buttons(i).isFocused then
              buttons(i).requestFocus()
      }
    }

object SegmentedBar:

  /** One segment: its value, its drawn label, its accessible name (the
    * group's name and the label, as the caller words it), and whether it is
    * the chosen one.
    */
  final case class Segment[A](value: A, label: String, accessibleText: String, chosen: Boolean)

  /** The bar's style class (`studio-controls.css`). */
  val StyleClass: String = "segmented-bar"

  /** Each segment's style class; it replaces the toggle button's own, so
    * modena draws nothing of it.
    */
  val SegmentStyleClass: String = "segment"

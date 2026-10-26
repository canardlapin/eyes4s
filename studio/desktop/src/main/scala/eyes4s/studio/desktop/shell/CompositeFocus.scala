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

import javafx.collections.ListChangeListener
import javafx.css.PseudoClass
import javafx.scene.{AccessibleAttribute, Node, Parent}
import javafx.scene.control.ListView
import scala.jdk.CollectionConverters.*

/** Composite focus (DESIGN_SPEC section 15): a list, a table or a navigator
  * is one focus stop whose cursor row carries the keyboard focus ring, and
  * the container draws none. A composite with no rows, or none under its
  * cursor yet, has no row to carry the ring, so the container draws it: the
  * focus is never hidden. JavaFX CSS cannot see that (it has no `:empty`
  * for a container), so the composite's host says it with the `no-cursor`
  * pseudo-class, which the stylesheets pair with `:focus-visible`. Use on
  * the JavaFX thread.
  */
private[desktop] object CompositeFocus:

  private val CursorTarget = new Object

  /** A custom composite supplies the actual drawn row that carries its cursor.
    * Standard lists/tables already expose this through FOCUS_ITEM.
    */
  def registerCursor(content: Node)(target: => Option[Node]): Unit =
    content.getProperties.put(CursorTarget, () => target): Unit

  def cursorTarget(node: Node): Option[Node] =
    val custom = Option(node.getProperties.get(CursorTarget))
      .flatMap(_.asInstanceOf[() => Option[Node]]())
    val native = Option(node.queryAccessibleAttribute(AccessibleAttribute.FOCUS_ITEM))
      .collect { case n: Node if n ne node => n }
    (custom orElse native)
      .filter(n =>
        n.getScene != null &&
          Iterator.iterate(n)(_.getParent).takeWhile(_ != null).forall(_.isVisible)
      )
      .orElse(node match
        case parent: Parent =>
          parent.getChildrenUnmodifiable.asScala.iterator
            .filter(_.isVisible)
            .flatMap(cursorTarget)
            .nextOption()
        case _ => None)

  /** Set on a composite with no row under its cursor: none, or no rows. */
  val NoCursor: PseudoClass = PseudoClass.getPseudoClass("no-cursor")

  /** Marks `composite` as having no cursor row, or as having one. */
  def noCursor(composite: Node, none: Boolean): Unit =
    composite.pseudoClassStateChanged(NoCursor, none)

  /** Whether `composite` is marked as having no cursor row. */
  def hasNoCursor(composite: Node): Boolean =
    composite.getPseudoClassStates.contains(NoCursor)

  /** The style class of a pane whose content's rows carry the pane's focus. */
  val PaneStyle: String = "pane-composite"

  private val PaneContentStyle = "pane-composite-content"

  /** Marks `content` as rows that carry the focus of the pane hosting it: the
    * pane, one focus stop, draws no ring of its own while a row is under the
    * cursor; the cursor row draws it. Its `no-cursor` state is the pane's.
    */
  def paneContent(content: Node): Unit =
    if !content.getStyleClass.contains(PaneContentStyle) then
      content.getStyleClass.add(PaneContentStyle): Unit

  /** Whether `content` carries its pane's focus ([[paneContent]]). */
  def isPaneContent(content: Node): Boolean =
    content.getStyleClass.contains(PaneContentStyle)

  /** Keeps `list` marked while no row is under its focus model's cursor (or
    * it has no rows), through every change of its items, of the list it
    * shows and of the cursor.
    */
  def track[A](list: ListView[A]): Unit =
    val changed: ListChangeListener[A] = _ => mark(list)
    list.itemsProperty.addListener { (_, was, now) =>
      Option(was).foreach(_.removeListener(changed))
      Option(now).foreach(_.addListener(changed))
      mark(list)
    }
    Option(list.getItems).foreach(_.addListener(changed))
    Option(list.getFocusModel).foreach(
      _.focusedIndexProperty.addListener((_, _, _) => mark(list))
    )
    mark(list)

  private def mark(list: ListView[?]): Unit =
    val rows   = Option(list.getItems).fold(0)(_.size)
    val cursor = Option(list.getFocusModel).fold(-1)(_.getFocusedIndex)
    noCursor(list, cursor < 0 || cursor >= rows)

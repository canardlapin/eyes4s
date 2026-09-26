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

package eyes4s.studio.desktop.harness

import javafx.event.{Event, EventTarget, EventType}
import javafx.geometry.Point2D
import javafx.scene.{Node, Parent}
import scala.jdk.CollectionConverters.*
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent, PickResult}

/** Keyboard modifiers held during a robot key press. */
final case class Modifiers(
    shift: Boolean = false,
    control: Boolean = false,
    alt: Boolean = false,
    meta: Boolean = false
)

object Modifiers:
  val None: Modifiers = Modifiers()

/** Clicks and keys for FX tests, delivered as synthesized JavaFX events.
  *
  * Events go through each node's normal dispatch chain, so controls react
  * exactly as to user input, but no OS pointer or keyboard is involved: the
  * robot needs no accessibility permission on macOS, is unaffected by window
  * focus under xvfb.
  *
  * A click is hit-tested first: the node must be in this stage's scene, visible
  * and enabled, and picking at its centre must land on it or a descendant, or
  * the click fails the test. Events are then fired at the picked node, as JavaFX
  * would. Picking follows JavaFX's rules for visibility, mouse transparency,
  * `pickOnBounds` and child order; it ignores clips and `viewOrder`.
  * Every action waits for the next layout pass before returning.
  */
final class StudioRobot private[harness] (fx: FxStage):

  /** Presses, releases and clicks `button` at the centre of `node`. */
  def click(node: Node, button: MouseButton = MouseButton.PRIMARY): Unit =
    fx.runOnFx {
      if node.getScene ne fx.scene then
        throw AssertionError(s"cannot click $node: it is not in this test's scene")
      if !StudioRobot.treeVisible(node) then
        throw AssertionError(s"cannot click $node: it or an ancestor is not visible")
      if node.isDisabled then throw AssertionError(s"cannot click $node: it is disabled")
      val bounds   = node.getLayoutBounds
      val local    = Point2D(bounds.getCenterX, bounds.getCenterY)
      val inScene  = node.localToScene(local)
      val onScreen = Option(node.localToScreen(local)).getOrElse(inScene)
      val hit      = StudioRobot.pick(fx.scene.getRoot, inScene)
      val picked   = hit
        .filter(StudioRobot.isSelfOrDescendant(_, node))
        .getOrElse(
          throw AssertionError(
            s"cannot click $node: picking at its centre (${inScene.getX}, ${inScene.getY}) " +
              s"lands on ${hit.fold("nothing")(_.toString)}"
          )
        )
      def mouse(kind: EventType[MouseEvent], down: Boolean, clicks: Int): MouseEvent =
        MouseEvent(
          kind,
          inScene.getX,
          inScene.getY,
          onScreen.getX,
          onScreen.getY,
          button,
          clicks,
          false,
          false,
          false,
          false,
          down && button == MouseButton.PRIMARY,
          down && button == MouseButton.MIDDLE,
          down && button == MouseButton.SECONDARY,
          true,
          false,
          true,
          PickResult(picked, inScene.getX, inScene.getY)
        )
      Event.fireEvent(picked, mouse(MouseEvent.MOUSE_PRESSED, down = true, clicks = 1))
      Event.fireEvent(picked, mouse(MouseEvent.MOUSE_RELEASED, down = false, clicks = 1))
      Event.fireEvent(picked, mouse(MouseEvent.MOUSE_CLICKED, down = false, clicks = 1))
    }
    fx.awaitLayout()

  /** Presses and releases `key` on the focus owner (the scene root if none). */
  def press(key: KeyCode, modifiers: Modifiers = Modifiers.None): Unit =
    fx.runOnFx {
      val target = focusTarget
      Event.fireEvent(target, keyEvent(KeyEvent.KEY_PRESSED, key, modifiers))
      Event.fireEvent(target, keyEvent(KeyEvent.KEY_RELEASED, key, modifiers))
    }
    fx.awaitLayout()

  /** Types `text` into the focus owner, one KEY_TYPED event per character. */
  def typeText(text: String): Unit =
    fx.runOnFx {
      val target = focusTarget
      text.foreach { c =>
        Event.fireEvent(
          target,
          KeyEvent(
            KeyEvent.KEY_TYPED,
            c.toString,
            "",
            KeyCode.UNDEFINED,
            false,
            false,
            false,
            false
          )
        )
      }
    }
    fx.awaitLayout()

  private def focusTarget: EventTarget =
    Option(fx.scene.getFocusOwner).getOrElse(fx.scene.getRoot)

  private def keyEvent(kind: EventType[KeyEvent], key: KeyCode, m: Modifiers): KeyEvent =
    KeyEvent(kind, KeyEvent.CHAR_UNDEFINED, key.getName, key, m.shift, m.control, m.alt, m.meta)

private[harness] object StudioRobot:

  /** The topmost node under `inScene` at or below `node`, by JavaFX's picking rules. */
  def pick(node: Node, inScene: Point2D): Option[Node] =
    if !node.isVisible || node.isMouseTransparent then None
    else
      val fromChildren = node match
        case parent: Parent =>
          parent.getChildrenUnmodifiable.asScala.reverseIterator
            .flatMap(pick(_, inScene))
            .nextOption()
        case _ => None
      fromChildren.orElse {
        val local = node.sceneToLocal(inScene)
        val hit   =
          if node.isPickOnBounds then node.getBoundsInLocal.contains(local)
          else node.contains(local)
        Option.when(hit)(node)
      }

  def isSelfOrDescendant(candidate: Node, ancestor: Node): Boolean =
    ancestors(candidate).contains(ancestor)

  def treeVisible(node: Node): Boolean = ancestors(node).forall(_.isVisible)

  private def ancestors(node: Node): Iterator[Node] =
    Iterator
      .iterate(Option(node))(_.flatMap(n => Option(n.getParent)))
      .takeWhile(_.isDefined)
      .flatten

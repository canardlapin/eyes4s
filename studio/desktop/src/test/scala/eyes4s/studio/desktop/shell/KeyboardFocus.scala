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

import javafx.scene.{Node, TraversalDirection}

/** Keyboard focus as a test reaches it (REVIEW ticket 14). A control draws
  * its focus ring only under `:focus-visible`, which JavaFX sets when
  * keyboard traversal brings the focus and clears on `requestFocus()` (a
  * click or the program). So [[byKeyboard]] arrives by traversal, as Tab
  * does: it focuses the stop before `n` and traverses forward to `n`.
  *
  * JavaFX shows focus only while the window holds the OS focus: Monocle's
  * headless glass always gives it; on a desktop glass that withholds it the
  * test is skipped, not passed. Use on the JavaFX thread.
  */
object KeyboardFocus:

  private val MaxSteps = 256

  /** Moves the keyboard focus to `n` by traversal; `n` must be a Tab stop. */
  def byKeyboard(n: Node): Unit =
    val scene = n.getScene
    munit.Assertions.assume(
      scene != null && scene.getWindow != null && scene.getWindow.isFocused,
      s"the test window does not hold OS focus, so $n cannot show keyboard focus"
    )
    n.requestFocus()
    n.requestFocusTraversal(TraversalDirection.PREVIOUS)
    var steps = 0
    while (scene.getFocusOwner ne n) || !n.isFocusVisible do
      if steps >= MaxSteps then
        throw AssertionError(s"Tab does not reach $n (the focus is on ${scene.getFocusOwner})")
      Option(scene.getFocusOwner).getOrElse(n).requestFocusTraversal(TraversalDirection.NEXT)
      steps += 1

  /** Moves the focus to `n` as a click or the program does: no ring. */
  def byPointer(n: Node): Unit =
    n.requestFocus()
    assert(!n.isFocusVisible, s"$n shows keyboard focus after requestFocus()")

  /** Moves the keyboard focus off `n`: to the next Tab stop, or to the scene
    * root when `n` is the only one.
    */
  def away(n: Node): Unit =
    if n.isFocused then n.requestFocusTraversal(TraversalDirection.NEXT): Unit
    if n.isFocused then n.getScene.getRoot.requestFocus()
    assert(!n.isFocused, s"$n kept the focus")

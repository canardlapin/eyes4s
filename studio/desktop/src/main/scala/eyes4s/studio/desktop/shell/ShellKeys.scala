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

import eyes4s.studio.app.keys.{Key, KeyChord, Modifier}
import javafx.scene.input.{KeyCode, KeyCodeCombination, KeyCombination, KeyEvent}

/** JavaFX key events as the studio's [[KeyChord]]s. The keymap itself is
  * studio-app's (`CommandRegistry.keymap`); this only names keys.
  *
  * ⌘ is the platform's shortcut key: Command on macOS, Control elsewhere
  * (JavaFX's `isShortcutDown`), so ⌘1 is Ctrl+1 on Linux and Windows.
  */
object ShellKeys:

  def key(code: KeyCode): Option[Key] = code match
    case KeyCode.DIGIT1 | KeyCode.NUMPAD1 => Some(Key.Digit1)
    case KeyCode.DIGIT2 | KeyCode.NUMPAD2 => Some(Key.Digit2)
    case KeyCode.DIGIT3 | KeyCode.NUMPAD3 => Some(Key.Digit3)
    case KeyCode.DIGIT4 | KeyCode.NUMPAD4 => Some(Key.Digit4)
    case KeyCode.DIGIT5 | KeyCode.NUMPAD5 => Some(Key.Digit5)
    case KeyCode.Z                        => Some(Key.Z)
    case KeyCode.OPEN_BRACKET             => Some(Key.BracketLeft)
    case KeyCode.CLOSE_BRACKET            => Some(Key.BracketRight)
    case KeyCode.ENTER                    => Some(Key.Enter)
    case KeyCode.ESCAPE                   => Some(Key.Escape)
    case KeyCode.F6                       => Some(Key.F6)
    case KeyCode.TAB                      => Some(Key.Tab)
    case _                                => None

  /** The key code a menu accelerator uses for `key`. */
  def code(key: Key): KeyCode = key match
    case Key.Digit1       => KeyCode.DIGIT1
    case Key.Digit2       => KeyCode.DIGIT2
    case Key.Digit3       => KeyCode.DIGIT3
    case Key.Digit4       => KeyCode.DIGIT4
    case Key.Digit5       => KeyCode.DIGIT5
    case Key.Z            => KeyCode.Z
    case Key.BracketLeft  => KeyCode.OPEN_BRACKET
    case Key.BracketRight => KeyCode.CLOSE_BRACKET
    case Key.Enter        => KeyCode.ENTER
    case Key.Escape       => KeyCode.ESCAPE
    case Key.F6           => KeyCode.F6
    case Key.Tab          => KeyCode.TAB

  /** A chord as a menu accelerator, so the system menu shows it (S1.9). ⌘
    * is the platform's shortcut key, as in [[chord]].
    */
  def combination(chord: KeyChord): KeyCombination =
    val mods = Modifier.values.toVector.filter(chord.modifiers.contains).map {
      case Modifier.Command => KeyCombination.SHORTCUT_DOWN
      case Modifier.Control => KeyCombination.CONTROL_DOWN
      case Modifier.Option  => KeyCombination.ALT_DOWN
      case Modifier.Shift   => KeyCombination.SHIFT_DOWN
    }
    KeyCodeCombination(code(chord.key), mods*)

  /** The chords a key event may stand for, most specific first. Where
    * Control is the shortcut key (Linux, Windows) Ctrl+Tab reads as ⌘⇥ and
    * as ⌃⇥; the first the keymap binds wins.
    */
  def chords(e: KeyEvent): Vector[KeyChord] =
    chord(e).toVector.flatMap { c =>
      val asControl =
        Option.when(e.isControlDown && c.modifiers.contains(Modifier.Command))(
          c.copy(modifiers = c.modifiers - Modifier.Command + Modifier.Control)
        )
      c +: asControl.toVector
    }

  def chord(e: KeyEvent): Option[KeyChord] =
    key(e.getCode).map { k =>
      val mods = Set(
        Option.when(e.isShortcutDown)(Modifier.Command),
        // Control is its own modifier only where it is not the shortcut key.
        Option.when(e.isControlDown && !e.isShortcutDown)(Modifier.Control),
        Option.when(e.isAltDown)(Modifier.Option),
        Option.when(e.isShiftDown)(Modifier.Shift)
      ).flatten
      KeyChord(k, mods)
    }

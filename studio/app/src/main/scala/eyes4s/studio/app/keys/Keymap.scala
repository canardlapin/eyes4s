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

package eyes4s.studio.app.keys

/** A modifier key, in the order the boards print them ("⌘⇧↩"). */
enum Modifier derives CanEqual:
  case Control, Option, Command, Shift

  def symbol: String = this match
    case Control => "⌃"
    case Option  => "⌥"
    case Command => "⌘"
    case Shift   => "⇧"

/** The keys the studio binds. A shell maps its platform key codes onto
  * these; a key outside this set is never an app intent.
  */
enum Key derives CanEqual:
  case Digit1, Digit2, Digit3, Digit4, Digit5, Z, BracketLeft, BracketRight, Enter, Escape, F6

  def symbol: String = this match
    case Digit1       => "1"
    case Digit2       => "2"
    case Digit3       => "3"
    case Digit4       => "4"
    case Digit5       => "5"
    case Z            => "Z"
    case BracketLeft  => "["
    case BracketRight => "]"
    case Enter        => "↩"
    case Escape       => "esc"
    case F6           => "F6"

/** A key with its modifiers. */
final case class KeyChord(key: Key, modifiers: Set[Modifier]) derives CanEqual:

  /** "⌘1", "⌘⇧Z", "F6". */
  def render: String =
    Modifier.values.filter(modifiers.contains).map(_.symbol).mkString + key.symbol

object KeyChord:
  def plain(key: Key): KeyChord        = KeyChord(key, Set.empty)
  def command(key: Key): KeyChord      = KeyChord(key, Set(Modifier.Command))
  def commandShift(key: Key): KeyChord =
    KeyChord(key, Set(Modifier.Command, Modifier.Shift))

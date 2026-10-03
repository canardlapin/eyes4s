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

package eyes4s.studio.app.text

/** The strings of the scale ladder and its table (ticket S4.5b; the Main
  * board's Contrast group). They are kept apart from [[MessageId]] so the
  * ladder adds its own ids without editing the shell's catalogue. Templates
  * name their arguments by position, as [[Messages]] does.
  */
enum LadderTextId derives CanEqual:
  /** The source's caption, the plot's tab title and its accessible summary. */
  case Caption, Title, Summary

  /** The table's column headers. */
  case ScaleHeader, RoleHeader, ReferenceHeader, ItemHeader, CosineHeader, DHeader

  /** What each row is: M, B, D or one control. */
  case Matched, ControlMean, Contrast, Control

  /** The reference cell of B and of D. */
  case MeanOf, Difference

  /** The words of a histogram bar: its scale and its bin's edges. */
  case Bin

  /** The cosine axis's title. */
  case CosineAxis

/** The scale ladder's strings in the boards' wording. */
object LadderText:

  /** The reference English template of `id`. */
  def english(id: LadderTextId): String =
    import LadderTextId.*
    id match
      case Caption         => "{0} contrast"
      case Title           => "Contrast"
      case Summary         =>
        "Scale ladder of {0}: matched, controls, control mean and D at {1} scales"
      case ScaleHeader     => "Scale σ"
      case RoleHeader      => "Role"
      case ReferenceHeader => "Reference"
      case ItemHeader      => "Item"
      case CosineHeader    => "Cosine"
      case DHeader         => "D = M − B"
      case Matched         => "M matched"
      case ControlMean     => "B control mean"
      case Contrast        => "D"
      case Control         => "Control"
      case MeanOf          => "mean of {0} controls"
      case Difference      => "M − B"
      case Bin             => "controls at {0} with cosine from {1} to below {2}"
      case CosineAxis      => "Cosine with each reference"

  /** `id`'s English template with `args` filled in. */
  def apply(id: LadderTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

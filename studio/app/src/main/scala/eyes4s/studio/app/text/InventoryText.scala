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

/** The trial inventory's strings (ticket S5.4; Data.dc.html, sources and
  * ledger), kept apart from the other catalogues so the join adds its own
  * ids. Templates name their arguments by position, as [[Messages]] does.
  */
enum InventoryTextId derives CanEqual:
  case Role, NoFile, Unmapped, Pending, Joined, NotJoined, Refused, Unavailable
  case AbsentLabel, AbsentDefinition, AbsentCount, AbsentUncounted, AbsentUnmapped
  case AbsentPending

/** The inventory's strings in the board's wording. */
object InventoryText:

  /** The reference English template of `id`. */
  def english(id: InventoryTextId): String =
    import InventoryTextId.*
    id match
      case Role             => "Inventory"
      case NoFile           => "No trials.csv"
      case Unmapped         => "columns not mapped"
      case Pending          => "counted on admission"
      case Joined           => "{0} trials"
      case NotJoined        => "not joined"
      case Refused          => "refused by eyes4s"
      case Unavailable      => "{0}"
      case AbsentLabel      => "Absent"
      case AbsentDefinition =>
        "in trials.csv, no fixation records at all"
      case AbsentCount     => "{0}"
      case AbsentUncounted =>
        "Absent trials cannot be counted without a trial inventory (trials.csv)."
      case AbsentUnmapped =>
        "Absent trials cannot be counted until the columns of {0} are mapped."
      case AbsentPending => "Absent trials are counted when {0} is admitted."

  /** `id`'s English template with its arguments filled. */
  def apply(id: InventoryTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

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

package eyes4s.studio.app.importing

import eyes4s.studio.app.text.{Format, InventoryText, InventoryTextId}
import eyes4s.studio.core.backend.{AdmissionSummary, BackendError, InventoryJoin}
import eyes4s.studio.core.document.DatasetRevisionSpec

/** What admission has said about a revision's inventory, as the app holds
  * it: nothing yet, its summary, or the backend's refusal.
  */
enum InventoryAnswer derives CanEqual:
  case NotAsked
  case Summary(summary: AdmissionSummary)
  case Refused(error: BackendError)

/** The inventory's line of the Data sources navigator and its absent count
  * in the ledger (ticket S5.4; Data.dc.html "trials.csv · Inventory · 960
  * trials" and "Absent — in trials.csv, no fixation records at all · 6").
  *
  *  - `source`: the trials file's line, or that there is none.
  *  - `absent`: the absent count when an inventory was joined; otherwise why
  *    it cannot be counted (`absentCount` is then `None`, never 0).
  *  - `issues`: eyes4s's refusal of the inventory, each naming its records,
  *    trial and columns; they block admission.
  *
  * Every count is the backend's ([[InventoryJoin]]); nothing is counted here.
  */
final case class InventorySourceVM(
    role: String,
    source: String,
    absentLabel: String,
    absentDefinition: String,
    absentCount: Option[String],
    absentNote: Option[String],
    issues: Vector[IssueVM]
) derives CanEqual

object InventorySource:

  private def t(id: InventoryTextId, args: String*): String = InventoryText(id, args*)

  /** The inventory of `spec` after `answer`. */
  def vm(spec: DatasetRevisionSpec, answer: InventoryAnswer): InventorySourceVM =
    val file = spec.sources.trials.map(_.path.value.split('/').last)
    def uncounted(note: String, source: String, issues: Vector[IssueVM] = Vector.empty) =
      InventorySourceVM(
        t(InventoryTextId.Role),
        source,
        t(InventoryTextId.AbsentLabel),
        t(InventoryTextId.AbsentDefinition),
        None,
        Some(note),
        issues
      )
    (file, spec.inventory) match
      case (None, _) =>
        uncounted(t(InventoryTextId.AbsentUncounted), t(InventoryTextId.NoFile))
      case (Some(f), None) =>
        uncounted(t(InventoryTextId.AbsentUnmapped, f), t(InventoryTextId.Unmapped, f))
      case (Some(f), Some(_)) =>
        answer match
          case InventoryAnswer.NotAsked =>
            uncounted(
              t(InventoryTextId.AbsentPending, spec.id.label),
              t(InventoryTextId.Pending, f)
            )
          case InventoryAnswer.Summary(summary) =>
            summary.inventory match
              case InventoryJoin.Joined(trials, absent) =>
                InventorySourceVM(
                  t(InventoryTextId.Role),
                  t(InventoryTextId.Joined, f, Format.count(trials.toLong)),
                  t(InventoryTextId.AbsentLabel),
                  t(InventoryTextId.AbsentDefinition),
                  Some(t(InventoryTextId.AbsentCount, Format.count(absent.toLong))),
                  None,
                  Vector.empty
                )
              // The backend joined no inventory: it cannot count absent trials.
              case InventoryJoin.Undeclared =>
                uncounted(t(InventoryTextId.AbsentUncounted), t(InventoryTextId.NotJoined, f))
          case InventoryAnswer.Refused(BackendError.InventoryRefused(_, issues)) =>
            uncounted(
              t(InventoryTextId.AbsentUncounted),
              t(InventoryTextId.Refused, f),
              issues.map(i => IssueVM(i.message, i.pointsAt, true))
            )
          case InventoryAnswer.Refused(other) =>
            uncounted(
              t(InventoryTextId.AbsentPending, spec.id.label),
              t(InventoryTextId.Unavailable, f, other.message)
            )

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

package eyes4s.studio.core.selection

import eyes4s.studio.core.backend.{
  AdmissionSummary,
  DatasetRevision,
  LedgerEntry,
  TrialDisposition
}

/** What a count of the admission ledger shows and opens (ticket S5.6): its
  * number, read from the backend's admission summary, and its trials, the
  * backend ledger's entries with the disposition the ref names. Nothing is
  * counted here, so a count and its trials agree exactly when eyes4s's
  * summary and ledger do.
  *
  * The counts are [[StudioRef.InventoryCount]] and, for the records an
  * `ExcludeRecord` policy keeps out of every map, the trials with records
  * outside the screen ([[StudioRef.WindowTally]] with
  * [[TallyRegion.OutsideScreen]]): a reported count, never a quarantine
  * cause. No other tally is a count of the ledger.
  */
object LedgerCounts:

  /** The summary's figure for `ref`, when `ref` is a count of the summary's
    * dataset that the summary holds. Absent trials have no figure without an
    * inventory, and a cause the summary does not name has none.
    */
  def count(ref: StudioRef, summary: AdmissionSummary): Option[Int] = ref match
    case StudioRef.InventoryCount(d, kind) if d == summary.dataset =>
      kind match
        case InventoryKind.Inventory   => summary.inventoryTrials
        case InventoryKind.Admitted    => Some(summary.admitted)
        case InventoryKind.Quarantined => Some(summary.quarantinedTrials + summary.noFixations)
        case InventoryKind.Cause(code) => summary.quarantined.find(_.code == code).map(_.trials)
        case InventoryKind.NoFixations => Some(summary.noFixations)
        case InventoryKind.Absent      => summary.absent
    case StudioRef.WindowTally(d, TallyRegion.OutsideScreen) if d == summary.dataset =>
      Some(summary.window.trialsOutsideScreen)
    case _ => None

  /** Whether an entry with `disposition` belongs to `kind`. */
  def holds(kind: InventoryKind, disposition: TrialDisposition): Boolean =
    (kind, disposition) match
      case (InventoryKind.Inventory, _)                                 => true
      case (InventoryKind.Admitted, TrialDisposition.Admitted)          => true
      case (InventoryKind.Quarantined, TrialDisposition.Quarantined(_)) => true
      case (InventoryKind.Quarantined, TrialDisposition.NoFixations)    => true
      case (InventoryKind.Cause(code), TrialDisposition.Quarantined(c)) => c.code == code
      case (InventoryKind.NoFixations, TrialDisposition.NoFixations)    => true
      case (InventoryKind.Absent, TrialDisposition.Absent)              => true
      case _                                                            => false

  /** The entries of `dataset`'s ledger that `ref` opens, in ledger order;
    * `None` when `ref` is not a count of that ledger.
    */
  def trials(
      ref: StudioRef,
      dataset: DatasetRevision,
      entries: Vector[LedgerEntry]
  ): Option[Vector[LedgerEntry]] = ref match
    case StudioRef.InventoryCount(d, kind) if d == dataset =>
      Some(entries.filter(e => holds(kind, e.disposition)))
    case StudioRef.WindowTally(d, TallyRegion.OutsideScreen) if d == dataset =>
      Some(entries.filter(_.outsideFrame.nonEmpty))
    case _ => None

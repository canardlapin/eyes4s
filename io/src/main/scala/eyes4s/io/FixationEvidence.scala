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

package eyes4s.io

import eyes4s.kernel.Unit2D
import eyes4s.plan.*

/** The boundary from an in-memory fixation import to the pure admission
  * ledger. Every source record receives exactly one disposition; io-specific
  * row errors become typed `AdmissionReason` values, never display strings.
  */
object FixationEvidence:
  /** Nominal reference to the decoded source records of an import. */
  def source[K, U <: Unit2D](label: String, imported: FixationImport[K, U]): SourceRef =
    SourceRef.of(label, imported.header, imported.sourceRows)

  def ledger[K, U <: Unit2D](
      label: String,
      imported: FixationImport[K, U],
      decision: AdmissionDecision
  ): Either[AdmissionError, AdmissionLedger[K]] =
    val admitted = imported.admitted.map(row =>
      row.rowNumber -> Disposition.Admitted[K](row.key, row.ordinal)
    )
    val rejected = imported.rejected.map(row =>
      row.rowNumber -> Disposition.Rejected[K](row.raw, row.key, reason(row.error))
    )
    AdmissionLedger.decide(
      source(label, imported),
      imported.header,
      (admitted ++ rejected).sortBy(_._1).map { case (record, disposition) =>
        SourceRecord(record, disposition)
      },
      decision,
      imported.policy,
      imported.outsideFrame
    )

  /** The ledger of an inventory admission: the fixation ledger joined to
    * the inventory, whose records are referenced as `inventoryLabel`.
    */
  def ledger[U <: Unit2D](
      label: String,
      inventoryLabel: String,
      imported: InventoryImport[U],
      decision: AdmissionDecision
  ): Either[AdmissionError, AdmissionLedger[TrialKey]] =
    val fixations = imported.fixations
    val admitted  = fixations.admitted.map(row =>
      row.rowNumber -> Disposition.Admitted[TrialKey](row.key, row.ordinal)
    )
    val rejected = fixations.rejected.map(row =>
      row.rowNumber -> Disposition.Rejected[TrialKey](row.raw, row.key, reason(row.error))
    )
    for
      inventory <- InventoryLedger
        .of(
          imported.inventory.source(inventoryLabel),
          imported.inventory.header,
          imported.inventory.columns.attributes,
          imported.trials,
          imported.unlisted,
          imported.recordAttributeColumns,
          imported.recordAttributes,
          imported.sampleCounts
        )
        .left
        .map(AdmissionError.Inventory.apply)
      joined <- AdmissionLedger.decide(
        source(label, fixations),
        fixations.header,
        (admitted ++ rejected).sortBy(_._1).map { case (record, disposition) =>
          SourceRecord(record, disposition)
        },
        decision,
        fixations.policy,
        fixations.outsideFrame,
        inventory,
        TrialIdentity.of,
        _.item
      )
    yield joined

  def reason(error: FixationRowError): AdmissionReason = error match
    case FixationRowError.Width(expected, actual) => AdmissionReason.Width(expected, actual)
    case FixationRowError.Key(text)               => AdmissionReason.Key(text)
    case FixationRowError.Number(column, value, requirement) =>
      AdmissionReason.Number(column, value, requirement)
    case FixationRowError.Time(onset, duration, unit, text) =>
      AdmissionReason.Time(onset, duration, unit.label, text)
    case FixationRowError.Position(x, y, frame) => AdmissionReason.Position(x, y, frame)
    case FixationRowError.Event(text)           => AdmissionReason.Event(text)
    case FixationRowError.Trial(rows, cause)    => AdmissionReason.Quarantined(rows, cause)

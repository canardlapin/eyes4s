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

import eyes4s.design.KeyDigest
import eyes4s.kernel.*
import eyes4s.plan.*

/** An import description and its complete ledger; source evidence is declared,
  * never labelled replay-verified just because an importer produced it.
  */
final class SourceAdmission[K, U <: Unit2D] private[io] (
    val spec: ImportSpec[K, U],
    val ledger: AdmissionLedger[K],
    val accepted: StudyInput[K, U]
):
  def admitted: Option[StudyInput[K, U]] = ledger.outcome match
    case AdmissionOutcome.Refused => None
    case _                        => Some(accepted)

/** Phase 1 interpreters for pure import descriptions. No host closures are
  * reconstructed from a custom reader's name.
  */
object SourceAdmission:
  def source[K, U <: Unit2D](
      label: String,
      contents: String,
      spec: ImportSpec[K, U]
  ): Either[SourceAdmissionError, SourceRef] =
    FixationCsv
      .table(contents, spec.keys.names ++ spec.columns.names)
      .left
      .map(SourceAdmissionError.Import(label, _))
      .map((header, rows) => spec.source(label, header, rows))

  def inventorySource(
      label: String,
      contents: String,
      spec: InventoryImportSpec
  ): Either[SourceAdmissionError, SourceRef] =
    FixationCsv
      .table(contents, spec.names)
      .left
      .map(SourceAdmissionError.Import(label, _))
      .map((header, rows) => spec.source(label, header, rows))

  def read[K, U <: Unit2D](
      label: String,
      contents: String,
      spec: ImportSpec[K, U],
      inventory: Option[(String, String)] = None
  ): Either[SourceAdmissionError, SourceAdmission[K, U]] =
    spec.keys match
      case SourceKeyColumns.Custom(reader, clock, _) =>
        Left(SourceAdmissionError.UnsupportedReplay(label, reader, clock))
      case SourceKeyColumns.Study(p, s, f) =>
        FixationKeyReader
          .study(p, s, f)
          .left
          .map(SourceAdmissionError.Import(label, _))
          .flatMap(reader => ordinary(label, contents, spec, reader))
      case SourceKeyColumns.Trial(p, f, t, item, occurrence) =>
        spec.inventory match
          case Some(binding) =>
            inventory
              .toRight(SourceAdmissionError.MissingInventory(label, binding.identity))
              .flatMap { (inventoryLabel, inventoryText) =>
                inventoried(
                  label,
                  contents,
                  spec,
                  binding,
                  inventoryLabel,
                  inventoryText,
                  p,
                  f,
                  t,
                  item,
                  occurrence
                )
              }
          case None =>
            item
              .toRight(SourceAdmissionError.MissingItem(label))
              .flatMap(i =>
                FixationKeyReader
                  .trial(p, f, t, i, occurrence)
                  .left
                  .map(SourceAdmissionError.Import(label, _))
              )
              .flatMap(reader => ordinary(label, contents, spec, reader))

  private def unit(value: SourceTimeUnit): TimestampUnit = value match
    case SourceTimeUnit.Microseconds => TimestampUnit.Microseconds
    case SourceTimeUnit.Milliseconds => TimestampUnit.Milliseconds
    case SourceTimeUnit.Seconds      => TimestampUnit.Seconds

  private def ordinary[K, U <: Unit2D](
      label: String,
      contents: String,
      spec: ImportSpec[K, U],
      keys: FixationKeyReader[K]
  ): Either[SourceAdmissionError, SourceAdmission[K, U]] =
    given KeyDigest[K] = keys.digest
    given Ordering[K]  = keys.ordering
    given UnitLabel[U] = spec.unit
    val c              = spec.columns
    for
      imported <- FixationCsv
        .decodeDeclared(
          contents,
          RowSpec(
            c.ordinal,
            c.x,
            c.y,
            c.onset,
            c.duration,
            c.samples,
            unit(spec.timeUnit),
            c.attributes
          ),
          keys,
          spec.frame,
          spec.policy,
          keys.participant,
          TimestampRounding.NearestMicrosecond
        )
        .left
        .map(SourceAdmissionError.Import(label, _))
      legacy <- FixationEvidence
        .ledger(label, imported, spec.decision)
        .left
        .map(SourceAdmissionError.Ledger(label, _))
      ledger <- AdmissionLedger
        .of(
          spec.source(label, imported.header, imported.sourceRows),
          legacy.header,
          legacy.records,
          legacy.outcome,
          legacy.policy,
          legacy.outsideFrame
        )
        .left
        .map(SourceAdmissionError.Ledger(label, _))
    yield new SourceAdmission(spec, ledger, StudyInput(imported.accepted))

  private def inventoried[U <: Unit2D](
      label: String,
      contents: String,
      spec: ImportSpec[TrialKey, U],
      binding: SourceInventory,
      inventoryLabel: String,
      inventoryText: String,
      p: String,
      f: String,
      t: String,
      item: Option[String],
      occurrence: Option[String]
  ): Either[SourceAdmissionError, SourceAdmission[TrialKey, U]] =
    given UnitLabel[U] = spec.unit
    val i              = binding.spec
    val c              = spec.columns
    for
      source <- inventorySource(inventoryLabel, inventoryText, i)
      _      <- Either.cond(
        source.identity.contains(binding.identity),
        (),
        SourceAdmissionError.InventoryIdentity(inventoryLabel, binding.identity, source)
      )
      trialColumns <- TrialColumns
        .of(i.participant, i.phase, i.trial, i.occurrence)
        .left
        .map(SourceAdmissionError.Import(inventoryLabel, _))
      inventoryColumns <- TrialInventoryColumns
        .of(trialColumns, i.item, i.attributes)
        .left
        .map(SourceAdmissionError.Import(inventoryLabel, _))
      inventory <- TrialInventory
        .read(inventoryText, inventoryColumns)
        .left
        .map(SourceAdmissionError.Import(inventoryLabel, _))
      keyColumns <- TrialColumns
        .of(p, f, t, occurrence)
        .left
        .map(SourceAdmissionError.Import(label, _))
      table <- FixationTable
        .of(
          keyColumns,
          c.ordinal,
          c.x,
          c.y,
          TimeColumns(c.onset, c.duration, unit(spec.timeUnit)),
          c.samples,
          item,
          c.attributes
        )
        .left
        .map(SourceAdmissionError.Import(label, _))
      imported <- FixationCsv
        .admitInventory(contents, table, inventory, spec.frame, spec.policy)
        .left
        .map(SourceAdmissionError.Import(label, _))
      legacy <- FixationEvidence
        .ledger(label, inventoryLabel, imported, spec.decision)
        .left
        .map(SourceAdmissionError.Ledger(label, _))
      evidence <- InventoryLedger
        .of(
          source,
          inventory.header,
          i.attributes,
          imported.trials,
          imported.unlisted,
          imported.recordAttributeColumns,
          imported.recordAttributes,
          imported.sampleCounts
        )
        .left
        .map(e => SourceAdmissionError.Ledger(label, AdmissionError.Inventory(e)))
      ledger <- AdmissionLedger
        .inventoried(
          spec.source(label, imported.fixations.header, imported.fixations.sourceRows),
          legacy.header,
          legacy.records,
          legacy.outcome,
          legacy.policy,
          legacy.outsideFrame,
          evidence,
          TrialIdentity.of,
          _.item
        )
        .left
        .map(SourceAdmissionError.Ledger(label, _))
    yield new SourceAdmission(spec, ledger, StudyInput(imported.fixations.accepted))

enum SourceAdmissionError derives CanEqual:
  case Import(source: String, underlying: FixationImportError)
  case Ledger(source: String, underlying: AdmissionError)
  case UnsupportedReplay(source: String, reader: DefinitionId, clock: DefinitionId)
  case MissingInventory(source: String, expected: SourceIdentity)
  case InventoryIdentity(source: String, expected: SourceIdentity, actual: SourceRef)
  case MissingItem(source: String)
  def message: String = this match
    case Import(source, e)                        => s"Source '$source': ${e.message}"
    case Ledger(source, e)                        => s"Source '$source': ${e.message}"
    case UnsupportedReplay(source, reader, clock) =>
      s"Source '$source' has no registered replay for reader $reader and clock $clock."
    case MissingInventory(source, expected) =>
      s"Source '$source' requires inventory ${expected.digest}."
    case InventoryIdentity(source, expected, actual) =>
      s"Inventory '$source' expected identity ${expected.digest}, found ${actual.identity.map(_.digest)}."
    case MissingItem(source) =>
      s"Source '$source' needs an item column without a trial inventory."

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

import eyes4s.kernel.*
import eyes4s.plan.*

/** Evidence that the supplied source, interpretation, ledger and input agree.
  * This verifies archive consistency, not authorship or authenticity.
  */
final class VerifiedAdmission[K, U <: Unit2D] private (
    val spec: ImportSpec[K, U],
    val ledger: AdmissionLedger[K],
    val input: StudyInput[K, U]
):
  def admitted: Option[StudyInput[K, U]] = ledger.outcome match
    case AdmissionOutcome.Refused => None
    case _                        => Some(input)

/** The exact ledger component that disagrees with fresh source admission. */
enum LedgerComponent derives CanEqual:
  case Header, Records, Outcome, Policy, OutsideFrame, InventoryPresence
  case InventoryHeader, InventoryAttributes, InventoryTrials, UnlistedTrials
  case RecordAttributeColumns, RecordAttributes, SampleCounts

/** Source-bound verification. The synchronous entry point drains admission;
  * it does not itself promise cooperative cancellation.
  */
object VerifiedAdmission:
  private[io] def verify[K, U <: Unit2D](
      label: String,
      contents: String,
      spec: ImportSpec[K, U],
      ledger: AdmissionLedger[K],
      input: StudyInput[K, U],
      inventory: Option[(String, String)] = None
  ): Either[LedgerVerificationError, VerifiedAdmission[K, U]] =
    for
      _ <- declared(ledger.source)
      _ <- ledger.inventory.fold[Either[LedgerVerificationError, Unit]](Right(()))(i =>
        declared(i.source)
      )
      replay <- SourceAdmission
        .read(label, contents, spec, inventory)
        .left
        .map(LedgerVerificationError.Import.apply)
      result <- compare(ledger, input, replay)
    yield result

  private[io] def declared(source: SourceRef): Either[LedgerVerificationError, Unit] =
    Either.cond(source.identity.nonEmpty, (), LedgerVerificationError.LegacyUnverified(source))

  private def source(
      expected: SourceRef,
      actual: SourceRef
  ): Either[LedgerVerificationError, Unit] =
    SourceComparison.of(false, expected, actual) match
      case SourceComparison.ChangedIdentity(causes) =>
        Left(LedgerVerificationError.SourceChanged(expected, actual, causes))
      case _ => Right(())

  private def equal[A](
      label: String,
      component: LedgerComponent,
      expected: A,
      actual: A
  ): Either[LedgerVerificationError, Unit] =
    Either.cond(
      expected == actual,
      (),
      LedgerVerificationError.LedgerMismatch(
        label,
        component,
        expected.toString,
        actual.toString
      )
    )

  private[io] def compare[K, U <: Unit2D](
      expected: AdmissionLedger[K],
      input: StudyInput[K, U],
      replay: SourceAdmission[K, U]
  ): Either[LedgerVerificationError, VerifiedAdmission[K, U]] =
    val actual = replay.ledger
    val label  = expected.source.label
    for
      _ <- source(expected.source, actual.source)
      _ <- equal(label, LedgerComponent.Header, expected.header, actual.header)
      _ <- equal(label, LedgerComponent.Records, expected.records, actual.records)
      _ <- equal(label, LedgerComponent.Outcome, expected.outcome, actual.outcome)
      _ <- equal(label, LedgerComponent.Policy, expected.policy, actual.policy)
      _ <- equal(
        label,
        LedgerComponent.OutsideFrame,
        expected.outsideFrame,
        actual.outsideFrame
      )
      _ <- (expected.inventory, actual.inventory) match
        case (Some(a), Some(b)) => inventory(a, b)
        case (None, None)       => Right(())
        case (a, b) => equal(label, LedgerComponent.InventoryPresence, a.nonEmpty, b.nonEmpty)
      _ <- Either.cond(
        input.hash == replay.accepted.hash,
        (),
        LedgerVerificationError.InputMismatch(label, input.hash, replay.accepted.hash)
      )
    yield new VerifiedAdmission(replay.spec, expected, input)

  private def inventory(
      a: InventoryLedger,
      b: InventoryLedger
  ): Either[LedgerVerificationError, Unit] =
    val label = a.source.label
    for
      _ <- source(a.source, b.source)
      _ <- equal(label, LedgerComponent.InventoryHeader, a.header, b.header)
      _ <- equal(
        label,
        LedgerComponent.InventoryAttributes,
        a.attributeColumns,
        b.attributeColumns
      )
      _ <- equal(label, LedgerComponent.InventoryTrials, a.trials, b.trials)
      _ <- equal(label, LedgerComponent.UnlistedTrials, a.unlisted, b.unlisted)
      _ <- equal(
        label,
        LedgerComponent.RecordAttributeColumns,
        a.recordAttributeColumns,
        b.recordAttributeColumns
      )
      _ <- equal(
        label,
        LedgerComponent.RecordAttributes,
        a.recordAttributes,
        b.recordAttributes
      )
      _ <- equal(label, LedgerComponent.SampleCounts, a.sampleCounts, b.sampleCounts)
    yield ()

object LedgerReverification:
  def verify[K, U <: Unit2D](
      label: String,
      contents: String,
      spec: ImportSpec[K, U],
      ledger: AdmissionLedger[K],
      input: StudyInput[K, U],
      inventory: Option[(String, String)] = None
  ): Either[LedgerVerificationError, VerifiedAdmission[K, U]] =
    VerifiedAdmission.verify(label, contents, spec, ledger, input, inventory)

enum LedgerVerificationError derives CanEqual:
  case LegacyUnverified(source: SourceRef)
  case Import(underlying: SourceAdmissionError)
  case SourceChanged(expected: SourceRef, actual: SourceRef, causes: IdentityChanges)
  case LedgerMismatch(
      source: String,
      component: LedgerComponent,
      expected: String,
      actual: String
  )
  case InputMismatch(source: String, expected: ContentHash, actual: ContentHash)

  def message: String = this match
    case LegacyUnverified(source) =>
      s"Source '${source.label}' (${source.records.digest}) has legacy unspecified interpretation; replay cannot verify it."
    case Import(error)                           => error.message
    case SourceChanged(expected, actual, causes) =>
      s"Source '${expected.label}' identity ${expected.identity.map(_.digest)} differs from '${actual.label}' ${actual.identity.map(_.digest)} in ${causes.values}."
    case LedgerMismatch(source, component, expected, actual) =>
      s"Source '$source' ledger $component declares $expected; replay produced $actual."
    case InputMismatch(source, expected, actual) =>
      s"Source '$source' input ${expected.render} differs from replayed input ${actual.render}."

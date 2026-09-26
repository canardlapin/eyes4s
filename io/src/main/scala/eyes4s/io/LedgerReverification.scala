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

import eyes4s.core.*
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

/** The exact input component that disagrees with fresh source admission. */
enum InputComponent derives CanEqual:
  case TrialCount, TrialKey, Frame, Clock, FixationCount, Fixation
  case Source, SampleSupport, SourceRecording

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
      _ <- compareInput(label, input, replay.accepted)
    yield new VerifiedAdmission(replay.spec, expected, input)

  /** Hash equality is only a fast check: it does not bind every scientific
    * field. Compare the complete ordered data before granting verification.
    * Transitions and extent are derived by Scanpath from these fixations.
    */
  private[io] def compareInput[K, U <: Unit2D](
      label: String,
      expected: StudyInput[K, U],
      actual: StudyInput[K, U]
  ): Either[LedgerVerificationError, Unit] =
    def check[A](
        component: InputComponent,
        trial: Option[Int],
        fixation: Option[Int],
        left: => A,
        right: => A
    )(agrees: Boolean): Either[LedgerVerificationError, Unit] =
      Either.cond(
        agrees,
        (),
        LedgerVerificationError.InputEvidenceMismatch(
          label,
          component,
          trial,
          fixation,
          left.toString,
          right.toString
        )
      )
    val left  = expected.trials.rows
    val right = actual.trials.rows
    check(InputComponent.TrialCount, None, None, left.size, right.size)(left.size == right.size)
      .flatMap { _ =>
        left
          .zip(right)
          .zipWithIndex
          .foldLeft[Either[LedgerVerificationError, Unit]](Right(())) {
            case (result, ((a, b), index)) =>
              result.flatMap { _ =>
                val x     = a.value
                val y     = b.value
                val trial = Some(index)
                for
                  _ <- check(InputComponent.TrialKey, trial, None, a.key, b.key)(a.key == b.key)
                  _ <- check(InputComponent.Frame, trial, None, x.frame, y.frame)(
                    Agreement.frames(x.frame, y.frame).isRight
                  )
                  _ <- check(InputComponent.Clock, trial, None, x.clock, y.clock)(
                    Agreement.clocks(x.clock, y.clock).isRight
                  )
                  _ <- check(InputComponent.FixationCount, trial, None, x.n, y.n)(x.n == y.n)
                  _ <- (0 until x.n).foldLeft[Either[LedgerVerificationError, Unit]](
                    Right(())
                  ) { (result, fix) =>
                    result.flatMap { _ =>
                      // Fixation is a structural value: span, centre, support and
                      // complete dispersion status/value/method/evidence all participate.
                      check(
                        InputComponent.Fixation,
                        trial,
                        Some(fix),
                        x.fixations(fix),
                        y.fixations(fix)
                      )(x.fixations(fix) == y.fixations(fix))
                    }
                  }
                  _ <- check(InputComponent.Source, trial, None, x.source, y.source)(
                    x.source == y.source
                  )
                  _ <- check(
                    InputComponent.SampleSupport,
                    trial,
                    None,
                    x.sampleSupport,
                    y.sampleSupport
                  )(x.sampleSupport == y.sampleSupport)
                  _ <- check(
                    InputComponent.SourceRecording,
                    trial,
                    None,
                    x.sourceRecording.map(recordingData),
                    y.sourceRecording.map(recordingData)
                  )(sameRecording(x.sourceRecording, y.sourceRecording))
                yield ()
              }
          }
      }

  private def recordingData[U <: Unit2D](recording: Recording[U]) =
    (
      recording.frame,
      recording.clock,
      recording.rate,
      recording.eye,
      recording.pupilUnit,
      recording.samplingTolerance.toSpan,
      recording.samplingEvidence,
      recording.samples.iterator
        .map(sample => (sample.t, sample.gaze, sample.lineage.toVector))
        .toVector
    )

  private def sameRecording[U <: Unit2D](
      left: Option[Recording[U]],
      right: Option[Recording[U]]
  ): Boolean =
    (left, right) match
      case (None, None)       => true
      case (Some(a), Some(b)) =>
        Agreement.frames(a.frame, b.frame).isRight && Agreement
          .clocks(a.clock, b.clock)
          .isRight &&
        a.rate == b.rate && a.eye == b.eye && a.pupilUnit == b.pupilUnit &&
        a.samplingTolerance == b.samplingTolerance && a.samplingEvidence == b.samplingEvidence &&
        a.samples.iterator.sameElements(b.samples.iterator)
      case _ => false

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

  case InputEvidenceMismatch(
      source: String,
      component: InputComponent,
      trial: Option[Int],
      fixation: Option[Int],
      expected: String,
      actual: String
  )

  case ManifestBinding(ledger: String, kind: String, count: Int)

  def message: String = this match
    case LegacyUnverified(source) =>
      s"Source '${source.label}' (${source.records.digest}) has legacy unspecified interpretation; replay cannot verify it."
    case Import(error)                           => error.message
    case SourceChanged(expected, actual, causes) =>
      s"Source '${expected.label}' identity ${expected.identity.map(_.digest)} differs from '${actual.label}' ${actual.identity.map(_.digest)} in ${causes.values}."
    case LedgerMismatch(source, component, expected, actual) =>
      s"Source '$source' ledger $component declares $expected; replay produced $actual."
    case ManifestBinding(ledger, kind, count) =>
      s"Ledger artifact '$ledger' has $count $kind bindings; expected exactly one."
    case InputMismatch(source, expected, actual) =>
      s"Source '$source' input ${expected.render} differs from replayed input ${actual.render}."
    case InputEvidenceMismatch(source, component, trial, fixation, expected, actual) =>
      s"Source '$source' input $component at trial $trial fixation $fixation declares $expected; replay produced $actual."

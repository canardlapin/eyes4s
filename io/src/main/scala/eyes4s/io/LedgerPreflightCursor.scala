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

import eyes4s.design.SampleQuantum
import eyes4s.kernel.Unit2D
import eyes4s.plan.*

/** A component gets only the remaining retention allowance. Failures are
  * translated back to the caller's original cap and cumulative observation.
  * Other dimensions remain unchanged. No component can first allocate beyond
  * the aggregate cap and rely on a later coordinator check to discover it.
  */
private[io] final case class LedgerRetentionScope private (
    original: LedgerExecutionLimits,
    limits: LedgerExecutionLimits,
    before: Long
):
  def restore(error: LedgerResourceError): LedgerResourceError = error match
    case LedgerResourceError.Exceeded(LedgerResource.RetainedEvidenceUnits, _, observed, at) =>
      LedgerResourceError.Exceeded(
        LedgerResource.RetainedEvidenceUnits,
        original(LedgerResource.RetainedEvidenceUnits),
        BigInt(before) + observed,
        at
      )
    case other => other

  def total(local: Long, at: LedgerResourceLocation): Either[LedgerResourceError, Long] =
    original.add(LedgerResource.RetainedEvidenceUnits, before, local, at)

private[io] object LedgerRetentionScope:
  def start(
      original: LedgerExecutionLimits,
      before: Long,
      at: LedgerResourceLocation
  ): Either[LedgerResourceError, LedgerRetentionScope] =
    import LedgerResource.*
    for
      _ <- Either.cond(
        before >= 0,
        (),
        LedgerResourceError.NegativeLimit(RetainedEvidenceUnits, before)
      )
      _         <- original.check(RetainedEvidenceUnits, before, at)
      remaining <- LedgerExecutionLimits.of(
        original(SourceCodeUnits),
        original(LogicalRecords),
        original(EncodedRecordCodeUnits),
        original(FieldCodeUnits),
        original(Columns),
        original(DeclaredAttributes),
        original(CorrectionRules),
        original(CorrectionOperandCodeUnits),
        original(NumericDigits),
        original(NumericExponentMagnitude),
        original(RetainedEvidenceUnits) - before,
        original(RenderedOperandCodeUnits)
      )
    yield new LedgerRetentionScope(original, remaining, before)

private[io] enum LedgerPreflightStage derives CanEqual:
  case Description, ExpectedLedger, ExpectedInput

private[io] enum LedgerPreflightWork[K, U <: Unit2D]:
  case Description(cursor: LedgerDescriptionCursor)
  case Evidence(cursor: LedgerEvidenceCursor[K])
  case Input(cursor: LedgerInputCursor[K, U])

/** Resource preflight only: completion grants no scientific verification.
  * Each advance stays within one visitor phase. Retention accumulates across
  * the supplied description, ledger and input, including their shared data.
  */
private[io] final case class LedgerPreflightCursor[K, U <: Unit2D] private (
    spec: ImportSpec[K, U],
    ledger: AdmissionLedger[K],
    input: StudyInput[K, U],
    scope: LedgerRetentionScope,
    work: LedgerPreflightWork[K, U]
):
  def stage: LedgerPreflightStage = work match
    case LedgerPreflightWork.Description(_) => LedgerPreflightStage.Description
    case LedgerPreflightWork.Evidence(_)    => LedgerPreflightStage.ExpectedLedger
    case LedgerPreflightWork.Input(_)       => LedgerPreflightStage.ExpectedInput

  def advance(
      quantum: SampleQuantum
  ): Either[LedgerDescriptionError, LedgerPreflightStep[K, U]] =
    import LedgerDescriptionError.*
    import LedgerPreflightWork.*
    def resource(error: LedgerResourceError): LedgerDescriptionError = Resource(
      scope.restore(error)
    )
    def total(local: Long, role: LedgerResourceSource): Either[LedgerDescriptionError, Long] =
      scope.total(local, LedgerResourceLocation(role)).left.map(Resource.apply)
    def nextScope(kept: Long, role: LedgerResourceSource) =
      LedgerRetentionScope
        .start(scope.original, kept, LedgerResourceLocation(role))
        .left
        .map(Resource.apply)
    def more(units: Int, kept: Long, next: LedgerPreflightCursor[K, U]) =
      LedgerPreflightStep(stage, units, kept, Some(next))
    work match
      case Description(cursor) =>
        cursor.advance(quantum).left.map(resource).flatMap { step =>
          total(step.retainedUnits, LedgerResourceSource.ImportDescription).flatMap { kept =>
            step.next match
              case Some(next) =>
                Right(more(step.workUnits, kept, copy(work = Description(next))))
              case None =>
                for
                  budget <- nextScope(kept, LedgerResourceSource.ExpectedLedger)
                  next   <- LedgerEvidenceCursor
                    .start(spec, ledger, budget.limits)
                    .left
                    .map(Unsupported.apply)
                yield more(step.workUnits, kept, copy(scope = budget, work = Evidence(next)))
          }
        }
      case Evidence(cursor) =>
        cursor.advance(quantum).left.map(resource).flatMap { step =>
          total(step.retainedUnits, LedgerResourceSource.ExpectedLedger).flatMap { kept =>
            step.next match
              case Some(next) => Right(more(step.workUnits, kept, copy(work = Evidence(next))))
              case None       =>
                for
                  budget <- nextScope(kept, LedgerResourceSource.ExpectedInput)
                  next   <- LedgerInputCursor
                    .start(spec, input, budget.limits)
                    .left
                    .map(Unsupported.apply)
                yield more(step.workUnits, kept, copy(scope = budget, work = Input(next)))
          }
        }
      case Input(cursor) =>
        cursor.advance(quantum).left.map(resource).flatMap { step =>
          total(step.retainedUnits, LedgerResourceSource.ExpectedInput).map { kept =>
            LedgerPreflightStep(
              stage,
              step.workUnits,
              kept,
              step.next.map(next => copy(work = Input(next)))
            )
          }
        }

private[io] object LedgerPreflightCursor:
  def start[K, U <: Unit2D](
      spec: ImportSpec[K, U],
      ledger: AdmissionLedger[K],
      input: StudyInput[K, U],
      limits: LedgerExecutionLimits
  ): Either[LedgerDescriptionError, LedgerPreflightCursor[K, U]] =
    for
      description <- LedgerDescriptionCursor.start(spec, limits)
      scope       <- LedgerRetentionScope
        .start(limits, 0L, LedgerResourceLocation(LedgerResourceSource.ImportDescription))
        .left
        .map(LedgerDescriptionError.Resource.apply)
    yield new LedgerPreflightCursor(
      spec,
      ledger,
      input,
      scope,
      LedgerPreflightWork.Description(description)
    )

private[io] final case class LedgerPreflightStep[K, U <: Unit2D](
    stage: LedgerPreflightStage,
    workUnits: Int,
    retainedUnits: Long,
    next: Option[LedgerPreflightCursor[K, U]]
)

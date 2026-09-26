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

/** Dimensions of the execution envelope, independent of scientific source identity. */
enum LedgerResource derives CanEqual:
  case SourceCodeUnits, LogicalRecords, EncodedRecordCodeUnits, FieldCodeUnits, Columns
  case DeclaredAttributes, CorrectionRules, CorrectionOperandCodeUnits
  case NumericDigits, NumericExponentMagnitude, RetainedEvidenceUnits, RenderedOperandCodeUnits

/** A fixed source role; refusals never render an unchecked source label. */
enum LedgerResourceSource derives CanEqual:
  case Primary, Inventory, ImportDescription, ExpectedLedger, ExpectedInput

/** Logical CSV records and fields count from one, including the header. */
final case class LedgerResourceLocation(
    source: LedgerResourceSource,
    record: Option[Long] = None,
    field: Option[Long] = None
) derives CanEqual

enum LedgerResourceError derives CanEqual:
  case NegativeLimit(dimension: LedgerResource, value: Long)
  case Exceeded(
      dimension: LedgerResource,
      limit: Long,
      observed: BigInt,
      location: LedgerResourceLocation
  )

  def message: String = this match
    case NegativeLimit(dimension, value) =>
      s"Execution limit $dimension must be non-negative, received $value."
    case Exceeded(dimension, limit, observed, location) =>
      s"Execution resource $dimension at $location exceeds $limit: observed $observed."

/** Explicit bounds for each atomic operation used by ledger execution.
  * String sizes count UTF-16 code units. Encoded record size includes its
  * terminating LF or CRLF. Logical records include the header. Retained
  * evidence counts code units plus one unit per retained field and record;
  * it is a logical storage bound, not an estimate of JVM or JS heap bytes.
  * No default envelope is selected for a caller.
  */
final class LedgerExecutionLimits private (private val values: Vector[Long]):
  def apply(dimension: LedgerResource): Long = values(dimension.ordinal)

  private[io] def check(
      dimension: LedgerResource,
      observed: Long,
      location: LedgerResourceLocation
  ): Either[LedgerResourceError, Unit] =
    Either.cond(
      observed <= apply(dimension),
      (),
      LedgerResourceError.Exceeded(dimension, apply(dimension), BigInt(observed), location)
    )

  /** Refuse before addition can wrap; the failure retains the exact first excess. */
  private[io] def add(
      dimension: LedgerResource,
      current: Long,
      amount: Long,
      location: LedgerResourceLocation
  ): Either[LedgerResourceError, Long] =
    val limit = apply(dimension)
    if current > limit || amount > limit - current then
      Left(
        LedgerResourceError
          .Exceeded(dimension, limit, BigInt(current) + BigInt(amount), location)
      )
    else Right(current + amount)

object LedgerExecutionLimits:
  def of(
      sourceCodeUnits: Long,
      logicalRecords: Long,
      encodedRecordCodeUnits: Long,
      fieldCodeUnits: Long,
      columns: Long,
      declaredAttributes: Long,
      correctionRules: Long,
      correctionOperandCodeUnits: Long,
      numericDigits: Long,
      numericExponentMagnitude: Long,
      retainedEvidenceUnits: Long,
      renderedOperandCodeUnits: Long
  ): Either[LedgerResourceError, LedgerExecutionLimits] =
    val values = Vector(
      sourceCodeUnits,
      logicalRecords,
      encodedRecordCodeUnits,
      fieldCodeUnits,
      columns,
      declaredAttributes,
      correctionRules,
      correctionOperandCodeUnits,
      numericDigits,
      numericExponentMagnitude,
      retainedEvidenceUnits,
      renderedOperandCodeUnits
    )
    values.zipWithIndex
      .collectFirst {
        case (value, index) if value < 0 =>
          LedgerResourceError.NegativeLimit(LedgerResource.fromOrdinal(index), value)
      }
      .toLeft(new LedgerExecutionLimits(values))

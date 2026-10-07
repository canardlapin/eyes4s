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

import eyes4s.kernel.{Span, Window}
import eyes4s.plan.{AttributeValue, Attributes, InventoryError, InventoryTrial}

/** An explicitly declared trial duration, defining [0, duration) from trial start.
  * The original cell remains a text attribute in the native inventory evidence.
  * Duplicate declarations must agree on that exact text: 5 and 5.0 remain
  * distinct attribute values even when their rounded durations agree.
  */
final class TrialDurationFromStart private (
    val column: String,
    val unit: TimestampUnit,
    val rounding: TimestampRounding
):
  def parse(raw: String, record: Int): Either[InventoryError, Option[Window]] =
    if raw.isEmpty then Right(None)
    else
      FixationCsv
        .micros(raw, column, unit, rounding)
        .left
        .map(e => InventoryError.Field(record, column, raw, e.message))
        .flatMap { micros =>
          if micros <= 0 then
            Left(
              InventoryError.Field(
                record,
                column,
                raw,
                "a positive trial duration after microsecond rounding"
              )
            )
          else
            Window
              .lasting(Span.micros(micros))
              .left
              .map(e => InventoryError.Field(record, column, raw, e.message))
              .map(Some(_))
        }

  /** Read the exact captured inventory value, including after ledger restoration. */
  def extent(trial: InventoryTrial): Either[InventoryError, Option[Window]] =
    captured(trial.identity.render, trial.rows, trial.attributes)

  def extent(row: InventoryRow): Either[InventoryError, Option[Window]] =
    captured(row.identity.render, row.records, row.attributes)

  private def captured(
      owner: String,
      records: Vector[Int],
      values: Attributes
  ): Either[InventoryError, Option[Window]] =
    records.headOption.toRight(InventoryError.RecordOrder(owner, records)).flatMap { record =>
      values.get(column) match
        case Some(AttributeValue.Text(raw)) => parse(raw, record)
        case Some(AttributeValue.Blank)     => Right(None)
        case Some(value)                    =>
          Left(InventoryError.AttributeKindMismatch(owner, column, "Text", value.productPrefix))
        case None => Left(InventoryError.AttributeNames(owner, Vector(column), values.names))
    }

object TrialDurationFromStart:
  def of(
      column: String,
      unit: TimestampUnit,
      rounding: TimestampRounding = TimestampRounding.NearestMicrosecond
  ): Either[FixationImportError, TrialDurationFromStart] =
    TrialColumns
      .distinct(Vector(column))
      .map(_ => new TrialDurationFromStart(column, unit, rounding))

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

package eyes4s.studio.core.document

import eyes4s.io.{TrialDurationFromStart, TimestampRounding, TimestampUnit}
import io.circe.{Codec, Decoder, Encoder}

/** The named source precision policy; timing is never inferred from a column name. */
enum InventoryDurationRounding derives CanEqual, Codec.AsObject:
  case NearestMicrosecond

/** An inventory duration defining [0, duration) relative to trial start. */
final case class InventoryDurationColumn private (
    column: ColumnName,
    unit: TimeUnit,
    rounding: InventoryDurationRounding
) derives CanEqual:
  def core: Either[eyes4s.io.FixationImportError, TrialDurationFromStart] =
    val units = unit match
      case TimeUnit.Microseconds => TimestampUnit.Microseconds
      case TimeUnit.Milliseconds => TimestampUnit.Milliseconds
      case TimeUnit.Seconds      => TimestampUnit.Seconds
    val precision = rounding match
      case InventoryDurationRounding.NearestMicrosecond => TimestampRounding.NearestMicrosecond
    TrialDurationFromStart.of(column.value, units, precision)

object InventoryDurationColumn:
  def of(
      column: ColumnName,
      unit: TimeUnit,
      rounding: InventoryDurationRounding = InventoryDurationRounding.NearestMicrosecond
  ): Either[DocumentError, InventoryDurationColumn] =
    Right(new InventoryDurationColumn(column, unit, rounding))

  given Codec.AsObject[InventoryDurationColumn] = Codec.AsObject.from(
    Decoder.forProduct3("column", "unit", "rounding")(of).emap(_.left.map(_.message)),
    Encoder.forProduct3("column", "unit", "rounding")(d => (d.column, d.unit, d.rounding))
  )

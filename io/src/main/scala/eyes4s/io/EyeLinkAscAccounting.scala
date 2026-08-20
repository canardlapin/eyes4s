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

/** The one disposition assigned to one physical ASC line. Comments are typed
  * records; only a physically blank line is exempt from the nonblank law.
  */
enum AscLineDisposition derives CanEqual:
  case TypedRecord
  case PreservedUnknown
  case Rejected
  case Blank

/** Inspectable witness for one physical source line. */
final class AscAccountedLine private[io] (
    val source: String,
    val line: Long,
    val byteOffset: Long,
    val disposition: AscLineDisposition
)

/** Evidence for the ASC exact-line-accounting law.
  *
  * For every input framing emission there is exactly one location witness. Each
  * nonblank witness is exactly one typed record, preserved unknown record, or
  * rejected line. Source locations are positive, nonnegative, and unique.
  */
final class AscLineAccounting private[io] (
    val entries: Vector[AscAccountedLine],
    val physicalLines: Int,
    val typedRecords: Int,
    val preservedUnknown: Int,
    val rejectedLines: Int,
    val blankLines: Int,
    val exactPartition: Boolean,
    val validLocations: Boolean,
    val uniqueLocations: Boolean
):
  def nonblankLines: Int = physicalLines - blankLines

  def nonblankPartition: Boolean =
    nonblankLines == typedRecords + preservedUnknown + rejectedLines

  def lawHolds: Boolean =
    exactPartition && nonblankPartition && validLocations && uniqueLocations

object EyeLinkAscAccounting:

  /** Audit already-framed output without interpreting sample or event values. */
  def audit(framing: Vector[AscFramingEmission]): AscLineAccounting =
    val entries = framing.map {
      case AscFramingEmission.Parsed(value) =>
        val disposition = value.record match
          case AscRecord.Blank         => AscLineDisposition.Blank
          case AscRecord.Unknown(_, _) => AscLineDisposition.PreservedUnknown
          case AscRecord.Comment(_) | AscRecord.Sample(_) | AscRecord.Message(_) |
              AscRecord.Boundary(_, _) | AscRecord.Configuration(_, _) |
              AscRecord.NativeEvent(_, _) | AscRecord.Button(_) | AscRecord.Input(_) |
              AscRecord.LostData(_) | AscRecord.MalformedKnown(_, _) =>
            AscLineDisposition.TypedRecord
        new AscAccountedLine(
          value.source.source,
          value.source.number,
          value.source.byteOffset,
          disposition
        )
      case AscFramingEmission.Rejected(value) =>
        val (source, line, byteOffset) = rejectedLocation(value)
        new AscAccountedLine(source, line, byteOffset, AscLineDisposition.Rejected)
    }
    val typed     = entries.count(_.disposition == AscLineDisposition.TypedRecord)
    val unknown   = entries.count(_.disposition == AscLineDisposition.PreservedUnknown)
    val rejected  = entries.count(_.disposition == AscLineDisposition.Rejected)
    val blank     = entries.count(_.disposition == AscLineDisposition.Blank)
    val locations = entries.map(value => (value.source, value.line, value.byteOffset))
    new AscLineAccounting(
      entries,
      framing.length,
      typed,
      unknown,
      rejected,
      blank,
      exactPartition = framing.length == typed + unknown + rejected + blank,
      validLocations = entries.forall(value => value.line > 0L && value.byteOffset >= 0L),
      uniqueLocations = locations.distinct.length == locations.length
    )

  private def rejectedLocation(value: AscFramingDiagnostic): (String, Long, Long) = value match
    case AscFramingDiagnostic.LineTooLong(source, line, byteOffset, _, _, _) =>
      (source, line, byteOffset)
    case AscFramingDiagnostic.LoneCarriageReturn(source, line, byteOffset, _, _) =>
      (source, line, byteOffset)
    case AscFramingDiagnostic.SourceLineRejected(source, line, byteOffset, _) =>
      (source, line, byteOffset)

end EyeLinkAscAccounting

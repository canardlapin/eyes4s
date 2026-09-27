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
import eyes4s.kernel.ContentHash

private[io] enum LedgerSourceStage derives CanEqual:
  case Decode, Digest

private[io] enum LedgerSourceWork:
  case Decode(cursor: CsvTokenCursor)
  case Record(index: Int, combined: ContentHash)
  case Field(record: Int, index: Int, combined: ContentHash, recordHash: ContentHash)
  case Text(
      record: Int,
      index: Int,
      combined: ContentHash,
      recordHash: ContentHash,
      cursor: LedgerTextHashCursor
  )

/** Complete CSV syntax is resolved before a caller can inspect the header:
  * S1 decodes the entire table before returning a header/row semantic error.
  * Digest work then visits record boundaries, fields and their UTF-16 units.
  * There is no whole-source digest or reconstruction in the completion step.
  */
private[io] final case class LedgerSourceCursor private (
    source: LedgerResourceSource,
    limits: LedgerExecutionLimits,
    records: Vector[Vector[String]],
    retainedUnits: Long,
    work: LedgerSourceWork
):
  def stage: LedgerSourceStage = work match
    case LedgerSourceWork.Decode(_) => LedgerSourceStage.Decode
    case _                          => LedgerSourceStage.Digest

  def advance(quantum: SampleQuantum): Either[CsvTokenError, LedgerSourceStep] =
    import LedgerSourceWork.*
    def more(units: Int, next: LedgerSourceWork): LedgerSourceStep =
      LedgerSourceStep.More(stage, units, copy(work = next))
    // SourceRef.digest always includes a header record, even for empty input.
    def fields(index: Int): Vector[String] =
      if records.isEmpty then Vector.empty else records(index)
    work match
      case Decode(cursor) =>
        cursor.advance(quantum).map { step =>
          val collected = step.row.fold(records)(row => records :+ row)
          val next      =
            step.next.fold[LedgerSourceWork](Record(0, ContentHash.empty))(Decode.apply)
          LedgerSourceStep.More(
            LedgerSourceStage.Decode,
            step.workUnits,
            copy(records = collected, retainedUnits = step.retainedUnits, work = next)
          )
        }
      case Record(index, combined) =>
        if index < math.max(1, records.size) then
          // An Int's decimal spelling is at most ten code units. This fixed
          // scalar digest is one transition, separate from variable field text.
          val count = ContentHash.ofString(fields(index).size.toString)
          Right(
            more(1, Field(index, 0, combined, ContentHash.combine(ContentHash.empty, count)))
          )
        else
          val rows   = if records.isEmpty then Vector.empty else records.tail
          val digest = ContentHash.combine(combined, ContentHash.ofString(rows.size.toString))
          Right(
            LedgerSourceStep.Done(
              1,
              LedgerDecodedSource(
                records.headOption.getOrElse(Vector.empty),
                rows,
                digest,
                retainedUnits
              )
            )
          )
      case Field(record, index, combined, recordHash) =>
        val row = fields(record)
        if index < row.size then
          LedgerTextHashCursor
            .start(
              row(index),
              limits,
              LedgerResourceLocation(source, Some(record.toLong + 1), Some(index.toLong + 1))
            )
            .left
            .map(CsvTokenError.Resource.apply)
            .map(cursor => more(1, Text(record, index, combined, recordHash, cursor)))
        else Right(more(1, Record(record + 1, ContentHash.combine(combined, recordHash))))
      case Text(record, index, combined, recordHash, cursor) =>
        Right(cursor.advance(quantum) match
          case LedgerTextHashStep.More(units, next) =>
            more(units, Text(record, index, combined, recordHash, next))
          case LedgerTextHashStep.Done(units, hash) =>
            more(
              units,
              Field(record, index + 1, combined, ContentHash.combine(recordHash, hash))
            ))

private[io] object LedgerSourceCursor:
  def start(
      contents: String,
      source: LedgerResourceSource,
      limits: LedgerExecutionLimits
  ): Either[LedgerResourceError, LedgerSourceCursor] =
    CsvTokenCursor
      .start(contents, source, limits)
      .map(cursor =>
        new LedgerSourceCursor(
          source,
          limits,
          Vector.empty,
          0L,
          LedgerSourceWork.Decode(cursor)
        )
      )

private[io] final case class LedgerDecodedSource(
    header: Vector[String],
    rows: Vector[Vector[String]],
    recordsDigest: ContentHash,
    retainedUnits: Long
)

private[io] enum LedgerSourceStep:
  case More(stage: LedgerSourceStage, workUnits: Int, next: LedgerSourceCursor)
  case Done(workUnits: Int, source: LedgerDecodedSource)

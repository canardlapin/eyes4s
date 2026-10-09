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

import eyes4s.plan.*

import scala.collection.mutable.ArrayBuffer

/** Why a text has no record layout. */
enum CsvLayoutError derives CanEqual:
  /** The text is not RFC 4180 CSV; the decoder refuses it the same way. */
  case Csv(underlying: TidyCsvError)

  /** The text's records do not form a layout (it has no header record). */
  case Layout(underlying: RecordIdentityError)

  def message: String = this match
    case Csv(error)    => error.message
    case Layout(error) => error.message

object CsvLayoutError:
  given diagnose: Diagnose[CsvLayoutError, Nothing] =
    import IoDiagnostics.given
    Diagnose.derived[CsvLayoutError, Nothing](IoDiagnosticCatalog.csvLayout)(_.message)

/** The records of a CSV text, with where each one lies in the text and among
  * its physical lines.
  *
  * Record boundaries are read exactly as the fixation and trial-inventory
  * importers' RFC 4180 decoder reads them: a record ends at an unquoted LF or
  * CRLF, a quoted field may hold line breaks, and a trailing record is kept
  * when it has a field or a character. So record `n` of the layout is record
  * `n` of the decoder, and the ledger's record numbers ([[eyes4s.plan.CsvRecord CsvRecord]]s) name
  * the same records. The text is kept so a record's verbatim text can be
  * shown.
  */
final class CsvLayout private (
    text: String,
    starts: IArray[Int],
    ends: IArray[Int],
    val lines: RecordLines
):
  /** The number of data records, the header excluded. */
  def records: Int = lines.records

  /** The header record's text, without its line terminator. */
  def header: String = text.substring(starts(0), ends(0))

  /** A data record's text as it stands in the source, without its line
    * terminator; a record with a quoted line break keeps it.
    */
  def verbatim(record: DataRecord): Either[RecordIdentityError, String] =
    Either.cond(
      record.value <= records,
      text.substring(starts(record.value), ends(record.value)),
      RecordIdentityError.RecordBeyond(record, records)
    )

  override def toString: String = s"CsvLayout($lines)"

object CsvLayout:
  /** The layout of a CSV text, the header being its first record. A text the
    * decoder refuses is refused with the decoder's error.
    */
  def scan(text: String): Either[CsvLayoutError, CsvLayout] =
    val starts                        = ArrayBuffer.empty[Int]
    val ends                          = ArrayBuffer.empty[Int]
    val lineCounts                    = Vector.newBuilder[Int]
    var quoted                        = false
    var closed                        = false
    var index                         = 0
    var start                         = 0
    var fieldChars                    = 0
    var fields                        = 0
    var breaks                        = 0
    var failure: Option[TidyCsvError] = None

    def finishField(): Unit =
      fields += 1
      fieldChars = 0
      closed = false

    def finishRecord(end: Int, next: Int): Unit =
      starts += start
      ends += end
      lineCounts += breaks + 1
      start = next
      fieldChars = 0
      fields = 0
      breaks = 0
      closed = false

    def terminator(character: Char): Boolean = character match
      case '\r' if index + 1 < text.length && text.charAt(index + 1) == '\n' =>
        finishRecord(index, index + 2)
        index += 1
        true
      case '\n' =>
        finishRecord(index, index + 1)
        true
      case _ => false

    while index < text.length && failure.isEmpty do
      val character = text.charAt(index)
      if quoted then
        if character == '"' then
          if index + 1 < text.length && text.charAt(index + 1) == '"' then
            fieldChars += 1
            index += 1
          else
            quoted = false
            closed = true
        else
          if character == '\n' then breaks += 1
          fieldChars += 1
      else if closed then
        if character == ',' then finishField()
        else if !terminator(character) then
          failure = Some(TidyCsvError.MalformedCsv(index, character))
      else if character == ',' then finishField()
      else if character == '"' && fieldChars == 0 then quoted = true
      else if character == '"' then failure = Some(TidyCsvError.MalformedCsv(index, character))
      else if !terminator(character) then fieldChars += 1
      index += 1

    failure match
      case Some(error)    => Left(CsvLayoutError.Csv(error))
      case None if quoted =>
        Left(CsvLayoutError.Csv(TidyCsvError.UnterminatedQuotedField(index)))
      case None =>
        if fieldChars > 0 || fields > 0 then finishRecord(text.length, text.length)
        RecordLines
          .of(lineCounts.result())
          .left
          .map(CsvLayoutError.Layout.apply)
          .map(new CsvLayout(text, IArray.from(starts), IArray.from(ends), _))

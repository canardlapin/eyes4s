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

import scala.collection.mutable.ArrayBuffer

/** Immutable position between complete RFC-4180 records. A step bounds the
  * number of records, including quoted records containing embedded newlines.
  */
private[io] final case class CsvCursor private (contents: String, index: Int):
  def advance(recordBudget: Int): Either[TidyCsvError, CsvPage] =
    val rows                          = ArrayBuffer.empty[Vector[String]]
    val fields                        = ArrayBuffer.empty[String]
    val field                         = new java.lang.StringBuilder
    var quoted                        = false
    var closed                        = false
    var offset                        = index
    var failure: Option[TidyCsvError] = None
    def finishField(): Unit           =
      fields += field.toString
      field.setLength(0)
      closed = false
    def finishRow(): Unit =
      finishField()
      rows += fields.toVector
      fields.clear()
    while offset < contents.length && rows.size < recordBudget && failure.isEmpty do
      val character = contents.charAt(offset)
      if quoted then
        if character == '"' then
          if offset + 1 < contents.length && contents.charAt(offset + 1) == '"' then
            field.append('"')
            offset += 1
          else
            quoted = false
            closed = true
        else field.append(character)
      else if closed then
        character match
          case ',' => finishField()
          case '\r' if offset + 1 < contents.length && contents.charAt(offset + 1) == '\n' =>
            finishRow()
            offset += 1
          case '\n'  => finishRow()
          case other => failure = Some(TidyCsvError.MalformedCsv(offset, other))
      else
        character match
          case ','                      => finishField()
          case '"' if field.length == 0 => quoted = true
          case '"' => failure = Some(TidyCsvError.MalformedCsv(offset, character))
          case '\r' if offset + 1 < contents.length && contents.charAt(offset + 1) == '\n' =>
            finishRow()
            offset += 1
          case '\n'  => finishRow()
          case other => field.append(other)
      offset += 1
    failure match
      case Some(error)                                 => Left(error)
      case None if offset == contents.length && quoted =>
        Left(TidyCsvError.UnterminatedQuotedField(offset))
      case None =>
        if offset == contents.length && (field.length > 0 || fields.nonEmpty) then finishRow()
        Right(
          CsvPage(
            rows.toVector,
            Option.when(offset < contents.length)(new CsvCursor(contents, offset))
          )
        )

private[io] object CsvCursor:
  def start(contents: String): CsvCursor = new CsvCursor(contents, 0)

private[io] final case class CsvPage(rows: Vector[Vector[String]], next: Option[CsvCursor])

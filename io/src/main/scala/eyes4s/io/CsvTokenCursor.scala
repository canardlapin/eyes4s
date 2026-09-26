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

private[io] enum CsvTokenError derives CanEqual:
  case Csv(error: TidyCsvError)
  case Resource(error: LedgerResourceError)

private[io] enum CsvTokenMode derives CanEqual:
  case Plain, Quoted, AfterQuote, BeforeLf

/** An immutable CSV position, including positions inside an escaped or quoted
  * field. One work unit consumes one UTF-16 code unit or handles end-of-input.
  * A step emits at most one record. Finishing a field/record materializes only
  * the explicitly bounded field/column envelope; no source-wide finalizer runs.
  */
private[io] final case class CsvTokenCursor private (
    contents: String,
    source: LedgerResourceSource,
    limits: LedgerExecutionLimits,
    index: Int,
    records: Long,
    encoded: Long,
    retained: Long,
    fields: Vector[String],
    field: Vector[Char],
    mode: CsvTokenMode
):
  def advance(quantum: SampleQuantum): Either[CsvTokenError, CsvTokenStep] =
    var at                             = index
    var completed                      = records
    var size                           = encoded
    var kept                           = retained
    var cells                          = fields
    var token                          = field
    var state                          = mode
    var units                          = 0
    var row: Option[Vector[String]]    = None
    var done                           = false
    var failure: Option[CsvTokenError] = None

    def location: LedgerResourceLocation =
      LedgerResourceLocation(source, Some(completed + 1), Some(cells.size.toLong + 1))
    def check(dimension: LedgerResource, observed: Long, recordOnly: Boolean = false): Boolean =
      limits.check(
        dimension,
        observed,
        if recordOnly then location.copy(field = None) else location
      ) match
        case Left(error) => failure = Some(CsvTokenError.Resource(error)); false
        case Right(())   => true
    def retain(recordOnly: Boolean = false): Boolean =
      val at = if recordOnly then location.copy(field = None) else location
      limits.add(LedgerResource.RetainedEvidenceUnits, kept, 1, at) match
        case Left(error)  => failure = Some(CsvTokenError.Resource(error)); false
        case Right(value) => kept = value; true
    def append(character: Char): Unit =
      if check(LedgerResource.Columns, cells.size.toLong + 1) &&
        check(LedgerResource.FieldCodeUnits, token.size.toLong + 1) && retain()
      then token = token :+ character
    def finishField(): Unit =
      if check(LedgerResource.Columns, cells.size.toLong + 1) && retain() then
        cells = cells :+ token.mkString
        token = Vector.empty
        state = CsvTokenMode.Plain
    def finishRow(): Unit =
      if check(LedgerResource.LogicalRecords, completed + 1, recordOnly = true) then
        finishField()
        if failure.isEmpty && retain(recordOnly = true) then
          row = Some(cells)
          cells = Vector.empty
          completed += 1
          size = 0
    def comma(): Unit =
      finishField()
      if failure.isEmpty then
        val _ = check(LedgerResource.Columns, cells.size.toLong + 1)
    def delimiter(character: Char, closed: Boolean): Unit = character match
      case ','                                                                 => comma()
      case '\n'                                                                => finishRow()
      case '\r' if at + 1 < contents.length && contents.charAt(at + 1) == '\n' =>
        state = CsvTokenMode.BeforeLf
      case other if closed =>
        failure = Some(CsvTokenError.Csv(TidyCsvError.MalformedCsv(at, other)))
      case '"' if token.isEmpty => state = CsvTokenMode.Quoted
      case '"'   => failure = Some(CsvTokenError.Csv(TidyCsvError.MalformedCsv(at, character)))
      case other => append(other)

    while units < quantum.value && row.isEmpty && !done && failure.isEmpty do
      units += 1
      if at == contents.length then
        done = true
        if state == CsvTokenMode.Quoted then
          failure = Some(CsvTokenError.Csv(TidyCsvError.UnterminatedQuotedField(at)))
        else if token.nonEmpty || cells.nonEmpty then finishRow()
      else if check(LedgerResource.EncodedRecordCodeUnits, size + 1) then
        size += 1
        val character = contents.charAt(at)
        state match
          case CsvTokenMode.Quoted =>
            if character == '"' then state = CsvTokenMode.AfterQuote else append(character)
          case CsvTokenMode.AfterQuote =>
            if character == '"' then
              append('"')
              state = CsvTokenMode.Quoted
            else delimiter(character, closed = true)
          case CsvTokenMode.BeforeLf => finishRow()
          case CsvTokenMode.Plain    => delimiter(character, closed = false)
        at += 1
    failure.toLeft(
      CsvTokenStep(
        units,
        row,
        Option.unless(done)(
          new CsvTokenCursor(
            contents,
            source,
            limits,
            at,
            completed,
            size,
            kept,
            cells,
            token,
            state
          )
        )
      )
    )

private[io] object CsvTokenCursor:
  def start(
      contents: String,
      source: LedgerResourceSource,
      limits: LedgerExecutionLimits
  ): Either[LedgerResourceError, CsvTokenCursor] =
    limits
      .check(
        LedgerResource.SourceCodeUnits,
        contents.length.toLong,
        LedgerResourceLocation(source)
      )
      .map { _ =>
        new CsvTokenCursor(
          contents,
          source,
          limits,
          0,
          0L,
          0L,
          0L,
          Vector.empty,
          Vector.empty,
          CsvTokenMode.Plain
        )
      }

private[io] final case class CsvTokenStep(
    workUnits: Int,
    row: Option[Vector[String]],
    next: Option[CsvTokenCursor]
)

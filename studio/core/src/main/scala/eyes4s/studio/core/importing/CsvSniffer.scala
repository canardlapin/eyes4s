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

package eyes4s.studio.core.importing

import eyes4s.studio.core.document.ColumnName

import scala.collection.mutable

/** The field separator of a delimited text file. */
enum Delimiter(val char: Char) derives CanEqual:
  case Comma     extends Delimiter(',')
  case Tab       extends Delimiter('\t')
  case Semicolon extends Delimiter(';')

  def label: String = this match
    case Comma     => "comma"
    case Tab       => "tab"
    case Semicolon => "semicolon"

/** Why a file could not be previewed; each case names the file and what in it
  * failed. A preview is a look at the file, not its admission: eyes4s reads
  * every record again when the dataset is admitted.
  */
enum SniffError derives CanEqual:
  /** The file has no header line. */
  case Empty(file: String)

  /** A quoted field opened in `record` is never closed. */
  case UnterminatedQuote(file: String, record: Int)

  /** No candidate separator splits the header into more than one column and
    * any head record into as many fields; `header` is the header line.
    */
  case NoDelimiter(file: String, header: String)

  /** The bytes are not UTF-8: the first malformed sequence starts at byte
    * `offset` (0-based).
    */
  case NotUtf8(file: String, offset: Long)

  /** The file starts with a UTF-16 byte-order mark; import reads UTF-8. */
  case Utf16(file: String)

  /** The header's column `position` (1-based) is blank. */
  case BlankColumn(file: String, position: Int)

  /** The header names `column` more than once, at `positions` (1-based). */
  case RepeatedColumn(file: String, column: String, positions: Vector[Int])

  def message: String = this match
    case Empty(file)                     => s"$file is empty: it has no header line."
    case UnterminatedQuote(file, record) =>
      s"$file: a quoted field in record $record is never closed."
    case NoDelimiter(file, header) =>
      s"$file: no comma, tab or semicolon splits the header '$header' into columns " +
        "that a first record matches."
    case NotUtf8(file, offset) =>
      s"$file is not UTF-8 text: the bytes at offset $offset are not a UTF-8 character."
    case Utf16(file) =>
      s"$file is UTF-16 text (it starts with a UTF-16 byte-order mark); save it as UTF-8."
    case BlankColumn(file, position) => s"$file: header column $position has no name."
    case RepeatedColumn(file, column, positions) =>
      s"$file: the header names column $column more than once (positions " +
        s"${positions.mkString(", ")})."

/** A record whose width differs from the header's: returned as data, never
  * dropped silently. Records are numbered from 1 in file order after the
  * header, counting non-blank records (fixtures/studio-golden README).
  */
final case class RaggedRecord(record: Int, expected: Int, actual: Int) derives CanEqual

/** One column of a preview: its header name and its values in the first
  * records, in record order.
  */
final case class PreviewColumn(name: ColumnName, samples: Vector[String]) derives CanEqual

/** What sniffing found in a delimited file: its separator, its columns with
  * the first records' values, how many records follow the header, how many
  * have a width other than the header's, and the first of those
  * ([[CsvSniffer.RaggedKept]] at most, so a preview does not grow with the
  * file).
  */
final case class CsvPreview(
    file: String,
    delimiter: Delimiter,
    columns: Vector[PreviewColumn],
    records: Int,
    raggedTotal: Int,
    ragged: Vector[RaggedRecord]
) derives CanEqual:
  def header: Vector[ColumnName] = columns.map(_.name)

  def column(name: ColumnName): Option[PreviewColumn] = columns.find(_.name == name)

/** CSV sniffing (ticket S5.2): the separator, the header and the first
  * records of a delimited text file, then one streaming pass that counts the
  * records and finds the ragged ones. Quoted fields follow RFC 4180 (a
  * doubled quote inside quotes is one quote; separators and line breaks
  * inside quotes are data). Blank records, before the header included, are
  * skipped; a leading byte-order mark is dropped. Nothing about units is
  * sniffed: time units are declared, never inferred (DESIGN_SPEC section 9).
  *
  * Only the head (the header and [[HeadRecords]] records) is parsed for each
  * candidate separator. The rest of the file is read once, a record at a
  * time, keeping only counts and the first [[RaggedKept]] ragged records.
  */
object CsvSniffer:

  /** How many records a preview keeps per column ("First records"). */
  val SampleRecords: Int = 4

  /** How many records after the header decide the separator. */
  val HeadRecords: Int = 200

  /** How many ragged records a preview lists; all are counted. */
  val RaggedKept: Int = 100

  /** Candidate separators, in preference order when several fit equally. */
  val candidates: Vector[Delimiter] = Delimiter.values.toVector

  private type Head = (Delimiter, Vector[Vector[String]])

  /** The separator the head of `text` shows, and the head's records under it
    * (header first). A separator fits when it splits the header into more
    * than one column and at least one head record (if any) into as many
    * fields; a record of another width is ragged data, reported by the scan.
    * The best fit matches the most head records, then splits the header
    * most. Reads nothing past the head.
    */
  def chooseDelimiter(file: String, text: CharSequence): Either[SniffError, Head] =
    val parsed                               = candidates.map(d => d -> head(file, text, d))
    def matching(rs: Vector[Vector[String]]) = rs.drop(1).count(_.size == rs.head.size)
    val fitting                              = parsed.collect {
      case (d, Right(rs))
          if rs.headOption.exists(_.size > 1) && (rs.size == 1 || matching(rs) > 0) =>
        (d, rs)
    }
    fitting.sortBy((d, rs) => (-matching(rs), -rs.head.size, d.ordinal)).headOption match
      case Some(found) => Right(found)
      case None        =>
        // A quote error explains the failure better than "no separator".
        val quote = parsed.collectFirst { case (_, Left(e)) => e }
        val first = parsed.collectFirst { case (_, Right(rs)) if rs.nonEmpty => rs.head }
        (quote, first) match
          case (Some(e), _)      => Left(e)
          case (None, Some(hdr)) => Left(SniffError.NoDelimiter(file, hdr.mkString))
          case (None, None)      => Left(SniffError.Empty(file))

  def sniff(
      file: String,
      text: CharSequence,
      samples: Int = SampleRecords
  ): Either[SniffError, CsvPreview] =
    for
      chosen <- chooseDelimiter(file, text)
      (delimiter, headRecords) = chosen
      header                   = headRecords.head
      names <- columnNames(file, header)
      scan  <- scan(file, text, delimiter, header.size)
    yield
      val sampled = headRecords.slice(1, 1 + samples)
      // A short record shows a blank cell, so every sample stays aligned
      // with its record number.
      val columns = names.zipWithIndex.map((name, i) =>
        PreviewColumn(name, sampled.map(_.lift(i).getOrElse("")))
      )
      CsvPreview(file, delimiter, columns, scan.records, scan.raggedTotal, scan.ragged)

  private def columnNames(
      file: String,
      header: Vector[String]
  ): Either[SniffError, Vector[ColumnName]] =
    for
      names <- header.zipWithIndex
        .foldLeft[Either[SniffError, Vector[ColumnName]]](Right(Vector.empty)) {
          case (acc, (raw, i)) =>
            acc.flatMap(done =>
              ColumnName
                .of(raw.trim)
                .left
                .map(_ => SniffError.BlankColumn(file, i + 1))
                .map(done :+ _)
            )
        }
      _ <- names.distinct
        .collectFirst {
          case name if names.count(_ == name) > 1 =>
            SniffError.RepeatedColumn(
              file,
              name.value,
              names.zipWithIndex.collect { case (n, i) if n == name => i + 1 }
            )
        }
        .toLeft(())
    yield names

  private final case class Scan(records: Int, raggedTotal: Int, ragged: Vector[RaggedRecord])

  /** One pass over every record after the header: the count and the ragged. */
  private def scan(
      file: String,
      text: CharSequence,
      delimiter: Delimiter,
      width: Int
  ): Either[SniffError, Scan] =
    val reader = RecordReader(file, text, delimiter)
    val ragged = Vector.newBuilder[RaggedRecord]
    var kept   = 0
    var total  = 0
    var count  = 0
    reader.next(): Unit // the header
    reader
      .fold(Right(())) { fields =>
        count += 1
        if fields.size != width then
          total += 1
          if kept < RaggedKept then
            ragged += RaggedRecord(count, width, fields.size)
            kept += 1
        true
      }
      .map(_ => Scan(count, total, ragged.result()))

  /** The header and up to [[HeadRecords]] records under `delimiter`. */
  private def head(
      file: String,
      text: CharSequence,
      delimiter: Delimiter
  ): Either[SniffError, Vector[Vector[String]]] =
    val out = Vector.newBuilder[Vector[String]]
    var n   = 0
    RecordReader(file, text, delimiter)
      .fold(Right(())) { fields =>
        out += fields
        n += 1
        n <= HeadRecords
      }
      .map(_ => out.result())

  /** Every non-blank record of `text` split by `delimiter`, the header
    * first. It parses the whole text: for small inputs.
    */
  def records(
      file: String,
      text: CharSequence,
      delimiter: Delimiter
  ): Either[SniffError, Vector[Vector[String]]] =
    val out = Vector.newBuilder[Vector[String]]
    RecordReader(file, text, delimiter)
      .fold(Right(()))(fields =>
        out += fields
        true
      )
      .map(_ => out.result())

/** Reads `text` one record at a time, skipping blank records and a leading
  * byte-order mark. The header is record 0.
  */
private[importing] final class RecordReader(
    file: String,
    text: CharSequence,
    delimiter: Delimiter
):
  private var at     = if text.length > 0 && text.charAt(0) == '﻿' then 1 else 0
  private var record = 0
  private val sep    = delimiter.char

  /** Feed records to `f` until it returns false or the text ends; the first
    * parse error stops the fold.
    */
  def fold(ok: Either[SniffError, Unit])(
      f: Vector[String] => Boolean
  ): Either[SniffError, Unit] =
    var result = ok
    var more   = true
    while more && result.isRight do
      next() match
        case None            => more = false
        case Some(Left(e))   => result = Left(e)
        case Some(Right(fs)) => more = f(fs)
    result

  /** The next non-blank record, or `None` at the end of the text. */
  def next(): Option[Either[SniffError, Vector[String]]] =
    var found = Option.empty[Either[SniffError, Vector[String]]]
    while found.isEmpty && at < text.length do
      one() match
        case Right(fields) if fields == Vector("") => ()
        case other                                 => found = Some(other)
    found.foreach {
      case Right(_) => record += 1
      case Left(_)  => at = text.length
    }
    found

  /** One physical record from `at`, which moves past its end. */
  private def one(): Either[SniffError, Vector[String]] =
    val fields = mutable.ArrayBuffer.empty[String]
    val field  = new StringBuilder
    var quoted = false
    var done   = false
    while !done && at < text.length do
      val c = text.charAt(at)
      if quoted then
        if c == '"' then
          if at + 1 < text.length && text.charAt(at + 1) == '"' then
            field += '"'
            at += 1
          else quoted = false
        else field += c
      else if c == '"' then quoted = true
      else if c == sep then
        fields += field.toString
        field.clear()
      else if c == '\n' then done = true
      else if c == '\r' then
        if at + 1 < text.length && text.charAt(at + 1) == '\n' then at += 1
        done = true
      else field += c
      at += 1
    if quoted then Left(SniffError.UnterminatedQuote(file, record))
    else
      fields += field.toString
      Right(fields.toVector)

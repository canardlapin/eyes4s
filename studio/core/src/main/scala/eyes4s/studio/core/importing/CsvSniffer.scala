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
    * any sampled record into as many fields; `header` is the first line.
    */
  case NoDelimiter(file: String, header: String)

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
        "that every sampled record matches."
    case BlankColumn(file, position) => s"$file: header column $position has no name."
    case RepeatedColumn(file, column, positions) =>
      s"$file: the header names column $column more than once (positions " +
        s"${positions.mkString(", ")})."

/** A record whose width differs from the header's: returned as data, never
  * dropped silently. Record numbers are 1-based in file order, excluding the
  * header (fixtures/studio-golden README).
  */
final case class RaggedRecord(record: Int, expected: Int, actual: Int) derives CanEqual

/** One column of a preview: its header name and its values in the first
  * records, in record order.
  */
final case class PreviewColumn(name: ColumnName, samples: Vector[String]) derives CanEqual

/** What sniffing found in a delimited file: its separator, its columns with
  * the first records' values, how many records follow the header, and every
  * record whose width is not the header's.
  */
final case class CsvPreview(
    file: String,
    delimiter: Delimiter,
    columns: Vector[PreviewColumn],
    records: Int,
    ragged: Vector[RaggedRecord]
) derives CanEqual:
  def header: Vector[ColumnName] = columns.map(_.name)

  def column(name: ColumnName): Option[PreviewColumn] = columns.find(_.name == name)

/** CSV sniffing (ticket S5.2): the separator, the header and the first
  * records of a delimited text file. Quoted fields follow RFC 4180 (a doubled
  * quote inside quotes is one quote; separators and line breaks inside quotes
  * are data). A leading byte-order mark is dropped. Nothing about units is
  * sniffed: time units are declared, never inferred (DESIGN_SPEC section 9).
  */
object CsvSniffer:

  /** How many records a preview keeps per column ("First records"). */
  val SampleRecords: Int = 4

  /** Candidate separators, in preference order when several fit equally. */
  val candidates: Vector[Delimiter] = Delimiter.values.toVector

  def sniff(
      file: String,
      text: String,
      samples: Int = SampleRecords
  ): Either[SniffError, CsvPreview] =
    val body  = text.stripPrefix("﻿")
    val lines = body.linesIterator.toVector
    lines.headOption.filter(_.trim.nonEmpty) match
      case None             => Left(SniffError.Empty(file))
      case Some(headerLine) =>
        val parsed = candidates.map(d => d -> records(file, body, d))
        // A separator fits when it splits the header into more than one
        // column and at least one sampled record (if any) into as many
        // fields; a record of another width is ragged data, reported. The
        // best fit matches the most sampled records, then splits the header
        // most.
        def matching(rs: Vector[Vector[String]]) =
          rs.slice(1, 1 + samples).count(_.size == rs.head.size)
        val fitting = parsed.collect {
          case (d, Right(rs))
              if rs.headOption.exists(_.size > 1) &&
                (rs.size == 1 || matching(rs) > 0) =>
            (d, rs)
        }
        fitting.sortBy((d, rs) => (-matching(rs), -rs.head.size, d.ordinal)).headOption match
          case Some((d, rs)) => preview(file, d, rs, samples)
          case None          =>
            // A quote error under the separator that splits the header most
            // explains the failure better than "no separator".
            parsed
              .collectFirst { case (_, Left(e: SniffError.UnterminatedQuote)) => e }
              .fold[Either[SniffError, CsvPreview]](
                Left(SniffError.NoDelimiter(file, headerLine))
              )(Left(_))

  private def preview(
      file: String,
      delimiter: Delimiter,
      rows: Vector[Vector[String]],
      samples: Int
  ): Either[SniffError, CsvPreview] =
    val header = rows.head
    val data   = rows.tail
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
      _ <- names.zipWithIndex
        .groupBy(_._1)
        .toVector
        .sortBy(_._2.head._2)
        .collectFirst {
          case (name, at) if at.size > 1 =>
            SniffError.RepeatedColumn(file, name.value, at.map(_._2 + 1))
        }
        .toLeft(())
    yield
      val sampled = data.take(samples)
      val columns =
        // A short record shows a blank cell, so every sample stays aligned
        // with its record number.
        names.zipWithIndex.map((name, i) =>
          PreviewColumn(name, sampled.map(_.lift(i).getOrElse("")))
        )
      val ragged = data.zipWithIndex.collect {
        case (r, i) if r.size != header.size => RaggedRecord(i + 1, header.size, r.size)
      }
      CsvPreview(file, delimiter, columns, data.size, ragged)

  /** Every non-blank record of `text` split by `delimiter`; the header is the
    * first. A record is numbered from 1 after the header.
    */
  def records(
      file: String,
      text: String,
      delimiter: Delimiter
  ): Either[SniffError, Vector[Vector[String]]] =
    val out               = Vector.newBuilder[Vector[String]]
    val fields            = mutable.ArrayBuffer.empty[String]
    val field             = new StringBuilder
    var quoted            = false
    var opened            = 0     // the record a quote was opened in (0 = header)
    var record            = 0
    var i                 = 0
    var touched           = false // the current record has any character
    val sep               = delimiter.char
    def endRecord(): Unit =
      fields += field.toString
      field.clear()
      if touched || fields.exists(_.nonEmpty) then out += fields.toVector
      fields.clear()
      touched = false
      record += 1
    while i < text.length do
      val c = text.charAt(i)
      if quoted then
        if c == '"' then
          if i + 1 < text.length && text.charAt(i + 1) == '"' then
            field += '"'
            i += 1
          else quoted = false
        else field += c
      else if c == '"' then
        quoted = true
        opened = record
        touched = true
      else if c == sep then
        fields += field.toString
        field.clear()
        touched = true
      else if c == '\n' then endRecord()
      else if c == '\r' then
        if i + 1 < text.length && text.charAt(i + 1) == '\n' then i += 1
        endRecord()
      else
        field += c
        touched = true
      i += 1
    if quoted then Left(SniffError.UnterminatedQuote(file, opened))
    else
      if touched || field.nonEmpty then endRecord()
      Right(out.result())

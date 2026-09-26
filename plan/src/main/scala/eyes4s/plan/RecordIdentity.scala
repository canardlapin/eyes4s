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

package eyes4s.plan

import scala.annotation.tailrec

/** Why a record, line or fixation identity was refused. Every case names the
  * value it refused.
  */
enum RecordIdentityError derives CanEqual:
  case DataRecordOutOfRange(value: Int, maximum: Int)
  case CsvRecordNotPositive(value: Int)

  /** A data record was asked of the header record. */
  case HeaderRecord(record: CsvRecord)
  case SourceLineNotPositive(value: Long)
  case LineSpanOrder(first: SourceLine, last: SourceLine)
  case ScanpathPositionOutOfRange(value: Int, maximum: Int)
  case FixationNumberNotPositive(value: Int)
  case RecordCountOutOfRange(records: Int, maximum: Int)

  /** A layout without its header record. */
  case NoHeaderRecord

  /** A record said to occupy fewer than one line. */
  case RecordLineCount(record: CsvRecord, lines: Int)
  case RecordBeyond(record: DataRecord, records: Int)
  case LineBeyond(line: SourceLine, lines: Long)

  def message: String = this match
    case DataRecordOutOfRange(v, max) =>
      s"Data record $v is outside 1 to $max (data records count from 1, header excluded)."
    case CsvRecordNotPositive(v) =>
      s"CSV record $v is not positive (CSV records count from 1, the header being record 1)."
    case HeaderRecord(record) =>
      s"CSV record ${record.value} is the header, not a data record."
    case SourceLineNotPositive(v)   => s"Source line $v is not positive (lines count from 1)."
    case LineSpanOrder(first, last) =>
      s"Line span ends at line ${last.value}, before its first line ${first.value}."
    case ScanpathPositionOutOfRange(v, max) =>
      s"Scanpath position $v is outside 0 to $max (positions count from 0)."
    case FixationNumberNotPositive(v) =>
      s"Fixation number $v is not positive (fixations count from 1)."
    case RecordCountOutOfRange(n, max) =>
      s"A layout has 0 to $max data records, not $n."
    case NoHeaderRecord => "A record layout needs its header record; it has no records."
    case RecordLineCount(record, lines) =>
      s"CSV record ${record.value} is said to occupy $lines lines; every record occupies one or more."
    case RecordBeyond(record, records) =>
      s"Data record ${record.value} is beyond the layout's $records data records."
    case LineBeyond(line, lines) => s"Line ${line.value} is beyond the layout's $lines lines."

/** A data record of a delimited source: the n-th record after the header,
  * counted from 1. "fixations.csv record 7,214" is `DataRecord` 7,214. The
  * header is never a data record; it is [[CsvRecord.header]].
  */
final case class DataRecord private[plan] (value: Int) derives CanEqual:
  /** The same record as a CSV record ordinal, which counts the header as
    * record 1: always this record's value plus one.
    */
  def csv: CsvRecord = new CsvRecord(value + 1)

object DataRecord:
  /** The largest data record, so that its CSV record ordinal is an `Int`. */
  val maximum: Int = Int.MaxValue - 1

  def of(value: Int): Either[RecordIdentityError, DataRecord] =
    Either.cond(
      value >= 1 && value <= maximum,
      new DataRecord(value),
      RecordIdentityError.DataRecordOutOfRange(value, maximum)
    )

  given Ordering[DataRecord] = Ordering.by(_.value)

/** A record of a delimited source in the RFC 4180 sense, counted from 1 with
  * the header as record 1. Admission ledgers, diagnostic loci and source
  * links store record numbers in this convention; [[role]] converts one to a
  * data record or identifies the header.
  */
final case class CsvRecord private[plan] (value: Int) derives CanEqual:
  /** The header, or the data record this is: always this value minus one. */
  def role: RecordRole =
    if value == 1 then RecordRole.Header else RecordRole.Data(new DataRecord(value - 1))

  /** The data record this is; the header is refused. */
  def dataRecord: Either[RecordIdentityError, DataRecord] = role match
    case RecordRole.Header       => Left(RecordIdentityError.HeaderRecord(this))
    case RecordRole.Data(record) => Right(record)

object CsvRecord:
  /** The header record. */
  val header: CsvRecord = new CsvRecord(1)

  def of(value: Int): Either[RecordIdentityError, CsvRecord] =
    Either.cond(
      value >= 1,
      new CsvRecord(value),
      RecordIdentityError.CsvRecordNotPositive(value)
    )

/** What a CSV record is: the header, or a data record. Every CSV record is
  * exactly one of them.
  */
enum RecordRole derives CanEqual:
  case Header
  case Data(record: DataRecord)

/** A physical line of a text source, counted from 1. A line ends at a line
  * feed: a CRLF ends one line, and a carriage return on its own does not end
  * a line. A record whose quoted field holds a line feed occupies more than
  * one line, so a line number is not a record number (see [[RecordLines]]).
  */
final case class SourceLine private[plan] (value: Long) derives CanEqual

object SourceLine:
  def of(value: Long): Either[RecordIdentityError, SourceLine] =
    Either.cond(
      value >= 1,
      new SourceLine(value),
      RecordIdentityError.SourceLineNotPositive(value)
    )

/** The lines a record occupies, both ends included. */
final case class LineSpan private[plan] (first: SourceLine, last: SourceLine) derives CanEqual:
  /** The number of lines, at least one. */
  def count: Long = last.value - first.value + 1

  def contains(line: SourceLine): Boolean =
    line.value >= first.value && line.value <= last.value

object LineSpan:
  def of(first: SourceLine, last: SourceLine): Either[RecordIdentityError, LineSpan] =
    Either.cond(
      last.value >= first.value,
      new LineSpan(first, last),
      RecordIdentityError.LineSpanOrder(first, last)
    )

/** A fixation's position in its trial's scanpath, counted from 0. Its display
  * form is [[FixationNumber]].
  */
final case class ScanpathPosition private[plan] (value: Int) derives CanEqual:
  /** The same fixation counted from 1: always this position plus one. */
  def number: FixationNumber = new FixationNumber(value + 1)

object ScanpathPosition:
  /** The largest position, so that its fixation number is an `Int`. */
  val maximum: Int = Int.MaxValue - 1

  def of(value: Int): Either[RecordIdentityError, ScanpathPosition] =
    Either.cond(
      value >= 0 && value <= maximum,
      new ScanpathPosition(value),
      RecordIdentityError.ScanpathPositionOutOfRange(value, maximum)
    )

/** A fixation of a trial counted from 1, as a reader says it: "fixation 6" is
  * [[ScanpathPosition]] 5.
  */
final case class FixationNumber private[plan] (value: Int) derives CanEqual:
  /** The same fixation counted from 0: always this number minus one. */
  def position: ScanpathPosition = new ScanpathPosition(value - 1)

object FixationNumber:
  def of(value: Int): Either[RecordIdentityError, FixationNumber] =
    Either.cond(
      value >= 1,
      new FixationNumber(value),
      RecordIdentityError.FixationNumberNotPositive(value)
    )

/** Where each record of a delimited text lies among its physical lines
  * ([[SourceLine]]), the header being CSV record 1. Both directions are exact
  * and total over the text: every data record has a span, and every line of
  * the text belongs to exactly one record. A record occupies more than one
  * line exactly when a quoted field holds a line feed; only such records are
  * stored, so a text without them costs nothing per record.
  *
  * `eyes4s-io` builds the layout of a text (`CsvLayout.scan`), reading record
  * boundaries as its CSV decoder does.
  */
final class RecordLines private (
    val records: Int,
    private val multi: IArray[Int],
    private val extra: IArray[Long],
    before: IArray[Long]
):
  /** The number of CSV records, the header included. */
  private def csvRecords: Int = records + 1

  /** Multi-line records strictly before CSV record `c`. */
  private def rank(c: Int): Int =
    @tailrec def go(lo: Int, hi: Int): Int =
      if lo >= hi then lo
      else
        val mid = (lo + hi) >>> 1
        if multi(mid) < c then go(mid + 1, hi) else go(lo, mid)
    go(0, multi.length)

  private def firstLine(c: Int): Long = c.toLong + before(rank(c))

  private def spanOf(c: Int): LineSpan =
    val i     = rank(c)
    val first = c.toLong + before(i)
    val more  = if i < multi.length && multi(i) == c then extra(i) else 0L
    new LineSpan(new SourceLine(first), new SourceLine(first + more))

  /** The lines of the text's records: the last line of its last record. */
  def lines: Long = csvRecords.toLong + before(multi.length)

  /** True when every record occupies one line, so a data record's line is
    * its value plus one.
    */
  def isUniform: Boolean = multi.isEmpty

  /** The lines of the header record. */
  def header: LineSpan = spanOf(1)

  /** The lines of a data record. */
  def span(record: DataRecord): Either[RecordIdentityError, LineSpan] =
    Either.cond(
      record.value <= records,
      spanOf(record.csv.value),
      RecordIdentityError.RecordBeyond(record, records)
    )

  /** The record a line belongs to: the header or a data record. */
  def owner(line: SourceLine): Either[RecordIdentityError, RecordRole] =
    if line.value > lines then Left(RecordIdentityError.LineBeyond(line, lines))
    else
      // The last CSV record whose first line is at or before `line`.
      @tailrec def go(lo: Int, hi: Int): Int =
        if lo >= hi then lo
        else
          val mid = lo + (hi - lo + 1) / 2
          if firstLine(mid) <= line.value then go(mid, hi) else go(lo, mid - 1)
      Right(new CsvRecord(go(1, csvRecords)).role)

  override def equals(other: Any): Boolean = other match
    case that: RecordLines =>
      records == that.records && multi.sameElements(that.multi) &&
      extra.sameElements(that.extra)
    case _ => false
  override def hashCode: Int    = (records, multi.toSeq, extra.toSeq).hashCode
  override def toString: String =
    s"RecordLines($records data records, $lines lines, ${multi.length} spanning more than one)"

object RecordLines:
  /** A text of one header line and `records` one-line data records. */
  def uniform(records: Int): Either[RecordIdentityError, RecordLines] =
    Either.cond(
      records >= 0 && records <= DataRecord.maximum,
      new RecordLines(records, IArray.empty, IArray.empty, IArray(0L)),
      RecordIdentityError.RecordCountOutOfRange(records, DataRecord.maximum)
    )

  /** A layout from the number of lines each CSV record occupies, the header
    * first.
    */
  def of(lineCounts: Vector[Int]): Either[RecordIdentityError, RecordLines] =
    if lineCounts.isEmpty then Left(RecordIdentityError.NoHeaderRecord)
    else
      lineCounts.zipWithIndex.collectFirst {
        case (n, i) if n < 1 => RecordIdentityError.RecordLineCount(new CsvRecord(i + 1), n)
      } match
        case Some(error) => Left(error)
        case None        =>
          val spanning = lineCounts.zipWithIndex.collect {
            case (n, i) if n > 1 => (i + 1, (n - 1).toLong)
          }
          Right(
            new RecordLines(
              lineCounts.size - 1,
              IArray.from(spanning.map(_._1)),
              IArray.from(spanning.map(_._2)),
              IArray.from(spanning.scanLeft(0L)(_ + _._2))
            )
          )

// ------------------------------------------------------------ typed accessors
//
// The ledger and the source index store record numbers as `Int` CSV record
// ordinals; these read them as the typed identities.

extension [K](entry: SourceRecord[K])
  /** The entry's record number as a CSV record ordinal. */
  def csvRecord: Either[RecordIdentityError, CsvRecord] = CsvRecord.of(entry.record)

  /** The entry's data record; an entry naming the header is refused. */
  def dataRecord: Either[RecordIdentityError, DataRecord] = csvRecord.flatMap(_.dataRecord)

extension [K](source: FixationSource[K])
  /** The data record that supplied the fixation. */
  def dataRecord: Either[RecordIdentityError, DataRecord] =
    CsvRecord.of(source.record).flatMap(_.dataRecord)

  /** The fixation's position in its trial's scanpath. */
  def position: Either[RecordIdentityError, ScanpathPosition] =
    ScanpathPosition.of(source.index)

extension [K](sources: StudySources[K])
  /** The ledger entry for one data record. */
  def entryAt(record: DataRecord): Option[SourceRecord[K]] = sources.record(record.csv.value)

  /** The trial key a data record admitted or rejected, when it named one. */
  def trialAt(record: DataRecord): Option[K] = sources.trialOf(record.csv.value)

  /** The source of the fixation at a scanpath position of a trial. */
  def fixationAt(
      key: K,
      position: ScanpathPosition
  ): Either[MissingSource[K], FixationSource[K]] =
    sources.fixation(key, position.value)

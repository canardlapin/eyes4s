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

/** One low-cardinality column of a [[KeyTable]]: its distinct values in
  * first-seen order (at most [[KeyTable.Cap]]) and, for each record, the
  * index of the record's value among them, in two bytes.
  */
final class KeyColumn private[importing] (
    val name: ColumnName,
    val values: Vector[String],
    codes: IArray[Short]
):
  /** How many records the column holds. */
  def size: Int = codes.length

  /** The index of record `index`'s value (0-based record index) in [[values]]. */
  def code(index: Int): Int = codes(index).toInt

  /** Record `index`'s value (0-based record index). */
  def value(index: Int): String = values(code(index))

  /** The bytes the column keeps: two a record, and its distinct values. */
  def footprint: Long = 2L * size + values.iterator.map(v => 40L + 2L * v.length).sum

  override def equals(other: Any): Boolean = other match
    case c: KeyColumn =>
      c.name == name && c.values == values && c.size == size &&
      (0 until size).forall(i => c.code(i) == code(i))
    case _ => false

  override def hashCode: Int = (name, values, size).##

  override def toString: String =
    s"KeyColumn(${name.value}, ${values.size} values, $size records)"

object KeyColumn:
  given CanEqual[KeyColumn, KeyColumn] = CanEqual.derived

/** The low-cardinality columns of a delimited file, dictionary-encoded in the
  * sniffer's one scan (ticket S5.3). Trial key roles (participant, phase,
  * trial, occurrence, item) take few distinct values, so the key can be
  * checked against every record without keeping the file: a column is kept
  * only while it has at most [[KeyTable.Cap]] distinct values, two bytes a
  * record; a column that exceeds the cap is dropped (`dropped`), and a key on
  * it is checked by one streaming pass over the file instead
  * ([[TrialKeyCheck.stream]]). A record shorter than the header has blank
  * trailing cells. Records are numbered as the preview numbers them.
  */
final class KeyTable private[importing] (
    val records: Int,
    val columns: Vector[KeyColumn],
    val dropped: Vector[ColumnName]
):
  def column(name: ColumnName): Option[KeyColumn] = columns.find(_.name == name)

  /** The bytes the table keeps. */
  def footprint: Long = columns.iterator.map(_.footprint).sum

  override def equals(other: Any): Boolean = other match
    case t: KeyTable => t.records == records && t.columns == columns && t.dropped == dropped
    case _           => false

  override def hashCode: Int = (records, columns, dropped).##

  override def toString: String =
    s"KeyTable($records records, kept ${columns.map(_.name.value).mkString(", ")}; " +
      s"dropped ${dropped.map(_.value).mkString(", ")})"

object KeyTable:
  given CanEqual[KeyTable, KeyTable] = CanEqual.derived

  /** The most distinct values a kept column may have. */
  val Cap: Int = 4096

  val empty: KeyTable = new KeyTable(0, Vector.empty, Vector.empty)

/** Builds a [[KeyTable]] one record at a time, inside the sniffer's scan. */
private[importing] final class KeyTableBuilder(header: Vector[ColumnName], cap: Int):
  private val width   = header.size
  private val interns = Array.fill(width)(mutable.HashMap.empty[String, Int])
  private val values  = Array.fill(width)(mutable.ArrayBuffer.empty[String])
  private val codes   = Array.fill(width)(mutable.ArrayBuilder.make[Short])
  private val live    = Array.fill(width)(true)
  private var count   = 0

  def add(fields: Vector[String]): Unit =
    var i = 0
    while i < width do
      if live(i) then
        val cell = if i < fields.size then fields(i) else ""
        val code = interns(i).getOrElseUpdate(
          cell, {
            values(i) += cell
            values(i).size - 1
          }
        )
        if code >= cap then
          // Too many distinct values for a key role: stop keeping the column.
          live(i) = false
          interns(i) = mutable.HashMap.empty
          values(i) = mutable.ArrayBuffer.empty
          codes(i) = mutable.ArrayBuilder.make[Short]
        else codes(i) += code.toShort
      i += 1
    count += 1

  def result(): KeyTable =
    val kept = header.indices.toVector.collect {
      case i if live(i) =>
        KeyColumn(header(i), values(i).toVector, IArray.unsafeFromArray(codes(i).result()))
    }
    new KeyTable(count, kept, header.indices.toVector.filterNot(live(_)).map(header))

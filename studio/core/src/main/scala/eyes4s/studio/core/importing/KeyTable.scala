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

/** One column of a [[KeyTable]]: its distinct values in first-seen order and,
  * for each record, the index of the record's value among them.
  */
final class KeyColumn private[importing] (
    val name: ColumnName,
    val values: Vector[String],
    codes: IArray[Int]
):
  /** How many records the column holds. */
  def size: Int = codes.length

  /** The index of record `index`'s value (0-based record index) in [[values]]. */
  def code(index: Int): Int = codes(index)

  /** Record `index`'s value (0-based record index). */
  def value(index: Int): String = values(codes(index))

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

/** Every record of a delimited file as a column store (ticket S5.3): the
  * trial key is checked against the whole file, live, as its columns are
  * mapped, so the file's cells are kept, each column's values once and one
  * index per cell. A record shorter than the header has blank trailing
  * cells; the cells of a longer one past the header are not kept (the
  * preview counts ragged records, and eyes4s rejects them on admission).
  * Records are numbered as the preview numbers them: from 1, after the
  * header, counting non-blank records.
  */
final class KeyTable private (val records: Int, val columns: Vector[KeyColumn]):
  def header: Vector[ColumnName] = columns.map(_.name)

  def column(name: ColumnName): Option[KeyColumn] = columns.find(_.name == name)

  override def equals(other: Any): Boolean = other match
    case t: KeyTable => t.records == records && t.columns == columns
    case _           => false

  override def hashCode: Int = (records, columns).##

  override def toString: String =
    s"KeyTable($records records, ${header.map(_.value).mkString(", ")})"

object KeyTable:
  given CanEqual[KeyTable, KeyTable] = CanEqual.derived

  /** Read every record of `text` under `delimiter`, keeping the columns of
    * `header` (the preview's header, which is the text's first record).
    */
  def read(
      file: String,
      text: CharSequence,
      delimiter: Delimiter,
      header: Vector[ColumnName]
  ): Either[SniffError, KeyTable] =
    val width   = header.size
    val interns = Vector.fill(width)(mutable.HashMap.empty[String, Int])
    val values  = Vector.fill(width)(mutable.ArrayBuffer.empty[String])
    val codes   = Vector.fill(width)(mutable.ArrayBuilder.make[Int])
    var count   = 0
    val reader  = RecordReader(file, text, delimiter)
    reader.next(): Unit // the header
    reader
      .fold(Right(())) { fields =>
        var i = 0
        while i < width do
          val cell = if i < fields.size then fields(i) else ""
          val code = interns(i).getOrElseUpdate(
            cell, {
              values(i) += cell
              values(i).size - 1
            }
          )
          codes(i) += code
          i += 1
        count += 1
        true
      }
      .map { _ =>
        val columns = header.indices.toVector.map(i =>
          KeyColumn(header(i), values(i).toVector, IArray.unsafeFromArray(codes(i).result()))
        )
        new KeyTable(count, columns)
      }

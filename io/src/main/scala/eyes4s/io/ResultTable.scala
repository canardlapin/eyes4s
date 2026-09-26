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

import eyes4s.results.TableJson
import io.circe.{Json, JsonNumber}

// The result-table layer lives in eyes4s-results (pure, no JSON library), so a
// Scala.js client reads the same tables. io keeps the circe-facing entry point
// and the CSV and Arrow transports; these names keep existing io code
// compiling against the one layer.

/** See [[eyes4s.results.ResultFamily]]. */
type ResultFamily = eyes4s.results.ResultFamily
val ResultFamily: eyes4s.results.ResultFamily.type = eyes4s.results.ResultFamily

/** See [[eyes4s.results.ResultColumnType]]. */
type ResultColumnType = eyes4s.results.ResultColumnType
val ResultColumnType: eyes4s.results.ResultColumnType.type = eyes4s.results.ResultColumnType

/** See [[eyes4s.results.ResultColumn]]. */
type ResultColumn = eyes4s.results.ResultColumn
val ResultColumn: eyes4s.results.ResultColumn.type = eyes4s.results.ResultColumn

/** See [[eyes4s.results.ResultCell]]. */
type ResultCell = eyes4s.results.ResultCell
val ResultCell: eyes4s.results.ResultCell.type = eyes4s.results.ResultCell

/** The one result table, [[eyes4s.results.ResultTable]]. */
type ResultTable = eyes4s.results.ResultTable

enum ResultExportError derives CanEqual:
  case Schema(columns: Vector[String], reason: String)
  case Cell(row: Int, column: String, value: ResultCell, reason: String)
  case Width(row: Int, expected: Int, actual: Int)
  case Codec(underlying: eyes4s.codec.CodecError)
  case Score(underlying: ContrastExportError)
  case Context(operand: String, reason: String)
  def message: String = this match
    case Schema(c, r)     => s"Export columns=$c: $r."
    case Cell(i, c, v, r) => s"Export row=$i column=$c value=$v: $r."
    case Width(i, e, a)   => s"Export row=$i width=$a, expected=$e."
    case Codec(e)         => e.message
    case Score(e)         => e.message
    case Context(o, r)    => s"Export context $o: $r."

object ResultExportError:
  /** The same refusal, as io has always reported it. */
  def of(error: eyes4s.results.ResultTableError): ResultExportError = error match
    case eyes4s.results.ResultTableError.Schema(c, r)     => Schema(c, r)
    case eyes4s.results.ResultTableError.Cell(i, c, v, r) => Cell(i, c, v, r)
    case eyes4s.results.ResultTableError.Width(i, e, a)   => Width(i, e, a)
    case eyes4s.results.ResultTableError.Context(o, r)    => Context(o, r)

/** The circe-facing constructor of [[eyes4s.results.ResultTable]]. */
object ResultTable:
  val schema: String = eyes4s.results.ResultTable.schema

  /** A checked table over a circe context (see
    * [[eyes4s.results.ResultTable.of]]). JSON cells are admitted by circe's
    * reader and kept in their canonical spelling, so identities are those io
    * has always written.
    */
  def of(
      family: ResultFamily,
      columns: Vector[ResultColumn],
      rows: Vector[Vector[ResultCell]],
      context: Json
  ): Either[ResultExportError, ResultTable] =
    // circe decides which JSON cells are readable, as it always has: a cell it
    // reads is passed on in its canonical spelling, and one it refuses is
    // passed on as an empty text, which the table refuses at the same row and
    // column. The refusal names the cell as it was supplied.
    val admitted = rows.map(row =>
      if row.size != columns.size then row
      else
        row.zip(columns).map { (v, c) =>
          v match
            case ResultCell.Text(s) if c.kind == ResultColumnType.JsonUtf8 =>
              ResultCell.Text(
                io.circe.parser.parse(s).fold(_ => "", j => ResultTableJson.canonical(j))
              )
            case _ => v
        }
    )
    eyes4s.results.ResultTable
      .of(family, columns, admitted, ResultTableJson.of(context))
      .left
      .map {
        case eyes4s.results.ResultTableError.Cell(i, c, _, reason) =>
          ResultExportError.Cell(i, c, rows(i)(columns.indexWhere(_.name == c)), reason)
        case other => ResultExportError.of(other)
      }

/** Conversions between circe documents and table JSON. */
object ResultTableJson:
  /** The same document as table JSON; each number keeps circe's spelling. */
  def of(json: Json): TableJson = json.fold(
    TableJson.Null,
    TableJson.Bool(_),
    n =>
      TableJson.Number
        .of(n.toString)
        .orElse(n.toBigDecimal.flatMap(d => TableJson.Number.of(d.bigDecimal.toString)))
        .getOrElse(TableJson.Text(n.toString)),
    TableJson.Text(_),
    items => TableJson.Arr(items.map(of)),
    members => TableJson.Obj(members.toVector.map((k, v) => k -> of(v)))
  )

  /** The same document as circe JSON; each number keeps its spelling. */
  def circe(json: TableJson): Json = json match
    case TableJson.Null        => Json.Null
    case TableJson.Bool(b)     => Json.fromBoolean(b)
    case TableJson.Number(t)   => Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe(t))
    case TableJson.Text(s)     => Json.fromString(s)
    case TableJson.Arr(items)  => Json.arr(items.map(circe)*)
    case TableJson.Obj(fields) => Json.fromFields(fields.map((k, v) => k -> circe(v)))

  /** The canonical spelling a table identity is taken over. */
  def canonical(json: Json): String = of(json).canonical

extension (table: ResultTable)
  /** The table as RFC 4180 CSV: the table digest, then each column, with an
    * explicit `__valid` column after each nullable one.
    */
  def csv: TidyCsvDocument = new TidyCsvDocument(table.csvHeader, table.csvRows)

extension (json: TableJson)
  /** The same document as circe JSON. */
  def circe: Json = ResultTableJson.circe(json)

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

package eyes4s.studio.app.plot

import eyes4s.studio.app.text.Format
import eyes4s.studio.core.selection.StudioRef

/** Why a plot's value source was refused. Every case names the columns, rows
  * or values it refused.
  */
enum PlotSourceError derives CanEqual:
  case BlankColumnId(value: String)
  case NoColumns(caption: String)
  case DuplicateColumn(column: String)
  case DuplicateRow(ref: StudioRef, first: Int, second: Int)
  case RowArity(ref: StudioRef, values: Int, columns: Int)
  case NotFinite(ref: StudioRef, column: String, value: Double)
  case TextInNumericColumn(ref: StudioRef, column: String, text: String)
  case NumberInLabelColumn(ref: StudioRef, column: String, value: Double)
  case NegativeDecimals(column: String, decimals: Int)
  case NotWhole(ref: StudioRef, column: String, value: Double)

  def message: String = this match
    case BlankColumnId(v)             => s"column id '$v' is blank"
    case NoColumns(c)                 => s"the value source '$c' has no columns"
    case DuplicateColumn(c)           => s"column '$c' appears more than once"
    case DuplicateRow(ref, a, b)      => s"rows $a and $b are both $ref"
    case RowArity(ref, n, columns)    => s"row $ref has $n values for $columns columns"
    case NotFinite(ref, c, v)         => s"row $ref, column '$c': $v is not a finite number"
    case TextInNumericColumn(r, c, t) =>
      s"row $r, column '$c': text '$t' in a numeric column"
    case NumberInLabelColumn(r, c, v) =>
      s"row $r, column '$c': number $v in a label column"
    case NegativeDecimals(c, d) => s"column '$c' shows $d decimal places"
    case NotWhole(r, c, v)      => s"row $r, column '$c': $v is not a whole count"

/** A column's identity within its source, such as "d" or "participant". */
final case class ColumnId private (value: String) derives CanEqual

object ColumnId:
  def of(value: String): Either[PlotSourceError, ColumnId] =
    Either.cond(
      value.trim.nonEmpty,
      new ColumnId(value.trim),
      PlotSourceError.BlankColumnId(value)
    )

/** How a column's values are written, in the boards' conventions
  * ([[eyes4s.studio.app.text.Format]]).
  */
enum ColumnFormat derives CanEqual:
  /** Text: a participant, a trial, a group. */
  case Label

  /** "0.38", "−0.08". */
  case Decimal(places: Int)

  /** A difference with its sign: "+0.38", "−0.08". */
  case Signed(places: Int)

  /** A whole count, grouped: "21,400". */
  case Count

  def numeric: Boolean = this != Label

/** One column of a value source: its id, the header the table shows, and how
  * its values are written.
  */
final case class PlotColumn(id: ColumnId, header: String, format: ColumnFormat) derives CanEqual

/** One value of a source. A missing value is not zero: a plot draws it
  * differently or not at all, and the table writes [[PlotSource.MissingText]].
  */
enum PlotValue derives CanEqual:
  case Number(value: Double)
  case Text(value: String)
  case Missing

/** One row of a source: the scientific identity every number in it traces
  * to, and one value per column.
  */
final case class PlotRow(ref: StudioRef, values: Vector[PlotValue]) derives CanEqual

/** The one value source of a plot and its TableTwin (ticket S4.5a).
  *
  * A plot's marks and its Table tab both read this: the plot places marks at
  * a row's numbers, the table writes the same numbers with [[text]], and a
  * focused mark speaks the same words as its row ([[rowText]]). Neither
  * computes a value; every value comes from an eyes4s result or the fake
  * backend and traces to its row's [[StudioRef]].
  *
  * Rows are in the order the table lists them and the plot orders its marks;
  * each ref appears once, every row has one value per column, a numeric
  * column holds numbers or missing values, every number is finite, and a
  * count is whole, so what the table writes is what the plot places.
  */
final case class PlotSource private (
    caption: String,
    columns: Vector[PlotColumn],
    rows: Vector[PlotRow]
) derives CanEqual:

  private lazy val columnIndex: Map[ColumnId, Int] = columns.map(_.id).zipWithIndex.toMap
  private lazy val rowIndex: Map[StudioRef, Int]   = rows.map(_.ref).zipWithIndex.toMap

  /** The position of column `id`. */
  def indexOf(id: ColumnId): Option[Int] = columnIndex.get(id)

  /** The position of the row of `ref`. */
  def rowOf(ref: StudioRef): Option[Int] = rowIndex.get(ref)

  /** The value of row `row` in column `id`. */
  def value(row: Int, id: ColumnId): Option[PlotValue] =
    for
      r <- rows.lift(row)
      c <- indexOf(id)
    yield r.values(c)

  /** The number of row `row` in column `id`, if it has one. */
  def number(row: Int, id: ColumnId): Option[Double] =
    value(row, id).collect { case PlotValue.Number(v) => v }

  /** Row `row`'s value in column `column` as the table writes it. */
  def text(row: Int, column: Int): String =
    PlotSource.write(rows(row).values(column), columns(column).format)

  /** Every cell of row `row`, as the table writes them. */
  def cells(row: Int): Vector[String] = columns.indices.map(text(row, _)).toVector

  /** The words of row `row`: each header with its value. The table row and
    * the plot mark of this row both say exactly this.
    */
  def rowText(row: Int): String =
    columns
      .zip(cells(row))
      .map((c, v) => PlotText(PlotTextId.Cell, c.header, v))
      .mkString(PlotText(PlotTextId.CellSeparator))

object PlotSource:

  /** What the table writes for a missing value. */
  val MissingText: String = "—"

  /** A value in `format`: the one formatter of plot values. */
  def write(value: PlotValue, format: ColumnFormat): String = (value, format) match
    case (PlotValue.Missing, _)                          => MissingText
    case (PlotValue.Text(t), _)                          => t
    case (PlotValue.Number(v), ColumnFormat.Decimal(dp)) => Format.decimal(v, dp)
    case (PlotValue.Number(v), ColumnFormat.Signed(dp))  => Format.signed(v, dp)
    case (PlotValue.Number(v), ColumnFormat.Count)       => Format.count(math.round(v))
    case (PlotValue.Number(v), ColumnFormat.Label)       => Format.decimal(v, 2)

  def apply(
      caption: String,
      columns: Vector[PlotColumn],
      rows: Vector[PlotRow]
  ): Either[PlotSourceError, PlotSource] =
    def each[A](as: Vector[A])(f: A => Either[PlotSourceError, Unit]) =
      as.foldLeft[Either[PlotSourceError, Unit]](Right(()))((acc, a) => acc.flatMap(_ => f(a)))
    def cell(ref: StudioRef, column: PlotColumn, v: PlotValue): Either[PlotSourceError, Unit] =
      val name = column.id.value
      (v, column.format) match
        case (PlotValue.Number(x), _) if !x.isFinite =>
          Left(PlotSourceError.NotFinite(ref, name, x))
        case (PlotValue.Number(x), ColumnFormat.Label) =>
          Left(PlotSourceError.NumberInLabelColumn(ref, name, x))
        case (PlotValue.Number(x), ColumnFormat.Count) if x != math.rint(x) =>
          Left(PlotSourceError.NotWhole(ref, name, x))
        case (PlotValue.Text(t), f) if f.numeric =>
          Left(PlotSourceError.TextInNumericColumn(ref, name, t))
        case _ => Right(())
    val firstRow = rows.map(_.ref).zipWithIndex.groupMap(_._1)(_._2)
    for
      _ <- Either.cond(columns.nonEmpty, (), PlotSourceError.NoColumns(caption))
      _ <- columns
        .groupBy(_.id)
        .collectFirst { case (id, cs) if cs.size > 1 => id }
        .toLeft(())
        .left
        .map(id => PlotSourceError.DuplicateColumn(id.value))
      _ <- each(columns)(c =>
        c.format match
          case ColumnFormat.Decimal(d) if d < 0 =>
            Left(PlotSourceError.NegativeDecimals(c.id.value, d))
          case ColumnFormat.Signed(d) if d < 0 =>
            Left(PlotSourceError.NegativeDecimals(c.id.value, d))
          case _ => Right(())
      )
      _ <- rows
        .map(_.ref)
        .distinct
        .flatMap(ref =>
          firstRow(ref) match
            case a +: b +: _ => Some(PlotSourceError.DuplicateRow(ref, a, b))
            case _           => None
        )
        .headOption
        .toLeft(())
      _ <- each(rows)(r =>
        Either.cond(
          r.values.size == columns.size,
          (),
          PlotSourceError.RowArity(r.ref, r.values.size, columns.size)
        )
      )
      _ <- each(rows)(r => each(columns.zip(r.values))((c, v) => cell(r.ref, c, v)))
    yield new PlotSource(caption, columns, rows)

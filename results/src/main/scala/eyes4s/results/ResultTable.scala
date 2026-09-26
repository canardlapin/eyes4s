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

package eyes4s.results

/** The finite export matrix. Adding a family requires its own schema and
  * artifact conformance. A family's name is part of every table identity, so
  * a case is never renamed; new families are appended.
  */
enum ResultFamily derives CanEqual:
  case PairScores, Reductions, Contrasts, PointSources, PointControls, PointSamples, PointBins
  case TemplateCoefficients, TemplatePredictions, TemplateExclusions
  case OlsCoefficients, OlsDiagnostics, OlsCells
  case StudyContrasts, TemporalContrasts, TemporalCoverage

  /** One row per report cell: a group, role and component's estimate,
    * counts and spread ([[ReportTables.cells]]).
    */
  case ReportCells

  /** One row per participant of a report cell ([[ReportTables.participants]]). */
  case ReportParticipants

  /** One row per within-participant level contrast ([[ReportTables.contrasts]]). */
  case ReportContrasts

enum ResultColumnType derives CanEqual:
  case Utf8, JsonUtf8, Int64, Float64, Boolean

/** Dictionary labels are written as UTF-8 in both transports, never inferred as numeric codes. */
final case class ResultColumn(
    name: String,
    kind: ResultColumnType,
    nullable: Boolean,
    unit: String,
    meaning: String,
    labels: Vector[String] = Vector.empty
) derives CanEqual

/** One cell. `Missing` is a null, written as an empty field with a `false`
  * validity column in CSV; it is never a zero or an empty string.
  */
enum ResultCell derives CanEqual:
  case Text(value: String)
  case Integer(value: Long)
  case Number(value: Double)
  case Flag(value: Boolean)
  case Missing

  /** The CSV spelling; numbers use the runtime-independent decimal spelling. */
  def text: String = this match
    case Text(v)    => v
    case Integer(v) => v.toString
    case Number(v)  => DecimalText.render(v)
    case Flag(v)    => v.toString
    case Missing    => ""

/** Why a table was refused; every case names the columns, row or operand. */
enum ResultTableError derives CanEqual:
  case Schema(columns: Vector[String], reason: String)
  case Cell(row: Int, column: String, value: ResultCell, reason: String)
  case Width(row: Int, expected: Int, actual: Int)
  case Context(operand: String, reason: String)

  def message: String = this match
    case Schema(c, r)     => s"Export columns=$c: $r."
    case Cell(i, c, v, r) => s"Export row=$i column=$c value=$v: $r."
    case Width(i, e, a)   => s"Export row=$i width=$a, expected=$e."
    case Context(o, r)    => s"Export context $o: $r."

/** Immutable checked rows, shared by portable CSV and JVM Arrow IPC.
  *
  * This is the one result-table layer: every export family, the baseline
  * matrix and the report families alike, is a `ResultTable`, and every
  * transport (`eyes4s-io`'s CSV and Arrow writers) renders one. Its identity
  * is the SHA-256 of its schema, family, canonical context, canonical column
  * declarations and cells, so the same table has the same identity on the
  * JVM and Scala.js.
  */
final class ResultTable private (
    val family: ResultFamily,
    val columns: Vector[ResultColumn],
    val rows: Vector[Vector[ResultCell]],
    val context: TableJson,
    val identity: TableDigest
):
  /** The CSV header: the table digest, then each column, a nullable column
    * followed by its explicit `__valid` column, so a missing value and an
    * empty string stay distinct.
    */
  def csvHeader: Vector[String] = Vector("table_sha256") ++ columns.flatMap(c =>
    if c.nullable then Vector(c.name, c.name + "__valid") else Vector(c.name)
  )

  /** The CSV records, in the order of [[csvHeader]]. */
  def csvRows: Vector[Vector[String]] = rows.map(row =>
    Vector(identity.hex) ++ row.zip(columns).flatMap { (v, c) =>
      if c.nullable then Vector(v.text, (v != ResultCell.Missing).toString)
      else Vector(v.text)
    }
  )

  /** The self-describing metadata document written beside every transport. */
  def metadata: TableJson = TableJson.obj(
    "schema"           -> TableJson.text(ResultTable.schema),
    "family"           -> TableJson.text(family.toString),
    "table_sha256"     -> TableJson.text(identity.hex),
    "row_count"        -> TableJson.integer(rows.size.toLong),
    "csv_nulls"        -> TableJson.text("explicit __valid column for each nullable field"),
    "arrow_dictionary" -> TableJson.text("none; finite labels retained as UTF-8"),
    "columns"          -> TableJson.Arr(columns.map(ResultTable.columnJson)),
    "context"          -> context
  )

  /** The values of column `name`, in row order, when the table has it. */
  def column(name: String): Option[Vector[ResultCell]] =
    Option(columns.indexWhere(_.name == name)).filter(_ >= 0).map(i => rows.map(_(i)))

object ResultTable:
  val schema: String = "eyes4s.result-table/1"

  /** The declaration of one column as it appears in the metadata. */
  def columnJson(c: ResultColumn): TableJson = TableJson.obj(
    "name"     -> TableJson.text(c.name),
    "type"     -> TableJson.text(c.kind.toString),
    "nullable" -> TableJson.Bool(c.nullable),
    "unit"     -> TableJson.text(c.unit),
    "meaning"  -> TableJson.text(c.meaning),
    "labels"   -> TableJson.Arr(c.labels.map(TableJson.text))
  )

  /** A checked table. Columns need non-empty unique names (with their
    * validity columns), units and meanings, and label domains only on UTF-8
    * columns; the context must be an object; every row has the table's
    * width and every cell its column's kind: a finite number, a label of the
    * column's domain, parseable JSON in a JSON column, and a missing value
    * only where the column is nullable. JSON cells are kept in their
    * canonical spelling.
    */
  def of(
      family: ResultFamily,
      columns: Vector[ResultColumn],
      rows: Vector[Vector[ResultCell]],
      context: TableJson
  ): Either[ResultTableError, ResultTable] =
    val names     = columns.map(_.name)
    val wireNames = Vector("table_sha256") ++ columns.flatMap(c =>
      if c.nullable then Vector(c.name, c.name + "__valid") else Vector(c.name)
    )
    val badSchema = columns.isEmpty || names.exists(
      _.trim.isEmpty
    ) || wireNames.distinct.size != wireNames.size ||
      columns.exists(c =>
        c.unit.trim.isEmpty || c.meaning.trim.isEmpty || c.labels.distinct != c.labels ||
          (c.labels.nonEmpty && c.kind != ResultColumnType.Utf8)
      )
    if badSchema then
      Left(
        ResultTableError.Schema(
          names,
          "nonempty unique names, units, meanings and valid UTF-8 label domains required"
        )
      )
    else if !context.isInstanceOf[TableJson.Obj] then
      Left(ResultTableError.Context("metadata", "expected an object"))
    else
      val failure = rows.zipWithIndex.iterator
        .flatMap { (row, i) =>
          if row.size != columns.size then
            Iterator.single(ResultTableError.Width(i, columns.size, row.size))
          else
            row
              .zip(columns)
              .iterator
              .collectFirst {
                case (v, c) if !valid(v, c) =>
                  ResultTableError.Cell(
                    i,
                    c.name,
                    v,
                    "wrong kind, nonfinite number, disallowed label or missing required value"
                  )
              }
              .iterator
        }
        .take(1)
        .toVector
        .headOption
      failure match
        case Some(e) => Left(e)
        case None    =>
          // CSV numbers have a portable exact round-trip spelling. Length framing prevents
          // collisions between embedded delimiters; metadata identity also binds scientific
          // context.
          def pack(v: Vector[String]) = v.map(s => s.length.toString + ":" + s).mkString
          val normalized              = rows.map(_.zip(columns).map { (v, c) =>
            v match
              case ResultCell.Text(s) if c.kind == ResultColumnType.JsonUtf8 =>
                // Admission above proves parsing succeeds; retain a total fallback.
                ResultCell.Text(TableJson.parse(s).fold(_ => s, _.canonical))
              case _ => v
          })
          val content = pack(
            Vector(schema, family.toString, context.canonical) ++
              columns.map(c => columnJson(c).canonical) ++
              normalized.map(r =>
                pack(
                  r.map(v =>
                    v match
                      case ResultCell.Missing => "missing"
                      case _                  => "present:" + v.text
                  )
                )
              )
          )
          Right(
            new ResultTable(family, columns, normalized, context, TableDigest.ofUtf8(content))
          )

  private def valid(v: ResultCell, c: ResultColumn): Boolean = (v, c.kind) match
    case (ResultCell.Missing, _)                     => c.nullable
    case (ResultCell.Text(s), ResultColumnType.Utf8) => c.labels.isEmpty || c.labels.contains(s)
    case (ResultCell.Text(s), ResultColumnType.JsonUtf8)  => TableJson.parse(s).isRight
    case (ResultCell.Integer(_), ResultColumnType.Int64)  => true
    case (ResultCell.Number(n), ResultColumnType.Float64) => n.isFinite
    case (ResultCell.Flag(_), ResultColumnType.Boolean)   => true
    case _                                                => false

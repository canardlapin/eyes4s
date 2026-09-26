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

import io.circe.Json

/** Finite export matrix; adding a family requires its own schema and artifact conformance. */
enum ResultFamily derives CanEqual:
  case PairScores, Reductions, Contrasts, PointSources, PointControls, PointSamples, PointBins
  case TemplateCoefficients, TemplatePredictions, TemplateExclusions
  case OlsCoefficients, OlsDiagnostics, OlsCells
  case StudyContrasts, TemporalContrasts, TemporalCoverage

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

enum ResultCell derives CanEqual:
  case Text(value: String)
  case Integer(value: Long)
  case Number(value: Double)
  case Flag(value: Boolean)
  case Missing
  def text: String = this match
    case Text(v)    => v
    case Integer(v) => v.toString
    case Number(v)  => CsvNumber.render(v)
    case Flag(v)    => v.toString
    case Missing    => ""

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

/** Immutable checked rows, shared by portable CSV and JVM Arrow IPC. */
final class ResultTable private (
    val family: ResultFamily,
    val columns: Vector[ResultColumn],
    val rows: Vector[Vector[ResultCell]],
    val context: Json,
    val identity: Sha256
):
  /** Nullable cells get explicit validity columns so empty strings and missing remain distinct. */
  def csv: TidyCsvDocument =
    val header = Vector("table_sha256") ++ columns.flatMap(c =>
      if c.nullable then Vector(c.name, c.name + "__valid") else Vector(c.name)
    )
    val cells = rows.map(row =>
      Vector(identity.hex) ++ row.zip(columns).flatMap { (v, c) =>
        if c.nullable then Vector(v.text, (v != ResultCell.Missing).toString)
        else Vector(v.text)
      }
    )
    new TidyCsvDocument(header, cells)
  def metadata: Json = Json.obj(
    "schema"           -> Json.fromString(ResultTable.schema),
    "family"           -> Json.fromString(family.toString),
    "table_sha256"     -> Json.fromString(identity.hex),
    "row_count"        -> Json.fromString(rows.size.toLong.toString),
    "csv_nulls"        -> Json.fromString("explicit __valid column for each nullable field"),
    "arrow_dictionary" -> Json.fromString("none; finite labels retained as UTF-8"),
    "columns"          -> Json.arr(columns.map(ResultTable.columnJson)*),
    "context"          -> context
  )

object ResultTable:
  val schema: String                                = "eyes4s.result-table/1"
  private[io] def columnJson(c: ResultColumn): Json = Json.obj(
    "name"     -> Json.fromString(c.name),
    "type"     -> Json.fromString(c.kind.toString),
    "nullable" -> Json.fromBoolean(c.nullable),
    "unit"     -> Json.fromString(c.unit),
    "meaning"  -> Json.fromString(c.meaning),
    "labels"   -> Json.arr(c.labels.map(Json.fromString)*)
  )
  def of(
      family: ResultFamily,
      columns: Vector[ResultColumn],
      rows: Vector[Vector[ResultCell]],
      context: Json
  ): Either[ResultExportError, ResultTable] =
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
        ResultExportError.Schema(
          names,
          "nonempty unique names, units, meanings and valid UTF-8 label domains required"
        )
      )
    else if !context.isObject then
      Left(ResultExportError.Context("metadata", "expected an object"))
    else
      val failure = rows.zipWithIndex.iterator
        .flatMap { (row, i) =>
          if row.size != columns.size then
            Iterator.single(ResultExportError.Width(i, columns.size, row.size))
          else
            row
              .zip(columns)
              .iterator
              .collectFirst {
                case (v, c) if !valid(v, c) =>
                  ResultExportError.Cell(
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
          // CSV numbers have a portable exact round-trip spelling. Length framing prevents collisions
          // between embedded delimiters; metadata identity also binds scientific context.
          def pack(v: Vector[String]) = v.map(s => s.length.toString + ":" + s).mkString
          val normalized              = rows.map(_.zip(columns).map { (v, c) =>
            v match
              case ResultCell.Text(s) if c.kind == ResultColumnType.JsonUtf8 =>
                // Admission above proves parsing succeeds; retain a total fallback.
                ResultCell.Text(io.circe.parser.parse(s).fold(_ => s, canonical))
              case _ => v
          })
          val content = pack(
            Vector(schema, family.toString, canonical(context)) ++ columns.map(c =>
              canonical(columnJson(c))
            ) ++
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
          Right(new ResultTable(family, columns, normalized, context, Sha256.ofUtf8(content)))
  private def canonical(value: Json): String = value.fold(
    "null",
    _.toString,
    n => n.toBigDecimal.fold(n.toString)(_.bigDecimal.stripTrailingZeros.toPlainString),
    s => Json.fromString(s).noSpaces,
    v => v.map(canonical).mkString("[", ",", "]"),
    o =>
      o.toVector
        .sortBy(_._1)
        .map((k, v) => Json.fromString(k).noSpaces + ":" + canonical(v))
        .mkString("{", ",", "}")
  )
  private def valid(v: ResultCell, c: ResultColumn): Boolean = (v, c.kind) match
    case (ResultCell.Missing, _)                     => c.nullable
    case (ResultCell.Text(s), ResultColumnType.Utf8) => c.labels.isEmpty || c.labels.contains(s)
    case (ResultCell.Text(s), ResultColumnType.JsonUtf8)  => io.circe.parser.parse(s).isRight
    case (ResultCell.Integer(_), ResultColumnType.Int64)  => true
    case (ResultCell.Number(n), ResultColumnType.Float64) => n.isFinite
    case (ResultCell.Flag(_), ResultColumnType.Boolean)   => true
    case _                                                => false

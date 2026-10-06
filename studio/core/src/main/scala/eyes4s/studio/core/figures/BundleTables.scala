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

package eyes4s.studio.core.figures

import eyes4s.results.{
  ResultCell,
  ResultColumn,
  ResultColumnType,
  ResultFamily,
  ResultTable,
  ResultTableError,
  TableJson
}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ReportingId, ReportingWeight}

/** Why a bundle table could not be made. Every case names its operands. */
enum BundleTableError derives CanEqual:
  /** The summary or the rows were read for another run. */
  case OtherRun(table: String, expected: RunId, found: RunId)

  /** eyes4s refused the table. */
  case Table(table: String, error: ResultTableError)

  /** Rows of a scale the run's summary does not compute. */
  case UnknownScale(table: String, scale: Int, scales: Vector[String])
  case FailureScales(run: RunId, query: TrialKey, expected: Int, found: Int)

  case OtherReporting(table: String, expected: ReportingId, found: ReportingId)

  def message: String = this match
    case FailureScales(run, query, expected, found) =>
      s"${run.label} query ${query.label} has $found failure diagnostics for $expected declared scales."
    case OtherRun(table, expected, found) =>
      s"The $table table of ${expected.label} was given the rows of ${found.label}."
    case Table(table, error)                => s"The $table table: ${error.message}"
    case UnknownScale(table, scale, scales) =>
      s"The $table table has rows of scale $scale; the run computes ${scales.mkString(", ")}."
    case OtherReporting(table, expected, found) =>
      s"The $table table of reporting ${expected.value} was given reporting ${found.value}."

/** The result tables of a figure's export bundle (ticket S9.5;
  * Figures.dc.html, bundle), as eyes4s `ResultTable`s: keyed rows, the table
  * digest, and a missing value written as a missing cell with its absence
  * named beside it, never a zero. Every value is read from the backend's
  * views; nothing is computed.
  */
object BundleTables:
  import ResultCell.{Text as T, Integer as I, Number as N, Missing as M}

  /** Why a score of a query is missing. */
  val Absences: Vector[String] =
    Vector("failed", "no-match", "not-admitted", "not-scored", "empty-group")

  /** Why a pair's score is missing (comparisons.csv's own domain, so the
    * query tables' label domain, and their digests, do not change).
    */
  val PairAbsences: Vector[String]        = Vector("failed", "not-served")
  val ParticipantAbsences: Vector[String] = Vector(
    "empty-group",
    "below-minimum",
    "unpaired",
    "not-recorded",
    "unparsed",
    "failed",
    "undefined"
  )

  /** A query's outcome. */
  val Statuses: Vector[String] = Vector("contributing", "failed", "no-match", "not-admitted")

  private def text(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Utf8, false, "text", meaning)
  private def note(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Utf8, true, "text", meaning)
  private def label(name: String, meaning: String, labels: Vector[String]) =
    ResultColumn(name, ResultColumnType.Utf8, false, "label", meaning, labels)
  private def count(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Int64, false, "count", meaning)
  private def optionalCount(name: String, meaning: String) =
    ResultColumn(name, ResultColumnType.Int64, true, "count", meaning)
  private def score(name: String, meaning: String, absences: Vector[String] = Absences) =
    Vector(
      ResultColumn(name, ResultColumnType.Float64, true, "score", meaning),
      ResultColumn(
        s"${name}_absence",
        ResultColumnType.Utf8,
        true,
        "label",
        s"why $name is missing",
        absences
      )
    )
  private def scored(value: Option[Double], absence: String): Vector[ResultCell] =
    value.fold(Vector(M, T(absence)))(v => Vector(N(v), M))

  /** The context both tables carry: what they are bound to. */
  def context(source: FigureSource, scales: Vector[String]): TableJson =
    TableJson.obj(
      "figure"    -> TableJson.text(source.figure.id.label),
      "run"       -> TableJson.text(source.run.id.label),
      "analysis"  -> TableJson.text(source.bound.analysis.id.label),
      "dataset"   -> TableJson.text(source.bound.dataset.id.label),
      "reporting" -> TableJson.text(source.reporting.id.value),
      "weighting" -> TableJson.text(source.reporting.weighting match
        case ReportingWeight.ParticipantMeans => "participant means, each participant equally"
        case ReportingWeight.PooledQueries    => "pooled queries, each query equally"),
      "scales"     -> TableJson.Arr(scales.map(TableJson.text)),
      "difference" -> TableJson.text("D = M - B: matched minus control mean")
    )

  private val queryColumns: Vector[ResultColumn] =
    Vector(
      text("participant", "participant id"),
      text("phase", "query phase"),
      text("trial", "query trial"),
      count("occurrence", "query occurrence"),
      text("item", "query item"),
      text("response", "query response"),
      note("matched_trial", "the matched reference trial, when one was selected"),
      optionalCount("controls", "control references the query was compared with"),
      count("scale", "estimation scale index, from 0"),
      text("sigma", "estimation scale"),
      label("status", "what became of the query", Statuses),
      note("reason", "why the query has no scores, as eyes4s reported it")
    ) ++ score("m", "matched similarity") ++ score("b", "control mean similarity") ++
      score("d", "M minus B")

  private def status(s: QueryStatus, scale: Int): (String, Option[String]) = s match
    case QueryStatus.Contributing(_, _, _)       => ("contributing", None)
    case QueryStatus.Failed(d)                   => ("failed", Some(s"${d.code}: ${d.message}"))
    case QueryStatus.FailedAtScales(diagnostics) =>
      ("failed", diagnostics.lift(scale).map(d => s"${d.code}: ${d.message}"))
    case QueryStatus.NoMatch(d)     => ("no-match", Some(s"${d.code}: ${d.message}"))
    case QueryStatus.NotAdmitted(t) =>
      (
        "not-admitted",
        Some(t match
          case TrialDisposition.Quarantined(c) => s"${c.code}: ${c.message}"
          case TrialDisposition.NoFixations    => "no-fixations"
          case TrialDisposition.Absent         => "absent"
          case TrialDisposition.Admitted       => "admitted")
      )

  /** results.csv: one row per query and scale, every query of the run. */
  def results(
      source: FigureSource,
      summary: ResultSummary,
      rows: Vector[QueryRow]
  ): Either[BundleTableError, ResultTable] =
    val run = source.run.id
    if summary.run != run then Left(BundleTableError.OtherRun("results", run, summary.run))
    else
      val invalid = rows.collectFirst {
        case row @ QueryRow(_, _, _, _, _, QueryStatus.FailedAtScales(diagnostics))
            if diagnostics.size != summary.scales.size =>
          BundleTableError.FailureScales(run, row.query, summary.scales.size, diagnostics.size)
      }
      invalid.toLeft(()).flatMap { _ =>
        val cells = for
          row            <- rows
          (sigma, scale) <- summary.scales.zipWithIndex
          (status, reason) = this.status(row.status, scale)
        yield
          val (m, b, d) = row.status match
            case QueryStatus.Contributing(m, b, d) =>
              (m.lift(scale), b.lift(scale), d.lift(scale))
            case _ => (None, None, None)
          val absence = if status == "contributing" then "not-scored" else status
          Vector(
            T(row.query.participant),
            T(row.query.phase.label),
            T(row.query.trial),
            I(row.query.occurrence.toLong),
            T(row.item),
            T(row.response.label),
            row.matched.fold(M)(k => T(k.trial)),
            row.controls.fold(M)(c => I(c.toLong)),
            I(scale.toLong),
            T(sigma),
            T(status),
            reason.fold(M)(T(_))
          ) ++ scored(m, absence) ++ scored(b, absence) ++ scored(d, absence)
        ResultTable
          .of(ResultFamily.StudyContrasts, queryColumns, cells, context(source, summary.scales))
          .left
          .map(BundleTableError.Table("results", _))
      }

  private def participantColumns(attribute: String): Vector[ResultColumn] =
    Vector(
      text("participant", "participant id"),
      text("group", s"reporting group ($attribute)"),
      count("n", "queries the participant's group mean averages")
    ) ++ score("m", "participant mean of matched similarity", ParticipantAbsences) ++
      score("b", "participant mean of control mean similarity", ParticipantAbsences) ++
      score("d", "participant mean of D", ParticipantAbsences) ++ Vector(
        count("requested", "queries the participant was asked"),
        count("contributing", "queries with scores"),
        count("failed", "queries that failed"),
        count("no_match", "queries without an admitted matched reference"),
        count("not_admitted", "queries whose trial was not admitted")
      )

  /** participants.csv: one row per participant and reporting group; a group
    * a participant has no queries in is a row with n 0 and missing means.
    */
  def participants(
      source: FigureSource,
      summary: ResultSummary,
      report: ReportView
  ): Either[BundleTableError, ResultTable] =
    val run = source.run.id
    if summary.run != run then Left(BundleTableError.OtherRun("participants", run, summary.run))
    else if report.run != run then
      Left(BundleTableError.OtherRun("participants", run, report.run))
    else if report.reporting != source.reporting.id then
      Left(
        BundleTableError.OtherReporting("participants", source.reporting.id, report.reporting)
      )
    else if !summary.scales.indices.contains(report.scale) then
      Left(BundleTableError.UnknownScale("participants", report.scale, summary.scales))
    else
      val attribute = source.reporting.groupBy.fold("all queries")(_.label)
      val labels    = report.cells.filter(_.role == ReportRole.Difference).map(_.group).distinct
      val cells     = for
        p     <- summary.participants
        group <- labels
      yield
        def value(role: ReportRole) = report.participant(group, role, p.participant)
        def cell(role: ReportRole): Vector[ResultCell] =
          val served = value(role)
          val why    = served.flatMap(_.absence).fold("empty-group") {
            case ReportAbsence.EmptyGroup         => "empty-group"
            case ReportAbsence.BelowMinimum(_, _) => "below-minimum"
            case ReportAbsence.Unpaired           => "unpaired"
            case ReportAbsence.NotRecorded        => "not-recorded"
            case ReportAbsence.Unparsed           => "unparsed"
            case ReportAbsence.Failed(_, _)       => "failed"
            case ReportAbsence.Undefined(_)       => "undefined"
          }
          scored(served.flatMap(_.value), why)
        val means = value(ReportRole.Difference)
        Vector(
          T(p.participant),
          T(group.fold("all queries")(_.label)),
          I(means.fold(0L)(_.queries.toLong))
        ) ++
          cell(ReportRole.Matched) ++ cell(ReportRole.Control) ++ cell(ReportRole.Difference) ++
          Vector(
            I(p.requested.toLong),
            I(p.contributing.toLong),
            I(p.failed.toLong),
            I(p.noMatch.toLong),
            I(p.notAdmitted.toLong)
          )
      ResultTable
        .of(
          ResultFamily.ReportParticipants,
          participantColumns(attribute),
          cells,
          context(source, summary.scales) match
            case TableJson.Obj(fields) =>
              TableJson.Obj(
                fields ++ Vector(
                  "means_scale"       -> TableJson.text(summary.scales(report.scale)),
                  "means_scale_index" -> TableJson.integer(report.scale.toLong)
                )
              )
            case other => other
        )
        .left
        .map(BundleTableError.Table("participants", _))

  /** A pair's design as every table labels it. */
  val Designs: Vector[String] = Vector("matched", "control")

  private val pairColumns: Vector[ResultColumn] =
    Vector(
      count("scale", "estimation scale index, from 0"),
      text("sigma", "estimation scale"),
      text("participant", "participant id"),
      text("phase", "query phase"),
      text("trial", "query trial"),
      count("occurrence", "query occurrence"),
      label("design", "the reference's design: the matched reference or a control", Designs),
      text("reference_phase", "reference phase"),
      text("reference_trial", "reference trial"),
      count("reference_occurrence", "reference occurrence"),
      text("reference_item", "reference item")
    ) ++ score("score", "the pair's similarity", PairAbsences) ++
      Vector(note("reason", "why the pair has no score, as eyes4s reported it"))

  /** comparisons.csv: every pair row of the run at every scale (eyes4s
    * `PairScores`, read through protocol 1.9). `pairs` holds each scale's
    * rows, in scale order. A failed pair's score is missing with its
    * diagnostic; one the backend does not serve is missing as not-served.
    */
  def comparisons(
      source: FigureSource,
      summary: ResultSummary,
      pairs: Vector[PairRowPage]
  ): Either[BundleTableError, ResultTable] =
    val run     = source.run.id
    val unknown = pairs.find(p => !summary.scales.indices.contains(p.scale))
    (pairs.find(_.run != run), unknown) match
      case (Some(other), _) => Left(BundleTableError.OtherRun("comparisons", run, other.run))
      case (_, Some(page))  =>
        Left(BundleTableError.UnknownScale("comparisons", page.scale, summary.scales))
      case _ =>
        if summary.run != run then
          Left(BundleTableError.OtherRun("comparisons", run, summary.run))
        else
          val cells = for
            page <- pairs
            row  <- page.rows
          yield
            val (value, absence, reason) = row.score match
              case PairScoreState.Scored(v) => (Some(v), "", None)
              case PairScoreState.Failed(d) =>
                (None, "failed", Some(s"${d.code}: ${d.message}"))
              case PairScoreState.NotServed => (None, "not-served", None)
            Vector(
              I(page.scale.toLong),
              T(summary.scales(page.scale)),
              T(row.query.participant),
              T(row.query.phase.label),
              T(row.query.trial),
              I(row.query.occurrence.toLong),
              T(row.design match
                case PairDesign.Matched => "matched"
                case PairDesign.Control => "control"),
              T(row.reference.phase.label),
              T(row.reference.trial),
              I(row.reference.occurrence.toLong),
              T(row.referenceItem)
            ) ++ scored(value, absence) :+ reason.fold(M)(T(_))
          ResultTable
            .of(ResultFamily.PairScores, pairColumns, cells, context(source, summary.scales))
            .left
            .map(BundleTableError.Table("comparisons", _))

  /** How every bundle table is written, said once in the README. */
  val Convention: Vector[String] = Vector(
    "Every table is RFC 4180 CSV with a header row. Its first column, table_sha256, is the",
    "table's digest: the same for the same table, whatever wrote it. A column that may be",
    "missing is followed by <column>__valid: true when the cell holds a value, false when it is",
    "missing. A missing cell is empty, never a zero; where the table says why, the reason is in",
    "<column>_absence, one of the labels listed for it."
  )

  private def kind(c: ResultColumn): String = c.kind match
    case ResultColumnType.Utf8     => "text"
    case ResultColumnType.JsonUtf8 => "JSON text"
    case ResultColumnType.Int64    => "integer"
    case ResultColumnType.Float64  => "number"
    case ResultColumnType.Boolean  => "true or false"

  /** `table`'s columns as written to `file`, each from its declaration
    * (name, type, unit, meaning, labels), so the README cannot drift from
    * the CSV: one line per column, its `__valid` column after a nullable one.
    */
  def describe(file: String, table: ResultTable): Vector[String] =
    val columns = table.columns.flatMap { c =>
      val labels = if c.labels.isEmpty then "" else s"; one of ${c.labels.mkString(", ")}"
      // A text column's unit says nothing more than its type.
      val unit = if c.unit == "text" then "" else s", ${c.unit}"
      val line =
        s"- ${c.name} (${kind(c)}$unit${if c.nullable then ", may be missing" else ""}" +
          s"$labels): ${c.meaning}"
      if c.nullable then
        Vector(line, s"- ${c.name}__valid (true or false): whether ${c.name} holds a value")
      else Vector(line)
    }
    Vector(
      s"$file (eyes4s ${table.family} table, ${table.rows.size} rows):",
      "- table_sha256 (text): the table's digest"
    ) ++ columns

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
import eyes4s.studio.core.document.ReportingWeight

/** Why a bundle table could not be made. Every case names its operands. */
enum BundleTableError derives CanEqual:
  /** The summary or the rows were read for another run. */
  case OtherRun(table: String, expected: RunId, found: RunId)

  /** eyes4s refused the table. */
  case Table(table: String, error: ResultTableError)

  /** Rows of a scale the run's summary does not compute. */
  case UnknownScale(table: String, scale: Int, scales: Vector[String])

  def message: String = this match
    case OtherRun(table, expected, found) =>
      s"The $table table of ${expected.label} was given the rows of ${found.label}."
    case Table(table, error)                => s"The $table table: ${error.message}"
    case UnknownScale(table, scale, scales) =>
      s"The $table table has rows of scale $scale; the run computes ${scales.mkString(", ")}."

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
  val PairAbsences: Vector[String] = Vector("failed", "not-served")

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
      text("matched_trial", "the matched reference trial"),
      optionalCount("controls", "control references the query was compared with"),
      count("scale", "estimation scale index, from 0"),
      text("sigma", "estimation scale"),
      label("status", "what became of the query", Statuses),
      note("reason", "why the query has no scores, as eyes4s reported it")
    ) ++ score("m", "matched similarity") ++ score("b", "control mean similarity") ++
      score("d", "M minus B")

  private def status(s: QueryStatus): (String, Option[String]) = s match
    case QueryStatus.Contributing(_, _, _) => ("contributing", None)
    case QueryStatus.Failed(d)             => ("failed", Some(s"${d.code}: ${d.message}"))
    case QueryStatus.NoMatch(d)            => ("no-match", Some(s"${d.code}: ${d.message}"))
    case QueryStatus.NotAdmitted(t)        =>
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
      val cells = for
        row            <- rows
        (sigma, scale) <- summary.scales.zipWithIndex
        (status, reason) = this.status(row.status)
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
          T(row.matched.trial),
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

  private def participantColumns(attribute: String): Vector[ResultColumn] =
    Vector(
      text("participant", "participant id"),
      text("group", s"reporting group ($attribute)"),
      count("n", "queries the participant's group mean averages")
    ) ++ score("m", "participant mean of matched similarity") ++
      score("b", "participant mean of control mean similarity") ++
      score("d", "participant mean of D") ++ Vector(
        count("requested", "queries the participant was asked"),
        count("contributing", "queries with scores"),
        count("failed", "queries that failed"),
        count("no_match", "queries without an admitted matched reference"),
        count("not_admitted", "queries whose trial was not admitted")
      )

  /** The scale the summary's participant and group means are at: the one
    * scale at which every group's grand mean equals its D at that scale, or
    * "undeclared" when none or several do (the summary does not declare it).
    */
  def meansScale(summary: ResultSummary): String =
    summary.scales.indices.filter(i =>
      summary.groups.nonEmpty && summary.groups.forall(g => g.dByScale.lift(i).contains(g.d))
    ) match
      case Seq(i) => summary.scales(i)
      case _      => "undeclared"

  /** participants.csv: one row per participant and reporting group; a group
    * a participant has no queries in is a row with n 0 and missing means.
    */
  def participants(
      source: FigureSource,
      summary: ResultSummary
  ): Either[BundleTableError, ResultTable] =
    val run = source.run.id
    if summary.run != run then Left(BundleTableError.OtherRun("participants", run, summary.run))
    else
      val attribute = summary.groups.headOption.fold("all queries")(_.attribute)
      val labels    = summary.groups.map(_.label)
      val cells     = for
        p     <- summary.participants
        group <- labels
      yield
        val means = p.groups.find(_.label == group).filter(_.n > 0)
        Vector(T(p.participant), T(group.label), I(means.fold(0L)(_.n.toLong))) ++
          scored(means.map(_.m), "empty-group") ++ scored(means.map(_.b), "empty-group") ++
          scored(means.map(_.d), "empty-group") ++
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
              TableJson.Obj(fields :+ ("means_scale" -> TableJson.text(meansScale(summary))))
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

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

package eyes4s.studio.app.compare

import eyes4s.studio.app.plot.{
  ColumnFormat,
  ColumnId,
  ParticipantColumns,
  ParticipantMeans,
  PlotColumn,
  PlotRow,
  PlotSource,
  PlotValue,
  ProfileColumns,
  ScaleProfile
}
import eyes4s.studio.app.text.{Format, Messages, SummaryText, SummaryTextId}
import eyes4s.studio.app.vm.{Labels, Shell}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.backend.{QueryRow, QueryStatus, ReportRole, ResultSummary, RunId}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** One σ of the selector: whether the participant means are served at it
  * and whether it is the one shown.
  */
final case class ScaleChoice(
    scale: ScaleIndex,
    label: String,
    available: Boolean,
    chosen: Boolean,
    /** Why the σ cannot be chosen, when it cannot. */
    unavailable: Option[String]
) derives CanEqual

/** The Explain action for the selected participant: its label, what the
  * trail keeps, and the intents that walk down to it.
  */
final case class ExplainVM(label: String, keeps: String, intents: Vector[Intent])
    derives CanEqual

/** What Compare's summary layout shows (ticket S8.6). Every source is built
  * from the backend's summary and queries; nothing is computed here. A part
  * the backend did not answer, or that cannot be built, says why.
  */
final case class CompareSummaryVM(
    status: Option[String],
    participantPlot: Option[Either[String, PlotSource]],
    profile: Option[Either[String, PlotSource]],
    participants: Option[Either[String, PlotSource]],
    queries: Option[Either[String, PlotSource]],
    notes: Vector[String],
    scales: Vector[ScaleChoice],
    freshness: Option[String],
    explain: Option[ExplainVM]
) derives CanEqual

object CompareSummaryVM:

  def of(
      s: CompareSummary,
      m: AppModel,
      messages: Messages = Messages.english
  ): CompareSummaryVM =
    val freshness = Shell.context(m, messages).newer.map(_.text)
    (s.run, s.summary) match
      case (None, _) =>
        empty(Some(SummaryText(SummaryTextId.NoRun)), freshness)
      case (Some(run), None) =>
        empty(Some(SummaryText(SummaryTextId.Reading, run.number.toString)), freshness)
      case (Some(run), Some(SummaryAnswer.Refused(e))) =>
        empty(
          Some(SummaryText(SummaryTextId.Unreadable, run.number.toString, e.message)),
          freshness
        )
      case (Some(run), Some(SummaryAnswer.Failed(why))) =>
        empty(Some(SummaryText(SummaryTextId.Unreadable, run.number.toString, why)), freshness)
      case (Some(run), Some(SummaryAnswer.Answered(r))) =>
        answered(s, m, run, r, freshness, messages)

  private def empty(status: Option[String], freshness: Option[String]) =
    CompareSummaryVM(
      status,
      None,
      None,
      None,
      None,
      Vector.empty,
      Vector.empty,
      freshness,
      None
    )

  private def answered(
      s: CompareSummary,
      m: AppModel,
      run: RunId,
      r: ResultSummary,
      freshness: Option[String],
      messages: Messages
  ): CompareSummaryVM =
    val available = s.available
    val shown     = s.shown
    val scales    = r.scales.zipWithIndex.flatMap { (label, i) =>
      ScaleIndex
        .of(i)
        .toOption
        .map(scale =>
          ScaleChoice(
            scale,
            SummaryText(SummaryTextId.Scale, label),
            available.contains(scale),
            shown.contains(scale),
            Option.unless(available.contains(scale))(
              SummaryText(
                SummaryTextId.ScaleUnavailable,
                available.flatMap(a => r.scales.lift(a.value)).mkString(", ")
              )
            )
          )
        )
    }
    def displayed(scale: ScaleIndex) = s.reports.get((scale, false)).collect {
      case ReportAnswer.Answered(view) => view
    }
    def ungrouped(scale: ScaleIndex) = s.reports.get((scale, true)).collect {
      case ReportAnswer.Answered(view) => view
    }
    val reporting       = s.reporting
    val participantPlot = for
      scale <- shown
      view <- displayed(scale)
    yield ParticipantMeans
      .of(view, r.scales.lift(scale.value).getOrElse(scale.value.toString))
      .left
      .map(_.message)
      .flatMap(means =>
        ParticipantColumns.standard
          .flatMap(ParticipantMeans.source(means, _))
          .left
          .map(_.message)
      )
    val profile = reporting.map { _ =>
      m.document
        .run(run)
        .flatMap(rr => m.document.analysis(rr.analysis))
        .map(_.recipe.scales)
        .toRight(s"run ${run.number} has no analysis revision in the document")
        .flatMap(scales =>
          val grouped = scales.values.indices.toVector.flatMap(i => ScaleIndex.of(i).toOption.flatMap(displayed))
          val overall = scales.values.indices.toVector.flatMap(i => ScaleIndex.of(i).toOption.flatMap(ungrouped))
          ScaleProfile.of(grouped, overall, scales, r.scales).left.map(_.message)
        )
        .flatMap(p =>
          ProfileColumns.standard.flatMap(ScaleProfile.source(p, _)).left.map(_.message)
        )
    }
    val participants = for
      rep   <- reporting
      scale <- shown
      grouped <- displayed(scale)
      overall <- ungrouped(scale)
    yield participantTable(run, rep, scale, r, grouped, overall)
    val queries = for
      scale <- shown
      q     <- s.queries
    yield q match
      case QueriesAnswer.Answered(rows) => queryTable(run, scale, r, rows)
      case QueriesAnswer.Failed(why)    => Left(why)
    val notes = shown.flatMap(displayed).toVector.flatMap { view =>
      val contrast = view.contrast(ReportRole.Difference)
      val range    = view.queryRange(ReportRole.Difference)
      Vector(
      SummaryText(SummaryTextId.PairedN, contrast.fold("0")(_.pairedN.toString)),
      SummaryText(SummaryTextId.Weighting),
      SummaryText(SummaryTextId.Unit),
      SummaryText(SummaryTextId.GroupRange, range.fold("0")(_.fewest.toString), range.fold("0")(_.most.toString))
      )
    }
    CompareSummaryVM(
      None,
      participantPlot,
      profile,
      participants,
      queries,
      notes,
      scales,
      freshness,
      explain(m, run, reporting, messages)
    )

  /** The participant table (Results board; FIXTURE.md's table): one row per
    * participant, its counts, its means over all queries and its mean in
    * each group with that mean's n.
    */
  def participantTable(
      run: RunId,
      rep: ReportingId,
      scale: ScaleIndex,
      r: ResultSummary,
      grouped: eyes4s.studio.core.backend.ReportView,
      overall: eyes4s.studio.core.backend.ReportView
  ): Either[String, PlotSource] =
    import SummaryTextId.*
    def column(id: String, header: String, format: ColumnFormat) =
      ColumnId.of(id).map(PlotColumn(_, header, format))
    val label = r.scales.lift(scale.value).getOrElse(scale.value.toString)
    val fixed = Vector(
      ("participant", Participant, ColumnFormat.Label),
      ("requested", Requested, ColumnFormat.Count),
      ("contributing", Contributing, ColumnFormat.Count),
      ("failed", Failed, ColumnFormat.Count),
      ("no-match", NoMatch, ColumnFormat.Count),
      ("not-admitted", NotAdmitted, ColumnFormat.Count),
      ("mean-m", MeanM, ColumnFormat.Decimal(2)),
      ("mean-b", MeanB, ColumnFormat.Decimal(2)),
      ("mean-d", MeanD, ColumnFormat.Signed(2))
    )
    val groups = grouped.cells.collect {
      case c if c.role == ReportRole.Difference && c.group.nonEmpty => c.group.get
    }.distinct
    val groupColumns = groups.zipWithIndex.map((g, k) =>
      column(s"group-$k", SummaryText(GroupHeader, g.label), ColumnFormat.Label)
    )
    val columns = fixed.map((id, h, f) => column(id, SummaryText(h), f)) ++ groupColumns
    val ids = overall.participants.collect {
      case p if p.role == ReportRole.Difference && p.group.isEmpty => p.participant
    }.distinct
    val rows    = ids.map { id =>
      val legacy = r.participants.find(_.participant == id)
      val counts = legacy.map(p => Vector(p.requested, p.contributing, p.failed, p.noMatch, p.notAdmitted))
        .getOrElse(Vector.fill(5)(0))
      def value(role: ReportRole) =
        overall.participant(None, role, id).flatMap(_.value).fold(PlotValue.Missing)(PlotValue.Number(_))
      PlotRow(
        overall.participant(None, ReportRole.Difference, id).map(_.ref)
          .getOrElse(StudioRef.ParticipantSummary(run, rep, scale, None, id)),
        Vector(PlotValue.Text(id)) ++
          counts.map(n => PlotValue.Number(n.toDouble)) ++
          Vector(value(ReportRole.Matched), value(ReportRole.Control), value(ReportRole.Difference)) ++
          groups.map { g =>
            grouped
              .participant(Some(g), ReportRole.Difference, id)
              .flatMap(p => p.value.map(v => SummaryText(GroupCell, Format.signed(v, 2), p.queries.toString)))
              .fold(PlotValue.Missing)(PlotValue.Text(_))
          }
      )
    }
    columns
      .foldLeft[Either[String, Vector[PlotColumn]]](Right(Vector.empty))((acc, c) =>
        acc.flatMap(cs => c.left.map(_.message).map(cs :+ _))
      )
      .flatMap(cs =>
        PlotSource(
          SummaryText(ParticipantCaption, label, run.number.toString),
          cs,
          rows
        ).left.map(_.message)
      )

  /** The query table: every query of the run, its item, response and status,
    * and its M, B and D at the shown σ when it contributes.
    */
  def queryTable(
      run: RunId,
      scale: ScaleIndex,
      r: ResultSummary,
      rows: Vector[QueryRow]
  ): Either[String, PlotSource] =
    import SummaryTextId.*
    val label = r.scales.lift(scale.value).getOrElse(scale.value.toString)
    def column(id: String, header: SummaryTextId, format: ColumnFormat) =
      ColumnId.of(id).map(PlotColumn(_, SummaryText(header), format)).left.map(_.message)
    val specs = Vector(
      ("query", Query, ColumnFormat.Label),
      ("item", Item, ColumnFormat.Label),
      ("response", Response, ColumnFormat.Label),
      ("status", Status, ColumnFormat.Label),
      ("m", M, ColumnFormat.Decimal(2)),
      ("b", B, ColumnFormat.Decimal(2)),
      ("d", D, ColumnFormat.Signed(2))
    )
    def at(v: Vector[Double]) = v.lift(scale.value).fold(PlotValue.Missing)(PlotValue.Number(_))
    val table                 = rows.map { q =>
      val (status, scores) = q.status match
        case QueryStatus.Contributing(m, b, d) =>
          (SummaryText(StatusContributing), Vector(at(m), at(b), at(d)))
        case QueryStatus.Failed(_) =>
          (SummaryText(StatusFailed), Vector.fill(3)(PlotValue.Missing))
        case QueryStatus.NoMatch(_) =>
          (SummaryText(StatusNoMatch), Vector.fill(3)(PlotValue.Missing))
        case QueryStatus.NotAdmitted(_) =>
          (SummaryText(StatusNotAdmitted), Vector.fill(3)(PlotValue.Missing))
      PlotRow(
        StudioRef.QueryContrast(run, scale, q.query),
        Vector(
          PlotValue.Text(q.query.label),
          PlotValue.Text(q.item),
          PlotValue.Text(q.response.label),
          PlotValue.Text(status)
        ) ++ scores
      )
    }
    specs
      .foldLeft[Either[String, Vector[PlotColumn]]](Right(Vector.empty))((acc, c) =>
        acc.flatMap(cs => column(c._1, c._2, c._3).map(cs :+ _))
      )
      .flatMap(cs =>
        PlotSource(SummaryText(QueryCaption, run.number.toString, label), cs, table).left
          .map(_.message)
      )

  /** Explain from the selected participant mean: in a group, the trail
    * keeps the spec and the group (Summary › group › participant), which the
    * ref names; the query layout then shows its queries.
    */
  private def explain(
      m: AppModel,
      run: RunId,
      reporting: Option[ReportingId],
      messages: Messages
  ): Option[ExplainVM] =
    val labels = Labels(m, messages)
    m.selection.selected.collectFirst {
      case ref @ StudioRef.ParticipantSummary(`run`, rep, _, group, p)
          if reporting.contains(rep) =>
        val keeps = (Vector(labels.reporting(rep)) ++ group.map(_.label) :+ p).mkString(" › ")
        ExplainVM(
          SummaryText(SummaryTextId.Explain, p),
          SummaryText(SummaryTextId.Keeps, keeps),
          // The trail fills in the spec and the group from the ref itself
          // (Provenance.explain), so one step walks Summary › group › participant.
          Vector(Intent.Explain(Place.At(ref)))
        )
    }

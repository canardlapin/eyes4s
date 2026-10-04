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
import eyes4s.studio.core.backend.{QueryRow, QueryStatus, ResultSummary, RunId}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** One σ of the selector: whether the participant means are served at it
  * and whether it is the one shown.
  */
final case class ScaleChoice(
    scale: ScaleIndex,
    label: String,
    available: Boolean,
    chosen: Boolean
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
            shown.contains(scale)
          )
        )
    }
    val reporting       = s.reporting
    val participantPlot = for
      rep   <- reporting
      scale <- shown
    yield ParticipantMeans
      .of(r, rep, scale)
      .left
      .map(_.message)
      .flatMap(means =>
        ParticipantColumns.standard
          .flatMap(ParticipantMeans.source(means, _))
          .left
          .map(_.message)
      )
    val profile = reporting.map { rep =>
      m.document
        .run(run)
        .flatMap(rr => m.document.analysis(rr.analysis))
        .map(_.recipe.scales)
        .toRight(s"run ${run.number} has no analysis revision in the document")
        .flatMap(ScaleProfile.of(r, rep, _).left.map(_.message))
        .flatMap(p =>
          ProfileColumns.standard.flatMap(ScaleProfile.source(p, _)).left.map(_.message)
        )
    }
    val participants = for
      rep   <- reporting
      scale <- shown
    yield participantTable(run, rep, scale, r)
    val queries = for
      scale <- shown
      q     <- s.queries
    yield q match
      case QueriesAnswer.Answered(rows) => queryTable(run, scale, r, rows)
      case QueriesAnswer.Failed(why)    => Left(why)
    val notes = Vector(
      SummaryText(SummaryTextId.PairedN, r.pairedN.toString),
      SummaryText(SummaryTextId.Weighting),
      SummaryText(SummaryTextId.Unit),
      SummaryText(SummaryTextId.GroupRange, r.groupNMinimum.toString, r.groupNMaximum.toString)
    )
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
      r: ResultSummary
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
    val groupColumns = r.groups.zipWithIndex.map((g, k) =>
      column(s"group-$k", SummaryText(GroupHeader, g.label.label), ColumnFormat.Label)
    )
    val columns = fixed.map((id, h, f) => column(id, SummaryText(h), f)) ++ groupColumns
    val rows    = r.participants.map { p =>
      val counts = Vector(p.requested, p.contributing, p.failed, p.noMatch, p.notAdmitted)
      PlotRow(
        StudioRef.ParticipantSummary(run, rep, scale, None, p.participant),
        Vector(PlotValue.Text(p.participant)) ++
          counts.map(n => PlotValue.Number(n.toDouble)) ++
          Vector(p.all.m, p.all.b, p.all.d).map(PlotValue.Number(_)) ++
          r.groups.map { g =>
            p.groups
              .find(gm => gm.label == g.label && gm.n > 0)
              .fold(PlotValue.Missing)(gm =>
                PlotValue.Text(SummaryText(GroupCell, Format.signed(gm.d, 2), gm.n.toString))
              )
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
    * keeps the spec and the group (Summary › group › participant); the
    * query layout then shows its queries.
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
          group.map(g => Intent.Explain(Place.Group(rep, g))).toVector :+ Intent.Explain(
            Place.At(ref)
          )
        )
    }

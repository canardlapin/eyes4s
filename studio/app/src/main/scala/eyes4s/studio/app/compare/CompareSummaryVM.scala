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

import cats.syntax.all.*

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
import eyes4s.studio.core.document.{Perspective, ReportingId, ReportingWeight}
import eyes4s.studio.core.selection.{ReportGroup, ScaleIndex, StudioRef}

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
      view  <- displayed(scale)
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
          // An already ungrouped spec is its own whole-report evaluation;
          // deriving another identity would lose that cell's provenance.
          val whole   = if s.spec.exists(_.groupBy.isEmpty) then displayed else ungrouped
          val reports = scales.values.indices.toVector
            .flatMap(i => ScaleIndex.of(i).toOption.flatMap(whole))
          ScaleProfile.overall(reports, scales, r.scales).left.map(_.message)
        )
        .flatMap(p =>
          ProfileColumns.standard.flatMap(ScaleProfile.source(p, _)).left.map(_.message)
        )
    }
    val participants = for
      rep     <- reporting
      scale   <- shown
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
      val contrast  = view.contrast(ReportRole.Difference)
      val range     = view.queryRange(ReportRole.Difference)
      val weighting = s.spec.map(_.weighting).fold(SummaryTextId.WeightingParticipantMeans) {
        case ReportingWeight.ParticipantMeans => SummaryTextId.WeightingParticipantMeans
        case ReportingWeight.PooledQueries    => SummaryTextId.WeightingPooledQueries
      }
      Vector(
        SummaryText(weighting),
        SummaryText(SummaryTextId.Unit)
      ) ++ contrast.toVector.map(c => SummaryText(SummaryTextId.PairedN, c.pairedN.toString)) ++
        range.toVector
          .map(r => SummaryText(SummaryTextId.GroupRange, r.fewest.toString, r.most.toString))
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
      explain(s, m, run, reporting, messages)
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
    val ids     = overall.participants.collect {
      case p if p.role == ReportRole.Difference && p.group.isEmpty => p.participant
    }.distinct
    val rows = ids.traverse { id =>
      def value(role: ReportRole) =
        overall
          .participant(None, role, id)
          .flatMap(_.value)
          .fold(PlotValue.Missing)(PlotValue.Number(_))
      for
        accounting <- r.participants
          .find(_.participant == id)
          .toRight(s"${run.label} has no query accounting for participant $id.")
        participant <- overall
          .participant(None, ReportRole.Difference, id)
          .toRight(
            s"${run.label}, reporting ${overall.reporting.value}, scale ${scale.value} has no served participant $id."
          )
      yield PlotRow(
        participant.ref,
        Vector(PlotValue.Text(id)) ++
          Vector(
            accounting.requested,
            accounting.contributing,
            accounting.failed,
            accounting.noMatch,
            accounting.notAdmitted
          ).map(n => PlotValue.Number(n.toDouble)) ++
          Vector(
            value(ReportRole.Matched),
            value(ReportRole.Control),
            value(ReportRole.Difference)
          ) ++
          groups.map { g =>
            grouped
              .participant(Some(g), ReportRole.Difference, id)
              .flatMap(p =>
                p.value
                  .map(v => SummaryText(GroupCell, Format.signed(v, 2), p.queries.toString))
              )
              .fold(PlotValue.Missing)(PlotValue.Text(_))
          }
      )
    }
    val context = for
      _ <- Either.cond(
        grouped.reporting == rep,
        (),
        s"${run.label} requested reporting ${rep.value} but received ${grouped.reporting.value}."
      )
      _ <- Vector(grouped, overall).traverse_(view =>
        Either.cond(
          view.run == run && view.scale == scale.value,
          (),
          s"${run.label}, scale ${scale.value} received ${view.run.label}, scale ${view.scale}."
        )
      )
      _ <- Either.cond(
        r.run == run,
        (),
        s"${run.label} received accounting for ${r.run.label}."
      )
    yield ()
    context.flatMap(_ =>
      columns
        .foldLeft[Either[String, Vector[PlotColumn]]](Right(Vector.empty))((acc, c) =>
          acc.flatMap(cs => c.left.map(_.message).map(cs :+ _))
        )
        .flatMap(cs =>
          rows.flatMap(rs =>
            PlotSource(SummaryText(ParticipantCaption, label, run.number.toString), cs, rs).left
              .map(_.message)
          )
        )
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
        case QueryStatus.Failed(_) | QueryStatus.FailedAtScales(_) =>
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
      s: CompareSummary,
      m: AppModel,
      run: RunId,
      reporting: Option[ReportingId],
      messages: Messages
  ): Option[ExplainVM] =
    val labels = Labels(m, messages)
    def served(
        scale: ScaleIndex,
        whole: Boolean,
        ref: StudioRef.ReportParticipant
    ): Boolean =
      s.reports
        .get((scale, whole))
        .collect { case ReportAnswer.Answered(view) => view.participants.exists(_.ref == ref) }
        .contains(true)
    def view(
        ref: StudioRef,
        rep: ReportingId,
        group: Option[eyes4s.studio.core.backend.Response],
        p: String
    ) =
      val keeps = (Vector(labels.reporting(rep)) ++ group.map(_.label) :+ p).mkString(" › ")
      ExplainVM(
        SummaryText(SummaryTextId.Explain, p),
        SummaryText(SummaryTextId.Keeps, keeps),
        Vector(Intent.Explain(Place.At(ref)))
      )
    m.selection.selected.collectFirst {
      case ref @ StudioRef.ParticipantSummary(`run`, rep, _, group, p)
          if reporting.contains(rep) =>
        view(ref, rep, group, p)
      case ref @ StudioRef.ReportParticipant(`run`, rep, scale, ReportGroup.Level(group), _, p)
          if reporting.contains(rep) && served(scale, whole = false, ref) =>
        val keeps = Vector(labels.reporting(rep), group.label, p).mkString(" › ")
        ExplainVM(
          SummaryText(SummaryTextId.Explain, p),
          SummaryText(SummaryTextId.Keeps, keeps),
          Vector(
            Intent.Navigate(
              eyes4s.studio.app.nav.Location(
                Perspective.Compare,
                Vector(Place.Summary(rep), Place.Group(rep, group), Place.At(ref))
              )
            )
          )
        )
      case ref @ StudioRef.ReportParticipant(`run`, derived, scale, ReportGroup.Whole, _, p)
          if reporting.exists(rep => rep != derived && served(scale, whole = true, ref)) =>
        // The table is served from the generated ungrouped spec. Its id has
        // no saved navigation root. Keep its exact reference and explicitly
        // place it below the displayed spec rather than fabricating a grouped
        // report value under that spec.
        val rep   = reporting.get
        val keeps = Vector(labels.reporting(rep), SummaryText.reportGroup(ReportGroup.Whole), p)
          .mkString(" › ")
        ExplainVM(
          SummaryText(SummaryTextId.Explain, p),
          SummaryText(SummaryTextId.Keeps, keeps),
          Vector(
            Intent.Navigate(
              eyes4s.studio.app.nav.Location(
                Perspective.Compare,
                Vector(Place.Summary(rep), Place.At(ref))
              )
            )
          )
        )
    }

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
  ParticipantMeans,
  ParticipantNotes,
  PlotColumn,
  PlotRow,
  PlotSource,
  PlotValue,
  ProfileColumns,
  ScaleProfile,
  SlopeColumns,
  SlopeDelta,
  SlopeFrame,
  Slopegraphs
}
import eyes4s.studio.app.text.{Messages, SummaryText, SummaryTextId}
import eyes4s.studio.app.vm.{Labels, Shell}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.backend.{
  QueryRow,
  QueryStatus,
  ReportRole,
  ReportView,
  ResultSummary,
  RunId
}
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
    /** What the slopegraphs draw beyond their source (overhaul v4). */
    slopes: SlopeFrame,
    /** The participant plot's annotation column: the selected participant
      * and every participant with failed queries (review ticket 26).
      */
    plotNotes: ParticipantNotes,
    /** The headline's paired descriptive of the shown σ's contrast, when the
      * shown report has one (design review ticket 27).
      */
    paired: Option[PairedDescriptiveVM],
    profile: Option[Either[String, PlotSource]],
    participants: Option[Either[String, PlotSource]],
    queries: Option[Either[String, PlotSource]],
    notes: Vector[String],
    scales: Vector[ScaleChoice],
    freshness: Option[String],
    explain: Option[ExplainVM]
) derives CanEqual

object CompareSummaryVM:

  /** Curves, tables and their navigation use one whole-report evaluation.
    * An ungrouped spec already names it; grouped specs use the explicit
    * derived overall report, preserving filters and weighting.
    */
  private def wholeReport(s: CompareSummary, scale: ScaleIndex): Option[ReportView] =
    val derived = !s.spec.exists(_.groupBy.isEmpty)
    s.reports.get((scale, derived)).collect { case ReportAnswer.Answered(view) => view }

  /** The layout's view-model. `noteSlots` is how many lines the
    * participant plot's annotation column has room for: the drawn plot's
    * frame measured by its host, or the Results board's frame.
    */
  def of(
      s: CompareSummary,
      m: AppModel,
      messages: Messages = Messages.english,
      noteSlots: Int = ParticipantNotes.BoardSlots
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
        answered(s, m, run, r, freshness, messages, noteSlots)

  private def empty(status: Option[String], freshness: Option[String]) =
    CompareSummaryVM(
      status,
      None,
      SlopeFrame.empty,
      ParticipantNotes.empty,
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
      messages: Messages,
      noteSlots: Int
  ): CompareSummaryVM =
    val available = s.available
    val shown     = s.shown
    val scales    = r.scaleLabels.zipWithIndex.flatMap { (label, i) =>
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
              if available.isEmpty then SummaryText(SummaryTextId.NoScalesAvailable)
              else
                SummaryText(
                  SummaryTextId.ScaleUnavailable,
                  available.flatMap(a => r.scaleLabels.lift(a.value)).mkString(", ")
                )
            )
          )
        )
    }
    def displayed(scale: ScaleIndex) = s.reports.get((scale, false)).collect {
      case ReportAnswer.Answered(view) => view
    }
    val reporting                = s.reporting
    def label(scale: ScaleIndex) =
      r.scaleLabels.lift(scale.value).getOrElse(scale.value.toString)
    val means = for
      scale <- shown
      view  <- displayed(scale)
    yield ParticipantMeans.of(view, label(scale)).left.map(_.message)
    // Every σ the report has answered at, for the slopegraphs and Δ by σ.
    val everyMeans = available
      .flatMap(scale => displayed(scale).map(view => ParticipantMeans.of(view, label(scale))))
      .traverse(_.left.map(_.message))
    val participantPlot = means.map(
      _.flatMap(_ =>
        everyMeans.flatMap(ms =>
          SlopeColumns.standard.flatMap(Slopegraphs.source(ms, _)).left.map(_.message)
        )
      )
    )
    val slopes = everyMeans.toOption.fold(SlopeFrame.empty) { ms =>
      SlopeFrame(
        shown.map(scale => SummaryText(SummaryTextId.Scale, label(scale))),
        ParticipantNotes.selectedParticipant(m.selection.selected, run),
        Slopegraphs.thin(ms),
        ms.headOption.fold(Vector.empty[String]) { first =>
          val names = ParticipantNotes.shortNames(first.groups.map(_.group))
          first.groups.map(g => names.getOrElse(g.group, g.group.label))
        },
        available.flatMap { scale =>
          displayed(scale)
            .flatMap(PairedDescriptive.of)
            .map(p => SlopeDelta(label(scale), p.mean, p.higher, p.pairedN))
        }
      )
    }
    val plotNotes = means.flatMap(_.toOption).fold(ParticipantNotes.empty) { ms =>
      ParticipantNotes.of(
        ms,
        r.participants,
        s.queries.collect { case QueriesAnswer.Answered(rows) => rows },
        ParticipantNotes.selectedParticipant(m.selection.selected, run),
        noteSlots
      )
    }
    val paired = for
      scale <- shown
      view  <- displayed(scale)
      p     <- PairedDescriptive.of(view)
    yield PairedDescriptiveVM.of(p, SummaryText(SummaryTextId.Scale, label(scale)))
    val profile = reporting.map { _ =>
      m.document
        .run(run)
        .flatMap(rr => m.document.analysis(rr.analysis))
        .map(_.recipe.scales)
        .toRight(s"run ${run.number} has no analysis revision in the document")
        .flatMap(scales =>
          val reports = scales.values.indices.toVector
            .flatMap(i => ScaleIndex.of(i).toOption.flatMap(wholeReport(s, _)))
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
      overall <- wholeReport(s, scale)
    yield participantTable(
      run,
      rep,
      scale,
      r,
      grouped,
      overall,
      available.map(k => (label(k), displayed(k).flatMap(PairedDescriptive.of)))
    )
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
      slopes,
      plotNotes,
      paired,
      profile,
      participants,
      queries,
      notes,
      scales,
      freshness,
      explain(s, m, run, reporting, messages)
    )

  /** The participant table (overhaul v4, `results.html`): one row per
    * participant, its contributing queries, its mean D and n in each group
    * at the shown σ, then its paired Δ at every σ. Every value is served or
    * computed by the paired descriptive; a cell without one is missing.
    */
  def participantTable(
      run: RunId,
      rep: ReportingId,
      scale: ScaleIndex,
      r: ResultSummary,
      grouped: eyes4s.studio.core.backend.ReportView,
      overall: eyes4s.studio.core.backend.ReportView,
      deltas: Vector[(String, Option[PairedDescriptive])] = Vector.empty
  ): Either[String, PlotSource] =
    import SummaryBoardTextId.*
    def column(id: String, header: String, format: ColumnFormat) =
      ColumnId.of(id).map(PlotColumn(_, header, format))
    val label  = r.scaleLabels.lift(scale.value).getOrElse(scale.value.toString)
    val groups = grouped.cells.collect {
      case c if c.role == ReportRole.Difference && c.group.nonEmpty => c.group.get
    }.distinct
    val shorts  = ParticipantNotes.shortNames(groups)
    val columns =
      Vector(
        column("participant", SummaryText(SummaryTextId.Participant), ColumnFormat.Label),
        column("queries", SummaryBoardText(QueriesHeader), ColumnFormat.Count)
      ) ++ groups.zipWithIndex.flatMap((g, k) =>
        Vector(
          column(s"group-$k", SummaryBoardText(GroupD, g.label), ColumnFormat.Signed(2)),
          column(
            s"group-$k-n",
            SummaryBoardText(GroupN, shorts.getOrElse(g, g.label)),
            ColumnFormat.Count
          )
        )
      ) ++ deltas.zipWithIndex.map((d, k) =>
        column(s"delta-$k", SummaryBoardText(DeltaAt, d._1), ColumnFormat.Signed(2))
      )
    val ids = overall.participants.collect {
      case p if p.role == ReportRole.Difference && p.group.isEmpty => p.participant
    }.distinct
    val rows = ids.traverse { id =>
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
        Vector(PlotValue.Text(id), PlotValue.Number(accounting.contributing.toDouble)) ++
          groups.flatMap { g =>
            val cell = grouped.participant(Some(g), ReportRole.Difference, id)
            Vector(
              cell.flatMap(_.value).fold(PlotValue.Missing)(PlotValue.Number(_)),
              cell.fold(PlotValue.Missing)(c => PlotValue.Number(c.queries.toDouble))
            )
          } ++
          deltas.map((_, paired) =>
            paired
              .flatMap(_.differences.find(_.participant == id))
              .fold(PlotValue.Missing)(d => PlotValue.Number(d.d))
          )
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
            PlotSource(
              SummaryText(SummaryTextId.ParticipantCaption, label, run.number.toString),
              cs,
              rs
            ).left
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
    val label = r.scaleLabels.lift(scale.value).getOrElse(scale.value.toString)
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
        case QueryStatus.ByDesign(_) =>
          (SummaryText(StatusByDesign), Vector.fill(3)(PlotValue.Missing))
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
                Perspective.Results,
                Vector(Place.Summary(rep), Place.Group(rep, group), Place.At(ref))
              )
            )
          )
        )
      case ref @ StudioRef.ReportParticipant(`run`, _, scale, ReportGroup.Whole, _, p)
          if reporting.isDefined && wholeReport(s, scale).exists(
            _.participants.exists(_.ref == ref)
          ) =>
        // Keep the exact whole-report reference below the current reporting
        // root, for both saved ungrouped and explicitly derived evaluations.
        val rep   = reporting.get
        val keeps = Vector(labels.reporting(rep), SummaryText.reportGroup(ReportGroup.Whole), p)
          .mkString(" › ")
        ExplainVM(
          SummaryText(SummaryTextId.Explain, p),
          SummaryText(SummaryTextId.Keeps, keeps),
          Vector(
            Intent.Navigate(
              eyes4s.studio.app.nav.Location(
                Perspective.Results,
                Vector(Place.Summary(rep), Place.At(ref))
              )
            )
          )
        )
    }

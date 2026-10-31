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

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.plot.{ParticipantNotes, Slopegraphs}
import eyes4s.studio.app.text.{Format, Messages}
import eyes4s.studio.core.backend.{QueryStatus, ReportRole, ReportView}
import eyes4s.studio.core.document.{ReportingId, ReportingWeight}
import eyes4s.studio.core.selection.ScaleIndex

/** The words of the Results summary's chrome (overhaul v4, `results.html`). */
enum SummaryBoardTextId derives CanEqual:
  case PlotTitle, PlotSubtitle, ParticipantMeans, PooledQueries, PairedN
  case Choice, EachParticipant, EveryQuery, ReportAt, EditReporting
  case MeanOf, OneParticipant, Selected, Thin, Calibration
  case TableSubtitle, Contribute, FailedQueries, ThinQueries, ThinQuery
  case Caveat, More, EnterOpens
  case QueriesHeader, GroupD, GroupN, DeltaAt

object SummaryBoardText:
  def english(id: SummaryBoardTextId): String =
    import SummaryBoardTextId.*
    id match
      case PlotTitle        => "{0} vs {1}"
      case PlotSubtitle     => "{0} · paired n {1}"
      case ParticipantMeans => "participant means of D, equal weight"
      case PooledQueries    => "pooled queries' D"
      case PairedN          => "paired n {0}"
      case Choice           => "{0} · {1}"
      case EachParticipant  => "each participant equally"
      case EveryQuery       => "every query equally"
      case ReportAt         => "Report at {0}"
      case EditReporting    => "Edit reporting…"
      case MeanOf           => "mean of {0}"
      case OneParticipant   => "one participant"
      case Selected         => "{0}, selected"
      case Thin             => "n<3 in a group"
      case Calibration      => "σ in degrees, declared {0} px/° (not calibrated)"
      case TableSubtitle    => "{0} participants · group D at {1}, Δ at every σ"
      case Contribute       =>
        "{0} of {1} queries contribute ({2} not admitted, {3} no match, {4} failed)"
      case FailedQueries => "{0}: {1} failed"
      case ThinQueries   => "{0}: {1} {2} queries"
      case ThinQuery     => "{0}: 1 {1} query"
      case Caveat        =>
        "D is spatial correspondence, not replay; it mixes reinstatement with salience shared " +
          "across images and residual centre bias."
      case More          => "More"
      case EnterOpens    => "Enter opens {0}'s queries"
      case QueriesHeader => "Queries"
      case GroupD        => "{0} D"
      case GroupN        => "n {0}"
      case DeltaAt       => "Δ {0}"

  def apply(id: SummaryBoardTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

/** One choice in the plot header's reporting menu: a saved reporting
  * specification or a σ to report at.
  */
enum HeaderChoice derives CanEqual:
  case Spec(id: ReportingId, label: String, chosen: Boolean)
  case Scale(scale: ScaleIndex, label: String, chosen: Boolean, available: Boolean)

/** One key of the legend under the slopegraphs. */
enum SlopeKey derives CanEqual:
  case Mean(text: String)
  case Participant(text: String)
  case Selected(text: String)
  case Thin(text: String)
  case Note(text: String)

/** The Results summary's chrome around its plot and table. */
final case class SummaryBoardVM(
    plotTitle: Option[String],
    plotSubtitle: String,
    reporting: String,
    choices: Vector[HeaderChoice],
    legend: Vector[SlopeKey],
    tableSubtitle: String,
    footer: Vector[String],
    hint: Option[String]
) derives CanEqual

object SummaryBoard:
  import SummaryBoardTextId.*

  val empty: SummaryBoardVM =
    SummaryBoardVM(None, "", "", Vector.empty, Vector.empty, "", Vector.empty, None)

  def vm(s: CompareSummary, m: AppModel, v: CompareSummaryVM): SummaryBoardVM =
    def t(id: SummaryBoardTextId, args: String*) = SummaryBoardText(id, args*)
    (s.answered, s.spec) match
      case (Some(r), Some(spec)) =>
        val shown                        = s.shown
        def scaleName(scale: ScaleIndex) =
          eyes4s.studio.app.text.SummaryText(
            eyes4s.studio.app.text.SummaryTextId.Scale,
            r.scaleLabels.lift(scale.value).getOrElse(scale.value.toString)
          )
        val report: Option[ReportView] = shown.flatMap(scale =>
          s.reports.get((scale, false)).collect { case ReportAnswer.Answered(view) => view }
        )
        val contrast = report.flatMap(_.contrast(ReportRole.Difference))
        val paired   = report.flatMap(PairedDescriptive.of)
        val weight   = spec.weighting match
          case ReportingWeight.ParticipantMeans => (t(ParticipantMeans), t(EachParticipant))
          case ReportingWeight.PooledQueries    => (t(PooledQueries), t(EveryQuery))
        val n        = paired.map(_.pairedN).orElse(contrast.map(_.pairedN))
        val subtitle = n.fold(weight._1)(k => t(PlotSubtitle, weight._1, k.toString))
        val choices  = m.document.reporting.map(r =>
          HeaderChoice.Spec(r.id, r.name, s.reporting.contains(r.id))
        ) ++ v.scales.map(c =>
          HeaderChoice.Scale(c.scale, t(ReportAt, c.label), c.chosen, c.available)
        )
        val means    = paired.map(_ => r.participants.size)
        val selected =
          shown.flatMap(_ =>
            ParticipantNotes.selectedParticipant(
              m.selection.selected,
              s.run.getOrElse(r.run)
            )
          )
        val geometry = s.run
          .flatMap(m.document.run)
          .flatMap(run => m.document.dataset(run.dataset))
          .map(d => t(Calibration, Format.decimal(d.geometry.pixelsPerDegree.value, 0)))
        val legend =
          Option
            .when(v.participantPlot.exists(_.isRight))(
              means.map(k => SlopeKey.Mean(t(MeanOf, k.toString))).toVector ++
                Vector(SlopeKey.Participant(t(OneParticipant))) ++
                selected.map(p => SlopeKey.Selected(t(Selected, p))) ++
                Option.when(v.slopes.thin.nonEmpty)(SlopeKey.Thin(t(Thin))) ++
                geometry.map(SlopeKey.Note(_))
            )
            .getOrElse(Vector.empty)
        val counts     = r.participants
        val contribute = Option.when(counts.nonEmpty)(
          t(
            Contribute,
            Format.count(counts.map(_.contributing.toLong).sum),
            Format.count(counts.map(_.requested.toLong).sum),
            Format.count(counts.map(_.notAdmitted.toLong).sum),
            Format.count(counts.map(_.noMatch.toLong).sum),
            Format.count(counts.map(_.failed.toLong).sum)
          )
        )
        val rows = s.queries
          .collect { case QueriesAnswer.Answered(rows) => rows }
          .getOrElse(Vector.empty)
        val failed = rows
          .filter(q =>
            q.status match
              case QueryStatus.Failed(_) | QueryStatus.FailedAtScales(_) => true
              case _                                                     => false
          )
          .groupBy(_.query.participant)
          .toVector
          .sortBy(_._1)
          .map { (p, qs) =>
            // "ret_04 (Forgotten), ret_11, ret_16 (Remembered)": the trials of
            // each response in turn, the response named after its last.
            val byResponse = qs.groupBy(_.response).toVector.sortBy(_._1.label)
            val listed     = byResponse
              .map((g, gs) => gs.map(_.query.trial).sorted.mkString(", ") + s" (${g.label})")
            t(FailedQueries, p, listed.mkString(", "))
          }
        val thin = report.toVector
          .flatMap(_.participants)
          .filter(p => p.role == ReportRole.Difference && p.group.nonEmpty)
          .filter(_.queries < Slopegraphs.ThinQueries)
          .groupBy(p => (p.queries, p.group.map(_.label).getOrElse("")))
          .toVector
          .sortBy(_._1)
          .map { case ((k, g), ps) =>
            val who = ps.map(_.participant).distinct.sorted.mkString(", ")
            if k == 1 then t(ThinQuery, who, g) else t(ThinQueries, who, k.toString, g)
          }
        SummaryBoardVM(
          paired.map(p => t(PlotTitle, p.minuend.label, p.subtrahend.label)),
          subtitle,
          t(Choice, spec.name, weight._2),
          choices,
          legend,
          shown.fold("")(scale => t(TableSubtitle, counts.size.toString, scaleName(scale))),
          contribute.toVector ++ failed ++ thin,
          selected.map(p => t(EnterOpens, p))
        )
      case _ => empty

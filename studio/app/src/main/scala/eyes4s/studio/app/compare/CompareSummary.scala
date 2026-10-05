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

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.backend.{BackendError, QueryRow, ReportView, ResultSummary, RunId}
import eyes4s.studio.core.document.{Perspective, ReportingId, ReportingSpec}
import eyes4s.studio.core.selection.ScaleIndex

/** What the backend answered for a run's summary. */
enum SummaryAnswer derives CanEqual:
  case Answered(summary: ResultSummary)
  case Refused(error: BackendError)
  case Failed(reason: String)

/** What the backend answered for a run's queries. */
enum QueriesAnswer derives CanEqual:
  case Answered(rows: Vector[QueryRow])
  case Failed(reason: String)

/** What eyes4s answered for the displayed reporting spec at one scale. */
enum ReportAnswer derives CanEqual:
  case Answered(report: ReportView)
  case Refused(error: BackendError)
  case Failed(reason: String)

/** An input to the summary layout: a backend answer or a user action. */
enum SummaryIntent derives CanEqual:
  case SummaryRead(run: RunId, answer: SummaryAnswer)
  case QueriesRead(run: RunId, answer: QueriesAnswer)
  case ReportRead(run: RunId, scale: ScaleIndex, overall: Boolean, answer: ReportAnswer)
  case ChooseScale(scale: ScaleIndex)

/** What the summary layout asks of the app and the backend. */
enum SummaryEffect derives CanEqual:
  case App(intent: Intent)
  case RequestSummary(run: RunId)
  case RequestQueries(run: RunId)
  case RequestReport(run: RunId, reporting: ReportingSpec, scale: ScaleIndex, overall: Boolean)

/** Compare's summary layout (ticket S8.6; Results.dc.html): the shown run's
  * summary and queries as the backend serves them, and the σ the
  * participant plot and tables show. It follows the document's shown run
  * (S8.8: results never swap without the user choosing Show) and asks for
  * a run's summary and queries once. Pure; a host performs the effects.
  */
final case class CompareSummary(
    run: Option[RunId],
    reporting: Option[ReportingId],
    summary: Option[SummaryAnswer],
    queries: Option[QueriesAnswer],
    scale: Option[ScaleIndex],
    spec: Option[ReportingSpec] = None,
    reports: Map[(ScaleIndex, Boolean), ReportAnswer] = Map.empty
) derives CanEqual

object CompareSummary:

  val empty: CompareSummary = CompareSummary(None, None, None, None, None)

  /** An explicit eyes4s-derived ungrouped report for the overall table
    * columns. Its identity differs so every returned ref names this spec.
    */
  private def overall(spec: ReportingSpec): Option[ReportingSpec] =
    ReportingSpec
      .of(
        ReportingId.of(spec.id.value + "-overall").toOption.getOrElse(spec.id),
        spec.name + " (overall)",
        None,
        spec.filters,
        spec.minimumPerGroup,
        spec.weighting
      )
      .toOption

  private def reportEffects(s: CompareSummary): Vector[SummaryEffect] =
    for
      result <- s.answered.toVector
      spec   <- s.spec.toVector
      (_, i) <- result.scales.zipWithIndex
      scale  <- ScaleIndex.of(i).toOption.toVector
      whole  <- Vector(false, true)
      report <- (if whole then overall(spec).toVector else Vector(spec))
      if !s.reports.contains((scale, whole))
    yield SummaryEffect.RequestReport(s.run.get, report, scale, whole)

  /** The run Compare shows. */
  def shownRun(m: AppModel): Option[RunId] = m.document.presentation.shownRun

  /** The reporting spec Compare's trail is in, else the document's first.
    * A trail naming a spec the document no longer holds (Save as… undone)
    * falls back too, so Compare never shows a missing spec.
    */
  def reporting(m: AppModel): Option[ReportingId] =
    m.navigation
      .trail(Perspective.Compare)
      .collectFirst {
        case Place.Summary(r)  => r
        case Place.Group(r, _) => r
      }
      .filter(r => m.document.reporting.exists(_.id == r))
      .orElse(m.document.reporting.headOption.map(_.id))

  /** Follows the model: a newly shown run is read afresh. */
  def sync(s: CompareSummary, m: AppModel): (CompareSummary, Vector[SummaryEffect]) =
    val run  = shownRun(m)
    val rep  = reporting(m)
    val spec = rep.flatMap(id => m.document.reporting.find(_.id == id))
    if run == s.run && spec == s.spec then (s.copy(reporting = rep), Vector.empty)
    else if run == s.run then
      val next = s.copy(reporting = rep, spec = spec, reports = Map.empty)
      (next, reportEffects(next))
    else
      (
        CompareSummary(run, rep, None, None, None, spec),
        run.toVector.flatMap(r =>
          Vector(SummaryEffect.RequestSummary(r), SummaryEffect.RequestQueries(r))
        )
      )

  /** A backend answer or a user action. An answer for a run no longer shown
    * is dropped; only a scale the participant means are served at can be
    * chosen.
    */
  def update(
      s: CompareSummary,
      intent: SummaryIntent
  ): (CompareSummary, Vector[SummaryEffect]) =
    intent match
      case SummaryIntent.SummaryRead(run, a) if s.run.contains(run) =>
        val next = s.copy(summary = Some(a))
        (next, reportEffects(next))
      case SummaryIntent.QueriesRead(run, a) if s.run.contains(run) =>
        (s.copy(queries = Some(a)), Vector.empty)
      case SummaryIntent.ReportRead(run, scale, whole, a) if s.run.contains(run) =>
        (s.copy(reports = s.reports.updated((scale, whole), a)), Vector.empty)
      case SummaryIntent.ChooseScale(scale) if s.available.contains(scale) =>
        (s.copy(scale = Some(scale)), Vector.empty)
      case _ => (s, Vector.empty)

  extension (s: CompareSummary)
    /** The answered summary, if any. */
    def answered: Option[ResultSummary] = s.summary.collect { case SummaryAnswer.Answered(r) =>
      r
    }

    /** The scales the summary's participant means are at: the heuristic
      * grand-mean check of [[ParticipantMeans.of]], until ResultSummary
      * declares its means scale (bead bd-01M420VXE7NFGZHSM71SGY7KW6, the
      * means-scale identity). The σ selector, and with it the query table's
      * σ, is gated by it.
      */
    def available: Vector[ScaleIndex] =
      for
        r      <- s.answered.toVector
        i      <- r.scales.indices.toVector
        scale  <- ScaleIndex.of(i).toOption.toVector
        answer <- s.reports.get((scale, false)).toVector
        report <- answer match
          case ReportAnswer.Answered(view) => Vector(view)
          case _                           => Vector.empty
        if report.scale == scale.value
      yield scale

    /** The σ shown: the one chosen, else the first the means are at. */
    def shown: Option[ScaleIndex] = s.scale.orElse(s.available.headOption)

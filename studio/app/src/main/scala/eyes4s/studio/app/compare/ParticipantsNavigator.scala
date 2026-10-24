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
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.text.{Format, ParticipantsNavText, ParticipantsNavTextId}
import eyes4s.studio.core.backend.{ReportRole, RunId}
import eyes4s.studio.core.document.{Perspective, ReportingSpec}
import eyes4s.studio.core.selection.{ReportGroup, ScaleIndex, StudioRef}

/** One served overall participant cell, with separately labelled run accounting. */
final case class ParticipantNavRow(
    ref: StudioRef,
    participant: String,
    value: String,
    queries: String,
    accounting: String,
    spoken: String,
    selected: Boolean,
    cursor: Boolean,
    open: Intent
) derives CanEqual

final case class ParticipantsNavigatorVM(
    heading: String,
    accounting: String,
    status: Option[String],
    rows: Vector[ParticipantNavRow]
) derives CanEqual:
  def cursorText: Option[String] = rows.find(_.cursor).map(_.spoken)

/** A row cursor belongs to the exact run/report specification/scale context. */
final case class ParticipantsNavigator(
    context: Option[(RunId, ReportingSpec, ScaleIndex)],
    cursor: Option[StudioRef]
) derives CanEqual

object ParticipantsNavigator:
  val empty: ParticipantsNavigator = ParticipantsNavigator(None, None)

  def follow(nav: ParticipantsNavigator, s: CompareSummary): ParticipantsNavigator =
    val context = for r <- s.run; p <- s.spec; k <- s.shown yield (r, p, k)
    if context == nav.context then nav else ParticipantsNavigator(context, None)

  /** No means, counts, groups or refs are inferred from a plot or recomputed here. */
  def vm(nav: ParticipantsNavigator, s: CompareSummary, m: AppModel): ParticipantsNavigatorVM =
    import ParticipantsNavTextId.*
    def result(status: String) = ParticipantsNavigatorVM("", "", Some(status), Vector.empty)
    (s.run, s.spec, s.shown, s.answered) match
      case (None, _, _, _) => result(ParticipantsNavText(NoRun))
      case (Some(run), Some(spec), Some(scale), Some(summary)) =>
        val whole     = spec.groupBy.nonEmpty
        val requested = if whole then CompareSummary.overall(spec) else Some(spec)
        s.reports.get((scale, whole)) match
          case Some(ReportAnswer.Answered(report)) =>
            val label = summary.scaleLabels.lift(scale.value).getOrElse(scale.value.toString)
            val cells = report.participants.filter(p =>
              p.group.isEmpty && p.role == ReportRole.Difference
            )
            val valid =
              summary.run == run && report.run == run && report.scale == scale.value &&
                requested.exists(_.id == report.reporting) &&
                cells.map(_.participant).distinct.size == cells.size &&
                cells.forall(p =>
                  p.ref == StudioRef.ReportParticipant(
                    run,
                    report.reporting,
                    scale,
                    ReportGroup.Whole,
                    ReportRole.Difference,
                    p.participant
                  )
                )
            if !valid then
              result(
                ParticipantsNavText(Invalid, run.label, spec.id.value, scale.value.toString)
              )
            else
              val selected = QueriesNavigator.selection(m)
              val home     = nav.cursor
                .filter(ref => cells.exists(_.ref == ref))
                .orElse(cells.find(p => selected.contains(p.ref)).map(_.ref))
                .orElse(cells.headOption.map(_.ref))
              val rows = cells.traverse { p =>
                summary.participants
                  .find(_.participant == p.participant)
                  .toRight(ParticipantsNavText(MissingCounts, run.label, p.participant))
                  .map { counts =>
                    val value =
                      p.value.fold(ParticipantsNavText(MissingValue))(Format.signed(_, 2))
                    val n            = Format.count(p.queries.toLong)
                    val contributing = Format.count(counts.contributing.toLong)
                    val requested    = Format.count(counts.requested.toLong)
                    val failed       = Format.count(counts.failed.toLong)
                    ParticipantNavRow(
                      p.ref,
                      p.participant,
                      value,
                      n,
                      ParticipantsNavText(Row, contributing, requested, failed),
                      ParticipantsNavText(
                        Spoken,
                        p.participant,
                        value,
                        n,
                        contributing,
                        requested,
                        failed
                      ),
                      selected.contains(p.ref),
                      home.contains(p.ref),
                      Intent.Navigate(
                        Location(
                          Perspective.Compare,
                          Vector(Place.Summary(spec.id), Place.At(p.ref))
                        )
                      )
                    )
                  }
              }
              rows.fold(
                result,
                rs =>
                  ParticipantsNavigatorVM(
                    ParticipantsNavText(Heading, label),
                    ParticipantsNavText(Accounting),
                    Option.when(rs.isEmpty)(ParticipantsNavText(Empty, run.label)),
                    rs
                  )
              )
          case Some(ReportAnswer.Refused(e))  => result(e.message)
          case Some(ReportAnswer.Failed(why)) => result(why)
          case None                           => result(ParticipantsNavText(Reading, run.label))
      case (Some(run), _, _, _) =>
        s.summary match
          case Some(SummaryAnswer.Refused(e))  => result(e.message)
          case Some(SummaryAnswer.Failed(why)) => result(why)
          case _ => result(ParticipantsNavText(Reading, run.label))

  def key(
      nav: ParticipantsNavigator,
      vm: ParticipantsNavigatorVM,
      key: NavigatorKey
  ): (ParticipantsNavigator, Option[Intent]) =
    val rows             = vm.rows
    val current          = rows.indexWhere(_.cursor).max(0)
    def move(index: Int) =
      (nav.copy(cursor = rows.lift(index.max(0).min(rows.size - 1)).map(_.ref)), None)
    if rows.isEmpty then (nav.copy(cursor = None), None)
    else
      key match
        case NavigatorKey.Up       => move(current - 1)
        case NavigatorKey.Down     => move(current + 1)
        case NavigatorKey.First    => move(0)
        case NavigatorKey.Last     => move(rows.size - 1)
        case NavigatorKey.Activate =>
          (nav.copy(cursor = Some(rows(current).ref)), Some(rows(current).open))
        case _ => (nav, None)

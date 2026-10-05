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

package eyes4s.studio.app.plot

import cats.syntax.all.*
import eyes4s.studio.app.text.{ParticipantText, ProfileText, ProfileTextId}
import eyes4s.studio.core.backend.{QueryRow, QueryStatus, ReportRole, ReportView, ResultSummary, Response, RunId}
import eyes4s.studio.core.document.{ReportingId, ScaleSet, Sigma}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** Why a run's scale profile could not be read from its summary and its
  * declared scales. Every case names the run and the scale, group or
  * participant that failed.
  */
enum ProfileError derives CanEqual:

  /** The run declares `declared` scales but its summary has `served`. */
  case ScaleCount(run: RunId, declared: Int, served: Int)

  /** The summary's scale `index` is labelled `label`, which is not `sigma`. */
  case ScaleLabel(run: RunId, index: Int, label: String, sigma: Sigma)

  /** The summary lists `group` more than once. */
  case DuplicateGroup(run: RunId, group: Response)

  /** The summary lists `participant` more than once. */
  case DuplicateParticipant(run: RunId, participant: String)

  /** A scale index could not be formed. */
  case Scale(run: RunId, index: Int)

  def message: String = this match
    case ScaleCount(r, d, s) =>
      s"Scale profile of run ${r.number}: $d scales are declared but the summary has $s."
    case ScaleLabel(r, i, l, s) =>
      s"Scale profile of run ${r.number}: scale $i is labelled $l, not ${s.render}."
    case DuplicateGroup(r, g) =>
      s"Scale profile of run ${r.number}: group ${g.label} is listed twice."
    case DuplicateParticipant(r, p) =>
      s"Scale profile of run ${r.number}: participant $p is listed twice."
    case Scale(r, i) => s"Scale profile of run ${r.number}: scale $i is not a scale index."

/** One point of a profile: the series' mean D at one scale, missing when the
  * summary serves none there.
  */
final case class ProfilePoint(
    ref: StudioRef,
    scale: ScaleIndex,
    label: String,
    sigma: Sigma,
    d: Option[Double]
) derives CanEqual

/** One series of a profile: a group's grand means over its `n`
  * participants, or one participant's means over its `n` contributing
  * queries, at each declared scale in order.
  */
final case class ProfileSeries(name: String, n: String, points: Vector[ProfilePoint])
    derives CanEqual

/** A run's scale profile under one reporting spec (ticket S4.5d): each
  * group's grand mean D at every declared scale, and every participant's
  * mean D over all its queries at every scale, every value as the summary
  * served it.
  */
final case class ScaleProfile(
    run: RunId,
    reporting: ReportingId,
    groups: Vector[ProfileSeries],
    participants: Vector[ProfileSeries]
) derives CanEqual

/** The columns of the scale profile's value source. */
final case class ProfileColumns(
    series: ColumnId,
    scale: ColumnId,
    sigma: ColumnId,
    d: ColumnId,
    n: ColumnId
) derives CanEqual

object ProfileColumns:
  val standard: Either[PlotSourceError, ProfileColumns] =
    for
      series <- ColumnId.of("series")
      scale  <- ColumnId.of("scale")
      sigma  <- ColumnId.of("sigma")
      d      <- ColumnId.of("d")
      n      <- ColumnId.of("n")
    yield ProfileColumns(series, scale, sigma, d, n)

object ScaleProfile:

  /** Builds the profile from the report evaluations at each declared scale.
    * `grouped` supplies the visible level series and `overall` supplies each
    * participant's ungrouped D series. Both were evaluated by eyes4s.
    */
  def of(
      grouped: Vector[ReportView],
      overall: Vector[ReportView],
      scales: ScaleSet,
      labels: Vector[String]
  ): Either[ProfileError, ScaleProfile] =
    grouped.headOption.toRight(ProfileError.Scale(RunId(0), 0)).flatMap { first =>
      val run = first.run
      val declared = scales.values
      for
        _ <- Either.cond(grouped.size == declared.size && overall.size == declared.size && labels.size == declared.size, (),
          ProfileError.ScaleCount(run, declared.size, grouped.size))
        at <- declared.zipWithIndex.traverse { case (sigma, i) =>
          ScaleIndex.of(i).leftMap(_ => ProfileError.Scale(run, i)).map((_, sigma))
        }
      yield
        val levels = first.cells.collect {
          case c if c.role == ReportRole.Difference => c.group
        }.flatten.distinct
        val people = overall.headOption.toVector.flatMap(_.participants.collect {
          case p if p.role == ReportRole.Difference && p.group.isEmpty => p.participant
        }).distinct
        def groupedPoint(group: Response, index: ScaleIndex): Option[(Double, String)] =
          grouped.lift(index.value).flatMap(_.cell(Some(group), ReportRole.Difference)).flatMap(c =>
            c.estimate.map(_ -> ParticipantText.participants(c.participants))
          )
        def overallPoint(person: String, index: ScaleIndex): Option[(Double, String)] =
          overall.lift(index.value).flatMap(_.participant(None, ReportRole.Difference, person)).flatMap(p =>
            p.value.map(_ -> ParticipantText.queries(p.queries))
          )
        ScaleProfile(
          run,
          first.reporting,
          levels.map(group =>
            ProfileSeries(
              group.label,
              groupedPoint(group, at.head._1).fold(ParticipantText.participants(0))(_._2),
              at.map((index, sigma) =>
                ProfilePoint(
                  grouped(index.value).cell(Some(group), ReportRole.Difference).map(_.ref)
                    .getOrElse(StudioRef.GroupCell(run, first.reporting, index, group)),
                  index, labels(index.value), sigma,
                  groupedPoint(group, index).map(_._1)
                )
              )
            )
          ),
          people.map(person =>
            ProfileSeries(
              person,
              overallPoint(person, at.head._1).fold(ParticipantText.queries(0))(_._2),
              at.map((index, sigma) =>
                ProfilePoint(
                  overall(index.value).participant(None, ReportRole.Difference, person).map(_.ref)
                    .getOrElse(StudioRef.ParticipantSummary(run, first.reporting, index, None, person)),
                  index, labels(index.value), sigma,
                  overallPoint(person, index).map(_._1)
                )
              )
            )
          )
        )
    }

  /** The profile of `summary` under `reporting`, at the run's declared
    * `scales` (its analysis revision's scale set, in order), which must be
    * the summary's scales one for one. A group's point is its
    * [[StudioRef.GroupCell]] at that scale; a participant's is its
    * [[StudioRef.ParticipantSummary]] over all its queries (no group). A
    * participant with no contributing query has no means. Nothing is
    * computed here.
    */
  def of(
      summary: ResultSummary,
      reporting: ReportingId,
      scales: ScaleSet
  ): Either[ProfileError, ScaleProfile] =
    val run      = summary.run
    val declared = scales.values
    val labels   = summary.groups.map(_.label)
    val ids      = summary.participants.map(_.participant)
    for
      _ <- Either.cond(
        declared.size == summary.scales.size,
        (),
        ProfileError.ScaleCount(run, declared.size, summary.scales.size)
      )
      at <- declared.zip(summary.scales).zipWithIndex.traverse { case ((sigma, label), i) =>
        for
          _ <- Either.cond(
            sigma.render == s"σ $label",
            (),
            ProfileError.ScaleLabel(run, i, label, sigma)
          )
          index <- ScaleIndex.of(i).leftMap(_ => ProfileError.Scale(run, i))
        yield (index, label, sigma)
      }
      _ <- labels
        .diff(labels.distinct)
        .headOption
        .map(ProfileError.DuplicateGroup(run, _))
        .toLeft(())
      _ <- ids
        .diff(ids.distinct)
        .headOption
        .map(ProfileError.DuplicateParticipant(run, _))
        .toLeft(())
    yield
      def points(ref: ScaleIndex => StudioRef, ds: Vector[Double], served: Boolean) =
        at.map((index, label, sigma) =>
          ProfilePoint(
            ref(index),
            index,
            label,
            sigma,
            ds.lift(index.value).filter(_ => served)
          )
        )
      ScaleProfile(
        run,
        reporting,
        summary.groups.map(g =>
          ProfileSeries(
            g.label.label,
            ParticipantText.participants(g.n),
            points(StudioRef.GroupCell(run, reporting, _, g.label), g.dByScale, true)
          )
        ),
        summary.participants.map(p =>
          ProfileSeries(
            p.participant,
            ParticipantText.queries(p.contributing),
            points(
              StudioRef.ParticipantSummary(run, reporting, _, None, p.participant),
              p.all.dByScale,
              p.contributing > 0
            )
          )
        )
      )

  /** One query's scale profile (ticket S8.5; Main.dc.html, the contrast
    * group's "Scale profile" tab): its D at each of the run's `scales`, as
    * the run's query row served it, each point its
    * [[StudioRef.QueryContrast]]. `labels` are the run's scale labels, one
    * for each declared scale. A query that does not contribute has no D at
    * any scale: every point is missing, never zero.
    */
  def ofQuery(
      run: RunId,
      reporting: ReportingId,
      row: QueryRow,
      labels: Vector[String],
      scales: ScaleSet
  ): Either[ProfileError, ScaleProfile] =
    val declared = scales.values
    for
      _ <- Either.cond(
        declared.size == labels.size,
        (),
        ProfileError.ScaleCount(run, declared.size, labels.size)
      )
      at <- declared.zip(labels).zipWithIndex.traverse { case ((sigma, label), i) =>
        for
          _ <- Either.cond(
            degreesOf(label).contains(sigma.degrees),
            (),
            ProfileError.ScaleLabel(run, i, label, sigma)
          )
          index <- ScaleIndex.of(i).leftMap(_ => ProfileError.Scale(run, i))
        yield (index, label, sigma)
      }
    yield
      val ds = row.status match
        case QueryStatus.Contributing(_, _, d) => d
        case _                                 => Vector.empty
      ScaleProfile(
        run,
        reporting,
        Vector(
          ProfileSeries(
            s"${row.query.label} · ${row.item}",
            ProfileText(ProfileTextId.QueryN),
            at.map((index, label, sigma) =>
              ProfilePoint(
                StudioRef.QueryContrast(run, index, row.query),
                index,
                label,
                sigma,
                ds.lift(index.value)
              )
            )
          )
        ),
        Vector.empty
      )

  /** A served scale label's degrees ("2°", "0.5°"), compared with a declared
    * σ by value rather than as text.
    */
  private def degreesOf(label: String): Option[Double] =
    label.trim.stripSuffix("°").trim.toDoubleOption

  /** The profile as a value source: every group's points, then every
    * participant's, each series in scale order, each point a row with its
    * own ref. A mean the summary does not serve is missing, never zero.
    */
  def source(
      profile: ScaleProfile,
      columns: ProfileColumns
  ): Either[PlotSourceError, PlotSource] =
    val rows = (profile.groups ++ profile.participants).flatMap(s =>
      s.points.map(p =>
        PlotRow(
          p.ref,
          Vector(
            PlotValue.Text(s.name),
            PlotValue.Text(p.label),
            PlotValue.Number(p.sigma.degrees),
            p.d.fold(PlotValue.Missing)(PlotValue.Number(_)),
            PlotValue.Text(s.n)
          )
        )
      )
    )
    PlotSource(
      ProfileText(ProfileTextId.Caption, profile.run.number.toString),
      Vector(
        PlotColumn(columns.series, ProfileText(ProfileTextId.SeriesHeader), ColumnFormat.Label),
        PlotColumn(columns.scale, ProfileText(ProfileTextId.ScaleHeader), ColumnFormat.Label),
        PlotColumn(
          columns.sigma,
          ProfileText(ProfileTextId.SigmaHeader),
          ColumnFormat.Decimal(2)
        ),
        PlotColumn(columns.d, ProfileText(ProfileTextId.DHeader), ColumnFormat.Signed(2)),
        PlotColumn(columns.n, ProfileText(ProfileTextId.NHeader), ColumnFormat.Label)
      ),
      rows
    )

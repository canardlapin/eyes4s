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
import eyes4s.studio.core.backend.{
  QueryRow,
  QueryStatus,
  ReportRole,
  ReportView,
  Response,
  RunId
}
import eyes4s.studio.core.document.{ReportingId, ScaleSet, Sigma}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** Why a run's scale profile could not be read from its summary and its
  * declared scales. Every case names the run and the scale, group or
  * participant that failed.
  */
enum ProfileError derives CanEqual:

  /** The run declares `declared` scales but its summary has `served`. */
  case ScaleCount(run: RunId, declared: Int, served: Int)

  /** The summary's scale `index` differs from the declared scale. */
  case ScaleIdentity(run: RunId, index: Int, served: Sigma, declared: Sigma)

  /** The summary lists `group` more than once. */
  case DuplicateGroup(run: RunId, group: Response)

  /** The summary lists `participant` more than once. */
  case DuplicateParticipant(run: RunId, participant: String)

  /** No report payload was supplied for the declared scales. */
  case MissingReports(degrees: Vector[Double])

  /** The report indices differ from the declared scale indices. */
  case ReportScales(
      run: RunId,
      reporting: ReportingId,
      expected: Vector[Int],
      found: Vector[Int]
  )

  /** A report's explicit context disagrees with its series. */
  case ReportContext(
      run: RunId,
      reporting: ReportingId,
      scale: Int,
      foundRun: RunId,
      foundReporting: ReportingId,
      foundScale: Int
  )

  /** The served report has no cell for a series; no reference is invented. */
  case MissingSeries(
      run: RunId,
      reporting: ReportingId,
      scale: Int,
      group: Option[Response],
      participant: Option[String]
  )

  /** A scale index could not be formed. */
  case Scale(run: RunId, index: Int)

  def message: String = this match
    case ScaleCount(r, d, s) =>
      s"Scale profile of run ${r.number}: $d scales are declared but the summary has $s."
    case ScaleIdentity(r, i, l, s) =>
      s"Scale profile of run ${r.number}: scale $i is ${l.render}, not ${s.render}."
    case DuplicateGroup(r, g) =>
      s"Scale profile of run ${r.number}: group ${g.label} is listed twice."
    case DuplicateParticipant(r, p) =>
      s"Scale profile of run ${r.number}: participant $p is listed twice."
    case MissingReports(degrees) =>
      s"Scale profile has no reports for declared scales $degrees degrees."
    case ReportScales(r, rep, expected, found) =>
      s"Scale profile of ${r.label}, ${rep.value} requires report scales $expected but received $found."
    case ReportContext(r, rep, scale, foundRun, foundRep, foundScale) =>
      s"Scale profile of ${r.label}, ${rep.value}, scale $scale received ${foundRun.label}, ${foundRep.value}, scale $foundScale."
    case MissingSeries(r, rep, scale, group, participant) =>
      s"Scale profile of ${r.label}, ${rep.value}, scale $scale has no served cell for group $group, participant $participant."
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
      served: Vector[Sigma]
  ): Either[ProfileError, ScaleProfile] =
    grouped.headOption
      .toRight(ProfileError.MissingReports(scales.values.map(_.degrees)))
      .flatMap { first =>
        val run       = first.run
        val declared  = scales.values
        val overallId = overall.headOption.map(_.reporting).getOrElse(first.reporting)
        def checked(views: Vector[ReportView], id: ReportingId) =
          views
            .traverse { view =>
              Either.cond(
                view.run == run && view.reporting == id &&
                  declared.indices.contains(view.scale),
                view,
                ProfileError
                  .ReportContext(run, id, view.scale, view.run, view.reporting, view.scale)
              )
            }
            .flatMap { found =>
              val indexed = found.map(v => v.scale -> v).toMap
              Either.cond(
                indexed.size == declared.size && found.size == declared.size,
                indexed,
                ProfileError
                  .ReportScales(run, id, declared.indices.toVector, found.map(_.scale))
              )
            }
        for
          _ <- Either.cond(
            served.size == declared.size,
            (),
            ProfileError.ScaleCount(run, declared.size, served.size)
          )
          _ <- declared.zip(served).zipWithIndex.traverse_ { case ((sigma, found), i) =>
            Either.cond(found == sigma, (), ProfileError.ScaleIdentity(run, i, found, sigma))
          }
          groupedAt <- checked(grouped, first.reporting)
          overallAt <- checked(overall, overallId)
          at        <- declared.zipWithIndex.traverse { case (sigma, i) =>
            ScaleIndex.of(i).leftMap(_ => ProfileError.Scale(run, i)).map((_, sigma))
          }
          levels = groupedAt(0).cells.collect {
            case c if c.role == ReportRole.Difference => c.group
          }
          _ <- levels
            .diff(levels.distinct)
            .headOption
            .map(g =>
              ProfileError.DuplicateGroup(
                run,
                g.getOrElse(
                  Response(ParticipantText(eyes4s.studio.app.text.ParticipantTextId.AllQueries))
                )
              )
            )
            .toLeft(())
          people = overallAt(0).participants.collect {
            case p if p.role == ReportRole.Difference && p.group.isEmpty => p.participant
          }
          _ <- people
            .diff(people.distinct)
            .headOption
            .map(ProfileError.DuplicateParticipant(run, _))
            .toLeft(())
          groups <- levels.traverse { group =>
            at.traverse { (index, sigma) =>
              groupedAt(index.value)
                .cell(group, ReportRole.Difference)
                .toRight(
                  ProfileError.MissingSeries(run, first.reporting, index.value, group, None)
                )
                .map(c =>
                  ProfilePoint(
                    c.ref,
                    index,
                    served(index.value).label,
                    sigma,
                    c.estimate
                  ) -> c.participants
                )
            }.map(points =>
              ProfileSeries(
                group.fold(
                  ParticipantText(eyes4s.studio.app.text.ParticipantTextId.AllQueries)
                )(_.label),
                ParticipantText.participants(points.head._2),
                points.map(_._1)
              )
            )
          }
          participants <- people.traverse { person =>
            at.traverse { (index, sigma) =>
              overallAt(index.value)
                .participant(None, ReportRole.Difference, person)
                .toRight(
                  ProfileError.MissingSeries(run, overallId, index.value, None, Some(person))
                )
                .map(p =>
                  ProfilePoint(
                    p.ref,
                    index,
                    served(index.value).label,
                    sigma,
                    p.value
                  ) -> p.queries
                )
            }.map(points =>
              ProfileSeries(person, ParticipantText.queries(points.head._2), points.map(_._1))
            )
          }
        yield ScaleProfile(run, first.reporting, groups, participants)
      }

  /** One query's scale profile (ticket S8.5; Main.dc.html, the contrast
    * group's "Scale profile" tab): its D at each of the run's `scales`, as
    * the run's query row served it, each point its
    * [[StudioRef.QueryContrast]]. `served` are the run's typed scales, one
    * for each declared scale. A query that does not contribute has no D at
    * any scale: every point is missing, never zero.
    */
  def ofQuery(
      run: RunId,
      reporting: ReportingId,
      row: QueryRow,
      served: Vector[Sigma],
      scales: ScaleSet
  ): Either[ProfileError, ScaleProfile] =
    val declared = scales.values
    for
      _ <- Either.cond(
        declared.size == served.size,
        (),
        ProfileError.ScaleCount(run, declared.size, served.size)
      )
      at <- declared.zip(served).zipWithIndex.traverse { case ((sigma, found), i) =>
        for
          _ <- Either.cond(
            found == sigma,
            (),
            ProfileError.ScaleIdentity(run, i, found, sigma)
          )
          index <- ScaleIndex.of(i).leftMap(_ => ProfileError.Scale(run, i))
        yield (index, found.label, sigma)
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

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
import eyes4s.studio.app.text.{ParticipantText, ParticipantTextId}
import eyes4s.studio.core.backend.{ResultSummary, Response, RunId}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** Why a run's participant means could not be read from its summary. Every
  * case names the run and the scale, group or participant that failed.
  */
enum ParticipantMeansError derives CanEqual:

  /** The run has no scale `scale`; its scales are `scales`. */
  case Scale(run: RunId, scale: ScaleIndex, scales: Vector[String])

  /** The summary lists `group` more than once. */
  case DuplicateGroup(run: RunId, group: Response)

  /** The summary lists `participant` more than once. */
  case DuplicateParticipant(run: RunId, participant: String)

  /** `participant` has means in `group`, which is not one of the summary's
    * groups `groups`, or has them twice.
    */
  case UnknownGroup(run: RunId, participant: String, group: Response, groups: Vector[Response])

  /** The summary's grand mean `d` of `group` is not its grand mean `atScale`
    * at `scale`: the summary's means are at another scale.
    */
  case OtherScale(run: RunId, group: Response, scale: ScaleIndex, d: Double, atScale: Double)

  def message: String = this match
    case Scale(r, s, scales) =>
      s"Participant means of run ${r.number}: no scale ${s.value} among ${scales.mkString(", ")}."
    case DuplicateGroup(r, g) =>
      s"Participant means of run ${r.number}: group ${g.label} is listed twice."
    case DuplicateParticipant(r, p) =>
      s"Participant means of run ${r.number}: participant $p is listed twice."
    case UnknownGroup(r, p, g, gs) =>
      s"Participant means of run ${r.number}: $p has means in ${g.label}, which is not " +
        s"once among the groups ${gs.map(_.label).mkString(", ")}."
    case OtherScale(r, g, s, d, at) =>
      s"Participant means of run ${r.number}: the grand mean of ${g.label} is $d, but $at " +
        s"at scale ${s.value}."

/** One participant's mean D in one group, as the summary serves it: `d` is
  * missing when the participant has no queries in the group (the backend
  * lists no means for it, or means of `n` 0, which are not a value); `n` is
  * the number of queries the mean is over, missing when none is listed.
  */
final case class ParticipantCell(
    ref: StudioRef,
    group: Response,
    participant: String,
    d: Option[Double],
    n: Option[Int]
) derives CanEqual

/** A group's grand mean D over its participants' means, and its `n`
  * participants.
  */
final case class GroupGrandMean(ref: StudioRef, group: Response, d: Double, n: Int)
    derives CanEqual

/** A run's participant means under one reporting spec at one scale (ticket
  * S4.5c): each group's grand mean and every participant's mean in every
  * group, every value as the summary served it.
  */
final case class ParticipantMeans(
    run: RunId,
    reporting: ReportingId,
    scale: ScaleIndex,
    scaleLabel: String,
    groups: Vector[GroupGrandMean],
    cells: Vector[ParticipantCell]
) derives CanEqual

/** The columns of the participant plot's value source. */
final case class ParticipantColumns(group: ColumnId, meanOf: ColumnId, d: ColumnId, n: ColumnId)
    derives CanEqual

object ParticipantColumns:
  val standard: Either[PlotSourceError, ParticipantColumns] =
    for
      group  <- ColumnId.of("group")
      meanOf <- ColumnId.of("mean-of")
      d      <- ColumnId.of("d")
      n      <- ColumnId.of("n")
    yield ParticipantColumns(group, meanOf, d, n)

object ParticipantMeans:

  /** The participant means of `summary` under `reporting`, at `scale`, the
    * scale the summary's means are at: the summary's grand mean of every
    * group must be its grand mean at that scale. Groups are in the
    * summary's order and so are participants; every participant has a cell
    * in every group. Nothing is computed here.
    */
  def of(
      summary: ResultSummary,
      reporting: ReportingId,
      scale: ScaleIndex
  ): Either[ParticipantMeansError, ParticipantMeans] =
    val run    = summary.run
    val labels = summary.groups.map(_.label)
    for
      scaleLabel <- summary.scales
        .lift(scale.value)
        .toRight(ParticipantMeansError.Scale(run, scale, summary.scales))
      _ <- labels
        .diff(labels.distinct)
        .headOption
        .map(ParticipantMeansError.DuplicateGroup(run, _))
        .toLeft(())
      ids = summary.participants.map(_.participant)
      _ <- ids
        .diff(ids.distinct)
        .headOption
        .map(ParticipantMeansError.DuplicateParticipant(run, _))
        .toLeft(())
      _ <- summary.participants
        .flatMap(p => p.groups.map(_.label).diff(labels).map(p.participant -> _))
        .headOption
        .map((p, g) => ParticipantMeansError.UnknownGroup(run, p, g, labels))
        .toLeft(())
      groups <- summary.groups.traverse { g =>
        g.dByScale
          .lift(scale.value)
          .toRight(ParticipantMeansError.Scale(run, scale, summary.scales))
          .flatMap { at =>
            Either.cond(
              at == g.d,
              GroupGrandMean(
                StudioRef.GroupCell(run, reporting, scale, g.label),
                g.label,
                g.d,
                g.n
              ),
              ParticipantMeansError.OtherScale(run, g.label, scale, g.d, at)
            )
          }
      }
    yield
      val cells = labels.flatMap { label =>
        summary.participants.map { p =>
          val means = p.groups.find(_.label == label)
          ParticipantCell(
            StudioRef.ParticipantSummary(run, reporting, scale, Some(label), p.participant),
            label,
            p.participant,
            means.filter(_.n > 0).map(_.d),
            means.map(_.n)
          )
        }
      }
      ParticipantMeans(run, reporting, scale, scaleLabel, groups, cells)

  /** The means as a value source: for each group, in order, every
    * participant's mean and then the group's grand mean, each a row with its
    * own ref. A mean the summary does not hold is missing, never zero; the
    * grand mean's n counts participants, a participant's n queries.
    */
  def source(
      means: ParticipantMeans,
      columns: ParticipantColumns
  ): Either[PlotSourceError, PlotSource] =
    def text(s: String)   = PlotValue.Text(s)
    def number(v: Double) = PlotValue.Number(v)
    val rows              = means.groups.flatMap { g =>
      means.cells
        .filter(_.group == g.group)
        .map(c =>
          PlotRow(
            c.ref,
            Vector(
              text(c.group.label),
              text(c.participant),
              c.d.fold(PlotValue.Missing)(number),
              c.n.fold(PlotValue.Missing)(n => number(n.toDouble))
            )
          )
        ) :+ PlotRow(
        g.ref,
        Vector(
          text(g.group.label),
          text(ParticipantText(ParticipantTextId.AllParticipants)),
          number(g.d),
          number(g.n.toDouble)
        )
      )
    }
    PlotSource(
      ParticipantText(ParticipantTextId.Caption, means.scaleLabel),
      Vector(
        PlotColumn(
          columns.group,
          ParticipantText(ParticipantTextId.GroupHeader),
          ColumnFormat.Label
        ),
        PlotColumn(
          columns.meanOf,
          ParticipantText(ParticipantTextId.MeanOfHeader),
          ColumnFormat.Label
        ),
        PlotColumn(
          columns.d,
          ParticipantText(ParticipantTextId.DHeader),
          ColumnFormat.Signed(2)
        ),
        PlotColumn(columns.n, ParticipantText(ParticipantTextId.NHeader), ColumnFormat.Count)
      ),
      rows
    )

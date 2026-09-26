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

package eyes4s.results

import eyes4s.plan.*

/** A reference to one of a report's levels: a cell of the summary (a
  * group's estimate for one role and component at the report's scale), or
  * one participant within a cell. References are checked when they are
  * built; [[ReportNavigation]] checks them against a report.
  */
sealed trait ReportRef derives CanEqual

object ReportRef:
  final case class Cell private[results] (
      scale: Int,
      group: GroupKey,
      role: Role,
      component: String
  ) extends ReportRef

  /** One participant of a cell. Its parent is `cell`. */
  final case class Participant private[results] (cell: Cell, participant: String)
      extends ReportRef

  def cell(
      scale: Int,
      group: GroupKey,
      role: Role,
      component: String
  ): Either[ReportNavigationError[Nothing], Cell] =
    if scale < 0 then Left(ReportNavigationError.NegativeScale(scale))
    else if component.trim.isEmpty then Left(ReportNavigationError.BlankComponent(component))
    else Right(new Cell(scale, group, role, component))

  def participant(
      cell: Cell,
      participant: String
  ): Either[ReportNavigationError[Nothing], Participant] =
    Either.cond(
      participant.trim.nonEmpty,
      new Participant(cell, participant),
      ReportNavigationError.BlankParticipant(participant)
    )

/** Why a report navigation step was refused. Every case names its operands. */
enum ReportNavigationError[+K] derives CanEqual:
  case NegativeScale(scale: Int)
  case BlankComponent(component: String)
  case BlankParticipant(participant: String)

  /** The reference is at another scale than the report. */
  case ScaleMismatch(ref: Int, report: Int)

  /** The report has no cell for this group, role and component. */
  case UnknownCell(group: GroupKey, role: Role, component: String)

  /** The participant has neither a value nor a query in the cell. */
  case NotInCell(participant: String, group: GroupKey, role: Role, component: String)

  /** The reference is not at the level the step starts from. */
  case WrongLevel(ref: ResultRef[K], expected: NavigationLevel)

  /** The query trial is not among the cell's members. */
  case NotAMember(key: K, group: GroupKey, role: Role, component: String)

  def message: String = this match
    case NegativeScale(scale)          => s"A report cell's scale is 0 or more, not $scale."
    case BlankComponent(component)     => s"A report cell names a component, not '$component'."
    case BlankParticipant(participant) => s"A participant is named, not '$participant'."
    case ScaleMismatch(ref, report)    =>
      s"The reference is at scale $ref; the report is at scale $report."
    case UnknownCell(group, role, component) =>
      s"The report has no cell for ${group.render}, $role, $component."
    case NotInCell(participant, group, role, component) =>
      s"Participant $participant has no value or query in the cell ${group.render}, $role, $component."
    case WrongLevel(ref, expected) => s"$ref is not a ${expected.toString} reference."
    case NotAMember(key, group, role, component) =>
      s"Query trial $key is not a member of the cell ${group.render}, $role, $component."

/** The top of the provenance chain: a report's summary cells, a cell's
  * participants, and a participant's query contrasts; and back up. Below the
  * query contrast, `ResultNavigation` (`eyes4s-plan`) continues to pairs,
  * maps, fixations and source records. A trial's participant is read through
  * the study layout, the one projection the plan and the report share.
  */
object ReportNavigation:
  private def cellOf[K](
      report: Report[K],
      ref: ReportRef.Cell
  ): Either[ReportNavigationError[K], Cell[K]] =
    if ref.scale != report.spec.scale then
      Left(ReportNavigationError.ScaleMismatch(ref.scale, report.spec.scale))
    else
      report
        .cell(ref.group, ref.role, ref.component)
        .toRight(ReportNavigationError.UnknownCell(ref.group, ref.role, ref.component))

  private def keyOf[K](ref: ResultRef[K]): Option[K] = ref match
    case ResultRef.ContrastRow(_, key)  => Some(key)
    case ResultRef.Reduction(_, _, key) => Some(key)
    case ResultRef.InCell(_, _, inner)  => keyOf(inner)
    case _                              => None

  /** The summary level: one reference per cell, in the report's order. */
  def cells[K](report: Report[K]): Vector[ReportRef.Cell] =
    report.cells.map(c => new ReportRef.Cell(report.spec.scale, c.group, c.role, c.component))

  /** The participants whose values the cell averages, in its order. */
  def participants[K](
      report: Report[K],
      cell: ReportRef.Cell
  ): Either[ReportNavigationError[K], Vector[ReportRef.Participant]] =
    cellOf(report, cell).map(
      _.perParticipant.map(v => new ReportRef.Participant(cell, v.participant))
    )

  /** A participant's query contrasts in the cell: the contrast row of each
    * of the cell's member queries that belongs to the participant, in member
    * order, once each.
    */
  def queries[K](
      report: Report[K],
      participant: ReportRef.Participant,
      layout: StudyLayout[K]
  ): Either[ReportNavigationError[K], Vector[ResultRef[K]]] =
    val ref = participant.cell
    cellOf(report, ref).flatMap { cell =>
      val keys = cell.members
        .flatMap(keyOf)
        .distinct
        .filter(k => layout.participant(k) == participant.participant)
      val listed = cell.perParticipant.exists(_.participant == participant.participant)
      Either.cond(
        keys.nonEmpty || listed,
        keys.map(ResultRef.ContrastRow(report.spec.scale, _)),
        ReportNavigationError.NotInCell(
          participant.participant,
          ref.group,
          ref.role,
          ref.component
        )
      )
    }

  /** The participant of a cell that a query contrast belongs to. */
  def participantOf[K](
      report: Report[K],
      cell: ReportRef.Cell,
      contrast: ResultRef[K],
      layout: StudyLayout[K]
  ): Either[ReportNavigationError[K], ReportRef.Participant] =
    contrast match
      case ResultRef.ContrastRow(scale, key) =>
        for
          found <- cellOf(report, cell)
          _     <- Either.cond(
            scale == report.spec.scale,
            (),
            ReportNavigationError.ScaleMismatch(scale, report.spec.scale)
          )
          _ <- Either.cond(
            found.members.flatMap(keyOf).contains(key),
            (),
            ReportNavigationError.NotAMember(key, cell.group, cell.role, cell.component)
          )
        yield new ReportRef.Participant(cell, layout.participant(key))
      case other =>
        Left(ReportNavigationError.WrongLevel(other, NavigationLevel.QueryContrast))

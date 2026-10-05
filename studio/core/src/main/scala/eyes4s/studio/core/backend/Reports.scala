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

package eyes4s.studio.core.backend

import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.selection.{ReportCount, StudioRef}
import io.circe.Codec

/** A role of a report's cells (eyes4s `Role`): the matched reduction, the
  * control reduction, or their difference D = M − B.
  */
enum ReportRole derives CanEqual, Codec.AsObject:
  case Matched, Control, Difference

/** Why a value an eyes4s quantity leaves undefined is (eyes4s
  * `UndefinedReason`).
  */
enum ReportUndefined derives CanEqual, Codec.AsObject:
  /** A spread needs at least two values; `n` were available. */
  case TooFewForSpread(n: Int)

  /** A duration share of a trial whose fixations have no duration. */
  case ZeroDuration

  /** The arithmetic left the finite doubles: `operation` over `n` values. */
  case NotFinite(operation: String, n: Int)

/** Why a report value is missing (eyes4s `Absence`), never a zero. */
enum ReportAbsence derives CanEqual, Codec.AsObject:
  /** The source recorded nothing. */
  case NotRecorded

  /** A covariate cell did not parse as its declared type. */
  case Unparsed

  /** The stored result failed; `code` is its diagnostic code (`family.name`). */
  case Failed(code: String, message: String)

  /** Nothing failed, but the quantity is not defined. */
  case Undefined(reason: ReportUndefined)

  /** The group has no query for this role. */
  case EmptyGroup

  /** A within-participant contrast has the participant in one level only. */
  case Unpaired

  /** The participant has `queries` queries in the group, fewer than `required`. */
  case BelowMinimum(queries: Int, required: Int)

/** One cell of a report (eyes4s `Cell`): a group's estimate for one role,
  * missing with its absence rather than zero, with the participants and
  * queries behind it.
  */
final case class ReportCellView(
    group: Option[Response],
    role: ReportRole,
    estimate: Option[Double],
    absence: Option[ReportAbsence],
    participants: Int,
    queries: Int,
    failed: Int,
    ref: StudioRef
) derives CanEqual,
      Codec.AsObject

/** One participant's value in a group's cell for one role (eyes4s
  * `ParticipantValue`): the mean of its queries there, or why it has none.
  */
final case class ReportParticipantView(
    group: Option[Response],
    role: ReportRole,
    participant: String,
    queries: Int,
    value: Option[Double],
    absence: Option[ReportAbsence],
    ref: StudioRef
) derives CanEqual,
      Codec.AsObject

/** A participant in one level of the contrast only (eyes4s
  * `UnpairedParticipant`).
  */
final case class ReportUnpairedView(participant: String, present: Response, missing: Response)
    derives CanEqual,
      Codec.AsObject

/** The within-participant level contrast of a report for one role (eyes4s
  * `LevelContrastStat`): `minuend` minus `subtrahend`, the mean over paired
  * participants of their differences, or why it has none; `pairedN`, the
  * paired participants it averages (eyes4s's n); and the unpaired ones.
  */
final case class ReportContrastView(
    role: ReportRole,
    minuend: Response,
    subtrahend: Response,
    estimate: Option[Double],
    absence: Option[ReportAbsence],
    pairedN: Int,
    unpaired: Vector[ReportUnpairedView],
    ref: StudioRef
) derives CanEqual,
      Codec.AsObject

/** The fewest to the most queries a participant has in a group for one role
  * (eyes4s `ReportFacts`' `GroupSizeRange`).
  */
final case class ReportQueryRange(role: ReportRole, fewest: Int, most: Int, ref: StudioRef)
    derives CanEqual,
      Codec.AsObject

/** A participant left out of a group because it has fewer queries there than
  * the spec's minimum (eyes4s `ReportFinding.BelowMinimum`): the dropped
  * cell a reporting edit previews.
  */
final case class DroppedCell(
    group: Option[Response],
    participant: String,
    queries: Int,
    required: Int,
    ref: StudioRef
) derives CanEqual,
      Codec.AsObject

/** One count of where a role's eligible queries went (eyes4s `Accounting`),
  * or eyes4s's count of the queries the outside-window filter alone left
  * out or could not decide.
  */
final case class ReportTallyView(
    role: ReportRole,
    count: ReportCount,
    value: Int,
    ref: StudioRef
) derives CanEqual,
      Codec.AsObject

/** A reporting spec evaluated over a run at one scale by eyes4s-results
  * (`Report.evaluate`, UI-C; protocol 1.11): its cells, each participant's
  * value for every role, the level contrast when the spec has one, the
  * participants a minimum drops, the queries-per-participant range, and the
  * accounting, each with the [[StudioRef]] that names it.
  */
final case class ReportView(
    run: RunId,
    reporting: ReportingId,
    scale: Int,
    cells: Vector[ReportCellView],
    participants: Vector[ReportParticipantView],
    contrasts: Vector[ReportContrastView],
    dropped: Vector[DroppedCell],
    queryRanges: Vector[ReportQueryRange],
    tallies: Vector[ReportTallyView]
) derives CanEqual,
      Codec.AsObject:
  def cell(group: Option[Response], role: ReportRole): Option[ReportCellView] =
    cells.find(c => c.group == group && c.role == role)

  def participant(
      group: Option[Response],
      role: ReportRole,
      participant: String
  ): Option[ReportParticipantView] =
    participants.find(p => p.group == group && p.role == role && p.participant == participant)

  def contrast(role: ReportRole): Option[ReportContrastView] = contrasts.find(_.role == role)

  def queryRange(role: ReportRole): Option[ReportQueryRange] = queryRanges.find(_.role == role)

  def tally(role: ReportRole, count: ReportCount): Option[Int] =
    tallies.find(t => t.role == role && t.count == count).map(_.value)

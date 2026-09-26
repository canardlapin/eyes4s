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

package eyes4s.studio.core.navigation

import cats.arrow.FunctionK
import eyes4s.plan.MissingSource as CoreMissingSource
import eyes4s.studio.core.backend.{
  BackendError,
  PageInfo,
  PageRequest,
  PairDesign,
  QueryStatus,
  ResultAddress,
  Response,
  RunId,
  TrialKey
}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** A report step of the provenance chain (UI-G's planned `ReportRef` in
  * eyes4s-results, mirrored until G3 lands; S3.7 swaps the fake down
  * functions for the real ones).
  */
sealed trait ReportRef derives CanEqual:
  /** The same step as a [[StudioRef]], when one exists: a whole-report cell
    * (no grouping) has none; its place is the summary itself.
    */
  def ref: Option[StudioRef]

object ReportRef:

  /** One cell of a reporting spec's summary at one scale of a run: a group,
    * or the whole report when the spec does not group.
    */
  final case class Cell(
      run: RunId,
      reporting: ReportingId,
      scale: ScaleIndex,
      group: Option[Response]
  ) extends ReportRef:
    def ref: Option[StudioRef] = group.map(StudioRef.GroupCell(run, reporting, scale, _))

  /** One participant's mean within a cell. */
  final case class Participant(cell: Cell, participant: String) extends ReportRef:
    def ref: Option[StudioRef] = Some(summary)

    def summary: StudioRef =
      StudioRef.ParticipantSummary(
        cell.run,
        cell.reporting,
        cell.scale,
        cell.group,
        participant
      )

  /** The report step a studio ref names, if it names one. */
  def of(ref: StudioRef): Option[ReportRef] = ref match
    case StudioRef.GroupCell(run, reporting, scale, group) =>
      Some(Cell(run, reporting, scale, Some(group)))
    case StudioRef.ParticipantSummary(run, reporting, scale, group, participant) =>
      Some(Participant(Cell(run, reporting, scale, group), participant))
    case _ => None

/** Why a fixation has no source record: eyes4s `MissingSource` over
  * [[TrialKey]], the cases a studio view can meet. Fixation positions count
  * from 1 here, as studio shows them ("fixation 6").
  */
enum MissingSource derives CanEqual:
  /** The input was not linked to an admission ledger. */
  case NoLedger

  /** The ledger names no record for this trial. */
  case UnknownTrial(key: TrialKey)

  /** The trial has fewer fixations than the position asked for. */
  case FixationOutOfRange(key: TrialKey, fixation: Int, fixations: Int)

  /** The ledger admitted no scanpath for this trial. */
  case NotAdmitted(key: TrialKey)

  /** Any other eyes4s reason, by its case name and rendering. */
  case Other(reason: String, text: String)

  def message: String = this match
    case NoLedger                          => "The input is not linked to an admission ledger."
    case UnknownTrial(key)                 => s"The ledger names no record for ${key.label}."
    case FixationOutOfRange(key, i, count) =>
      s"${key.label} has $count fixations; there is no fixation $i."
    case NotAdmitted(key)    => s"${key.label} was not admitted, so it has no fixations."
    case Other(reason, text) => s"$reason: $text"

object MissingSource:
  /** eyes4s' reason with its keys mapped; its 0-based index becomes a 1-based
    * fixation position.
    */
  def of[K](missing: CoreMissingSource[K], key: K => TrialKey): MissingSource = missing match
    case CoreMissingSource.NoLedger                    => NoLedger
    case CoreMissingSource.UnknownTrial(k)             => UnknownTrial(key(k))
    case CoreMissingSource.FixationOutOfRange(k, i, n) => FixationOutOfRange(key(k), i + 1, n)
    case CoreMissingSource.NotAdmitted(k, _)           => NotAdmitted(key(k))
    case other => Other(other.productPrefix, other.mapKeys(key).toString)

/** The step of the provenance chain a ref sits at (DESIGN_SPEC section 3's
  * trail grammar), coarsest first.
  */
enum ChainLevel derives CanEqual:
  case Summary, Group, Participant, Query, Pair, Map, Fixation, Record

/** Why a navigation step was refused. Every case names its operands. */
enum NavigationError derives CanEqual:
  /** The backend refused the lookup behind the step. */
  case Backend(error: BackendError)

  /** `ref` is not at the level the step walks down from. */
  case WrongLevel(ref: StudioRef, expected: ChainLevel)

  /** The run's summary has no cell for `group`; it has `known`. */
  case UnknownGroup(cell: ReportRef.Cell, known: Vector[Response])

  /** `participant` has no mean in `cell`. */
  case NotInCell(participant: String, cell: ReportRef.Cell)

  /** The query has no contrast row: it was not scored, and `status` says why. */
  case NoContrast(query: StudioRef, status: QueryStatus)

  /** The pairs of `contrast` under `design` are not held by this backend. */
  case NoPairs(contrast: StudioRef, design: PairDesign)

  /** `map` is not one of `pair`'s two maps. */
  case NotInPair(map: StudioRef, pair: StudioRef)

  /** The fixation's source record is missing, for `reason`. */
  case Source(subject: StudioRef, reason: MissingSource)

  def message: String = this match
    case Backend(error)          => error.message
    case WrongLevel(ref, level)  => s"$ref is not a ${level.productPrefix.toLowerCase} step."
    case UnknownGroup(cell, all) =>
      s"The summary of ${cell.run.label} has no group ${cell.group.fold("(whole report)")(_.label)}; " +
        s"it has ${all.map(_.label).mkString(", ")}."
    case NotInCell(p, cell) =>
      s"$p has no mean in ${cell.group.fold("the whole report")(_.label)} of ${cell.run.label}."
    case NoContrast(query, status) => s"$query has no contrast row (${status.productPrefix})."
    case NoPairs(contrast, design) =>
      s"The ${design.render} pairs of $contrast are not held by this backend."
    case NotInPair(map, pair)    => s"$map is not a map of $pair."
    case Source(subject, reason) => s"No source record for $subject: ${reason.message}"

/** One offset page of a step's listing (UI-G amendment A3): its entries,
  * where it starts, the listing's total (known before any page is
  * materialised) and where the next page starts, if any.
  */
final case class Page[+A](entries: Vector[A], offset: Int, total: Int, next: Option[Int])
    derives CanEqual:
  def map[B](f: A => B): Page[B] = Page(entries.map(f), offset, total, next)

object Page:
  /** The page `request` asks for of `all`. */
  def of[A](all: Vector[A], request: PageRequest): Page[A] =
    val entries = all.slice(request.offset, request.offset + request.size)
    val info    = PageInfo.of(request, all.size, entries.size)
    Page(entries, info.offset, info.total, info.next)

/** How a trial's map enters the pairs of a run at one scale (UI-G's
  * `usedBy` reverse index; amendment A1).
  */
enum UsedByRole derives CanEqual:
  /** As the query of its own matched and control pairs. */
  case AsQuery

  /** As the matched reference of another query. */
  case AsMatched

  /** As a control reference of other queries. */
  case AsControl

/** The pairs a trial's map is used by, counted without listing them. */
final case class UsedBy(asQuery: Int, asMatched: Int, asControl: Int) derives CanEqual:
  def count(role: UsedByRole): Int = role match
    case UsedByRole.AsQuery   => asQuery
    case UsedByRole.AsMatched => asMatched
    case UsedByRole.AsControl => asControl

  def total: Int = asQuery + asMatched + asControl

/** A pair's two maps: the query's and the reference's (both `Estimation`
  * refs of the pair's run and scale).
  */
final case class PairMaps(query: StudioRef, reference: StudioRef) derives CanEqual:
  def both: Vector[StudioRef] = Vector(query, reference)

/** The down functions of the provenance chain (ticket S3.4): summary →
  * group → participant → query contrast → pair → map → fixation → record.
  * Each step is a value: its children, or a [[NavigationError]] naming what
  * refused. Up is [[StudioRef]]'s structure plus the trail's context (studio
  * app's `Provenance`).
  *
  * The shapes mirror UI-G's planned `ReportNavigation`, `ResultInspection`
  * and `StudySources` steps. `FakeStudyBackend` serves the fixture now; the
  * real backend implements this over eyes4s in S3.7.
  */
trait StudyNavigator[F[_]]:

  /** The cells of a reporting spec's summary at one scale of a run. */
  def cells(
      run: RunId,
      reporting: ReportingId,
      scale: ScaleIndex,
      page: PageRequest
  ): F[Either[NavigationError, Page[ReportRef.Cell]]]

  /** Every participant with a mean in `cell`, in study order. */
  def participants(
      cell: ReportRef.Cell,
      page: PageRequest
  ): F[Either[NavigationError, Page[ReportRef.Participant]]]

  /** The participant's query contrasts in the cell (`QueryContrast` refs), in
    * source order; only scored queries have one.
    */
  def queries(
      participant: ReportRef.Participant,
      page: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]]

  /** A query contrast's pairs under `design` (`Pair` refs). */
  def pairs(
      contrast: StudioRef,
      design: PairDesign,
      page: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]]

  /** A pair's two maps. */
  def maps(pair: StudioRef): F[Either[NavigationError, PairMaps]]

  /** A map's fixations (`Fixation` refs), in scanpath order. */
  def fixations(map: StudioRef, page: PageRequest): F[Either[NavigationError, Page[StudioRef]]]

  /** A fixation's source record (a `SourceRecord` ref), or
    * [[NavigationError.Source]] with the typed [[MissingSource]].
    */
  def record(fixation: StudioRef): F[Either[NavigationError, StudioRef]]

  /** How many pairs of the map's run and scale use its trial, by role,
    * without listing them (UI-G A1: `UsedBy.counts`).
    */
  def usedByCounts(map: StudioRef): F[Either[NavigationError, UsedBy]]

  /** The pairs (`Pair` refs) that use the map's trial in `role`. Every pair
    * listed here is also reachable from its query's contrast.
    */
  def usedBy(
      map: StudioRef,
      role: UsedByRole,
      page: PageRequest
  ): F[Either[NavigationError, Page[StudioRef]]]

object StudyNavigator:

  /** The same navigator in another effect. */
  def mapK[F[_], G[_]](navigator: StudyNavigator[F])(fk: FunctionK[F, G]): StudyNavigator[G] =
    new StudyNavigator[G]:
      def cells(run: RunId, reporting: ReportingId, scale: ScaleIndex, page: PageRequest) =
        fk(navigator.cells(run, reporting, scale, page))
      def participants(cell: ReportRef.Cell, page: PageRequest) =
        fk(navigator.participants(cell, page))
      def queries(participant: ReportRef.Participant, page: PageRequest) =
        fk(navigator.queries(participant, page))
      def pairs(contrast: StudioRef, design: PairDesign, page: PageRequest) =
        fk(navigator.pairs(contrast, design, page))
      def maps(pair: StudioRef)                        = fk(navigator.maps(pair))
      def fixations(map: StudioRef, page: PageRequest) = fk(navigator.fixations(map, page))
      def record(fixation: StudioRef)                  = fk(navigator.record(fixation))
      def usedByCounts(map: StudioRef)                 = fk(navigator.usedByCounts(map))
      def usedBy(map: StudioRef, role: UsedByRole, page: PageRequest) =
        fk(navigator.usedBy(map, role, page))

  /** The level of a studio ref in the chain, if it is a chain step. */
  def level(ref: StudioRef): Option[ChainLevel] = ref match
    case StudioRef.GroupCell(_, _, _, _)             => Some(ChainLevel.Group)
    case StudioRef.ParticipantSummary(_, _, _, _, _) => Some(ChainLevel.Participant)
    case StudioRef.Fixation(_, _)                    => Some(ChainLevel.Fixation)
    case StudioRef.SourceRecord(_, _, _, _)          => Some(ChainLevel.Record)
    case StudioRef.Result(_, address)                =>
      address.value match
        case ResultAddress.ContrastRow(_, _)   => Some(ChainLevel.Query)
        case ResultAddress.PairRow(_, _, _, _) => Some(ChainLevel.Pair)
        case ResultAddress.Estimation(_, _)    => Some(ChainLevel.Map)
        case ResultAddress.Reduction(_, _, _)  => None
    case _ => None

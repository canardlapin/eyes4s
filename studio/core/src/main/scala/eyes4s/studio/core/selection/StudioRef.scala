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

package eyes4s.studio.core.selection

import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  PairDesign,
  Phase,
  ResultAddress,
  Response,
  RunId,
  TrialKey
}
import eyes4s.studio.core.assets.DisplayKind
import eyes4s.studio.core.document.{FigureId, PanelLetter, ReportingId, SourceRole}
import io.circe.{Codec, Decoder, Encoder}

/** Why a ref component was refused; every case names its operand. */
enum RefError derives CanEqual:
  case NegativeScale(value: Int)
  case FixationIndexNotPositive(value: Int)
  case RecordNotPositive(value: Int)

  def message: String = this match
    case NegativeScale(v)            => s"Scale index $v is negative."
    case FixationIndexNotPositive(v) =>
      s"Fixation index $v is not positive (fixations count from 1)."
    case RecordNotPositive(v) => s"Record number $v is not positive (records count from 1)."

private object RefCodecs:
  def validated[A](of: Int => Either[RefError, A], unwrap: A => Int): Codec[A] =
    Codec.from(Decoder[Int].emap(of(_).left.map(_.message)), Encoder[Int].contramap(unwrap))

/** A result's scale index, as in eyes4s `ResultRef`: 0 or more. */
final case class ScaleIndex private[selection] (value: Int) derives CanEqual

object ScaleIndex:
  def of(value: Int): Either[RefError, ScaleIndex] =
    Either.cond(value >= 0, new ScaleIndex(value), RefError.NegativeScale(value))

  given Codec[ScaleIndex] = RefCodecs.validated(of, _.value)

/** A fixation's position within its trial, from 1 ("fixation 6"). */
final case class FixationIndex private (value: Int) derives CanEqual

object FixationIndex:
  def of(value: Int): Either[RefError, FixationIndex] =
    Either.cond(value >= 1, new FixationIndex(value), RefError.FixationIndexNotPositive(value))

  given Codec[FixationIndex] = RefCodecs.validated(of, _.value)

/** A record of a source file, from 1 ("fixations.csv record 7,214"). */
final case class RecordNumber private (value: Int) derives CanEqual

object RecordNumber:
  def of(value: Int): Either[RefError, RecordNumber] =
    Either.cond(value >= 1, new RecordNumber(value), RefError.RecordNotPositive(value))

  given Codec[RecordNumber] = RefCodecs.validated(of, _.value)

/** A backend [[ResultAddress]] (eyes4s `ResultRef`) whose scale index has been
  * checked.
  */
final case class CheckedAddress private (value: ResultAddress) derives CanEqual:
  def scale: ScaleIndex = new ScaleIndex(value.scale)

object CheckedAddress:
  def of(address: ResultAddress): Either[RefError, CheckedAddress] =
    ScaleIndex.of(address.scale).map(_ => new CheckedAddress(address))

  private[selection] def trusted(address: ResultAddress): CheckedAddress =
    new CheckedAddress(address)

  given Codec[CheckedAddress] = Codec.from(
    Decoder[ResultAddress].emap(of(_).left.map(_.message)),
    Encoder[ResultAddress].contramap(_.value)
  )

/** What a ref denotes (ticket S3.3). Selecting an aggregate never selects the
  * observations it summarizes, and selecting an observation never selects
  * the aggregates it contributes to.
  */
enum RefKind derives CanEqual:
  /** A unit of the study (a participant), neither measured nor derived. */
  case Entity

  /** A measured or row-level value: a trial, fixation, source record, a
    * trial's map or a pair score.
    */
  case Observation

  /** A value computed over observations: a reduction, a query contrast, a
    * participant summary, a group cell, a figure panel.
    */
  case Aggregate

/** A typed reference to anything a studio view can show, select or trace a
  * number to (ticket S3.3; DESIGN_SPEC section 3's trail grammar).
  *
  * Every run result is [[StudioRef.Result]]: a run and the eyes4s
  * `ResultRef` address the backend already mirrors, so UI-G extends
  * [[ResultAddress]] without changing this type. [[StudioRef.Pair]] and
  * [[StudioRef.QueryContrast]] are constructors and extractors of `Result`.
  * Reporting refs name the reporting spec that defines them (UI-C).
  */
enum StudioRef derives CanEqual, Codec.AsObject:
  case Participant(participant: String)
  case Trial(key: TrialKey)
  case Fixation(trial: TrialKey, index: FixationIndex)

  /** One record of a source file. A fixation record names its fixation; a
    * trial-inventory record names none.
    */
  case SourceRecord(
      trial: TrialKey,
      fixation: Option[FixationIndex],
      source: SourceRole,
      record: RecordNumber
  )

  /** One item of a run's result, by its eyes4s address. */
  case Result(run: RunId, address: CheckedAddress)

  /** One participant's summary under a reporting spec, within a group when
    * the spec groups.
    */
  case ParticipantSummary(
      run: RunId,
      reporting: ReportingId,
      scale: ScaleIndex,
      group: Option[Response],
      participant: String
  )

  /** One group's cell of a reporting spec's summary. */
  case GroupCell(run: RunId, reporting: ReportingId, scale: ScaleIndex, group: Response)

  case FigurePanel(figure: FigureId, panel: PanelLetter)

  /** A dataset revision's count of fixation records in admitted trials that
    * fall outside a frame (eyes4s `WindowSummary`; ticket S5.5).
    */
  case WindowTally(dataset: DatasetRevision, region: TallyRegion)

  /** One count of an analysis revision's resolved design, as the backend's
    * preview reports it (ticket S7.5).
    */
  case DesignTally(revision: AnalysisRevision, tally: DesignCount)

  /** A dataset revision's count of inventory trials with one admission
    * disposition (eyes4s `TrialDisposition`; ticket S5.6): the trials the
    * admission ledger lists under it.
    */
  case InventoryCount(dataset: DatasetRevision, count: InventoryKind)

  /** A group of a dataset revision's trials as the trials navigator lists
    * them (ticket S6.1): one participant's phase, or the trials matched on
    * one item. Its figures count the backend ledger's entries in it.
    */
  case TrialGroup(dataset: DatasetRevision, group: TrialGrouping)

  /** A dataset revision's count of trial displays or asset files, as its
    * asset registry states them (ticket S5.7).
    */
  case DisplayTally(dataset: DatasetRevision, tally: DisplayCount)

  /** One count of a run's query contrasts (eyes4s `QueryContrasts`; ticket
    * S8.1): the queries the run requested, or those with one outcome.
    */
  case QueryTally(run: RunId, tally: QueryCount)

  def kind: RefKind = this match
    case Participant(_)                                       => RefKind.Entity
    case Trial(_) | Fixation(_, _) | SourceRecord(_, _, _, _) => RefKind.Observation
    case Result(_, address)                                   =>
      address.value match
        case ResultAddress.Estimation(_, _) | ResultAddress.PairRow(_, _, _, _) =>
          RefKind.Observation
        case ResultAddress.Reduction(_, _, _) | ResultAddress.ContrastRow(_, _) =>
          RefKind.Aggregate
    case ParticipantSummary(_, _, _, _, _) | GroupCell(_, _, _, _) | FigurePanel(_, _) |
        WindowTally(_, _) | DesignTally(_, _) | InventoryCount(_, _) | TrialGroup(_, _) |
        DisplayTally(_, _) | QueryTally(_, _) =>
      RefKind.Aggregate

  def isAggregate: Boolean = kind == RefKind.Aggregate

  /** The eyes4s result address, for run results. */
  def resultAddress: Option[ResultAddress] = this match
    case Result(_, address) => Some(address.value)
    case _                  => None

  /** The structural container of this ref, derivable from the ref alone:
    *
    *  - participant ⊃ trial ⊃ fixation ⊃ fixation record, and trial ⊃
    *    inventory record;
    *  - query contrast ⊃ its reductions ⊃ their pairs (same run and scale).
    *  - group cell ⊃ participant summary (same run, spec and scale);
    *  - quarantined trials ⊃ the trials of each quarantine cause and the
    *    no-fixations trials (same dataset);
    *  - participant ⊃ the trials of one of its phases.
    *
    * Which group a query belongs to depends on the data; a view that knows it
    * supplies it through [[Lineage]].
    */
  def parent: Option[StudioRef] = this match
    case Participant(_)                            => None
    case Trial(key)                                => Some(Participant(key.participant))
    case Fixation(trial, _)                        => Some(Trial(trial))
    case SourceRecord(trial, Some(fixation), _, _) => Some(Fixation(trial, fixation))
    case SourceRecord(trial, None, _, _)           => Some(Trial(trial))
    case Result(run, address)                      =>
      val within = address.value match
        case ResultAddress.PairRow(s, design, focal, _) =>
          Some(ResultAddress.Reduction(s, design, focal))
        case ResultAddress.Reduction(s, _, key) => Some(ResultAddress.ContrastRow(s, key))
        case ResultAddress.Estimation(_, _) | ResultAddress.ContrastRow(_, _) => None
      within.map(a => Result(run, CheckedAddress.trusted(a)))
    case ParticipantSummary(run, reporting, scale, Some(group), _) =>
      Some(GroupCell(run, reporting, scale, group))
    case ParticipantSummary(_, _, _, None, _) => None
    case GroupCell(_, _, _, _)                => None
    case FigurePanel(_, _)                    => None
    case WindowTally(_, _)                    => None
    case DesignTally(_, _)                    => None
    case QueryTally(run, tally)               =>
      Option.when(tally != QueryCount.Requested)(QueryTally(run, QueryCount.Requested))
    case InventoryCount(dataset, count) =>
      count match
        case InventoryKind.Cause(_) | InventoryKind.NoFixations =>
          Some(InventoryCount(dataset, InventoryKind.Quarantined))
        case _ => None
    case TrialGroup(_, group) =>
      group match
        case TrialGrouping.PhaseOf(participant, _) => Some(Participant(participant))
        case TrialGrouping.MatchedOn(_)            => None
    case DisplayTally(dataset, tally) =>
      tally match
        case DisplayCount.ImagesFound   => Some(DisplayTally(dataset, DisplayCount.ImagesNamed))
        case DisplayCount.MissingTrials =>
          Some(DisplayTally(dataset, DisplayCount.MissingFiles))
        case _ => None

/** Which count of a dataset revision's displays a [[StudioRef.DisplayTally]]
  * names: the trials showing one kind in one phase (a missing asset is not
  * shown, so it is not counted there), the distinct image files named and
  * those found, and the files missing with the trials naming them.
  */
enum DisplayCount derives CanEqual, Codec.AsObject:
  case Shown(kind: DisplayKind, phase: Phase)
  case ImagesNamed, ImagesFound, MissingFiles, MissingTrials

  /** The trials the registry lists, each with its display. */
  case Trials

/** Which count of a run's query contrasts a [[StudioRef.QueryTally]] names:
  * every requested query, or those with one outcome (each is within the
  * requested).
  */
enum QueryCount derives CanEqual, Codec.AsObject:
  case Requested, Contributing, Failed, NoMatch, QueryNotAdmitted

/** Which trials a [[StudioRef.TrialGroup]] holds: one participant's trials
  * of one phase, or every trial matched on one item.
  */
enum TrialGrouping derives CanEqual, Codec.AsObject:
  case PhaseOf(participant: String, phase: Phase)
  case MatchedOn(item: String)

/** Which frame a [[StudioRef.WindowTally]] counts records outside of: the
  * analysis window (the image frame, on the screen) or the screen itself.
  */
enum TallyRegion derives CanEqual, Codec.AsObject:
  case OutsideWindow, OutsideScreen

/** Which count of a resolved design a [[StudioRef.DesignTally]] names: the
  * focal trials by status, the trials the candidate pairs cross, or the pairs
  * per scale before and after paging.
  */
enum DesignCount derives CanEqual, Codec.AsObject:
  case RequestedQueries, EligibleQueries, UnmatchedQueries, QueriesNotAdmitted, ByDesignQueries
  case FocalTrials, ReferenceTrials, CandidatePairsPerScale, EligiblePairsPerScale

  /** The participants the preview counts, one page of pairs each. */
  case Participants

/** Which inventory trials a [[StudioRef.InventoryCount]] counts, by their
  * admission disposition. `Quarantined` holds every trial admission held
  * back: those quarantined with a cause and the no-fixations trials, whose
  * records exist but none is admissible. `Absent` trials are in the
  * inventory with no records at all; only an inventory can count them.
  */
enum InventoryKind derives CanEqual, Codec.AsObject:
  /** Every trial of the inventory. */
  case Inventory
  case Admitted
  case Quarantined

  /** The trials quarantined with the eyes4s cause `code`
    * ("quarantine.overlap").
    */
  case Cause(code: String)
  case NoFixations
  case Absent

object StudioRef:
  /** A run result from a backend address, refusing a negative scale. */
  def fromAddress(run: RunId, address: ResultAddress): Either[RefError, StudioRef] =
    CheckedAddress.of(address).map(Result(run, _))

  /** eyes4s `ResultRef.PairRow` of one run. */
  object Pair:
    def apply(
        run: RunId,
        scale: ScaleIndex,
        design: PairDesign,
        focal: TrialKey,
        reference: TrialKey
    ): StudioRef =
      Result(
        run,
        CheckedAddress.trusted(ResultAddress.PairRow(scale.value, design, focal, reference))
      )

    def unapply(ref: StudioRef): Option[(RunId, ScaleIndex, PairDesign, TrialKey, TrialKey)] =
      ref match
        case Result(run, a) =>
          a.value match
            case ResultAddress.PairRow(_, design, focal, reference) =>
              Some((run, a.scale, design, focal, reference))
            case _ => None
        case _ => None

  /** eyes4s `ResultRef.Estimation` of one run: one trial's map at one scale
    * (S3.4's map step).
    */
  object TrialMap:
    def apply(run: RunId, scale: ScaleIndex, key: TrialKey): StudioRef =
      Result(run, CheckedAddress.trusted(ResultAddress.Estimation(scale.value, key)))

    def unapply(ref: StudioRef): Option[(RunId, ScaleIndex, TrialKey)] = ref match
      case Result(run, a) =>
        a.value match
          case ResultAddress.Estimation(_, key) => Some((run, a.scale, key))
          case _                                => None
      case _ => None

  /** eyes4s `ResultRef.ContrastRow` of one run: a query's M, B and D. */
  object QueryContrast:
    def apply(run: RunId, scale: ScaleIndex, query: TrialKey): StudioRef =
      Result(run, CheckedAddress.trusted(ResultAddress.ContrastRow(scale.value, query)))

    def unapply(ref: StudioRef): Option[(RunId, ScaleIndex, TrialKey)] = ref match
      case Result(run, a) =>
        a.value match
          case ResultAddress.ContrastRow(_, query) => Some((run, a.scale, query))
          case _                                   => None
      case _ => None

/** Where refs sit in a containment hierarchy: the structural one of
  * [[StudioRef.parent]], optionally extended by data a view holds (such as
  * which participant summary a query contrast contributes to). The graph must
  * be acyclic.
  */
trait Lineage:
  def parents(ref: StudioRef): Vector[StudioRef]

  /** Every container of `ref`, nearest first, each once. */
  final def ancestors(ref: StudioRef): Vector[StudioRef] =
    @annotation.tailrec
    def go(frontier: Vector[StudioRef], seen: Vector[StudioRef]): Vector[StudioRef] =
      val next = frontier.flatMap(parents).distinct.filterNot(seen.contains)
      if next.isEmpty then seen else go(next, seen ++ next)
    go(Vector(ref), Vector.empty)

  final def contains(container: StudioRef, ref: StudioRef): Boolean =
    ancestors(ref).contains(container)

object Lineage:
  /** Only the containment a ref states by itself. */
  val structural: Lineage = ref => ref.parent.toVector

  /** The structural lineage plus `extra` parents. */
  def extended(extra: StudioRef => Vector[StudioRef]): Lineage =
    ref => (ref.parent.toVector ++ extra(ref)).distinct

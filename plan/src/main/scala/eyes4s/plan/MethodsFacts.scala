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

package eyes4s.plan

import cats.syntax.all.*

/** A code a methods fact names: a quarantine cause as the admission ledger
  * codes it, without its family (`overlap`, `wrong-clock`), or a contrast
  * failure's diagnostic code (`study-contrast.off-window`). Lowercase words
  * joined by `-`, segments by `.`.
  */
final case class FactCode private (value: String) derives CanEqual:
  /** The code's last segment, as a methods text names it: `off-window`. */
  def label: String             = value.substring(value.lastIndexOf('.') + 1)
  override def toString: String = value

object FactCode:
  private val Pattern = "[a-z0-9]+(-[a-z0-9]+)*(\\.[a-z0-9]+(-[a-z0-9]+)*)*".r

  def of(value: String): Either[FactError, FactCode] =
    Either.cond(Pattern.matches(value), new FactCode(value), FactError.InvalidCode(value))

  /** The ledger's code for a quarantine cause, without its family. */
  def of(cause: QuarantineCause): FactCode =
    new FactCode(DiagnosticFamily.slug(cause.productPrefix))

/** Why a trial is missing from the analysis: quarantined with a cause, no
  * admitted fixations, or absent from the fixation records.
  */
enum TrialLoss derives CanEqual:
  case Quarantined(cause: FactCode)
  case NoFixations
  case Absent

  def label: String = this match
    case Quarantined(cause) => cause.label
    case NoFixations        => "no admitted fixations"
    case Absent             => "absent"

object TrialLoss:
  /** The loss a disposition names; an admitted trial has none. */
  def of(disposition: TrialDisposition): Option[TrialLoss] = disposition match
    case TrialDisposition.Admitted       => None
    case TrialDisposition.Quarantined(c) => Some(Quarantined(FactCode.of(c)))
    case TrialDisposition.NoFixations    => Some(NoFixations)
    case TrialDisposition.Absent         => Some(Absent)

/** A count, share or policy of the admission ledger and its tallies. */
enum LedgerCount derives CanEqual:
  /** The decoded source records. */
  case FixationRecords

  /** The fixation records of admitted trials, which the window and screen
    * tallies count.
    */
  case TalliedRecords
  case InventoryTrials, Admitted, Quarantined
  case Cause(cause: FactCode)
  case NoFixations, Absent
  case RecordsOutsideWindow, TrialsOutsideWindow, OutsideWindowShare
  case RecordsOutsideScreen, TrialsOutsideScreen
  case OffScreenPolicy

/** A count of queries: of a prepared design (the preview, `StudyCounts`) or
  * of a run's query contrasts.
  */
enum QueryTotal derives CanEqual:
  case Requested, Eligible, NotAdmitted, Unmatched, Contributing, Failed
  case Failure(code: FactCode)
  case ControlsPerQuery

/** A report cell in the report's own terms: its group levels by term, its
  * role and component, its scale index, and the participant when the cell is
  * one participant's. The plan does not name the results module's types;
  * `eyes4s.results` builds these keys from its cells.
  */
final case class CellKey(
    group: Vector[(String, String)],
    role: String,
    component: String,
    scale: Int,
    participant: Option[String]
) derives CanEqual

/** A report's level contrast in the report's own terms, at a scale index. */
final case class ContrastKey(
    stratum: Vector[(String, String)],
    term: String,
    minuend: String,
    subtrahend: String,
    scale: Int
) derives CanEqual

/** Where a fact in a methods text comes from, so a host links the number to
  * what it counts.
  */
enum FactSource derives CanEqual:
  /** A recipe field of the plan. */
  case PlanField(field: FieldId)

  /** The admission ledger of the dataset the run used. */
  case Ledger(count: LedgerCount)

  /** The prepared design, before a run (the preview). */
  case Design(count: QueryTotal)

  /** A run's query contrasts. */
  case Run(count: QueryTotal)

  /** The report cells a value is read from (a range spans several). */
  case ReportCells(cells: Vector[CellKey])

  /** A report's level contrast. */
  case ReportContrast(key: ContrastKey)

  /** The reporting specification itself, by its id. */
  case ReportSpec(id: String)

  /** A fact only the host knows (a dataset revision, a run label). */
  case Host(name: String)

/** What a fact states in the methods text (CR6d). Each slot takes one kind
  * of [[FactValue]].
  */
enum FactSlot derives CanEqual:
  case DatasetRevision, FixationRecords, TalliedRecords, InventoryTrials, Admitted, Quarantined
  case QuarantineCause(cause: FactCode)
  case NoFixations, Absent
  case RecordsOutsideWindow, TrialsOutsideWindow, OutsideWindowShare
  case RecordsOutsideScreen, TrialsOutsideScreen, OffScreenPolicy
  case RequestedQueries, EligibleQueries, NotAdmittedQueries, UnmatchedQueries
  case ContributingQueries, FailedQueries
  case FailureCause(code: FactCode)
  case ControlsPerQuery
  case ReportingSpec, MinimumQueries, GroupSizeRange
  case GroupN(level: String)
  case PairedN, BelowMinimumQueries, SmallestGroups

  /** The slot's stable identity, for a host to link and diff facts by:
    * `eligibleQueries`, `quarantineCause.overlap`, `groupN.Remembered`.
    * Distinct slots have distinct ids, and [[FactSlot.fromSlotId]] reads one
    * back.
    */
  def slotId: String = this match
    case DatasetRevision      => "datasetRevision"
    case FixationRecords      => "fixationRecords"
    case TalliedRecords       => "talliedRecords"
    case InventoryTrials      => "inventoryTrials"
    case Admitted             => "admitted"
    case Quarantined          => "quarantined"
    case QuarantineCause(c)   => s"${FactSlot.CausePrefix}$c"
    case NoFixations          => "noFixations"
    case Absent               => "absent"
    case RecordsOutsideWindow => "recordsOutsideWindow"
    case TrialsOutsideWindow  => "trialsOutsideWindow"
    case OutsideWindowShare   => "outsideWindowShare"
    case RecordsOutsideScreen => "recordsOutsideScreen"
    case TrialsOutsideScreen  => "trialsOutsideScreen"
    case OffScreenPolicy      => "offScreenPolicy"
    case RequestedQueries     => "requestedQueries"
    case EligibleQueries      => "eligibleQueries"
    case NotAdmittedQueries   => "notAdmittedQueries"
    case UnmatchedQueries     => "unmatchedQueries"
    case ContributingQueries  => "contributingQueries"
    case FailedQueries        => "failedQueries"
    case FailureCause(c)      => s"${FactSlot.FailurePrefix}$c"
    case ControlsPerQuery     => "controlsPerQuery"
    case ReportingSpec        => "reportingSpec"
    case MinimumQueries       => "minimumQueries"
    case GroupSizeRange       => "groupSizeRange"
    case GroupN(level)        => s"${FactSlot.GroupPrefix}$level"
    case PairedN              => "pairedN"
    case BelowMinimumQueries  => "belowMinimumQueries"
    case SmallestGroups       => "smallestGroups"

object FactSlot:
  private val CausePrefix   = "quarantineCause."
  private val FailurePrefix = "failureCause."
  private val GroupPrefix   = "groupN."

  /** Every slot without a parameter. */
  val fixed: Vector[FactSlot] = Vector(
    DatasetRevision,
    FixationRecords,
    TalliedRecords,
    InventoryTrials,
    Admitted,
    Quarantined,
    NoFixations,
    Absent,
    RecordsOutsideWindow,
    TrialsOutsideWindow,
    OutsideWindowShare,
    RecordsOutsideScreen,
    TrialsOutsideScreen,
    OffScreenPolicy,
    RequestedQueries,
    EligibleQueries,
    NotAdmittedQueries,
    UnmatchedQueries,
    ContributingQueries,
    FailedQueries,
    ControlsPerQuery,
    ReportingSpec,
    MinimumQueries,
    GroupSizeRange,
    PairedN,
    BelowMinimumQueries,
    SmallestGroups
  )

  /** The slot a [[FactSlot.slotId]] names. */
  def fromSlotId(id: String): Option[FactSlot] =
    def after(prefix: String) = Option.when(id.startsWith(prefix))(id.drop(prefix.length))
    fixed
      .find(_.slotId == id)
      .orElse(after(CausePrefix).flatMap(FactCode.of(_).toOption).map(QuarantineCause(_)))
      .orElse(after(FailurePrefix).flatMap(FactCode.of(_).toOption).map(FailureCause(_)))
      .orElse(after(GroupPrefix).filter(_.trim.nonEmpty).map(GroupN(_)))

/** The basis of a share. */
enum ShareBasis derives CanEqual:
  /** Of the total fixation duration. */
  case FixationDuration

  def label: String = this match
    case FixationDuration => "fixation duration"

/** Queries that had a number of controls, and, below the most, why they had
  * fewer when that is known.
  */
final case class ControlCount(controls: Int, queries: Long, loss: Option[TrialLoss])
    derives CanEqual

/** One participant-and-group cell of a breakdown, with its queries and the
  * report cell it is read from.
  */
final case class GroupCell(participant: String, group: String, queries: Int, source: FactSource)
    derives CanEqual

/** The value of a fact. */
enum FactValue derives CanEqual:
  /** A non-negative count. */
  case Count(n: Long)

  /** An inclusive range of counts. */
  case Range(min: Long, max: Long)

  /** A share in [0, 1]; a share that is undefined is not given at all. */
  case Share(fraction: Double, basis: ShareBasis)

  /** Controls per compared query (contributing and failed): a query count
    * for every control count.
    */
  case Controls(counts: Vector[ControlCount])

  /** Participant-and-group cells, each with its own source. */
  case Breakdown(cells: Vector[GroupCell])

  /** A name: a dataset revision, a reporting specification. */
  case Label(text: String)

  /** The admission's policy for records off the screen. */
  case OffScreen(policy: eyes4s.plan.OffScreenPolicy)

/** Why a fact or a set of facts was refused; every case names its operands. */
enum FactError derives CanEqual:
  /** The slot takes another kind of value. */
  case WrongValue(slot: String, value: String, expected: String)
  case NegativeCount(slot: String, value: Long)
  case EmptyRange(slot: String, min: Long, max: Long)
  case BlankLabel(slot: String)

  /** A share outside [0, 1] or not finite. */
  case InvalidShare(slot: String, fraction: Double)

  /** A controls fact with no counts, or one control count given twice. */
  case NoCounts(slot: String)
  case RepeatedControls(slot: String, controls: Int)

  /** A loss given for the queries with the most controls. */
  case NotFewer(slot: String, controls: Int)
  case Duplicate(slot: String)
  case InvalidCode(value: String)

  /** A total that its parts, all given, do not sum to. */
  case Inconsistent(total: String, value: Long, parts: Vector[String], sum: Long)

  def message: String = this match
    case WrongValue(slot, value, expected) => s"The $slot fact is $value; it takes $expected."
    case NegativeCount(slot, n)            => s"The $slot fact counts $n, which is negative."
    case EmptyRange(slot, min, max)        => s"The $slot range runs from $min to $max."
    case BlankLabel(slot)                  => s"The $slot fact names something blank."
    case InvalidShare(slot, f) => s"The $slot share is $f, which is not a fraction in [0, 1]."
    case NoCounts(slot)        => s"The $slot fact gives no control counts."
    case RepeatedControls(slot, controls) =>
      s"The $slot fact gives the queries with $controls controls twice."
    case NotFewer(slot, controls) =>
      s"The $slot fact gives a loss for the queries with $controls controls, the most."
    case Duplicate(slot)    => s"The $slot fact is given twice."
    case InvalidCode(value) =>
      s"'$value' is not a code: lowercase words joined by '-', segments by '.'."
    case Inconsistent(total, value, parts, sum) =>
      s"The $total fact counts $value, but ${parts.mkString(" + ")} sum to $sum."

/** One fact for a methods text: what it states, where it comes from, and its
  * value.
  */
final case class Fact private (slot: FactSlot, source: FactSource, value: FactValue)
    derives CanEqual

object Fact:
  /** A fact whose value has the kind its slot takes: counts and ranges are
    * non-negative and ranges non-empty, shares are fractions, labels and
    * group levels are not blank, and a controls fact gives each control count
    * once, with a loss only below the most.
    */
  def of(slot: FactSlot, source: FactSource, value: FactValue): Either[FactError, Fact] =
    import FactSlot.*
    val name                      = slot.slotId
    def count(n: Long)            = Either.cond(n >= 0, (), FactError.NegativeCount(name, n))
    def label(t: String)          = Either.cond(t.trim.nonEmpty, (), FactError.BlankLabel(name))
    def range(r: FactValue.Range) =
      count(r.min) *> count(r.max) *>
        Either.cond(r.min <= r.max, (), FactError.EmptyRange(name, r.min, r.max))
    val level = slot match
      case GroupN(l) => label(l)
      case _         => Right(())
    val kind = (slot, value) match
      case (DatasetRevision | ReportingSpec, FactValue.Label(t)) => label(t)
      case (GroupSizeRange, r: FactValue.Range)                  => range(r)
      case (OutsideWindowShare, FactValue.Share(f, _))           =>
        Either.cond(f >= 0 && f <= 1, (), FactError.InvalidShare(name, f))
      case (OffScreenPolicy, FactValue.OffScreen(_))  => Right(())
      case (ControlsPerQuery, FactValue.Controls(cs)) =>
        val most     = cs.map(_.controls).maxOption
        val repeated = cs.map(_.controls).diff(cs.map(_.controls).distinct).headOption
        Either.cond(cs.nonEmpty, (), FactError.NoCounts(name)) *>
          repeated.map(FactError.RepeatedControls(name, _)).toLeft(()) *>
          cs.traverse_ { c =>
            count(c.controls.toLong) *> count(c.queries) *>
              Either.cond(
                c.loss.isEmpty || !most.contains(c.controls),
                (),
                FactError.NotFewer(name, c.controls)
              )
          }
      case (BelowMinimumQueries | SmallestGroups, FactValue.Breakdown(cells)) =>
        cells.traverse_(c => label(c.participant) *> label(c.group) *> count(c.queries.toLong))
      case (
            FixationRecords | TalliedRecords | InventoryTrials | Admitted | Quarantined |
            QuarantineCause(_) | NoFixations | Absent | RecordsOutsideWindow |
            TrialsOutsideWindow | RecordsOutsideScreen | TrialsOutsideScreen |
            RequestedQueries | EligibleQueries | NotAdmittedQueries | UnmatchedQueries |
            ContributingQueries | FailedQueries | FailureCause(_) | MinimumQueries | GroupN(_) |
            PairedN,
            FactValue.Count(n)
          ) =>
        count(n)
      case _ => Left(FactError.WrongValue(name, value.toString, expectedKind(slot)))
    (level *> kind).map(_ => new Fact(slot, source, value))

  private def expectedKind(slot: FactSlot): String = slot match
    case FactSlot.DatasetRevision | FactSlot.ReportingSpec      => "a label"
    case FactSlot.GroupSizeRange                                => "a range"
    case FactSlot.OutsideWindowShare                            => "a share"
    case FactSlot.OffScreenPolicy                               => "an off-screen policy"
    case FactSlot.ControlsPerQuery                              => "control counts"
    case FactSlot.BelowMinimumQueries | FactSlot.SmallestGroups => "a breakdown"
    case _                                                      => "a count"

/** The facts a methods text cites beyond its plan, one per slot.
  * [[StudyText.methods]] states every one of them; each number is a token
  * carrying its slot, its typed value and its one source.
  */
final case class MethodsFacts private (facts: Vector[Fact]) derives CanEqual:
  def get(slot: FactSlot): Option[Fact] = facts.find(_.slot == slot)

object MethodsFacts:
  val empty: MethodsFacts = new MethodsFacts(Vector.empty)

  /** The facts, each slot once (the first repeated slot, in input order, is
    * refused), whose totals agree with their parts when all are given: the
    * inventory trials are the admitted, quarantined, no-fixation and absent
    * trials; the quarantined trials are the sum of their causes; the failed
    * queries are the sum of their failure causes.
    */
  def of(facts: Vector[Fact]): Either[FactError, MethodsFacts] =
    val ids      = facts.map(_.slot.slotId)
    val repeated = ids.zipWithIndex.collectFirst {
      case (id, i) if ids.indexOf(id) < i => FactError.Duplicate(id)
    }
    def n(slot: FactSlot): Option[Long] = facts.find(_.slot == slot).collect {
      case Fact(_, _, FactValue.Count(c)) => c
    }
    def counts(p: FactSlot => Boolean) = facts.filter(f => p(f.slot)).collect {
      case Fact(s, _, FactValue.Count(c)) => (s.slotId, c)
    }
    def agree(total: FactSlot, parts: Vector[(String, Long)]) =
      n(total) match
        case Some(t) if parts.nonEmpty && parts.map(_._2).sum != t =>
          Left(FactError.Inconsistent(total.slotId, t, parts.map(_._1), parts.map(_._2).sum))
        case _ => Right(())
    val dispositions =
      Vector(FactSlot.Admitted, FactSlot.Quarantined, FactSlot.NoFixations, FactSlot.Absent)
    val trials =
      if dispositions.forall(n(_).isDefined) then
        agree(FactSlot.InventoryTrials, dispositions.map(s => (s.slotId, n(s).getOrElse(0L))))
      else Right(())
    repeated.toLeft(()) *> trials *>
      agree(
        FactSlot.Quarantined,
        counts {
          case FactSlot.QuarantineCause(_) => true
          case _                           => false
        }
      ) *>
      agree(
        FactSlot.FailedQueries,
        counts {
          case FactSlot.FailureCause(_) => true
          case _                        => false
        }
      ) *> Right(new MethodsFacts(facts))

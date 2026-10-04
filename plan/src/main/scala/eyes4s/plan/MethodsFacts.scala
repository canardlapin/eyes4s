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

/** Why an inventory trial left the analysis, as a methods text names it.
  * The labels are the boards' words.
  */
enum AdmissionCause derives CanEqual:
  case Overlap, NoFixations, DuplicateOrdinals, RejectedRecords, Absent

  /** Any other quarantine cause, by its slug (`wrong-clock`). */
  case Other(slug: String)

  def label: String = this match
    case Overlap           => "overlap"
    case NoFixations       => "no-fixations"
    case DuplicateOrdinals => "duplicate-ordinals"
    case RejectedRecords   => "rejected-records"
    case Absent            => "absent (no fixation records)"
    case Other(slug)       => slug

object AdmissionCause:
  def of(cause: QuarantineCause): AdmissionCause = cause match
    case QuarantineCause.Overlap(_, _, _)  => Overlap
    case QuarantineCause.NoFixations       => NoFixations
    case QuarantineCause.DuplicateOrdinals => DuplicateOrdinals
    case QuarantineCause.RejectedRecords   => RejectedRecords
    case other                             => Other(DiagnosticFamily.slug(other.productPrefix))

  /** The cause a disposition names; an admitted trial has none. */
  def of(disposition: TrialDisposition): Option[AdmissionCause] = disposition match
    case TrialDisposition.Admitted       => None
    case TrialDisposition.Quarantined(c) => Some(of(c))
    case TrialDisposition.NoFixations    => Some(NoFixations)
    case TrialDisposition.Absent         => Some(Absent)

/** A count of the admission ledger and its window tallies. */
enum LedgerCount derives CanEqual:
  case FixationRecords, InventoryTrials, Admitted, Quarantined
  case Cause(cause: AdmissionCause)
  case RecordsOutsideWindow, TrialsOutsideWindow, RecordsOutsideScreen

/** A count of the prepared design (the preview, `StudyCounts`). */
enum DesignCount derives CanEqual:
  case RequestedQueries, EligibleQueries, NotAdmittedQueries, UnmatchedQueries
  case ControlsPerQuery

/** A report cell in the report's own terms: its group levels by term, its
  * role and component. The plan does not name the results module's types;
  * `eyes4s.results` builds these keys from its cells.
  */
final case class CellKey(group: Vector[(String, String)], role: String, component: String)
    derives CanEqual

/** A report's level contrast in the report's own terms. */
final case class ContrastKey(
    stratum: Vector[(String, String)],
    term: String,
    minuend: String,
    subtrahend: String
) derives CanEqual

/** Where a fact in a methods text comes from, so a host links the number to
  * what it counts.
  */
enum FactSource derives CanEqual:
  /** A recipe field of the plan. */
  case PlanField(field: FieldId)

  /** The admission ledger of the dataset the run used. */
  case Ledger(count: LedgerCount)

  /** The prepared design. */
  case Design(count: DesignCount)

  /** The report cells a value is read from (a range spans several). */
  case ReportCells(cells: Vector[CellKey])

  /** A report's level contrast. */
  case ReportContrast(key: ContrastKey)

  /** The reporting specification itself, by its id. */
  case ReportSpec(id: String)

  /** A fact only the host knows (a run label, a build). */
  case Host(name: String)

/** What a fact states in the methods text (CR6d). Each slot takes one kind
  * of [[FactValue]].
  */
enum FactSlot derives CanEqual:
  case DatasetRevision, FixationRecords, InventoryTrials, Admitted, Quarantined
  case QuarantineCause(cause: AdmissionCause)
  case Absent, RecordsOutsideWindow, TrialsOutsideWindow, RecordsOutsideScreen
  case RequestedQueries, EligibleQueries, NotAdmittedQueries, UnmatchedQueries
  case ControlsPerQuery
  case ReportingSpec, GroupSizeRange, PairedN, BelowMinimumQueries

  /** The slot's stable name: `eligibleQueries`, `quarantineCause.overlap`. */
  def name: String = this match
    case QuarantineCause(cause) =>
      val kind = cause match
        case AdmissionCause.Other(slug) => slug
        case other                      => DiagnosticFamily.slug(other.toString)
      s"quarantineCause.$kind"
    case other => other.toString.head.toLower +: other.toString.tail

/** One participant-and-group cell left out under the minimum, with the
  * queries it had.
  */
final case class BelowMinimum(participant: String, group: String, queries: Int) derives CanEqual

/** A number of queries with fewer controls than the most any query had, and
  * why.
  */
final case class FewerControls(queries: Int, controls: Int, cause: AdmissionCause)
    derives CanEqual

/** The value of a fact. */
enum FactValue derives CanEqual:
  /** A non-negative count. */
  case Count(n: Long)

  /** An inclusive range of counts. */
  case Range(min: Long, max: Long)

  /** Controls per eligible query: the range, and the queries below its
    * maximum with their cause.
    */
  case Controls(range: Range, fewer: Vector[FewerControls])

  /** Participant-and-group cells below a minimum, whether or not the minimum
    * was applied.
    */
  case Breakdown(cells: Vector[BelowMinimum])

  /** A name: a dataset revision, a reporting specification. */
  case Label(text: String)

/** Why a fact was refused; every case names its operands. */
enum FactError derives CanEqual:
  /** The slot takes another kind of value. */
  case WrongValue(slot: String, value: String, expected: String)
  case NegativeCount(slot: String, value: Long)
  case EmptyRange(slot: String, min: Long, max: Long)
  case BlankLabel(slot: String)

  /** A query below the most controls states as many controls as the most. */
  case NotFewer(slot: String, controls: Int, maximum: Long)
  case Duplicate(slot: String)

  def message: String = this match
    case WrongValue(slot, value, expected) => s"The $slot fact is $value; it takes $expected."
    case NegativeCount(slot, n)            => s"The $slot fact counts $n, which is negative."
    case EmptyRange(slot, min, max)        => s"The $slot range runs from $min to $max."
    case BlankLabel(slot)                  => s"The $slot fact is blank."
    case NotFewer(slot, controls, maximum) =>
      s"The $slot fact lists queries with $controls controls as fewer than the most, $maximum."
    case Duplicate(slot) => s"The $slot fact is given twice."

/** One fact for a methods text: what it states, where it comes from, and its
  * value.
  */
final case class Fact private (slot: FactSlot, source: FactSource, value: FactValue)
    derives CanEqual

object Fact:
  /** A fact whose value has the kind its slot takes: counts and ranges are
    * non-negative and ranges non-empty, labels are not blank.
    */
  def of(slot: FactSlot, source: FactSource, value: FactValue): Either[FactError, Fact] =
    import FactSlot.*
    val name                      = slot.name
    def count(n: Long)            = Either.cond(n >= 0, (), FactError.NegativeCount(name, n))
    def range(r: FactValue.Range) =
      count(r.min) *> count(r.max) *>
        Either.cond(r.min <= r.max, (), FactError.EmptyRange(name, r.min, r.max))
    val kind = (slot, value) match
      case (DatasetRevision | ReportingSpec, FactValue.Label(t)) =>
        Either.cond(t.trim.nonEmpty, (), FactError.BlankLabel(name))
      case (GroupSizeRange, r: FactValue.Range)             => range(r)
      case (ControlsPerQuery, FactValue.Controls(r, fewer)) =>
        range(r) *> fewer.traverse_(f =>
          count(f.queries.toLong) *> count(f.controls.toLong) *>
            Either.cond(
              f.controls < r.max,
              (),
              FactError.NotFewer(name, f.controls, r.max)
            )
        )
      case (BelowMinimumQueries, FactValue.Breakdown(cells)) =>
        cells.traverse_(c => count(c.queries.toLong))
      case (
            FixationRecords | InventoryTrials | Admitted | Quarantined | QuarantineCause(_) |
            Absent | RecordsOutsideWindow | TrialsOutsideWindow | RecordsOutsideScreen |
            RequestedQueries | EligibleQueries | NotAdmittedQueries | UnmatchedQueries |
            PairedN,
            FactValue.Count(n)
          ) =>
        count(n)
      case _ => Left(FactError.WrongValue(name, value.toString, expectedKind(slot)))
    kind.map(_ => new Fact(slot, source, value))

  private def expectedKind(slot: FactSlot): String = slot match
    case FactSlot.DatasetRevision | FactSlot.ReportingSpec => "a label"
    case FactSlot.GroupSizeRange                           => "a range"
    case FactSlot.ControlsPerQuery                         => "a controls range"
    case FactSlot.BelowMinimumQueries                      => "a breakdown"
    case _                                                 => "a count"

/** The facts a methods text cites beyond its plan, one per slot. */
final case class MethodsFacts private (facts: Vector[Fact]) derives CanEqual:
  def get(slot: FactSlot): Option[Fact] = facts.find(_.slot == slot)

object MethodsFacts:
  val empty: MethodsFacts = new MethodsFacts(Vector.empty)

  def of(facts: Vector[Fact]): Either[FactError, MethodsFacts] =
    facts
      .groupBy(_.slot)
      .collectFirst { case (slot, all) if all.size > 1 => FactError.Duplicate(slot.name) }
      .toLeft(new MethodsFacts(facts))

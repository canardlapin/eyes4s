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

import eyes4s.core.ScanpathError
import eyes4s.kernel.*

/** Nominal reference to the source records an importer admitted. The records
  * themselves live outside plan JSON; the digest is the portable semantic
  * `ContentHash` of the decoded header and records, not a byte checksum, and
  * the label is a display name rather than an identity.
  */
final case class SourceRef(label: String, records: ArtifactRef[Vector[Vector[String]]])
    derives CanEqual

object SourceRef:
  def of(label: String, header: Vector[String], rows: Vector[Vector[String]]): SourceRef =
    SourceRef(label, ArtifactRef.of(digest(header, rows)))

  /** Field counts and the record count are mixed in so that shifted fields differ. */
  def digest(header: Vector[String], rows: Vector[Vector[String]]): ContentHash =
    def record(fields: Vector[String]): ContentHash =
      ContentHash.combineAll(
        ContentHash.ofString(fields.size.toString) +: fields.map(ContentHash.ofString)
      )
    ContentHash.combineAll(
      (header +: rows).map(record) :+ ContentHash.ofString(rows.size.toString)
    )

/** Why a whole keyed trial was quarantined. The case is the machine-readable
  * reason; rendered spans and messages are operands for a reader.
  */
enum QuarantineCause derives CanEqual:
  case RejectedRecords
  case DuplicateOrdinals
  case NoFixations
  case Overlap(index: Int, previous: String, current: String)
  case WrongClock(index: Int, expected: String, actual: String)
  case InvalidTransition(index: Int, reason: String)
  case InvalidExtent(reason: String)
  case UnmappableFixation(index: Int, from: FrameId, to: FrameId, x: Double, y: Double)

  def message: String = this match
    case RejectedRecords     => "one or more source rows were rejected"
    case DuplicateOrdinals   => "duplicate fixation ordinals"
    case NoFixations         => "a scanpath needs at least one fixation"
    case Overlap(i, p, c)    => s"fixation $i at $c begins before the previous fixation $p ends"
    case WrongClock(i, e, a) => s"fixation $i is on clock $a, expected $e"
    case InvalidTransition(i, r)               => s"transition into fixation $i: $r"
    case InvalidExtent(r)                      => s"scanpath extent: $r"
    case UnmappableFixation(i, from, to, x, y) =>
      s"fixation $i at ($x, $y) cannot be mapped from ${from.name} to ${to.name}"

object QuarantineCause:
  /** Total over the scanpath constructor's errors; each case keeps its operands. */
  def of(error: ScanpathError): QuarantineCause = error match
    case ScanpathError.NoFixations                           => NoFixations
    case ScanpathError.OutOfOrder(i, p, c)                   => Overlap(i, p, c)
    case ScanpathError.WrongClock(i, e, a)                   => WrongClock(i, e, a)
    case ScanpathError.InvalidTransitionSpan(i, e)           => InvalidTransition(i, e.message)
    case ScanpathError.InvalidExtent(e)                      => InvalidExtent(e.message)
    case ScanpathError.UnmappableFixation(i, from, to, x, y) =>
      UnmappableFixation(i, from, to, x, y)

/** Why one source record was not admitted. Every case names its operands. */
enum AdmissionReason derives CanEqual:
  case Width(expected: Int, actual: Int)
  case Key(reason: String)
  case Number(column: String, value: String, requirement: String)
  case Time(onset: String, duration: String, unit: String, reason: String)
  case Position(x: Double, y: Double, frame: FrameId)
  case Event(reason: String)
  case Quarantined(records: Vector[Int], cause: QuarantineCause)

  def message: String = this match
    case Width(e, a)          => s"Expected $e fields, got $a."
    case Key(reason)          => s"Trial key: $reason"
    case Number(c, v, r)      => s"Column '$c' has '$v'; expected $r."
    case Time(o, d, u, r)     => s"Onset '$o', duration '$d' in $u: $r"
    case Position(x, y, f)    => s"Position ($x,$y) is outside frame ${f.name}."
    case Event(reason)        => s"Fixation: $reason"
    case Quarantined(rows, c) => s"Trial from records $rows: ${c.message}"

/** What happened to one source record. An admitted record links its logical
  * record number to a typed trial key and the ordinal it supplied; a rejected
  * record keeps its raw fields, its key when one could be read, and the reason.
  */
sealed trait Disposition[K] derives CanEqual
object Disposition:
  final case class Admitted[K](key: K, ordinal: Int) extends Disposition[K]
  final case class Rejected[K](raw: Vector[String], key: Option[K], reason: AdmissionReason)
      extends Disposition[K]

final case class SourceRecord[K](record: Int, disposition: Disposition[K]) derives CanEqual:
  def isAdmitted: Boolean = disposition match
    case Disposition.Admitted(_, _)    => true
    case Disposition.Rejected(_, _, _) => false

/** What the analyst decided when the import finished. */
enum AdmissionDecision derives CanEqual:
  case RequireComplete
  case ReviewExclusions

/** The recorded outcome. `Complete` means every record was admitted; the other
  * two are only valid when rejected records exist, so the decision is data.
  */
enum AdmissionOutcome derives CanEqual:
  case Complete
  case Refused
  case ReviewedExclusions

enum AdmissionError derives CanEqual:
  case NonPositiveRecord(record: Int)
  case RecordOrder(index: Int, previous: Int, record: Int)
  case NegativeOrdinal(record: Int, value: Int)
  case DuplicateOrdinal(records: Vector[Int], value: Int)
  case QuarantineScope(record: Int, records: Vector[Int])
  case QuarantineAdmitted(record: Int, admitted: Int)
  case QuarantinedKeyAdmitted(quarantined: Int, admitted: Int)
  case OutcomeMismatch(outcome: AdmissionOutcome, rejected: Int)
  case AmbiguousTrial(indices: Vector[Int])
  case UnknownTrial(records: Vector[Int])
  case UnadmittedTrial(index: Int)
  case FixationCount(index: Int, fixations: Int, records: Int)

  def message: String = this match
    case NonPositiveRecord(r) => s"Source record numbers are positive, got $r."
    case RecordOrder(i, p, r) =>
      s"Source records must be strictly increasing; entry $i has record $r after $p."
    case NegativeOrdinal(r, o)   => s"Record $r admits a negative ordinal $o."
    case DuplicateOrdinal(rs, o) => s"Records $rs admit the same ordinal $o for one trial."
    case QuarantineScope(r, rs)  =>
      s"Record $r is quarantined with records $rs, which must include it and exist in the ledger."
    case QuarantineAdmitted(r, a) =>
      s"Record $r is quarantined with record $a, but record $a is admitted."
    case QuarantinedKeyAdmitted(q, a) =>
      s"Record $q quarantines a trial key that record $a admits."
    case OutcomeMismatch(o, n) => s"Outcome $o is inconsistent with $n rejected records."
    case AmbiguousTrial(is)    =>
      s"Input trials $is repeat one key; the ledger cannot address them."
    case UnknownTrial(rs)       => s"Admitted records $rs name a key with no input trial."
    case UnadmittedTrial(i)     => s"Input trial $i has no admitted source records."
    case FixationCount(i, f, r) =>
      s"Input trial $i has $f fixations but $r admitted source records."

/** The explicit admission ledger of one imported source: every source record
  * with exactly one disposition, in record order, plus the recorded outcome.
  * Trial identity is the typed key; no digest or label stands in for it.
  */
final case class AdmissionLedger[K] private (
    source: SourceRef,
    header: Vector[String],
    records: Vector[SourceRecord[K]],
    outcome: AdmissionOutcome
) derives CanEqual:
  def admitted: Vector[SourceRecord[K]] = records.filter(_.isAdmitted)
  def rejected: Vector[SourceRecord[K]] = records.filterNot(_.isAdmitted)

  /** Keys of trials quarantined as a whole, in first-record order. */
  def quarantined: Vector[K] =
    records.collect {
      case SourceRecord(
            _,
            Disposition.Rejected(_, Some(key), AdmissionReason.Quarantined(_, _))
          ) =>
        key
    }.distinct

  /** Admitted records must address every input trial exactly once per fixation.
    * Ordinals are the source's own values: distinct within a trial and as many
    * as the trial has fixations; their rank order is the fixation order.
    * Errors name the first offender in input or record order.
    */
  def checkAgainst[U <: Unit2D](input: StudyInput[K, U]): Either[AdmissionError, Unit] =
    val byKey = records
      .collect { case SourceRecord(r, Disposition.Admitted(k, _)) => k -> r }
      .groupMap(_._1)(_._2)
    val trials    = input.trials.rows.zipWithIndex
    val known     = trials.map(_._1.key).toSet
    val ambiguous = trials.collectFirst {
      case (trial, i) if trials.count(_._1.key == trial.key) > 1 =>
        AdmissionError.AmbiguousTrial(trials.collect { case (t, j) if t.key == trial.key => j })
    }
    val orphans = records.collect {
      case SourceRecord(r, Disposition.Admitted(k, _)) if !known.contains(k) => r
    }
    ambiguous match
      case Some(error)              => Left(error)
      case None if orphans.nonEmpty => Left(AdmissionError.UnknownTrial(orphans))
      case None                     =>
        trials
          .collectFirst {
            case (trial, i) if !byKey.contains(trial.key) => AdmissionError.UnadmittedTrial(i)
            case (trial, i) if byKey(trial.key).size != trial.value.n =>
              AdmissionError.FixationCount(i, trial.value.n, byKey(trial.key).size)
          }
          .toLeft(())

object AdmissionLedger:
  /** Every invariant reports the first offending record in record order. */
  def of[K](
      source: SourceRef,
      header: Vector[String],
      records: Vector[SourceRecord[K]],
      outcome: AdmissionOutcome
  ): Either[AdmissionError, AdmissionLedger[K]] =
    val rejected      = records.count(!_.isAdmitted)
    val numbers       = records.map(_.record).toSet
    val admittedByKey = records
      .collect { case SourceRecord(r, Disposition.Admitted(k, _)) => k -> r }
      .groupMap(_._1)(_._2)
    val admittedNumbers = records.collect { case SourceRecord(r, Disposition.Admitted(_, _)) =>
      r
    }.toSet
    def ordered = records.zipWithIndex.collectFirst {
      case (r, _) if r.record < 1 => AdmissionError.NonPositiveRecord(r.record)
      case (r, i) if i > 0 && r.record <= records(i - 1).record =>
        AdmissionError.RecordOrder(i, records(i - 1).record, r.record)
    }
    def ordinals = records.collectFirst {
      case SourceRecord(r, Disposition.Admitted(_, o)) if o < 0 =>
        AdmissionError.NegativeOrdinal(r, o)
      case SourceRecord(r, Disposition.Admitted(k, o)) if records.exists {
            case SourceRecord(other, Disposition.Admitted(k2, o2)) =>
              other != r && k2 == k && o2 == o
            case _ => false
          } =>
        AdmissionError.DuplicateOrdinal(
          records.collect {
            case SourceRecord(other, Disposition.Admitted(k2, o2)) if k2 == k && o2 == o =>
              other
          },
          o
        )
    }
    def scope(entry: SourceRecord[K]): Option[AdmissionError] = entry match
      case SourceRecord(r, Disposition.Rejected(_, key, AdmissionReason.Quarantined(rs, _))) =>
        if !rs.contains(r) || !rs.forall(numbers.contains) then
          Some(AdmissionError.QuarantineScope(r, rs))
        else
          rs.find(admittedNumbers.contains)
            .map(AdmissionError.QuarantineAdmitted(r, _))
            .orElse(
              key
                .flatMap(admittedByKey.get)
                .map(admitted => AdmissionError.QuarantinedKeyAdmitted(r, admitted.min))
            )
      case _ => None
    def scopes     = records.iterator.map(scope).collectFirst { case Some(error) => error }
    def consistent = outcome match
      case AdmissionOutcome.Complete if rejected > 0 =>
        Some(AdmissionError.OutcomeMismatch(outcome, rejected))
      case AdmissionOutcome.Complete => None
      case other if rejected == 0    => Some(AdmissionError.OutcomeMismatch(other, rejected))
      case _                         => None
    ordered
      .orElse(ordinals)
      .orElse(scopes)
      .orElse(consistent)
      .toLeft(new AdmissionLedger(source, header, records, outcome))

  /** Derive the outcome from the analyst's decision and the rejected count. */
  def decide[K](
      source: SourceRef,
      header: Vector[String],
      records: Vector[SourceRecord[K]],
      decision: AdmissionDecision
  ): Either[AdmissionError, AdmissionLedger[K]] =
    val outcome =
      if records.forall(_.isAdmitted) then AdmissionOutcome.Complete
      else
        decision match
          case AdmissionDecision.RequireComplete  => AdmissionOutcome.Refused
          case AdmissionDecision.ReviewExclusions => AdmissionOutcome.ReviewedExclusions
    of(source, header, records, outcome)

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
final case class SourceRef(
    label: String,
    records: ArtifactRef[Vector[Vector[String]]],
    interpretation: SourceInterpretation = SourceInterpretation.LegacyUnspecified
) derives CanEqual:
  def identity: Option[SourceIdentity] = interpretation match
    case SourceInterpretation.LegacyUnspecified                 => None
    case SourceInterpretation.Declared(format, parser, options) =>
      Some(SourceIdentity.of(records, format, parser, options))

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

  /** Two correction rules of the admission policy, by position, both cover
    * the trial; which to apply is not decided for the analyst.
    */
  case CorrectionConflict(first: Int, second: Int)

  /** Records of one trial identity name different match items. */
  case ItemConflict(items: Vector[String])

  /** Records of one trial identity name different occurrences. */
  case OccurrenceConflict(occurrences: Vector[Int])

  /** The records' trial is not in the declared trial inventory. */
  case NotInInventory(participant: String, phase: String, trial: String, occurrence: Int)

  /** The records name items other than the one the inventory declares. */
  case InventoryItemConflict(inventory: String, records: Vector[String])

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
    case CorrectionConflict(first, second) =>
      s"correction rules $first and $second both apply to the trial"
    case ItemConflict(items) => s"the trial's records name different match items $items"
    case OccurrenceConflict(occurrences) =>
      s"the trial's records name different occurrences $occurrences"
    case NotInInventory(p, f, t, o) => s"trial $p/$f/$t#$o is not in the trial inventory"
    case InventoryItemConflict(inventory, records) =>
      s"the trial's records name items $records, but the inventory declares '$inventory'"

object QuarantineCause:
  /** True for the causes a version-1 ledger can name; correction, item and
    * occurrence conflicts arrived with the admission policy.
    */
  def isVersion1(cause: QuarantineCause): Boolean = version(cause) == 1

  /** The earliest ledger version that can name the cause: 2 for the
    * admission policy's conflicts, 3 for the trial inventory's.
    */
  def version(cause: QuarantineCause): Int = cause match
    case CorrectionConflict(_, _) | ItemConflict(_) | OccurrenceConflict(_) => 2
    case NotInInventory(_, _, _, _) | InventoryItemConflict(_, _)           => 3
    case _                                                                  => 1

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
  case OutsideFrameRecord(record: Int, policy: OffScreenPolicy)
  case CorrectionConflict(record: Int, first: Int, second: Int)

  /** The ledger's trial inventory was refused. */
  case Inventory(underlying: InventoryError)

  /** A record names a cause only a ledger with a trial inventory can name. */
  case UninventoriedCause(record: Int, cause: QuarantineCause)

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
    case OutsideFrameRecord(r, policy) =>
      s"Record $r is listed as admitted outside the frame, but under $policy it must be an " +
        "admitted record listed once, in record order, and ExcludeRecord must be the policy."
    case CorrectionConflict(r, first, second) =>
      s"Admitted record $r belongs to a trial that correction rules $first and $second both cover."
    case Inventory(underlying)        => s"Trial inventory: ${underlying.message}"
    case UninventoriedCause(r, cause) =>
      s"Record $r is quarantined with ${cause.productPrefix}, which only a ledger with a " +
        "trial inventory can record."

/** What admission does with a record whose coordinates are finite but lie
  * outside the admission frame (off the screen). Unparseable or non-finite
  * coordinates are never covered: they always reject the record and
  * quarantine its trial.
  */
enum OffScreenPolicy derives CanEqual:
  /** Admit the record and list it in the ledger as outside the frame; a study
    * leaves it out of every map and reports it as "outside screen".
    */
  case ExcludeRecord

  /** Reject the record and quarantine its whole trial: the version-1 meaning. */
  case QuarantineTrial

/** Which trials a correction rule covers. A participant scope matches the
  * participant projection of the key layout it is resolved under.
  */
enum CorrectionScope[K] derives CanEqual:
  case AllTrials()
  case Participant(participant: String)
  case Trial(key: K)

  def covers(key: K, participant: K => String): Boolean = this match
    case AllTrials()    => true
    case Participant(p) => participant(key) == p
    case Trial(k)       => k == key

/** One recorded coordinate correction and the trials it covers. */
final case class AppliedCorrection[K](scope: CorrectionScope[K], correction: Correction)
    derives CanEqual

/** The analyst's admission choices for one import, recorded in its ledger:
  * the off-screen policy and the coordinate corrections, in rule order.
  * Corrections apply to the parsed coordinates before containment is
  * checked; raw fields and the source digest are untouched. At most one rule
  * may cover a trial.
  */
final case class AdmissionPolicy[K](
    offScreen: OffScreenPolicy,
    corrections: Vector[AppliedCorrection[K]]
) derives CanEqual:
  /** True when the policy means exactly what a version-1 import meant. */
  def isVersion1: Boolean = offScreen == OffScreenPolicy.QuarantineTrial && corrections.isEmpty

  /** The rule covering a trial, by position, or the first two that both do. */
  def correctionFor(
      key: K,
      participant: K => String
  ): Either[(Int, Int), Option[(Int, Correction)]] =
    val covering = corrections.zipWithIndex.filter(_._1.scope.covers(key, participant))
    if covering.size > 1 then Left(covering(0)._2 -> covering(1)._2)
    else Right(covering.headOption.map { case (rule, index) => index -> rule.correction })

object AdmissionPolicy:
  /** New imports: off-screen records are admitted and reported; no corrections. */
  def default[K]: AdmissionPolicy[K] =
    AdmissionPolicy(OffScreenPolicy.ExcludeRecord, Vector.empty)

  /** The version-1 meaning: off-screen records quarantine their trial. */
  def version1[K]: AdmissionPolicy[K] =
    AdmissionPolicy(OffScreenPolicy.QuarantineTrial, Vector.empty)

/** An admitted record whose position lies outside the admission frame. */
final case class OutsideFrame(record: Int, x: Double, y: Double, frame: FrameId)
    derives CanEqual

/** The explicit admission ledger of one imported source: every source record
  * with exactly one disposition, in record order, plus the recorded outcome.
  * Trial identity is the typed key; no digest or label stands in for it.
  */
final case class AdmissionLedger[K] private (
    source: SourceRef,
    header: Vector[String],
    records: Vector[SourceRecord[K]],
    outcome: AdmissionOutcome,
    policy: AdmissionPolicy[K],
    outsideFrame: Vector[OutsideFrame],
    inventory: Option[InventoryLedger]
) derives CanEqual:
  /** True when the ledger means exactly what a version-1 ledger meant. */
  def isVersion1: Boolean = version == 1

  /** The earliest ledger version that expresses this ledger: 1 for the
    * version-1 admission, 2 with an admission policy, admitted records outside
    * the frame or a policy conflict, 3 with a trial inventory, 4 with declared
    * source interpretation.
    */
  def version: Int =
    val causes = records.iterator
      .collect {
        case SourceRecord(_, Disposition.Rejected(_, _, AdmissionReason.Quarantined(_, c))) =>
          QuarantineCause.version(c)
      }
      .maxOption
      .getOrElse(1)
    val base   = if policy.isVersion1 && outsideFrame.isEmpty then 1 else 2
    val legacy = math.max(if inventory.isDefined then 3 else base, causes)
    if source.identity.isDefined || inventory.exists(_.source.identity.isDefined) then 4
    else legacy

  /** Join a trial inventory to this ledger (see [[AdmissionLedger.inventoried]]
    * for a ledger whose records name the inventory's own causes). `identity`
    * and `item` project a key's trial identity and match item. Every keyed record must be listed
    * under its own trial, and every trial's disposition must agree with its
    * records: an admitted trial's records are all admitted with its resolved
    * item; a trial with no fixations has only records rejected on their own;
    * a quarantined trial admits none and quarantines one with its cause; an
    * unlisted trial admits none and quarantines only as not in the inventory.
    * Errors name the first offending trial in inventory order.
    */
  def withInventory(
      value: InventoryLedger,
      identity: K => TrialIdentity,
      item: K => String
  ): Either[AdmissionError, AdmissionLedger[K]] =
    val byRecord = records.iterator.map(r => r.record -> r.disposition).toMap
    def outcome(d: Disposition[K]): String = d match
      case Disposition.Admitted(_, _)                                    => "admitted"
      case Disposition.Rejected(_, _, AdmissionReason.Quarantined(_, c)) =>
        s"quarantined (${c.productPrefix})"
      case Disposition.Rejected(_, _, reason) => s"rejected (${reason.productPrefix})"
    def keyOf(d: Disposition[K]): Option[K] = d match
      case Disposition.Admitted(k, _)    => Some(k)
      case Disposition.Rejected(_, k, _) => k
    val entries = value.trials.map(t => (t.identity, t.records)) ++
      value.unlisted.map(u => (u.identity, u.records))
    val owner  = entries.flatMap((id, rs) => rs.map(_ -> id)).toMap
    def listed = entries.iterator
      .flatMap((id, rs) =>
        rs.iterator.map(r =>
          byRecord.get(r) match
            case None    => Some(InventoryError.UnknownRecord(id.render, r))
            case Some(d) =>
              keyOf(d)
                .map(identity)
                .filter(_ != id)
                .map(found => InventoryError.ForeignRecord(id.render, r, found.render))
        )
      )
      .collectFirst { case Some(e) => e }
    def claimed = records.iterator
      .collect { case SourceRecord(r, d) if keyOf(d).isDefined => r -> identity(keyOf(d).get) }
      .collectFirst {
        case (r, id) if !owner.get(r).contains(id) =>
          InventoryError.UnclaimedRecord(r, id.render)
      }
    def mismatch(t: TrialIdentity, label: String)(bad: Disposition[K] => Boolean)(
        rs: Vector[Int]
    ) = rs.collectFirst {
      case r if bad(byRecord(r)) =>
        InventoryError.DispositionMismatch(t.render, label, r, outcome(byRecord(r)))
    }
    def admitted(d: Disposition[K]) = d match
      case Disposition.Admitted(_, _) => true
      case _                          => false
    def quarantinedAs(d: Disposition[K]) = d match
      case Disposition.Rejected(_, _, AdmissionReason.Quarantined(_, c)) => Some(c)
      case _                                                             => None
    def dispositions = value.trials.iterator
      .map { t =>
        val label = t.disposition.label
        val check = mismatch(t.identity, label)
        t.disposition match
          case TrialDisposition.Absent      => None
          case TrialDisposition.NoFixations =>
            check(d => admitted(d) || quarantinedAs(d).isDefined)(t.records)
          case TrialDisposition.Admitted =>
            check(d => !admitted(d))(t.records).orElse(
              Option
                .when(t.item.isEmpty)(t.records.headOption)
                .flatten
                .map(r =>
                  InventoryError
                    .DispositionMismatch(t.identity.render, label, r, "unresolved item")
                )
            )
          case TrialDisposition.Quarantined(cause) =>
            val conflict = t.itemConflict
              .filter(_ != cause)
              .map(c =>
                InventoryError.DispositionMismatch(
                  t.identity.render,
                  label,
                  t.records.head,
                  s"an item conflict (${c.productPrefix})"
                )
              )
            val carries = cause match
              case QuarantineCause.NotInInventory(_, _, _, _) =>
                t.records.headOption.map(r =>
                  InventoryError
                    .DispositionMismatch(t.identity.render, label, r, outcome(byRecord(r)))
                )
              case _ =>
                Option.when(!t.records.exists(r => quarantinedAs(byRecord(r)).contains(cause)))(
                  InventoryError.DispositionMismatch(
                    t.identity.render,
                    label,
                    t.records.head,
                    outcome(byRecord(t.records.head))
                  )
                )
            check(admitted)(t.records).orElse(conflict).orElse(carries)
      }
      .collectFirst { case Some(e) => e }
    // Every keyed record of a trial carries the item the trial's records are
    // keyed under: its resolved item, or under a conflict the inventory's.
    def items = value.trials.iterator
      .flatMap(t =>
        t.records.iterator.flatMap(r =>
          keyOf(byRecord(r))
            .map(item)
            .filterNot(t.keyItem.contains)
            .map(actual =>
              InventoryError.ItemMismatch(t.identity.render, r, t.keyItem.getOrElse(""), actual)
            )
        )
      )
      .nextOption()
    def unlisted = value.unlisted.iterator
      .map { u =>
        val label = "not in the inventory"
        mismatch(u.identity, label)(d =>
          admitted(d) || quarantinedAs(d).exists {
            case QuarantineCause.NotInInventory(p, f, t, o) =>
              (p, f, t, o) != (
                u.identity.participant,
                u.identity.phase,
                u.identity.trial,
                u.identity.occurrence.value
              )
            case _ => true
          }
        )(u.records)
      }
      .collectFirst { case Some(e) => e }
    def attributes = value.recordAttributes.collectFirst {
      case entry if !byRecord.get(entry.record).exists(admitted) =>
        InventoryError.AttributeRecord(entry.record)
    }
    listed
      .orElse(claimed)
      .orElse(dispositions)
      .orElse(items)
      .orElse(unlisted)
      .orElse(attributes)
      .map(AdmissionError.Inventory.apply)
      .toLeft(copy(inventory = Some(value)))

  /** Every admitted record must lie in a trial at most one correction rule
    * covers, under the layout's participant projection.
    */
  def checkCorrections(participant: K => String): Either[AdmissionError, Unit] =
    records.iterator
      .collect { case SourceRecord(r, Disposition.Admitted(k, _)) => r -> k }
      .map { case (r, k) =>
        policy.correctionFor(k, participant).left.map { case (a, b) =>
          AdmissionError.CorrectionConflict(r, a, b)
        }
      }
      .collectFirst { case Left(e) => e }
      .toLeft(())

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
  /** A version-1 ledger: no corrections, and off-screen records quarantine. */
  def of[K](
      source: SourceRef,
      header: Vector[String],
      records: Vector[SourceRecord[K]],
      outcome: AdmissionOutcome
  ): Either[AdmissionError, AdmissionLedger[K]] =
    of(source, header, records, outcome, AdmissionPolicy.version1[K], Vector.empty)

  /** Every invariant reports the first offending record in record order.
    * Records listed as admitted outside the frame must be admitted, strictly
    * increasing and only present under [[OffScreenPolicy.ExcludeRecord]].
    */
  def of[K](
      source: SourceRef,
      header: Vector[String],
      records: Vector[SourceRecord[K]],
      outcome: AdmissionOutcome,
      policy: AdmissionPolicy[K],
      outsideFrame: Vector[OutsideFrame]
  ): Either[AdmissionError, AdmissionLedger[K]] =
    // The inventory's causes need the inventory that explains them.
    records
      .collectFirst {
        case SourceRecord(r, Disposition.Rejected(_, _, AdmissionReason.Quarantined(_, cause)))
            if QuarantineCause.version(cause) >= 3 =>
          AdmissionError.UninventoriedCause(r, cause)
      }
      .toLeft(())
      .flatMap(_ => build(source, header, records, outcome, policy, outsideFrame))

  /** A ledger joined to its trial inventory: the invariants of [[of]], then
    * those of `withInventory`. Only such a ledger may quarantine a record as
    * not in the inventory or in conflict with the inventory's item.
    */
  def inventoried[K](
      source: SourceRef,
      header: Vector[String],
      records: Vector[SourceRecord[K]],
      outcome: AdmissionOutcome,
      policy: AdmissionPolicy[K],
      outsideFrame: Vector[OutsideFrame],
      inventory: InventoryLedger,
      identity: K => TrialIdentity,
      item: K => String
  ): Either[AdmissionError, AdmissionLedger[K]] =
    build(source, header, records, outcome, policy, outsideFrame)
      .flatMap(_.withInventory(inventory, identity, item))

  /** As [[decide]], joined to a trial inventory (see [[inventoried]]). */
  def decide[K](
      source: SourceRef,
      header: Vector[String],
      records: Vector[SourceRecord[K]],
      decision: AdmissionDecision,
      policy: AdmissionPolicy[K],
      outsideFrame: Vector[OutsideFrame],
      inventory: InventoryLedger,
      identity: K => TrialIdentity,
      item: K => String
  ): Either[AdmissionError, AdmissionLedger[K]] =
    inventoried(
      source,
      header,
      records,
      outcome(records, decision),
      policy,
      outsideFrame,
      inventory,
      identity,
      item
    )

  private def outcome[K](
      records: Vector[SourceRecord[K]],
      decision: AdmissionDecision
  ): AdmissionOutcome =
    if records.forall(_.isAdmitted) then AdmissionOutcome.Complete
    else
      decision match
        case AdmissionDecision.RequireComplete  => AdmissionOutcome.Refused
        case AdmissionDecision.ReviewExclusions => AdmissionOutcome.ReviewedExclusions

  private def build[K](
      source: SourceRef,
      header: Vector[String],
      records: Vector[SourceRecord[K]],
      outcome: AdmissionOutcome,
      policy: AdmissionPolicy[K],
      outsideFrame: Vector[OutsideFrame]
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
    def outside = outsideFrame.zipWithIndex.collectFirst {
      case (entry, i)
          if policy.offScreen != OffScreenPolicy.ExcludeRecord ||
            !admittedNumbers.contains(entry.record) ||
            (i > 0 && entry.record <= outsideFrame(i - 1).record) =>
        AdmissionError.OutsideFrameRecord(entry.record, policy.offScreen)
    }
    ordered
      .orElse(ordinals)
      .orElse(scopes)
      .orElse(consistent)
      .orElse(outside)
      .toLeft(new AdmissionLedger(source, header, records, outcome, policy, outsideFrame, None))

  /** Derive the outcome from the analyst's decision and the rejected count. */
  def decide[K](
      source: SourceRef,
      header: Vector[String],
      records: Vector[SourceRecord[K]],
      decision: AdmissionDecision
  ): Either[AdmissionError, AdmissionLedger[K]] =
    decide(source, header, records, decision, AdmissionPolicy.version1[K], Vector.empty)

  /** As [[decide]], recording the admission policy and the admitted records
    * outside the frame.
    */
  def decide[K](
      source: SourceRef,
      header: Vector[String],
      records: Vector[SourceRecord[K]],
      decision: AdmissionDecision,
      policy: AdmissionPolicy[K],
      outsideFrame: Vector[OutsideFrame]
  ): Either[AdmissionError, AdmissionLedger[K]] =
    of(source, header, records, outcome(records, decision), policy, outsideFrame)

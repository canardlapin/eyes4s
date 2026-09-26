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

import eyes4s.design.*

/** A trial's identity without the item it is matched on: participant, phase,
  * trial label and occurrence. It is what a trial inventory declares and what
  * fixation records are joined on; the item is resolved separately (see
  * [[InventoryTrial.item]]).
  */
final case class TrialIdentity private (
    participant: String,
    phase: String,
    trial: String,
    occurrence: TrialOccurrence
) derives CanEqual:
  /** The trial key of this identity matched on `item`. */
  def withItem(item: String): Either[PlanError, TrialKey] =
    TrialKey.of(participant, phase, trial, occurrence, item)

  /** `participant/phase/trial#occurrence`, for messages. */
  def render: String = s"$participant/$phase/$trial#${occurrence.value}"

object TrialIdentity:
  def of(
      participant: String,
      phase: String,
      trial: String,
      occurrence: TrialOccurrence
  ): Either[PlanError, TrialIdentity] =
    Vector("participant" -> participant, "phase" -> phase, "trial" -> trial)
      .collectFirst {
        case (field, value) if value.trim.isEmpty => PlanError.BlankKeyField(field)
      }
      .toLeft(new TrialIdentity(participant, phase, trial, occurrence))

  /** The identity of a trial key: everything but its item. */
  def of(key: TrialKey): TrialIdentity =
    new TrialIdentity(key.participant, key.phase, key.trial, key.occurrence)

  /** The identity a layout projects from its keys, when it names a trial
    * label; a layout without an occurrence projection means occurrence 1.
    */
  def projection[K](layout: StudyLayout[K]): Option[K => TrialIdentity] =
    layout.trial.map(trial =>
      key =>
        new TrialIdentity(
          layout.participant(key),
          layout.phase(key),
          trial(key),
          layout.occurrence.fold(TrialOccurrence.first)(_(key))
        )
    )

  given KeyDigest[TrialIdentity] = KeyDigest.derived[TrialIdentity]
  given Ordering[TrialIdentity]  =
    Ordering.by(k => (k.participant, k.phase, k.trial, k.occurrence.value))

/** The declared type of an attribute column. */
enum AttributeKind derives CanEqual:
  /** Any text, kept as written. */
  case Text

  /** A signed 64-bit integer written in decimal. */
  case Integer

  /** A finite decimal number. */
  case Number

/** A column the caller declared as an attribute, by name and type. Only
  * declared columns become attributes; no other column of a table is read.
  */
final case class AttributeColumn(name: String, kind: AttributeKind) derives CanEqual:
  /** An empty cell is [[AttributeValue.Blank]] whatever the kind; any other
    * cell must parse as the kind, or the requirement it fails is returned.
    */
  def parse(raw: String): Either[String, AttributeValue] =
    if raw.isEmpty then Right(AttributeValue.Blank)
    else
      kind match
        case AttributeKind.Text    => Right(AttributeValue.Text(raw))
        case AttributeKind.Integer =>
          raw.toLongOption.map(AttributeValue.Integer.apply).toRight("a decimal integer")
        case AttributeKind.Number =>
          raw.toDoubleOption
            .filter(_.isFinite)
            .map(AttributeValue.Number.apply)
            .toRight("a finite number")

/** One typed attribute value. */
enum AttributeValue derives CanEqual:
  case Text(value: String)
  case Integer(value: Long)
  case Number(value: Double)

  /** The cell was empty. */
  case Blank

/** Named attribute values in declaration order; names are distinct. */
final case class Attributes private (entries: Vector[(String, AttributeValue)])
    derives CanEqual:
  def names: Vector[String]                     = entries.map(_._1)
  def get(name: String): Option[AttributeValue] = entries.collectFirst { case (`name`, value) =>
    value
  }
  def isEmpty: Boolean = entries.isEmpty

object Attributes:
  val empty: Attributes = new Attributes(Vector.empty)

  def of(entries: Vector[(String, AttributeValue)]): Either[InventoryError, Attributes] =
    val names = entries.map(_._1)
    val twice = names.diff(names.distinct).distinct
    if twice.nonEmpty then Left(InventoryError.DuplicateAttribute(twice))
    else Right(new Attributes(entries))

/** What admission did with one inventory trial. Exactly one applies. */
enum TrialDisposition derives CanEqual:
  /** Its records built its scanpath; every one of them was admitted. */
  case Admitted

  /** It has admissible records but was quarantined as a whole. */
  case Quarantined(cause: QuarantineCause)

  /** It has records and every one was rejected on its own; this takes
    * precedence over `Quarantined(RejectedRecords)`.
    */
  case NoFixations

  /** It has no fixation records at all. */
  case Absent

  def label: String = this match
    case Admitted       => "admitted"
    case Quarantined(c) => s"quarantined (${c.productPrefix})"
    case NoFixations    => "no fixations"
    case Absent         => "absent"

/** One trial of a declared inventory: the inventory records that declare it,
  * its item and attributes there, the items its fixation records name, those
  * records and its disposition.
  *
  * The item is resolved from the inventory and the records together: the
  * inventory's item when it declares one, otherwise the one item the records
  * name. Records that name different items, or an item other than the
  * inventory's, are a conflict (see [[itemConflict]]).
  */
final case class InventoryTrial(
    identity: TrialIdentity,
    rows: Vector[Int],
    inventoryItem: Option[String],
    attributes: Attributes,
    recordItems: Vector[String],
    records: Vector[Int],
    disposition: TrialDisposition
) derives CanEqual:
  def itemConflict: Option[QuarantineCause] =
    if recordItems.size > 1 then Some(QuarantineCause.ItemConflict(recordItems))
    else
      inventoryItem.flatMap(expected =>
        Option.when(recordItems.exists(_ != expected))(
          QuarantineCause.InventoryItemConflict(expected, recordItems)
        )
      )

  /** The resolved item, when there is exactly one and no conflict. */
  def item: Option[String] =
    if itemConflict.isDefined then None else inventoryItem.orElse(recordItems.headOption)

/** Fixation records of a trial the inventory does not declare. They are
  * never admitted; each admissible one is quarantined with
  * `QuarantineCause.NotInInventory`.
  */
final case class UnlistedTrial(
    identity: TrialIdentity,
    recordItems: Vector[String],
    records: Vector[Int]
) derives CanEqual

/** The declared record attributes of one admitted fixation record. */
final case class RecordAttributes(record: Int, attributes: Attributes) derives CanEqual

/** The trial inventory half of an admission ledger: the inventory source,
  * every inventory trial with exactly one disposition, the trials that only
  * the fixation table names, and the declared attributes of admitted records.
  * Fixation record numbers are those of the ledger's own records; inventory
  * record numbers (`rows`) count the inventory's header as record 1.
  */
final case class InventoryLedger private (
    source: SourceRef,
    header: Vector[String],
    trials: Vector[InventoryTrial],
    unlisted: Vector[UnlistedTrial],
    recordAttributes: Vector[RecordAttributes]
) derives CanEqual:
  def trial(identity: TrialIdentity): Option[InventoryTrial] =
    trials.find(_.identity == identity)

  /** The inventory attributes of an admitted trial key's trial. */
  def attributes(key: TrialKey): Option[Attributes] =
    trial(TrialIdentity.of(key)).map(_.attributes)

  def admitted: Vector[InventoryTrial] =
    trials.filter(_.disposition == TrialDisposition.Admitted)
  def absent: Vector[InventoryTrial] = trials.filter(_.disposition == TrialDisposition.Absent)

  /** Quarantined trials, including those with no admissible fixation. */
  def quarantined: Vector[InventoryTrial] = trials.filter(t =>
    t.disposition match
      case TrialDisposition.Quarantined(_) | TrialDisposition.NoFixations => true
      case _                                                              => false
  )

object InventoryLedger:
  /** Identities are distinct, no fixation record belongs to two trials,
    * record lists are strictly increasing, a trial is absent exactly when it
    * has no records, and record attributes are listed once, in record order.
    */
  def of(
      source: SourceRef,
      header: Vector[String],
      trials: Vector[InventoryTrial],
      unlisted: Vector[UnlistedTrial],
      recordAttributes: Vector[RecordAttributes]
  ): Either[InventoryError, InventoryLedger] =
    val identities = trials.map(_.identity) ++ unlisted.map(_.identity)
    val lists      = trials.map(t => t.identity -> t.records) ++
      unlisted.map(u => u.identity -> u.records)
    def duplicate = identities.diff(identities.distinct).headOption.map { id =>
      InventoryError.DuplicateTrial(id.participant, id.phase, id.trial, id.occurrence.value)
    }
    def ordered = lists.collectFirst {
      case (id, records) if records.zip(records.drop(1)).exists((a, b) => a >= b) =>
        InventoryError.RecordOrder(id.render, records)
    }
    def shared =
      lists
        .flatMap((id, records) => records.map(_ -> id.render))
        .groupMap(_._1)(_._2)
        .toVector
        .sortBy(_._1)
        .collectFirst {
          case (record, owners) if owners.size > 1 =>
            InventoryError.SharedRecord(record, owners)
        }
    def absent = trials.collectFirst {
      case t if (t.disposition == TrialDisposition.Absent) != t.records.isEmpty =>
        InventoryError.AbsentMismatch(t.identity.render, t.disposition.label, t.records)
    }
    def attributes = recordAttributes.zipWithIndex.collectFirst {
      case (entry, i) if i > 0 && entry.record <= recordAttributes(i - 1).record =>
        InventoryError.AttributeRecord(entry.record)
    }
    duplicate
      .orElse(ordered)
      .orElse(shared)
      .orElse(absent)
      .orElse(attributes)
      .toLeft(new InventoryLedger(source, header, trials, unlisted, recordAttributes))

/** Why a trial inventory, or its ledger, was refused. Every case names the
  * records, trials or columns it concerns.
  */
enum InventoryError derives CanEqual:
  /** An inventory record has another number of fields than its header. */
  case Width(record: Int, expected: Int, actual: Int)

  /** An inventory record's declared field does not meet its requirement. */
  case Field(record: Int, column: String, value: String, requirement: String)

  /** Inventory records declare one trial with different values. */
  case Conflict(
      participant: String,
      phase: String,
      trial: String,
      records: Vector[Int],
      columns: Vector[String]
  )

  /** Attribute names repeat. */
  case DuplicateAttribute(names: Vector[String])

  /** A ledger lists one trial identity twice. */
  case DuplicateTrial(participant: String, phase: String, trial: String, occurrence: Int)

  /** A trial's fixation records are not strictly increasing. */
  case RecordOrder(trial: String, records: Vector[Int])

  /** One fixation record is listed under more than one trial. */
  case SharedRecord(record: Int, trials: Vector[String])

  /** A trial is absent but lists records, or lists none but is not absent. */
  case AbsentMismatch(trial: String, disposition: String, records: Vector[Int])

  /** Record attributes are listed twice or out of record order, or for a
    * record that was not admitted.
    */
  case AttributeRecord(record: Int)

  /** A trial lists a fixation record the admission ledger does not have. */
  case UnknownRecord(trial: String, record: Int)

  /** A trial lists a fixation record keyed to another trial. */
  case ForeignRecord(trial: String, record: Int, found: String)

  /** A keyed fixation record is not listed under its trial. */
  case UnclaimedRecord(record: Int, trial: String)

  /** A record's disposition contradicts its trial's. */
  case DispositionMismatch(trial: String, disposition: String, record: Int, found: String)

  /** An admitted record's item is not its trial's resolved item. */
  case ItemMismatch(trial: String, record: Int, expected: String, actual: String)

  /** The key layout names no trial label, so no identity can be joined. */
  case NoTrialProjection(layout: DefinitionId)

  def message: String = this match
    case Width(r, e, a)            => s"Inventory record $r has $a fields; the header has $e."
    case Field(r, c, v, q)         => s"Inventory record $r, column '$c' has '$v'; expected $q."
    case Conflict(p, f, t, rs, cs) =>
      s"Inventory records $rs declare trial $p/$f/$t with different values in columns $cs."
    case DuplicateAttribute(names)  => s"Attribute names $names are declared more than once."
    case DuplicateTrial(p, f, t, o) => s"Trial $p/$f/$t#$o is listed more than once."
    case RecordOrder(t, rs) => s"Trial $t lists records $rs, which are not strictly increasing."
    case SharedRecord(r, ts)      => s"Record $r is listed under trials $ts."
    case AbsentMismatch(t, d, rs) =>
      s"Trial $t is $d with records $rs; a trial is absent exactly when it has none."
    case AttributeRecord(r) =>
      s"Attributes of record $r are repeated, out of record order, or of a record not admitted."
    case UnknownRecord(t, r) => s"Trial $t lists record $r, which the admission ledger lacks."
    case ForeignRecord(t, r, found) => s"Trial $t lists record $r, which is keyed to $found."
    case UnclaimedRecord(r, t) => s"Record $r is keyed to trial $t but not listed under it."
    case DispositionMismatch(t, d, r, found) =>
      s"Trial $t is $d, but its record $r is $found."
    case ItemMismatch(t, r, e, a) =>
      s"Record $r of trial $t is admitted with item '$a'; the trial's item is '$e'."
    case NoTrialProjection(layout) =>
      s"Layout ${layout.name}@${layout.version} names no trial label to join an inventory on."

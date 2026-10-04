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

/** One parsed source record considered for trial-identity consistency.
  *
  * An application parses an occurrence with [[TrialOccurrence.of]], builds its
  * [[TrialIdentity]], and then calls [[of]]. Thus a Studio import builder can
  * use the same typed boundary as `eyes4s-io`; raw occurrence text and invalid
  * record positions cannot reach the grouping.
  */
final case class TrialConflictRow private (key: TrialKey, record: CsvRecord) derives CanEqual:
  def identity: TrialIdentity = TrialIdentity.of(key)

object TrialConflictRow:
  /** Bind a parsed identity, its item attribute and a physical CSV record. */
  def of(
      identity: TrialIdentity,
      item: String,
      record: CsvRecord
  ): Either[PlanError, TrialConflictRow] =
    identity.withItem(item).map(new TrialConflictRow(_, record))

  /** Bind values that have already passed the trial-key and CSV-record
    * constructors. This is the importer adapter: it has no failure path.
    */
  def from(key: TrialKey, record: CsvRecord): TrialConflictRow =
    new TrialConflictRow(key, record)

  /** Internal adapter for a parser's positive CSV-record ordinal. The CSV
    * parser creates ordinals as its zero-based row index plus two.
    */
  private[eyes4s] def fromParsed(key: TrialKey, record: Int): TrialConflictRow =
    new TrialConflictRow(key, new CsvRecord(record))

/** The consistency status of all records with one participant, phase and trial label. */
enum TrialConflictClassification derives CanEqual:
  case Ok
  case ItemConflict(items: Vector[String])
  case OccurrenceConflict(occurrences: Vector[TrialOccurrence])

/** One canonical trial-label group. `representative` is the least complete
  * [[TrialKey]] and records are in increasing physical-record order.
  */
final case class TrialConflictGroup private[plan] (
    representative: TrialKey,
    keys: Vector[TrialKey],
    records: Vector[CsvRecord],
    classification: TrialConflictClassification
) derives CanEqual

/** Incrementally groups parsed trial identities for the same conflict policy
  * used by fixation admission. The state keeps one entry per participant,
  * phase and trial label, with the distinct keys and record positions that the
  * completed result must report; it does not retain source rows.
  */
final class TrialConflictGrouping private (
    private val states: Map[(String, String, String), TrialConflictGrouping.State]
):
  /** Add one already parsed record. */
  def add(row: TrialConflictRow): TrialConflictGrouping =
    val label = (row.key.participant, row.key.phase, row.key.trial)
    val next  = states.getOrElse(label, TrialConflictGrouping.State.empty).add(row)
    new TrialConflictGrouping(states.updated(label, next))

  /** Every trial-label group in canonical representative-key order. */
  def groups: Vector[TrialConflictGroup] =
    states.valuesIterator.map(_.result).toVector.sortBy(_.representative)

object TrialConflictGrouping:
  private final case class State(recordsByKey: Map[TrialKey, Vector[CsvRecord]]):
    def add(row: TrialConflictRow): State =
      val records = recordsByKey.getOrElse(row.key, Vector.empty) :+ row.record
      State(recordsByKey.updated(row.key, records))

    def result: TrialConflictGroup =
      val keys        = recordsByKey.keysIterator.toVector.sorted
      val items       = keys.map(_.item).distinct.sorted
      val occurrences = keys.map(_.occurrence).distinct.sortBy(_.value)
      val status      =
        if keys.size == 1 then TrialConflictClassification.Ok
        else if items.size > 1 then TrialConflictClassification.ItemConflict(items)
        else TrialConflictClassification.OccurrenceConflict(occurrences)
      new TrialConflictGroup(
        keys.head,
        keys,
        recordsByKey.valuesIterator.flatten.toVector.sortBy(_.value),
        status
      )

  private object State:
    val empty: State = State(Map.empty)

  val empty: TrialConflictGrouping = new TrialConflictGrouping(Map.empty)

  /** Fold a row stream without materializing the input. */
  def group(rows: IterableOnce[TrialConflictRow]): Vector[TrialConflictGroup] =
    rows.iterator.foldLeft(empty)((state, row) => state.add(row)).groups

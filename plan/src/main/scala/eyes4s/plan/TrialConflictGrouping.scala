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
  * `R` names the record: a [[CsvRecord]] for a Studio import builder, or an
  * importer's own row identity. The grouping only orders records.
  *
  * An application parses an occurrence with [[TrialOccurrence.of]], builds its
  * [[TrialIdentity]], and then calls [[of]]. Thus a Studio import builder can
  * use the same typed boundary as `eyes4s-io`; raw occurrence text and invalid
  * record positions cannot reach the grouping.
  */
final case class TrialConflictRow[R] private (key: TrialKey, record: R) derives CanEqual:
  def identity: TrialIdentity = TrialIdentity.of(key)

object TrialConflictRow:
  /** Bind a parsed identity, its item attribute and a physical CSV record. */
  def of(
      identity: TrialIdentity,
      item: String,
      record: CsvRecord
  ): Either[PlanError, TrialConflictRow[CsvRecord]] =
    identity.withItem(item).map(new TrialConflictRow(_, record))

  /** Bind a validated trial key to a record the caller already identifies:
    * a [[CsvRecord]], or an importer's own row identity. It has no failure
    * path because neither value can be invalid here.
    */
  def from[R](key: TrialKey, record: R): TrialConflictRow[R] =
    new TrialConflictRow(key, record)

/** The consistency status of all records with one participant, phase and trial label. */
enum TrialConflictClassification derives CanEqual:
  case Ok
  case ItemConflict(items: Vector[String])
  case OccurrenceConflict(occurrences: Vector[TrialOccurrence])

/** One canonical trial-label group. `representative` is the least complete
  * [[TrialKey]] and records are in increasing order.
  */
final case class TrialConflictGroup[R] private[plan] (
    representative: TrialKey,
    keys: Vector[TrialKey],
    records: Vector[R],
    classification: TrialConflictClassification
) derives CanEqual

/** Incrementally groups parsed trial identities for the same conflict policy
  * used by fixation admission.
  *
  * The state holds one entry per participant, phase and trial label: its
  * distinct keys and its record positions, which the completed result must
  * report. It never holds source rows or field text beyond the keys, so a
  * label's state grows only with its own record count. A record of a label
  * that is consistent so far must still be kept, since a later record can
  * turn the label into a conflict that quarantines every one of them.
  */
final class TrialConflictGrouping[R: Ordering] private (
    private val states: Map[(String, String, String), TrialConflictGrouping.State[R]]
):
  /** Add one already parsed record. */
  def add(row: TrialConflictRow[R]): TrialConflictGrouping[R] =
    val label = (row.key.participant, row.key.phase, row.key.trial)
    val next  = states.getOrElse(label, TrialConflictGrouping.State.empty[R]).add(row)
    new TrialConflictGrouping(states.updated(label, next))

  /** Every trial-label group in canonical representative-key order. */
  def groups: Vector[TrialConflictGroup[R]] =
    states.valuesIterator.flatMap(_.result).toVector.sortBy(_.representative)

object TrialConflictGrouping:
  private final case class State[R](recordsByKey: Map[TrialKey, Vector[R]]):
    def add(row: TrialConflictRow[R]): State[R] =
      val records = recordsByKey.getOrElse(row.key, Vector.empty) :+ row.record
      State(recordsByKey.updated(row.key, records))

    // A state exists only once a row was added, so it has a least key.
    def result(using Ordering[R]): Option[TrialConflictGroup[R]] =
      val keys = recordsByKey.keysIterator.toVector.sorted
      keys.headOption.map { representative =>
        val items       = keys.map(_.item).distinct.sorted
        val occurrences = keys.map(_.occurrence).distinct.sortBy(_.value)
        val status      =
          if keys.size == 1 then TrialConflictClassification.Ok
          else if items.size > 1 then TrialConflictClassification.ItemConflict(items)
          else TrialConflictClassification.OccurrenceConflict(occurrences)
        new TrialConflictGroup(
          representative,
          keys,
          recordsByKey.valuesIterator.flatten.toVector.sorted,
          status
        )
      }

  private object State:
    def empty[R]: State[R] = State(Map.empty)

  def empty[R: Ordering]: TrialConflictGrouping[R] = new TrialConflictGrouping[R](Map.empty)

  /** Fold a row stream without materializing the input. */
  def group[R: Ordering](
      rows: IterableOnce[TrialConflictRow[R]]
  ): Vector[TrialConflictGroup[R]] =
    rows.iterator.foldLeft(empty[R])((state, row) => state.add(row)).groups

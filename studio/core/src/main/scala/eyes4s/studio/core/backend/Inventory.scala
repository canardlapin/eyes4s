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

import eyes4s.plan.InventoryError
import io.circe.Codec

/** What admission knows of a dataset's trial inventory (ticket S5.4, UI-H).
  * eyes4s joins fixation records to the inventory's trials by label
  * (participant, phase and trial); every inventory trial then has exactly one
  * disposition, and a trial with no records at all is absent. Duplicate
  * inventory records with equal values declare one trial, so they never
  * multiply its fixations.
  */
enum InventoryJoin derives CanEqual, Codec.AsObject:
  /** The dataset declares an inventory of `trials` trials, `absent` of which
    * have no fixation records.
    */
  case Joined(trials: Int, absent: Int)

  /** The dataset declares no trial inventory: absent trials cannot be
    * counted, since nothing lists the trials that have no records.
    */
  case Undeclared

  def trialCount: Option[Int] = this match
    case Joined(n, _) => Some(n)
    case Undeclared   => None

  def absentCount: Option[Int] = this match
    case Joined(_, n) => Some(n)
    case Undeclared   => None

/** A trial's label, as eyes4s joins an inventory on it. */
final case class TrialLabel(participant: String, phase: String, trial: String)
    derives CanEqual,
      Codec.AsObject:
  def render: String = s"$participant/$phase/$trial"

/** Why eyes4s refused a trial inventory (`InventoryError`), every case with
  * its operands. Inventory record numbers count the header as record 1, as
  * eyes4s numbers them. A refusal this build does not map arrives as
  * [[Other]] with its eyes4s case name and message.
  */
enum InventoryIssue derives CanEqual, Codec.AsObject:
  /** Record `record` has `actual` fields; the header has `expected`. */
  case Width(record: Int, expected: Int, actual: Int)

  /** Record `record`'s `column` holds `value`, which is not `requirement`. */
  case Field(record: Int, column: String, value: String, requirement: String)

  /** Inventory `records` declare `trial` with different values in `columns`:
    * a trial-level value must be the same on every record that declares it.
    */
  case Conflict(trial: TrialLabel, records: Vector[Int], columns: Vector[String])

  case Other(kind: String, text: String)

  def code: String = this match
    case Width(_, _, _)    => "studio-inventory.width"
    case Field(_, _, _, _) => "studio-inventory.field"
    case Conflict(_, _, _) => "studio-inventory.conflict"
    case Other(kind, _)    => s"studio-inventory.other.$kind"

  def message: String = this match
    case Width(r, e, a) => s"trials.csv record $r has $a fields; the header has $e."
    case Field(r, column, value, need) =>
      s"trials.csv record $r, column $column holds '$value'; eyes4s requires $need."
    case Conflict(trial, records, columns) =>
      s"Trial ${trial.render} has conflicting values in ${columns.mkString(", ")}: " +
        s"trials.csv records ${records.mkString(", ")} declare it differently."
    case Other(_, text) => text

  /** The column the issue points at first, when it names one. */
  def pointsAt: Option[String] = this match
    case Conflict(_, _, columns) => columns.headOption
    case Field(_, column, _, _)  => Some(column)
    case _                       => None

  /** The loci the issue names: its records and columns. */
  def loci: Vector[DiagnosticLocus] = this match
    case Width(r, _, _)         => Vector(DiagnosticLocus.Record(r))
    case Field(r, column, _, _) =>
      Vector(DiagnosticLocus.Record(r), DiagnosticLocus.Field(column))
    case Conflict(_, rs, columns) =>
      DiagnosticLocus.Records(rs) +: columns.map(DiagnosticLocus.Field(_))
    case Other(_, _) => Vector.empty

object InventoryIssue:
  /** eyes4s's refusal, with its operands. */
  def of(error: InventoryError): InventoryIssue = error match
    case InventoryError.Width(r, e, a)            => Width(r, e, a)
    case InventoryError.Field(r, c, v, q)         => Field(r, c, v, q)
    case InventoryError.Conflict(p, f, t, rs, cs) => Conflict(TrialLabel(p, f, t), rs, cs)
    case other                                    => Other(other.productPrefix, other.message)

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

package eyes4s.studio.core.importing

import eyes4s.plan.{TrialIdentity, TrialOccurrence}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.document.{ColumnName, ColumnRole, SourceRole}

import scala.collection.mutable

// ---------------------------------------------------------------------------
// The key's composition
// ---------------------------------------------------------------------------

/** A part of the trial key: eyes4s `TrialKey` less the item it is matched on
  * (the item is an attribute, not identity).
  */
enum KeyPart derives CanEqual:
  case Participant, Phase, Trial, Occurrence

  def role: ColumnRole = this match
    case Participant => ColumnRole.Participant
    case Phase       => ColumnRole.Phase
    case Trial       => ColumnRole.Trial
    case Occurrence  => ColumnRole.Occurrence

object KeyPart:
  /** The parts eyes4s reads a trial key from (`FixationKeyReader.trial`,
    * `TrialColumns`). The occurrence is optional: 1 when no column is named.
    */
  val required: Vector[KeyPart] = Vector(Participant, Phase, Trial)

/** The columns a file's trial key is composed of: the columns mapped to the
  * participant, phase, trial and occurrence roles. The composition is the
  * column mapping itself; it is not recorded anywhere else.
  */
final case class KeyColumns(
    participant: Option[ColumnName],
    phase: Option[ColumnName],
    trial: Option[ColumnName],
    occurrence: Option[ColumnName]
) derives CanEqual:
  def column(part: KeyPart): Option[ColumnName] = part match
    case KeyPart.Participant => participant
    case KeyPart.Phase       => phase
    case KeyPart.Trial       => trial
    case KeyPart.Occurrence  => occurrence

  /** Required parts no column fills, in key order. */
  def missing: Vector[KeyPart] = KeyPart.required.filter(column(_).isEmpty)

object KeyColumns:
  /** The key of a mapping's choices: the first column of each key role (a
    * role on two columns is the mapping's own issue).
    */
  def of(columns: Vector[(ColumnName, ColumnChoice)]): KeyColumns =
    def first(part: KeyPart) =
      columns.collectFirst { case (c, ColumnChoice.Role(r)) if r == part.role => c }
    KeyColumns(
      first(KeyPart.Participant),
      first(KeyPart.Phase),
      first(KeyPart.Trial),
      first(KeyPart.Occurrence)
    )

  /** The column that holds the occurrence when the key leaves it out: the
    * first column without a role whose name suggests the occurrence role
    * (occurrence, block, repeat, repetition). None when the key has one.
    */
  def occurrenceCandidate(columns: Vector[(ColumnName, ColumnChoice)]): Option[ColumnName] =
    if columns.exists(_._2 == ColumnChoice.Role(ColumnRole.Occurrence)) then None
    else
      columns.collectFirst {
        case (c, ColumnChoice.Attribute)
            if RoleProposals.propose(Vector(c)) ==
              Vector(ColumnChoice.Role(ColumnRole.Occurrence)) =>
          c
      }

// ---------------------------------------------------------------------------
// The check
// ---------------------------------------------------------------------------

/** What makes one trial among the records of one key. */
enum TrialUnit derives CanEqual:
  /** Each record is one trial: a trial inventory lists one trial a record. */
  case Record

  /** Records with one value of `column` are one trial: fixation records of
    * one presentation share their occurrence.
    */
  case Occurrence(column: ColumnName)

  /** A key's records are one trial: nothing tells presentations apart. */
  case Key

/** A trial's label: participant, phase and trial. eyes4s identifies a trial
  * by its label; the occurrence is a value every record of the trial must
  * agree with (`TrialInventory`, and `QuarantineCause.OccurrenceConflict` in
  * `FixationCsv`).
  */
final case class KeyLabel(participant: String, phase: String, trial: String) derives CanEqual:
  def render: String = s"$participant · $phase · $trial"

object KeyLabel:
  /** eyes4s `TrialIdentity` order: participant, phase, trial. */
  given Ordering[KeyLabel] = Ordering.by(l => (l.participant, l.phase, l.trial))

/** One trial of a repeated key: the occurrence its records name (the value
  * as written, when the unit reads one) and its records, ascending.
  */
final case class KeyTrial(occurrence: Option[String], records: Vector[Int]) derives CanEqual

/** A key more than one trial resolves to, with every one of those trials in
  * first-record order. None is preferred: a repeated key is never resolved
  * by taking its first match.
  */
final case class RepeatedKey(key: KeyLabel, trials: Vector[KeyTrial]) derives CanEqual:
  /** The studio key eyes4s gives each record under `columns`: the label and
    * the record's occurrence when the key has an occurrence column, else 1.
    */
  def trialKey(trial: KeyTrial, keyHasOccurrence: Boolean): TrialKey =
    val occurrence =
      if keyHasOccurrence then trial.occurrence.flatMap(_.trim.toIntOption).getOrElse(1)
      else TrialOccurrence.first.value
    TrialKey(key.participant, Phase(key.phase), key.trial, occurrence)

/** A record whose key does not resolve: `column`'s `value` is not what eyes4s
  * requires (`requirement`).
  */
final case class UnresolvedRecord(
    record: Int,
    column: ColumnName,
    value: String,
    requirement: String
) derives CanEqual:
  def message: String =
    s"record $record: ${column.value} is '$value', not $requirement"

/** Why a file's key cannot be checked. Each case names its operands. */
enum KeyGap derives CanEqual:
  /** No column fills the required `parts`. */
  case Missing(file: String, parts: Vector[KeyPart])

  /** `column` fills `part` but is not in the file (a mapping defect). */
  case Absent(file: String, part: KeyPart, column: ColumnName)

  def message: String = this match
    case Missing(file, parts) =>
      val names = parts.map(_.toString.toLowerCase).mkString(", ")
      s"$file: the trial key has no $names column; eyes4s reads a trial key from " +
        "participant, phase and trial columns."
    case Absent(file, part, column) =>
      s"$file has no column ${column.value}, which the key reads as its " +
        s"${part.toString.toLowerCase}."

/** The trial key of one file, checked against every record (ticket S5.3).
  *
  *  - `keys`: distinct keys among the records that resolve to one.
  *  - `repeated`: keys that more than one trial resolves to, each with all
  *    its trials, in key order.
  *  - `unresolved`: every record whose key does not resolve, in record order
  *    (one entry per failing column).
  *  - `occurrences`: the distinct occurrences of resolved records, ascending,
  *    when the key has an occurrence column (empty otherwise: eyes4s then
  *    takes 1 for every trial); `laterPresentations` counts the distinct
  *    trials (key and occurrence) whose occurrence is above 1.
  */
final case class KeyReport(
    file: String,
    source: SourceRole,
    columns: KeyColumns,
    unit: TrialUnit,
    records: Int,
    keys: Int,
    repeated: Vector[RepeatedKey],
    unresolved: Vector[UnresolvedRecord],
    occurrences: Vector[Int],
    laterPresentations: Int
) derives CanEqual:
  /** Every record resolves to exactly one key and every key to one trial. */
  def unique: Boolean = repeated.isEmpty && unresolved.isEmpty

  def keyHasOccurrence: Boolean = columns.occurrence.isDefined

  /** Records of more than one occurrence share a key that leaves the
    * occurrence out: eyes4s would read them as one trial.
    */
  def repeatsWithoutOccurrence: Boolean =
    repeated.nonEmpty && !keyHasOccurrence && (unit match
      case TrialUnit.Occurrence(_) => true
      case _                       => false)

/** Why the trial key blocks an import: a Studio check, not an eyes4s one.
  * Each case names the file and its operands.
  */
enum KeyBlock derives CanEqual:
  /** The key lacks a required part. */
  case Incomplete(gap: KeyGap)

  /** The key leaves the occurrence out, and records of more than one
    * occurrence (`occurrence`'s values) share `repeated` keys: eyes4s would
    * read each key's records as one trial.
    */
  case RepeatWithoutOccurrence(
      file: String,
      occurrence: ColumnName,
      repeated: Vector[RepeatedKey]
  )

  def message: String = this match
    case Incomplete(gap)                                     => gap.message
    case RepeatWithoutOccurrence(file, occurrence, repeated) =>
      val n     = repeated.size
      val keys  = if n == 1 then "1 key repeats" else s"$n keys repeat"
      val first = repeated.headOption.fold("")(k => s" First: ${k.key.render}.")
      s"Studio check · $file: $keys without Occurrence. Records with different " +
        s"${occurrence.value} values share each key, and eyes4s would read them as one " +
        s"trial; add Occurrence to the key, or give each presentation its own trial " +
        s"label.$first"

object TrialKeyCheck:

  /** What eyes4s requires of an identity field. */
  val nonBlank: String = "a non-blank value"

  /** What eyes4s requires of an occurrence. */
  val positiveOccurrence: String = "a positive integer occurrence"

  /** eyes4s's occurrence reading for `source`: a trial inventory takes only
    * decimal digits (`TrialColumns`); fixation records take any integer
    * spelling (`FixationKeyReader.trial`). Either way it is at least 1.
    */
  private def occurrence(source: SourceRole, raw: String): Option[TrialOccurrence] =
    val digits = source match
      case SourceRole.Trials    => Option.when(raw.matches("[0-9]+"))(raw)
      case SourceRole.Fixations => Some(raw)
    digits.flatMap(_.toIntOption).flatMap(TrialOccurrence.of(_).toOption)

  /** Check `columns`'s key against every record of `table`, counting trials
    * by `unit`.
    */
  def check(
      file: String,
      source: SourceRole,
      table: KeyTable,
      columns: KeyColumns,
      unit: TrialUnit
  ): Either[KeyGap, KeyReport] =
    def get(part: KeyPart, name: Option[ColumnName]) =
      name
        .toRight(KeyGap.Missing(file, columns.missing))
        .flatMap(n => table.column(n).toRight(KeyGap.Absent(file, part, n)))
    for
      _ <- Either.cond(columns.missing.isEmpty, (), KeyGap.Missing(file, columns.missing))
      p <- get(KeyPart.Participant, columns.participant)
      f <- get(KeyPart.Phase, columns.phase)
      t <- get(KeyPart.Trial, columns.trial)
      o <- columns.occurrence match
        case Some(c) => get(KeyPart.Occurrence, Some(c)).map(Some(_))
        case None    => Right(None)
      split <- unit match
        case TrialUnit.Occurrence(c) => get(KeyPart.Occurrence, Some(c)).map(Some(_))
        case _                       => Right(None)
    yield run(file, source, table.records, columns, unit, p, f, t, o, split)

  private def run(
      file: String,
      source: SourceRole,
      records: Int,
      columns: KeyColumns,
      unit: TrialUnit,
      p: KeyColumn,
      f: KeyColumn,
      t: KeyColumn,
      o: Option[KeyColumn],
      split: Option[KeyColumn]
  ): KeyReport =
    // Each distinct (participant, phase, trial, occurrence) code tuple is
    // resolved once, through eyes4s's own identity constructor.
    val resolved = mutable.HashMap
      .empty[(Int, Int, Int, Int), Either[Vector[(KeyColumn, String)], TrialIdentity]]
    val unresolved = Vector.newBuilder[UnresolvedRecord]
    // label codes -> trial (split code or record) -> records, in first-seen order
    val byKey = mutable.LinkedHashMap
      .empty[(Int, Int, Int), mutable.LinkedHashMap[Int, mutable.ArrayBuffer[Int]]]
    val trials = mutable.HashSet.empty[(Int, Int, Int, Int)]
    var i      = 0
    while i < records do
      val codes    = (p.code(i), f.code(i), t.code(i), o.fold(-1)(_.code(i)))
      val identity = resolved.getOrElseUpdate(codes, resolve(source, p, f, t, o, i))
      identity match
        case Left(failures) =>
          failures.foreach((column, requirement) =>
            unresolved += UnresolvedRecord(i + 1, column.name, column.value(i), requirement)
          )
        case Right(_) =>
          val label = (codes._1, codes._2, codes._3)
          val trial = unit match
            case TrialUnit.Record        => i
            case TrialUnit.Occurrence(_) => split.fold(0)(_.code(i))
            case TrialUnit.Key           => 0
          byKey
            .getOrElseUpdate(label, mutable.LinkedHashMap.empty)
            .getOrElseUpdate(trial, mutable.ArrayBuffer.empty) += (i + 1)
          trials += codes
      i += 1
    val occurrenceOf: (Int, Int) => Option[String] = (trial, record) =>
      unit match
        case TrialUnit.Occurrence(_) => split.map(_.values(trial))
        case _                       => o.map(_.value(record - 1))
    val repeated = byKey.iterator
      .collect {
        case ((pc, fc, tc), group) if group.size > 1 =>
          RepeatedKey(
            KeyLabel(p.values(pc), f.values(fc), t.values(tc)),
            group.iterator
              .map((trial, rs) => KeyTrial(occurrenceOf(trial, rs.head), rs.toVector))
              .toVector
          )
      }
      .toVector
      .sortBy(_.key)
    val identities =
      o.fold(Vector.empty[TrialIdentity])(_ =>
        trials.iterator.flatMap(c => resolved(c).toOption).toVector.distinct
      )
    val occurrences = identities.map(_.occurrence.value).distinct.sorted
    val later       = identities.count(_.occurrence.value > 1)
    KeyReport(
      file,
      source,
      columns,
      unit,
      records,
      byKey.size,
      repeated,
      unresolved.result(),
      occurrences,
      later
    )

  /** Record `i`'s identity under eyes4s's rules, or each failing column with
    * the requirement it fails.
    */
  private def resolve(
      source: SourceRole,
      p: KeyColumn,
      f: KeyColumn,
      t: KeyColumn,
      o: Option[KeyColumn],
      i: Int
  ): Either[Vector[(KeyColumn, String)], TrialIdentity] =
    val blank           = Vector(p, f, t).filter(_.value(i).trim.isEmpty).map(_ -> nonBlank)
    val occurrenceValue = o match
      case None    => Right(TrialOccurrence.first)
      case Some(c) => occurrence(source, c.value(i)).toRight(Vector(c -> positiveOccurrence))
    (blank, occurrenceValue) match
      case (Vector(), Right(n)) =>
        TrialIdentity
          .of(p.value(i), f.value(i), t.value(i), n)
          .left
          .map(e => Vector(p -> e.message))
      case (b, n) => Left(b ++ n.left.toOption.toVector.flatten)

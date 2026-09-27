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
  * participant, phase, trial and occurrence roles, and the item the trial is
  * matched on (an attribute every record of a trial must agree with). The
  * composition is the column mapping itself; it is not recorded elsewhere.
  */
final case class KeyColumns(
    participant: Option[ColumnName],
    phase: Option[ColumnName],
    trial: Option[ColumnName],
    occurrence: Option[ColumnName],
    item: Option[ColumnName] = None
) derives CanEqual:
  def column(part: KeyPart): Option[ColumnName] = part match
    case KeyPart.Participant => participant
    case KeyPart.Phase       => phase
    case KeyPart.Trial       => trial
    case KeyPart.Occurrence  => occurrence

  /** Required parts no column fills, in key order. */
  def missing: Vector[KeyPart] = KeyPart.required.filter(column(_).isEmpty)

  /** Every column the key reads. */
  def names: Vector[ColumnName] =
    Vector(participant, phase, trial, occurrence, item).flatten

object KeyColumns:
  /** The key of a mapping's choices: the first column of each key role (a
    * role on two columns is the mapping's own issue).
    */
  def of(columns: Vector[(ColumnName, ColumnChoice)]): KeyColumns =
    def first(role: ColumnRole) =
      columns.collectFirst { case (c, ColumnChoice.Role(r)) if r == role => c }
    KeyColumns(
      first(ColumnRole.Participant),
      first(ColumnRole.Phase),
      first(ColumnRole.Trial),
      first(ColumnRole.Occurrence),
      first(ColumnRole.Item)
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
// The report
// ---------------------------------------------------------------------------

/** What makes one trial among the records of one key. */
enum TrialUnit derives CanEqual:
  /** Each record is one trial: a trial inventory lists one trial a record. */
  case Record

  /** Records of one presentation, one (occurrence, item), are one trial, as
    * eyes4s groups fixation records (`FixationCsv`). `occurrence` is the
    * key's occurrence column or, when the key leaves it out, the column that
    * can hold it.
    */
  case Presentation(occurrence: Option[ColumnName])

/** A trial's label: participant, phase and trial. eyes4s identifies a trial
  * by its label; the occurrence and the item are values every record of the
  * trial must agree with (`TrialInventory`; `QuarantineCause.ItemConflict`
  * and `OccurrenceConflict` in `FixationCsv`).
  */
final case class KeyLabel(participant: String, phase: String, trial: String) derives CanEqual:
  def render: String = s"$participant · $phase · $trial"

object KeyLabel:
  /** eyes4s `TrialIdentity` order: participant, phase, trial. */
  given Ordering[KeyLabel] = Ordering.by(l => (l.participant, l.phase, l.trial))

/** Why one key names more than one trial, in eyes4s's terms. */
enum KeyConflict derives CanEqual:
  /** Its records name different items: eyes4s quarantines the trial
    * (`QuarantineCause.ItemConflict`), whatever the occurrences.
    */
  case Items(items: Vector[String])

  /** One item, different occurrences: eyes4s quarantines the trial
    * (`OccurrenceConflict`) when the occurrence is in the key, and reads the
    * records as one trial when it is not.
    */
  case Occurrences(occurrences: Vector[String])

  /** More than one trial inventory entry declares it. */
  case Entries

/** One trial of a repeated key: the occurrence (as eyes4s parses it, or as
  * written when it does not parse) and the item its records name, its first
  * and last record, and how many records it has.
  */
final case class KeyTrial(
    occurrence: Option[String],
    item: Option[String],
    first: Int,
    last: Int,
    records: Int
) derives CanEqual

/** A key more than one trial resolves to: why, how many trials (the first
  * [[TrialKeyCheck.TrialsKept]] of them listed, in first-record order), and
  * its first and last record and record count. It is never resolved by
  * taking its first match.
  */
final case class RepeatedKey(
    key: KeyLabel,
    conflict: KeyConflict,
    trialCount: Int,
    trials: Vector[KeyTrial],
    first: Int,
    last: Int,
    records: Int
) derives CanEqual:
  /** The studio key eyes4s gives `trial`'s records: the label with the
    * trial's occurrence when the key has an occurrence column, else 1.
    */
  def trialKey(trial: KeyTrial, keyHasOccurrence: Boolean): TrialKey =
    val occurrence =
      if keyHasOccurrence then trial.occurrence.flatMap(_.toIntOption).getOrElse(1)
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

  /** The file could not be read again for a streaming check. */
  case Unreadable(file: String, reason: String)

  def message: String = this match
    case Missing(file, parts) =>
      val names = parts.map(_.toString.toLowerCase).mkString(", ")
      s"$file: the trial key has no $names column; eyes4s reads a trial key from " +
        "participant, phase and trial columns."
    case Absent(file, part, column) =>
      s"$file has no column ${column.value}, which the key reads as its " +
        s"${part.toString.toLowerCase}."
    case Unreadable(file, reason) => s"$file could not be checked: $reason"

/** The trial key of one file, checked against every record (ticket S5.3).
  * Counts are exact; lists are bounded ([[TrialKeyCheck.Kept]]), so a report
  * does not grow with the file.
  *
  *  - `keys`: distinct keys among the records that resolve to one.
  *  - `repeatedCount`: keys more than one trial resolves to, of which
  *    `itemConflicts` and `occurrenceConflicts` by cause; `repeated`: the
  *    first of them in key order.
  *  - `unresolvedCount`: records whose key does not resolve; `unresolved`:
  *    the first failures in record order (one entry per failing column).
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
    repeatedCount: Int,
    itemConflicts: Int,
    occurrenceConflicts: Int,
    repeated: Vector[RepeatedKey],
    unresolvedCount: Int,
    unresolved: Vector[UnresolvedRecord],
    occurrences: Vector[Int],
    laterPresentations: Int
) derives CanEqual:
  /** Every record resolves to exactly one key and every key to one trial. */
  def unique: Boolean = repeatedCount == 0 && unresolvedCount == 0

  def keyHasOccurrence: Boolean = columns.occurrence.isDefined

  /** The column that holds the occurrence the key leaves out, if any. */
  def occurrenceLeftOut: Option[ColumnName] =
    if keyHasOccurrence then None
    else
      unit match
        case TrialUnit.Presentation(column) => column
        case TrialUnit.Record               => None

  /** Records of one item and more than one occurrence share a key that
    * leaves the occurrence out: eyes4s would read them as one trial.
    */
  def repeatsWithoutOccurrence: Boolean =
    occurrenceLeftOut.isDefined && occurrenceConflicts > 0

/** Why the trial key blocks an import: a Studio check, not an eyes4s one.
  * Each case names the file and its operands.
  */
enum KeyBlock derives CanEqual:
  /** The key cannot be checked. */
  case Incomplete(gap: KeyGap)

  /** The key leaves the occurrence out, and records of one item with
    * different `occurrence` values share `count` keys (the first is
    * `first`): eyes4s would read each key's records as one trial.
    */
  case RepeatWithoutOccurrence(
      file: String,
      occurrence: ColumnName,
      count: Int,
      first: Option[KeyLabel]
  )

  /** The key reads a column with too many distinct values to keep, and the
    * streaming check of the file has not answered yet.
    */
  case Checking(file: String)

  def message: String = this match
    case Incomplete(gap)                                         => gap.message
    case RepeatWithoutOccurrence(file, occurrence, count, first) =>
      val keys  = if count == 1 then "1 key repeats" else s"$count keys repeat"
      val named = first.fold("")(k => s" First: ${k.render}.")
      s"Studio check · $file: $keys without Occurrence. Records with different " +
        s"${occurrence.value} values share each key, and eyes4s would read them as one " +
        "trial. Relabel the trials so that each presentation has its own trial label. " +
        "With Occurrence in the key the import proceeds, but eyes4s quarantines these " +
        s"trials (occurrence conflict).$named"
    case Checking(file) =>
      s"Studio check · $file: the trial key is being checked against every record."

// ---------------------------------------------------------------------------
// The check
// ---------------------------------------------------------------------------

object TrialKeyCheck:

  /** How many repeated keys and unresolved entries a report lists. */
  val Kept: Int = 100

  /** How many trials a repeated key lists. */
  val TrialsKept: Int = 16

  /** What eyes4s requires of an identity field or an item. */
  val nonBlank: String = "a non-blank value"

  /** What eyes4s requires of an occurrence. */
  val positiveOccurrence: String = "a positive integer occurrence"

  /** eyes4s's occurrence reading for `source`: a trial inventory takes only
    * decimal digits (`TrialColumns`); fixation records take any integer
    * spelling (`FixationKeyReader.trial`). Either way it is at least 1.
    */
  def occurrence(source: SourceRole, raw: String): Option[TrialOccurrence] =
    val digits = source match
      case SourceRole.Trials    => Option.when(raw.matches("[0-9]+"))(raw)
      case SourceRole.Fixations => Some(raw)
    digits.flatMap(_.toIntOption).flatMap(TrialOccurrence.of(_).toOption)

  /** The columns the check reads for `columns` and `unit`. */
  def reads(columns: KeyColumns, unit: TrialUnit): Vector[ColumnName] =
    val split = unit match
      case TrialUnit.Presentation(c) => c.toVector
      case TrialUnit.Record          => Vector.empty
    (columns.names ++ split).distinct

  /** The columns the check reads that `table` did not keep: a key on them is
    * checked by [[stream]] instead.
    */
  def unencoded(table: KeyTable, columns: KeyColumns, unit: TrialUnit): Vector[ColumnName] =
    reads(columns, unit).filter(table.dropped.contains)

  /** Check `columns`'s key against every record of `table`. Every column
    * read must be kept ([[unencoded]] is empty).
    */
  def check(
      file: String,
      source: SourceRole,
      table: KeyTable,
      columns: KeyColumns,
      unit: TrialUnit
  ): Either[KeyGap, KeyReport] =
    locate(file, columns, unit, name => table.column(name)).map { found =>
      val grouping = KeyGrouping(file, source, columns, unit)
      val cells    = found.map(_.map(c => (i: Int) => c.value(i)))
      var i        = 0
      while i < table.records do
        grouping.add(i + 1, cells.map(_.map(_(i))))
        i += 1
      grouping.result(table.records)
    }

  /** Check `columns`'s key in one streaming pass over `text`, keeping only
    * the grouping (for a key on a column the sniffer did not keep).
    */
  def stream(
      file: String,
      source: SourceRole,
      text: CharSequence,
      delimiter: Delimiter,
      columns: KeyColumns,
      unit: TrialUnit
  ): Either[KeyGap, KeyReport] =
    val reader = RecordReader(file, text, delimiter)
    reader.next() match
      case None           => Left(KeyGap.Unreadable(file, "it has no header line"))
      case Some(Left(e))  => Left(KeyGap.Unreadable(file, e.message))
      case Some(Right(h)) =>
        val header = h.map(_.trim)
        locate(file, columns, unit, n => Option(header.indexOf(n.value)).filter(_ >= 0))
          .flatMap { found =>
            val grouping = KeyGrouping(file, source, columns, unit)
            var count    = 0
            reader
              .fold(Right(())) { fields =>
                count += 1
                grouping.add(count, found.map(_.map(i => fields.lift(i).getOrElse(""))))
                true
              }
              .left
              .map(e => KeyGap.Unreadable(file, e.message))
              .map(_ => grouping.result(count))
          }

  /** The columns in reading order: participant, phase, trial, the key's
    * occurrence, the item, and the occurrence that splits presentations.
    */
  private def locate[A](
      file: String,
      columns: KeyColumns,
      unit: TrialUnit,
      find: ColumnName => Option[A]
  ): Either[KeyGap, Vector[Option[A]]] =
    def get(part: KeyPart, name: ColumnName) =
      find(name).toRight(KeyGap.Absent(file, part, name))
    def optional(part: KeyPart, name: Option[ColumnName]) =
      name.fold[Either[KeyGap, Option[A]]](Right(None))(n => get(part, n).map(Some(_)))
    val split = unit match
      case TrialUnit.Presentation(c) => c
      case TrialUnit.Record          => None
    (columns.participant, columns.phase, columns.trial) match
      case (Some(p), Some(f), Some(t)) =>
        for
          pv <- get(KeyPart.Participant, p)
          fv <- get(KeyPart.Phase, f)
          tv <- get(KeyPart.Trial, t)
          ov <- optional(KeyPart.Occurrence, columns.occurrence)
          iv <- optional(KeyPart.Trial, columns.item)
          sv <- optional(KeyPart.Occurrence, split)
        yield Vector(Some(pv), Some(fv), Some(tv), ov, iv, sv)
      case _ => Left(KeyGap.Missing(file, columns.missing))

/** Groups resolved records by trial label and finds the labels more than one
  * trial resolves to, as eyes4s's `FixationCsv` conflict grouping does
  * (items first, then occurrences). It mirrors eyes4s until eyes4s offers a
  * pure grouping over `TrialIdentity` (bead bd-01M3HFWC66KQKV2YAA1XPN34KP,
  * "Pure trial-identity conflict grouping in eyes4s-plan"); then [[add]] and
  * [[result]] delegate to it and the mirror goes. Memory is bounded by the
  * distinct keys and their presentations, never by the records.
  */
private[importing] final class KeyGrouping(
    file: String,
    source: SourceRole,
    columns: KeyColumns,
    unit: TrialUnit
):
  import TrialKeyCheck.{Kept, TrialsKept, nonBlank, occurrence, positiveOccurrence}

  /** A presentation: its occurrence (parsed, or as written) and its item. */
  private type Presentation = (Option[Either[String, Int]], Option[String])

  private final class Span(val first: Int):
    var last: Int              = first
    var count: Int             = 1
    def add(record: Int): Unit =
      last = record
      count += 1

  private final class State(first: Int):
    val span            = Span(first)
    val presentations   = mutable.LinkedHashMap.empty[Presentation, Span]
    val entries         = Vector.newBuilder[(Presentation, Span)]
    var entryCount      = 0
    val occurrences     = mutable.Set.empty[Int]
    def trialCount: Int = if unit == TrialUnit.Record then entryCount else presentations.size

  private val keys            = mutable.HashMap.empty[(String, String, String), State]
  private val unresolved      = Vector.newBuilder[UnresolvedRecord]
  private var unresolvedKept  = 0
  private var unresolvedCount = 0
  private val keyOccurrence   = columns.occurrence
  private val splitIsKey      = unit match
    case TrialUnit.Presentation(c) => c.isDefined && c == keyOccurrence
    case TrialUnit.Record          => false

  private def fail(record: Int, column: ColumnName, value: String, requirement: String): Unit =
    if unresolvedKept < Kept then
      unresolved += UnresolvedRecord(record, column, value, requirement)
      unresolvedKept += 1

  /** One record's cells, in [[TrialKeyCheck.locate]]'s order. */
  def add(record: Int, cells: Vector[Option[String]]): Unit =
    val p                                                      = cells(0).getOrElse("")
    val f                                                      = cells(1).getOrElse("")
    val t                                                      = cells(2).getOrElse("")
    val item                                                   = cells(4)
    var clean                                                  = true
    def blank(column: Option[ColumnName], value: String): Unit =
      if value.trim.isEmpty then
        clean = false
        column.foreach(fail(record, _, value, nonBlank))
    blank(columns.participant, p)
    blank(columns.phase, f)
    blank(columns.trial, t)
    item.foreach(blank(columns.item, _))
    val keyed = (keyOccurrence, cells(3)) match
      case (Some(column), Some(raw)) =>
        val parsed = occurrence(source, raw)
        if parsed.isEmpty then
          clean = false
          fail(record, column, raw, positiveOccurrence)
        parsed
      case _ => Some(TrialOccurrence.first)
    if !clean then unresolvedCount += 1
    else
      keyed.flatMap(n => TrialIdentity.of(p, f, t, n).toOption) match
        case None     => unresolvedCount += 1
        case Some(id) =>
          val state = keys.getOrElseUpdate((p, f, t), State(record))
          if state.span.first != record then state.span.add(record)
          if keyOccurrence.isDefined then state.occurrences += id.occurrence.value
          val split: Option[Either[String, Int]] =
            if splitIsKey || (unit == TrialUnit.Record && keyOccurrence.isDefined) then
              Some(Right(id.occurrence.value))
            else
              cells(5).map(raw =>
                occurrence(SourceRole.Fixations, raw).map(_.value).toRight(raw)
              )
          val presentation: Presentation = (split, item)
          unit match
            case TrialUnit.Record =>
              state.entryCount += 1
              if state.entryCount <= TrialsKept then
                state.entries += presentation -> Span(record)
            case TrialUnit.Presentation(_) =>
              state.presentations.get(presentation) match
                case Some(span) => span.add(record)
                case None       => state.presentations.update(presentation, Span(record))

  private def render(o: Either[String, Int]): String = o.fold(identity, _.toString)

  def result(records: Int): KeyReport =
    val repeated                        = keys.iterator.filter(_._2.trialCount > 1).toVector
    def conflict(s: State): KeyConflict = unit match
      case TrialUnit.Record          => KeyConflict.Entries
      case TrialUnit.Presentation(_) =>
        val items = s.presentations.keys.flatMap(_._2).toVector.distinct
        if items.size > 1 then KeyConflict.Items(items.sorted)
        else
          KeyConflict.Occurrences(
            s.presentations.keys.flatMap(_._1).toVector.distinct.sortBy(_.toOption).map(render)
          )
    val classified = repeated.map((k, s) => (k, s, conflict(s)))
    val listed     = classified
      .sortBy((k, _, _) => KeyLabel(k._1, k._2, k._3))
      .take(Kept)
      .map { case ((p, f, t), s, c) =>
        val trials = unit match
          case TrialUnit.Record          => s.entries.result()
          case TrialUnit.Presentation(_) => s.presentations.toVector.take(TrialsKept)
        RepeatedKey(
          KeyLabel(p, f, t),
          c,
          s.trialCount,
          trials.map { case ((o, i), span) =>
            KeyTrial(o.map(render), i, span.first, span.last, span.count)
          },
          s.span.first,
          s.span.last,
          s.span.count
        )
      }
    val occurrences = keys.valuesIterator.flatMap(_.occurrences).toVector.distinct.sorted
    KeyReport(
      file,
      source,
      columns,
      unit,
      records,
      keys.size,
      repeated.size,
      classified.count { case (_, _, KeyConflict.Items(_)) => true; case _ => false },
      classified.count { case (_, _, KeyConflict.Occurrences(_)) => true; case _ => false },
      listed,
      unresolvedCount,
      unresolved.result(),
      occurrences,
      keys.valuesIterator.map(_.occurrences.count(_ > 1)).sum
    )

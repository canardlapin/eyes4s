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

package eyes4s.studio.app.importing

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.text.{Format, KeyText, KeyTextId}
import eyes4s.studio.core.document.{ColumnName, ColumnRole, SourceRole}
import eyes4s.studio.core.importing.*
import eyes4s.studio.core.selection.{RecordNumber, StudioRef}

// ---------------------------------------------------------------------------
// State: each file's key check, recomputed only when its inputs change
// ---------------------------------------------------------------------------

/** One file's trial key check and what it was computed from: the file's
  * bytes, the key's columns and the trial unit.
  */
final case class KeyCheckOf(
    source: ByteDigest,
    columns: KeyColumns,
    unit: TrialUnit,
    result: Either[KeyGap, KeyReport]
) derives CanEqual

/** The trial key checks of the wizard's files (ticket S5.3). The key is
  * composed by the column mapping (the participant, phase, trial and
  * occurrence roles), so the checks follow the drafts: each is recomputed
  * when its file or its key columns change, and only then.
  */
final case class KeyChecks(fixations: Option[KeyCheckOf], trials: Option[KeyCheckOf])
    derives CanEqual:

  def fixationReport: Option[KeyReport] = fixations.flatMap(_.result.toOption)
  def trialReport: Option[KeyReport]    = trials.flatMap(_.result.toOption)

  /** The Studio checks that block the import: the fixation key (the one the
    * document records) lacks the phase, or leaves the occurrence out while
    * records of more than one occurrence share a key. A missing participant
    * or trial is already the mapping's required-role issue.
    */
  def blocks: Vector[KeyBlock] = fixations.map(_.result).toVector.flatMap {
    case Left(KeyGap.Missing(file, parts)) =>
      val uncovered = parts.filterNot(p => ColumnRole.required.contains(p.role))
      Option.when(uncovered.nonEmpty)(KeyBlock.Incomplete(KeyGap.Missing(file, uncovered)))
    case Left(gap)                              => Some(KeyBlock.Incomplete(gap))
    case Right(r) if r.repeatsWithoutOccurrence =>
      r.unit match
        case TrialUnit.Occurrence(column) =>
          Some(KeyBlock.RepeatWithoutOccurrence(r.file, column, r.repeated))
        case _ => None
    case Right(_) => None
  }

object KeyChecks:
  val none: KeyChecks = KeyChecks(None, None)

  private def named(columns: Vector[(PreviewColumn, ColumnChoice)]) =
    columns.map((c, choice) => c.name -> choice)

  /** A fixation file's key and trial unit: records of one occurrence are one
    * trial, read from the key's occurrence column or, when the key leaves it
    * out, from the column that can hold it; with neither, a key's records
    * are one trial.
    */
  def fixationKey(columns: Vector[(ColumnName, ColumnChoice)]): (KeyColumns, TrialUnit) =
    val key  = KeyColumns.of(columns)
    val unit = key.occurrence
      .orElse(KeyColumns.occurrenceCandidate(columns))
      .fold(TrialUnit.Key)(TrialUnit.Occurrence(_))
    (key, unit)

  private def recheck(
      previous: Option[KeyCheckOf],
      source: SniffedSource,
      role: SourceRole,
      key: (KeyColumns, TrialUnit)
  ): KeyCheckOf =
    val (columns, unit) = key
    previous
      .filter(p => p.source == source.bytes && p.columns == columns && p.unit == unit)
      .getOrElse(
        KeyCheckOf(
          source.bytes,
          columns,
          unit,
          TrialKeyCheck.check(source.preview.file, role, source.table, columns, unit)
        )
      )

  /** The checks of `w`'s files, reusing `w`'s own where nothing changed. */
  def refresh(w: ImportWizard): KeyChecks =
    KeyChecks(
      w.fixations.map((src, draft) =>
        recheck(w.keys.fixations, src, SourceRole.Fixations, fixationKey(named(draft.columns)))
      ),
      w.trials.map((src, draft) =>
        recheck(
          w.keys.trials,
          src,
          SourceRole.Trials,
          (KeyColumns.of(named(draft.columns)), TrialUnit.Record)
        )
      )
    )

// ---------------------------------------------------------------------------
// View-model
// ---------------------------------------------------------------------------

/** The occurrence block's action: add the occurrence to the key or leave it
  * out ([[WizardIntent.IncludeOccurrence]]).
  */
final case class KeyToggleVM(include: Boolean, label: String) derives CanEqual

/** One block of the key (board: Participant + Phase + Trial + Occurrence).
  * `column` is the column that fills it; `toggle` is set on the occurrence
  * block when a column can hold the occurrence.
  */
final case class KeyBlockVM(
    part: KeyPart,
    label: String,
    column: Option[String],
    included: Boolean,
    toggle: Option[KeyToggleVM],
    accessible: String
) derives CanEqual

/** How a key line reads: every key unique, a repeat eyes4s reports, or a
  * Studio check that blocks the import.
  */
enum KeyTone derives CanEqual:
  case Unique, Warning, Blocking

/** One trial of a repeated key: its words and a ref per record. */
final case class KeyTrialVM(text: String, refs: Vector[StudioRef]) derives CanEqual

/** A repeated key with every trial it resolves to: `label` names the key
  * (and its file, off the first line), `summary` lists its trials.
  */
final case class RepeatedKeyVM(
    key: String,
    label: String,
    trials: Vector[KeyTrialVM],
    summary: String,
    accessible: String
) derives CanEqual

/** One file's key line (board: "960 unique keys · 0 duplicates · Occurrence
  * is 1 for every trial"). The first line names no file; later lines do.
  */
final case class KeyLineVM(
    source: SourceRole,
    file: Option[String],
    count: String,
    detail: String,
    tone: KeyTone,
    repeated: Vector[RepeatedKeyVM],
    accessible: String
) derives CanEqual

/** Everything the key builder draws (ticket S5.3; Data.dc.html, key
  * builder). The view binds it and dispatches [[WizardIntent]]s.
  */
final case class TrialKeyVM(
    title: String,
    rule: String,
    plus: String,
    equals: String,
    blocks: Vector[KeyBlockVM],
    lines: Vector[KeyLineVM],
    check: Option[String],
    empty: Option[String]
) derives CanEqual

object TrialKeyVM:

  private def t(id: KeyTextId, args: String*): String = KeyText(id, args*)

  def partLabel(part: KeyPart): String = t(part match
    case KeyPart.Participant => KeyTextId.PartParticipant
    case KeyPart.Phase       => KeyTextId.PartPhase
    case KeyPart.Trial       => KeyTextId.PartTrial
    case KeyPart.Occurrence  => KeyTextId.PartOccurrence)

  /** "1–12, 15, 481–492": ascending records as runs. */
  def records(numbers: Vector[Int]): String =
    val runs = numbers.foldLeft(Vector.empty[(Int, Int)]) {
      case (acc :+ ((from, to)), n) if n == to + 1 => acc :+ (from -> n)
      case (acc, n)                                => acc :+ (n    -> n)
    }
    runs
      .map((from, to) =>
        if from == to then Format.count(from.toLong)
        else s"${Format.count(from.toLong)}–${Format.count(to.toLong)}"
      )
      .mkString(", ")

  private def trialVM(report: KeyReport, key: RepeatedKey, trial: KeyTrial): KeyTrialVM =
    val rs =
      if trial.records.size == 1 then t(KeyTextId.TrialRecord, records(trial.records))
      else t(KeyTextId.TrialRecords, records(trial.records))
    val text      = trial.occurrence.fold(rs)(o => t(KeyTextId.TrialOccurrence, o, rs))
    val studioKey = key.trialKey(trial, report.keyHasOccurrence)
    KeyTrialVM(
      text,
      trial.records.flatMap(r =>
        RecordNumber
          .of(r)
          .toOption
          .map(StudioRef.SourceRecord(studioKey, None, report.source, _))
      )
    )

  private def repeatedVM(report: KeyReport, key: RepeatedKey, first: Boolean): RepeatedKeyVM =
    val trials = key.trials.map(trialVM(report, key, _))
    RepeatedKeyVM(
      key.key.render,
      if first then key.key.render
      else t(KeyTextId.RepeatedInFile, report.file, key.key.render),
      trials,
      trials.map(_.text).mkString(t(KeyTextId.TrialSeparator)),
      t(
        KeyTextId.RepeatedAccessible,
        key.key.render,
        key.trials.size.toString,
        trials.map(_.text).mkString("; ")
      )
    )

  /** "38 keys repeat without Occurrence", or its kin for the report's unit. */
  def repeats(report: KeyReport): String =
    val n = report.repeated.size
    if n == 0 then t(KeyTextId.NoDuplicates)
    else
      val (one, many) =
        if report.repeatsWithoutOccurrence then
          (KeyTextId.RepeatOneWithout, KeyTextId.RepeatManyWithout)
        else
          report.unit match
            case TrialUnit.Record =>
              (KeyTextId.RepeatOneRecords, KeyTextId.RepeatManyRecords)
            case _ => (KeyTextId.RepeatOneOccurrences, KeyTextId.RepeatManyOccurrences)
      if n == 1 then t(one) else t(many, Format.count(n.toLong))

  private def unresolved(report: KeyReport): Option[String] =
    val n = report.unresolved.map(_.record).distinct.size
    Option.when(n > 0)(
      if n == 1 then t(KeyTextId.UnresolvedOne)
      else t(KeyTextId.UnresolvedMany, Format.count(n.toLong))
    )

  private def occurrence(report: KeyReport): Option[String] =
    if !report.keyHasOccurrence then Some(t(KeyTextId.OccurrenceLeftOut))
    else
      report.occurrences match
        case Vector()  => None
        case Vector(1) => Some(t(KeyTextId.OccurrenceAllOne))
        case os        =>
          Some(
            t(
              KeyTextId.OccurrenceRange,
              os.head.toString,
              os.last.toString,
              Format.count(report.laterPresentations.toLong)
            )
          )

  private def count(report: KeyReport): String =
    (report.unique, report.keys) match
      case (true, 1)  => t(KeyTextId.UniqueKey)
      case (true, n)  => t(KeyTextId.UniqueKeys, Format.count(n.toLong))
      case (false, 1) => t(KeyTextId.OneKey)
      case (false, n) => t(KeyTextId.Keys, Format.count(n.toLong))

  private def line(report: KeyReport, first: Boolean, blocking: Boolean): KeyLineVM =
    val count  = this.count(report)
    val detail =
      (Vector(repeats(report)) ++ unresolved(report) ++ occurrence(report))
        .map("· " + _)
        .mkString(" ")
    val tone =
      if blocking then KeyTone.Blocking
      else if report.unique then KeyTone.Unique
      else KeyTone.Warning
    KeyLineVM(
      report.source,
      Option.when(!first)(report.file),
      count,
      detail,
      tone,
      report.repeated.map(repeatedVM(report, _, first)),
      t(KeyTextId.FileLine, report.file, count, detail)
    )

  private def blocks(w: ImportWizard): Vector[KeyBlockVM] =
    val columns   = w.fixations.fold(Vector.empty)(_._2.columns.map((c, ch) => c.name -> ch))
    val key       = KeyColumns.of(columns)
    val candidate = KeyColumns.occurrenceCandidate(columns)
    KeyPart.values.toVector.map { part =>
      val label  = partLabel(part)
      val column = key.column(part).map(_.value)
      part match
        case KeyPart.Occurrence =>
          val toggle = column match
            case Some(_) => Some(KeyToggleVM(false, t(KeyTextId.LeaveOutOccurrence)))
            case None    =>
              candidate.map(c => KeyToggleVM(true, t(KeyTextId.AddOccurrence, c.value)))
          val accessible = (column, candidate) match
            case (Some(c), _)    => t(KeyTextId.BlockColumn, label, c)
            case (None, Some(c)) => t(KeyTextId.BlockCandidate, label, c.value)
            case (None, None)    => t(KeyTextId.BlockLeftOut, label)
          KeyBlockVM(part, label, column, column.isDefined, toggle, accessible)
        case _ =>
          val accessible =
            column.fold(t(KeyTextId.BlockNoColumn, label))(t(KeyTextId.BlockColumn, label, _))
          KeyBlockVM(part, label, column, column.isDefined, None, accessible)
    }

  def of(w: ImportWizard): TrialKeyVM =
    val blocked                = w.keys.blocks
    def blocking(r: KeyReport) = blocked.exists {
      case KeyBlock.RepeatWithoutOccurrence(file, _, _) => file == r.file
      case KeyBlock.Incomplete(_)                       => false
    }
    // The inventory's line first: it is the board's count (every inventory
    // entry), then the fixation records'.
    val reports = w.keys.trialReport.toVector ++ w.keys.fixationReport.toVector
    TrialKeyVM(
      title = t(KeyTextId.Title),
      rule = t(KeyTextId.Rule),
      plus = t(KeyTextId.Plus),
      equals = t(KeyTextId.Equals),
      blocks = blocks(w),
      lines = reports.zipWithIndex.map((r, i) => line(r, i == 0, blocking(r))),
      check = Option.when(blocked.nonEmpty)(blocked.map(_.message).mkString(" ")),
      empty = Option.when(w.fixations.isEmpty)(t(KeyTextId.NoFile))
    )

  /** The key's entries in the Data issues tab: its Studio checks block;
    * occurrence conflicts, unresolved records and repeated inventory entries
    * are warnings (eyes4s reports them on admission; the inventory is mapped
    * in S5.4).
    */
  def issues(w: ImportWizard): Vector[IssueVM] =
    val blocking = w.keys.blocks.map {
      case b @ KeyBlock.RepeatWithoutOccurrence(_, column, _) =>
        IssueVM(b.message, Some(column.value), true)
      case b @ KeyBlock.Incomplete(_) => IssueVM(t(KeyTextId.GapIssue, b.message), None, true)
    }
    val warnings = (w.keys.fixationReport.toVector ++ w.keys.trialReport.toVector).flatMap {
      r =>
        val repeat = r.repeated.headOption.flatMap { first =>
          if r.repeatsWithoutOccurrence then None
          else
            r.unit match
              case TrialUnit.Record =>
                Some(
                  IssueVM(
                    t(KeyTextId.InventoryIssue, r.file, repeats(r), first.key.render),
                    None,
                    false
                  )
                )
              case _ =>
                Some(
                  IssueVM(
                    t(KeyTextId.ConflictIssue, r.file, repeats(r)),
                    r.columns.occurrence.map(_.value),
                    false
                  )
                )
        }
        val missing = r.unresolved.headOption.map(first =>
          IssueVM(
            t(KeyTextId.UnresolvedIssue, r.file, unresolved(r).getOrElse(""), first.message),
            Some(first.column.value),
            false
          )
        )
        repeat.toVector ++ missing
    }
    blocking ++ warnings

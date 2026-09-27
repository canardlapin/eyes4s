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
import eyes4s.studio.core.document.{ColumnName, SourceRole}
import eyes4s.studio.core.importing.*
import eyes4s.studio.core.selection.{RecordNumber, StudioRef}

// ---------------------------------------------------------------------------
// State: each file's key check, recomputed only when its inputs change
// ---------------------------------------------------------------------------

/** A file's key check: done, or waiting for the platform's streaming pass
  * over the file (a key on a column the sniffer did not keep).
  */
enum KeyResult derives CanEqual:
  case Checked(result: Either[KeyGap, KeyReport])
  case Pending

/** One file's trial key check and what it was computed from: the file's
  * bytes, the key's columns and the trial unit.
  */
final case class KeyCheckOf(
    source: ByteDigest,
    columns: KeyColumns,
    unit: TrialUnit,
    result: KeyResult
) derives CanEqual:
  def report: Option[KeyReport] = result match
    case KeyResult.Checked(Right(r)) => Some(r)
    case _                           => None

/** The trial key checks of the wizard's files (ticket S5.3). The key is
  * composed by the column mapping (the participant, phase, trial,
  * occurrence and item roles), so the checks follow the drafts: each is
  * recomputed when its file or its key columns change, and only then.
  */
final case class KeyChecks(fixations: Option[KeyCheckOf], trials: Option[KeyCheckOf])
    derives CanEqual:

  def fixationReport: Option[KeyReport] = fixations.flatMap(_.report)
  def trialReport: Option[KeyReport]    = trials.flatMap(_.report)

  def of(role: SourceRole): Option[KeyCheckOf] = role match
    case SourceRole.Fixations => fixations
    case SourceRole.Trials    => trials

  /** The Studio checks that block the import, on the fixation key (the one
    * the document records): it is still being checked, it cannot be checked,
    * or it leaves the occurrence out while records of one item and more than
    * one occurrence share a key. A missing key role is the mapping's own
    * required-role issue.
    */
  def blocks(file: String): Vector[KeyBlock] = fixations.map(_.result).toVector.flatMap {
    case KeyResult.Pending                             => Some(KeyBlock.Checking(file))
    case KeyResult.Checked(Left(KeyGap.Missing(_, _))) => None
    case KeyResult.Checked(Left(gap))                  => Some(KeyBlock.Incomplete(gap))
    case KeyResult.Checked(Right(r))                   =>
      r.occurrenceLeftOut.filter(_ => r.repeatsWithoutOccurrence).map { column =>
        KeyBlock.RepeatWithoutOccurrence(
          r.file,
          column,
          r.occurrenceConflicts,
          r.repeated.collectFirst {
            case RepeatedKey(key, KeyConflict.Occurrences(_), _, _, _, _, _) =>
              key
          }
        )
      }
  }

object KeyChecks:
  val none: KeyChecks = KeyChecks(None, None)

  private def named(columns: Vector[(PreviewColumn, ColumnChoice)]) =
    columns.map((c, choice) => c.name -> choice)

  /** A fixation file's key and trial unit: records of one presentation (one
    * occurrence and item) are one trial, the occurrence read from the key's
    * column or, when the key leaves it out, from the column that can hold it.
    */
  def fixationKey(columns: Vector[(ColumnName, ColumnChoice)]): (KeyColumns, TrialUnit) =
    val key = KeyColumns.of(columns)
    (
      key,
      TrialUnit.Presentation(key.occurrence.orElse(KeyColumns.occurrenceCandidate(columns)))
    )

  private def recheck(
      previous: Option[KeyCheckOf],
      source: SniffedSource,
      role: SourceRole,
      key: (KeyColumns, TrialUnit)
  ): (KeyCheckOf, Option[WizardEffect]) =
    val (columns, unit) = key
    previous.filter(p =>
      p.source == source.bytes && p.columns == columns && p.unit == unit
    ) match
      case Some(same) => (same, None)
      case None       =>
        val table = source.preview.keys
        if columns.missing.isEmpty && TrialKeyCheck.unencoded(table, columns, unit).nonEmpty
        then
          (
            KeyCheckOf(source.bytes, columns, unit, KeyResult.Pending),
            Some(
              WizardEffect.CheckKey(
                role,
                source.path,
                source.bytes,
                source.preview.file,
                source.preview.delimiter,
                columns,
                unit
              )
            )
          )
        else
          val result = TrialKeyCheck.check(source.preview.file, role, table, columns, unit)
          (KeyCheckOf(source.bytes, columns, unit, KeyResult.Checked(result)), None)

  /** The checks of `w`'s files, reusing `w`'s own where nothing changed, and
    * the streaming checks the platform must run for the others.
    */
  def refresh(w: ImportWizard): (KeyChecks, Vector[WizardEffect]) =
    val fixations = w.fixations.map((src, draft) =>
      recheck(w.keys.fixations, src, SourceRole.Fixations, fixationKey(named(draft.columns)))
    )
    val trials = w.trials.map((src, draft) =>
      recheck(
        w.keys.trials,
        src,
        SourceRole.Trials,
        (KeyColumns.of(named(draft.columns)), TrialUnit.Record)
      )
    )
    (
      KeyChecks(fixations.map(_._1), trials.map(_._1)),
      (fixations.flatMap(_._2) ++ trials.flatMap(_._2)).toVector
    )

  /** Record the platform's streaming check, if it is for the current inputs. */
  def answer(
      checks: KeyChecks,
      role: SourceRole,
      source: ByteDigest,
      columns: KeyColumns,
      unit: TrialUnit,
      result: Either[KeyGap, KeyReport]
  ): KeyChecks =
    def fill(c: Option[KeyCheckOf]) = c.map(k =>
      if k.source == source && k.columns == columns && k.unit == unit &&
        k.result == KeyResult.Pending
      then k.copy(result = KeyResult.Checked(result))
      else k
    )
    role match
      case SourceRole.Fixations => checks.copy(fixations = fill(checks.fixations))
      case SourceRole.Trials    => checks.copy(trials = fill(checks.trials))

  /** Run a [[WizardEffect.CheckKey]] over the file's `text`, as the
    * platform does off the UI thread: one streaming pass.
    */
  def run(effect: WizardEffect.CheckKey, text: CharSequence): WizardIntent.KeyChecked =
    WizardIntent.KeyChecked(
      effect.role,
      effect.source,
      effect.columns,
      effect.unit,
      TrialKeyCheck.stream(
        effect.file,
        effect.role,
        text,
        effect.delimiter,
        effect.columns,
        effect.unit
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

/** One trial of a repeated key: its words and refs to its first and last
  * record.
  */
final case class KeyTrialVM(text: String, refs: Vector[StudioRef]) derives CanEqual

/** A repeated key with its trials: `label` names the key (and its file, off
  * the first line), `summary` lists its trials.
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
  * `repeated` lists the report's first repeated keys; `more` says how many
  * it does not list.
  */
final case class KeyLineVM(
    source: SourceRole,
    file: Option[String],
    count: String,
    detail: String,
    tone: KeyTone,
    repeated: Vector[RepeatedKeyVM],
    more: Option[String],
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

  private def n(value: Int): String = Format.count(value.toLong)

  def partLabel(part: KeyPart): String = t(part match
    case KeyPart.Participant => KeyTextId.PartParticipant
    case KeyPart.Phase       => KeyTextId.PartPhase
    case KeyPart.Trial       => KeyTextId.PartTrial
    case KeyPart.Occurrence  => KeyTextId.PartOccurrence)

  /** "record 7", "records 1–3" or "12 records, 55–90". */
  def records(first: Int, last: Int, count: Int): String =
    if count == 1 then t(KeyTextId.TrialRecord, n(first))
    else if last - first + 1 == count then t(KeyTextId.TrialRecords, n(first), n(last))
    else t(KeyTextId.TrialRecordsSpread, n(first), n(last), n(count))

  private def trialVM(report: KeyReport, key: RepeatedKey, trial: KeyTrial): KeyTrialVM =
    val item = key.conflict match
      case KeyConflict.Items(_) => trial.item.map(t(KeyTextId.TrialItem, _))
      case _                    => None
    val text = (trial.occurrence.map(t(KeyTextId.TrialOccurrence, _)).toVector ++ item :+
      records(trial.first, trial.last, trial.records)).mkString(" · ")
    val studioKey = key.trialKey(trial, report.keyHasOccurrence)
    KeyTrialVM(
      text,
      Vector(trial.first, trial.last).distinct.flatMap(r =>
        RecordNumber
          .of(r)
          .toOption
          .map(StudioRef.SourceRecord(studioKey, None, report.source, _))
      )
    )

  private def repeatedVM(report: KeyReport, key: RepeatedKey, first: Boolean): RepeatedKeyVM =
    val trials = key.trials.map(trialVM(report, key, _))
    val more   = Option.when(key.trialCount > key.trials.size)(
      t(KeyTextId.TrialsMore, n(key.trialCount - key.trials.size))
    )
    val summary = (trials.map(_.text) ++ more).mkString(t(KeyTextId.TrialSeparator))
    RepeatedKeyVM(
      key.key.render,
      if first then key.key.render
      else t(KeyTextId.RepeatedInFile, report.file, key.key.render),
      trials,
      summary,
      t(KeyTextId.RepeatedAccessible, key.key.render, n(key.trialCount), summary)
    )

  private def counted(count: Int, one: KeyTextId, many: KeyTextId): String =
    if count == 1 then t(one) else t(many, n(count))

  /** "38 keys repeat without Occurrence", or its kin for the report's causes. */
  def repeats(report: KeyReport): String =
    if report.repeatedCount == 0 then t(KeyTextId.NoDuplicates)
    else
      report.unit match
        case TrialUnit.Record =>
          counted(report.repeatedCount, KeyTextId.RepeatOneRecords, KeyTextId.RepeatManyRecords)
        case TrialUnit.Presentation(_) =>
          val occurrences = Option.when(report.occurrenceConflicts > 0)(
            if report.repeatsWithoutOccurrence then
              counted(
                report.occurrenceConflicts,
                KeyTextId.RepeatOneWithout,
                KeyTextId.RepeatManyWithout
              )
            else
              counted(
                report.occurrenceConflicts,
                KeyTextId.RepeatOneOccurrences,
                KeyTextId.RepeatManyOccurrences
              )
          )
          val items = Option.when(report.itemConflicts > 0)(
            counted(report.itemConflicts, KeyTextId.RepeatOneItems, KeyTextId.RepeatManyItems)
          )
          (occurrences.toVector ++ items).mkString(" · ")

  private def unresolved(report: KeyReport): Option[String] =
    Option.when(report.unresolvedCount > 0)(
      counted(report.unresolvedCount, KeyTextId.UnresolvedOne, KeyTextId.UnresolvedMany)
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
              n(report.laterPresentations)
            )
          )

  private def count(report: KeyReport): String =
    (report.unique, report.keys) match
      case (true, 1)  => t(KeyTextId.UniqueKey)
      case (true, k)  => t(KeyTextId.UniqueKeys, n(k))
      case (false, 1) => t(KeyTextId.OneKey)
      case (false, k) => t(KeyTextId.Keys, n(k))

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
      Option.when(report.repeatedCount > report.repeated.size)(
        t(KeyTextId.RepeatedMore, n(report.repeatedCount - report.repeated.size))
      ),
      t(KeyTextId.FileLine, report.file, count, detail)
    )

  /** A file still being checked: its line says so. */
  private def checking(source: SourceRole, file: String, first: Boolean): KeyLineVM =
    val text = t(KeyTextId.Checking)
    KeyLineVM(
      source,
      Option.when(!first)(file),
      "",
      text,
      if source == SourceRole.Fixations then KeyTone.Blocking else KeyTone.Warning,
      Vector.empty,
      None,
      t(KeyTextId.FileLine, file, "", text)
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

  /** The fixation file's key blocks (a Studio check). */
  def keyBlocks(w: ImportWizard): Vector[KeyBlock] =
    w.keys.blocks(w.fixations.fold("")(_._1.preview.file))

  def of(w: ImportWizard): TrialKeyVM =
    val blocked                = keyBlocks(w)
    def blocking(r: KeyReport) = blocked.exists {
      case KeyBlock.RepeatWithoutOccurrence(file, _, _, _) => file == r.file
      case _                                               => false
    }
    // The inventory's line first: it is the board's count (every inventory
    // entry), then the fixation records'.
    val files = Vector(
      (SourceRole.Trials, w.trials.map(_._1.preview.file), w.keys.trials),
      (SourceRole.Fixations, w.fixations.map(_._1.preview.file), w.keys.fixations)
    ).collect { case (role, Some(file), Some(check)) => (role, file, check.result) }
    val lines = files
      .collect {
        case (role, file, KeyResult.Pending)          => Left((role, file))
        case (_, _, KeyResult.Checked(Right(report))) => Right(report)
      }
      .zipWithIndex
      .map {
        case (Right(r), i)           => line(r, i == 0, blocking(r))
        case (Left((role, file)), i) => checking(role, file, i == 0)
      }
    TrialKeyVM(
      title = t(KeyTextId.Title),
      rule = t(KeyTextId.Rule),
      plus = t(KeyTextId.Plus),
      equals = t(KeyTextId.Equals),
      blocks = blocks(w),
      lines = lines,
      check = Option.when(blocked.nonEmpty)(blocked.map(_.message).mkString(" ")),
      empty = Option.when(w.fixations.isEmpty)(t(KeyTextId.NoFile))
    )

  /** The key's entries in the Data issues tab: its Studio checks block;
    * occurrence and item conflicts, unresolved records and repeated inventory
    * entries are warnings (eyes4s reports them on admission; the inventory is
    * mapped in S5.4).
    */
  def issues(w: ImportWizard): Vector[IssueVM] =
    val blocking = keyBlocks(w).map {
      case b @ KeyBlock.RepeatWithoutOccurrence(_, column, _, _) =>
        IssueVM(b.message, Some(column.value), true)
      case b => IssueVM(b.message, None, true)
    }
    val warnings = (w.keys.fixationReport.toVector ++ w.keys.trialReport.toVector).flatMap {
      r =>
        val firstKey = r.repeated.headOption.map(_.key.render).getOrElse("")
        val repeat   = r.unit match
          case TrialUnit.Record =>
            Option
              .when(r.repeatedCount > 0)(
                IssueVM(t(KeyTextId.InventoryIssue, r.file, repeats(r), firstKey), None, false)
              )
              .toVector
          case TrialUnit.Presentation(_) =>
            val occurrences =
              Option.when(r.occurrenceConflicts > 0 && !r.repeatsWithoutOccurrence)(
                IssueVM(
                  t(
                    KeyTextId.ConflictIssue,
                    r.file,
                    counted(
                      r.occurrenceConflicts,
                      KeyTextId.RepeatOneOccurrences,
                      KeyTextId.RepeatManyOccurrences
                    )
                  ),
                  r.columns.occurrence.map(_.value),
                  false
                )
              )
            val items = Option.when(r.itemConflicts > 0)(
              IssueVM(
                t(
                  KeyTextId.ItemIssue,
                  r.file,
                  counted(r.itemConflicts, KeyTextId.RepeatOneItems, KeyTextId.RepeatManyItems)
                ),
                r.columns.item.map(_.value),
                false
              )
            )
            occurrences.toVector ++ items
        val missing = r.unresolved.headOption.map(first =>
          IssueVM(
            t(KeyTextId.UnresolvedIssue, r.file, unresolved(r).getOrElse(""), first.message),
            Some(first.column.value),
            false
          )
        )
        repeat ++ missing
    }
    blocking ++ warnings

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

import eyes4s.studio.app.text.{Format, ImportText, ImportTextId, KeyText, KeyTextId}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.command.ChangeKind
import eyes4s.studio.core.document.{
  AdmissionDecision,
  ColumnName,
  ColumnRole,
  DisplayColumns,
  SourceRole,
  StudioDocument,
  TimeUnit
}
import eyes4s.studio.core.importing.*

/** One tab button: its label (with its issue count when it has issues). */
final case class WizardTabVM(tab: WizardTab, label: String, selected: Boolean) derives CanEqual

/** One entry of a role menu: `label` as the closed menu shows it (the
  * board's words), `menuLabel` in the open list, which marks required roles.
  */
final case class ChoiceVM(choice: ColumnChoice, label: String, menuLabel: String)
    derives CanEqual

/** One mapping row (board: column, first records, role, units). */
final case class MappingRowVM(
    role: SourceRole,
    column: ColumnName,
    samples: String,
    choice: ColumnChoice,
    choiceLabel: String,
    accessible: String,
    units: String,
    unitsDeclared: Boolean,
    issue: Option[String]
) derives CanEqual

final case class TimeUnitOptionVM(unit: Option[TimeUnit], label: String) derives CanEqual

/** The declared time unit's control. */
final case class TimeUnitVM(
    label: String,
    options: Vector[TimeUnitOptionVM],
    selected: Option[TimeUnit],
    note: String
) derives CanEqual

/** Optional inventory display bindings, selected explicitly by column name. */
final case class DisplayColumnOptionVM(column: Option[ColumnName], label: String)
    derives CanEqual

final case class DisplayColumnsVM(
    kindLabel: String,
    fileLabel: String,
    options: Vector[DisplayColumnOptionVM],
    selected: Option[DisplayColumns],
    enabled: Boolean,
    note: String
) derives CanEqual

final case class GeometryFieldVM(field: GeometryField, label: String, value: String)
    derives CanEqual

/** One mapping table (fixations or trials): its header row, file summary
  * and rows, or the empty-state text and the choose button. A re-map reads
  * the revision's own files, so it offers no choose button (`canChoose`).
  */
final case class MappingTableVM(
    role: SourceRole,
    headers: Vector[String],
    summary: Option[String],
    empty: Option[String],
    choose: String,
    canChoose: Boolean,
    rows: Vector[MappingRowVM],
    choices: Vector[ChoiceVM]
) derives CanEqual

/** The Data issues tab: each issue's words, the column it names, and
  * whether it blocks the import (a warning does not).
  */
final case class IssueVM(text: String, column: Option[String], blocking: Boolean)
    derives CanEqual

final case class PresetsVM(
    label: String,
    names: Vector[String],
    empty: String,
    apply: String,
    nameLabel: String,
    name: String,
    save: String,
    canSave: Boolean
) derives CanEqual

/** Everything the wizard's view draws (ticket S5.2; Data.dc.html, column
  * mapping). The view binds these values and dispatches [[WizardIntent]]s;
  * it computes nothing.
  */
final case class ImportWizardVM(
    title: String,
    kind: String,
    showTabs: Boolean,
    tabs: Vector[WizardTabVM],
    tab: WizardTab,
    fixations: MappingTableVM,
    trials: MappingTableVM,
    time: TimeUnitVM,
    attributesNote: String,
    trialsNote: String,
    geometryNote: String,
    geometry: Vector[GeometryFieldVM],
    issuesSummary: String,
    issues: Vector[IssueVM],
    presets: PresetsVM,
    commit: String,
    canCommit: Boolean,
    cancel: String,
    status: Option[String],
    problem: Option[String],
    key: TrialKeyVM,
    trialsSource: Option[(String, WizardIntent)],
    displays: DisplayColumnsVM
) derives CanEqual

object ImportWizardVM:

  private def t(id: ImportTextId, args: String*): String = ImportText(id, args*)

  def roleLabel(role: ColumnRole): String =
    import ColumnRole.*
    t(role match
      case Participant => ImportTextId.RoleParticipant
      case Phase       => ImportTextId.RolePhase
      case Trial       => ImportTextId.RoleTrial
      case Occurrence  => ImportTextId.RoleOccurrence
      case Item        => ImportTextId.RoleItem
      case Response    => ImportTextId.RoleResponse
      case Ordinal     => ImportTextId.RoleOrdinal
      case SampleCount => ImportTextId.RoleSampleCount
      case X           => ImportTextId.RoleX
      case Y           => ImportTextId.RoleY
      case Onset       => ImportTextId.RoleOnset
      case Duration    => ImportTextId.RoleDuration)

  def choiceLabel(choice: ColumnChoice): String = choice match
    case ColumnChoice.Role(r)   => roleLabel(r)
    case ColumnChoice.Attribute => t(ImportTextId.RoleAttribute)

  def timeLabel(unit: Option[TimeUnit]): String = unit match
    case None                        => t(ImportTextId.TimeUndeclared)
    case Some(TimeUnit.Milliseconds) => t(ImportTextId.TimeMilliseconds)
    case Some(TimeUnit.Microseconds) => t(ImportTextId.TimeMicroseconds)
    case Some(TimeUnit.Seconds)      => t(ImportTextId.TimeSeconds)

  def tabLabel(tab: WizardTab): String = t(tab match
    case WizardTab.FixationMapping => ImportTextId.TabFixationMapping
    case WizardTab.TrialMetadata   => ImportTextId.TabTrialMetadata
    case WizardTab.Geometry        => ImportTextId.TabGeometry
    case WizardTab.DataIssues      => ImportTextId.TabDataIssues)

  def geometryLabel(field: GeometryField): String = t(field match
    case GeometryField.ScreenWidth     => ImportTextId.GeometryScreenWidth
    case GeometryField.ScreenHeight    => ImportTextId.GeometryScreenHeight
    case GeometryField.ImageLeft       => ImportTextId.GeometryImageLeft
    case GeometryField.ImageTop        => ImportTextId.GeometryImageTop
    case GeometryField.ImageWidth      => ImportTextId.GeometryImageWidth
    case GeometryField.ImageHeight     => ImportTextId.GeometryImageHeight
    case GeometryField.PixelsPerDegree => ImportTextId.GeometryPixelsPerDegree)

  /** The declared units a role's column is read in (board "Units
    * (declared)"): positions in screen px, times in the declared unit
    * ("undeclared" until it is), counts in samples, anything else none.
    */
  def unitsOf(choice: ColumnChoice, time: Option[TimeUnit]): (String, Boolean) = choice match
    case ColumnChoice.Role(ColumnRole.X | ColumnRole.Y) => (t(ImportTextId.UnitScreenPx), true)
    case ColumnChoice.Role(ColumnRole.Onset | ColumnRole.Duration) =>
      time.fold((t(ImportTextId.UnitUndeclared), false))(u => (timeLabel(Some(u)), true))
    case ColumnChoice.Role(ColumnRole.SampleCount) => (t(ImportTextId.UnitSamples), true)
    case ColumnChoice.Attribute                    => (t(ImportTextId.UnitText), true)
    case ColumnChoice.Role(_)                      => (t(ImportTextId.UnitNone), false)

  def problemText(p: WizardProblem): String = p match
    // An unreadable file's error already says which file could not be read.
    case WizardProblem.ReadFailed(_, e: SourceReadError.Unreadable) => e.message
    case WizardProblem.ReadFailed(path, e)       => t(ImportTextId.ReadFailed, path, e.message)
    case WizardProblem.Preset(e)                 => e.message
    case WizardProblem.PresetMapping(_, es)      => es.toVector.map(_.message).mkString(" ")
    case WizardProblem.DatasetMapping(_, es)     => es.toVector.map(_.message).mkString(" ")
    case WizardProblem.Mapping(e)                => e.message
    case WizardProblem.Blocked(es)               => issuesSummary(es.length)
    case WizardProblem.BadGeometry(e)            => e.message
    case WizardProblem.BadSources(e)             => e.message
    case WizardProblem.NoFixations               => t(ImportTextId.NeedFixations)
    case WizardProblem.NoTrials                  => t(ImportTextId.NoTrialsFile)
    case WizardProblem.UnknownDataset(d)         => s"Dataset ${d.label} is not in the project."
    case WizardProblem.NoChange(d)               => t(ImportTextId.NoChange, d.label)
    case WizardProblem.StoreFailed(reason)       => t(ImportTextId.StoreFailed, reason)
    case WizardProblem.NotDatasetSource(d, path) =>
      t(ImportTextId.NotDatasetSource, path, d.label)
    case WizardProblem.TrialKey(block)        => block.message
    case WizardProblem.NeedsRemap(d, missing) =>
      KeyText(KeyTextId.NeedsRemap, d.label, missing.map(_.label).mkString(", "))
    case WizardProblem.NoOccurrenceColumn(file) =>
      KeyText(KeyTextId.NoOccurrenceColumn, file)
    case WizardProblem.InventoryNeedsRemap(d, file, e) =>
      t(ImportTextId.InventoryNeedsRemap, d.label, file, e.message)

  /** In a re-map, the file name of the revision's trial inventory. */
  private def trialsFile(w: ImportWizard, document: StudioDocument): Option[String] =
    w.target match
      case WizardTarget.Remap(id) =>
        document.dataset(id).flatMap(_.sources.trials).map(_.path.value.split('/').last)
      case _ => None

  def noteText(n: WizardNote): String = n match
    case WizardNote.PresetSaved(name)         => t(ImportTextId.PresetSaved, name.value)
    case WizardNote.PresetApplied(name, file) => t(ImportTextId.PresetApplied, name.value, file)

  private def issuesSummary(n: Int): String =
    if n == 0 then t(ImportTextId.NoIssues)
    else if n == 1 then t(ImportTextId.IssuesSummaryOne)
    else t(ImportTextId.IssuesSummaryMany, Format.count(n.toLong))

  private def summaryOf(preview: CsvPreview): String =
    val args = Vector(
      preview.file,
      Format.count(preview.records.toLong),
      Format.count(preview.columns.size.toLong),
      preview.delimiter.label
    )
    if preview.raggedTotal == 0 then t(ImportTextId.FileSummary, args*)
    else t(ImportTextId.FileSummaryRagged, (args :+ Format.count(preview.raggedTotal.toLong))*)

  private def rows(
      role: SourceRole,
      columns: Vector[(PreviewColumn, ColumnChoice)],
      time: Option[TimeUnit],
      issues: Vector[MappingError]
  ): Vector[MappingRowVM] =
    columns.map { (c, choice) =>
      val (units, declared) = unitsOf(choice, time)
      MappingRowVM(
        role,
        c.name,
        c.samples.mkString(", "),
        choice,
        choiceLabel(choice),
        t(ImportTextId.RoleFor, c.name.value),
        units,
        declared,
        issues.find(_.pointsAt.contains(c.name)).map(_.message)
      )
    }

  private def choicesFor(
      offered: Vector[ColumnRole],
      required: Vector[ColumnRole]
  ): Vector[ChoiceVM] =
    ColumnChoice.all
      .filter(_.roleOption.forall(offered.contains))
      .map(c =>
        val menuLabel = c.roleOption.filter(required.contains) match
          case Some(_) => t(ImportTextId.Required, choiceLabel(c))
          case None    => choiceLabel(c)
        ChoiceVM(c, choiceLabel(c), menuLabel)
      )

  /** The commit button's words: what the document will get. */
  private def commitLabel(w: ImportWizard, document: StudioDocument): String =
    val next = DatasetRevision(document.datasets.lastOption.fold(1)(_.id.number + 1))
    w.target match
      case WizardTarget.NewImport => t(ImportTextId.CommitImportAs, next.label)
      case WizardTarget.Remap(id) =>
        document.dataset(id).map(_.decision) match
          case Some(AdmissionDecision.Pending) => t(ImportTextId.CommitApply, id.label)
          case Some(_)                         => t(ImportTextId.CommitReadmit, next.label)
          case None                            => t(ImportTextId.CommitImport)

  def of(w: ImportWizard, document: StudioDocument): ImportWizardVM =
    val fixationIssues = w.fixations.fold(Vector.empty)(_._2.issues)
    val trialIssues    = w.trialIssues
    val all            = w.issues
    val geometryIssues =
      w.geometry.parse.left.toOption.toVector.map(e => IssueVM(e.message, None, true))
    // The trial key's Studio checks block too (S5.3); its other findings warn.
    val keyIssues   = TrialKeyVM.issues(w)
    val keyBlocking = keyIssues.count(_.blocking)
    val ragged      = (w.fixations.map(_._1) ++ w.trials.map(_._1)).toVector.flatMap(s =>
      s.preview.ragged.map(r =>
        IssueVM(
          t(
            ImportTextId.RaggedIssue,
            s.preview.file,
            Format.count(r.record.toLong),
            r.actual.toString,
            r.expected.toString
          ),
          None,
          false
        )
      ) ++ Option
        .when(s.preview.raggedTotal > s.preview.ragged.size)(
          IssueVM(
            t(
              ImportTextId.RaggedMore,
              s.preview.file,
              Format.count((s.preview.raggedTotal - s.preview.ragged.size).toLong)
            ),
            None,
            false
          )
        )
        .toVector
    )
    val time      = w.fixations.flatMap(_._2.time)
    val newImport = w.target == WizardTarget.NewImport
    // A re-map is hosted in the column-mapping pane: it shows the mapping
    // page and, when the revision has a trials file, its trial metadata
    // (S5.4); geometry and the issues belong to the sibling panes.
    val offered =
      if newImport then WizardTab.values.toVector
      else
        WizardTab.FixationMapping +: Option
          .when(w.trials.isDefined || trialsFile(w, document).isDefined)(
            WizardTab.TrialMetadata
          )
          .toVector
    val tabs = offered.map { tab =>
      val count = tab match
        case WizardTab.FixationMapping => fixationIssues.size + keyBlocking
        case WizardTab.TrialMetadata   => trialIssues.size
        case WizardTab.Geometry        => w.geometry.parse.fold(_ => 1, _ => 0)
        case WizardTab.DataIssues      => all.size + keyBlocking + geometryIssues.size
      val label =
        if count == 0 then tabLabel(tab)
        else t(ImportTextId.TabIssues, tabLabel(tab), Format.count(count.toLong))
      WizardTabVM(tab, label, tab == w.tab)
    }
    ImportWizardVM(
      title = t(ImportTextId.Title),
      kind = ChangeKind.DatasetReadmit.label,
      // A re-map is hosted in the column-mapping pane, whose dock tab already
      // names it: it shows the mapping page only, with no tab strip (the
      // other pages belong to the sibling panes).
      showTabs = offered.size > 1,
      tabs = tabs,
      tab = if offered.contains(w.tab) then w.tab else WizardTab.FixationMapping,
      fixations = MappingTableVM(
        SourceRole.Fixations,
        Vector(
          t(ImportTextId.HeaderColumn),
          t(ImportTextId.HeaderSamples),
          t(ImportTextId.HeaderRole),
          t(ImportTextId.HeaderUnits)
        ),
        w.fixations.map(f => summaryOf(f._1.preview)),
        // A re-map has no file to choose: the pane says what it is reading.
        Option.when(w.fixations.isEmpty && newImport)(t(ImportTextId.NoFixationFile)),
        t(ImportTextId.ChooseFile),
        newImport,
        w.fixations.fold(Vector.empty)((_, d) =>
          rows(SourceRole.Fixations, d.columns, d.time, fixationIssues)
        ),
        choicesFor(ColumnRole.values.toVector, ColumnRole.required)
      ),
      trials = MappingTableVM(
        SourceRole.Trials,
        Vector(
          t(ImportTextId.HeaderTrialsColumn),
          t(ImportTextId.HeaderSamples),
          t(ImportTextId.HeaderRole),
          t(ImportTextId.HeaderUnits)
        ),
        w.trials.map(f => summaryOf(f._1.preview)),
        Option.when(w.trials.isEmpty)(t(ImportTextId.NoTrialsFile)),
        t(ImportTextId.ChooseTrials),
        newImport,
        w.trials.fold(Vector.empty)((_, d) =>
          rows(SourceRole.Trials, d.columns, None, trialIssues)
        ),
        choicesFor(TrialMetadataDraft.offered, TrialMetadataDraft.required)
      ),
      displays = DisplayColumnsVM(
        t(ImportTextId.DisplayKindColumn),
        t(ImportTextId.DisplayFileColumn),
        Vector(DisplayColumnOptionVM(None, t(ImportTextId.DisplayUnmapped))) ++
          w.trials.toVector
            .flatMap(_._2.preview.header)
            .map(c => DisplayColumnOptionVM(Some(c), c.value)),
        w.trials.flatMap(_._2.displays),
        w.trials.isDefined,
        t(ImportTextId.DisplayNote)
      ),
      time = TimeUnitVM(
        t(ImportTextId.TimeUnitLabel),
        (None +: TimeUnit.values.toVector.map(Some(_))).map(u =>
          TimeUnitOptionVM(u, timeLabel(u))
        ),
        time,
        t(ImportTextId.TimeNote)
      ),
      attributesNote = t(ImportTextId.AttributesNote),
      trialsNote = trialsFile(w, document)
        .filter(_ => w.dropTrials)
        .fold(
          t(ImportTextId.TrialsNote)
        )(t(ImportTextId.TrialsDropped, _)),
      geometryNote = t(ImportTextId.GeometryNote),
      geometry = GeometryField.values.toVector.map(f =>
        GeometryFieldVM(f, geometryLabel(f), w.geometry.field(f))
      ),
      issuesSummary = issuesSummary(all.size + keyBlocking + geometryIssues.size),
      issues =
        all.map(e => IssueVM(e.message, e.pointsAt.map(_.value), true)) ++ geometryIssues ++
          keyIssues.filter(_.blocking) ++ keyIssues.filterNot(_.blocking) ++ ragged,
      presets = PresetsVM(
        t(ImportTextId.PresetLabel),
        w.presets.names.map(_.value),
        t(ImportTextId.PresetNone),
        t(ImportTextId.PresetApply),
        t(ImportTextId.PresetNameLabel),
        w.presetName,
        t(ImportTextId.PresetSave),
        w.fixations.isDefined && w.presetName.trim.nonEmpty
      ),
      commit = commitLabel(w, document),
      // Enabled once there is a file: a refused commit says why and opens
      // the tab that holds the issue.
      canCommit = w.fixations.isDefined,
      // A re-map is hosted in its pane, where cancelling reverts the edits.
      cancel = t(if newImport then ImportTextId.Cancel else ImportTextId.Revert),
      status = w.note.map(noteText),
      problem = w.problem.map(problemText),
      key = TrialKeyVM.of(w),
      // A re-map may leave the revision's trial inventory out (S5.4 follow-up).
      trialsSource = trialsFile(w, document).map(file =>
        if w.dropTrials then (t(ImportTextId.KeepTrials, file), WizardIntent.DropTrials(false))
        else (t(ImportTextId.DropTrials, file), WizardIntent.DropTrials(true))
      )
    )

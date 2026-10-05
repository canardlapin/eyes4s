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

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.app.text.{ImportText, ImportTextId}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective, Source, StudioDocument}
import eyes4s.studio.core.importing.{ImportPresets, KeyPart, SourceReadError}

/** What the column-mapping pane shows around its wizard: why it is empty,
  * which revision's files it is reading from the project, and a notice
  * (edits it had to drop, presets it could not read, a revision it could
  * not open).
  */
final case class ColumnMappingPaneVM(
    empty: Option[String],
    reading: Option[String],
    notice: Option[String]
) derives CanEqual

/** Something the pane tells the user once, until it loads again. */
enum PaneNotice derives CanEqual:
  /** `dataset` changed elsewhere while the pane held edits not yet applied. */
  case EditsDropped(dataset: DatasetRevision)

  /** Saved presets that could not be read, each message naming its file. */
  case PresetsUnreadable(errors: Vector[String])

  /** The selected revision could not be opened for a re-map. */
  case CannotOpen(problem: WizardProblem)

  def message: String = this match
    case EditsDropped(d)       => ImportText(ImportTextId.PaneEditsDropped, d.label)
    case PresetsUnreadable(es) =>
      ImportText(ImportTextId.PanePresetsUnreadable, es.mkString("; "))
    case CannotOpen(p) => ImportWizardVM.problemText(p)

/** The Data perspective's column-mapping pane (Data.dc.html, "Column
  * mapping", tagged "Dataset · re-admit"). It hosts the import wizard's
  * re-map ([[ImportWizard.remap]]) on the selected dataset revision; the
  * platform reads that revision's own sources back from the project into it
  * and dispatches each as [[WizardIntent.SourceRead]], so the wizard's
  * `NotDatasetSource` guard applies. Its commit is the wizard's one command:
  * `ReviseDataset` for a pending revision, a re-import for an admitted one.
  *
  * The pane reloads whenever the selected revision changes, the selection
  * or the revision itself (after a re-map, or its undo), and its wizard's
  * Cancel reverts the edits: in a pane, [[WizardEffect.Close]] reloads. A
  * re-map that creates a revision moves the Data selection to it
  * ([[follow]]), so the pane never keeps edits already applied.
  */
object ColumnMappingPane:

  /** The dataset revision the Data perspective has selected: the last
    * dataset of its trail that the document holds, else the latest revision.
    */
  def selected(model: AppModel): Option[DatasetRevisionSpec] =
    val document = model.document
    model.navigation
      .trail(Perspective.Data)
      .reverseIterator
      .collectFirst { case Place.Dataset(id) if document.dataset(id).isDefined => id }
      .flatMap(document.dataset)
      .orElse(document.datasets.lastOption)

  /** A pane showing `shown` must reload for `model`: the revision it would
    * show now is another, or has changed.
    */
  def mustReload(shown: Option[DatasetRevisionSpec], model: AppModel): Boolean =
    selected(model) != shown

  /** Where the Data selection goes after a commit turned `before` into
    * `after`: to the revision the commit created, if it created one (the
    * re-import of an admitted revision). A revision edited in place keeps the
    * selection.
    */
  def follow(before: StudioDocument, after: AppModel): Option[Intent] =
    val known = before.datasets.map(_.id).toSet
    after.document.datasets
      .map(_.id)
      .filterNot(known)
      .lastOption
      .map(id => Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(id)))))

  /** The re-map of `spec` and the files to read into it from the project,
    * in role order (fixations first).
    */
  def open(
      document: StudioDocument,
      spec: DatasetRevisionSpec,
      presets: ImportPresets
  ): Either[WizardProblem, (ImportWizard, Vector[Source])] =
    ImportWizard.remap(document, spec.id, presets).map(_ -> spec.sources.entries)

  /** What the wizard is told when `source` cannot be read from the project. */
  def unreadable(source: Source, reason: String): WizardIntent =
    WizardIntent.ReadFailed(
      source.path.value,
      SourceReadError.Unreadable(source.path.value, reason)
    )

  /** The reason given when no project is open to read from. */
  def noProject: String = ImportText(ImportTextId.PaneNoProject)

  def vm(
      shown: Option[DatasetRevisionSpec],
      reading: Boolean,
      notice: Option[PaneNotice] = None
  ): ColumnMappingPaneVM =
    ColumnMappingPaneVM(
      Option.when(shown.isEmpty)(ImportText(ImportTextId.PaneNoDataset)),
      shown.filter(_ => reading).map(s => ImportText(ImportTextId.PaneReading, s.id.label)),
      notice.map(_.message)
    )

  /** The wizard's own focus stops inside the pane, in the order Tab visits
    * them (S1.11: they follow the pane's stop): its selected tab (when it
    * shows its tabs), the shown page's enabled controls, then Revert and the
    * commit when enabled.
    */
  def focusStops(vm: ImportWizardVM): Vector[FocusStop] =
    def table(t: MappingTableVM, extra: Vector[FocusStop]): Vector[FocusStop] =
      Option.when(t.canChoose)(FocusStop(A11yRole.Button, t.choose)).toVector ++ extra ++
        t.rows.map(r => FocusStop(A11yRole.ComboBox, r.accessible))
    val hasRows = vm.fixations.rows.nonEmpty
    val presets = vm.presets
    val page    = vm.tab match
      case WizardTab.FixationMapping =>
        table(
          vm.fixations,
          Option.when(hasRows)(FocusStop(A11yRole.ComboBox, vm.time.label)).toVector
        ) ++
          Option
            .when(hasRows && presets.names.nonEmpty)(
              Vector(
                FocusStop(A11yRole.ComboBox, presets.label),
                FocusStop(A11yRole.Button, presets.apply)
              )
            )
            .toVector
            .flatten ++
          Vector(FocusStop(A11yRole.TextField, presets.nameLabel)) ++
          Option.when(presets.canSave)(FocusStop(A11yRole.Button, presets.save)) ++
          // The trial key builder (S5.3) under the mapping: its occurrence
          // block is a toggle when a column can hold the occurrence.
          vm.key.blocks.collect {
            case b if b.part == KeyPart.Occurrence && b.toggle.isDefined =>
              FocusStop(A11yRole.ToggleButton, b.accessible)
          }
      case WizardTab.TrialMetadata =>
        table(
          vm.trials,
          vm.trialsSource.map((label, _) => FocusStop(A11yRole.Button, label)).toVector
        )
      case WizardTab.Geometry   => vm.geometry.map(g => FocusStop(A11yRole.TextField, g.label))
      case WizardTab.DataIssues => Vector.empty
    // The tabs are one toggle group: Tab stops on the selected one. A
    // re-map shows no tab strip.
    val tabs = if vm.showTabs then vm.tabs.filter(_.selected) else Vector.empty
    tabs.map(t => FocusStop(A11yRole.ToggleButton, t.label)) ++ page ++
      Vector(FocusStop(A11yRole.Button, vm.cancel)) ++
      Option.when(vm.canCommit)(FocusStop(A11yRole.Button, vm.commit))

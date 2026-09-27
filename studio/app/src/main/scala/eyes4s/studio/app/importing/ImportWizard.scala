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

import cats.data.NonEmptyVector
import eyes4s.studio.app.Intent
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{
  AdmissionDecision,
  ColumnName,
  DatasetRevisionSpec,
  DocumentError,
  Geometry,
  Sources,
  SourceRole,
  StudioDocument,
  TimeUnit
}
import eyes4s.studio.core.importing.*

/** The wizard's tabs (ticket S5.2), in the board's order. */
enum WizardTab derives CanEqual:
  case FixationMapping, TrialMetadata, Geometry, DataIssues

/** What committing the wizard does to the document.
  *
  *  - `NewImport`: a new dataset revision from freshly read files. It
  *    re-imports the document's latest revision at the time of the commit,
  *    if there is one, and inherits its admission choices.
  *  - `Remap`: edit the mapping, units and geometry of `dataset`'s own
  *    files. A pending revision is edited in place; a verifying or admitted
  *    one is re-imported as a new pending revision, since admitted data never
  *    changes under a run.
  */
enum WizardTarget derives CanEqual:
  case NewImport
  case Remap(dataset: DatasetRevision)

/** Why the wizard refused an action; nothing else changed. Each case names
  * its operand.
  */
enum WizardProblem derives CanEqual:
  case ReadFailed(path: String, error: SourceReadError)
  case Preset(error: PresetError)
  case PresetMapping(preset: PresetName, errors: NonEmptyVector[MappingError])
  case DatasetMapping(dataset: DatasetRevision, errors: NonEmptyVector[MappingError])
  case Mapping(error: MappingError)
  case Blocked(errors: NonEmptyVector[MappingError])
  case BadGeometry(error: GeometryInputError)
  case BadSources(error: DocumentError)
  case NoFixations
  case NoTrials
  case UnknownDataset(dataset: DatasetRevision)
  case NoChange(dataset: DatasetRevision)

  /** A re-map read a file other than `dataset`'s own fixation source. */
  case NotDatasetSource(dataset: DatasetRevision, path: String)

  /** The platform could not store a preset or an imported file. */
  case StoreFailed(reason: String)

/** A fact that is not a document command: a note the wizard shows once. */
enum WizardNote derives CanEqual:
  case PresetSaved(name: PresetName)
  case PresetApplied(name: PresetName, file: String)

/** A user action or platform fact the wizard's view dispatches. */
enum WizardIntent derives CanEqual:
  /** The platform read a chosen file (bytes digested and sniffed in studio-core). */
  case SourceRead(source: SniffedSource)
  case ReadFailed(path: String, error: SourceReadError)
  case ChooseTab(tab: WizardTab)

  /** "Choose fixations.csv…": the platform's file dialog. */
  case RequestFile(role: SourceRole)
  case Choose(role: SourceRole, column: ColumnName, choice: ColumnChoice)
  case DeclareTime(unit: Option[TimeUnit])
  case EditGeometry(field: GeometryField, value: String)
  case TypePresetName(text: String)
  case SavePreset
  case ApplyPreset(name: String)

  /** The platform could not store a preset or a file; nothing was applied. */
  case StoreFailed(reason: String)

  /** The platform's saved presets (on open). */
  case PresetsLoaded(presets: ImportPresets)
  case Commit
  case Cancel

/** What the platform or the app must do after a wizard update. */
enum WizardEffect derives CanEqual:
  /** Apply a document command ("Dataset · re-admit"). */
  case Dispatch(command: Command)

  /** Persist a newly saved preset. */
  case StorePreset(preset: ImportPreset)

  /** Show the platform's file dialog for a file of `role`. */
  case OpenFile(role: SourceRole)
  case Close

object WizardEffect:
  /** The app intents a wizard's effects carry: its document commands. */
  def appIntents(effects: Vector[WizardEffect]): Vector[Intent] =
    effects.collect { case Dispatch(c) => Intent.Dispatch(c) }

/** The import wizard (ticket S5.2): an Elm-style component of studio-app.
  * Its state, its pure [[ImportWizard.update]] and its view-model
  * ([[ImportWizardVM]]) are UI-neutral; the platform reads files and stores
  * presets, and the app applies the document commands it emits, every one a
  * "Dataset · re-admit" change.
  */
final case class ImportWizard private (
    target: WizardTarget,
    fixations: Option[(SniffedSource, MappingDraft)],
    trials: Option[(SniffedSource, TrialMetadataDraft)],
    geometry: GeometryFields,
    presets: ImportPresets,
    presetName: String,
    tab: WizardTab,
    problem: Option[WizardProblem],
    note: Option[WizardNote]
) derives CanEqual:

  /** Every issue that blocks the commit: the fixation mapping's. */
  def issues: Vector[MappingError] = fixations.fold(Vector.empty)(_._2.issues)

  /** The trial inventory's mapping issues. They do not block the commit:
    * the document does not record the trial mapping yet (S5.4 joins and
    * records it), so they are shown as warnings.
    */
  def warnings: Vector[MappingError] = trials.fold(Vector.empty)(_._2.issues)

object ImportWizard:

  private val none: Vector[WizardEffect] = Vector.empty

  /** A wizard for a new import. Its geometry starts as the parent's, else
    * blank: geometry is declared, never guessed.
    */
  def newImport(document: StudioDocument, presets: ImportPresets): ImportWizard =
    val parent = document.datasets.lastOption
    ImportWizard(
      WizardTarget.NewImport,
      None,
      None,
      parent.fold(GeometryFields.blank)(p => GeometryFields.of(p.geometry)),
      presets,
      "",
      WizardTab.FixationMapping,
      None,
      None
    )

  /** A wizard re-mapping `dataset`'s files; the platform then reads them
    * from the project and dispatches [[WizardIntent.SourceRead]].
    */
  def remap(
      document: StudioDocument,
      dataset: DatasetRevision,
      presets: ImportPresets
  ): Either[WizardProblem, ImportWizard] =
    document
      .dataset(dataset)
      .toRight(WizardProblem.UnknownDataset(dataset))
      .map(spec =>
        ImportWizard(
          WizardTarget.Remap(dataset),
          None,
          None,
          GeometryFields.of(spec.geometry),
          presets,
          "",
          WizardTab.FixationMapping,
          None,
          None
        )
      )

  /** The pure update. A refused action changes nothing but the problem. */
  def update(
      w: ImportWizard,
      intent: WizardIntent,
      document: StudioDocument
  ): (ImportWizard, Vector[WizardEffect]) =
    val cleared                  = w.copy(problem = None, note = None)
    def refuse(p: WizardProblem) = (cleared.copy(problem = Some(p)), none)
    intent match
      case WizardIntent.SourceRead(source) =>
        source.role match
          case SourceRole.Fixations =>
            fixationDraft(w, source, document) match
              case Left(p)      => refuse(p)
              case Right(draft) => (cleared.copy(fixations = Some((source, draft))), none)
          case SourceRole.Trials =>
            (
              cleared.copy(trials =
                Some((source, TrialMetadataDraft.proposed(source.preview)))
              ),
              none
            )
      case WizardIntent.ReadFailed(path, error) => refuse(WizardProblem.ReadFailed(path, error))
      case WizardIntent.ChooseTab(tab)          => (cleared.copy(tab = tab), none)
      case WizardIntent.RequestFile(role) => (cleared, Vector(WizardEffect.OpenFile(role)))

      case WizardIntent.Choose(SourceRole.Fixations, column, choice) =>
        w.fixations match
          case None               => refuse(WizardProblem.NoFixations)
          case Some((src, draft)) =>
            draft
              .choose(column, choice)
              .fold(
                e => refuse(WizardProblem.Mapping(e)),
                d => (cleared.copy(fixations = Some((src, d))), none)
              )
      case WizardIntent.Choose(SourceRole.Trials, column, choice) =>
        w.trials match
          case None               => refuse(WizardProblem.NoTrials)
          case Some((src, draft)) =>
            draft
              .choose(column, choice)
              .fold(
                e => refuse(WizardProblem.Mapping(e)),
                d => (cleared.copy(trials = Some((src, d))), none)
              )

      case WizardIntent.DeclareTime(unit) =>
        w.fixations match
          case None               => refuse(WizardProblem.NoFixations)
          case Some((src, draft)) =>
            (cleared.copy(fixations = Some((src, draft.declare(unit)))), none)

      case WizardIntent.EditGeometry(field, value) =>
        (cleared.copy(geometry = w.geometry.set(field, value)), none)

      case WizardIntent.TypePresetName(text) => (cleared.copy(presetName = text), none)

      case WizardIntent.SavePreset =>
        w.fixations match
          case None             => refuse(WizardProblem.NoFixations)
          case Some((_, draft)) =>
            val saved = for
              name    <- PresetName.of(w.presetName)
              preset  <- draft.preset(name)
              presets <- w.presets.add(preset)
            yield (preset, presets)
            saved match
              case Left(e)                  => refuse(WizardProblem.Preset(e))
              case Right((preset, presets)) =>
                (
                  cleared.copy(
                    presets = presets,
                    presetName = "",
                    note = Some(WizardNote.PresetSaved(preset.name))
                  ),
                  Vector(WizardEffect.StorePreset(preset))
                )

      case WizardIntent.ApplyPreset(name) =>
        (w.presets.find(name), w.fixations) match
          case (Left(e), _)                    => refuse(WizardProblem.Preset(e))
          case (_, None)                       => refuse(WizardProblem.NoFixations)
          case (Right(preset), Some((src, _))) =>
            preset
              .applyTo(src.preview)
              .fold(
                errors => refuse(WizardProblem.PresetMapping(preset.name, errors)),
                d =>
                  (
                    cleared.copy(
                      fixations = Some((src, d)),
                      note = Some(WizardNote.PresetApplied(preset.name, src.preview.file))
                    ),
                    none
                  )
              )

      case WizardIntent.StoreFailed(reason)    => refuse(WizardProblem.StoreFailed(reason))
      case WizardIntent.PresetsLoaded(presets) => (cleared.copy(presets = presets), none)
      case WizardIntent.Cancel                 => (cleared, Vector(WizardEffect.Close))
      case WizardIntent.Commit                 => commit(cleared, document)

  /** A fixation file's first draft: a re-mapped dataset's own mapping, else
    * the suggested roles.
    */
  private def fixationDraft(
      w: ImportWizard,
      source: SniffedSource,
      document: StudioDocument
  ): Either[WizardProblem, MappingDraft] =
    w.target match
      case WizardTarget.NewImport => Right(MappingDraft.proposed(source.preview))
      case WizardTarget.Remap(id) =>
        document.dataset(id) match
          case None       => Left(WizardProblem.UnknownDataset(id))
          case Some(spec) =>
            // A re-map reads the revision's own file, byte for byte.
            if !spec.sources.fixations.exists(_.bytes == source.bytes) then
              Left(WizardProblem.NotDatasetSource(id, source.path.value))
            else
              MappingDraft
                .ofDataset(source.preview, id, spec.mapping, spec.units, spec.attributes)
                .left
                .map(WizardProblem.DatasetMapping(id, _))

  /** The document commands that apply the wizard, every one a "Dataset ·
    * re-admit" change, or why there are none. On success the wizard closes.
    */
  def commands(
      w: ImportWizard,
      document: StudioDocument
  ): Either[WizardProblem, Vector[Command]] =
    for
      (src, draft) <- w.fixations.toRight(WizardProblem.NoFixations)
      blocking = w.issues
      _        <- NonEmptyVector.fromVector(blocking).map(WizardProblem.Blocked(_)).toLeft(())
      resolved <- draft.resolve.left.map(WizardProblem.Blocked(_))
      geometry <- w.geometry.parse.left.map(WizardProblem.BadGeometry(_))
      commands <- w.target match
        case WizardTarget.NewImport =>
          // The parent is the live document's latest revision, not the one
          // there was when the wizard opened.
          val parent = document.datasets.lastOption.map(_.id)
          Sources
            .of(Vector(src.source) ++ w.trials.map(_._1.source))
            .left
            .map(WizardProblem.BadSources(_))
            .map(sources =>
              Vector(
                Command.ImportSources(
                  parent,
                  sources,
                  resolved.mapping,
                  resolved.units,
                  geometry,
                  resolved.attributes
                )
              )
            )
        case WizardTarget.Remap(id) =>
          document.dataset(id).toRight(WizardProblem.UnknownDataset(id)).flatMap { spec =>
            remapCommands(spec, resolved, geometry)
          }
    yield commands

  private def remapCommands(
      spec: DatasetRevisionSpec,
      resolved: ResolvedMapping,
      geometry: Geometry
  ): Either[WizardProblem, Vector[Command]] =
    val id      = spec.id
    val changed =
      spec.mapping != resolved.mapping || spec.units != resolved.units ||
        spec.geometry != geometry || spec.attributes != resolved.attributes
    if !changed then Left(WizardProblem.NoChange(id))
    else
      spec.decision match
        // One command, so one undo restores the revision as it was.
        case AdmissionDecision.Pending =>
          Right(
            Vector(
              Command.ReviseDataset(
                id,
                resolved.mapping,
                resolved.units,
                geometry,
                resolved.attributes
              )
            )
          )
        case AdmissionDecision.Verifying(_) | AdmissionDecision.Admitted(_, _) =>
          Right(
            Vector(
              Command.ImportSources(
                Some(id),
                spec.sources,
                resolved.mapping,
                resolved.units,
                geometry,
                resolved.attributes
              )
            )
          )

  private def commit(
      w: ImportWizard,
      document: StudioDocument
  ): (ImportWizard, Vector[WizardEffect]) =
    commands(w, document) match
      case Right(cs) => (w, cs.map(WizardEffect.Dispatch(_)) :+ WizardEffect.Close)
      case Left(p)   =>
        val tab = p match
          case WizardProblem.BadGeometry(_) => WizardTab.Geometry
          case WizardProblem.Blocked(_)     => WizardTab.DataIssues
          case _                            => w.tab
        (w.copy(problem = Some(p), tab = tab), none)

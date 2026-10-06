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

package eyes4s.studio.core.command

import eyes4s.codec.{CanonicalDigest, CodecError}
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.document.*

/** The two undo histories: the document's science, and how it is viewed. */
enum HistoryStack derives CanEqual:
  case Science, Presentation

  def label: String = this match
    case Science      => "edit"
    case Presentation => "view change"

/** The document value a command acts on, so an error can point at it. */
enum Target derives CanEqual:
  case OnDataset(dataset: DatasetRevision)
  case OnAnalysis(revision: AnalysisRevision)

  /** The draft: its id, or the id a new draft would take (`None` when the
    * document has no analysis revision to draft from).
    */
  case OnDraft(draft: Option[AnalysisRevision])
  case OnRun(run: RunId)
  case OnReporting(reporting: ReportingId)
  case OnFigure(figure: FigureId)
  case OnPanel(figure: FigureId, panel: PanelLetter)
  case OnPresentation

  def label: String = this match
    case OnDataset(d)       => s"dataset ${d.label}"
    case OnAnalysis(r)      => s"analysis ${r.label}"
    case OnDraft(Some(d))   => s"draft ${d.label}"
    case OnDraft(None)      => "the draft"
    case OnRun(r)           => r.label
    case OnReporting(r)     => s"reporting spec ${r.value}"
    case OnFigure(f)        => f.label
    case OnPanel(f, letter) => s"${f.label} panel ${letter.value}"
    case OnPresentation     => "the presentation"

/** Why a command, undo or redo was refused (ticket S2.2). Every case names
  * its operands; `message` is a default English rendering.
  */
enum CommandError derives CanEqual:
  /** `command` on `target` would give an invalid document; `error` says why. */
  case Refused(command: String, target: Target, error: DocumentError)

  /** `command` would leave `target` as it is. */
  case NoChange(command: String, target: Target)

  /** A digest could not be computed for `target`. */
  case Undigestible(target: Target, error: CodecError)

  case UnknownDataset(dataset: DatasetRevision)
  case DatasetExists(dataset: DatasetRevision)
  case DatasetNotPending(dataset: DatasetRevision)
  case DatasetNotAdmitted(dataset: DatasetRevision)
  case CorrectionIndex(dataset: DatasetRevision, index: Int, rules: Int)

  /** `dataset` is verifying `content`; it cannot change until withdrawn. */
  case VerificationPending(
      dataset: DatasetRevision,
      content: CanonicalDigest[DatasetRevisionSpec]
  )
  case NotVerified(dataset: DatasetRevision)

  /** An admission for `admitted`, but `dataset` is verifying `recorded`. */
  case VerificationMismatch(
      dataset: DatasetRevision,
      recorded: CanonicalDigest[DatasetRevisionSpec],
      admitted: CanonicalDigest[DatasetRevisionSpec]
  )

  /** `dataset` was verified as `verified` but its content is now `current`. */
  case ChangedSinceVerification(
      dataset: DatasetRevision,
      verified: CanonicalDigest[DatasetRevisionSpec],
      current: CanonicalDigest[DatasetRevisionSpec]
  )

  case NoAnalysis(command: String)
  case UnknownAnalysis(revision: AnalysisRevision)

  /** `command` needs a draft; `target` is the draft it would act on. */
  case NoDraft(command: String, target: Target)
  case DraftExists(draft: AnalysisRevision)

  /** A recipe change whose `before` (`expected`) is not what the draft's
    * recipe holds (`held`).
    */
  case StaleChange(field: RecipeField, expected: String, held: String)

  /** Save & run was given a preset the saved recipe does not hold (S7.1). */
  case PresetNotHeld(preset: Preset, revision: AnalysisRevision)

  case PlanAlreadyBound(revision: AnalysisRevision, bound: CanonicalDigest[StudyPlanArtifact])
  case InputMismatch(
      revision: AnalysisRevision,
      recorded: SemanticIdentity,
      prepared: SemanticIdentity
  )

  case UnknownRun(run: RunId)
  case RunNotRunning(run: RunId, state: RunLifecycle)
  case OutcomeStillRunning(run: RunId)
  case NoJobHandle(run: RunId)
  case RunNotCompleted(run: RunId, state: RunLifecycle)
  case ArtifactScopeMismatch(
      run: RunId,
      recordedRevision: AnalysisRevision,
      recordedDataset: DatasetRevision,
      preparedRevision: AnalysisRevision,
      preparedDataset: DatasetRevision
  )
  case ArtifactRecipeMismatch(revision: AnalysisRevision, recorded: Recipe, prepared: Recipe)
  case ArtifactBindingMismatch(run: RunId, field: String, recorded: String, prepared: String)

  case UnknownReporting(reporting: ReportingId)
  case ReportingInUse(reporting: ReportingId, figures: Vector[FigureId])
  case UnknownFigure(figure: FigureId)
  case FigureExists(figure: FigureId)
  case UnknownPanel(figure: FigureId, panel: PanelLetter)
  case PanelIndex(figure: FigureId, index: Int, panels: Int)

  case NothingToUndo(stack: HistoryStack)
  case NothingToRedo(stack: HistoryStack)
  case UndoBlocked(barrier: HistoryBarrier)

  /** Undoing or redoing `command` was refused by the current document. */
  case HistoryRefused(stack: HistoryStack, command: String, cause: CommandError)

  def message: String = this match
    case Refused(command, target, error) =>
      s"$command on ${target.label} is refused: ${error.message}"
    case NoChange(command, target)  => s"$command changes nothing in ${target.label}."
    case Undigestible(target, e)    => s"${target.label} cannot be digested: ${e.message}"
    case UnknownDataset(dataset)    => s"Dataset ${dataset.label} is not in the document."
    case DatasetExists(dataset)     => s"Dataset ${dataset.label} is already in the document."
    case DatasetNotPending(dataset) =>
      s"Dataset ${dataset.label} is admitted; edit a re-import instead."
    case DatasetNotAdmitted(dataset) =>
      s"Dataset ${dataset.label} is not admitted, so nothing can run on it."
    case CorrectionIndex(dataset, index, rules) =>
      s"Dataset ${dataset.label} has $rules correction rules; position $index is outside them."
    case VerificationPending(dataset, content) =>
      s"Dataset ${dataset.label} is being verified as ${content.display}; withdraw the " +
        "verification to edit it."
    case NotVerified(dataset) => s"Dataset ${dataset.label} has not been sent for verification."
    case VerificationMismatch(dataset, recorded, admitted) =>
      s"The admission is for ${admitted.display}, but dataset ${dataset.label} is verifying " +
        s"${recorded.display}."
    case ChangedSinceVerification(dataset, verified, current) =>
      s"Dataset ${dataset.label} was verified as ${verified.display} but is now ${current.display}."
    case NoAnalysis(command)       => s"$command needs an analysis revision to draft from."
    case UnknownAnalysis(revision) => s"Analysis ${revision.label} is not in the document."
    case NoDraft(command, target)  => s"$command needs ${target.label}, and there is no draft."
    case DraftExists(draft)        => s"Draft ${draft.label} already exists."
    case StaleChange(field, expected, held) =>
      s"The change to the ${field.label} starts from $expected, but the draft has $held."
    case PresetNotHeld(preset, revision) =>
      s"Analysis ${revision.label} cannot be saved as the $preset preset: its recipe does not " +
        "hold that preset's fields."
    case PlanAlreadyBound(revision, bound) =>
      s"Analysis ${revision.label} is already bound to plan ${bound.display}."
    case InputMismatch(revision, recorded, prepared) =>
      s"Analysis ${revision.label} records input ${recorded.value}, but preparation bound " +
        s"${prepared.value}."
    case UnknownRun(run)           => s"${run.label} is not in the document."
    case RunNotRunning(run, state) => s"${run.label} is not running (it is $state)."
    case OutcomeStillRunning(run)  => s"The outcome recorded for ${run.label} is still running."
    case NoJobHandle(run)          =>
      s"${run.label} has no backend job in this session, so it cannot be cancelled."
    case RunNotCompleted(run, state) =>
      s"${run.label} cannot bind native artifacts: it is $state, not completed."
    case ArtifactScopeMismatch(run, revision, dataset, preparedRevision, preparedDataset) =>
      s"${run.label} records ${revision.label} on ${dataset.label}, but native artifacts name ${preparedRevision.label} on ${preparedDataset.label}."
    case ArtifactRecipeMismatch(revision, recorded, prepared) =>
      val changes = RecipeChange.between(recorded, prepared).map { change =>
        s"${change.field.label}: ${change.renderedValues._1} -> ${change.renderedValues._2}"
      }
      s"${revision.label} native recipe disagrees with its saved recipe (${changes.mkString("; ")})."
    case ArtifactBindingMismatch(run, field, recorded, prepared) =>
      s"${run.label} records $field $recorded, but native artifacts bind $prepared."
    case UnknownReporting(id)        => s"Reporting spec ${id.value} is not in the document."
    case ReportingInUse(id, figures) =>
      s"Reporting spec ${id.value} is bound by ${figures.map(_.label).mkString(", ")}."
    case UnknownFigure(figure)             => s"${figure.label} is not in the document."
    case FigureExists(figure)              => s"${figure.label} is already in the document."
    case UnknownPanel(figure, panel)       => s"${figure.label} has no panel ${panel.value}."
    case PanelIndex(figure, index, panels) =>
      s"${figure.label} has $panels panels; position $index is outside them."
    case NothingToUndo(stack)                  => s"There is no ${stack.label} to undo."
    case NothingToRedo(stack)                  => s"There is no ${stack.label} to redo."
    case UndoBlocked(barrier)                  => barrier.message
    case HistoryRefused(stack, command, cause) =>
      s"The ${stack.label} history cannot apply $command: ${cause.message}"

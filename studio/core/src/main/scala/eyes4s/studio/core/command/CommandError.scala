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

import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.document.*

/** The two undo histories: the document's science, and how it is viewed. */
enum HistoryStack derives CanEqual:
  case Science, Presentation

  def label: String = this match
    case Science      => "edit"
    case Presentation => "view change"

/** Why a command, undo or redo was refused (ticket S2.2). Every case names
  * its operands; `message` is a default English rendering.
  */
enum CommandError derives CanEqual:
  /** The command's result is not a valid document; `error` says why. */
  case Refused(command: String, error: DocumentError)

  /** The command would leave the document as it is. */
  case NoChange(command: String)

  case UnknownDataset(dataset: DatasetRevision)
  case DatasetExists(dataset: DatasetRevision)
  case DatasetNotPending(dataset: DatasetRevision)
  case DatasetNotAdmitted(dataset: DatasetRevision)
  case CorrectionIndex(dataset: DatasetRevision, index: Int, rules: Int)

  case NoAnalysis(command: String)
  case UnknownAnalysis(revision: AnalysisRevision)
  case NoDraft(command: String)
  case DraftExists(draft: AnalysisRevision)

  /** A recipe change whose `before` (`expected`) is not what the draft's
    * recipe holds (`held`).
    */
  case StaleChange(field: RecipeField, expected: String, held: String)

  case UnknownRun(run: RunId)
  case RunNotRunning(run: RunId, state: RunLifecycle)
  case OutcomeStillRunning(run: RunId)

  case UnknownReporting(reporting: ReportingId)
  case ReportingInUse(reporting: ReportingId, figures: Vector[FigureId])
  case UnknownFigure(figure: FigureId)
  case FigureExists(figure: FigureId)
  case UnknownPanel(figure: FigureId, panel: PanelLetter)

  case NothingToUndo(stack: HistoryStack)
  case NothingToRedo(stack: HistoryStack)
  case UndoBlocked(barrier: HistoryBarrier)

  /** Undoing or redoing `command` was refused by the current document. */
  case HistoryRefused(stack: HistoryStack, command: String, cause: CommandError)

  def message: String = this match
    case Refused(command, error)    => s"$command is refused: ${error.message}"
    case NoChange(command)          => s"$command changes nothing."
    case UnknownDataset(dataset)    => s"Dataset ${dataset.label} is not in the document."
    case DatasetExists(dataset)     => s"Dataset ${dataset.label} is already in the document."
    case DatasetNotPending(dataset) =>
      s"Dataset ${dataset.label} is admitted; edit a re-import instead."
    case DatasetNotAdmitted(dataset) =>
      s"Dataset ${dataset.label} is not admitted, so nothing can run on it."
    case CorrectionIndex(dataset, index, rules) =>
      s"Dataset ${dataset.label} has $rules correction rules; position $index is outside them."
    case NoAnalysis(command)       => s"$command needs an analysis revision to draft from."
    case UnknownAnalysis(revision) => s"Analysis ${revision.label} is not in the document."
    case NoDraft(command)          => s"$command needs a draft, and there is none."
    case DraftExists(draft)        => s"Draft ${draft.label} already exists."
    case StaleChange(field, expected, held) =>
      s"The change to the ${field.label} starts from $expected, but the draft has $held."
    case UnknownRun(run)           => s"${run.label} is not in the document."
    case RunNotRunning(run, state) => s"${run.label} is not running (it is $state)."
    case OutcomeStillRunning(run)  => s"The outcome recorded for ${run.label} is still running."
    case UnknownReporting(id)      => s"Reporting spec ${id.value} is not in the document."
    case ReportingInUse(id, figures) =>
      s"Reporting spec ${id.value} is bound by ${figures.map(_.label).mkString(", ")}."
    case UnknownFigure(figure)       => s"${figure.label} is not in the document."
    case FigureExists(figure)        => s"${figure.label} is already in the document."
    case UnknownPanel(figure, panel) => s"${figure.label} has no panel ${panel.value}."
    case NothingToUndo(stack)        => s"There is no ${stack.label} to undo."
    case NothingToRedo(stack)        => s"There is no ${stack.label} to redo."
    case UndoBlocked(barrier)        => barrier.message
    case HistoryRefused(stack, command, cause) =>
      s"The ${stack.label} history cannot apply $command: ${cause.message}"

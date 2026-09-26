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

import eyes4s.studio.core.document.StudioDocument

/** A reversible command and the inverse the reducer computed for it. */
final case class UndoEntry(command: Command, inverse: Command) derives CanEqual

/** One undo history: entries to undo (most recent first), entries to redo,
  * and the barrier undo stops at, if one has been passed. Entries behind a
  * barrier are dropped: they can never be reached again.
  */
final case class UndoStack private[command] (
    done: List[UndoEntry],
    undone: List[UndoEntry],
    barrier: Option[HistoryBarrier]
) derives CanEqual:
  def canUndo: Boolean = done.nonEmpty
  def canRedo: Boolean = undone.nonEmpty

  /** A new edit: it becomes the next undo, and the redo list is cleared. */
  private[command] def push(entry: UndoEntry): UndoStack =
    new UndoStack(entry :: done, Nil, barrier)

  /** An irreversible edit: nothing before it can be undone or redone. */
  private[command] def fence(at: HistoryBarrier): UndoStack =
    new UndoStack(Nil, Nil, Some(at))

object UndoStack:
  val empty: UndoStack = new UndoStack(Nil, Nil, None)

/** A history step's result: the history after it and the effects to perform. */
final case class Step(history: History, effects: Vector[Effect]) derives CanEqual

/** The document with its two undo histories (ticket S2.2).
  *
  *  - '''Science''' holds every reversible dataset, analysis, reporting and
  *    figure edit. Save & run and Admit are barriers: they start a run or
  *    admit data the backend has acted on, so undo stops at them rather than
  *    deleting a run or an admission. Backend facts ([[Recording.Unrecorded]])
  *    leave both stacks as they are.
  *  - '''Presentation''' holds view-only edits on a separate stack. Undo on
  *    the science stack therefore never spends itself on a theme toggle or a
  *    perspective switch, a barrier never blocks undoing a view change, and
  *    no presentation edit can reach the science or its hash. Coalescing a
  *    slider drag into one entry is the app's job (dispatch on release).
  */
final case class History private (
    document: StudioDocument,
    science: UndoStack,
    presentation: UndoStack
) derives CanEqual:
  import CommandError.*

  def stack(which: HistoryStack): UndoStack = which match
    case HistoryStack.Science      => science
    case HistoryStack.Presentation => presentation

  private def withStack(which: HistoryStack, next: UndoStack): History = which match
    case HistoryStack.Science      => copy(science = next)
    case HistoryStack.Presentation => copy(presentation = next)

  /** Apply `command` and record it on the stack its kind belongs to. */
  def apply(command: Command): Either[CommandError, Step] =
    Reducer.run(document, command).map { o =>
      val next     = copy(document = o.document)
      val recorded = o.recording match
        case Recording.Reversible(inverse) =>
          val which = History.stackOf(command)
          next.withStack(which, next.stack(which).push(UndoEntry(command, inverse)))
        case Recording.Barrier(barrier) => next.copy(science = science.fence(barrier))
        case Recording.Unrecorded       => next
      Step(recorded, o.effects)
    }

  def undo: Either[CommandError, Step]     = undoOn(HistoryStack.Science)
  def redo: Either[CommandError, Step]     = redoOn(HistoryStack.Science)
  def undoView: Either[CommandError, Step] = undoOn(HistoryStack.Presentation)
  def redoView: Either[CommandError, Step] = redoOn(HistoryStack.Presentation)

  def undoOn(which: HistoryStack): Either[CommandError, Step] =
    val s = stack(which)
    s.done match
      case Nil =>
        Left(s.barrier.fold(NothingToUndo(which))(UndoBlocked(_)))
      case entry :: rest =>
        Reducer
          .run(document, entry.inverse)
          .left
          .map(HistoryRefused(which, entry.inverse.name, _))
          .map { o =>
            Step(
              copy(document = o.document)
                .withStack(which, new UndoStack(rest, entry :: s.undone, s.barrier)),
              o.effects
            )
          }

  /** Redo reapplies the command and records the inverse it computes now. */
  def redoOn(which: HistoryStack): Either[CommandError, Step] =
    val s = stack(which)
    s.undone match
      case Nil           => Left(NothingToRedo(which))
      case entry :: rest =>
        Reducer
          .run(document, entry.command)
          .left
          .map(HistoryRefused(which, entry.command.name, _))
          .map { o =>
            val redone = o.recording match
              case Recording.Reversible(inverse) => UndoEntry(entry.command, inverse)
              case _                             => entry
            Step(
              copy(document = o.document)
                .withStack(which, new UndoStack(redone :: s.done, rest, s.barrier)),
              o.effects
            )
          }

  /** Perform one journal entry. */
  def perform(entry: JournalEntry): Either[CommandError, Step] = entry match
    case JournalEntry.Apply(command) => apply(command)
    case JournalEntry.Undo           => undo
    case JournalEntry.Redo           => redo
    case JournalEntry.UndoView       => undoView
    case JournalEntry.RedoView       => redoView

object History:
  /** A document with empty histories. */
  def start(document: StudioDocument): History =
    History(document, UndoStack.empty, UndoStack.empty)

  /** View-only commands go on the presentation stack; all others on science. */
  def stackOf(command: Command): HistoryStack =
    if command.kind == ChangeKind.ViewOnly then HistoryStack.Presentation
    else HistoryStack.Science

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

package eyes4s.studio.core.session

import eyes4s.codec.ByteDigest
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.bundle.{BundleError, BundlePath, LockOwner, ScienceCheck, StoreError}
import eyes4s.studio.core.command.{
  CommandError,
  History,
  JournalEntry,
  JournalError,
  Step,
  TornLine
}
import eyes4s.studio.core.document.StudioDocument

/** One journal entry the recovery screen lists: its number in the journal
  * (from 1) and the entry.
  */
final case class RestoreItem(seq: Int, entry: JournalEntry) derives CanEqual:
  def label: String = entry match
    case JournalEntry.Apply(command) => s"${command.name} (${command.kind.label})"
    case JournalEntry.Undo           => "Undo"
    case JournalEntry.Redo           => "Redo"
    case JournalEntry.UndoView       => "Undo view change"
    case JournalEntry.RedoView       => "Redo view change"

/** A run the recovered document shows as running although no backend job
  * runs it: the job belonged to the session that crashed. Recovery never
  * resubmits it; it lists it so the user can. `sinceSave` is true when the
  * run was requested by a journaled Save & run rather than already running
  * in the saved document.
  */
final case class UnsubmittedRun(
    run: RunId,
    analysis: AnalysisRevision,
    dataset: DatasetRevision,
    sinceSave: Boolean
) derives CanEqual:
  def label: String = s"${run.label}: ${analysis.label} on ${dataset.label}"

/** How far the journal's own checkpoints vouch for the replay. A checkpoint
  * that disagreed would have made the journal unreplayable
  * ([[JournalError.Drift]]), so an offer carries only agreement.
  */
enum DriftCheck derives CanEqual:
  /** The replay matched the science recorded after each of these entry
    * counts; `unchecked` entries followed the last checkpoint.
    */
  case Checked(checkpoints: Vector[Int], unchecked: Int)

  /** The journal ended before its first checkpoint: `entries` replayed
    * without a recorded digest to compare with.
    */
  case NoCheckpoint(entries: Int)

/** What recovery would restore: the journal (by the digest of its bytes)
  * replayed on the saved document. Accepting saves `recovered` with the
  * undo history the replay rebuilt; declining archives the journal.
  */
final case class RecoveryOffer(
    journal: ByteDigest,
    saved: StudioDocument,
    recovered: StudioDocument,
    history: History,
    restores: Vector[RestoreItem],
    unsubmitted: Vector[UnsubmittedRun],
    drift: DriftCheck,
    torn: Option[TornLine]
) derives CanEqual

/** A journal found on open that newer work may be in. */
enum Recovery derives CanEqual:
  case Offered(offer: RecoveryOffer)

  /** The journal extends the saved document but does not replay: a line
    * before the last is unreadable, an entry is refused, or a checkpoint
    * disagrees. It can only be declined (archived).
    */
  case Unreplayable(digest: ByteDigest, error: JournalError)

  /** `project.json` did not open (`problem`), so the session opened the
    * previous manifest, and the journal starts from some other document:
    * most likely the damaged one. Its work cannot be replayed on anything the
    * bundle still holds, but it is kept, never taken for superseded; it can
    * only be declined (archived) explicitly.
    */
  case JournalOnDamagedManifest(digest: ByteDigest, problem: BundleError)

  def journal: ByteDigest = this match
    case Offered(offer)                 => offer.journal
    case Unreplayable(j, _)             => j
    case JournalOnDamagedManifest(j, _) => j

/** What opening found in the autosave journal. */
enum JournalFinding derives CanEqual:
  /** No journal, or one with nothing to replay. */
  case Clean

  /** A journal over an older document than the current manifest's: the
    * manifest already holds its work (a save completed but its journal reset
    * did not). Archived at `archived`. Never reported when the session
    * opened the previous manifest (see [[Recovery.JournalOnDamagedManifest]]).
    */
  case Superseded(archived: BundlePath)

  /** A journal over the saved document with entries after it: the session
    * waits for [[ProjectSession.accept]] or [[ProjectSession.decline]].
    */
  case Pending(recovery: Recovery)

/** Which manifest the session opened. */
enum ManifestSource derives CanEqual:
  case Current

  /** `project.json` did not open (`problem`); the retained previous manifest
    * did. The next save replaces `project.json`.
    */
  case Previous(problem: BundleError)

/** What opening a project reported beside the session. `orphaned` lists
  * the runs the saved document shows as running: no job runs them any more,
  * since a job belongs to the session that started it, so each is offered
  * for resubmission on every open, recovery or not.
  */
final case class OpenReport(
    source: ManifestSource,
    science: ScienceCheck,
    journal: JournalFinding,
    orphaned: Vector[UnsubmittedRun]
) derives CanEqual

/** What happened to an entry's autosave. */
enum JournalWrite derives CanEqual:
  /** `lines` lines were appended to the journal. */
  case Appended(lines: Int)

  /** The journal was written whole, `lines` lines: the first write of a
    * session or the first after a failed one.
    */
  case Rewritten(lines: Int)

  /** The entry changed nothing a replay could reproduce, such as a cancel
    * request, so it was not journaled.
    */
  case NotJournaled

  /** The write failed; the entry stands in memory, and the next autosave,
    * or the next save before it swaps the manifest, writes the journal whole.
    */
  case Failed(error: StoreError)

/** A performed entry: its step (with the effects the caller performs) and
  * its autosave.
  */
final case class Applied(step: Step, journal: JournalWrite) derives CanEqual

/** A completed save: the manifest's digest, the parts it wrote (the others
  * were already present), whether the manifest was swapped (not when it was
  * already these bytes), the failure of the journal rewrite a save makes
  * before its swap when an autosave had failed (`journalReconcile`), and the
  * journal reset's failure. A failed reset leaves a journal the next open
  * recognises as superseded. A failed reconcile is the one weaker window: a
  * crash before this save's swap recovers only what the journal holds.
  */
final case class SaveReceipt(
    manifest: ByteDigest,
    written: Vector[BundlePath],
    swapped: Boolean,
    journalReconcile: Option[StoreError],
    journalReset: Option[StoreError]
) derives CanEqual

/** How a session autosaves: a checkpoint (the science digest) after every
  * `checkpointEvery` journaled entries; at least 1.
  */
final case class SessionOptions private (checkpointEvery: Int) derives CanEqual

object SessionOptions:
  val default: SessionOptions = new SessionOptions(16)

  def of(checkpointEvery: Int): Either[SessionError, SessionOptions] =
    Either.cond(
      checkpointEvery >= 1,
      new SessionOptions(checkpointEvery),
      SessionError.CheckpointInterval(checkpointEvery)
    )

/** Why a session operation failed. Each case names what it was applied to. */
enum SessionError derives CanEqual:
  /** Another writer holds the project (E2E-19). */
  case WriterRefused(requester: LockOwner, holder: Option[LockOwner])

  /** The bundle has no `project.json` to open. */
  case NoProject

  /** `create` found a bundle that already has a manifest. */
  case ProjectExists

  case Store(operation: String, error: StoreError)
  case Bundle(operation: String, error: BundleError)
  case Journal(operation: String, error: JournalError)
  case Command(entry: JournalEntry, error: CommandError)

  /** An entry changed the document in a way the journal cannot replay. */
  case Unjournalable(entry: JournalEntry)

  /** A recovery offer for `journal` waits to be accepted or declined. */
  case RecoveryPending(journal: ByteDigest)

  /** No recovery is pending, so the answer to `journal` is refused. */
  case NoRecoveryPending(journal: ByteDigest)

  /** The answer names `answered`, but the pending journal is `pending`. */
  case OtherRecovery(answered: ByteDigest, pending: ByteDigest)

  /** An unreplayable journal cannot be accepted, only declined. */
  case NotReplayable(journal: ByteDigest, error: JournalError)

  /** A journal over a damaged manifest cannot be accepted, only declined. */
  case DamagedBase(journal: ByteDigest, problem: BundleError)

  /** The session of `owner` was closed. */
  case Closed(owner: LockOwner)

  case CheckpointInterval(requested: Int)

  def message: String = this match
    case WriterRefused(requester, holder) =>
      s"${requester.description} cannot open the project for writing: it is open in " +
        s"${holder.fold("another writer")(_.description)}. Close it there, or open a copy."
    case NoProject             => "The bundle has no project.json to open."
    case ProjectExists         => "The bundle already holds a project; open it instead."
    case Store(operation, e)   => s"Cannot $operation: ${e.message}"
    case Bundle(operation, e)  => s"Cannot $operation: ${e.message}"
    case Journal(operation, e) => s"Cannot $operation: ${e.message}"
    case Command(entry, e)     => s"${entry.productPrefix} was refused: ${e.message}"
    case Unjournalable(entry)  =>
      s"${entry.productPrefix} changed the document in a way the autosave journal cannot " +
        "replay, so it was refused."
    case RecoveryPending(journal) =>
      s"Recovered work (journal ${journal.hex.take(12)}) must be restored or set aside first."
    case NoRecoveryPending(journal) =>
      s"No recovery is pending; journal ${journal.hex.take(12)} was already answered."
    case OtherRecovery(answered, pending) =>
      s"The answer is for journal ${answered.hex.take(12)}, but journal " +
        s"${pending.hex.take(12)} is pending."
    case NotReplayable(journal, e) =>
      s"Journal ${journal.hex.take(12)} cannot be restored (${e.message}); it can only be " +
        "set aside."
    case DamagedBase(journal, problem) =>
      s"Journal ${journal.hex.take(12)} was written on a project.json that no longer opens " +
        s"(${problem.message}); it cannot be restored, only set aside."
    case Closed(owner)                 => s"The session of ${owner.description} is closed."
    case CheckpointInterval(requested) =>
      s"A checkpoint interval of $requested entries is refused; it must be at least 1."

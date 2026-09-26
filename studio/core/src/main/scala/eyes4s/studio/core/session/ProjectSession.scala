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

import cats.data.EitherT
import cats.effect.std.Mutex
import cats.effect.{Concurrent, Ref}
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.{CommandJournal, History, JournalEntry, JournalError, Step}
import eyes4s.studio.core.document.{RunLifecycle, StudioDocument}

import java.nio.charset.StandardCharsets.UTF_8

/** One writer's session on an `.eyes` bundle (tickets S2.4a and S2.4b).
  *
  * '''Single writer.''' Opening takes the bundle's [[WriterLock]] and keeps
  * it until [[close]]; a second writer is refused with
  * [[SessionError.WriterRefused]], naming the holder (E2E-19).
  *
  * '''Atomic save.''' [[save]] writes, in order: every new immutable part
  * (a part already present with the same bytes is skipped, one with other
  * bytes is refused), the last valid manifest as the
  * [[Sidecar.PreviousManifest]], the new `project.json` by compare-and-swap
  * against the manifest this session last saw, and finally a fresh journal.
  * If an autosave had failed, the journal is first rewritten whole, so a
  * crash before the swap still recovers everything; if that rewrite fails
  * too, the save goes on and its receipt reports the narrower window.
  * Parts are content-addressed and never rewritten, so until the swap the old
  * manifest's parts are untouched, and after it every part the new manifest
  * names is already stored. A crash at any point therefore reopens at the old
  * document or the new one, never a mix; the journal (below) restores the
  * old one's lost work. If a store reports a failed swap, the manifest is
  * read back: when it already holds the new bytes the save counts as done.
  *
  * '''Last valid manifest.''' When `project.json` does not open (a damaged
  * part, say) the session opens the retained previous manifest instead and
  * reports [[ManifestSource.Previous]]; the next save replaces the damaged
  * one. A journal that does not start from the previous manifest's document
  * then most likely started from the damaged one: it is reported as
  * [[Recovery.JournalOnDamagedManifest]] and kept until explicitly declined.
  *
  * '''Autosave journal.''' Each performed entry is appended to the
  * [[Sidecar.Journal]] as a [[CommandJournal]] line before [[perform]]
  * returns, with a science checkpoint every `checkpointEvery` entries. The
  * journal starts from the saved document and is reset by every save. The
  * session keeps a replay mirror (a [[History]] started on the saved
  * document), and journals exactly what that mirror can replay to the same
  * document: an undo of an edit made before the last save, which the mirror's
  * history does not hold, is journaled as its inverse command. An entry that
  * changes nothing replayable (a cancel request, whose job handle is session
  * state) is not journaled.
  *
  * '''Recovery.''' On open, a journal over the saved document with entries
  * after it becomes a [[RecoveryOffer]]: the replayed document, the entries
  * it restores, the runs that were running with no job, and the checkpoint
  * check. Replay performs entries only; it never re-issues their effects.
  * [[accept]] saves the recovered document; [[decline]] archives the journal
  * under `journals/archive/`, which is not disposable like `cache/`. Until
  * one of them, the session refuses edits and saves. A journal over an older
  * document is superseded (a save finished, its journal reset did not) and is
  * archived on open. Runs the saved document shows as running have no job any
  * more; every open lists them ([[OpenReport.orphaned]]).
  *
  * '''Cancellation.''' Each operation runs under the session's mutex, and
  * once it holds the mutex it is uncancelable: the store's writes and the
  * session state that records them happen together, so a cancelled perform
  * or save cannot leave a journaled entry unrecorded (a repeated sequence
  * number) or a swapped manifest unrecorded (a stale compare-and-swap
  * digest). Only waiting for the mutex can be cancelled. Opening and
  * creating are uncancelable once the writer lock is taken, so a cancelled
  * open never leaks the lock.
  */
final class ProjectSession[F[_]: Concurrent] private (
    store: ProjectStore[F],
    val owner: LockOwner,
    options: SessionOptions,
    mutex: Mutex[F],
    state: Ref[F, ProjectSession.State]
):
  import ProjectSession.*

  /** The document as edited, saved or not. */
  def document: F[StudioDocument] = state.get.map(_.history.document)

  /** The document with its undo histories. */
  def history: F[History] = state.get.map(_.history)

  /** The document the bundle holds, as last saved or opened. */
  def saved: F[StudioDocument] = state.get.map(_.saved.document)

  /** Whether the edited document differs from the saved one. */
  def unsaved: F[Boolean] = state.get.map(s => stripped(s.history.document) != s.saved.document)

  /** The recovery waiting for an answer, if any. */
  def pending: F[Option[Recovery]] = state.get.map(_.pending.map(_.recovery))

  /** Perform `entry` and autosave it to the journal. The step's effects are
    * returned for the caller to perform; the session performs none.
    */
  def perform(entry: JournalEntry): F[Either[SessionError, Applied]] =
    exclusive { s =>
      (for
        lock <- EitherT.fromEither[F](writable(s))
        step <- EitherT.fromEither[F](
          s.history.perform(entry).leftMap(SessionError.Command(entry, _))
        )
        replayed <- EitherT.fromEither[F](mirror(s, entry, step.history))
        outcome  <- replayed match
          case None =>
            EitherT.pure[F, SessionError](
              (s.copy(history = step.history), Applied(step, JournalWrite.NotJournaled))
            )
          case Some((journaled, next)) =>
            val seq = s.journal.seq + 1
            val cp  = seq % options.checkpointEvery == 0
            for
              line  <- journalLine(CommandJournal.entry(seq, journaled))
              check <-
                if cp then
                  journalLine(CommandJournal.checkpoint(seq, next.document)).map(Some(_))
                else EitherT.pure[F, SessionError](None)
              pendingJournal = s.journal.copy(
                lines = s.journal.lines ++ (line +: check.toVector),
                seq = seq,
                mirror = next
              )
              written <- EitherT.liftF(writeJournal(lock, pendingJournal))
            yield (
              s.copy(history = step.history, journal = written._1),
              Applied(step, written._2)
            )
      yield outcome).value
    }

  /** Save the edited document atomically (see the class comment). */
  def save: F[Either[SessionError, SaveReceipt]] =
    exclusive(s => EitherT.fromEither[F](writable(s)).flatMap(saveFrom(s, _)).value)

  /** Copy an input's bytes into the bundle and list it at the next save. */
  def importInput(
      kind: InputKind,
      name: String,
      bytes: IArray[Byte]
  ): F[Either[SessionError, InputEntry]] =
    exclusive { s =>
      (for
        lock  <- EitherT.fromEither[F](writable(s))
        entry <- EitherT(ProjectBundle.importInput(store, lock, kind, name, bytes))
          .leftMap(SessionError.Bundle(s"import $name", _))
      yield (
        s.copy(inputs = if s.inputs.contains(entry) then s.inputs else s.inputs :+ entry),
        entry
      )).value
    }

  /** Restore the offered work: save its recovered document, keeping the undo
    * history its replay rebuilt.
    */
  def accept(offer: RecoveryOffer): F[Either[SessionError, SaveReceipt]] =
    exclusive { s =>
      (for
        lock    <- EitherT.fromEither[F](held(s))
        pending <- EitherT.fromEither[F](answering(s, offer.journal))
        found   <- EitherT.fromEither[F](pending.recovery match
          case Recovery.Offered(o)               => Right(o)
          case Recovery.Unreplayable(journal, e) =>
            Left(SessionError.NotReplayable(journal, e))
          case Recovery.JournalOnDamagedManifest(journal, problem) =>
            Left(SessionError.DamagedBase(journal, problem)))
        // The journal on disk is the offer's, and replays to `found` exactly:
        // it is not rewritten before the swap.
        saved <- saveFrom(
          s.copy(history = found.history, pending = None),
          lock,
          reconcile = false
        )
      yield saved).value
    }

  /** Set the pending journal aside: archive it under `journals/archive/` and
    * keep the saved document. Returns the archive's path.
    */
  def decline(journal: ByteDigest): F[Either[SessionError, BundlePath]] =
    exclusive { s =>
      (for
        lock    <- EitherT.fromEither[F](held(s))
        pending <- EitherT.fromEither[F](answering(s, journal))
        path    <- EitherT(archive(store, lock, pending.bytes))
      yield (s.copy(pending = None), path)).value
    }

  /** Release the writer lock. The journal stays: unsaved work is offered
    * again the next time the project opens.
    */
  def close: F[Either[SessionError, Unit]] =
    exclusive { s =>
      s.lock match
        case None       => Concurrent[F].pure(Left(SessionError.Closed(owner)))
        case Some(lock) =>
          store
            .release(lock)
            .map(
              _.bimap(
                SessionError.Store("release the writer lock", _),
                _ => (s.copy(lock = None), ())
              )
            )
    }

  // -------------------------------------------------------------------------

  private def exclusive[A](
      body: State => F[Either[SessionError, (State, A)]]
  ): F[Either[SessionError, A]] =
    mutex.lock.surround(
      Concurrent[F].uncancelable(_ =>
        state.get.flatMap(body).flatMap {
          case Right((next, a)) => state.set(next).as(Right(a))
          case Left(e)          => Concurrent[F].pure(Left(e))
        }
      )
    )

  private def held(s: State): Either[SessionError, WriterLock] =
    s.lock.toRight(SessionError.Closed(owner))

  private def writable(s: State): Either[SessionError, WriterLock] =
    held(s).flatMap(lock =>
      s.pending.map(p => SessionError.RecoveryPending(p.recovery.journal)).toLeft(lock)
    )

  private def answering(s: State, journal: ByteDigest): Either[SessionError, PendingJournal] =
    s.pending match
      case None => Left(SessionError.NoRecoveryPending(journal))
      case Some(p) if p.recovery.journal == journal => Right(p)
      case Some(p) => Left(SessionError.OtherRecovery(journal, p.recovery.journal))

  private def journalLine(
      line: Either[JournalError, String]
  ): EitherT[F, SessionError, String] =
    EitherT.fromEither[F](line.leftMap(SessionError.Journal("write the journal", _)))

  /** Write the journal's new lines: appended when the file is known to hold
    * the earlier ones, else the whole journal. A failure leaves the next
    * write to rewrite it whole.
    */
  private def writeJournal(lock: WriterLock, j: JournalState): F[(JournalState, JournalWrite)] =
    val whole = j.durable == 0
    val lines = if whole then j.lines else j.lines.drop(j.durable)
    val bytes = utf8(lines.map(_ + "\n").mkString)
    val write =
      if whole then store.replaceSidecar(lock, Sidecar.Journal, bytes)
      else store.appendSidecar(lock, Sidecar.Journal, bytes)
    write.map {
      case Right(()) =>
        (
          j.copy(durable = j.lines.size),
          if whole then JournalWrite.Rewritten(lines.size)
          else JournalWrite.Appended(lines.size)
        )
      case Left(e) => (j.copy(durable = 0), JournalWrite.Failed(e))
    }

  /** The atomic save of `s`'s document; on success the state it leaves. */
  private def saveFrom(
      s: State,
      lock: WriterLock,
      reconcile: Boolean = true
  ): EitherT[F, SessionError, (State, SaveReceipt)] =
    val document = stripped(s.history.document)
    val behind   = reconcile && s.journal.durable != s.journal.lines.size
    for
      encoded <- EitherT.fromEither[F](
        ProjectBundle
          .encode(document, s.saved.manifest.sharing, s.inputs)
          .leftMap(SessionError.Bundle("encode the document", _))
      )
      digest = ByteDigest.sha256(encoded.manifestBytes)
      swap   = !s.saved.current.contains(digest)
      reconciled <-
        if swap && behind then
          EitherT.liftF[F, SessionError, Option[StoreError]](
            store
              .replaceSidecar(
                lock,
                Sidecar.Journal,
                utf8(s.journal.lines.map(_ + "\n").mkString)
              )
              .map(_.left.toOption)
          )
        else EitherT.pure[F, SessionError](Option.empty[StoreError])
      written <-
        if !swap then EitherT.pure[F, SessionError](Vector.empty[BundlePath])
        else
          encoded.parts.flatTraverse((path, bytes) =>
            EitherT(writePart(lock, path, bytes)).map(_.toVector)
          )
      _ <- (s.saved.valid, swap) match
        case (Some(valid), true) =>
          EitherT(store.replaceSidecar(lock, Sidecar.PreviousManifest, valid))
            .leftMap(SessionError.Store(s"keep the previous ${ProjectStore.ManifestName}", _))
        case _ => EitherT.pure[F, SessionError](())
      _ <-
        if swap then EitherT(swapManifest(lock, s.saved.current, encoded.manifestBytes, digest))
        else EitherT.pure[F, SessionError](())
      start <- journalLine(CommandJournal.start(document))
      reset <- EitherT.liftF(store.replaceSidecar(lock, Sidecar.Journal, utf8(start + "\n")))
    yield (
      s.copy(
        saved = Saved(document, encoded.manifest, Some(encoded.manifestBytes), Some(digest)),
        journal = JournalState(
          Vector(start),
          0,
          if reset.isRight then 1 else 0,
          History.start(document)
        )
      ),
      SaveReceipt(digest, written, swap, reconciled, reset.left.toOption)
    )

  /** Write one part unless it is already stored; `Some(path)` if written. */
  private def writePart(
      lock: WriterLock,
      path: BundlePath,
      bytes: IArray[Byte]
  ): F[Either[SessionError, Option[BundlePath]]] =
    store.read(path).flatMap {
      case Right(existing) =>
        val (was, now) = (ByteDigest.sha256(existing), ByteDigest.sha256(bytes))
        Concurrent[F].pure(
          Either.cond(
            was == now,
            None,
            SessionError.Bundle(s"write $path", BundleError.Collision(path, was, now))
          )
        )
      case Left(StoreError.Missing(_)) =>
        store
          .write(lock, path, bytes)
          .map(_.bimap(SessionError.Store(s"write $path", _), _ => Some(path)))
      case Left(e) => Concurrent[F].pure(Left(SessionError.Store(s"read $path", e)))
    }

  /** Compare-and-swap the manifest. A reported failure is checked by reading
    * the manifest back: if it already holds `next`, the swap happened.
    */
  private def swapManifest(
      lock: WriterLock,
      expected: Option[ByteDigest],
      next: IArray[Byte],
      digest: ByteDigest
  ): F[Either[SessionError, Unit]] =
    store.swapManifest(lock, expected, next).flatMap {
      case Right(()) => Concurrent[F].pure(Right(()))
      case Left(e)   =>
        store.readManifest.map {
          case Right(bytes) if ByteDigest.sha256(bytes) == digest => Right(())
          case _ => Left(SessionError.Store(s"swap ${ProjectStore.ManifestName}", e))
        }
    }

object ProjectSession:

  /** A session and what opening it found. */
  final case class Opened[F[_]](session: ProjectSession[F], report: OpenReport)

  /** The saved state: the document the bundle holds (without job handles),
    * its manifest, the bytes of the last manifest known to open (`None` for
    * a bundle not yet saved), and the digest of the store's current
    * `project.json` (`None`: there is none), which the next swap expects.
    */
  private[session] final case class Saved(
      document: StudioDocument,
      manifest: ProjectManifest,
      valid: Option[IArray[Byte]],
      current: Option[ByteDigest]
  )

  /** The session's journal: its lines (start line first), the number of
    * entries, how many lines are known to be in the file (0: rewrite it
    * whole), and the replay mirror.
    */
  private[session] final case class JournalState(
      lines: Vector[String],
      seq: Int,
      durable: Int,
      mirror: History
  )

  private[session] final case class PendingJournal(recovery: Recovery, bytes: IArray[Byte])

  private[session] final case class State(
      history: History,
      saved: Saved,
      inputs: Vector[InputEntry],
      journal: JournalState,
      pending: Option[PendingJournal],
      lock: Option[WriterLock]
  )

  /** Open the bundle in `store` for writing as `owner`. */
  def open[F[_]: Concurrent](
      store: ProjectStore[F],
      owner: LockOwner,
      options: SessionOptions = SessionOptions.default
  ): F[Either[SessionError, Opened[F]]] =
    locked(store, owner)(lock =>
      (for
        bytes <- EitherT(store.readManifest).leftMap {
          case StoreError.NoManifest => SessionError.NoProject
          case e => SessionError.Store(s"read ${ProjectStore.ManifestName}", e)
        }
        chosen <- EitherT(ProjectBundle.openFrom(store, bytes).flatMap {
          case Right(opened) =>
            Concurrent[F].pure(Right((ManifestSource.Current, opened, bytes)))
          case Left(problem) =>
            val refused = SessionError.Bundle(s"open ${ProjectStore.ManifestName}", problem)
            store.readSidecar(Sidecar.PreviousManifest).flatMap {
              case Left(_)     => Concurrent[F].pure(Left(refused))
              case Right(prev) =>
                ProjectBundle
                  .openFrom(store, prev)
                  .map(_.bimap(_ => refused, (ManifestSource.Previous(problem), _, prev)))
            }
        })
        (source, opened, valid) = chosen
        base                    = opened.document
        start <- EitherT.fromEither[F](
          CommandJournal.start(base).leftMap(SessionError.Journal("start the journal", _))
        )
        finding <- EitherT(inspect(store, lock, base, source))
        session <- EitherT.liftF(
          build(
            store,
            owner,
            options,
            State(
              History.start(base),
              Saved(base, opened.manifest, Some(valid), Some(ByteDigest.sha256(bytes))),
              opened.manifest.inputs,
              JournalState(Vector(start), 0, 0, History.start(base)),
              finding._2,
              Some(lock)
            )
          )
        )
      yield Opened(
        session,
        OpenReport(
          source,
          opened.science,
          finding._1,
          base.running.map(r => UnsubmittedRun(r.id, r.analysis, r.dataset, sinceSave = false))
        )
      )).value
    )

  /** Create a project in the empty bundle `store` holding `document` (its job
    * handles are not saved), with `sharing` and the listed `inputs`, and
    * open it for writing as `owner`.
    */
  def create[F[_]: Concurrent](
      store: ProjectStore[F],
      owner: LockOwner,
      document: StudioDocument,
      sharing: SharingOptions,
      inputs: Vector[InputEntry],
      options: SessionOptions = SessionOptions.default
  ): F[Either[SessionError, ProjectSession[F]]] =
    locked(store, owner)(lock =>
      (for
        _ <- EitherT(store.readManifest.map {
          case Left(StoreError.NoManifest) => Right(())
          case Left(e)  => Left(SessionError.Store(s"read ${ProjectStore.ManifestName}", e))
          case Right(_) => Left(SessionError.ProjectExists)
        })
        base = stripped(document)
        encoded <- EitherT.fromEither[F](
          ProjectBundle
            .encode(base, sharing, inputs)
            .leftMap(SessionError.Bundle("encode the document", _))
        )
        start <- EitherT.fromEither[F](
          CommandJournal.start(base).leftMap(SessionError.Journal("start the journal", _))
        )
        session <- EitherT.liftF(
          build(
            store,
            owner,
            options,
            State(
              History.start(base),
              Saved(base, encoded.manifest, None, None),
              inputs,
              JournalState(Vector(start), 0, 0, History.start(base)),
              None,
              Some(lock)
            )
          )
        )
        // The first save writes everything; the session is then at `base`.
        _ <- EitherT(session.save)
      yield session).value
    )

  private def build[F[_]: Concurrent](
      store: ProjectStore[F],
      owner: LockOwner,
      options: SessionOptions,
      initial: State
  ): F[ProjectSession[F]] =
    (Mutex[F], Ref.of[F, State](initial)).mapN(new ProjectSession(store, owner, options, _, _))

  /** Run `body` holding a new writer lock, releasing it if `body` fails. */
  private def locked[F[_]: Concurrent, A](store: ProjectStore[F], owner: LockOwner)(
      body: WriterLock => F[Either[SessionError, A]]
  ): F[Either[SessionError, A]] =
    Concurrent[F].uncancelable(_ => acquireThen(store, owner)(body))

  private def acquireThen[F[_]: Concurrent, A](store: ProjectStore[F], owner: LockOwner)(
      body: WriterLock => F[Either[SessionError, A]]
  ): F[Either[SessionError, A]] =
    store.acquire(owner).flatMap {
      case Left(StoreError.Locked(requester, holder)) =>
        Concurrent[F].pure(Left(SessionError.WriterRefused(requester, holder)))
      case Left(e) => Concurrent[F].pure(Left(SessionError.Store("take the writer lock", e)))
      case Right(lock) =>
        body(lock)
          .flatTap {
            case Left(_)  => store.release(lock).void
            case Right(_) => Concurrent[F].unit
          }
          .onError { case _ => store.release(lock).attempt.void }
    }

  /** Read the journal against the saved document `base`, opened from
    * `source`.
    */
  private def inspect[F[_]: Concurrent](
      store: ProjectStore[F],
      lock: WriterLock,
      base: StudioDocument,
      source: ManifestSource
  ): F[Either[SessionError, (JournalFinding, Option[PendingJournal])]] =
    store.readSidecar(Sidecar.Journal).flatMap {
      case Left(StoreError.NoSidecar(_)) =>
        Concurrent[F].pure(Right((JournalFinding.Clean, None)))
      case Left(e)      => Concurrent[F].pure(Left(SessionError.Store("read the journal", e)))
      case Right(bytes) =>
        val digest               = ByteDigest.sha256(bytes)
        def pending(r: Recovery) =
          Right((JournalFinding.Pending(r), Some(PendingJournal(r, bytes))))
        CommandJournal.replay(base, String(Array.from(bytes), UTF_8)) match
          case Left(JournalError.BaseMismatch(_, _)) =>
            source match
              // A journal holding only its start line holds no work.
              case ManifestSource.Previous(_) if entryLines(bytes) == 0 =>
                Concurrent[F].pure(Right((JournalFinding.Clean, None)))
              case ManifestSource.Previous(problem) =>
                Concurrent[F].pure(pending(Recovery.JournalOnDamagedManifest(digest, problem)))
              case ManifestSource.Current =>
                archive(store, lock, bytes).map(
                  _.map(p => (JournalFinding.Superseded(p), None))
                )
          case Left(JournalError.Empty) =>
            Concurrent[F].pure(Right((JournalFinding.Clean, None)))
          case Left(e) => Concurrent[F].pure(pending(Recovery.Unreplayable(digest, e)))
          case Right(replay) if replay.entries.isEmpty =>
            Concurrent[F].pure(Right((JournalFinding.Clean, None)))
          case Right(replay) =>
            val recovered = replay.history.document
            val offer     = RecoveryOffer(
              digest,
              base,
              recovered,
              replay.history,
              replay.entries.zipWithIndex.map((e, i) => RestoreItem(i + 1, e)),
              recovered.running.map(r =>
                UnsubmittedRun(
                  r.id,
                  r.analysis,
                  r.dataset,
                  !base.run(r.id).exists(_.state == RunLifecycle.Running)
                )
              ),
              replay.checkpoints.lastOption.fold(DriftCheck.NoCheckpoint(replay.entries.size))(
                last => DriftCheck.Checked(replay.checkpoints, replay.entries.size - last)
              ),
              replay.torn
            )
            Concurrent[F].pure(pending(Recovery.Offered(offer)))
    }

  /** Archive journal bytes at `journals/archive/<sha256>.jsonl`, then remove
    * the journal. The copy is written first, so a crash between the two
    * leaves the journal to be found again; archiving the same bytes again
    * writes nothing.
    */
  private def archive[F[_]: Concurrent](
      store: ProjectStore[F],
      lock: WriterLock,
      bytes: IArray[Byte]
  ): F[Either[SessionError, BundlePath]] =
    (for
      path <- EitherT.fromEither[F](
        BundlePath
          .in(BundleArea.Journals, s"archive/${ByteDigest.sha256(bytes).hex}.jsonl")
          .leftMap(SessionError.Bundle("name the journal archive", _))
      )
      _ <- EitherT(ProjectBundle.writeImmutable(store, lock, path, bytes))
        .leftMap(SessionError.Bundle("archive the journal", _))
      _ <- EitherT(store.removeSidecar(lock, Sidecar.Journal))
        .recover { case StoreError.NoSidecar(_) =>
          ()
        }
        .leftMap(SessionError.Store("remove the archived journal", _))
    yield path).value

  /** The replay mirror's answer for `entry`, whose live result is `live`:
    * the entry to journal and the mirror after it, `None` if nothing
    * replayable changed, or an error if the change cannot be journaled.
    */
  private def mirror(
      s: State,
      entry: JournalEntry,
      live: History
  ): Either[SessionError, Option[(JournalEntry, History)]] =
    val target                                               = stripped(live.document)
    val before                                               = s.history
    def on(e: JournalEntry): Option[(JournalEntry, History)] =
      s.journal.mirror.perform(e).toOption.collect {
        case Step(h, _) if h.document == target => (e, h)
      }
    // An undo or redo the mirror's shorter history cannot reach is the
    // command the live history applied.
    val fallback = entry match
      case JournalEntry.Undo =>
        before.science.done.headOption.map(u => JournalEntry.Apply(u.inverse))
      case JournalEntry.UndoView =>
        before.presentation.done.headOption.map(u => JournalEntry.Apply(u.inverse))
      case JournalEntry.Redo =>
        before.science.undone.headOption.map(u => JournalEntry.Apply(u.command))
      case JournalEntry.RedoView =>
        before.presentation.undone.headOption.map(u => JournalEntry.Apply(u.command))
      case JournalEntry.Apply(_) => None
    on(entry).orElse(fallback.flatMap(on)) match
      case some @ Some(_)                              => Right(some)
      case None if stripped(before.document) == target => Right(None)
      case None => Left(SessionError.Unjournalable(entry))

  /** The journal's lines after its start line, torn ones included. */
  private def entryLines(bytes: IArray[Byte]): Int =
    String(Array.from(bytes), UTF_8).split('\n').count(_.nonEmpty) - 1

  /** The document without its session-only job handles. */
  private def stripped(d: StudioDocument): StudioDocument =
    d.withJobs(Vector.empty).getOrElse(d)

  private def utf8(text: String): IArray[Byte] = IArray.unsafeFromArray(text.getBytes(UTF_8))

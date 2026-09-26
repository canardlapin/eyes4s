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

package eyes4s.studio.core.bundle

import cats.effect.{Ref, Sync}
import cats.syntax.all.*
import eyes4s.codec.ByteDigest

/** Who holds, or asks for, a bundle's writer lock, as a person would
  * recognise it ("Eyes Studio on lab-mac, process 4121"). A refused writer
  * is shown this text.
  */
final case class LockOwner private (description: String) derives CanEqual

object LockOwner:
  def of(description: String): Either[BundleError, LockOwner] =
    if description.trim.isEmpty then Left(BundleError.BlankLockOwner)
    else Right(new LockOwner(description.trim))

/** The capability to write one bundle: issued by [[ProjectStore.acquire]] and
  * valid until [[ProjectStore.release]]. A store accepts a lock only while it
  * is the one that store issued last and has not released; the token is what
  * it compares, so a lock of another store, or a released one, is refused.
  */
final class WriterLock private (val owner: LockOwner, val token: String):
  override def equals(other: Any): Boolean = other match
    case that: WriterLock => token == that.token && owner == that.owner
    case _                => false
  override def hashCode: Int    = token.hashCode
  override def toString: String = s"WriterLock(${owner.description})"

object WriterLock:
  given CanEqual[WriterLock, WriterLock] = CanEqual.derived

  /** A lock with this owner and token; for [[ProjectStore]] implementations. */
  def issue(owner: LockOwner, token: String): WriterLock = new WriterLock(owner, token)

/** A store-kept file beside the manifest (tickets S2.4a and S2.4b).
  * Sidecars are not bundle entries: no [[BundlePath]] names them, `list`
  * never returns them and the manifest never lists them. They hold what a
  * writing session keeps for recovery, not the document.
  *
  *  - `Journal`, `project.journal`: the autosave command journal of the
  *    session writing the bundle, appended to as the user works (S2.4b);
  *  - `PreviousManifest`, `project.previous.json`: the last manifest that
  *    opened valid before the current one was swapped in (S2.4a).
  */
enum Sidecar(val fileName: String) derives CanEqual:
  case Journal          extends Sidecar("project.journal")
  case PreviousManifest extends Sidecar("project.previous.json")

/** Why a store operation failed. Each case names what it was applied to. */
enum StoreError derives CanEqual:
  case Missing(path: BundlePath)
  case NoManifest
  case NoSidecar(file: Sidecar)
  case Unreadable(target: String, reason: String)
  case Unwritable(target: String, reason: String)

  /** Another writer holds the lock; `holder` is its owner, when readable. */
  case Locked(requester: LockOwner, holder: Option[LockOwner])

  /** The presented lock is not the store's current lock. */
  case NotHolder(presented: LockOwner, holder: Option[LockOwner])

  /** A compare-and-swap of the manifest found other bytes than expected
    * (another save got there first); digests are of the whole manifest.
    */
  case ManifestMoved(expected: Option[ByteDigest], found: Option[ByteDigest])

  def message: String = this match
    case Missing(path)              => s"The bundle has no entry $path."
    case NoManifest                 => s"The bundle has no ${ProjectStore.ManifestName}."
    case NoSidecar(file)            => s"The bundle has no ${file.fileName}."
    case Unreadable(target, reason) => s"Cannot read $target: $reason."
    case Unwritable(target, reason) => s"Cannot write $target: $reason."
    case Locked(requester, holder)  =>
      s"${requester.description} cannot open the project for writing: it is open in " +
        s"${holder.fold("another writer")(_.description)}."
    case NotHolder(presented, holder) =>
      s"The writer lock of ${presented.description} is not this project's current lock" +
        holder.fold(" (no writer holds it).")(h => s" (${h.description} holds it).")
    case ManifestMoved(expected, found) =>
      s"The manifest changed under this save: expected " +
        s"${expected.fold("no manifest")(_.hex)}, found ${found.fold("no manifest")(_.hex)}."

/** Storage for one `.eyes` bundle (DESIGN_SPEC section 13: a platform service
  * behind an interface). studio-core names no file system; the JVM
  * implementation lives in studio-desktop, [[InMemoryProjectStore]] is the
  * portable one, and both pass the same conformance suite.
  *
  * The contract:
  *
  *  - '''Entries''' are addressed by [[BundlePath]], so nothing outside the
  *    bundle's areas can be named. `read` returns exactly the bytes of the
  *    last completed `write`; a reader never observes a partly written entry.
  *    `list` returns every entry, in path order, and never the manifest or a
  *    store's own files.
  *  - '''The manifest''' (`project.json`) is changed only by `swapManifest`,
  *    an atomic compare-and-swap: it replaces the manifest if and only if the
  *    current one has digest `expected` (`None`: there is none), and a reader
  *    sees either the old manifest or the new one, whole. On
  *    [[StoreError.ManifestMoved]] nothing changes.
  *  - '''The writer lock''' is exclusive per bundle, across every store
  *    instance on the same bundle: `acquire` while another lock is held is
  *    refused with [[StoreError.Locked]] naming the holder. Every mutation
  *    takes the current lock and is refused with [[StoreError.NotHolder]]
  *    otherwise. Reads need no lock.
  *  - '''Sidecars''' ([[Sidecar]]) are the store's files beside the
  *    manifest. `replaceSidecar` is atomic like the manifest swap: a reader
  *    sees the old bytes or the new, whole. `appendSidecar` adds bytes at the
  *    end and is durable when it returns; a crash during it may leave a
  *    prefix of the appended bytes (a torn tail) but never changes the bytes
  *    before them. Both need the current lock, as does `removeSidecar`.
  *
  * Atomic save (write every new immutable entry, keep the last valid
  * manifest, then swap the manifest) is built on this contract by
  * `eyes4s.studio.core.session.ProjectSession` (S2.4a).
  */
trait ProjectStore[F[_]]:
  def read(path: BundlePath): F[Either[StoreError, IArray[Byte]]]
  def list: F[Either[StoreError, Vector[BundlePath]]]
  def write(
      lock: WriterLock,
      path: BundlePath,
      bytes: IArray[Byte]
  ): F[Either[StoreError, Unit]]
  def delete(lock: WriterLock, path: BundlePath): F[Either[StoreError, Unit]]
  def readManifest: F[Either[StoreError, IArray[Byte]]]
  def swapManifest(
      lock: WriterLock,
      expected: Option[ByteDigest],
      next: IArray[Byte]
  ): F[Either[StoreError, Unit]]
  def acquire(owner: LockOwner): F[Either[StoreError, WriterLock]]
  def release(lock: WriterLock): F[Either[StoreError, Unit]]
  def readSidecar(file: Sidecar): F[Either[StoreError, IArray[Byte]]]
  def appendSidecar(
      lock: WriterLock,
      file: Sidecar,
      bytes: IArray[Byte]
  ): F[Either[StoreError, Unit]]
  def replaceSidecar(
      lock: WriterLock,
      file: Sidecar,
      bytes: IArray[Byte]
  ): F[Either[StoreError, Unit]]
  def removeSidecar(lock: WriterLock, file: Sidecar): F[Either[StoreError, Unit]]

object ProjectStore:
  /** The manifest's name at the bundle root. */
  val ManifestName: String = "project.json"

/** A bundle held in memory: the portable [[ProjectStore]], for tests and for
  * a host without a file system. One instance is one bundle.
  */
object InMemoryProjectStore:
  private final case class State(
      entries: Map[BundlePath, IArray[Byte]],
      manifest: Option[IArray[Byte]],
      sidecars: Map[Sidecar, IArray[Byte]],
      lock: Option[WriterLock],
      issued: Long
  )

  def create[F[_]: Sync]: F[ProjectStore[F]] =
    Ref.of[F, State](State(Map.empty, None, Map.empty, None, 0L)).map(new Store(_))

  private final class Store[F[_]: Sync](state: Ref[F, State]) extends ProjectStore[F]:
    private def held(s: State, lock: WriterLock): Either[StoreError, Unit] =
      Either.cond(
        s.lock.contains(lock),
        (),
        StoreError.NotHolder(lock.owner, s.lock.map(_.owner))
      )

    private def mutate(lock: WriterLock)(
        f: State => Either[StoreError, State]
    ): F[Either[StoreError, Unit]] =
      state.modify(s =>
        held(s, lock).flatMap(_ => f(s)).fold(e => (s, Left(e)), n => (n, Right(())))
      )

    def read(path: BundlePath): F[Either[StoreError, IArray[Byte]]] =
      state.get.map(_.entries.get(path).toRight(StoreError.Missing(path)))

    def list: F[Either[StoreError, Vector[BundlePath]]] =
      state.get.map(s => Right(s.entries.keys.toVector.sorted))

    def write(
        lock: WriterLock,
        path: BundlePath,
        bytes: IArray[Byte]
    ): F[Either[StoreError, Unit]] =
      mutate(lock)(s => Right(s.copy(entries = s.entries.updated(path, bytes))))

    def delete(lock: WriterLock, path: BundlePath): F[Either[StoreError, Unit]] =
      mutate(lock)(s =>
        Either.cond(
          s.entries.contains(path),
          s.copy(entries = s.entries - path),
          StoreError.Missing(path)
        )
      )

    def readManifest: F[Either[StoreError, IArray[Byte]]] =
      state.get.map(_.manifest.toRight(StoreError.NoManifest))

    def swapManifest(
        lock: WriterLock,
        expected: Option[ByteDigest],
        next: IArray[Byte]
    ): F[Either[StoreError, Unit]] =
      mutate(lock) { s =>
        val found = s.manifest.map(ByteDigest.sha256)
        Either.cond(
          found == expected,
          s.copy(manifest = Some(next)),
          StoreError.ManifestMoved(expected, found)
        )
      }

    def acquire(owner: LockOwner): F[Either[StoreError, WriterLock]] =
      state.modify { s =>
        s.lock match
          case Some(current) => (s, Left(StoreError.Locked(owner, Some(current.owner))))
          case None          =>
            val lock = WriterLock.issue(owner, s"memory-${s.issued + 1}")
            (s.copy(lock = Some(lock), issued = s.issued + 1), Right(lock))
      }

    def release(lock: WriterLock): F[Either[StoreError, Unit]] =
      mutate(lock)(s => Right(s.copy(lock = None)))

    def readSidecar(file: Sidecar): F[Either[StoreError, IArray[Byte]]] =
      state.get.map(_.sidecars.get(file).toRight(StoreError.NoSidecar(file)))

    def appendSidecar(
        lock: WriterLock,
        file: Sidecar,
        bytes: IArray[Byte]
    ): F[Either[StoreError, Unit]] =
      mutate(lock)(s =>
        Right(
          s.copy(sidecars =
            s.sidecars.updated(file, s.sidecars.getOrElse(file, IArray.empty[Byte]) ++ bytes)
          )
        )
      )

    def replaceSidecar(
        lock: WriterLock,
        file: Sidecar,
        bytes: IArray[Byte]
    ): F[Either[StoreError, Unit]] =
      mutate(lock)(s => Right(s.copy(sidecars = s.sidecars.updated(file, bytes))))

    def removeSidecar(lock: WriterLock, file: Sidecar): F[Either[StoreError, Unit]] =
      mutate(lock)(s =>
        Either.cond(
          s.sidecars.contains(file),
          s.copy(sidecars = s.sidecars - file),
          StoreError.NoSidecar(file)
        )
      )

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

package eyes4s.studio.desktop.platform

import cats.effect.Sync
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.{
  BundlePath,
  LockOwner,
  ProjectStore,
  Sidecar,
  StoreError,
  WriterLock
}

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption, StandardOpenOption}
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** An `.eyes` bundle as a directory: the JVM [[ProjectStore]] (ticket S2.3).
  *
  * '''Paths.''' Entries are files under `root` at their bundle paths. Every
  * path component below the bundle's real root is checked before anything is
  * created or read: a symbolic link anywhere on the way (a linked `runs/`, a
  * linked file) is refused, so no operation reaches outside the bundle through
  * a link. [[BundlePath]] cannot name a hidden file, so the store's own files
  * (`.lock`, `.lock.owner`, staged writes) are never entries.
  *
  * '''Durability and crash windows.''' A write goes to a hidden staging file
  * in the target's directory, which is flushed (`FileChannel.force`), moved
  * into place atomically, and followed by a flush of every directory from the
  * target's up to the bundle root. `ProjectBundle.save` writes every part
  * this way before it swaps the manifest, and the swap is the same staged,
  * flushed, atomic move. So a crash before the manifest's move leaves the old
  * manifest, whose parts are untouched (new parts are unreferenced orphans); a
  * crash after it leaves either manifest whole, and every part the new one
  * names was durable before it was written. A reader never sees a partial
  * entry or manifest. On macOS `force` is `fsync`, which does not flush the
  * drive's own cache (`F_FULLFSYNC`); that power-loss window is the
  * platform's.
  *
  * '''Sidecars''' (S2.4a/b) are files at the bundle root beside
  * `project.json`. Replacing one is staged and moved like the manifest.
  * Appending writes in place and flushes before returning, so a crash can
  * leave only a torn tail of the last append, which the journal's reader
  * tolerates.
  *
  * '''The writer lock''' is an OS file lock on `.lock`. POSIX drops a
  * process's locks on a file when any descriptor on it closes, so each JVM
  * keeps exactly one channel per bundle, held from acquire to release in a
  * process-wide registry, and no other code opens `.lock`. Contention in the
  * same process is answered from the registry without touching the file;
  * another process is refused by the OS lock. The holder's description is
  * kept in `.lock.owner` for refused writers to name.
  */
final class FileProjectStore[F[_]: Sync] private (root: Path) extends ProjectStore[F]:
  import FileProjectStore.*

  private def blocking[A](target: String, reading: Boolean)(
      body: => Either[StoreError, A]
  ): F[Either[StoreError, A]] =
    Sync[F].blocking {
      try body
      catch
        case NonFatal(e) =>
          val reason = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
          Left(failure(target, reading, reason))
    }

  private def failure(target: String, reading: Boolean, reason: String): StoreError =
    if reading then StoreError.Unreadable(target, reason)
    else StoreError.Unwritable(target, reason)

  /** The bundle's real root, if it exists. */
  private def realRoot: Option[Path] =
    if Files.isDirectory(root) then Some(root.toRealPath()) else None

  /** The file for `segments` under `base`, refusing any component that is a
    * symbolic link. Checked before any directory is created.
    */
  private def contained(
      base: Path,
      target: String,
      segments: Vector[String],
      reading: Boolean
  ): Either[StoreError, Path] =
    segments.foldLeft[Either[StoreError, Path]](Right(base)) { (at, segment) =>
      at.flatMap { dir =>
        val next = dir.resolve(segment)
        if Files.isSymbolicLink(next) then
          Left(
            failure(
              target,
              reading,
              s"${base.relativize(next)} is a symbolic link, which could lead outside the bundle"
            )
          )
        else Right(next)
      }
    }

  private def readFile(file: Path, target: String): Either[StoreError, Option[IArray[Byte]]] =
    if !Files.exists(file, LinkOption.NOFOLLOW_LINKS) then Right(None)
    else if !Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) then
      Left(StoreError.Unreadable(target, "it is not a regular file"))
    else Right(Some(IArray.unsafeFromArray(Files.readAllBytes(file))))

  /** Stage, flush, move into place, then flush each directory up to `base`. */
  private def stage(base: Path, file: Path, bytes: IArray[Byte]): Unit =
    Files.createDirectories(file.getParent)
    val staged = file.resolveSibling(s".${file.getFileName}.$StagePrefix${UUID.randomUUID()}")
    try
      val channel =
        FileChannel.open(staged, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
      try
        val buffer = ByteBuffer.wrap(Array.from(bytes))
        while buffer.hasRemaining do channel.write(buffer): Unit
        channel.force(true)
      finally channel.close()
      Files.move(
        staged,
        file,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
      Iterator
        .iterate(file.getParent)(_.getParent)
        .takeWhile(d => d != null && d.startsWith(base))
        .foreach(syncDirectory)
    finally Files.deleteIfExists(staged): Unit

  /** Run `body` with the bundle's real root if `lock` is its current lock. */
  private def holding(lock: WriterLock, target: String)(
      body: Path => Either[StoreError, Unit]
  ): F[Either[StoreError, Unit]] =
    blocking(target, reading = false) {
      realRoot.flatMap(r => Registry.current(r).map(r -> _)) match
        case Some((r, h)) if h.lock == lock => body(r)
        case Some((_, h)) => Left(StoreError.NotHolder(lock.owner, Some(h.lock.owner)))
        case None         => Left(StoreError.NotHolder(lock.owner, realRoot.flatMap(ownerOf)))
    }

  def read(path: BundlePath): F[Either[StoreError, IArray[Byte]]] =
    blocking(path.value, reading = true) {
      realRoot match
        case None    => Left(StoreError.Missing(path))
        case Some(r) =>
          contained(r, path.value, path.segments, reading = true)
            .flatMap(readFile(_, path.value))
            .flatMap(_.toRight(StoreError.Missing(path)))
    }

  def list: F[Either[StoreError, Vector[BundlePath]]] =
    blocking(".", reading = true) {
      realRoot match
        case None    => Right(Vector.empty)
        case Some(r) =>
          val walk = Files.walk(r)
          try
            Right(
              walk
                .iterator()
                .asScala
                .filter(Files.isRegularFile(_, LinkOption.NOFOLLOW_LINKS))
                .flatMap(f =>
                  BundlePath.of(r.relativize(f).iterator().asScala.mkString("/")).toOption
                )
                .toVector
                .sorted
            )
          finally walk.close()
    }

  def write(
      lock: WriterLock,
      path: BundlePath,
      bytes: IArray[Byte]
  ): F[Either[StoreError, Unit]] =
    holding(lock, path.value)(r =>
      contained(r, path.value, path.segments, reading = false).map(stage(r, _, bytes))
    )

  def delete(lock: WriterLock, path: BundlePath): F[Either[StoreError, Unit]] =
    holding(lock, path.value) { r =>
      contained(r, path.value, path.segments, reading = false).flatMap { f =>
        if !Files.exists(f, LinkOption.NOFOLLOW_LINKS) then Left(StoreError.Missing(path))
        else
          Files.delete(f)
          // Remove directories the deletion emptied, up to the bundle root.
          Iterator
            .iterate(f.getParent)(_.getParent)
            .takeWhile(d => d != null && d.startsWith(r) && d != r)
            .takeWhile(emptyDirectory)
            .foreach(Files.delete(_))
          syncDirectory(r)
          Right(())
      }
    }

  def readManifest: F[Either[StoreError, IArray[Byte]]] =
    blocking(ProjectStore.ManifestName, reading = true) {
      realRoot match
        case None    => Left(StoreError.NoManifest)
        case Some(r) =>
          contained(
            r,
            ProjectStore.ManifestName,
            Vector(ProjectStore.ManifestName),
            reading = true
          )
            .flatMap(readFile(_, ProjectStore.ManifestName))
            .flatMap(_.toRight(StoreError.NoManifest))
    }

  def swapManifest(
      lock: WriterLock,
      expected: Option[ByteDigest],
      next: IArray[Byte]
  ): F[Either[StoreError, Unit]] =
    holding(lock, ProjectStore.ManifestName) { r =>
      for
        file <- contained(
          r,
          ProjectStore.ManifestName,
          Vector(ProjectStore.ManifestName),
          reading = false
        )
        current <- readFile(file, ProjectStore.ManifestName)
        found = current.map(ByteDigest.sha256)
        _ <- Either.cond(found == expected, (), StoreError.ManifestMoved(expected, found))
      yield stage(r, file, next)
    }

  /** The sidecar's file at the bundle root, refused if it is a link. */
  private def sidecarFile(r: Path, file: Sidecar, reading: Boolean): Either[StoreError, Path] =
    contained(r, file.fileName, Vector(file.fileName), reading)

  def readSidecar(file: Sidecar): F[Either[StoreError, IArray[Byte]]] =
    blocking(file.fileName, reading = true) {
      realRoot match
        case None    => Left(StoreError.NoSidecar(file))
        case Some(r) =>
          sidecarFile(r, file, reading = true)
            .flatMap(readFile(_, file.fileName))
            .flatMap(_.toRight(StoreError.NoSidecar(file)))
    }

  /** Appended in place and flushed; a new file's directory is flushed too.
    * A crash mid-append can leave a prefix of `bytes` at the end.
    */
  def appendSidecar(
      lock: WriterLock,
      file: Sidecar,
      bytes: IArray[Byte]
  ): F[Either[StoreError, Unit]] =
    holding(lock, file.fileName) { r =>
      sidecarFile(r, file, reading = false).flatMap { f =>
        if Files.exists(f, LinkOption.NOFOLLOW_LINKS) &&
          !Files.isRegularFile(f, LinkOption.NOFOLLOW_LINKS)
        then Left(StoreError.Unwritable(file.fileName, "it is not a regular file"))
        else
          val created = !Files.exists(f, LinkOption.NOFOLLOW_LINKS)
          val channel = FileChannel.open(
            f,
            StandardOpenOption.CREATE,
            StandardOpenOption.WRITE,
            StandardOpenOption.APPEND,
            LinkOption.NOFOLLOW_LINKS
          )
          try
            val buffer = ByteBuffer.wrap(Array.from(bytes))
            while buffer.hasRemaining do channel.write(buffer): Unit
            channel.force(true)
          finally channel.close()
          if created then syncDirectory(r)
          Right(())
      }
    }

  /** Staged, flushed and moved into place, like the manifest. */
  def replaceSidecar(
      lock: WriterLock,
      file: Sidecar,
      bytes: IArray[Byte]
  ): F[Either[StoreError, Unit]] =
    holding(lock, file.fileName)(r =>
      sidecarFile(r, file, reading = false).map(stage(r, _, bytes))
    )

  def removeSidecar(lock: WriterLock, file: Sidecar): F[Either[StoreError, Unit]] =
    holding(lock, file.fileName) { r =>
      sidecarFile(r, file, reading = false).flatMap { f =>
        if !Files.exists(f, LinkOption.NOFOLLOW_LINKS) then Left(StoreError.NoSidecar(file))
        else
          Files.delete(f)
          syncDirectory(r)
          Right(())
      }
    }

  def acquire(owner: LockOwner): F[Either[StoreError, WriterLock]] =
    blocking(LockName, reading = false) {
      Files.createDirectories(root)
      Registry.acquire(root.toRealPath(), owner)
    }

  def release(lock: WriterLock): F[Either[StoreError, Unit]] =
    blocking(LockName, reading = false) {
      realRoot match
        case None    => Left(StoreError.NotHolder(lock.owner, None))
        case Some(r) => Registry.release(r, lock)
    }

object FileProjectStore:
  /** The lock file's name at the bundle root. Only the registry opens it. */
  val LockName: String = ".lock"

  /** The holder's description, beside the lock file. */
  val OwnerName: String = ".lock.owner"

  private val StagePrefix = "staged-"

  /** A store on the bundle directory `root`, created on the first acquire. */
  def at[F[_]: Sync](root: Path): F[ProjectStore[F]] =
    Sync[F].delay(new FileProjectStore(root.toAbsolutePath.normalize))

  private def emptyDirectory(d: Path): Boolean =
    val entries = Files.list(d)
    try !entries.iterator().hasNext
    finally entries.close()

  private def syncDirectory(d: Path): Unit =
    val channel = FileChannel.open(d, StandardOpenOption.READ)
    try channel.force(true)
    finally channel.close()

  /** The description in `.lock.owner`, if any. */
  private def ownerOf(root: Path): Option[LockOwner] =
    try
      val file = root.resolve(OwnerName)
      if Files.isRegularFile(file, LinkOption.NOFOLLOW_LINKS) then
        LockOwner.of(String(Files.readAllBytes(file), UTF_8)).toOption
      else None
    catch case NonFatal(_) => None

  /** Every bundle lock this JVM holds, one open channel each, by real root. */
  private object Registry:
    final case class Held(lock: WriterLock, channel: FileChannel, fileLock: FileLock)

    private val held   = scala.collection.mutable.Map.empty[Path, Held]
    private val strays = scala.collection.mutable.ArrayBuffer.empty[FileChannel]

    def current(root: Path): Option[Held] = synchronized(held.get(root))

    def acquire(root: Path, owner: LockOwner): Either[StoreError, WriterLock] = synchronized {
      held.get(root) match
        case Some(h) => Left(StoreError.Locked(owner, Some(h.lock.owner)))
        case None    =>
          val channel = FileChannel.open(
            root.resolve(LockName),
            StandardOpenOption.CREATE,
            StandardOpenOption.WRITE
          )
          val fileLock =
            try Right(Option(channel.tryLock()))
            catch
              // Another class loader in this JVM holds it. Closing this channel
              // could drop that lock (POSIX), so the channel is kept open.
              case _: OverlappingFileLockException =>
                strays += channel
                Left(StoreError.Locked(owner, ownerOf(root)))
              case NonFatal(e) =>
                channel.close()
                throw e
          fileLock.flatMap {
            case None =>
              // No lock of this JVM is on the file (the registry has none), so
              // closing this channel releases nothing.
              channel.close()
              Left(StoreError.Locked(owner, ownerOf(root)))
            case Some(fl) =>
              val lock = WriterLock.issue(owner, UUID.randomUUID().toString)
              held.update(root, Held(lock, channel, fl))
              try
                writeOwner(root, owner)
                Right(lock)
              catch
                case NonFatal(e) =>
                  held.remove(root)
                  fl.release()
                  channel.close()
                  throw e
          }
    }

    def release(root: Path, lock: WriterLock): Either[StoreError, Unit] = synchronized {
      held.get(root) match
        case Some(h) if h.lock == lock =>
          held.remove(root)
          try Files.deleteIfExists(root.resolve(OwnerName)): Unit
          finally
            try h.fileLock.release()
            finally h.channel.close()
          Right(())
        case Some(h) => Left(StoreError.NotHolder(lock.owner, Some(h.lock.owner)))
        case None    => Left(StoreError.NotHolder(lock.owner, ownerOf(root)))
    }

    private def writeOwner(root: Path, owner: LockOwner): Unit =
      val staged = root.resolve(s"$OwnerName.$StagePrefix${UUID.randomUUID()}")
      Files.write(staged, owner.description.getBytes(UTF_8))
      Files.move(
        staged,
        root.resolve(OwnerName),
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      ): Unit

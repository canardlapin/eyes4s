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

import cats.effect.{Ref, Sync}
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.{BundlePath, LockOwner, ProjectStore, StoreError, WriterLock}

import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption, StandardOpenOption}
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** An `.eyes` bundle as a directory: the JVM [[ProjectStore]] (ticket S2.3).
  *
  * Entries are files under `root` at their bundle paths. Every write goes to
  * a hidden staging file in the target's directory and is then moved into
  * place atomically, so a reader never sees a partial entry or manifest.
  * [[BundlePath]] cannot name a hidden file, so staging files and the lock
  * file (`.lock`) are never entries. A path that resolves through a symbolic
  * link out of `root` is refused.
  *
  * The writer lock is an OS file lock on `.lock`, which holds its owner's
  * description while held; it excludes other processes and other store
  * instances in this process alike.
  */
final class FileProjectStore[F[_]: Sync] private (
    root: Path,
    held: Ref[F, Option[FileProjectStore.Held]]
) extends ProjectStore[F]:
  import FileProjectStore.*

  private val manifestFile = root.resolve(ProjectStore.ManifestName)
  private val lockFile     = root.resolve(LockName)

  private def file(path: BundlePath): Path =
    path.segments.foldLeft(root)(_.resolve(_))

  private def blocking[A](target: String, reading: Boolean)(
      body: => Either[StoreError, A]
  ): F[Either[StoreError, A]] =
    Sync[F].blocking {
      try body
      catch
        case NonFatal(e) =>
          val reason = Option(e.getMessage).getOrElse(e.getClass.getSimpleName)
          Left(
            if reading then StoreError.Unreadable(target, reason)
            else StoreError.Unwritable(target, reason)
          )
    }

  /** The file, if its nearest existing ancestor is inside the real root. */
  private def contained(target: String, file: Path): Either[StoreError, Path] =
    val realRoot = root.toRealPath()
    val existing =
      Iterator.iterate(file)(_.getParent).takeWhile(_ != null).find(Files.exists(_))
    existing match
      case Some(e) if !e.toRealPath().startsWith(realRoot) =>
        Left(StoreError.Unreadable(target, "it resolves outside the bundle through a link"))
      case _ => Right(file)

  private def readFile(target: String, file: Path): Either[StoreError, Option[IArray[Byte]]] =
    if !Files.exists(file, LinkOption.NOFOLLOW_LINKS) then Right(None)
    else
      contained(target, file).flatMap { f =>
        if !Files.isRegularFile(f, LinkOption.NOFOLLOW_LINKS) then
          Left(StoreError.Unreadable(target, "it is not a regular file"))
        else Right(Some(IArray.unsafeFromArray(Files.readAllBytes(f))))
      }

  private def stage(target: String, file: Path, bytes: IArray[Byte]): Either[StoreError, Unit] =
    Files.createDirectories(file.getParent)
    contained(target, file).map { f =>
      val staged = f.resolveSibling(s".${f.getFileName}.$StagePrefix${UUID.randomUUID()}")
      try
        Files.write(staged, Array.from(bytes))
        Files.move(
          staged,
          f,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      finally Files.deleteIfExists(staged): Unit
      ()
    }

  private def holding(lock: WriterLock, target: String)(
      body: => Either[StoreError, Unit]
  ): F[Either[StoreError, Unit]] =
    held.get.flatMap {
      case Some(h) if h.lock == lock => blocking(target, reading = false)(body)
      case other                     =>
        other match
          case Some(h) =>
            Sync[F].pure(Left(StoreError.NotHolder(lock.owner, Some(h.lock.owner))))
          case None => holder.map(h => Left(StoreError.NotHolder(lock.owner, h)))
    }

  private def emptyDirectory(d: Path): Boolean =
    val entries = Files.list(d)
    try !entries.iterator().hasNext
    finally entries.close()

  /** The owner recorded in the lock file, if any. */
  private def holder: F[Option[LockOwner]] =
    Sync[F].blocking {
      try
        if Files.isRegularFile(lockFile) then
          LockOwner.of(String(Files.readAllBytes(lockFile), UTF_8)).toOption
        else None
      catch case NonFatal(_) => None
    }

  def read(path: BundlePath): F[Either[StoreError, IArray[Byte]]] =
    blocking(path.value, reading = true)(
      readFile(path.value, file(path)).flatMap(_.toRight(StoreError.Missing(path)))
    )

  def list: F[Either[StoreError, Vector[BundlePath]]] =
    blocking(".", reading = true) {
      if !Files.isDirectory(root) then Right(Vector.empty)
      else
        val walk = Files.walk(root)
        try
          Right(
            walk
              .iterator()
              .asScala
              .filter(Files.isRegularFile(_, LinkOption.NOFOLLOW_LINKS))
              .flatMap(f =>
                BundlePath.of(root.relativize(f).iterator().asScala.mkString("/")).toOption
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
    holding(lock, path.value)(stage(path.value, file(path), bytes))

  def delete(lock: WriterLock, path: BundlePath): F[Either[StoreError, Unit]] =
    holding(lock, path.value) {
      val f = file(path)
      if !Files.exists(f, LinkOption.NOFOLLOW_LINKS) then Left(StoreError.Missing(path))
      else
        contained(path.value, f).map { f =>
          Files.delete(f)
          // Remove directories the deletion emptied, up to the bundle root.
          Iterator
            .iterate(f.getParent)(_.getParent)
            .takeWhile(d => d != null && d.startsWith(root) && d != root)
            .takeWhile(emptyDirectory)
            .foreach(Files.delete(_))
        }
    }

  def readManifest: F[Either[StoreError, IArray[Byte]]] =
    blocking(ProjectStore.ManifestName, reading = true)(
      readFile(ProjectStore.ManifestName, manifestFile).flatMap(
        _.toRight(StoreError.NoManifest)
      )
    )

  def swapManifest(
      lock: WriterLock,
      expected: Option[ByteDigest],
      next: IArray[Byte]
  ): F[Either[StoreError, Unit]] =
    holding(lock, ProjectStore.ManifestName) {
      for
        current <- readFile(ProjectStore.ManifestName, manifestFile)
        found = current.map(ByteDigest.sha256)
        _ <- Either.cond(found == expected, (), StoreError.ManifestMoved(expected, found))
        _ <- stage(ProjectStore.ManifestName, manifestFile, next)
      yield ()
    }

  def acquire(owner: LockOwner): F[Either[StoreError, WriterLock]] =
    held.get.flatMap {
      case Some(h) => Sync[F].pure(Left(StoreError.Locked(owner, Some(h.lock.owner))))
      case None    =>
        Sync[F]
          .blocking {
            Files.createDirectories(root)
            val channel = FileChannel.open(
              lockFile,
              StandardOpenOption.CREATE,
              StandardOpenOption.READ,
              StandardOpenOption.WRITE
            )
            val os =
              try Option(channel.tryLock())
              catch case _: OverlappingFileLockException => None
            os match
              case None =>
                channel.close()
                None
              case Some(fileLock) =>
                channel.truncate(0)
                channel.write(ByteBuffer.wrap(owner.description.getBytes(UTF_8)), 0)
                channel.force(true)
                Some(
                  Held(WriterLock.issue(owner, UUID.randomUUID().toString), channel, fileLock)
                )
          }
          .attempt
          .flatMap {
            case Left(e) =>
              Sync[F].pure(Left(StoreError.Unwritable(LockName, String.valueOf(e.getMessage))))
            case Right(None)    => holder.map(h => Left(StoreError.Locked(owner, h)))
            case Right(Some(h)) => held.set(Some(h)).as(Right(h.lock))
          }
    }

  def release(lock: WriterLock): F[Either[StoreError, Unit]] =
    held.get.flatMap {
      case Some(h) if h.lock == lock =>
        blocking(LockName, reading = false) {
          h.channel.truncate(0)
          h.fileLock.release()
          h.channel.close()
          Right(())
        } <* held.set(None)
      case other =>
        other match
          case Some(h) =>
            Sync[F].pure(Left(StoreError.NotHolder(lock.owner, Some(h.lock.owner))))
          case None => holder.map(h => Left(StoreError.NotHolder(lock.owner, h)))
    }

object FileProjectStore:
  /** The lock file's name at the bundle root. */
  val LockName: String = ".lock"

  private val StagePrefix = "staged-"

  private[platform] final case class Held(
      lock: WriterLock,
      channel: FileChannel,
      fileLock: FileLock
  )

  /** A store on the bundle directory `root`, created on the first write. */
  def at[F[_]: Sync](root: Path): F[ProjectStore[F]] =
    Ref.of[F, Option[Held]](None).map(new FileProjectStore(root.toAbsolutePath.normalize, _))

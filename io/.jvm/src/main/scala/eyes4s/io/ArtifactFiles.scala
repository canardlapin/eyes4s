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

package eyes4s.io

import cats.data.NonEmptyVector
import cats.effect.kernel.{Resource, Sync}
import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.kernel.Unit2D

import java.io.IOException
import java.nio.file.{Files, NoSuchFileException, Path}

/** A JVM file source for [[ArtifactLoading]]: every request is served from a
  * regular file under one root directory.
  *
  * The application decides the file name of every request (by default the
  * manifest in one file and each entry under its manifest name); eyes4s only
  * reads what that name resolves to, and refuses without reading:
  *
  *   - a name that leaves the root lexically (`..`, an absolute name) or
  *     through a symbolic link, by comparing real paths
  *     (`SourceFailure.OutsideRoot`);
  *   - anything but a regular file, such as a device or a directory
  *     (`SourceFailure.NotRegularFile`);
  *   - a file larger than the request can use: an entry's declared length, or
  *     the manifest bound (`SourceFailure.Oversize`). Reads are bounded as
  *     well, so a file that grows after the check is still refused.
  *
  * Each file is opened as a `Resource` and read under `Sync.interruptible`,
  * so resolution can be cancelled during a read and the file is closed.
  */
object ArtifactFiles:
  /** The largest manifest this source reads by default: 16 MiB. */
  val maximumManifestBytes: Long = 16L * 1024 * 1024

  /** Serve requests from `root`, naming each request's file with `name`. */
  def source[F[_]: Sync](
      root: Path,
      name: ByteRequest => String,
      manifestLimit: Long = maximumManifestBytes
  ): ByteRequest => F[ArtifactLoading.Read] = request =>
    val limit = request match
      case ByteRequest.Manifest(_)  => manifestLimit
      case ByteRequest.Entry(entry) => entry.length
    locate(root, name(request), limit).flatMap {
      case Left(failure) => Sync[F].pure(Left(failure): ArtifactLoading.Read)
      case Right(path)   => bounded(path, name(request), limit)
    }

  /** The manifest in `manifestFile` and each entry under its manifest name. */
  def directory[F[_]: Sync](
      root: Path,
      manifestFile: String
  ): ByteRequest => F[ArtifactLoading.Read] =
    source(
      root,
      {
        case ByteRequest.Manifest(_)  => manifestFile
        case ByteRequest.Entry(entry) => entry.name.value
      }
    )

  /** Resolve the manifest stored under `address` in `root/manifestFile`. */
  def resolve[F[_]: Sync, K, U <: Unit2D](
      address: ByteDigest,
      root: Path,
      manifestFile: String,
      decoders: ArtifactDecoders[K, U]
  ): F[Either[NonEmptyVector[ResolveError], ResolvedManifest[K, U]]] =
    ArtifactLoading.resolve(address, directory[F](root, manifestFile), decoders)

  /** The file a name denotes, if it is a regular file inside the root no
    * larger than `limit`; nothing is read.
    */
  private def locate[F[_]: Sync](
      root: Path,
      name: String,
      limit: Long
  ): F[Either[SourceFailure, Path]] =
    Sync[F].blocking {
      try
        val base    = root.toRealPath()
        val lexical = base.resolve(name).normalize
        if !lexical.startsWith(base) || lexical == base then
          Left(SourceFailure.OutsideRoot(name, base.toString))
        else
          val real = lexical.toRealPath()
          if !real.startsWith(base) then Left(SourceFailure.OutsideRoot(name, base.toString))
          else if !Files.isRegularFile(real) then Left(SourceFailure.NotRegularFile(name))
          else
            val size = Files.size(real)
            if size > limit then Left(SourceFailure.Oversize(name, size, limit))
            else Right(real)
      catch
        case _: NoSuchFileException => Left(SourceFailure.Missing)
        case e: IOException         =>
          Left(SourceFailure.Unreadable(s"$name: ${e.getClass.getName}: ${e.getMessage}"))
        case e: SecurityException => Left(SourceFailure.Unreadable(s"$name: ${e.getMessage}"))
    }

  /** Read at most `limit` bytes; one more byte means the file grew. */
  private def bounded[F[_]: Sync](
      path: Path,
      name: String,
      limit: Long
  ): F[ArtifactLoading.Read] =
    val cap = math.min(limit, PayloadLayout.maximumBytes.toLong - 1).toInt + 1
    Resource
      .fromAutoCloseable(Sync[F].blocking(Files.newInputStream(path)))
      .use(in => Sync[F].interruptible(in.readNBytes(cap)))
      .map { bytes =>
        if bytes.length.toLong > limit then
          Left(SourceFailure.Oversize(name, bytes.length.toLong, limit)): ArtifactLoading.Read
        else Right(IArray.unsafeFromArray(bytes))
      }
      .recover {
        case _: NoSuchFileException => Left(SourceFailure.Missing)
        case e: IOException         =>
          Left(SourceFailure.Unreadable(s"$name: ${e.getClass.getName}: ${e.getMessage}"))
      }

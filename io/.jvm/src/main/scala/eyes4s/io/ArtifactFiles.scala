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
import cats.effect.kernel.Sync
import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.kernel.Unit2D

import java.io.IOException
import java.nio.file.{Files, NoSuchFileException, Path}

/** A minimal file adapter for [[eyes4s.codec.ArtifactResolver]] on the JVM.
  *
  * The application decides where every artifact lives through `locate`;
  * eyes4s only reads the paths it is given. Each file is read whole under
  * `Sync.interruptible`, so resolution can be cancelled between and during
  * reads, and nothing is decoded until every entry has been read; the pure
  * resolver then verifies and decodes them. Project layout, autosave and
  * missing-file repair stay in the application.
  */
object ArtifactFiles:
  type Read = Either[SourceFailure, IArray[Byte]]

  /** Read one file; a missing file is `SourceFailure.Missing` and any other
    * I/O failure `SourceFailure.Unreadable`, naming the path.
    */
  def read[F[_]: Sync](path: Path): F[Read] =
    Sync[F]
      .interruptible(Files.readAllBytes(path))
      .map(bytes => Right(IArray.unsafeFromArray(bytes)): Read)
      .recover {
        case _: NoSuchFileException => Left(SourceFailure.Missing)
        case e: IOException         =>
          Left(SourceFailure.Unreadable(s"$path: ${e.getClass.getName}: ${e.getMessage}"))
      }

  /** One directory: the manifest in `manifestFile` and each entry under its
    * name. A name that would leave the directory is refused unread.
    */
  def inDirectory(
      root: Path,
      manifestFile: String
  ): ByteRequest => Either[SourceFailure, Path] =
    val base                                              = root.toAbsolutePath.normalize
    def within(name: String): Either[SourceFailure, Path] =
      val path = base.resolve(name).normalize
      Either.cond(
        path.startsWith(base) && path != base,
        path,
        SourceFailure.Unreadable(s"'$name' is outside $base")
      )
    val locate: ByteRequest => Either[SourceFailure, Path] = {
      case ByteRequest.Manifest(_)  => within(manifestFile)
      case ByteRequest.Entry(entry) => within(entry.name.value)
    }
    locate

  /** Read the manifest at `locate(Manifest(address))` and every entry it
    * lists, then verify and decode them with the pure resolver.
    */
  def resolve[F[_]: Sync, K, U <: Unit2D](
      address: ByteDigest,
      locate: ByteRequest => Either[SourceFailure, Path],
      decoders: ArtifactDecoders[K, U]
  ): F[Either[NonEmptyVector[ResolveError], ResolvedManifest[K, U]]] =
    type Result = Either[NonEmptyVector[ResolveError], ResolvedManifest[K, U]]
    def load(request: ByteRequest): F[Read] =
      locate(request).fold(failure => Sync[F].pure(Left(failure): Read), read[F])
    load(ByteRequest.Manifest(address)).flatMap { stored =>
      ArtifactResolver.manifest(address, ByteSource(_ => stored)) match
        case Left(error)     => Sync[F].pure(Left(NonEmptyVector.one(error)): Result)
        case Right(manifest) =>
          manifest.entries
            .traverse(entry => load(ByteRequest.Entry(entry)).map(entry.name -> _))
            .map { reads =>
              val table = reads.toMap
              ArtifactResolver.resolveManifest(
                manifest,
                ByteSource {
                  case ByteRequest.Entry(entry) =>
                    table.getOrElse(entry.name, Left(SourceFailure.Missing))
                  case ByteRequest.Manifest(_) => stored
                },
                decoders
              ): Result
            }
    }

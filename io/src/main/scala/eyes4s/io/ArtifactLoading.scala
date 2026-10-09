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

import cats.MonadThrow
import cats.data.NonEmptyVector
import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.kernel.Unit2D

/** The effectful half of manifest resolution over any storage an application
  * reads in `F`: a JVM directory, a browser store, an object store.
  *
  * `resolve` reads the manifest, then every entry it lists, one request at a
  * time, and only then hands the bytes to the pure [[eyes4s.codec.ArtifactResolver ArtifactResolver]], which
  * verifies every length and digest before decoding anything. Each read is a
  * separate `F`, so an `F` that supports cancellation can cancel resolution
  * between reads or inside one; the source owns whatever it opens and
  * releases it in its own finalizers. A read that fails in `F` is reported as
  * `SourceFailure.Unreadable` rather than raised, so the result is total.
  */
object ArtifactLoading:
  type Read = Either[SourceFailure, IArray[Byte]]

  def resolve[F[_], K, U <: Unit2D](
      address: ByteDigest,
      read: ByteRequest => F[Read],
      decoders: ArtifactDecoders[K, U]
  )(using F: MonadThrow[F]): F[Either[NonEmptyVector[ResolveError], ResolvedManifest[K, U]]] =
    type Result = Either[NonEmptyVector[ResolveError], ResolvedManifest[K, U]]
    def load(request: ByteRequest): F[Read] =
      F.catchNonFatal(read(request))
        .flatten
        .handleError(e =>
          Left(SourceFailure.Unreadable(s"${e.getClass.getName}: ${e.getMessage}"))
        )
    load(ByteRequest.Manifest(address)).flatMap { stored =>
      ArtifactResolver.manifest(address, ByteSource(_ => stored)) match
        case Left(error)     => F.pure(Left(NonEmptyVector.one(error)): Result)
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

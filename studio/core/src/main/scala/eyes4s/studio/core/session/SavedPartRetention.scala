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

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.*

/** Post-save reclamation is deliberately limited to parts named by the retired
  * previous manifest. Unknown files and older unindexed orphans are untouched.
  * Archived journals pin all generations: their base digest alone is not a
  * complete manifest index (bead bd-01M44PBGT3KAFQ160QTEV3G3HK).
  */
enum PartRetention derives CanEqual:
  case NotRequired
  case ArchivedJournals(paths: Vector[BundlePath])
  case Deferred(error: StoreError)
  case Collected(removed: Vector[BundlePath], failures: Vector[StoreError])

  def warnings: Vector[StoreError] = this match
    case Deferred(error)        => Vector(error)
    case Collected(_, failures) => failures
    case _                      => Vector.empty

private[session] object SavedPartRetention:
  def collect[F[_]: Monad](
      store: ProjectStore[F],
      lock: WriterLock,
      retired: Either[StoreError, IArray[Byte]],
      current: IArray[Byte],
      previous: Option[IArray[Byte]],
      journal: IArray[Byte],
      reset: Either[StoreError, Unit]
  ): F[PartRetention] =
    def pure(value: PartRetention): F[PartRetention]          = Monad[F].pure(value)
    def uncertain(target: String, reason: String): StoreError =
      StoreError.Unreadable(target, s"Saved-part retention deferred: $reason")
    def same(target: String, found: IArray[Byte], expected: IArray[Byte]) =
      Either.cond(
        ByteDigest.sha256(found) == ByteDigest.sha256(expected),
        (),
        uncertain(target, "bytes changed after the successful save")
      )
    def opened(bytes: IArray[Byte], target: String) =
      EitherT(ProjectBundle.openFrom(store, bytes)).leftMap(e => uncertain(target, e.message))

    reset match
      case Left(error) => pure(PartRetention.Deferred(error))
      case Right(_)    =>
        retired match
          case Left(_: StoreError.NoSidecar) => pure(PartRetention.NotRequired)
          case Left(error)                   => pure(PartRetention.Deferred(error))
          case Right(old)                    =>
            val checked = for
              paths <- EitherT(store.list)
              archives = paths.filter(_.area == BundleArea.Journals)
              result <-
                if archives.nonEmpty then
                  EitherT.pure[F, StoreError](
                    PartRetention.ArchivedJournals(archives): PartRetention
                  )
                else
                  for
                    found <- EitherT(store.readManifest)
                    _ <- EitherT.fromEither[F](same(ProjectStore.ManifestName, found, current))
                    active <- EitherT(store.readSidecar(Sidecar.Journal))
                    _ <- EitherT.fromEither[F](same(Sidecar.Journal.fileName, active, journal))
                    keepCurrent  <- opened(current, ProjectStore.ManifestName)
                    keepPrevious <- previous.traverse { expected =>
                      for
                        actual <- EitherT(store.readSidecar(Sidecar.PreviousManifest))
                        _      <- EitherT.fromEither[F](
                          same(Sidecar.PreviousManifest.fileName, actual, expected)
                        )
                        value <- opened(actual, Sidecar.PreviousManifest.fileName)
                      yield value
                    }
                    oldProject <- opened(old, "retired previous manifest")
                    keep = (keepCurrent.manifest.parts.all ++ keepPrevious.toVector.flatMap(
                      _.manifest.parts.all
                    )).map(_.path).toSet
                    candidates = oldProject.manifest.parts.all
                      .map(_.path)
                      .distinct
                      .filterNot(keep)
                    outcomes <- EitherT.liftF(
                      candidates.traverse(path => store.delete(lock, path).map(path -> _))
                    )
                  yield PartRetention.Collected(
                    outcomes.collect { case (path, Right(_)) => path },
                    outcomes.collect { case (_, Left(error)) => error }
                  ): PartRetention
            yield result
            checked.value.map(_.fold(PartRetention.Deferred(_), identity))

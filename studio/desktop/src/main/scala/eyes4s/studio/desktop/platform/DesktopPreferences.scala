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

import cats.effect.{IO, Resource}
import eyes4s.studio.core.platform.PlatformError
import eyes4s.studio.core.preferences.{PreferencesProblem, PreferencesStore, UserPreferences}
import eyes4s.studio.core.platform.HostPath
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.util.concurrent.TimeoutException
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/** The successful patch, including any unusable file kept before replacement. */
final case class PreferenceUpdate(
    preferences: UserPreferences,
    kept: Option[HostPath],
    problems: Vector[PreferencesProblem]
)

/** Desktop writers patch the latest file while holding the same OS writer lock.
  * Atomic replacement and corrupt-file preservation remain the store's operations.
  * Independent windows, repository instances and processes cannot overwrite a
  * newer unrelated preference with their old in-memory snapshot.
  */
final class DesktopPreferences(
    store: PreferencesStore[IO],
    lockTimeout: FiniteDuration = 30.seconds
):
  val path: HostPath = store.path
  private val retry  = 20.millis

  def load: IO[(UserPreferences, Vector[PreferencesProblem])] = store.load

  private def channel: Resource[IO, FileChannel] = Resource.fromAutoCloseable(IO.blocking {
    val target = Paths.get(path.value).toAbsolutePath.normalize()
    val parent = target.getParent
    Files.createDirectories(parent)
    val file = parent.toRealPath().resolve(target.getFileName.toString + ".lock")
    FileChannel.open(file, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
  })

  private def writer(channel: FileChannel): IO[FileLock] =
    def attempt: IO[Option[FileLock]] = IO.blocking {
      try Option(channel.tryLock())
      catch case _: OverlappingFileLockException => None
    }
    IO.monotonic.flatMap { start =>
      def loop: IO[FileLock] = attempt.flatMap {
        case Some(lock) => IO.pure(lock)
        case None       =>
          IO.monotonic.flatMap { now =>
            if now - start >= lockTimeout then
              IO.raiseError(
                TimeoutException(
                  s"Timed out after $lockTimeout waiting for the preferences writer at ${path.value}.lock"
                )
              )
            else IO.sleep(retry) >> IO.defer(loop)
          }
      }
      loop
    }

  /** Only `change`'s fields change; unrelated fields come from the locked read.
    * A missing file starts at defaults. An unreadable file is never replaced.
    * Cancellation releases its channel, including while waiting for another writer.
    */
  def update(
      change: UserPreferences => UserPreferences
  ): IO[Either[PreferencesProblem, PreferenceUpdate]] =
    channel
      .flatMap { c =>
        Resource.makeFull[IO, FileLock](poll => poll(writer(c)))(lock =>
          IO.blocking(lock.release())
        )
      }
      .use { _ =>
        store.load.flatMap { (current, problems) =>
          problems.collectFirst { case PreferencesProblem.Unreadable(p, e) =>
            PreferencesProblem.NotOverwritten(p, e)
          } match
            case Some(problem) => IO.pure(Left(problem))
            case None          =>
              val next = change(current)
              store.save(next).map(_.map(kept => PreferenceUpdate(next, kept, problems)))
        }
      }
      .recover { case NonFatal(error) =>
        Left(
          PreferencesProblem.Unwritable(path, PlatformError.Unwritable(path, error.toString))
        )
      }

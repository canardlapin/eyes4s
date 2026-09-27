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

package eyes4s.studio.desktop.harness

import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.file.{Path, Paths, StandardOpenOption}

/** One JavaFX test run per machine at a time.
  *
  * The FX suites show real windows, so parallel runs from several worktrees
  * fight over the screen and each costs a test JVM. The first FX test of a
  * forked test JVM takes an exclusive lock on [[file]] and holds it until the
  * JVM exits, which releases it even when the run is killed. A run that finds
  * the lock held says so on stderr and waits, up to [[WaitMinutes]].
  *
  * `EYES4S_STUDIO_FX_LOCK=off` skips the lock, for a machine whose runs are
  * already serialised (a CI job).
  */
object FxRunLock:

  /** Set to `off` to skip the lock. */
  val DisableVariable = "EYES4S_STUDIO_FX_LOCK"

  /** How long a run waits for another to finish before it fails. */
  val WaitMinutes = 90L

  /** The lock file, shared by every run of the same user. */
  val file: Path = Paths.get(sys.props("java.io.tmpdir"), "eyes4s-studio-fx.lock")

  // The outcome is computed once: a lazy val whose initialiser throws would
  // run again on the next access, and every later suite would wait again.
  // A held lock stays referenced here for the JVM's lifetime, which keeps its
  // channel open.
  private lazy val acquired: Either[Throwable, Option[FileLock]] =
    if sys.env.get(DisableVariable).contains("off") then Right(None)
    else
      scala.util
        .Try(FileChannel.open(file, StandardOpenOption.CREATE, StandardOpenOption.WRITE))
        .toEither
        .flatMap { channel =>
          val lock = scala.util
            .Try(
              await(
                () => attempt(channel),
                waitNanos = WaitMinutes * 60L * 1000000000L,
                pauseMillis = 2000L,
                onWait = () =>
                  System.err.println(
                    s"[studio-fx] another JavaFX test run holds $file; waiting up to $WaitMinutes min " +
                      s"(set $DisableVariable=off to skip the lock)"
                  )
              )
            )
            .toEither
          lock match
            case Right(Some(l)) => Right(Some(l))
            case Right(None)    =>
              channel.close()
              Left(
                AssertionError(
                  s"another JavaFX test run held $file for more than $WaitMinutes min"
                )
              )
            case Left(e) =>
              channel.close()
              Left(e)
        }

  /** Takes the lock once per JVM, waiting for any other run to finish; after a
    * failure, every later call fails at once with the same error.
    */
  def hold(): Unit = acquired.fold(e => throw e, _ => ())

  /** One try at the lock on `channel`; a lock this JVM already holds is `None`. */
  private[harness] def attempt(channel: FileChannel): Option[FileLock] =
    try Option(channel.tryLock())
    catch case _: OverlappingFileLockException => None

  /** Calls `attempt` until it yields a value or `waitNanos` pass, pausing
    * `pauseMillis` between tries; `onWait` runs once, before the first pause.
    */
  private[harness] def await[A](
      attempt: () => Option[A],
      waitNanos: Long,
      pauseMillis: Long,
      onWait: () => Unit,
      nanoTime: () => Long = () => System.nanoTime(),
      pause: Long => Unit = Thread.sleep
  ): Option[A] =
    val start = nanoTime()
    var got   = attempt()
    if got.isEmpty then onWait()
    while got.isEmpty && nanoTime() - start < waitNanos do
      pause(pauseMillis)
      got = attempt()
    got

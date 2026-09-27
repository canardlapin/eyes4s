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

  // Held for the JVM's lifetime; the reference keeps the channel open.
  @volatile private var held: Option[FileLock] = None

  private lazy val acquired: Unit =
    if !sys.env.get(DisableVariable).contains("off") then
      val channel = FileChannel.open(file, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
      def attempt(): Option[FileLock] =
        try Option(channel.tryLock())
        catch case _: OverlappingFileLockException => None
      val lock = await(
        attempt,
        waitNanos = WaitMinutes * 60L * 1000000000L,
        pauseMillis = 2000L,
        onWait = () =>
          System.err.println(
            s"[studio-fx] another JavaFX test run holds $file; waiting up to $WaitMinutes min " +
              s"(set $DisableVariable=off to skip the lock)"
          )
      )
      lock match
        case Some(l) => held = Some(l)
        case None    =>
          channel.close()
          throw AssertionError(
            s"another JavaFX test run held $file for more than $WaitMinutes min"
          )

  /** Takes the lock once per JVM, waiting for any other run to finish. */
  def hold(): Unit = acquired

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

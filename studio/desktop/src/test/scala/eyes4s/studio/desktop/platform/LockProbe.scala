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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.core.bundle.{LockOwner, StoreError}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Path, Paths}
import java.util.concurrent.TimeUnit

/** A second process for the writer-lock test: it tries to take the lock of
  * the bundle named by its argument and prints `ACQUIRED` (releasing it
  * again) or `LOCKED <holder>`.
  */
object LockProbe:
  def main(args: Array[String]): Unit =
    val outcome =
      for
        store <- FileProjectStore.at[IO](Paths.get(args(0)))
        owner <- IO.fromEither(
          LockOwner.of("probe").left.map(e => new IllegalStateException(e.message))
        )
        got  <- store.acquire(owner)
        text <- got match
          case Right(lock)                        => store.release(lock).as("ACQUIRED")
          case Left(StoreError.Locked(_, holder)) =>
            IO.pure(s"LOCKED ${holder.fold("unknown")(_.description)}")
          case Left(other) => IO.pure(s"ERROR ${other.message}")
      yield text
    println(outcome.unsafeRunSync())

  /** Run the probe in a new JVM on this test's class path; its last line. */
  def run(root: Path): IO[String] =
    IO.blocking {
      val java    = Paths.get(sys.props("java.home"), "bin", "java").toString
      val process = new ProcessBuilder(
        java,
        "-cp",
        sys.props("java.class.path"),
        "eyes4s.studio.desktop.platform.LockProbe",
        root.toString
      ).redirectErrorStream(true).start()
      val output = String(process.getInputStream.readAllBytes, UTF_8)
      if !process.waitFor(60, TimeUnit.SECONDS) then process.destroyForcibly(): Unit
      output.linesIterator.toVector.lastOption.getOrElse("").trim
    }

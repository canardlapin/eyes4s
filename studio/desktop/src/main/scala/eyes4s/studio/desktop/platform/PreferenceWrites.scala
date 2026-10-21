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
import cats.effect.unsafe.IORuntime
import eyes4s.studio.core.platform.PlatformError
import eyes4s.studio.core.preferences.{PreferencesProblem, UserPreferences}
import java.util.concurrent.CompletableFuture

/** Submissions start in call order; completion never waits for a UI callback.
  * Closing drains all accepted writes and refuses later submissions.
  */
final class PreferenceWrites(store: DesktopPreferences)(using IORuntime):
  private var tail   = CompletableFuture.completedFuture(())
  private var closed = false

  private def failure(reason: String): PreferencesProblem =
    PreferencesProblem.Unwritable(store.path, PlatformError.Unwritable(store.path, reason))

  def submit(
      change: UserPreferences => UserPreferences
  )(done: Either[PreferencesProblem, PreferenceUpdate] => Unit): Unit = synchronized {
    if closed then done(Left(failure("The preference writer is closed.")))
    else
      val previous = tail
      val next     = CompletableFuture[Unit]()
      tail = next
      previous.whenComplete { (_, _) =>
        store.update(change).unsafeRunAsync { result =>
          val answer = result.fold(error => Left(failure(error.toString)), identity)
          try done(answer)
          finally next.complete(()): Unit
        }
      }: Unit
  }

  def close: IO[Unit] = IO.uncancelable(_ =>
    IO.fromCompletableFuture(IO(synchronized {
      closed = true
      tail
    }))
  )

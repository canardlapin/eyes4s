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
import java.util.concurrent.atomic.AtomicBoolean
import eyes4s.studio.app.appearance.AppearancePreference
import eyes4s.studio.core.preferences.UserPreferences
import eyes4s.studio.desktop.runtime.StudioRuntime

/** The appearance preference on the desktop (tickets S2.8, S1.10): applies
  * it when a window opens ([[AppearancePreference.atBoot]]) and saves it
  * when, and only when, the user chooses an appearance
  * ([[AppearancePreference.chosen]]).
  *
  * Saves run off the JavaFX thread one at a time, the latest choice
  * winning: a choice made while a save runs is written after it, and only
  * the newest of several. [[saved]] changes only when a save succeeds, so a
  * failed save is logged and written again with the next save. `onFx` runs
  * a save's completion on the JavaFX thread. Use on the JavaFX thread.
  */
final class AppearancePreferenceHost(
    store: Option[DesktopPreferences],
    initial: UserPreferences,
    log: String => Unit,
    onFx: (() => Unit) => Unit = f => javafx.application.Platform.runLater(() => f())
)(using IORuntime):
  // What the user chose last; what the file last held after a successful
  // save; the choice waiting to be written; whether a save is running.
  private var wanted  = initial
  private var stored  = initial
  private var pending = Option.empty[UserPreferences]
  private var saving  = false
  private val writes  = store.map(PreferenceWrites(_))
  private val stopped = AtomicBoolean(false)

  /** The preferences as the user last chose them. */
  def current: UserPreferences = wanted

  /** The preferences the file holds, as far as this host knows. */
  def saved: UserPreferences = stored

  def attach(runtime: StudioRuntime): Unit =
    AppearancePreference.atBoot(wanted, runtime.model).foreach(runtime.dispatch)
    runtime.observe((intent, _) => AppearancePreference.chosen(wanted, intent).foreach(choose))

  // A choice the file already holds needs no save; any other is written,
  // after the save running, if any.
  private def sameChoice(a: UserPreferences, b: UserPreferences): Boolean =
    a.appearance == b.appearance

  private def choose(p: UserPreferences): Unit =
    if !stopped.get then
      wanted = p
      pending = Option.when(!sameChoice(p, stored))(p)
      next()

  /** Drain the last explicit choice without waiting for queued FX completions. */
  def close: IO[Unit] =
    val finalChoice = wanted.appearance
    val unfinished  = saving || !sameChoice(wanted, stored)
    IO(stopped.getAndSet(true)).flatMap { alreadyClosed =>
      writes.fold(IO.unit) { queue =>
        val last = if unfinished && !alreadyClosed then
          IO(queue.submit(_.withAppearance(finalChoice)) {
            case Left(problem)  => log(problem.message)
            case Right(written) => written.problems.foreach(p => log(p.message))
          })
        else IO.unit
        last >> queue.close
      }
    }

  private def next(): Unit =
    (writes, pending) match
      case (Some(queue), Some(p)) if !saving && !stopped.get =>
        saving = true
        pending = None
        queue.submit(_.withAppearance(p.appearance)) { result =>
          if !stopped.get then
            onFx { () =>
              if !stopped.get then
                saving = false
                result match
                  case Right(written) =>
                    stored = written.preferences
                    written.problems.foreach(problem => log(problem.message))
                    // A choice made during the save, back to what the file held
                    // before it, must now be written too.
                    if !sameChoice(wanted, stored) && pending.isEmpty then
                      pending = Some(wanted)
                  // `stored` stays: the next choice writes again.
                  case Left(problem) => log(problem.message)
                next()
            }
        }
      case _ => ()

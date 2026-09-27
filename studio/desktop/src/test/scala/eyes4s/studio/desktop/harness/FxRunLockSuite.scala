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

class FxRunLockSuite extends munit.FunSuite:

  private final class Clock:
    var now: Long = 0L

  test("a free lock is taken at once, without waiting or pausing") {
    var waited = 0
    var paused = 0
    val got    = FxRunLock.await(
      () => Some("lock"),
      waitNanos = 100L,
      pauseMillis = 7L,
      onWait = () => waited += 1,
      pause = _ => paused += 1
    )
    assertEquals((got, waited, paused), (Some("lock"), 0, 0))
  }

  test("a held lock is waited for, announced once, and taken when freed") {
    val clock    = Clock()
    var tries    = 0
    var waited   = 0
    val attempts = () =>
      tries += 1
      if tries < 4 then None else Some(tries)
    val got = FxRunLock.await(
      attempts,
      waitNanos = 1000L,
      pauseMillis = 10L,
      onWait = () => waited += 1,
      nanoTime = () => clock.now,
      pause = ms => clock.now += ms
    )
    assertEquals((got, waited, tries), (Some(4), 1, 4))
  }

  test("a lock held past the wait gives up with None") {
    val clock = Clock()
    var tries = 0
    val got   = FxRunLock.await(
      () => { tries += 1; None },
      waitNanos = 25L,
      pauseMillis = 10L,
      onWait = () => (),
      nanoTime = () => clock.now,
      pause = ms => clock.now += ms
    )
    assertEquals(got, None)
    assertEquals(tries, 4) // at 0, 10, 20 and 30: the last try is after the deadline passes
  }

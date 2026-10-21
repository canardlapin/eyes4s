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

package eyes4s.studio.desktop.runtime

import cats.effect.{IO, Ref}
import eyes4s.studio.core.platform.*
import eyes4s.studio.desktop.StudioMain
import eyes4s.studio.desktop.platform.JavaFxFonts
import munit.CatsEffectSuite
import scala.concurrent.duration.*

class NativePlatformSuite extends CatsEffectSuite:
  test("startup registers every bundled face through the injected host") {
    for
      host     <- InMemoryPlatform.create[IO]()
      errors   <- StudioMain.loadFonts(host.platform.fonts)
      requests <- host.registered
    yield
      assertEquals(errors, Vector.empty)
      assertEquals(requests, JavaFxFonts.bundled)
      assertEquals(requests.size, 7)
  }

  test("a rejected face is reported without skipping the remaining registrations") {
    val rejected = JavaFxFonts.bundled.head
    val failure  = PlatformError.FontRejected(rejected, "synthetic font refusal")
    for
      seen <- Ref.of[IO, Vector[FontRequest]](Vector.empty)
      fonts = new Fonts[IO]:
        def register(font: FontRequest): IO[Either[PlatformError, Unit]] =
          seen.update(_ :+ font).as(if font == rejected then Left(failure) else Right(()))
      errors   <- StudioMain.loadFonts(fonts)
      requests <- seen.get
    yield
      assertEquals(errors, Vector(failure))
      assertEquals(requests, JavaFxFonts.bundled)
  }

  test("save status reads the host clock at completion and retains its UTC offset") {
    for
      host <- InMemoryPlatform.create[IO]()
      clock = DesktopEffects.clockFrom(host.platform.scheduler)
      before <- IO.blocking(clock())
      _      <- host.advance(8.hours + 25.minutes)
      after  <- IO.blocking(clock())
      shifted = new Scheduler[IO]:
        def now: IO[ClockReading] = IO.fromEither(
          ClockReading.of(-1L, 30).left.map(e => IllegalArgumentException(e.message))
        )
        def after(delay: FiniteDuration)(task: IO[Unit]): IO[Scheduled[IO]] =
          host.platform.scheduler.after(delay)(task)
      local <- IO.blocking(DesktopEffects.clockFrom(shifted)())
    yield
      assertEquals(before.map(t => (t.hour, t.minute)), Some((0, 0)))
      assertEquals(after.map(t => (t.hour, t.minute)), Some((8, 25)))
      assertEquals(local.map(t => (t.hour, t.minute)), Some((0, 29)))
  }

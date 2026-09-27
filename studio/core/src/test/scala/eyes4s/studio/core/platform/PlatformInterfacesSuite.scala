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

package eyes4s.studio.core.platform

import cats.effect.{IO, Ref, Resource}

import scala.concurrent.duration.*

/** PlatformInterfacesSuite (S0.9, JVM and JS): the platform interfaces'
  * parsed values, the [[PlatformConformance]] contract on the portable
  * [[InMemoryPlatform]], and the portable effect-timer [[Scheduler]].
  */
class PlatformInterfacesSuite extends PlatformConformance:

  private val faces = Vector(
    right(FontRequest.of("IBM Plex Sans", "fonts/IBMPlexSans-Regular.ttf")),
    right(FontRequest.of("IBM Plex Mono", "fonts/IBMPlexMono-Regular.ttf"))
  )

  private val platform: Resource[IO, InMemoryPlatform[IO]] =
    Resource.eval(
      InMemoryPlatform
        .create[IO](start = 1_790_000_000_000L)
        .flatTap(_.makeDirectory(right(HostPath.of("/data"))))
    )

  def subject: Resource[IO, PlatformConformance.Subject] =
    platform.map { p =>
      PlatformConformance.Subject(
        p.platform,
        right(HostPath.of("/data")),
        right(HostPath.of("/")),
        faces,
        p.opened.map(_.map(_.render)),
        _.render,
        p.advance
      )
    }

  private def path(text: String): HostPath = right(HostPath.of(text))

  test("host paths, file kinds, URLs, preference keys and fonts are parsed, not validated") {
    assertEquals(HostPath.of("  "), Left(PlatformError.BlankPath))
    assertEquals(HostPath.of("/tmp/a\u0000b"), Left(PlatformError.ControlCharacter(8, 6)))
    assertEquals(HostPath.of("/tmp/a\u007f"), Left(PlatformError.ControlCharacter(7, 6)))
    assertEquals(
      FileKind.of("CSV", Vector("csv", "tsv")).map(_.extensions),
      Right(Vector("csv", "tsv"))
    )
    Vector(Vector.empty, Vector(".csv"), Vector("*"), Vector("")).foreach { e =>
      assertEquals(FileKind.of("CSV", e), Left(PlatformError.InvalidFileKind("CSV", e)))
    }
    assertEquals(
      FileKind.of(" ", Vector("csv")),
      Left(PlatformError.InvalidFileKind(" ", Vector("csv")))
    )
    Vector("https://eyes4s.org", "http://x.y/z?q=1", "mailto:lab@example.org").foreach { u =>
      assertEquals(ExternalUrl.of(u).map(_.value), Right(u))
    }
    Vector("file:///etc/passwd", "javascript:alert(1)", "https://", "https://a b", "").foreach {
      u =>
        assertEquals(ExternalUrl.of(u), Left(PlatformError.InvalidUrl(u)))
    }
    assertEquals(PreferenceKey.of("window.main.x").map(_.value), Right("window.main.x"))
    Vector("", "Upper", "a b", "x" * 81).foreach { k =>
      assertEquals(PreferenceKey.of(k), Left(PlatformError.InvalidPreferenceKey(k)))
    }
    assertEquals(
      FontRequest.of("", "fonts/a.ttf"),
      Left(PlatformError.InvalidFont("", "fonts/a.ttf"))
    )
  }

  test("a clock reading gives local time at its offset, across midnight both ways") {
    val noon = 12L * 3600L * 1000L
    assertEquals(right(ClockReading.of(noon, 0)).localHour, 12)
    assertEquals(
      right(ClockReading.of(noon + 90_000L, 330)).localHour -> right(
        ClockReading.of(noon + 90_000L, 330)
      ).localMinute,
      (17, 31)
    )
    assertEquals(right(ClockReading.of(0L, -300)).localHour, 19)
    assertEquals(right(ClockReading.of(-60_000L, 0)).localMinute, 59)
    assertEquals(right(ClockReading.of(23L * 3600000L, 120)).localHour, 1)
    assertEquals(
      ClockReading.of(0L, 18 * 60 + 1),
      Left(PlatformError.InvalidOffset(18 * 60 + 1))
    )
    assertEquals(ClockReading.utc(noon), right(ClockReading.of(noon, 0)))
  }

  test("every error message names its operands") {
    val p = path("/data/a.csv")
    val f = faces.head
    val k = right(PreferenceKey.of("theme"))
    Vector(
      PlatformError.Missing(p)                                       -> "/data/a.csv",
      PlatformError.Unreadable(p, "denied")                          -> "denied",
      PlatformError.Unwritable(p, "read-only")                       -> "/data/a.csv",
      PlatformError.InvalidName(p, "..")                             -> "'..'",
      PlatformError.Unsupported("clipboard", "reading")              -> "clipboard",
      PlatformError.FontRejected(f, "not bundled")                   -> "IBM Plex Sans",
      PlatformError.OpenFailed(ExternalTarget.Reveal(p), "no shell") -> "/data/a.csv",
      PlatformError.PreferenceFailed(k, "backing store")             -> "theme",
      PlatformError.InvalidOffset(2000)                              -> "2000"
    ).foreach((e, operand) => assert(e.message.contains(operand), e.message))
  }

  test("in memory: dialogs answer in order, cancel when unscripted, and record requests") {
    val csv     = right(FileKind.of("Fixation CSV", Vector("csv")))
    val request = FileRequest("Import sources", Vector(csv), None)
    val save    = FileRequest("Export figure", Vector(csv), Some("figure-1.csv"))
    for
      p      <- InMemoryPlatform.create[IO]()
      _      <- p.answer(Some(path("/data/a.csv")))
      first  <- p.platform.dialogs.chooseOpen(request)
      second <- p.platform.dialogs.chooseSave(save)
      _      <- p.answer(Some(path("/data/study.eyes")))
      third  <- p.platform.dialogs.chooseDirectory("Open project")
      asked  <- p.requests
    yield
      assertEquals(first, Some(path("/data/a.csv")))
      assertEquals(second, None)
      assertEquals(third, Some(path("/data/study.eyes")))
      assertEquals(
        asked,
        Vector(
          DialogRequest.Open(request),
          DialogRequest.Save(save),
          DialogRequest.Directory("Open project")
        )
      )
  }

  test(
    "in memory: files make their directories, list direct children only, and refuse a directory"
  ) {
    for
      p <- InMemoryPlatform.create[IO]()
      fs = p.platform.files
      _     <- ok(fs.write(path("/data/raw/a.csv"), IArray.empty[Byte]))
      _     <- ok(fs.write(path("/data/b.csv"), IArray.empty[Byte]))
      root  <- ok(fs.list(path("/")))
      data  <- ok(fs.list(path("/data")))
      none  <- fs.list(path("/nowhere"))
      dir   <- fs.write(path("/data"), IArray.empty[Byte])
      fonts <- ok(p.platform.fonts.register(faces.head)) >> p.registered
    yield
      assertEquals(root, Vector(path("/data")))
      assertEquals(data, Vector(path("/data/b.csv"), path("/data/raw")))
      assertEquals(none, Left(PlatformError.Missing(path("/nowhere"))))
      assertEquals(dir, Left(PlatformError.Unwritable(path("/data"), "a directory")))
      assertEquals(fonts, Vector(faces.head))
  }

  test(
    "in memory: advancing the clock runs due tasks in due order, tasks scheduled by tasks too"
  ) {
    for
      p   <- InMemoryPlatform.create[IO]()
      log <- Ref.of[IO, Vector[String]](Vector.empty)
      s = p.platform.scheduler
      _ <- s.after(30.millis)(log.update(_ :+ "c"))
      _ <- s.after(10.millis)(
        log.update(_ :+ "a") >> s.after(5.millis)(log.update(_ :+ "b")).void
      )
      _   <- s.after(100.millis)(log.update(_ :+ "late"))
      _   <- p.advance(50.millis)
      at  <- s.now
      out <- log.get
    yield
      assertEquals(out, Vector("a", "b", "c"))
      assertEquals(at.epochMillis, 50L)
  }

  test("the effect-timer scheduler runs, cancels and reads the host's offset") {
    val scheduler = Scheduler.temporal[IO](_ => 60)
    for
      ran       <- Ref.of[IO, Int](0)
      _         <- scheduler.after(10.millis)(ran.update(_ + 1))
      cancelled <- scheduler.after(300.millis)(ran.update(_ + 10))
      _         <- cancelled.cancel
      _         <- IO.sleep(500.millis)
      count     <- ran.get
      now       <- scheduler.now
      real      <- IO.realTime
    yield
      assertEquals(count, 1)
      assertEquals(now.utcOffsetMinutes, 60)
      assert(Math.abs(real.toMillis - now.epochMillis) < 5000L, (real, now))
  }

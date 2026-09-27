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
import cats.syntax.all.*
import eyes4s.studio.core.bundle.{BundlePath, LockOwner}
import munit.CatsEffectSuite

import scala.concurrent.duration.*

/** The contract of every host's platform services (ticket S0.9,
  * docs/studio/PORTING.md), run on [[InMemoryPlatform]] here and on the
  * JVM services in studio-desktop. Dialogs are interactive and are checked
  * per host; the rest is checked through the interfaces alone.
  */
abstract class PlatformConformance extends CatsEffectSuite:
  import PlatformConformance.*

  /** A fresh platform and what the suite needs to drive it. */
  def subject: Resource[IO, Subject]

  protected def ok[A](io: IO[Either[PlatformError, A]]): IO[A] =
    io.flatMap(_.fold(e => IO.raiseError(new AssertionError(e.message)), IO.pure))

  protected def right[A](e: Either[PlatformError, A]): A =
    e.fold(x => fail(x.message), identity)

  private def bytes(text: String): IArray[Byte] =
    IArray.from(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  private def text(b: IArray[Byte]): String =
    new String(Array.from(b), java.nio.charset.StandardCharsets.UTF_8)

  test("files: a write reads back exactly, a rewrite replaces, a missing file is named") {
    subject.use { s =>
      val fs = s.platform.files
      val a  = right(fs.child(s.directory, "a.csv"))
      val b  = right(fs.child(s.directory, "b.csv"))
      for
        _       <- ok(fs.write(a, bytes("one")))
        _       <- ok(fs.write(a, bytes("two")))
        _       <- ok(fs.write(b, IArray.empty[Byte]))
        read    <- ok(fs.read(a))
        empty   <- ok(fs.read(b))
        missing <- fs.read(right(fs.child(s.directory, "c.csv")))
        listed  <- ok(fs.list(s.directory))
      yield
        assertEquals(text(read), "two")
        assertEquals(empty.length, 0)
        assertEquals(
          missing,
          Left(PlatformError.Missing(right(fs.child(s.directory, "c.csv"))))
        )
        assertEquals(listed, Vector(a, b))
    }
  }

  test(
    "files: a child name may not be blank, a separator, '.', '..' or hold a control character"
  ) {
    subject.use { s =>
      IO {
        Vector("", ".", "..", "a/b", "a\\b", "a\u0000b", "tab\there").foreach { bad =>
          assertEquals(
            s.platform.files.child(s.directory, bad),
            Left(PlatformError.InvalidName(s.directory, bad))
          )
        }
      }
    }
  }

  test("files: a directory or the root is not a file, and a file is not a directory") {
    subject.use { s =>
      val fs   = s.platform.files
      val file = right(fs.child(s.directory, "a.csv"))
      val gone = right(fs.child(s.directory, "gone"))
      def refusal(e: Either[PlatformError, Any]): (String, HostPath) = e match
        case Left(PlatformError.Unwritable(p, _)) => ("unwritable", p)
        case Left(PlatformError.Unreadable(p, _)) => ("unreadable", p)
        case Left(PlatformError.Missing(p))       => ("missing", p)
        case other                                => fail(s"not a refusal: $other")
      for
        _         <- ok(fs.write(file, bytes("x")))
        intoDir   <- fs.write(s.directory, bytes("x"))
        intoRoot  <- fs.write(s.root, bytes("x"))
        readDir   <- fs.read(s.directory)
        listFile  <- fs.list(file)
        listGone  <- fs.list(gone)
        streamDir <- fs.readStream(s.directory, 8)
        still     <- ok(fs.read(file))
      yield
        assertEquals(refusal(intoDir), ("unwritable", s.directory))
        assertEquals(refusal(intoRoot), ("unwritable", s.root))
        assertEquals(refusal(readDir), ("unreadable", s.directory))
        assertEquals(refusal(listFile), ("unreadable", file))
        assertEquals(refusal(listGone), ("missing", gone))
        assertEquals(refusal(streamDir), ("unreadable", s.directory))
        assertEquals(text(still), "x")
    }
  }

  test("files: a streamed read yields the file's bytes in chunks of at most the size asked") {
    subject.use { s =>
      val fs   = s.platform.files
      val file = right(fs.child(s.directory, "large.asc"))
      val data = IArray.tabulate[Byte](1000)(i => (i % 251).toByte)
      val gone = right(fs.child(s.directory, "gone.asc"))
      for
        _       <- ok(fs.write(file, data))
        stream  <- ok(fs.readStream(file, 7))
        chunks  <- stream.chunks.compile.toVector
        missing <- fs.readStream(gone, 7)
      yield
        assertEquals(chunks.flatMap(_.toVector), data.toVector)
        assert(chunks.forall(c => c.size >= 1 && c.size <= 7), chunks.map(_.size))
        assertEquals(missing.map(_ => ()), Left(PlatformError.Missing(gone)))
    }
  }

  test("files: a project opened twice is two views of one bundle") {
    subject.use { s =>
      val fs    = s.platform.files
      val where = right(fs.child(s.directory, "study.eyes"))
      val entry = BundlePath.of("figures/f.json").fold(e => fail(e.message), identity)
      val owner = LockOwner.of("conformance").fold(e => fail(e.message), identity)
      for
        one  <- ok(fs.project(where))
        two  <- ok(fs.project(where))
        lock <- one.acquire(owner).map(_.fold(e => fail(e.message), identity))
        _    <- one.write(lock, entry, bytes("{}"))
        read <- two.read(entry)
        held <- two.acquire(owner)
      yield
        assertEquals(read.map(text), Right("{}"))
        assert(held.isLeft, held)
    }
  }

  test("clipboard: written text reads back where the host can read it") {
    subject.use { s =>
      val clip = s.platform.clipboard
      for
        _    <- ok(clip.writeText("Run 8 · Comparing · 21,400 / 44,845 pairs"))
        back <- clip.readText
      yield
        if s.platform.capabilities.clipboardRead then
          assertEquals(back, Right(Some("Run 8 · Comparing · 21,400 / 44,845 pairs")))
        else assertEquals(back, Left(PlatformError.Unsupported("clipboard", "reading")))
    }
  }

  test("fonts: every face the host bundles registers") {
    subject.use(s => s.fonts.traverse_(f => ok(s.platform.fonts.register(f))))
  }

  test("preferences: put, get, replace and remove by key") {
    subject.use { s =>
      val prefs = s.platform.preferences
      val key   = right(PreferenceKey.of("studio.recent.0"))
      for
        none    <- ok(prefs.get(key))
        _       <- ok(prefs.put(key, "/tmp/a.eyes"))
        _       <- ok(prefs.put(key, "/tmp/b.eyes"))
        some    <- ok(prefs.get(key))
        _       <- ok(prefs.remove(key))
        removed <- ok(prefs.get(key))
      yield
        assertEquals(none, None)
        assertEquals(some, Some("/tmp/b.eyes"))
        assertEquals(removed, None)
    }
  }

  test("external open: a URL and a reveal are handed to the host") {
    subject.use { s =>
      val url    = ExternalTarget.Url(right(ExternalUrl.of("https://example.org/eyes4s")))
      val reveal = ExternalTarget.Reveal(s.directory)
      for
        _     <- ok(s.platform.external.open(url))
        _     <- ok(s.platform.external.open(reveal))
        shown <- s.shown
      yield assertEquals(shown, Vector(url, reveal).map(s.rendered))
    }
  }

  test("scheduler: the clock does not go back; a task runs once after its delay") {
    subject.use { s =>
      val scheduler = s.platform.scheduler
      for
        ran    <- Ref.of[IO, Int](0)
        before <- scheduler.now
        _      <- scheduler.after(200.millis)(ran.update(_ + 1))
        early  <- ran.get
        _      <- s.elapse(700.millis)
        late   <- ran.get
        _      <- s.elapse(700.millis)
        after  <- scheduler.now
        once   <- ran.get
      yield
        assertEquals((early, late, once), (0, 1, 1))
        assert(after.epochMillis >= before.epochMillis + 1400, (before, after))
    }
  }

  test("scheduler: a task cancelled before its delay never runs") {
    subject.use { s =>
      for
        ran       <- Ref.of[IO, Boolean](false)
        scheduled <- s.platform.scheduler.after(300.millis)(ran.set(true))
        _         <- scheduled.cancel
        _         <- s.elapse(600.millis)
        _         <- scheduled.cancel
        result    <- ran.get
      yield assert(!result)
    }
  }

object PlatformConformance:

  /** A platform under test.
    *
    * @param directory an existing, empty, writable directory
    * @param root the file system's root, which is never written to
    * @param fonts faces the host bundles
    * @param shown what the host handed to its browser or file browser, in order
    * @param rendered what the host hands over for a target
    * @param elapse lets `by` pass on the platform's clock: a virtual clock
    *   advances, a real one sleeps
    */
  final case class Subject(
      platform: Platform[IO],
      directory: HostPath,
      root: HostPath,
      fonts: Vector[FontRequest],
      shown: IO[Vector[String]],
      rendered: ExternalTarget => String,
      elapse: FiniteDuration => IO[Unit]
  )

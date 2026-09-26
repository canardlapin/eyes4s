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
import eyes4s.studio.core.bundle.{BundleSamples, ProjectStore, SharingOptions}
import eyes4s.studio.core.command.CommandSamples
import eyes4s.studio.core.document.DocumentSamples.t2
import eyes4s.studio.core.session.*

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}

/** A new bundle directory; each view is a new [[FileProjectStore]] on it. */
private object FileBundles:
  val bundle: Resource[IO, IO[ProjectStore[IO]]] =
    TempDirs
      .resource("eyes4s-session")
      .map(dir => FileProjectStore.at[IO](dir.resolve("project.eyes")))

/** Atomic save and the single-writer lock on the file system (S2.4a). */
class FileAtomicSaveFaultSuite extends AtomicSaveFaultConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] = FileBundles.bundle

/** Cancelling session operations on the file system (S2.4a review). */
class FileSessionCancellationSuite extends SessionCancellationConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] = FileBundles.bundle

/** Faults beyond the save on the file system (S2.4a/b review). */
class FileRecoveryFaultSuite extends RecoveryFaultConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] = FileBundles.bundle

/** The autosave journal and crash recovery on the file system (S2.4b). */
class FileRecoverySuite extends RecoveryConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] = FileBundles.bundle

  private val root: Resource[IO, Path] =
    TempDirs.resource("eyes4s-session").map(_.resolve("project.eyes"))

  test("the journal and the previous manifest are files beside project.json") {
    root.use { dir =>
      for
        store   <- FileProjectStore.at[IO](dir)
        session <- ok(
          ProjectSession.create(
            store,
            alice,
            t2,
            SharingOptions.complete,
            BundleSamples.inputsFor(t2)
          )
        )
        _     <- ok(session.perform(CommandSamples.session.head))
        _     <- ok(session.save)
        _     <- ok(session.perform(CommandSamples.session(1)))
        files <- IO.blocking(TempDirs.files(dir))
        text  <- IO.blocking(Files.readString(dir.resolve("project.journal"), UTF_8))
        _     <- ok(session.close)
      yield
        assert(files.contains("project.json"), files)
        assert(files.contains("project.journal"), files)
        assert(files.contains("project.previous.json"), files)
        assert(!files.exists(_.contains("staged")), files)
        assertEquals(text.count(_ == '\n'), 2)
        assert(text.endsWith("\n"))
    }
  }

  test("a journal torn on disk mid-line reopens with the entries before the tear") {
    root.use { dir =>
      for
        store   <- FileProjectStore.at[IO](dir)
        session <- ok(
          ProjectSession.create(
            store,
            alice,
            t2,
            SharingOptions.complete,
            BundleSamples.inputsFor(t2)
          )
        )
        _    <- ok(session.perform(CommandSamples.session.head))
        live <- session.document
        _    <- ok(session.close)
        // A crash that wrote part of the next line.
        _ <- IO.blocking(
          Files.writeString(
            dir.resolve("project.journal"),
            "{\"schema\":{\"name\":\"studio.jour",
            UTF_8,
            StandardOpenOption.APPEND
          )
        )
        other  <- FileProjectStore.at[IO](dir)
        opened <- ok(ProjectSession.open(other, bob))
        _      <- ok(opened.session.close)
      yield opened.report.journal match
        case JournalFinding.Pending(Recovery.Offered(offer)) =>
          assertEquals(offer.recovered, live)
          assertEquals(offer.restores.size, 1)
          assertEquals(offer.torn.map(_.line), Some(3))
        case other => fail(s"expected an offer, got $other")
    }
  }

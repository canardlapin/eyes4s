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
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.{SourceRole, StudioDocument}
import eyes4s.studio.core.document.DocumentSamples.t2
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

/** The golden t2 bundle on disk, fixtures/studio-bundles/t2.eyes (ticket
  * S2.3): the files the file-system store writes for the t2 story moment.
  * Its inputs are not checked in (they are fixtures/studio-golden's CSVs);
  * the round trip imports them from there.
  *
  * Regenerate with `-Deyes4s.studio.writeGoldenBundle=true` after a
  * deliberate format change, and update BundlePins with it.
  */
class ProjectBundleGoldenSuite extends CatsEffectSuite:
  import BundleSamples.{inputsFor, right}

  private val buildRoot: Path = Paths.get(
    String(
      getClass.getClassLoader
        .getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
        .readAllBytes,
      UTF_8
    ).trim
  )
  private val golden  = buildRoot.resolve("fixtures/studio-bundles/t2.eyes")
  private val sources = buildRoot.resolve("fixtures/studio-golden")
  private val me      = right(LockOwner.of("ProjectBundleGoldenSuite"))

  private def ok[E, A](io: IO[Either[E, A]]): IO[A] =
    io.flatMap(_.fold(e => IO.raiseError(new AssertionError(e.toString)), IO.pure))

  private def bytes(file: Path): IArray[Byte] = IArray.unsafeFromArray(Files.readAllBytes(file))

  private def tree(root: Path): Vector[(String, String)] =
    TempDirs
      .files(root)
      .filterNot(_ == FileProjectStore.LockName)
      .map(f => f -> ByteDigest.sha256(bytes(root.resolve(f))).hex)

  private def copy(from: Path, to: Path): Unit =
    TempDirs.files(from).foreach { f =>
      val target = to.resolve(f)
      Files.createDirectories(target.getParent)
      Files.copy(from.resolve(f), target, StandardCopyOption.REPLACE_EXISTING)
    }

  private val encoded = right(ProjectBundle.encode(t2, SharingOptions.complete, inputsFor(t2)))

  if sys.props.get("eyes4s.studio.writeGoldenBundle").contains("true") then
    test("write the golden bundle") {
      for
        _     <- IO.blocking(TempDirs.remove(golden))
        store <- FileProjectStore.at[IO](golden)
        lock  <- ok(store.acquire(me))
        _     <- ok(ProjectBundle.save(store, lock, None, encoded))
        _     <- ok(store.release(lock))
        _     <- IO.blocking(Files.deleteIfExists(golden.resolve(FileProjectStore.LockName)))
      yield ()
    }

  test("the golden bundle is exactly the files the store writes for t2") {
    val expected =
      (encoded.parts.map((p, b) => p.value -> ByteDigest.sha256(b).hex) :+
        (ProjectStore.ManifestName -> ByteDigest.sha256(encoded.manifestBytes).hex))
        .sortBy(_._1)
    assertEquals(tree(golden), expected)
    assertEquals(
      ByteDigest.sha256(bytes(golden.resolve("project.json"))).hex,
      BundlePins.t2ManifestSha256
    )
  }

  test("the golden bundle opens as t2; its inputs match fixtures/studio-golden byte for byte") {
    for
      store  <- FileProjectStore.at[IO](golden)
      opened <- ok(ProjectBundle.open(store))
      status <- ProjectBundle.checkInputs(store, opened.manifest)
    yield
      assertEquals(opened.document, t2)
      assertEquals(status, opened.manifest.inputs.map(InputStatus.Missing(_)))
      val onDisk = opened.manifest.inputs.map { e =>
        val file = sources.resolve(e.name)
        (e.name, Files.size(file), ByteDigest.sha256(bytes(file)))
      }
      assertEquals(onDisk, opened.manifest.inputs.map(e => (e.name, e.length, e.sha256)))
  }

  test("open → import inputs → save → open on disk is lossless, and cache/ is disposable") {
    TempDirs.resource("eyes4s-golden").use { tmp =>
      val root = tmp.resolve("t2.eyes")
      for
        _       <- IO.blocking(copy(golden, root))
        store   <- FileProjectStore.at[IO](root)
        opened  <- ok(ProjectBundle.open(store))
        lock    <- ok(store.acquire(me))
        entries <- Vector(
          SourceRole.Fixations -> "fixations.csv",
          SourceRole.Trials    -> "trials.csv"
        )
          .traverse((role, name) =>
            IO.blocking(bytes(sources.resolve(name)))
              .flatMap(b =>
                ok(ProjectBundle.importInput(store, lock, InputKind.Source(role), name, b))
              )
          )
        again = right(
          ProjectBundle.encode(opened.document, opened.manifest.sharing, opened.manifest.inputs)
        )
        _ <- ok(ProjectBundle.save(store, lock, Some(opened.manifestDigest), again))
        _ <- ok(
          store.write(lock, right(BundlePath.of("cache/maps/run7.bin")), IArray[Byte](1, 2, 3))
        )
        withCache <- ok(ProjectBundle.open(store))
        _         <- ok(store.delete(lock, right(BundlePath.of("cache/maps/run7.bin"))))
        reopened  <- ok(ProjectBundle.open(store))
        status    <- ProjectBundle.checkInputs(store, reopened.manifest)
        _         <- ok(store.release(lock))
      yield
        assertEquals(entries.sortBy(_.path), opened.manifest.inputs)
        assertEquals(reopened, opened)
        assertEquals(withCache, opened)
        assertEquals(
          StudioDocument.scienceDigest(reopened.document).map(_.display),
          StudioDocument.scienceDigest(t2).map(_.display)
        )
        assertEquals(status, opened.manifest.inputs.map(InputStatus.Present(_)))
        assertEquals(
          tree(root).filterNot(_._1.startsWith("inputs/")),
          tree(golden)
        )
        assert(!Files.exists(root.resolve("cache")))
    }
  }

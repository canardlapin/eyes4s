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
import eyes4s.studio.core.assets.DisplayKind
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.DocumentSamples.t2
import eyes4s.studio.core.fixture.{ExampleFile, ExampleProject, ExampleSources}
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The bundled example, fixtures/studio-bundles/example.eyes (ticket S0.8):
  * story moment t2 of fixtures/studio-golden with its sources and all 257
  * stimulus images stored.
  *
  * The first test is the `--check`: it regenerates the bundle in memory from
  * fixtures/studio-golden and requires the checked-in files to be exactly
  * those bytes. Regenerate with `-Deyes4s.studio.writeExampleBundle=true`.
  */
class ExampleBundleSuite extends CatsEffectSuite:

  private val buildRoot: Path = Paths.get(
    String(
      getClass.getClassLoader
        .getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
        .readAllBytes,
      UTF_8
    ).trim
  )
  private val example = buildRoot.resolve("fixtures/studio-bundles/example.eyes")
  private val golden  = buildRoot.resolve("fixtures/studio-golden")
  private val me      = LockOwner.of("ExampleBundleSuite").toOption.get

  private def ok[E, A](io: IO[Either[E, A]]): IO[A] =
    io.flatMap(_.fold(e => IO.raiseError(new AssertionError(e.toString)), IO.pure))

  private def bytes(file: Path): IArray[Byte] = IArray.unsafeFromArray(Files.readAllBytes(file))

  private def file(path: Path): ExampleFile =
    ExampleFile(path.getFileName.toString, bytes(path))

  private val sources: IO[ExampleSources] = IO.blocking {
    val stimuli = Files.list(golden.resolve("stimuli"))
    try
      ExampleSources(
        file(golden.resolve("fixations.csv")),
        file(golden.resolve("trials.csv")),
        stimuli.iterator.asScala.filter(_.toString.endsWith(".png")).map(file).toVector
      )
    finally stimuli.close()
  }

  private def writeTo(store: ProjectStore[IO]): IO[ByteDigest] =
    for
      src  <- sources
      lock <- ok(store.acquire(me))
      d    <- ok(ExampleProject.write(store, lock, t2, src))
      _    <- ok(store.release(lock))
    yield d

  /** Every file of a bundle with its SHA-256, the manifest included. */
  private def tree(store: ProjectStore[IO]): IO[Vector[(String, String)]] =
    for
      paths    <- ok(store.list)
      entries  <- paths.traverse(p => ok(store.read(p)).map(b => p.value -> b))
      manifest <- ok(store.readManifest)
    yield ((ProjectStore.ManifestName -> manifest) +: entries)
      .map((p, b) => p -> ByteDigest.sha256(b).hex)
      .sortBy(_._1)

  private def onDisk(root: Path): Vector[(String, String)] =
    TempDirs
      .files(root)
      .filterNot(_ == FileProjectStore.LockName)
      .map(f => f -> ByteDigest.sha256(bytes(root.resolve(f))).hex)

  if sys.props.get("eyes4s.studio.writeExampleBundle").contains("true") then
    test("write the example bundle") {
      for
        _     <- IO.blocking(TempDirs.remove(example))
        store <- FileProjectStore.at[IO](example)
        _     <- writeTo(store)
        _     <- IO.blocking(Files.deleteIfExists(example.resolve(FileProjectStore.LockName)))
      yield ()
    }

  test("--check: the example bundle is exactly what regeneration from studio-golden writes") {
    for
      memory   <- InMemoryProjectStore.create[IO]
      digest   <- writeTo(memory)
      again    <- InMemoryProjectStore.create[IO]
      digest2  <- writeTo(again)
      expected <- tree(memory)
    yield
      assertEquals(digest2, digest)
      assertEquals(onDisk(example), expected)
      assertEquals(
        ByteDigest.sha256(bytes(example.resolve(ProjectStore.ManifestName))),
        digest
      )
  }

  test("the example opens as t2 with every input present: 2 sources and 257 stimuli") {
    for
      store  <- FileProjectStore.at[IO](example)
      opened <- ok(ProjectBundle.open(store))
      status <- ProjectBundle.checkInputs(store, opened.manifest)
    yield
      assertEquals(opened.document, t2)
      assert(opened.science.verified)
      assertEquals(opened.manifest.sharing, SharingOptions.complete)
      assertEquals(status, opened.manifest.inputs.map(InputStatus.Present(_)))
      assertEquals(
        opened.manifest.inputs.groupMapReduce(_.kind)(_ => 1)(_ + _),
        Map(
          InputKind.Source(eyes4s.studio.core.document.SourceRole.Fixations) -> 1,
          InputKind.Source(eyes4s.studio.core.document.SourceRole.Trials)    -> 1,
          InputKind.StimulusImage                                            -> 257
        )
      )
      val registry = ExampleProject.registry(opened).fold(m => fail(m), identity)
      assertEquals((registry.summary.files, registry.summary.present), (259, 257))
      assertEquals(
        registry.missing.map(_.file.value),
        Vector("forest-044.png", "kitchen-081.png")
      )
      assertEquals(registry.count(DisplayKind.BlankWithFixationCross), 480)
  }

  test("opening the example copies it as 'memory-study (copy)'; the example is untouched") {
    TempDirs.resource("eyes4s-example").use { tmp =>
      val root = tmp.resolve("memory-study (copy).eyes")
      for
        before <- IO.blocking(onDisk(example))
        from   <- FileProjectStore.at[IO](example)
        to     <- FileProjectStore.at[IO](root)
        lock   <- ok(to.acquire(me))
        copy   <- ok(ExampleProject.openCopy(from, to, lock))
        status <- ProjectBundle.checkInputs(to, copy.project.manifest)
        _      <- ok(to.release(lock))
        after  <- IO.blocking(onDisk(example))
        copied <- IO.blocking(onDisk(root))
      yield
        assertEquals(copy.name, "memory-study (copy)")
        assertEquals(copy.project.document, t2)
        assertEquals(status, copy.project.manifest.inputs.map(InputStatus.Present(_)))
        assertEquals(after, before)
        assertEquals(copied, before)
    }
  }

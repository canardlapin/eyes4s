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
import cats.syntax.all.*
import eyes4s.studio.core.bundle.{ProjectStore, ProjectStoreConformance, StoreError}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.jdk.CollectionConverters.*

/** Temporary directories for store tests, removed afterwards. */
object TempDirs:
  def resource(prefix: String): Resource[IO, Path] =
    Resource.make(IO.blocking(Files.createTempDirectory(prefix)))(dir =>
      IO.blocking(remove(dir))
    )

  def remove(dir: Path): Unit =
    if Files.exists(dir) then
      val walk = Files.walk(dir)
      try walk.sorted(Comparator.reverseOrder()).iterator().asScala.foreach(Files.delete(_))
      finally walk.close()

  /** Every file under `dir`, relative, with `/` separators, in order. */
  def files(dir: Path): Vector[String] =
    val walk = Files.walk(dir)
    try
      walk
        .iterator()
        .asScala
        .filter(Files.isRegularFile(_))
        .map(f => dir.relativize(f).iterator().asScala.mkString("/"))
        .toVector
        .sorted
    finally walk.close()

/** The file-system store passes the [[ProjectStoreConformance]] suite, with
  * two store instances on one directory as the two views, and keeps its own
  * files out of the bundle's entries.
  */
class FileProjectStoreSuite extends ProjectStoreConformance:

  private val bundle: Resource[IO, Path] =
    TempDirs.resource("eyes4s-store").map(_.resolve("project.eyes"))

  def views: Resource[IO, (ProjectStore[IO], ProjectStore[IO])] =
    bundle.evalMap(root =>
      (FileProjectStore.at[IO](root), FileProjectStore.at[IO](root)).tupled
    )

  test(
    "the directory holds entries, project.json and the lock files only; staging never lingers"
  ) {
    bundle.use { root =>
      for
        store <- FileProjectStore.at[IO](root)
        lock  <- ok(store.acquire(owner("alice")))
        _     <- ok(store.write(lock, path("runs/7/run.json"), bytes("{}")))
        _     <- ok(store.swapManifest(lock, None, bytes("{}")))
        lost  <- store.swapManifest(lock, None, bytes("{}"))
        _     <- ok(store.write(lock, path("cache/a/b.bin"), bytes("x")))
        _     <- ok(store.delete(lock, path("cache/a/b.bin")))
        files <- IO.blocking(TempDirs.files(root))
        held  <- IO.blocking(String(Files.readAllBytes(root.resolve(".lock.owner")), UTF_8))
        _     <- ok(store.release(lock))
        after <- IO.blocking(TempDirs.files(root))
      yield
        assert(lost.isLeft)
        assertEquals(files, Vector(".lock", ".lock.owner", "project.json", "runs/7/run.json"))
        assert(
          !Files.exists(root.resolve("cache")),
          "deleting the last cache entry removes cache/"
        )
        assertEquals(held, "alice")
        assertEquals(after, Vector(".lock", "project.json", "runs/7/run.json"))
    }
  }

  test("another process is refused while the lock is held, even after in-process contention") {
    bundle.use { root =>
      for
        store     <- FileProjectStore.at[IO](root)
        other     <- FileProjectStore.at[IO](root)
        lock      <- ok(store.acquire(owner("parent")))
        inJvm     <- other.acquire(owner("sibling"))
        inJvm2    <- store.acquire(owner("sibling"))
        whileHeld <- LockProbe.run(root)
        _         <- ok(store.write(lock, path("figures/f.json"), bytes("x")))
        _         <- ok(store.release(lock))
        released  <- LockProbe.run(root)
        again     <- ok(other.acquire(owner("sibling")))
        _         <- ok(other.release(again))
      yield
        assertEquals(inJvm, Left(StoreError.Locked(owner("sibling"), Some(owner("parent")))))
        assertEquals(inJvm2, Left(StoreError.Locked(owner("sibling"), Some(owner("parent")))))
        assertEquals(whileHeld, "LOCKED parent")
        assertEquals(released, "ACQUIRED")
    }
  }

  test("an entry reached through a symbolic link is refused, and nothing is created outside") {
    TempDirs.resource("eyes4s-outside").use { outside =>
      bundle.use { root =>
        for
          _ <- IO.blocking {
            Files.writeString(outside.resolve("secret.json"), "{}")
            Files.createDirectories(root.resolve("datasets"))
            Files.createSymbolicLink(
              root.resolve("datasets/r1.json"),
              outside.resolve("secret.json")
            )
            Files.createSymbolicLink(root.resolve("figures"), outside)
            Files.createSymbolicLink(root.resolve("runs"), outside)
          }
          store  <- FileProjectStore.at[IO](root)
          link   <- store.read(path("datasets/r1.json"))
          viaDir <- store.read(path("figures/secret.json"))
          lock   <- ok(store.acquire(owner("alice")))
          write  <- store.write(lock, path("figures/escape.json"), bytes("x"))
          run    <- store.write(lock, path("runs/7/run.json"), bytes("x"))
          del    <- store.delete(lock, path("figures/secret.json"))
          listed <- ok(store.list)
          _      <- ok(store.release(lock))
        yield
          assert(link.left.exists(_.message.contains("symbolic link")), link)
          assert(viaDir.left.exists(_.message.contains("outside the bundle")), viaDir)
          assert(write.left.exists(_.isInstanceOf[StoreError.Unwritable]), write)
          assert(run.left.exists(_.message.contains("runs is a symbolic link")), run)
          assert(del.isLeft, del)
          assertEquals(listed, Vector.empty)
          assertEquals(TempDirs.files(outside), Vector("secret.json"))
          assert(
            !Files.exists(outside.resolve("7")),
            "no directory is created through the link"
          )
      }
    }
  }

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

package eyes4s.studio.core.bundle

import cats.effect.{IO, Resource}
import eyes4s.codec.ByteDigest
import munit.CatsEffectSuite

/** The [[ProjectStore]] contract (ticket S2.3), run against every
  * implementation: [[InMemoryProjectStoreSuite]] here and the file-system
  * store's suite in studio-desktop.
  */
abstract class ProjectStoreConformance extends CatsEffectSuite:

  /** Two views of one new, empty bundle: the same store twice, or two store
    * instances on the same location.
    */
  def views: Resource[IO, (ProjectStore[IO], ProjectStore[IO])]

  protected def path(value: String): BundlePath =
    BundlePath.of(value).fold(e => fail(e.message), identity)

  protected def owner(name: String): LockOwner =
    LockOwner.of(name).fold(e => fail(e.message), identity)

  protected def bytes(text: String): IArray[Byte] =
    IArray.from(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  protected def ok[A](io: IO[Either[StoreError, A]]): IO[A] =
    io.flatMap(_.fold(e => IO.raiseError(new AssertionError(e.message)), IO.pure))

  private def same(a: IArray[Byte], b: IArray[Byte]): Boolean =
    a.length == b.length && a.indices.forall(i => a(i) == b(i))

  private val alice = "Eyes Studio · alice"
  private val bob   = "Eyes Studio · bob"

  test("an empty bundle has no manifest, no entries and refuses a missing read by path") {
    views.use { (store, _) =>
      for
        manifest <- store.readManifest
        listed   <- store.list
        missing  <- store.read(path("datasets/r1.json"))
      yield
        assertEquals(manifest, Left(StoreError.NoManifest))
        assertEquals(listed, Right(Vector.empty))
        assertEquals(missing, Left(StoreError.Missing(path("datasets/r1.json"))))
    }
  }

  test(
    "a write returns exactly its bytes, empty and all 256 byte values alike, and a rewrite replaces them"
  ) {
    views.use { (store, other) =>
      val every = IArray.tabulate[Byte](256)(_.toByte)
      for
        lock  <- ok(store.acquire(owner(alice)))
        _     <- ok(store.write(lock, path("inputs/abc/every.bin"), every))
        _     <- ok(store.write(lock, path("cache/empty"), IArray.empty[Byte]))
        _     <- ok(store.write(lock, path("figures/f.json"), bytes("one")))
        _     <- ok(store.write(lock, path("figures/f.json"), bytes("two")))
        read  <- ok(other.read(path("inputs/abc/every.bin")))
        empty <- ok(other.read(path("cache/empty")))
        f     <- ok(other.read(path("figures/f.json")))
      yield
        assert(same(read, every))
        assertEquals(empty.length, 0)
        assert(same(f, bytes("two")))
    }
  }

  test("list returns every entry in path order, nested ones included, and never the manifest") {
    views.use { (store, other) =>
      val paths =
        Vector("runs/7/run.json", "cache/x/y/z", "analyses/rev4.json", "runs/10/run.json")
      for
        lock <- ok(store.acquire(owner(alice)))
        _    <- ok(store.swapManifest(lock, None, bytes("{}")))
        _ <- paths.foldLeft(IO.unit)((io, p) => io *> ok(store.write(lock, path(p), bytes(p))))
        listed <- ok(other.list)
      yield assertEquals(listed.map(_.value), paths.sorted)
    }
  }

  test("delete removes an entry and refuses a missing one by path") {
    views.use { (store, other) =>
      for
        lock    <- ok(store.acquire(owner(alice)))
        _       <- ok(store.write(lock, path("cache/a/b"), bytes("x")))
        _       <- ok(store.delete(lock, path("cache/a/b")))
        again   <- store.delete(lock, path("cache/a/b"))
        listed  <- ok(other.list)
        missing <- other.read(path("cache/a/b"))
      yield
        assertEquals(again, Left(StoreError.Missing(path("cache/a/b"))))
        assertEquals(listed, Vector.empty)
        assertEquals(missing, Left(StoreError.Missing(path("cache/a/b"))))
    }
  }

  test(
    "the writer lock is exclusive across views; a second writer is refused naming the holder"
  ) {
    views.use { (store, other) =>
      for
        lock    <- ok(store.acquire(owner(alice)))
        second  <- other.acquire(owner(bob))
        same    <- store.acquire(owner(bob))
        _       <- ok(store.release(lock))
        granted <- ok(other.acquire(owner(bob)))
      yield
        assertEquals(second, Left(StoreError.Locked(owner(bob), Some(owner(alice)))))
        assertEquals(same, Left(StoreError.Locked(owner(bob), Some(owner(alice)))))
        assertEquals(granted.owner, owner(bob))
    }
  }

  test("every mutation needs the current lock: a released or forged lock is refused") {
    views.use { (store, _) =>
      val p = path("datasets/r1.json")
      for
        lock     <- ok(store.acquire(owner(alice)))
        _        <- ok(store.release(lock))
        write    <- store.write(lock, p, bytes("x"))
        swap     <- store.swapManifest(lock, None, bytes("{}"))
        released <- store.release(lock)
        current  <- ok(store.acquire(owner(bob)))
        forged = WriterLock.issue(owner(alice), "forged")
        deleted  <- store.delete(forged, p)
        forgedW  <- store.write(forged, p, bytes("x"))
        manifest <- store.readManifest
        listed   <- ok(store.list)
      yield
        assertEquals(write, Left(StoreError.NotHolder(owner(alice), None)))
        assertEquals(swap, Left(StoreError.NotHolder(owner(alice), None)))
        assertEquals(released, Left(StoreError.NotHolder(owner(alice), None)))
        assertEquals(deleted, Left(StoreError.NotHolder(owner(alice), Some(current.owner))))
        assertEquals(forgedW, Left(StoreError.NotHolder(owner(alice), Some(current.owner))))
        assertEquals(manifest, Left(StoreError.NoManifest))
        assertEquals(listed, Vector.empty)
    }
  }

  test("the manifest swap is a compare-and-swap on its digest; a lost race changes nothing") {
    views.use { (store, other) =>
      val (m1, m2, m3) = (bytes("""{"v":1}"""), bytes("""{"v":2}"""), bytes("""{"v":3}"""))
      val (d1, d2)     = (ByteDigest.sha256(m1), ByteDigest.sha256(m2))
      for
        lock  <- ok(store.acquire(owner(alice)))
        _     <- ok(store.swapManifest(lock, None, m1))
        fresh <- store.swapManifest(lock, None, m3)
        stale <- store.swapManifest(lock, Some(d2), m3)
        seen1 <- ok(other.readManifest)
        _     <- ok(store.swapManifest(lock, Some(d1), m2))
        seen2 <- ok(other.readManifest)
        list  <- ok(other.list)
      yield
        assertEquals(fresh, Left(StoreError.ManifestMoved(None, Some(d1))))
        assertEquals(stale, Left(StoreError.ManifestMoved(Some(d2), Some(d1))))
        assert(same(seen1, m1))
        assert(same(seen2, m2))
        assertEquals(list, Vector.empty)
    }
  }

/** The in-memory store passes the conformance suite. */
class InMemoryProjectStoreSuite extends ProjectStoreConformance:
  def views: Resource[IO, (ProjectStore[IO], ProjectStore[IO])] =
    Resource.eval(InMemoryProjectStore.create[IO]).map(s => (s, s))

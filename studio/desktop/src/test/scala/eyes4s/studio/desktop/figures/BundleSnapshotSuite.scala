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

package eyes4s.studio.desktop.figures

import cats.effect.IO
import eyes4s.studio.core.bundle.{BundlePath, LockOwner}
import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

class BundleSnapshotSuite extends munit.CatsEffectSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def bytes(text: String): IArray[Byte]   = IArray.from(text.getBytes(UTF_8))

  test("the published snapshot excludes writer locks and keeps every project payload") {
    TempDirs.resource("eyes4s-bundle-snapshot").use { root =>
      val target                      = root.resolve("export")
      val take: BundleWriter.Snapshot = (to, done) =>
        (for
          store <- FileProjectStore.at[IO](to)
          lock  <- store.acquire(right(LockOwner.of("snapshot writer"))).map(right)
          _     <- store
            .write(lock, right(BundlePath.of("datasets/r1.json")), bytes("dataset"))
            .map(right)
          _ <- store.swapManifest(lock, None, bytes("manifest")).map(right)
          _ <- store.release(lock).map(right)
          // A stale owner sidecar must not enter the export either.
          _ <- IO.blocking(
            Files.writeString(to.resolve(FileProjectStore.OwnerName), "stale owner")
          )
        yield ()).unsafeRunAsync(result => done(result.left.map(_.toString)))
      for
        result <- IO.async_[Either[String, String]](done =>
          BundleWriter.write(
            target,
            Vector("methods.md" -> bytes("methods")),
            Some(take),
            answer => done(Right(answer))
          )
        )
        files    <- IO.blocking(TempDirs.files(target))
        manifest <- IO.blocking(Files.readString(target.resolve("project/project.json")))
        reopened <- FileProjectStore.at[IO](target.resolve("project"))
        payload  <- reopened.read(right(BundlePath.of("datasets/r1.json"))).map(right)
        lock     <- reopened.acquire(right(LockOwner.of("reader"))).map(right)
        _        <- reopened.release(lock).map(right)
      yield
        assertEquals(result, Right(target.toString))
        assertEquals(
          files,
          Vector("methods.md", "project/datasets/r1.json", "project/project.json")
        )
        assertEquals(manifest, "manifest")
        assertEquals(String(Array.from(payload), UTF_8), "dataset")
    }
  }

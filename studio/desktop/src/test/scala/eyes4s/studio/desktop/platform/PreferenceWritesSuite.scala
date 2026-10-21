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
import cats.effect.unsafe.implicits.global
import eyes4s.studio.core.platform.HostPath
import eyes4s.studio.core.preferences.PreferencesStore
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class PreferenceWritesSuite extends munit.FunSuite:
  test("accepted writes retain submission order and shutdown drains without UI callbacks") {
    TempDirs
      .resource("eyes4s-preference-order-")
      .use { dir =>
        val path      = HostPath.of(dir.resolve("preferences.json").toString).toOption.get
        val store     = PreferencesStore[IO](JvmFileSystem, path, IO.pure(1L))
        val queue     = PreferenceWrites(DesktopPreferences(store))
        val callbacks = ConcurrentLinkedQueue[() => Unit]()
        val first     = HostPath.of(dir.resolve("first.eyes").toString).toOption.get
        val second    = HostPath.of(dir.resolve("second.eyes").toString).toOption.get
        val lockFile  = dir.resolve("preferences.json.lock")
        Resource
          .fromAutoCloseable(
            IO.blocking(
              FileChannel.open(lockFile, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
            )
          )
          .use { channel =>
            for
              lock <- IO.blocking(channel.lock())
              _    <- IO {
                queue.submit(p => p.withRecent(p.recent.opened(first)))(answer =>
                  callbacks.add(() => assert(answer.isRight)): Unit
                )
                queue.submit(p => p.withRecent(p.recent.opened(second)))(answer =>
                  callbacks.add(() => assert(answer.isRight)): Unit
                )
              }
              closing <- queue.close.start
              _       <- IO.sleep(50.millis)
              _       <- IO.blocking(lock.release())
              _       <- closing.joinWithNever.timeout(5.seconds)
              loaded  <- store.load
              _       <- IO {
                assertEquals(loaded._1.recent.paths, Vector(second, first))
                assertEquals(callbacks.size(), 2)
                callbacks.asScala.foreach(_())
                var refused = false
                queue.submit(identity)(answer =>
                  refused = answer.left.exists(_.message.contains("closed"))
                )
                assert(refused)
              }
            yield ()
          }
      }
      .unsafeRunSync()
  }

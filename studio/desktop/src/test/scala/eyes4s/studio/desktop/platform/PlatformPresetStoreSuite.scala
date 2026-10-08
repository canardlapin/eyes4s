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
import eyes4s.studio.core.document.{ColumnBinding, ColumnName, ColumnRole, TimeUnit}
import eyes4s.studio.core.importing.{ImportPreset, PresetName}
import eyes4s.studio.core.platform.{HostPath, InMemoryPlatform, PlatformError}
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8

class PlatformPresetStoreSuite extends CatsEffectSuite:
  private def ok[E, A](result: Either[E, A]): A = result.fold(e => fail(e.toString), identity)
  private val directory = ok(HostPath.of("/user/import-presets"))
  private def names(path: HostPath): Either[PlatformError, String] = Right(path.value.split('/').last)
  private def preset(name: String, column: String = "Subject"): ImportPreset =
    ok(ImportPreset.of(ok(PresetName.of(name)), Vector(ColumnBinding(ColumnRole.Participant, ok(ColumnName.of(column)))), Some(TimeUnit.Milliseconds)))

  test("independent in-memory writes preserve both presets and legacy UTF8-hex files") {
    for
      memory <- InMemoryPlatform.create[IO]()
      store = PlatformPresetStore(memory.platform.files, directory, names)
      initial <- store.load
      results <- (store.save(preset("Alpha")), store.save(preset("µ lab"))).parTupled
      loaded <- store.load
      files <- memory.platform.files.list(directory).map(ok)
    yield
      assertEquals(initial._1.all, Vector.empty)
      assertEquals(initial._2, Vector.empty)
      assertEquals(results, (Right(()), Right(())))
      assertEquals(loaded._1.all.toSet, Set(preset("Alpha"), preset("µ lab")))
      assertEquals(loaded._2, Vector.empty)
      assertEquals(files.map(_.value), Vector("/user/import-presets/416c706861.json", "/user/import-presets/c2b5206c6162.json"))
  }

  test("malformed JSON and UTF8 presets are reported by name") {
    for
      memory <- InMemoryPlatform.create[IO]()
      _ <- memory.platform.files.write(ok(HostPath.of("/user/import-presets/bad.json")), IArray.from("broken".getBytes(UTF_8)))
      _ <- memory.platform.files.write(ok(HostPath.of("/user/import-presets/utf8.json")), IArray.from(Vector(0xc3.toByte, 0x28.toByte)))
      store = PlatformPresetStore(memory.platform.files, directory, names)
      loaded <- store.load
    yield
      assertEquals(loaded._1.all, Vector.empty)
      assertEquals(loaded._2.size, 2)
      assert(loaded._2.exists(_.contains("bad.json")))
      assert(loaded._2.exists(e => e.contains("utf8.json") && e.contains("UTF-8")))
  }

  test("same-name replacement preserves other names rather than overwriting an index") {
    for
      memory <- InMemoryPlatform.create[IO]()
      store = PlatformPresetStore(memory.platform.files, directory, names)
      _ <- store.save(preset("Alpha"))
      _ <- store.save(preset("Other"))
      _ <- store.save(preset("Alpha", "Participant"))
      loaded <- store.load
    yield
      assertEquals(loaded._1.all.toSet, Set(preset("Alpha", "Participant"), preset("Other")))
      assertEquals(loaded._2, Vector.empty)
  }


  test("a listed preset disappearing before read is retained as a named failure") {
    for
      memory <- InMemoryPlatform.create[IO]()
      missing = ok(HostPath.of("/user/import-presets/disappeared.json"))
      files = new eyes4s.studio.core.platform.FileSystem[IO]:
        def child(dir: HostPath, name: String) = memory.platform.files.child(dir, name)
        def read(path: HostPath) = memory.platform.files.read(path)
        def readStream(path: HostPath, size: Int) = memory.platform.files.readStream(path, size)
        def write(path: HostPath, bytes: IArray[Byte]) = memory.platform.files.write(path, bytes)
        def list(path: HostPath): IO[Either[PlatformError, Vector[HostPath]]] = IO.pure(Right(Vector(missing)))
        def project(path: HostPath) = memory.platform.files.project(path)
      store = PlatformPresetStore(files, directory, names)
      loaded <- store.load
    yield
      assertEquals(loaded._1.all, Vector.empty)
      assertEquals(loaded._2.size, 1)
      assert(loaded._2.head.contains("disappeared.json"))
  }

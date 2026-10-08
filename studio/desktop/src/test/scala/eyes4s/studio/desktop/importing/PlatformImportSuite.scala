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

package eyes4s.studio.desktop.importing

import cats.effect.{IO, Ref}
import eyes4s.studio.core.document.{Source, SourceRole}
import eyes4s.studio.core.importing.{ImportPresets, SniffedSource}
import eyes4s.studio.core.platform.{HostPath, InMemoryPlatform, PlatformError}
import eyes4s.studio.desktop.platform.PlatformPresetStore
import eyes4s.studio.desktop.runtime.ProjectPort
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8

class PlatformImportSuite extends CatsEffectSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val file                             = ok(HostPath.of("opaque-id-42"))
  private val directory                        = ok(HostPath.of("/presets"))
  private val original                         =
    IArray.from("participant,phase,trial\r\nP01,Encoding,t1\r\n".getBytes(UTF_8))
  private def names(path: HostPath): Either[PlatformError, String] = Right("actual µ.csv")

  test("opaque chooser paths preserve supplied source names and cancellation is no selection") {
    for
      memory <- InMemoryPlatform.create[IO]()
      store    = PlatformPresetStore(memory.platform.files, directory, names)
      platform = PlatformImport(memory.platform, Right(store), None, names)
      _         <- memory.answer(Some(file))
      chosen    <- platform.chooseFile(SourceRole.Trials)
      _         <- memory.answer(None)
      cancelled <- platform.chooseFile(SourceRole.Fixations)
      requests  <- memory.requests
    yield
      assertEquals(chosen, Right(Some(ChosenSource(file, "actual µ.csv"))))
      assertEquals(cancelled, Right(None))
      assertEquals(requests.size, 2)
  }

  test("changed bytes refuse project storage before any imported source can be committed") {
    for
      memory <- InMemoryPlatform.create[IO]()
      writes <- Ref.of[IO, Int](0)
      port = new ProjectPort:
        def journal(entry: eyes4s.studio.core.command.JournalEntry): Unit = ()
        def save(done: Either[String, eyes4s.studio.core.session.SaveReceipt] => Unit): Unit =
          done(Left("unused"))
        def close(): Unit = ()
        override def importInput(
            kind: eyes4s.studio.core.bundle.InputKind,
            name: String,
            bytes: IArray[Byte],
            done: Either[String, Unit] => Unit
        ): Unit =
          import cats.effect.unsafe.implicits.global
          writes.update(_ + 1).unsafeRunSync()
          done(Right(()))
      store    = PlatformPresetStore(memory.platform.files, directory, names)
      platform = PlatformImport(memory.platform, Right(store), Some(port), names)
      _ <- memory.platform.files.write(file, original)
      source = ok(SniffedSource.read(SourceRole.Trials, "actual µ.csv", original)).source
      _       <- memory.platform.files.write(file, IArray.from("changed".getBytes(UTF_8)))
      refused <- platform.importInput(source, file)
      total   <- writes.get
    yield
      assert(refused.left.exists(_.contains("changed after")))
      assertEquals(total, 0)
  }

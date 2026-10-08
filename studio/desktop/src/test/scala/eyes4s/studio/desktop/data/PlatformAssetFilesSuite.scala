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

package eyes4s.studio.desktop.data

import cats.effect.{IO, Ref}
import eyes4s.studio.core.assets.AssetFile
import eyes4s.studio.core.platform.{
  DialogRequest,
  FileSystem,
  HostPath,
  InMemoryPlatform,
  PlatformError,
  PlatformFailure
}
import fs2.Stream
import java.nio.file.Files

class PlatformAssetFilesSuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def locate(
      files: AssetFiles
  ): IO[Either[AssetFileRefusal, Option[(AssetFile, IArray[Byte])]]] =
    IO.async_(done =>
      files.locate(get(AssetFile.of("missing.png")), value => done(Right(value)))
    )
  private val opaque = get(HostPath.of("browser-handle:opaque-42"))
  private val png    = IArray.from(
    Files.readAllBytes(
      eyes4s.studio.desktop.trial.GoldenTrials.stimuli.resolve("beach-042.png")
    )
  )

  test(
    "opaque host handles use injected names, actual image bytes and original chooser filters"
  ) {
    for
      memory   <- InMemoryPlatform.create[IO]()
      _        <- memory.platform.files.write(opaque, png).map(get)
      _        <- memory.answer(Some(opaque))
      result   <- locate(AssetFiles.onPlatform(memory.platform, _ => Right("chosen.png")))
      requests <- memory.requests
    yield
      assertEquals(result.map(_.map(_._1.value)), Right(Some("chosen.png")))
      assertEquals(
        result.toOption.flatten.map(value => Array.from(value._2).toVector),
        Some(Array.from(png).toVector)
      )
      val request = requests
        .collectFirst { case DialogRequest.Open(request) => request }
        .getOrElse(fail("no chooser request"))
      assertEquals(request.title, "Locate missing.png")
      assertEquals(
        request.kinds.flatMap(_.extensions),
        Vector("png", "jpg", "jpeg", "bmp", "gif")
      )
  }

  test("cancellation returns None; malformed image and bounded streams return named refusal") {
    for
      memory <- InMemoryPlatform.create[IO]()
      files = AssetFiles.onPlatform(memory.platform, _ => Right("chosen.png"))
      cancelled <- locate(files)
      _         <- memory.platform.files
        .write(
          opaque,
          IArray.from("not an image".getBytes(java.nio.charset.StandardCharsets.UTF_8))
        )
        .map(get)
      _       <- memory.answer(Some(opaque))
      invalid <- locate(files)
      _       <- memory.platform.files.write(opaque, png).map(get)
      _       <- memory.answer(Some(opaque))
      short   <- locate(
        AssetFiles.onPlatform(memory.platform, _ => Right("chosen.png"), imageLimit = 4)
      )
    yield
      assertEquals(cancelled, Right(None))
      assertEquals(invalid, Left(AssetFileRefusal.NotAnImage("chosen.png")))
      assert(short.left.toOption.exists {
        case AssetFileRefusal.Unreadable("chosen.png", reason) =>
          reason.contains("4-byte budget")
        case _ => false
      })
  }

  test(
    "mid-stream host refusal answers once, names the source handle and releases the stream"
  ) {
    for
      memory   <- InMemoryPlatform.create[IO]()
      released <- Ref.of[IO, Boolean](false)
      calls    <- Ref.of[IO, Int](0)
      base    = memory.platform.files
      failing = new FileSystem[IO]:
        def child(directory: HostPath, name: String)   = base.child(directory, name)
        def read(path: HostPath)                       = base.read(path)
        def write(path: HostPath, bytes: IArray[Byte]) = base.write(path, bytes)
        def list(directory: HostPath)                  = base.list(directory)
        def project(path: HostPath)                    = base.project(path)
        def readStream(path: HostPath, chunkSize: Int) = IO.pure(
          Right(
            (Stream.emit(1.toByte).covary[IO] ++ Stream.raiseError[IO](
              PlatformFailure(PlatformError.Unreadable(path, "device disconnected"))
            )).onFinalize(released.set(true))
          )
        )
      _ <- memory.answer(Some(opaque))
      files = AssetFiles.onPlatform(
        memory.platform.copy(files = failing),
        _ => Right("chosen.png")
      )
      result <- IO.async_[Either[AssetFileRefusal, Option[(AssetFile, IArray[Byte])]]](done =>
        files.locate(
          get(AssetFile.of("missing.png")),
          value => (calls.update(_ + 1) *> IO(done(Right(value)))).unsafeRunSync()
        )
      )
      closed <- released.get
      count  <- calls.get
    yield
      assert(closed)
      assertEquals(count, 1)
      assert(result.left.toOption.exists {
        case AssetFileRefusal.Unreadable("chosen.png", reason) =>
          reason.contains(opaque.value) && reason.contains("device disconnected")
        case _ => false
      })
  }

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

package eyes4s.studio.core.importing

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.platform.{HostPath, PlatformError, PlatformFailure}
import fs2.{Chunk, Stream}
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets.UTF_8

class StreamedSourceSuite extends CatsEffectSuite:
  private def raw(text: String): IArray[Byte]               = IArray.from(text.getBytes(UTF_8))
  private def stream(bytes: IArray[Byte]): Stream[IO, Byte] = Stream.emits(bytes.toSeq)
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("supplementary Unicode and BOM survive every byte boundary with the exact digest") {
    val text     = "\ufeffparticipant,note\r\nP01,\"µ 😀 line\nsecond\"\r\n"
    val bytes    = raw(text)
    val expected = ok(SniffedSource.read(SourceRole.Trials, "trials.csv", bytes))
    (0 to bytes.length).toVector.traverse_ { split =>
      val chunks =
        Stream.chunk(Chunk.array(IArray.genericWrapArray(bytes.take(split)).toArray)) ++
          Stream.chunk(Chunk.array(IArray.genericWrapArray(bytes.drop(split)).toArray))
      StreamedSource.preview[IO](SourceRole.Trials, "trials.csv", chunks).map { found =>
        assertEquals(found, Right(expected), s"split $split")
      }
    } >> StreamedSource.text[IO]("trials.csv", stream(bytes).chunkLimit(1).unchunks).map {
      result =>
        val decoded = ok(result)
        assertEquals(decoded.value.toString, text)
        assertEquals(decoded.bytes, ByteDigest.sha256(bytes))
    }
  }

  test("malformed, overlong and truncated UTF8 retains legacy exact offsets") {
    val examples = Vector(
      Vector(0x61, 0xc3, 0x28),
      Vector(0x61, 0xc0, 0x80),
      Vector(0x61, 0xe0, 0x80, 0x80),
      Vector(0x61, 0xed, 0xa0, 0x80),
      Vector(0x61, 0xf4, 0x90, 0x80, 0x80),
      Vector(0x61, 0xf0, 0x9f),
      Vector(0xff, 0xfe),
      Vector(0xfe, 0xff),
      Vector(0x80)
    )
    examples.traverse_ { values =>
      val bytes    = IArray.from(values.map(_.toByte))
      val expected =
        SniffedSource.decodeUtf8("bad.csv", bytes).left.map(SourceReadError.Unpreviewable(_))
      (1 to 4).toVector.traverse_ { size =>
        StreamedSource.text[IO]("bad.csv", stream(bytes).chunkLimit(size).unchunks).map {
          actual =>
            assertEquals(actual.left.toOption, expected.left.toOption, values.toString)
        }
      }
    }
  }

  test("exact-byte collection is independent of UTF8 decoding") {
    val bytes = IArray.from(Vector(0xff.toByte, 0x00.toByte, 0x7f.toByte))
    StreamedSource.bytes[IO]("image.bin", stream(bytes).chunkLimit(1).unchunks).map { result =>
      assertEquals(ok(result).toSeq, bytes.toSeq)
    }
  }

  test("cancelling a pending read releases its resource without publishing a result") {
    for
      entered  <- Deferred[IO, Unit]
      released <- Ref.of[IO, Boolean](false)
      resource = Resource.make(entered.complete(()).void)(_ => released.set(true))
      source   = Stream.resource(resource).flatMap(_ => Stream.never[IO])
      fiber  <- StreamedSource.text[IO]("held.csv", source).start
      _      <- entered.get
      _      <- fiber.cancel
      done   <- fiber.join
      closed <- released.get
    yield
      assert(done.isCanceled)
      assert(closed)
  }

  test("midstream platform failures name the source and finalize the stream") {
    for
      released <- Ref.of[IO, Boolean](false)
      path   = ok(HostPath.of("/bad.csv"))
      source = (Stream.emit('a'.toByte).covary[IO] ++
        Stream.raiseError[IO](PlatformFailure(PlatformError.Unreadable(path, "read failed"))))
        .onFinalize(released.set(true))
      result <- StreamedSource.text[IO]("bad.csv", source)
      closed <- released.get
    yield
      assert(result.left.exists(_.message.contains("read failed")))
      assert(result.left.exists(_.message.contains("bad.csv")))
      assert(closed)
  }

  test("byte budgets stop oversized streams and release resources before retaining overflow") {
    for
      released <- Ref.of[IO, Boolean](false)
      reached  <- Ref.of[IO, Boolean](false)
      source = (Stream.chunk(Chunk.array(Array[Byte](1, 2))) ++
        Stream.chunk(Chunk.array(Array[Byte](3, 4))) ++
        Stream.eval(reached.set(true)).drain).onFinalize(released.set(true))
      result    <- StreamedSource.bytes[IO]("large.csv", source, maxBytes = 3)
      text      <- StreamedSource.text[IO]("large.csv", stream(raw("abcd")), maxBytes = 3)
      closed    <- released.get
      continued <- reached.get
    yield
      assert(result.left.exists(_.message.contains("3-byte budget")))
      assert(text.left.exists(_.message.contains("3-byte budget")))
      assert(result.left.exists(_.message.contains("large.csv")))
      assert(closed)
      assert(!continued)
  }

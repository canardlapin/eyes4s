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

package eyes4s.io

import _root_.fs2.Chunk
import _root_.fs2.Pure
import _root_.fs2.Stream

class EyeLinkAscStreamingSuite extends munit.FunSuite:

  private def bytes(value: String): IArray[Byte] =
    IArray.tabulate(value.length)(index => value.charAt(index).toByte)

  private def settings(maximum: Int = 1024): AscStreamSettings =
    AscStreamSettings.of("stream.asc", maximum, 16).toOption.get

  private def canonical(output: Vector[AscFramingEmission]): Vector[String] =
    output.map {
      case AscFramingEmission.Parsed(value) =>
        val source = value.source
        s"parsed:${source.number}:${source.byteOffset}:${source.terminator}:${source.bytes.toVector}:${value.record.getClass.getName}:${value.diagnostics.map(_.message)}"
      case AscFramingEmission.Rejected(value) => s"rejected:${value.message}"
    }

  private def streamed(
      settings: AscStreamSettings,
      chunks: Vector[IArray[Byte]]
  ): Vector[AscFramingEmission] =
    Stream
      .emits(chunks)
      .flatMap(chunk => Stream.chunk(Chunk.from(chunk.toVector)))
      .through(EyeLinkAscStreaming.pipe[Pure](settings))
      .toVector

  test("LF, CRLF, blank, and final unterminated lines retain exact locations") {
    val input  = bytes("MSG 1 alpha\r\n\n2 10 20 30\nFUTURE tail")
    val output = EyeLinkAscFraming.whole(settings(), input)
    val parsed = output.collect { case AscFramingEmission.Parsed(value) => value }

    assertEquals(parsed.length, 4)
    assertEquals(parsed.map(_.source.number), Vector(1L, 2L, 3L, 4L))
    assertEquals(parsed.map(_.source.byteOffset), Vector(0L, 13L, 14L, 25L))
    assertEquals(
      parsed.map(_.source.terminator),
      Vector(
        AscLineTerminator.CarriageReturnLineFeed,
        AscLineTerminator.LineFeed,
        AscLineTerminator.LineFeed,
        AscLineTerminator.EndOfFile
      )
    )
    assertEquals(parsed(3).source.bytes.toVector, bytes("FUTURE tail").toVector)
  }

  test("every byte partition agrees with pure whole-input framing") {
    val input     = bytes("START 1 LEFT SAMPLES\r\nSAMPLES GAZE LEFT RATE 1000\n2 . . 0\nEND 3")
    val reference = canonical(EyeLinkAscFraming.whole(settings(), input))
    val oneByte   = input.toVector.map(value => IArray(value))
    val partitions = Vector(
      Vector(input),
      oneByte,
      Vector(IArray.empty[Byte], input.take(5), input.slice(5, 31), input.drop(31)),
      input.grouped(7).map(group => IArray.from(group.toVector.toArray)).toVector
    )

    partitions.foreach(chunks =>
      assertEquals(canonical(streamed(settings(), chunks)), reference)
    )
  }

  test("line overflow is bounded, counted exactly, and parsing resumes") {
    val output = EyeLinkAscFraming.whole(settings(maximum = 5), bytes("123456789\nMSG 2\n"))

    output.head match
      case AscFramingEmission.Rejected(
            AscFramingDiagnostic.LineTooLong(source, line, offset, limit, actual, excerpt)
          ) =>
        assertEquals(source, "stream.asc")
        assertEquals(line, 1L)
        assertEquals(offset, 0L)
        assertEquals(limit, 5)
        assertEquals(actual, 9L)
        assertEquals(excerpt, "12345")
      case other => fail(s"expected line-too-long diagnostic, found=$other")
    output(1) match
      case AscFramingEmission.Parsed(value) =>
        assertEquals(value.source.number, 2L)
        assertEquals(value.source.byteOffset, 10L)
      case other => fail(s"expected resumed parsed line, found=$other")
  }

  test("lone carriage returns reject exactly one line and consume the following byte once") {
    val output = EyeLinkAscFraming.whole(settings(), bytes("MSG 1 bad\rMSG 2 good\n"))

    assertEquals(output.count(_.isInstanceOf[AscFramingEmission.Rejected]), 1)
    assertEquals(output.count(_.isInstanceOf[AscFramingEmission.Parsed]), 1)
    output.last match
      case AscFramingEmission.Parsed(value) =>
        assertEquals(value.source.number, 2L)
        assertEquals(value.source.byteOffset, 10L)
        assertEquals(value.source.bytes.toVector, bytes("MSG 2 good").toVector)
      case other => fail(s"expected second parsed line, found=$other")
  }

  test("budgets fail before streaming") {
    assert(AscStreamSettings.of(" ", 10, 10).isLeft)
    assert(AscStreamSettings.of("x.asc", 0, 10).isLeft)
    assert(AscStreamSettings.of("x.asc", 10, 0).isLeft)
    assert(AscReadChunkSize.of(-1).isLeft)
  }

  test("the streaming pipe enforces its byte chunk budget on a larger upstream chunk") {
    val input  = bytes("0123456789" * 100)
    val chunks = Stream
      .chunk(Chunk.from(input.toVector))
      .through(EyeLinkAscStreaming.boundedChunks[Pure](settings()))
      .toVector

    assertEquals(chunks.flatMap(_.toVector), input.toVector)
    assert(chunks.nonEmpty)
    assert(chunks.forall(_.length <= settings().readChunkSize.bytes))
    assertEquals(chunks.dropRight(1).map(_.length).distinct, Vector(16))
  }

end EyeLinkAscStreamingSuite
